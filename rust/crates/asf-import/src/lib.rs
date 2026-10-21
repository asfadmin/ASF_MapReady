use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

use asf_core::{path::image_path, Error, Result};
use asf_meta::{write_metadata, BandMetadata, DataType, GeneralMetadata, Metadata};
use tiff::decoder::{Decoder, DecodingResult, Limits};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputFormat {
    Sentinel,
}

impl std::str::FromStr for InputFormat {
    type Err = Error;

    fn from_str(value: &str) -> Result<Self> {
        match value.to_ascii_lowercase().as_str() {
            "sentinel" | "sentinel1" | "sentinel-1" | "s1" => Ok(Self::Sentinel),
            other => Err(Error::UnsupportedFormat(other.to_string())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportOptions {
    pub format: InputFormat,
    pub input: String,
    pub output_base: String,
}

pub fn import(options: &ImportOptions) -> Result<()> {
    match options.format {
        InputFormat::Sentinel => import_sentinel(&options.input, &options.output_base),
    }
}

pub fn import_sentinel(input: impl AsRef<Path>, output_base: impl AsRef<Path>) -> Result<()> {
    let safe_dir = find_safe_dir(input.as_ref())?;
    let measurement_dir = safe_dir.join("measurement");

    if !measurement_dir.is_dir() {
        return Err(Error::InvalidInput(format!(
            "Sentinel SAFE directory does not contain a measurement directory: {}",
            safe_dir.display()
        )));
    }

    let measurements = find_measurement_tiffs(&measurement_dir)?;

    if measurements.is_empty() {
        return Err(Error::InvalidInput(format!(
            "No measurement TIFF files found in {}",
            measurement_dir.display()
        )));
    }

    let output_img = image_path(output_base.as_ref());
    let mut writer = BufWriter::new(File::create(&output_img)?);

    let mut line_count = 0usize;
    let mut sample_count = 0usize;
    let mut bands = Vec::with_capacity(measurements.len());

    for (band_index, measurement) in measurements.iter().enumerate() {
        let decoded = decode_measurement_as_f32(measurement)?;

        if band_index == 0 {
            line_count = decoded.height;
            sample_count = decoded.width;
        } else if decoded.height != line_count || decoded.width != sample_count {
            return Err(Error::InvalidInput(format!(
                "Measurement dimensions do not match first band. First band is {}x{}, but {} is {}x{}",
                sample_count,
                line_count,
                measurement.display(),
                decoded.width,
                decoded.height
            )));
        }

        for value in decoded.samples {
            writer.write_all(&value.to_le_bytes())?;
        }

        bands.push(BandMetadata {
            name: infer_sentinel_band_name(measurement),
        });
    }

    writer.flush()?;

    let metadata = Metadata {
        general: GeneralMetadata {
            line_count,
            sample_count,
            band_count: bands.len(),
            data_type: DataType::F32,
            x_pixel_size: 10.0,
            y_pixel_size: 10.0,
            no_data: Some(0.0),
            sensor: Some("SENTINEL-1".to_string()),
        },
        bands,
        statistics: None,
    };

    write_metadata(output_base, &metadata)?;

    Ok(())
}

#[derive(Debug)]
struct DecodedMeasurement {
    width: usize,
    height: usize,
    samples: Vec<f32>,
}

fn find_safe_dir(input: &Path) -> Result<PathBuf> {
    if input.is_dir() && has_safe_extension(input) {
        return Ok(input.to_path_buf());
    }

    if input.is_dir() && input.join("manifest.safe").is_file() {
        return Ok(input.to_path_buf());
    }

    for ancestor in input.ancestors() {
        if ancestor.is_dir()
            && (has_safe_extension(ancestor) || ancestor.join("manifest.safe").is_file())
        {
            return Ok(ancestor.to_path_buf());
        }
    }

    Err(Error::InvalidInput(format!(
        "Could not locate Sentinel .SAFE directory from input path: {}",
        input.display()
    )))
}

fn has_safe_extension(path: &Path) -> bool {
    path.extension()
        .and_then(|extension| extension.to_str())
        .map(|extension| extension.eq_ignore_ascii_case("SAFE"))
        .unwrap_or(false)
}

fn find_measurement_tiffs(measurement_dir: &Path) -> Result<Vec<PathBuf>> {
    let mut measurements = fs::read_dir(measurement_dir)?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::io::Result<Vec<_>>>()?;

    measurements.retain(|path| {
        path.is_file()
            && path
                .extension()
                .and_then(|extension| extension.to_str())
                .map(|extension| {
                    extension.eq_ignore_ascii_case("tif") || extension.eq_ignore_ascii_case("tiff")
                })
                .unwrap_or(false)
    });

    measurements.sort();

    Ok(measurements)
}

fn decode_measurement_as_f32(path: &Path) -> Result<DecodedMeasurement> {
    let file = File::open(path)?;
    let mut decoder = Decoder::new(file).map_err(|error| {
        Error::InvalidInput(format!("Could not open TIFF {}: {error}", path.display()))
    })?;

    decoder = decoder.with_limits(Limits::unlimited());

    let (width, height) = decoder.dimensions().map_err(|error| {
        Error::InvalidInput(format!(
            "Could not read TIFF dimensions from {}: {error}",
            path.display()
        ))
    })?;

    let result = decoder.read_image().map_err(|error| {
        Error::InvalidInput(format!(
            "Could not read TIFF image from {}: {error}",
            path.display()
        ))
    })?;

    let samples = match result {
        DecodingResult::U8(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::U16(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::U32(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::U64(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::I8(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::I16(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::I32(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::I64(values) => values.into_iter().map(|value| value as f32).collect(),
        DecodingResult::F32(values) => values,
        DecodingResult::F64(values) => values.into_iter().map(|value| value as f32).collect(),
    };

    let expected_len = width as usize * height as usize;

    if samples.len() != expected_len {
        return Err(Error::InvalidInput(format!(
            "Unexpected sample count in {}. Expected {}, found {}",
            path.display(),
            expected_len,
            samples.len()
        )));
    }

    Ok(DecodedMeasurement {
        width: width as usize,
        height: height as usize,
        samples,
    })
}

fn infer_sentinel_band_name(path: &Path) -> String {
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or_default()
        .to_ascii_lowercase();

    for polarization in ["hh", "hv", "vh", "vv"] {
        let needle = format!("-{polarization}-");
        if file_name.contains(&needle) {
            return polarization.to_ascii_uppercase();
        }
    }

    path.file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("BAND")
        .to_ascii_uppercase()
}
