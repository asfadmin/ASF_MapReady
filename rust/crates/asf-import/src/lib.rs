use std::fs::{self, File};
use std::io::{BufWriter, Read, Write};
use std::path::{Path, PathBuf};

use asf_core::{path::image_path, Error, Result};
use asf_meta::{write_metadata, BandMetadata, DataType, GeneralMetadata, Metadata};
use roxmltree::{Document, Node};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radiometry {
    Amplitude,
    Sigma,
    Beta,
    Gamma,
    Power,
    SigmaDb,
    BetaDb,
    GammaDb,
}

impl Radiometry {
    fn calibration_field(self) -> Result<&'static str> {
        match self {
            Self::Sigma | Self::SigmaDb => Ok("sigmaNought"),
            Self::Beta | Self::BetaDb => Ok("betaNought"),
            Self::Gamma | Self::GammaDb => Ok("gamma"),
            Self::Amplitude => Ok("dn"),
            Self::Power => Err(Error::InvalidArgument(
                "Sentinel --power import is not implemented yet because the legacy Sentinel behavior needs a separate review".to_string(),
            )),
        }
    }

    fn is_db(self) -> bool {
        matches!(self, Self::SigmaDb | Self::BetaDb | Self::GammaDb)
    }

    fn is_calibrated_power(self) -> bool {
        matches!(
            self,
            Self::Sigma
                | Self::Beta
                | Self::Gamma
                | Self::SigmaDb
                | Self::BetaDb
                | Self::GammaDb
        )
    }
}

#[derive(Debug, Clone)]
pub struct ImportOptions {
    pub format: InputFormat,
    pub input: String,
    pub output_base: String,
    pub radiometry: Radiometry,
}

pub fn import(options: &ImportOptions) -> Result<()> {
    match options.format {
        InputFormat::Sentinel => {
            import_sentinel(&options.input, &options.output_base, options.radiometry)
        }
    }
}

pub fn import_sentinel(
    input: impl AsRef<Path>,
    output_base: impl AsRef<Path>,
    radiometry: Radiometry,
) -> Result<()> {
    let product = SentinelProduct::discover(input.as_ref())?;

    match product.product_type {
        SentinelProductType::Grd => import_sentinel_grd(&product, output_base.as_ref(), radiometry),
        SentinelProductType::Slc => Err(Error::InvalidInput(
            "Sentinel SLC import is not implemented yet".to_string(),
        )),
        SentinelProductType::Raw => Err(Error::InvalidInput(
            "Sentinel RAW import is not supported".to_string(),
        )),
        SentinelProductType::Ocn => Err(Error::InvalidInput(
            "Sentinel OCN import is not implemented yet".to_string(),
        )),
    }
}

#[derive(Debug)]
struct SentinelProduct {
    safe_dir: PathBuf,
    mission: String,
    beam_mode: String,
    product_type: SentinelProductType,
    measurements: Vec<SentinelMeasurement>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SentinelProductType {
    Grd,
    Slc,
    Raw,
    Ocn,
}

#[derive(Debug)]
struct SentinelMeasurement {
    polarization: String,
    measurement_path: PathBuf,
    annotation_path: PathBuf,
    calibration_path: PathBuf,
    noise_path: PathBuf,
    width: usize,
    height: usize,
}

impl SentinelProduct {
    fn discover(input: &Path) -> Result<Self> {
        let safe_dir = find_safe_dir(input)?;

        let product_type = infer_product_type(&safe_dir)?;
        let mission = infer_mission(&safe_dir);
        let beam_mode = infer_beam_mode(&safe_dir);

        let measurement_dir = safe_dir.join("measurement");
        let annotation_dir = safe_dir.join("annotation");
        let calibration_dir = annotation_dir.join("calibration");

        if !measurement_dir.is_dir() {
            return Err(Error::InvalidInput(format!(
                "Sentinel SAFE directory does not contain a measurement directory: {}",
                safe_dir.display()
            )));
        }

        if !annotation_dir.is_dir() {
            return Err(Error::InvalidInput(format!(
                "Sentinel SAFE directory does not contain an annotation directory: {}",
                safe_dir.display()
            )));
        }

        if !calibration_dir.is_dir() {
            return Err(Error::InvalidInput(format!(
                "Sentinel SAFE directory does not contain annotation/calibration: {}",
                safe_dir.display()
            )));
        }

        let measurement_paths = find_measurement_tiffs(&measurement_dir)?;

        if measurement_paths.is_empty() {
            return Err(Error::InvalidInput(format!(
                "No measurement TIFF files found in {}",
                measurement_dir.display()
            )));
        }

        let mut measurements = Vec::with_capacity(measurement_paths.len());

        for measurement_path in measurement_paths {
            let polarization = infer_sentinel_band_name(&measurement_path);
            let stem = measurement_path
                .file_stem()
                .and_then(|name| name.to_str())
                .ok_or_else(|| {
                    Error::InvalidInput(format!(
                        "Measurement path has no valid file stem: {}",
                        measurement_path.display()
                    ))
                })?;

            let annotation_path = annotation_dir.join(format!("{stem}.xml"));
            let calibration_path = calibration_dir.join(format!("calibration-{stem}.xml"));
            let noise_path = calibration_dir.join(format!("noise-{stem}.xml"));

            if !annotation_path.is_file() {
                return Err(Error::InvalidInput(format!(
                    "Missing Sentinel annotation XML for {}: {}",
                    measurement_path.display(),
                    annotation_path.display()
                )));
            }

            if !calibration_path.is_file() {
                return Err(Error::InvalidInput(format!(
                    "Missing Sentinel calibration XML for {}: {}",
                    measurement_path.display(),
                    calibration_path.display()
                )));
            }

            if !noise_path.is_file() {
                return Err(Error::InvalidInput(format!(
                    "Missing Sentinel noise XML for {}: {}",
                    measurement_path.display(),
                    noise_path.display()
                )));
            }

            let (width, height) = tiff_dimensions(&measurement_path)?;

            measurements.push(SentinelMeasurement {
                polarization,
                measurement_path,
                annotation_path,
                calibration_path,
                noise_path,
                width,
                height,
            });
        }

        Ok(Self {
            safe_dir,
            mission,
            beam_mode,
            product_type,
            measurements,
        })
    }
}

fn import_sentinel_grd(
    product: &SentinelProduct,
    output_base: &Path,
    radiometry: Radiometry,
) -> Result<()> {
    if product.beam_mode == "UNKNOWN" {
        return Err(Error::InvalidInput(format!(
            "Could not infer Sentinel beam mode from SAFE directory name: {}",
            product.safe_dir.display()
        )));
    }

    let first = product.measurements.first().ok_or_else(|| {
        Error::InvalidInput(format!(
            "No Sentinel measurements found in {}",
            product.safe_dir.display()
        ))
    })?;

    for measurement in &product.measurements {
        if !measurement.annotation_path.is_file() {
            return Err(Error::InvalidInput(format!(
                "Missing Sentinel annotation XML for {}: {}",
                measurement.measurement_path.display(),
                measurement.annotation_path.display()
            )));
        }

        if measurement.width != first.width || measurement.height != first.height {
            return Err(Error::InvalidInput(format!(
                "Measurement dimensions do not match first band. First band is {}x{}, but {} is {}x{}",
                first.width,
                first.height,
                measurement.measurement_path.display(),
                measurement.width,
                measurement.height
            )));
        }
    }

    let output_img = image_path(output_base);
    let mut writer = BufWriter::new(File::create(&output_img)?);
    let mut bands = Vec::with_capacity(product.measurements.len());

    for measurement in &product.measurements {
        let decoded = decode_measurement_as_f32(&measurement.measurement_path)?;

        let calibration = read_sentinel_calibration(&measurement.calibration_path, radiometry)?;
        let noise = read_sentinel_noise(&measurement.noise_path, decoded.height, decoded.width)?;

        for line in 0..decoded.height {
            let cal_line = interpolate_lut_line(&calibration, line, decoded.width)?;
            let noise_line = interpolate_lut_line(&noise, line, decoded.width)?;

            let row_start = line * decoded.width;
            let row_end = row_start + decoded.width;

            for (sample_index, dn) in decoded.samples[row_start..row_end].iter().enumerate() {
                let value = calibrated_grd_value(
                    *dn,
                    cal_line[sample_index],
                    noise_line[sample_index],
                    radiometry,
                )?;

                writer.write_all(&value.to_le_bytes())?;
            }
        }

        bands.push(BandMetadata {
            name: measurement.polarization.clone(),
        });
    }

    writer.flush()?;

        let metadata = Metadata {
            general: GeneralMetadata {
                line_count: first.height,
                sample_count: first.width,
                band_count: bands.len(),
                data_type: DataType::F32,
                x_pixel_size: 10.0,
                y_pixel_size: 10.0,
                no_data: Some(0.0),
                sensor: Some(product.mission.clone()),
            },
            bands,
            statistics: None,
            source_geolocation: None,
            georeferencing: None,
        };

    write_metadata(output_base, &metadata)?;

    Ok(())
}

fn calibrated_grd_value(dn: f32, cal_lut: f32, noise_lut: f32, radiometry: Radiometry) -> Result<f32> {
    if !dn.is_finite() || !cal_lut.is_finite() || !noise_lut.is_finite() || cal_lut == 0.0 {
        return Ok(0.0);
    }

    if radiometry == Radiometry::Amplitude {
        return Ok(dn);
    }

    if !radiometry.is_calibrated_power() {
        return Err(Error::InvalidArgument(format!(
            "Radiometry mode is not implemented for Sentinel GRD import: {radiometry:?}"
        )));
    }

    let scaled_power = (dn * dn - noise_lut) / (cal_lut * cal_lut);

    if radiometry.is_db() {
        if scaled_power < 0.0 {
            Ok(-40.0)
        } else {
            Ok(10.0 * scaled_power.log10())
        }
    } else {
        Ok(scaled_power)
    }
}

#[derive(Debug)]
struct DecodedMeasurement {
    width: usize,
    height: usize,
    samples: Vec<f32>,
}

#[derive(Debug)]
struct SentinelLut {
    lines: Vec<SentinelLutLine>,
}

#[derive(Debug)]
struct SentinelLutLine {
    line: usize,
    pixels: Vec<usize>,
    values: Vec<f32>,
}


fn read_sentinel_calibration(path: &Path, radiometry: Radiometry) -> Result<SentinelLut> {
    let xml = read_text(path)?;
    let document = parse_xml_document(&xml, path)?;
    let field = radiometry.calibration_field()?;

    let vectors = document
        .descendants()
        .filter(|node| node.has_tag_name("calibrationVector"));

    let mut lines = Vec::new();

    for vector in vectors {
        let line = parse_required_usize_child(vector, "line", path)?;
        let pixels = parse_required_usize_list_child(vector, "pixel", path)?;
        let values = parse_required_f32_list_child(vector, field, path)?;

        lines.push(SentinelLutLine {
            line,
            pixels,
            values,
        });
    }

    if lines.is_empty() {
        return Err(Error::InvalidInput(format!(
            "No calibrationVector elements found in {}",
            path.display()
        )));
    }

    validate_lut(path, &lines)?;

    Ok(SentinelLut { lines })
}

fn read_sentinel_noise(path: &Path, max_line: usize, max_pixel: usize) -> Result<SentinelLut> {
    let xml = read_text(path)?;
    let document = parse_xml_document(&xml, path)?;

    let regular_vectors = document
        .descendants()
        .filter(|node| node.has_tag_name("noiseVector"))
        .collect::<Vec<_>>();

    let range_vectors = document
        .descendants()
        .filter(|node| node.has_tag_name("noiseRangeVector"))
        .collect::<Vec<_>>();

    let (vectors, value_tag) = if !regular_vectors.is_empty() {
        (regular_vectors, "noiseLut")
    } else if !range_vectors.is_empty() {
        (range_vectors, "noiseRangeLut")
    } else {
        return Err(Error::InvalidInput(format!(
            "No noiseVector or noiseRangeVector elements found in {}",
            path.display()
        )));
    };

    let mut lines = Vec::with_capacity(vectors.len());

    for vector in vectors {
        let line = parse_required_usize_child(vector, "line", path)?;
        let mut pixels = parse_required_usize_list_child(vector, "pixel", path)?;
        let values = parse_required_f32_list_child(vector, value_tag, path)?;

        if let Some(last_pixel) = pixels.last_mut() {
            *last_pixel = max_pixel.saturating_sub(1);
        }

        lines.push(SentinelLutLine {
            line,
            pixels,
            values,
        });
    }

    if let Some(last_line) = lines.last_mut() {
        last_line.line = max_line.saturating_sub(1);
    }

    normalize_noise_lut(&mut lines);
    validate_lut(path, &lines)?;

    Ok(SentinelLut { lines })
}

fn normalize_noise_lut(lines: &mut [SentinelLutLine]) {
    let max_noise = lines
        .iter()
        .flat_map(|line| line.values.iter())
        .copied()
        .filter(|value| value.is_finite())
        .fold(f32::NEG_INFINITY, f32::max);

    let scale = if max_noise > 0.0 && max_noise < 5.0e-9 {
        Some(1.0e12)
    } else if max_noise > 0.0 && max_noise < 5.0e-7 {
        Some(2.0e10)
    } else {
        None
    };

    if let Some(scale) = scale {
        for line in lines {
            for value in &mut line.values {
                *value *= scale;
            }
        }
    }
}

fn interpolate_lut_line(lut: &SentinelLut, line: usize, sample_count: usize) -> Result<Vec<f32>> {
    if lut.lines.is_empty() {
        return Err(Error::InvalidInput("Cannot interpolate an empty Sentinel LUT".to_string()));
    }

    let lower_index = match lut
        .lines
        .binary_search_by_key(&line, |lut_line| lut_line.line)
    {
        Ok(index) => index,
        Err(0) => 0,
        Err(index) if index >= lut.lines.len() => lut.lines.len() - 1,
        Err(index) => index - 1,
    };

    let upper_index = if lower_index + 1 < lut.lines.len() {
        lower_index + 1
    } else {
        lower_index
    };

    let lower = &lut.lines[lower_index];
    let upper = &lut.lines[upper_index];

    let line_fraction = if upper.line == lower.line {
        0.0
    } else {
        (line.saturating_sub(lower.line)) as f32 / (upper.line - lower.line) as f32
    };

    let mut values = Vec::with_capacity(sample_count);

    for sample in 0..sample_count {
        let lower_value = interpolate_lut_pixels(lower, sample)?;
        let upper_value = interpolate_lut_pixels(upper, sample)?;
        values.push(lower_value + (upper_value - lower_value) * line_fraction);
    }

    Ok(values)
}

fn interpolate_lut_pixels(line: &SentinelLutLine, sample: usize) -> Result<f32> {
    if line.pixels.is_empty() || line.values.is_empty() {
        return Err(Error::InvalidInput(
            "Cannot interpolate an empty Sentinel LUT line".to_string(),
        ));
    }

    if sample <= line.pixels[0] {
        return Ok(line.values[0]);
    }

    for index in 0..line.pixels.len() - 1 {
        let left_pixel = line.pixels[index];
        let right_pixel = line.pixels[index + 1];

        if sample <= right_pixel {
            let left_value = line.values[index];
            let right_value = line.values[index + 1];

            if right_pixel == left_pixel {
                return Ok(left_value);
            }

            let fraction = (sample - left_pixel) as f32 / (right_pixel - left_pixel) as f32;
            return Ok(left_value + (right_value - left_value) * fraction);
        }
    }

    Ok(*line.values.last().expect("validated non-empty values"))
}

fn validate_lut(path: &Path, lines: &[SentinelLutLine]) -> Result<()> {
    if lines.is_empty() {
        return Err(Error::InvalidInput(format!(
            "Sentinel LUT is empty: {}",
            path.display()
        )));
    }

    let mut previous_line = None;

    for line in lines {
        if let Some(previous_line) = previous_line {
            if line.line < previous_line {
                return Err(Error::InvalidInput(format!(
                    "Sentinel LUT lines are not sorted in {}",
                    path.display()
                )));
            }
        }

        if line.pixels.len() != line.values.len() {
            return Err(Error::InvalidInput(format!(
                "Sentinel LUT pixel/value count mismatch in {} on line {}",
                path.display(),
                line.line
            )));
        }

        if line.pixels.len() < 2 {
            return Err(Error::InvalidInput(format!(
                "Sentinel LUT line {} in {} has fewer than two pixels",
                line.line,
                path.display()
            )));
        }

        for pair in line.pixels.windows(2) {
            if pair[1] < pair[0] {
                return Err(Error::InvalidInput(format!(
                    "Sentinel LUT pixels are not sorted in {} on line {}",
                    path.display(),
                    line.line
                )));
            }
        }

        previous_line = Some(line.line);
    }

    Ok(())
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

fn tiff_dimensions(path: &Path) -> Result<(usize, usize)> {
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

    Ok((width as usize, height as usize))
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

fn infer_mission(safe_dir: &Path) -> String {
    safe_dir
        .file_name()
        .and_then(|name| name.to_str())
        .and_then(|name| name.split('_').next())
        .map(|mission| mission.to_ascii_uppercase())
        .unwrap_or_else(|| "SENTINEL-1".to_string())
}

fn infer_beam_mode(safe_dir: &Path) -> String {
    safe_dir
        .file_name()
        .and_then(|name| name.to_str())
        .and_then(|name| name.split('_').nth(1))
        .map(|mode| mode.to_ascii_uppercase())
        .unwrap_or_else(|| "UNKNOWN".to_string())
}

fn infer_product_type(safe_dir: &Path) -> Result<SentinelProductType> {
    let name = safe_dir
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or_default()
        .to_ascii_uppercase();

    if name.contains("_GRD") || name.contains("_GRDH") || name.contains("_GRDM") {
        Ok(SentinelProductType::Grd)
    } else if name.contains("_SLC") {
        Ok(SentinelProductType::Slc)
    } else if name.contains("_RAW") {
        Ok(SentinelProductType::Raw)
    } else if name.contains("_OCN") {
        Ok(SentinelProductType::Ocn)
    } else {
        Err(Error::InvalidInput(format!(
            "Could not infer Sentinel product type from SAFE directory name: {}",
            safe_dir.display()
        )))
    }
}


fn read_text(path: &Path) -> Result<String> {
    let mut text = String::new();
    File::open(path)?.read_to_string(&mut text)?;
    Ok(text)
}

fn parse_xml_document<'a>(xml: &'a str, path: &Path) -> Result<Document<'a>> {
    Document::parse(xml).map_err(|error| {
        Error::InvalidInput(format!(
            "Could not parse Sentinel XML {}: {error}",
            path.display()
        ))
    })
}

fn required_child_text<'a>(node: Node<'a, 'a>, tag: &str, path: &Path) -> Result<&'a str> {
    node.children()
        .find(|child| child.has_tag_name(tag))
        .and_then(|child| child.text())
        .map(str::trim)
        .filter(|text| !text.is_empty())
        .ok_or_else(|| {
            Error::InvalidInput(format!(
                "Missing <{tag}> in Sentinel XML {}",
                path.display()
            ))
        })
}

fn parse_required_usize_child(node: Node<'_, '_>, tag: &str, path: &Path) -> Result<usize> {
    required_child_text(node, tag, path)?
        .parse::<usize>()
        .map_err(|error| {
            Error::InvalidInput(format!(
                "Invalid integer in <{tag}> in Sentinel XML {}: {error}",
                path.display()
            ))
        })
}

fn parse_required_usize_list_child(node: Node<'_, '_>, tag: &str, path: &Path) -> Result<Vec<usize>> {
    required_child_text(node, tag, path)?
        .split_whitespace()
        .map(|value| {
            value.parse::<usize>().map_err(|error| {
                Error::InvalidInput(format!(
                    "Invalid integer in <{tag}> in Sentinel XML {}: {error}",
                    path.display()
                ))
            })
        })
        .collect()
}

fn parse_required_f32_list_child(node: Node<'_, '_>, tag: &str, path: &Path) -> Result<Vec<f32>> {
    required_child_text(node, tag, path)?
        .split_whitespace()
        .map(|value| {
            value.parse::<f32>().map_err(|error| {
                Error::InvalidInput(format!(
                    "Invalid float in <{tag}> in Sentinel XML {}: {error}",
                    path.display()
                ))
            })
        })
        .collect()
}
