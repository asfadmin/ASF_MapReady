use std::fs::File;
use std::io::{Seek, Write};
use std::path::Path;

use asf_core::{Error, Result};
use asf_meta::{DataType, GeoreferencingMetadata, Metadata, ProjectionMetadata};
use asf_raster::{open_raster, read_f32_band};
use tiff::encoder::{colortype, TiffEncoder, TiffKind};
use tiff::tags::Tag;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    GeoTiff,
    Tiff,
    Png,
    Jpeg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteScale {
    None,
    Truncate,
    MinMax,
    Sigma,
}

#[derive(Debug, Clone)]
pub struct ExportOptions {
    pub input_base: String,
    pub output: String,
    pub format: OutputFormat,
    pub byte_scale: ByteScale,
    pub band: Option<String>,
    pub rgb: Option<String>,
}

#[derive(Debug, Clone, Copy)]
struct ScaleRange {
    min: f64,
    max: f64,
}

#[derive(Debug, Clone, Copy)]
struct SampleStats {
    valid_count: usize,
    min: f64,
    max: f64,
    mean: f64,
    std_dev: f64,
}

pub fn export(options: &ExportOptions) -> Result<()> {
    validate_options(options)?;

    let raster = open_raster(&options.input_base)?;
    let metadata = &raster.metadata;

    validate_raster(metadata)?;

    if let Some(rgb) = &options.rgb {
        export_rgb(options, metadata, rgb)
    } else {
        export_grayscale(options, metadata)
    }
}

fn export_rgb(options: &ExportOptions, metadata: &Metadata, rgb: &str) -> Result<()> {
    let rgb_names = parse_rgb_names(rgb)?;
    let rgb_indices = resolve_rgb_band_indices(metadata, &rgb_names)?;

    let red = read_f32_band(&options.input_base, rgb_indices[0])?;
    let green = read_f32_band(&options.input_base, rgb_indices[1])?;
    let blue = read_f32_band(&options.input_base, rgb_indices[2])?;

    let red_range = scale_range_for_band(metadata, rgb_indices[0], &red, options.byte_scale)?;
    let green_range = scale_range_for_band(metadata, rgb_indices[1], &green, options.byte_scale)?;
    let blue_range = scale_range_for_band(metadata, rgb_indices[2], &blue, options.byte_scale)?;

    let image = build_rgb8_image(
        &red,
        &green,
        &blue,
        options.byte_scale,
        [red_range, green_range, blue_range],
    )?;

    write_geotiff_rgb(
        Path::new(&options.output),
        metadata.general.sample_count,
        metadata.general.line_count,
        &image,
        metadata,
    )
}

fn export_grayscale(options: &ExportOptions, metadata: &Metadata) -> Result<()> {
    let band_index = match &options.band {
        Some(band_name) => resolve_band_index(metadata, band_name)?,
        None => {
            if metadata.general.band_count != 1 {
                return Err(Error::InvalidArgument(
                    "Single-band export without --rgb requires a one-band input or --band BAND_NAME"
                        .to_string(),
                ));
            }

            0
        }
    };

    let samples = read_f32_band(&options.input_base, band_index)?;
    let range = scale_range_for_band(metadata, band_index, &samples, options.byte_scale)?;
    let image = build_gray8_image(&samples, options.byte_scale, range)?;

    write_geotiff_gray(
        Path::new(&options.output),
        metadata.general.sample_count,
        metadata.general.line_count,
        &image,
        metadata,
    )
}

fn validate_options(options: &ExportOptions) -> Result<()> {
    if options.format != OutputFormat::GeoTiff {
        return Err(Error::UnsupportedFormat(
            "asf_export currently supports only --format geotiff".to_string(),
        ));
    }

    match options.byte_scale {
        ByteScale::Truncate | ByteScale::Sigma => {}
        ByteScale::None | ByteScale::MinMax => {
            return Err(Error::InvalidArgument(
                "asf_export currently supports only --byte truncate and --byte sigma".to_string(),
            ));
        }
    }

    if options.rgb.is_some() && options.band.is_some() {
        return Err(Error::InvalidArgument(
            "--rgb and --band cannot be used together".to_string(),
        ));
    }

    Ok(())
}

fn validate_raster(metadata: &Metadata) -> Result<()> {
    if metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "asf_export currently supports only REAL32 rasters, found {}",
            metadata.general.data_type.as_meta_str()
        )));
    }

    if metadata.general.line_count == 0 || metadata.general.sample_count == 0 {
        return Err(Error::InvalidInput(
            "Cannot export a raster with zero lines or samples".to_string(),
        ));
    }

    if metadata.georeferencing.is_none() {
        return Err(Error::InvalidInput(
            "GeoTIFF export requires projected georeferencing metadata. Run asf_geocode before asf_export."
                .to_string(),
        ));
    }

    Ok(())
}

fn parse_rgb_names(rgb: &str) -> Result<[String; 3]> {
    let names = rgb
        .split_whitespace()
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    if names.len() != 3 {
        return Err(Error::InvalidArgument(format!(
            "--rgb expects exactly three band names, found {} in {:?}",
            names.len(),
            rgb
        )));
    }

    Ok([names[0].clone(), names[1].clone(), names[2].clone()])
}

fn resolve_rgb_band_indices(metadata: &Metadata, rgb_names: &[String; 3]) -> Result<[usize; 3]> {
    Ok([
        resolve_band_index(metadata, &rgb_names[0])?,
        resolve_band_index(metadata, &rgb_names[1])?,
        resolve_band_index(metadata, &rgb_names[2])?,
    ])
}

fn resolve_band_index(metadata: &Metadata, requested_name: &str) -> Result<usize> {
    metadata
        .bands
        .iter()
        .position(|band| band.name.eq_ignore_ascii_case(requested_name))
        .ok_or_else(|| {
            let available = metadata
                .bands
                .iter()
                .map(|band| band.name.as_str())
                .collect::<Vec<_>>()
                .join(", ");

            Error::InvalidArgument(format!(
                "Requested band {:?} was not found. Available bands: {}",
                requested_name, available
            ))
        })
}

fn build_rgb8_image(
    red: &[f32],
    green: &[f32],
    blue: &[f32],
    byte_scale: ByteScale,
    ranges: [Option<ScaleRange>; 3],
) -> Result<Vec<u8>> {
    if red.len() != green.len() || red.len() != blue.len() {
        return Err(Error::InvalidInput(format!(
            "RGB bands have mismatched lengths: red={}, green={}, blue={}",
            red.len(),
            green.len(),
            blue.len()
        )));
    }

    let mut image = Vec::with_capacity(
        red.len()
            .checked_mul(3)
            .ok_or_else(|| Error::InvalidInput("RGB output size overflows usize".to_string()))?,
    );

    for index in 0..red.len() {
        image.push(scale_to_byte(red[index], byte_scale, ranges[0]));
        image.push(scale_to_byte(green[index], byte_scale, ranges[1]));
        image.push(scale_to_byte(blue[index], byte_scale, ranges[2]));
    }

    Ok(image)
}

fn build_gray8_image(
    samples: &[f32],
    byte_scale: ByteScale,
    range: Option<ScaleRange>,
) -> Result<Vec<u8>> {
    let mut image = Vec::with_capacity(samples.len());

    for sample in samples {
        image.push(scale_to_byte(*sample, byte_scale, range));
    }

    Ok(image)
}

fn scale_to_byte(value: f32, byte_scale: ByteScale, range: Option<ScaleRange>) -> u8 {
    match byte_scale {
        ByteScale::Truncate => truncate_to_byte(value),
        ByteScale::Sigma => sigma_to_byte(value, range),
        ByteScale::None | ByteScale::MinMax => 0,
    }
}

fn truncate_to_byte(value: f32) -> u8 {
    if !value.is_finite() || value < 0.0 {
        0
    } else if value > 255.0 {
        255
    } else {
        (value + 0.5) as u8
    }
}

fn sigma_to_byte(value: f32, range: Option<ScaleRange>) -> u8 {
    if !value.is_finite() {
        return 0;
    }

    let Some(range) = range else {
        return 0;
    };

    if range.max <= range.min {
        return 0;
    }

    let value = value as f64;

    if value <= range.min {
        0
    } else if value >= range.max {
        255
    } else {
        (((value - range.min) * 255.0) / (range.max - range.min)) as u8
    }
}

fn scale_range_for_band(
    metadata: &Metadata,
    band_index: usize,
    samples: &[f32],
    byte_scale: ByteScale,
) -> Result<Option<ScaleRange>> {
    match byte_scale {
        ByteScale::Truncate => Ok(None),
        ByteScale::Sigma => {
            let stats = metadata_stats_for_band(metadata, band_index)
                .unwrap_or_else(|| compute_sample_stats(samples, metadata.general.no_data));

            if stats.valid_count == 0 {
                return Ok(Some(ScaleRange { min: 0.0, max: 0.0 }));
            }

            let sigma_min = stats.mean - 2.0 * stats.std_dev;
            let sigma_max = stats.mean + 2.0 * stats.std_dev;

            Ok(Some(ScaleRange {
                min: sigma_min.max(stats.min),
                max: sigma_max.min(stats.max),
            }))
        }
        ByteScale::None | ByteScale::MinMax => Err(Error::InvalidArgument(
            "asf_export currently supports only --byte truncate and --byte sigma".to_string(),
        )),
    }
}

fn metadata_stats_for_band(metadata: &Metadata, band_index: usize) -> Option<SampleStats> {
    let statistics = metadata.statistics.as_ref()?;
    let band_stats = statistics.bands.get(band_index)?;

    Some(SampleStats {
        valid_count: band_stats.valid_count,
        min: band_stats.min,
        max: band_stats.max,
        mean: band_stats.mean,
        std_dev: band_stats.std_dev,
    })
}

fn compute_sample_stats(samples: &[f32], no_data: Option<f64>) -> SampleStats {
    let no_data = no_data.map(|value| value as f32);

    let mut valid_count = 0_usize;
    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;
    let mut sum = 0.0_f64;
    let mut sum_squares = 0.0_f64;

    for sample in samples {
        if !sample.is_finite() || no_data.is_some_and(|no_data| *sample == no_data) {
            continue;
        }

        let value = *sample as f64;

        valid_count += 1;
        min = min.min(value);
        max = max.max(value);
        sum += value;
        sum_squares += value * value;
    }

    if valid_count == 0 {
        return SampleStats {
            valid_count,
            min: 0.0,
            max: 0.0,
            mean: 0.0,
            std_dev: 0.0,
        };
    }

    let mean = sum / valid_count as f64;
    let variance = (sum_squares / valid_count as f64) - (mean * mean);

    SampleStats {
        valid_count,
        min,
        max,
        mean,
        std_dev: variance.max(0.0).sqrt(),
    }
}

fn write_geotiff_rgb(
    output: &Path,
    width: usize,
    height: usize,
    image: &[u8],
    metadata: &Metadata,
) -> Result<()> {
    let expected_len = width
        .checked_mul(height)
        .and_then(|pixels| pixels.checked_mul(3))
        .ok_or_else(|| Error::InvalidInput("GeoTIFF RGB output size overflows usize".to_string()))?;

    if image.len() != expected_len {
        return Err(Error::InvalidInput(format!(
            "GeoTIFF RGB buffer length mismatch: expected {}, found {}",
            expected_len,
            image.len()
        )));
    }

    let file = File::create(output)?;
    let mut encoder = TiffEncoder::new(file).map_err(tiff_error)?;

    let mut tiff_image = encoder
        .new_image::<colortype::RGB8>(width as u32, height as u32)
        .map_err(tiff_error)?;

    write_geotiff_tags(tiff_image.encoder(), metadata)?;
    tiff_image.write_data(image).map_err(tiff_error)?;

    Ok(())
}

fn write_geotiff_gray(
    output: &Path,
    width: usize,
    height: usize,
    image: &[u8],
    metadata: &Metadata,
) -> Result<()> {
    let expected_len = width
        .checked_mul(height)
        .ok_or_else(|| Error::InvalidInput("GeoTIFF grayscale output size overflows usize".to_string()))?;

    if image.len() != expected_len {
        return Err(Error::InvalidInput(format!(
            "GeoTIFF grayscale buffer length mismatch: expected {}, found {}",
            expected_len,
            image.len()
        )));
    }

    let file = File::create(output)?;
    let mut encoder = TiffEncoder::new(file).map_err(tiff_error)?;

    let mut tiff_image = encoder
        .new_image::<colortype::Gray8>(width as u32, height as u32)
        .map_err(tiff_error)?;

    write_geotiff_tags(tiff_image.encoder(), metadata)?;
    tiff_image.write_data(image).map_err(tiff_error)?;

    Ok(())
}

fn write_geotiff_tags<W, K>(
    encoder: &mut tiff::encoder::DirectoryEncoder<'_, W, K>,
    metadata: &Metadata,
) -> Result<()>
where
    W: Write + Seek,
    K: TiffKind,
{
    let georeferencing = metadata.georeferencing.as_ref().ok_or_else(|| {
        Error::InvalidInput(
            "GeoTIFF export requires projected georeferencing metadata. Run asf_geocode before asf_export."
                .to_string(),
        )
    })?;

    if let Some(affine) = georeferencing.affine {
        let transformation = [
            affine.pixel_x,
            affine.line_x,
            0.0_f64,
            affine.origin_x,
            affine.pixel_y,
            affine.line_y,
            0.0_f64,
            affine.origin_y,
            0.0_f64,
            0.0_f64,
            0.0_f64,
            0.0_f64,
            0.0_f64,
            0.0_f64,
            0.0_f64,
            1.0_f64,
        ];

        encoder
            .write_tag(Tag::Unknown(34264), &transformation[..])
            .map_err(tiff_error)?;
    } else {
        let pixel_scale = [
            georeferencing.pixel_size_x.abs(),
            georeferencing.pixel_size_y.abs(),
            0.0_f64,
        ];

        let tiepoint = [
            0.0_f64,
            0.0_f64,
            0.0_f64,
            georeferencing.origin_x,
            georeferencing.origin_y,
            0.0_f64,
        ];

        encoder
            .write_tag(Tag::Unknown(33550), &pixel_scale[..])
            .map_err(tiff_error)?;

        encoder
            .write_tag(Tag::Unknown(33922), &tiepoint[..])
            .map_err(tiff_error)?;
    }

    let geo_key_directory = geo_key_directory(georeferencing)?;

    encoder
        .write_tag(Tag::Unknown(34735), &geo_key_directory[..])
        .map_err(tiff_error)?;

    Ok(())
}

fn geo_key_directory(georeferencing: &GeoreferencingMetadata) -> Result<Vec<u16>> {
    match &georeferencing.projection {
        ProjectionMetadata::Utm { epsg, .. } => {
            let epsg = epsg.ok_or_else(|| {
                Error::InvalidInput(
                    "GeoTIFF export requires an EPSG code for UTM georeferencing".to_string(),
                )
            })?;

            Ok(vec![
                // GeoKeyDirectory header:
                // KeyDirectoryVersion, KeyRevision, MinorRevision, NumberOfKeys
                1, 1, 0, 4,
                // GTModelTypeGeoKey = ModelTypeProjected
                1024, 0, 1, 1,
                // GTRasterTypeGeoKey = RasterPixelIsArea
                1025, 0, 1, 1,
                // GeographicTypeGeoKey = GCS_WGS_84
                2048, 0, 1, 4326,
                // ProjectedCSTypeGeoKey = EPSG projected CRS, e.g. 32606
                3072, 0, 1, epsg,
            ])
        }
    }
}

fn tiff_error(error: tiff::TiffError) -> Error {
    Error::Message(format!("GeoTIFF write error: {error}"))
}
