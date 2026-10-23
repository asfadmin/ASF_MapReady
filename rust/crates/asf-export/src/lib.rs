use std::fs::File;
use std::path::Path;

use asf_core::{Error, Result};
use asf_meta::{DataType, Metadata};
use asf_raster::{open_raster, read_f32_band};
use tiff::encoder::{colortype, TiffEncoder};

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

pub fn export(options: &ExportOptions) -> Result<()> {
    validate_options(options)?;

    let raster = open_raster(&options.input_base)?;
    let metadata = &raster.metadata;

    validate_raster(metadata)?;

    let rgb_names = parse_rgb_names(
        options
            .rgb
            .as_deref()
            .ok_or_else(|| Error::InvalidArgument("--rgb is required for GeoTIFF color export".to_string()))?,
    )?;

    let rgb_indices = resolve_rgb_band_indices(metadata, &rgb_names)?;

    let red = read_f32_band(&options.input_base, rgb_indices[0])?;
    let green = read_f32_band(&options.input_base, rgb_indices[1])?;
    let blue = read_f32_band(&options.input_base, rgb_indices[2])?;

    let image = build_rgb8_image(&red, &green, &blue, options.byte_scale)?;

    write_geotiff(
        Path::new(&options.output),
        metadata.general.sample_count,
        metadata.general.line_count,
        &image,
        metadata,
    )?;

    Ok(())
}

fn validate_options(options: &ExportOptions) -> Result<()> {
    if options.format != OutputFormat::GeoTiff {
        return Err(Error::UnsupportedFormat(
            "asf_export currently supports only --format geotiff".to_string(),
        ));
    }

    if options.byte_scale != ByteScale::Truncate {
        return Err(Error::InvalidArgument(
            "asf_export currently supports only --byte truncate".to_string(),
        ));
    }

    if options.band.is_some() {
        return Err(Error::InvalidArgument(
            "asf_export currently supports --rgb only; --band is not implemented".to_string(),
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
                "Requested RGB band {:?} was not found. Available bands: {}",
                requested_name, available
            ))
        })
}

fn build_rgb8_image(red: &[f32], green: &[f32], blue: &[f32], byte_scale: ByteScale) -> Result<Vec<u8>> {
    if red.len() != green.len() || red.len() != blue.len() {
        return Err(Error::InvalidInput(format!(
            "RGB bands have mismatched lengths: red={}, green={}, blue={}",
            red.len(),
            green.len(),
            blue.len()
        )));
    }

    match byte_scale {
        ByteScale::Truncate => {
            let mut image = Vec::with_capacity(
                red.len()
                    .checked_mul(3)
                    .ok_or_else(|| Error::InvalidInput("RGB output size overflows usize".to_string()))?,
            );

            for index in 0..red.len() {
                image.push(truncate_to_byte(red[index]));
                image.push(truncate_to_byte(green[index]));
                image.push(truncate_to_byte(blue[index]));
            }

            Ok(image)
        }
        ByteScale::None | ByteScale::MinMax | ByteScale::Sigma => Err(Error::InvalidArgument(
            "asf_export currently supports only --byte truncate".to_string(),
        )),
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

fn write_geotiff(
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

    // Keep this isolated so georeferencing tags can be added here soon without
    // changing band resolution, scaling, or RGB interleaving logic.
    write_geotiff_rgb_image(&mut encoder, width, height, image, metadata)
}

fn write_geotiff_rgb_image(
    encoder: &mut TiffEncoder<File>,
    width: usize,
    height: usize,
    image: &[u8],
    _metadata: &Metadata,
) -> Result<()> {
    let tiff_image = encoder
        .new_image::<colortype::RGB8>(width as u32, height as u32)
        .map_err(tiff_error)?;

    tiff_image.write_data(image).map_err(tiff_error)?;

    Ok(())
}

fn tiff_error(error: tiff::TiffError) -> Error {
    Error::Message(format!("GeoTIFF write error: {error}"))
}
