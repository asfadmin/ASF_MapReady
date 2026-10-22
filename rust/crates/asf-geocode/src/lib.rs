use asf_core::{Error, Result};
use asf_meta::DataType;
use asf_raster::{create_raster, open_raster, read_f32_band, write_f32_band, ResampleMethod};

#[derive(Debug, Clone)]
pub struct GeocodeOptions {
    pub input_base: String,
    pub output_base: String,
    pub projection: Projection,
    pub pixel_size: Option<f64>,
    pub resample_method: ResampleMethod,
    pub background: Option<f32>,
    pub band: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Projection {
    LatLon,
    Utm { zone: u8, north: bool },
}

pub fn geocode(options: &GeocodeOptions) -> Result<()> {
    validate_options(options)?;

    let input_raster = open_raster(&options.input_base)?;
    let input_metadata = &input_raster.metadata;

    if input_metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "asf_geocode pass-through currently supports only REAL32 rasters, found {}",
            input_metadata.general.data_type.as_meta_str()
        )));
    }

    let mut output_metadata = input_metadata.clone();

    if let Some(pixel_size) = options.pixel_size {
        output_metadata.general.x_pixel_size = pixel_size;
        output_metadata.general.y_pixel_size = pixel_size;
    }

    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;

    for band_index in 0..input_metadata.general.band_count {
        let samples = read_f32_band(&options.input_base, band_index)?;
        write_f32_band(&options.output_base, band_index, &samples)?;
    }

    Ok(())
}

fn validate_options(options: &GeocodeOptions) -> Result<()> {
    if let Some(pixel_size) = options.pixel_size {
        if !pixel_size.is_finite() || pixel_size <= 0.0 {
            return Err(Error::InvalidArgument(format!(
                "--pixel-size must be a positive finite value, got {pixel_size}"
            )));
        }
    }

    if options.background.is_some() {
        return Err(Error::InvalidArgument(
            "--background is not supported by the current pass-through asf_geocode milestone"
                .to_string(),
        ));
    }

    if options.band.is_some() {
        return Err(Error::InvalidArgument(
            "--band is not supported by the current pass-through asf_geocode milestone".to_string(),
        ));
    }

    match &options.projection {
        Projection::LatLon => {}
        Projection::Utm { zone, .. } => {
            if !(1..=60).contains(zone) {
                return Err(Error::InvalidArgument(format!(
                    "UTM zone must be in the range 1..=60, got {zone}"
                )));
            }
        }
    }

    match options.resample_method {
        ResampleMethod::Nearest | ResampleMethod::Bilinear => {}
        ResampleMethod::Average | ResampleMethod::LogicalOr => {
            return Err(Error::InvalidArgument(format!(
                "Unsupported asf_geocode resample method for pass-through milestone: {:?}",
                options.resample_method
            )));
        }
    }

    Ok(())
}
