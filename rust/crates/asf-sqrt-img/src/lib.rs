use asf_core::{Error, Result};
use asf_meta::DataType;
use asf_raster::{create_raster, open_raster, read_f32_band, write_f32_band};

#[derive(Debug, Clone)]
pub struct SqrtImgOptions {
    pub input_base: String,
    pub output_base: String,
}

pub fn sqrt_img(options: &SqrtImgOptions) -> Result<()> {
    let input_raster = open_raster(&options.input_base)?;
    let input_metadata = &input_raster.metadata;

    if input_metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "sqrt_img currently supports only REAL32 rasters, found {}",
            input_metadata.general.data_type.as_meta_str()
        )));
    }

    let mut output_metadata = input_metadata.clone();

    // Pixel values change, so any previous statistics no longer describe the
    // output raster.
    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;

    for band_index in 0..input_metadata.general.band_count {
        let mut samples = read_f32_band(&options.input_base, band_index)?;

        for sample in &mut samples {
            if !sample.is_finite() || *sample < 0.0 {
                *sample = 0.0;
            } else {
                *sample = sample.sqrt();
            }
        }

        write_f32_band(&options.output_base, band_index, &samples)?;
    }

    Ok(())
}
