use asf_core::{Error, Result};
use asf_meta::DataType;
use asf_raster::{create_raster, open_raster, read_f32_band, write_f32_band};

#[derive(Debug, Clone)]
pub struct ResampleOptions {
    pub input_base: String,
    pub output_base: String,
    pub square_pixel_size: f64,
}

pub fn resample(options: &ResampleOptions) -> Result<()> {
    if !options.square_pixel_size.is_finite() || options.square_pixel_size <= 0.0 {
        return Err(Error::InvalidArgument(format!(
            "--square must be a positive finite pixel size, got {}",
            options.square_pixel_size
        )));
    }

    let input_raster = open_raster(&options.input_base)?;
    let input_metadata = &input_raster.metadata;

    if input_metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "resample currently supports only REAL32 rasters, found {}",
            input_metadata.general.data_type.as_meta_str()
        )));
    }

    if input_metadata.general.x_pixel_size <= 0.0 || input_metadata.general.y_pixel_size <= 0.0 {
        return Err(Error::InvalidInput(
            "Input pixel sizes must be positive".to_string(),
        ));
    }

    let x_scale = input_metadata.general.x_pixel_size / options.square_pixel_size;
    let y_scale = input_metadata.general.y_pixel_size / options.square_pixel_size;

    let output_sample_count = scaled_count(input_metadata.general.sample_count, x_scale)?;
    let output_line_count = scaled_count(input_metadata.general.line_count, y_scale)?;

    let mut output_metadata = input_metadata.clone();
    output_metadata.general.sample_count = output_sample_count;
    output_metadata.general.line_count = output_line_count;
    output_metadata.general.x_pixel_size = options.square_pixel_size;
    output_metadata.general.y_pixel_size = options.square_pixel_size;
    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;

    for band_index in 0..input_metadata.general.band_count {
        let input_band = read_f32_band(&options.input_base, band_index)?;
        let output_band = average_resample_band(
            &input_band,
            input_metadata.general.sample_count,
            input_metadata.general.line_count,
            output_sample_count,
            output_line_count,
            input_metadata.general.no_data.map(|value| value as f32),
        )?;

        write_f32_band(&options.output_base, band_index, &output_band)?;
    }

    Ok(())
}

fn scaled_count(input_count: usize, scale: f64) -> Result<usize> {
    if !scale.is_finite() || scale <= 0.0 {
        return Err(Error::InvalidInput(format!(
            "Invalid resampling scale factor: {scale}"
        )));
    }

    let output_count = ((input_count as f64) * scale).round();

    if output_count < 1.0 {
        return Ok(1);
    }

    if output_count > usize::MAX as f64 {
        return Err(Error::InvalidInput(format!(
            "Scaled dimension overflows usize: {output_count}"
        )));
    }

    Ok(output_count as usize)
}

fn average_resample_band(
    input: &[f32],
    input_width: usize,
    input_height: usize,
    output_width: usize,
    output_height: usize,
    no_data: Option<f32>,
) -> Result<Vec<f32>> {
    let expected_input_len = input_width
        .checked_mul(input_height)
        .ok_or_else(|| Error::InvalidInput("Input raster dimensions overflow usize".to_string()))?;

    if input.len() != expected_input_len {
        return Err(Error::InvalidInput(format!(
            "Input band expected {} samples, found {}",
            expected_input_len,
            input.len()
        )));
    }

    let output_len = output_width.checked_mul(output_height).ok_or_else(|| {
        Error::InvalidInput("Output raster dimensions overflow usize".to_string())
    })?;

    let fill_value = no_data.unwrap_or(0.0);
    let mut output = vec![fill_value; output_len];

    let x_ratio = input_width as f64 / output_width as f64;
    let y_ratio = input_height as f64 / output_height as f64;

    for output_y in 0..output_height {
        let input_y_start = output_y as f64 * y_ratio;
        let input_y_end = (output_y + 1) as f64 * y_ratio;

        let y_start = input_y_start.floor() as usize;
        let y_end = (input_y_end.ceil() as usize).min(input_height);

        for output_x in 0..output_width {
            let input_x_start = output_x as f64 * x_ratio;
            let input_x_end = (output_x + 1) as f64 * x_ratio;

            let x_start = input_x_start.floor() as usize;
            let x_end = (input_x_end.ceil() as usize).min(input_width);

            let mut sum = 0.0_f64;
            let mut valid_count = 0_usize;

            for input_y in y_start..y_end {
                let row_offset = input_y * input_width;

                for input_x in x_start..x_end {
                    let value = input[row_offset + input_x];

                    if !value.is_finite() {
                        continue;
                    }

                    if no_data.is_some_and(|mask| value == mask) {
                        continue;
                    }

                    sum += value as f64;
                    valid_count += 1;
                }
            }

            if valid_count > 0 {
                output[output_y * output_width + output_x] = (sum / valid_count as f64) as f32;
            }
        }
    }

    Ok(output)
}
