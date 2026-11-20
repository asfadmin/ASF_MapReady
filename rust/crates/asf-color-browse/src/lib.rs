use std::f32::consts::PI;
use std::path::PathBuf;

use asf_core::{Error, Result};
use asf_meta::{BandMetadata, DataType};
use asf_raster::{create_raster, open_raster, read_f32_band, write_f32_band};

#[derive(Debug, Clone)]
pub struct ColorBrowseOptions {
    pub co_pol_input_base: String,
    pub cross_pol_input_base: String,
    pub output_base: String,
    pub noise_cal_db: f32,
    pub tmp_dir: Option<PathBuf>,
}

pub fn color_browse(options: &ColorBrowseOptions) -> Result<()> {
    if let Some(tmp_dir) = &options.tmp_dir {
        if !tmp_dir.is_dir() {
            return Err(Error::InvalidArgument(format!(
                "--tmpDir must name an existing directory, found {}",
                tmp_dir.display()
            )));
        }
    }

    let co_pol_raster = open_raster(&options.co_pol_input_base)?;
    let cross_pol_raster = open_raster(&options.cross_pol_input_base)?;

    validate_single_band_real32("first input", &co_pol_raster.metadata)?;
    validate_single_band_real32("second input", &cross_pol_raster.metadata)?;
    validate_compatible_inputs(&co_pol_raster.metadata, &cross_pol_raster.metadata)?;

    let co_pol = read_f32_band(&options.co_pol_input_base, 0)?;
    let cross_pol = read_f32_band(&options.cross_pol_input_base, 0)?;

    let pixel_count = co_pol.len();
    if cross_pol.len() != pixel_count {
        return Err(Error::InvalidInput(format!(
            "Input raster sample counts differ: first input has {} samples, second input has {} samples",
            pixel_count,
            cross_pol.len()
        )));
    }

    let mut ps = vec![0.0_f32; pixel_count];
    let mut pv = vec![0.0_f32; pixel_count];
    let mut pd = vec![0.0_f32; pixel_count];

    apply_noise_cal_decomposition(options.noise_cal_db, &co_pol, &cross_pol, &mut ps, &mut pv, &mut pd);

    let mut output_metadata = co_pol_raster.metadata.clone();
    output_metadata.general.band_count = 3;
    output_metadata.general.data_type = DataType::F32;
    output_metadata.bands = vec![
        BandMetadata {
            name: "Ps".to_string(),
        },
        BandMetadata {
            name: "Pv".to_string(),
        },
        BandMetadata {
            name: "Pd".to_string(),
        },
    ];
    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;
    write_f32_band(&options.output_base, 0, &ps)?;
    write_f32_band(&options.output_base, 1, &pv)?;
    write_f32_band(&options.output_base, 2, &pd)?;

    Ok(())
}

fn validate_single_band_real32(label: &str, metadata: &asf_meta::Metadata) -> Result<()> {
    if metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "color_browse currently supports only REAL32 rasters for {label}, found {}",
            metadata.general.data_type.as_meta_str()
        )));
    }

    if metadata.general.band_count != 1 || metadata.bands.len() != 1 {
        return Err(Error::InvalidInput(format!(
            "color_browse --noise-cal currently expects {label} to be a single-band raster, found band_count={} and {} band names",
            metadata.general.band_count,
            metadata.bands.len()
        )));
    }

    Ok(())
}

fn validate_compatible_inputs(
    first: &asf_meta::Metadata,
    second: &asf_meta::Metadata,
) -> Result<()> {
    if first.general.line_count != second.general.line_count
        || first.general.sample_count != second.general.sample_count
    {
        return Err(Error::InvalidInput(format!(
            "color_browse inputs must have matching dimensions: first input is {}x{}, second input is {}x{}",
            first.general.line_count,
            first.general.sample_count,
            second.general.line_count,
            second.general.sample_count
        )));
    }

    if first.general.x_pixel_size != second.general.x_pixel_size
        || first.general.y_pixel_size != second.general.y_pixel_size
    {
        return Err(Error::InvalidInput(format!(
            "color_browse inputs must have matching pixel sizes: first input is {}x{}, second input is {}x{}",
            first.general.x_pixel_size,
            first.general.y_pixel_size,
            second.general.x_pixel_size,
            second.general.y_pixel_size
        )));
    }

    Ok(())
}

fn apply_noise_cal_decomposition(
    threshold_db: f32,
    co_pol: &[f32],
    cross_pol: &[f32],
    ps: &mut [f32],
    pv: &mut [f32],
    pd: &mut [f32],
) {
    let threshold_linear = 10.0_f32.powf(threshold_db / 10.0);

    for index in 0..co_pol.len() {
        if !co_pol[index].is_finite() || !cross_pol[index].is_finite() {
            ps[index] = 0.0;
            pv[index] = 0.0;
            pd[index] = 0.0;
            continue;
        }

        let cp = if co_pol[index] > 0.0 {
            co_pol[index]
        } else {
            0.0
        };

        let xp = if cross_pol[index] > 0.0 {
            cross_pol[index]
        } else {
            0.0
        };

        let blue_mask = if threshold_linear > xp { 1.0 } else { 0.0 };

        let zp = if cp > xp {
            (cp - xp).sqrt().atan() * 2.0 / PI
        } else {
            0.0
        };

        let rp = if cp > 3.0 * xp {
            (cp - 3.0 * xp).sqrt()
        } else {
            0.0
        };

        let bp = 0.0_f32;

        ps[index] = (2.0 * rp * (1.0 - blue_mask) + zp * blue_mask) * 255.0;
        pv[index] = (3.0 * xp.sqrt() * (1.0 - blue_mask) + 2.0 * zp * blue_mask) * 255.0;
        pd[index] = (2.0 * bp * (1.0 - blue_mask) + 5.0 * zp * blue_mask) * 255.0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn noise_cal_decomposition_uses_threshold_as_cross_pol_mask() {
        let co_pol = vec![4.0_f32, 4.0_f32];
        let cross_pol = vec![0.001_f32, 1.0_f32];

        let mut ps = vec![0.0_f32; 2];
        let mut pv = vec![0.0_f32; 2];
        let mut pd = vec![0.0_f32; 2];

        apply_noise_cal_decomposition(-23.5, &co_pol, &cross_pol, &mut ps, &mut pv, &mut pd);

        assert!(ps[0] > 0.0);
        assert!(pv[0] > 0.0);
        assert!(pd[0] > 0.0);

        assert!(ps[1] > 0.0);
        assert!(pv[1] > 0.0);
        assert_eq!(pd[1], 0.0);
    }

    #[test]
    fn noise_cal_decomposition_writes_zero_for_non_finite_samples() {
        let co_pol = vec![f32::NAN];
        let cross_pol = vec![1.0_f32];

        let mut ps = vec![1.0_f32];
        let mut pv = vec![1.0_f32];
        let mut pd = vec![1.0_f32];

        apply_noise_cal_decomposition(-23.5, &co_pol, &cross_pol, &mut ps, &mut pv, &mut pd);

        assert_eq!(ps, vec![0.0]);
        assert_eq!(pv, vec![0.0]);
        assert_eq!(pd, vec![0.0]);
    }
}
