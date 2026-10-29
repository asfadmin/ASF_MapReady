use asf_core::Result;
use asf_meta::{write_metadata, BandStatistics, RasterStatistics};
use asf_raster::{open_raster, read_f32_band};

#[derive(Debug, Clone, PartialEq)]
pub struct StatsOptions {
    pub input_base: String,
    pub mask: Option<f32>,
    pub overmeta: bool,
    pub nometa: bool,
    pub overstat: bool,
    pub nostat: bool,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct RasterStats {
    pub band_stats: Vec<BandStats>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct BandStats {
    pub band_name: String,
    pub valid_count: usize,
    pub no_data_count: usize,
    pub min: f64,
    pub max: f64,
    pub mean: f64,
    pub std_dev: f64,
}

pub fn compute_stats(options: &StatsOptions) -> Result<RasterStats> {
    let mut raster = open_raster(&options.input_base)?;

    let metadata_no_data = raster.metadata.general.no_data.map(|value| value as f32);
    let effective_mask = options.mask.or(metadata_no_data);

    let mut band_stats = Vec::with_capacity(raster.metadata.general.band_count);

    for band_index in 0..raster.metadata.general.band_count {
        let samples = read_f32_band(&options.input_base, band_index)?;

        let band_name = raster
            .metadata
            .bands
            .get(band_index)
            .map(|band| band.name.clone())
            .unwrap_or_else(|| format!("BAND{}", band_index + 1));

        band_stats.push(compute_band_stats(band_name, samples, effective_mask));
    }

    let stats = RasterStats { band_stats };

    if options.overmeta && !options.nometa {
        raster.metadata.statistics = Some(RasterStatistics {
            bands: stats
                .band_stats
                .iter()
                .map(|band| BandStatistics {
                    name: band.band_name.clone(),
                    valid_count: band.valid_count,
                    no_data_count: band.no_data_count,
                    min: band.min,
                    max: band.max,
                    mean: band.mean,
                    std_dev: band.std_dev,
                })
                .collect(),
        });

        write_metadata(&options.input_base, &raster.metadata)?;
    }

    if options.overstat && !options.nostat {
        // TODO:
        // Write a .stat file if a downstream Sentinel browse step needs it.
        //
        // For now, Sentinel browse requires stats --mask and --overmeta behavior,
        // so .meta stats writing is implemented first.
    }

    Ok(stats)
}

fn compute_band_stats(band_name: String, samples: Vec<f32>, mask: Option<f32>) -> BandStats {
    let mut valid_count = 0_usize;
    let mut no_data_count = 0_usize;
    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;
    let mut sum = 0.0_f64;
    let mut sum_squares = 0.0_f64;

    for sample in samples {
        if !sample.is_finite() || mask.is_some_and(|mask| sample == mask) {
            no_data_count += 1;
            continue;
        }

        let value = sample as f64;

        valid_count += 1;
        min = min.min(value);
        max = max.max(value);
        sum += value;
        sum_squares += value * value;
    }

    if valid_count == 0 {
        return BandStats {
            band_name,
            valid_count,
            no_data_count,
            min: 0.0,
            max: 0.0,
            mean: 0.0,
            std_dev: 0.0,
        };
    }

    let mean = sum / valid_count as f64;
    let variance = (sum_squares / valid_count as f64) - (mean * mean);
    let std_dev = variance.max(0.0).sqrt();

    BandStats {
        band_name,
        valid_count,
        no_data_count,
        min,
        max,
        mean,
        std_dev,
    }
}
