use asf_core::Result;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct RasterStats {
    pub valid_count: usize,
    pub no_data_count: usize,
    pub min: f64,
    pub max: f64,
    pub mean: f64,
    pub std_dev: f64,
}

pub fn compute_stats(_input_base: &str, _band: Option<&str>) -> Result<RasterStats> {
    todo!("compute raster statistics")
}
