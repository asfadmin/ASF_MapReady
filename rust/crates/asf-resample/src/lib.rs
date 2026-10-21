use asf_core::Result;
use asf_raster::ResampleMethod;

#[derive(Debug, Clone)]
pub struct ResampleOptions {
    pub input_base: String,
    pub output_base: String,
    pub x_scale: f64,
    pub y_scale: f64,
    pub method: ResampleMethod,
}

pub fn resample(_options: &ResampleOptions) -> Result<()> {
    todo!("implement raster resampling")
}
