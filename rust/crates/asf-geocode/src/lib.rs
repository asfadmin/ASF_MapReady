use asf_core::Result;
use asf_raster::ResampleMethod;

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

pub fn geocode(_options: &GeocodeOptions) -> Result<()> {
    todo!("implement geocoding")
}
