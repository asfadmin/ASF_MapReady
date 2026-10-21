use std::path::Path;

use asf_core::Result;
use asf_meta::Metadata;

#[derive(Debug, Clone)]
pub struct Raster {
    pub metadata: Metadata,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResampleMethod {
    Nearest,
    Bilinear,
    Average,
    LogicalOr,
}

pub fn open_raster(_base_name: impl AsRef<Path>) -> Result<Raster> {
    todo!("open ASF .img + .meta raster")
}

pub fn create_raster(_base_name: impl AsRef<Path>, _metadata: &Metadata) -> Result<Raster> {
    todo!("create ASF .img + .meta raster")
}

pub fn read_f32_band(_base_name: impl AsRef<Path>, _band_index: usize) -> Result<Vec<f32>> {
    todo!("read one band as f32")
}

pub fn write_f32_band(
    _base_name: impl AsRef<Path>,
    _band_index: usize,
    _samples: &[f32],
) -> Result<()> {
    todo!("write one f32 band")
}
