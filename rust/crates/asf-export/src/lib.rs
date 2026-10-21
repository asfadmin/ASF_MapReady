use asf_core::Result;

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
}

pub fn export(_options: &ExportOptions) -> Result<()> {
    todo!("implement export")
}
