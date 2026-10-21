use asf_core::Result;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stretch {
    MinMax,
    Sigma,
}

#[derive(Debug, Clone)]
pub struct ColorBrowseOptions {
    pub input_base: String,
    pub output: String,
    pub rgb: Option<[String; 3]>,
    pub stretch: Stretch,
}

pub fn color_browse(_options: &ColorBrowseOptions) -> Result<()> {
    todo!("implement color browse generation")
}
