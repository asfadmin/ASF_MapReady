use asf_core::Result;

#[derive(Debug, Clone)]
pub struct AdjustBandsOptions {
    pub input_base: String,
    pub output_base: String,
    pub bands: Vec<String>,
}

pub fn adjust_bands(_options: &AdjustBandsOptions) -> Result<()> {
    todo!("implement band selection/reordering")
}
