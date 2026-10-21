use anyhow::Result;
use clap::{Parser, ValueEnum};

use asf_raster::ResampleMethod;
use asf_resample::ResampleOptions;

#[derive(Debug, Clone, Copy, ValueEnum)]
enum MethodArg {
    Nearest,
    Bilinear,
    Average,
    LogicalOr,
}

impl From<MethodArg> for ResampleMethod {
    fn from(value: MethodArg) -> Self {
        match value {
            MethodArg::Nearest => Self::Nearest,
            MethodArg::Bilinear => Self::Bilinear,
            MethodArg::Average => Self::Average,
            MethodArg::LogicalOr => Self::LogicalOr,
        }
    }
}

#[derive(Debug, Parser)]
#[command(name = "resample")]
#[command(about = "Resample ASF internal rasters")]
struct Cli {
    #[arg(long = "x-scale", default_value_t = 1.0)]
    x_scale: f64,

    #[arg(long = "y-scale", default_value_t = 1.0)]
    y_scale: f64,

    #[arg(long = "method", value_enum, default_value_t = MethodArg::Bilinear)]
    method: MethodArg,

    #[arg(value_name = "INPUT_BASE")]
    input_base: String,

    #[arg(value_name = "OUTPUT_BASE")]
    output_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let options = ResampleOptions {
        input_base: cli.input_base,
        output_base: cli.output_base,
        x_scale: cli.x_scale,
        y_scale: cli.y_scale,
        method: cli.method.into(),
    };

    asf_resample::resample(&options)?;

    Ok(())
}
