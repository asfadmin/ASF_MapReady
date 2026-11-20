use anyhow::Result;
use clap::Parser;

use asf_sqrt_img::SqrtImgOptions;

#[derive(Debug, Parser)]
#[command(name = "sqrt_img")]
#[command(about = "Apply square root to all samples in an ASF internal REAL32 raster")]
struct Cli {
    #[arg(value_name = "INPUT_BASE")]
    input_base: String,

    #[arg(value_name = "OUTPUT_BASE")]
    output_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let options = SqrtImgOptions {
        input_base: cli.input_base,
        output_base: cli.output_base,
    };

    asf_sqrt_img::sqrt_img(&options)?;

    Ok(())
}
