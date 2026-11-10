use anyhow::Result;
use clap::Parser;

use asf_resample::ResampleOptions;

#[derive(Debug, Parser)]
#[command(name = "resample")]
#[command(about = "Resample ASF internal rasters")]
struct Cli {
    #[arg(long = "square", value_name = "PIXEL_SIZE")]
    square_pixel_size: f64,

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
        square_pixel_size: cli.square_pixel_size,
    };

    asf_resample::resample(&options)?;

    Ok(())
}
