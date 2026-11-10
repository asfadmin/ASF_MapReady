use anyhow::Result;
use clap::Parser;

use asf_adjust_bands::AdjustBandsOptions;

#[derive(Debug, Parser)]
#[command(name = "adjust_bands")]
#[command(about = "Select one band from ASF internal rasters")]
struct Cli {
    #[arg(long = "bands", value_name = "BAND_NAME")]
    bands: String,

    #[arg(value_name = "INPUT_BASE")]
    input_base: String,

    #[arg(value_name = "OUTPUT_BASE")]
    output_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let options = AdjustBandsOptions {
        input_base: cli.input_base,
        output_base: cli.output_base,
        bands: cli.bands,
    };

    asf_adjust_bands::adjust_bands(&options)?;

    Ok(())
}
