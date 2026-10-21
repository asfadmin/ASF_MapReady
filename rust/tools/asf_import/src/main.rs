use std::str::FromStr;

use anyhow::Result;
use clap::Parser;

use asf_import::{ImportOptions, InputFormat};

#[derive(Debug, Parser)]
#[command(name = "asf_import")]
#[command(about = "Import imagery into ASF internal format. Currently supports Sentinel only.")]
struct Cli {
    #[arg(short = 'f', long = "format", default_value = "sentinel")]
    format: String,

    #[arg(value_name = "INPUT")]
    input: String,

    #[arg(value_name = "OUTPUT_BASE")]
    output_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let format = InputFormat::from_str(&cli.format)?;

    let options = ImportOptions {
        format,
        input: cli.input,
        output_base: cli.output_base,
    };

    asf_import::import(&options)?;

    Ok(())
}
