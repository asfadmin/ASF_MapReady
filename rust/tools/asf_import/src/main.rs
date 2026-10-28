use std::str::FromStr;

use anyhow::Result;
use clap::{ArgGroup, Parser};

use asf_import::{ImportOptions, InputFormat, Radiometry};

#[derive(Debug, Parser)]
#[command(name = "asf_import")]
#[command(about = "Import imagery into ASF internal format. Currently supports Sentinel only.")]
#[command(group(
    ArgGroup::new("radiometry")
        .args(["amplitude", "sigma", "beta", "gamma", "power"])
        .multiple(false)
))]
struct Cli {
    #[arg(short = 'f', long = "format", default_value = "sentinel")]
    format: String,

    #[arg(long = "amplitude")]
    amplitude: bool,

    #[arg(long = "sigma")]
    sigma: bool,

    #[arg(long = "beta")]
    beta: bool,

    #[arg(long = "gamma")]
    gamma: bool,

    #[arg(long = "power")]
    power: bool,

    #[arg(long = "db")]
    db: bool,

    #[arg(value_name = "INPUT")]
    input: String,

    #[arg(value_name = "OUTPUT_BASE")]
    output_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let format = InputFormat::from_str(&cli.format)?;
    let radiometry = radiometry_from_cli(&cli)?;

    let options = ImportOptions {
        format,
        input: cli.input,
        output_base: cli.output_base,
        radiometry,
    };

    asf_import::import(&options)?;

    Ok(())
}

fn radiometry_from_cli(cli: &Cli) -> Result<Radiometry> {
    let radiometry = if cli.sigma {
        if cli.db {
            Radiometry::SigmaDb
        } else {
            Radiometry::Sigma
        }
    } else if cli.beta {
        if cli.db {
            Radiometry::BetaDb
        } else {
            Radiometry::Beta
        }
    } else if cli.gamma {
        if cli.db {
            Radiometry::GammaDb
        } else {
            Radiometry::Gamma
        }
    } else if cli.power {
        if cli.db {
            anyhow::bail!("--db can only be used with --sigma, --beta, or --gamma");
        }

        Radiometry::Power
    } else {
        if cli.db {
            anyhow::bail!("--db can only be used with --sigma, --beta, or --gamma");
        }

        Radiometry::Amplitude
    };

    Ok(radiometry)
}
