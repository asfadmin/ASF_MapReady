use anyhow::{bail, Result};
use clap::{Parser, ValueEnum};

use asf_geocode::{GeocodeOptions, Projection};
use asf_raster::ResampleMethod;

#[derive(Debug, Clone, Copy, ValueEnum)]
enum ProjectionArg {
    Latlon,
    Utm,
}

#[derive(Debug, Parser)]
#[command(name = "asf_geocode")]
#[command(about = "Geocode ASF internal rasters. Current Rust milestone is pass-through only.")]
struct Cli {
    #[arg(short = 'p', long = "projection", value_enum)]
    projection: ProjectionArg,

    #[arg(long = "zone")]
    zone: Option<u8>,

    #[arg(long = "south")]
    south: bool,

    #[arg(long = "pixel-size")]
    pixel_size: Option<f64>,

    #[arg(value_name = "INPUT_BASE")]
    input_base: String,

    #[arg(value_name = "OUTPUT_BASE")]
    output_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let projection = match cli.projection {
        ProjectionArg::Latlon => Projection::LatLon,
        ProjectionArg::Utm => {
            let Some(zone) = cli.zone else {
                bail!("--zone is required when using -p utm");
            };

            Projection::Utm {
                zone,
                north: !cli.south,
            }
        }
    };

    let options = GeocodeOptions {
        input_base: cli.input_base,
        output_base: cli.output_base,
        projection,
        pixel_size: cli.pixel_size,
        resample_method: ResampleMethod::Nearest,
        background: None,
        band: None,
    };

    asf_geocode::geocode(&options)?;

    Ok(())
}
