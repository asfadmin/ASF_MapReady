use anyhow::{bail, Result};
use clap::{Parser, ValueEnum};

use asf_geocode::{GeocodeOptions, Projection};
use asf_raster::ResampleMethod;

#[derive(Debug, Clone, Copy, ValueEnum)]
enum ProjectionArg {
    Latlon,
    Utm,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum MethodArg {
    Nearest,
    Bilinear,
}

impl From<MethodArg> for ResampleMethod {
    fn from(value: MethodArg) -> Self {
        match value {
            MethodArg::Nearest => Self::Nearest,
            MethodArg::Bilinear => Self::Bilinear,
        }
    }
}

#[derive(Debug, Parser)]
#[command(name = "asf_geocode")]
#[command(about = "Geocode ASF internal rasters")]
struct Cli {
    #[arg(short = 'p', long = "projection", value_enum)]
    projection: ProjectionArg,

    #[arg(long = "zone")]
    zone: Option<u8>,

    #[arg(long = "south")]
    south: bool,

    #[arg(long = "pixel-size")]
    pixel_size: Option<f64>,

    #[arg(long = "resample-method", value_enum, default_value_t = MethodArg::Bilinear)]
    resample_method: MethodArg,

    #[arg(long = "background")]
    background: Option<f32>,

    #[arg(long = "band")]
    band: Option<String>,

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
        resample_method: cli.resample_method.into(),
        background: cli.background,
        band: cli.band,
    };

    asf_geocode::geocode(&options)?;

    Ok(())
}
