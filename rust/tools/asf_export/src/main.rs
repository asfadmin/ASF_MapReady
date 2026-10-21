use anyhow::Result;
use clap::{Parser, ValueEnum};

use asf_export::{ByteScale, ExportOptions, OutputFormat};

#[derive(Debug, Clone, Copy, ValueEnum)]
enum FormatArg {
    Geotiff,
    Tiff,
    Png,
    Jpeg,
}

impl From<FormatArg> for OutputFormat {
    fn from(value: FormatArg) -> Self {
        match value {
            FormatArg::Geotiff => Self::GeoTiff,
            FormatArg::Tiff => Self::Tiff,
            FormatArg::Png => Self::Png,
            FormatArg::Jpeg => Self::Jpeg,
        }
    }
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum ByteScaleArg {
    None,
    Truncate,
    Minmax,
    Sigma,
}

impl From<ByteScaleArg> for ByteScale {
    fn from(value: ByteScaleArg) -> Self {
        match value {
            ByteScaleArg::None => Self::None,
            ByteScaleArg::Truncate => Self::Truncate,
            ByteScaleArg::Minmax => Self::MinMax,
            ByteScaleArg::Sigma => Self::Sigma,
        }
    }
}

#[derive(Debug, Parser)]
#[command(name = "asf_export")]
#[command(about = "Export ASF internal rasters")]
struct Cli {
    #[arg(long = "format", short = 'f', value_enum, default_value_t = FormatArg::Geotiff)]
    format: FormatArg,

    #[arg(long = "byte", value_enum, default_value_t = ByteScaleArg::None)]
    byte_scale: ByteScaleArg,

    #[arg(long = "band")]
    band: Option<String>,

    #[arg(value_name = "INPUT_BASE")]
    input_base: String,

    #[arg(value_name = "OUTPUT")]
    output: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let options = ExportOptions {
        input_base: cli.input_base,
        output: cli.output,
        format: cli.format.into(),
        byte_scale: cli.byte_scale.into(),
        band: cli.band,
    };

    asf_export::export(&options)?;

    Ok(())
}
