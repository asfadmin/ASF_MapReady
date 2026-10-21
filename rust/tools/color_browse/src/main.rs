use anyhow::{bail, Result};
use clap::{Parser, ValueEnum};

use asf_color_browse::{ColorBrowseOptions, Stretch};

#[derive(Debug, Clone, Copy, ValueEnum)]
enum StretchArg {
    Minmax,
    Sigma,
}

impl From<StretchArg> for Stretch {
    fn from(value: StretchArg) -> Self {
        match value {
            StretchArg::Minmax => Self::MinMax,
            StretchArg::Sigma => Self::Sigma,
        }
    }
}

#[derive(Debug, Parser)]
#[command(name = "color_browse")]
#[command(about = "Generate color browse images from ASF internal rasters")]
struct Cli {
    #[arg(long = "rgb", value_delimiter = ',', num_args = 3)]
    rgb: Option<Vec<String>>,

    #[arg(long = "stretch", value_enum, default_value_t = StretchArg::Sigma)]
    stretch: StretchArg,

    #[arg(value_name = "INPUT_BASE")]
    input_base: String,

    #[arg(value_name = "OUTPUT")]
    output: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let rgb = match cli.rgb {
        Some(values) => {
            if values.len() != 3 {
                bail!("--rgb requires exactly three comma-separated band names");
            }

            Some([values[0].clone(), values[1].clone(), values[2].clone()])
        }
        None => None,
    };

    let options = ColorBrowseOptions {
        input_base: cli.input_base,
        output: cli.output,
        rgb,
        stretch: cli.stretch.into(),
    };

    asf_color_browse::color_browse(&options)?;

    Ok(())
}
