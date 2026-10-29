use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

use asf_color_browse::ColorBrowseOptions;

#[derive(Debug, Parser)]
#[command(name = "color_browse")]
#[command(about = "Generate a narrow Sentinel color browse intermediate from ASF internal rasters")]
struct Cli {
    #[arg(long = "noise-cal", value_name = "DB_THRESHOLD", allow_hyphen_values = true)]
    noise_cal: f32,

    #[arg(long = "tmpDir", value_name = "DIR")]
    tmp_dir: Option<PathBuf>,

    #[arg(value_name = "CO_POL_INPUT_BASE")]
    co_pol_input_base: String,

    #[arg(value_name = "CROSS_POL_INPUT_BASE")]
    cross_pol_input_base: String,

    #[arg(value_name = "OUTPUT_BASE")]
    output_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let options = ColorBrowseOptions {
        co_pol_input_base: cli.co_pol_input_base,
        cross_pol_input_base: cli.cross_pol_input_base,
        output_base: cli.output_base,
        noise_cal_db: cli.noise_cal,
        tmp_dir: cli.tmp_dir,
    };

    asf_color_browse::color_browse(&options)?;

    Ok(())
}
