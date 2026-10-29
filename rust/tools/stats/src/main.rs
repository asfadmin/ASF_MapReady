use anyhow::Result;
use clap::Parser;

use asf_stats::StatsOptions;

#[derive(Debug, Parser)]
#[command(name = "stats")]
#[command(about = "Compute statistics for ASF internal rasters")]
struct Cli {
    #[arg(short = 'm', long = "mask", value_name = "VALUE")]
    mask: Option<f32>,

    #[arg(long = "overmeta")]
    overmeta: bool,

    #[arg(long = "nometa")]
    nometa: bool,

    #[arg(long = "overstat")]
    overstat: bool,

    #[arg(long = "nostat")]
    nostat: bool,

    #[arg(value_name = "INPUT_BASE")]
    input_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let options = StatsOptions {
        input_base: cli.input_base,
        mask: cli.mask,
        overmeta: cli.overmeta,
        nometa: cli.nometa,
        overstat: cli.overstat,
        nostat: cli.nostat,
    };

    let stats = asf_stats::compute_stats(&options)?;

    for band in stats.band_stats {
        println!("band: {}", band.band_name);
        println!("valid_count: {}", band.valid_count);
        println!("no_data_count: {}", band.no_data_count);
        println!("min: {}", band.min);
        println!("max: {}", band.max);
        println!("mean: {}", band.mean);
        println!("std_dev: {}", band.std_dev);
    }

    Ok(())
}
