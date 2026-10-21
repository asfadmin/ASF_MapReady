use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
#[command(name = "stats")]
#[command(about = "Compute statistics for ASF internal rasters")]
struct Cli {
    #[arg(long = "band")]
    band: Option<String>,

    #[arg(value_name = "INPUT_BASE")]
    input_base: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let stats = asf_stats::compute_stats(&cli.input_base, cli.band.as_deref())?;

    println!("valid_count: {}", stats.valid_count);
    println!("no_data_count: {}", stats.no_data_count);
    println!("min: {}", stats.min);
    println!("max: {}", stats.max);
    println!("mean: {}", stats.mean);
    println!("std_dev: {}", stats.std_dev);

    Ok(())
}
