use asf_core::{Error, Result};
use asf_meta::DataType;
use asf_raster::{create_raster, open_raster, read_f32_band, write_f32_band};

#[derive(Debug, Clone)]
pub struct AdjustBandsOptions {
    pub input_base: String,
    pub output_base: String,
    pub bands: String,
}

pub fn adjust_bands(options: &AdjustBandsOptions) -> Result<()> {
    let requested_band = parse_single_band_name(&options.bands)?;

    let input_raster = open_raster(&options.input_base)?;
    let input_metadata = &input_raster.metadata;

    if input_metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "adjust_bands currently supports only REAL32 rasters, found {}",
            input_metadata.general.data_type.as_meta_str()
        )));
    }

    let Some(selected_band_index) = input_metadata
        .bands
        .iter()
        .position(|band| band.name.eq_ignore_ascii_case(requested_band))
    else {
        let available_bands = input_metadata
            .bands
            .iter()
            .map(|band| band.name.as_str())
            .collect::<Vec<_>>()
            .join(", ");

        return Err(Error::InvalidArgument(format!(
            "Unknown band name '{requested_band}'. Available bands: {available_bands}"
        )));
    };

    let selected_band = input_metadata.bands[selected_band_index].clone();

    let mut output_metadata = input_metadata.clone();
    output_metadata.general.band_count = 1;
    output_metadata.bands = vec![selected_band];
    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;

    let samples = read_f32_band(&options.input_base, selected_band_index)?;
    write_f32_band(&options.output_base, 0, &samples)?;

    Ok(())
}

fn parse_single_band_name(bands: &str) -> Result<&str> {
    let band_name = bands.trim();

    if band_name.is_empty() {
        return Err(Error::InvalidArgument(
            "--bands requires one band name".to_string(),
        ));
    }

    // Future extension: if Sentinel browse generation needs multiple selected bands,
    // extend this option to accept a comma-separated list while preserving --bands.
    if band_name.contains(',') {
        return Err(Error::InvalidArgument(
            "Multiple --bands values are not supported yet; provide exactly one band name"
                .to_string(),
        ));
    }

    if band_name.split_whitespace().count() != 1 {
        return Err(Error::InvalidArgument(
            "Multiple --bands values are not supported yet; provide exactly one band name"
                .to_string(),
        ));
    }

    Ok(band_name)
}
