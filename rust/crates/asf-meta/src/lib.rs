use std::fmt::Write as _;
use std::fs;
use std::path::Path;

use asf_core::{path::metadata_path, Result};

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    pub general: GeneralMetadata,
    pub bands: Vec<BandMetadata>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GeneralMetadata {
    pub line_count: usize,
    pub sample_count: usize,
    pub band_count: usize,
    pub data_type: DataType,
    pub x_pixel_size: f64,
    pub y_pixel_size: f64,
    pub no_data: Option<f64>,
    pub sensor: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BandMetadata {
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataType {
    U8,
    I16,
    I32,
    F32,
    F64,
    ComplexF32,
}

impl DataType {
    pub fn byte_size(self) -> usize {
        match self {
            Self::U8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::F32 => 4,
            Self::F64 => 8,
            Self::ComplexF32 => 8,
        }
    }

    pub fn as_meta_str(self) -> &'static str {
        match self {
            Self::U8 => "ASF_BYTE",
            Self::I16 => "INTEGER16",
            Self::I32 => "INTEGER32",
            Self::F32 => "REAL32",
            Self::F64 => "REAL64",
            Self::ComplexF32 => "COMPLEX_REAL32",
        }
    }
}

pub fn read_metadata(_base_name: impl AsRef<Path>) -> Result<Metadata> {
    todo!("parse ASF .meta files")
}

pub fn write_metadata(base_name: impl AsRef<Path>, metadata: &Metadata) -> Result<()> {
    let path = metadata_path(base_name);

    let band_names = metadata
        .bands
        .iter()
        .map(|band| band.name.as_str())
        .collect::<Vec<_>>()
        .join(",");

    let mut text = String::new();

    writeln!(text, "meta_parameters {{").unwrap();
    writeln!(text, "  general {{").unwrap();
    writeln!(text, "    line_count: {}", metadata.general.line_count).unwrap();
    writeln!(text, "    sample_count: {}", metadata.general.sample_count).unwrap();
    writeln!(text, "    band_count: {}", metadata.general.band_count).unwrap();
    writeln!(text, "    bands: \"{}\"", band_names).unwrap();
    writeln!(
        text,
        "    data_type: {}",
        metadata.general.data_type.as_meta_str()
    )
    .unwrap();
    writeln!(text, "    x_pixel_size: {}", metadata.general.x_pixel_size).unwrap();
    writeln!(text, "    y_pixel_size: {}", metadata.general.y_pixel_size).unwrap();

    if let Some(no_data) = metadata.general.no_data {
        writeln!(text, "    no_data: {}", no_data).unwrap();
    }

    if let Some(sensor) = &metadata.general.sensor {
        writeln!(text, "    sensor: \"{}\"", sensor).unwrap();
    }

    writeln!(text, "  }}").unwrap();
    writeln!(text, "}}").unwrap();

    fs::write(path, text)?;

    Ok(())
}
