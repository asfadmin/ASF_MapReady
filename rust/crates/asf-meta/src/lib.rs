use std::fmt::Write as _;
use std::fs;
use std::path::Path;

use asf_core::{path::metadata_path, Error, Result};

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    pub general: GeneralMetadata,
    pub bands: Vec<BandMetadata>,
    pub statistics: Option<RasterStatistics>,
    pub source_geolocation: Option<SourceGeolocationMetadata>,
    pub georeferencing: Option<GeoreferencingMetadata>,
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

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SourceGeolocationMetadata {
    pub points: Vec<GeolocationGridPoint>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GeolocationGridPoint {
    pub line: usize,
    pub pixel: usize,
    pub latitude: f64,
    pub longitude: f64,
    pub height: Option<f64>,
    pub incidence_angle: Option<f64>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GeoreferencingMetadata {
    pub projection: ProjectionMetadata,
    pub pixel_size_x: f64,
    pub pixel_size_y: f64,
    pub origin_x: f64,
    pub origin_y: f64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectionMetadata {
    Utm {
        zone: u8,
        hemisphere: Hemisphere,
        epsg: Option<u16>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hemisphere {
    North,
    South,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct RasterStatistics {
    pub bands: Vec<BandStatistics>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct BandStatistics {
    pub name: String,
    pub valid_count: usize,
    pub no_data_count: usize,
    pub min: f64,
    pub max: f64,
    pub mean: f64,
    pub std_dev: f64,
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

    fn from_meta_str(value: &str) -> Result<Self> {
        match value.trim() {
            "ASF_BYTE" => Ok(Self::U8),
            "INTEGER16" => Ok(Self::I16),
            "INTEGER32" => Ok(Self::I32),
            "REAL32" => Ok(Self::F32),
            "REAL64" => Ok(Self::F64),
            "COMPLEX_REAL32" => Ok(Self::ComplexF32),
            other => Err(Error::UnsupportedDataType(other.to_string())),
        }
    }
}

pub fn read_metadata(base_name: impl AsRef<Path>) -> Result<Metadata> {
    let path = metadata_path(base_name);
    let text = fs::read_to_string(&path)?;

    let mut line_count = None;
    let mut sample_count = None;
    let mut band_count = None;
    let mut band_names = None;
    let mut data_type = None;
    let mut x_pixel_size = None;
    let mut y_pixel_size = None;
    let mut no_data = None;
    let mut sensor = None;

    let mut statistics_bands = Vec::new();
    let mut current_stats_band: Option<BandStatistics> = None;
    let mut in_statistics = false;
    let mut in_stats_band = false;

    let mut source_geolocation_points = Vec::new();
    let mut current_geolocation_point: Option<GeolocationGridPoint> = None;
    let mut in_source_geolocation = false;
    let mut in_geolocation_point = false;

    let mut georeferencing_projection = None;
    let mut georeferencing_utm_zone = None;
    let mut georeferencing_hemisphere = None;
    let mut georeferencing_epsg = None;
    let mut georeferencing_pixel_size_x = None;
    let mut georeferencing_pixel_size_y = None;
    let mut georeferencing_origin_x = None;
    let mut georeferencing_origin_y = None;
    let mut in_georeferencing = false;

    for line in text.lines() {
        let trimmed = line.trim();

                match trimmed {
            "statistics {" => {
                in_statistics = true;
                continue;
            }
            "band {" if in_statistics => {
                in_stats_band = true;
                current_stats_band = Some(BandStatistics::default());
                continue;
            }
            "}" if in_stats_band => {
                in_stats_band = false;

                if let Some(band) = current_stats_band.take() {
                    statistics_bands.push(band);
                }

                continue;
            }
            "}" if in_statistics => {
                in_statistics = false;
                continue;
            }
            "source_geolocation {" => {
                in_source_geolocation = true;
                continue;
            }
            "point {" if in_source_geolocation => {
                in_geolocation_point = true;
                current_geolocation_point = Some(GeolocationGridPoint {
                    line: 0,
                    pixel: 0,
                    latitude: 0.0,
                    longitude: 0.0,
                    height: None,
                    incidence_angle: None,
                });
                continue;
            }
            "}" if in_geolocation_point => {
                in_geolocation_point = false;

                if let Some(point) = current_geolocation_point.take() {
                    source_geolocation_points.push(point);
                }

                continue;
            }
            "}" if in_source_geolocation => {
                in_source_geolocation = false;
                continue;
            }
            "georeferencing {" => {
                in_georeferencing = true;
                continue;
            }
            "}" if in_georeferencing => {
                in_georeferencing = false;
                continue;
            }
            _ => {}
        }

        let Some((key, value)) = trimmed.split_once(':') else {
            continue;
        };

        let key = key.trim();
        let value = value.trim();

        if in_stats_band {
            if let Some(band) = &mut current_stats_band {
                match key {
                    "name" => band.name = parse_string_field(value),
                    "valid_count" => band.valid_count = parse_usize_field(key, value)?,
                    "no_data_count" => band.no_data_count = parse_usize_field(key, value)?,
                    "min" => band.min = parse_f64_field(key, value)?,
                    "max" => band.max = parse_f64_field(key, value)?,
                    "mean" => band.mean = parse_f64_field(key, value)?,
                    "std_dev" => band.std_dev = parse_f64_field(key, value)?,
                    _ => {}
                }
            }

            continue;
        }
        if in_geolocation_point {
            if let Some(point) = &mut current_geolocation_point {
                match key {
                    "line" => point.line = parse_usize_field(key, value)?,
                    "pixel" => point.pixel = parse_usize_field(key, value)?,
                    "latitude" => point.latitude = parse_f64_field(key, value)?,
                    "longitude" => point.longitude = parse_f64_field(key, value)?,
                    "height" => point.height = Some(parse_f64_field(key, value)?),
                    "incidence_angle" => point.incidence_angle = Some(parse_f64_field(key, value)?),
                    _ => {}
                }
            }

            continue;
        }

        if in_georeferencing {
            match key {
                "projection" => georeferencing_projection = Some(parse_string_field(value)),
                "utm_zone" => georeferencing_utm_zone = Some(parse_u8_field(key, value)?),
                "hemisphere" => georeferencing_hemisphere = Some(parse_hemisphere_field(value)?),
                "epsg" => georeferencing_epsg = Some(parse_u16_field(key, value)?),
                "pixel_size_x" => georeferencing_pixel_size_x = Some(parse_f64_field(key, value)?),
                "pixel_size_y" => georeferencing_pixel_size_y = Some(parse_f64_field(key, value)?),
                "origin_x" => georeferencing_origin_x = Some(parse_f64_field(key, value)?),
                "origin_y" => georeferencing_origin_y = Some(parse_f64_field(key, value)?),
                _ => {}
            }

            continue;
        }

        match key {
            "line_count" => line_count = Some(parse_usize_field(key, value)?),
            "sample_count" => sample_count = Some(parse_usize_field(key, value)?),
            "band_count" => band_count = Some(parse_usize_field(key, value)?),
            "bands" => band_names = Some(parse_string_field(value)),
            "data_type" => data_type = Some(DataType::from_meta_str(value)?),
            "x_pixel_size" => x_pixel_size = Some(parse_f64_field(key, value)?),
            "y_pixel_size" => y_pixel_size = Some(parse_f64_field(key, value)?),
            "no_data" => no_data = Some(parse_f64_field(key, value)?),
            "sensor" => sensor = Some(parse_string_field(value)),
            _ => {}
        }
    }

    let line_count = require_field("line_count", line_count)?;
    let sample_count = require_field("sample_count", sample_count)?;
    let band_count = require_field("band_count", band_count)?;
    let data_type = require_field("data_type", data_type)?;
    let x_pixel_size = require_field("x_pixel_size", x_pixel_size)?;
    let y_pixel_size = require_field("y_pixel_size", y_pixel_size)?;

    let bands = band_names
        .unwrap_or_default()
        .split(',')
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(|name| BandMetadata {
            name: name.to_string(),
        })
        .collect::<Vec<_>>();

    if bands.len() != band_count {
        return Err(Error::Metadata(format!(
            "Metadata band_count is {}, but bands field contains {} names in {}",
            band_count,
            bands.len(),
            path.display()
        )));
    }

    let statistics = if statistics_bands.is_empty() {
        None
    } else {
        Some(RasterStatistics {
            bands: statistics_bands,
        })
    };

    let source_geolocation = if source_geolocation_points.is_empty() {
        None
    } else {
        Some(SourceGeolocationMetadata {
            points: source_geolocation_points,
        })
    };

    let georeferencing = match georeferencing_projection.as_deref() {
        Some("utm") => {
            let zone = require_field("georeferencing.utm_zone", georeferencing_utm_zone)?;
            let hemisphere = require_field("georeferencing.hemisphere", georeferencing_hemisphere)?;
            let pixel_size_x = require_field("georeferencing.pixel_size_x", georeferencing_pixel_size_x)?;
            let pixel_size_y = require_field("georeferencing.pixel_size_y", georeferencing_pixel_size_y)?;
            let origin_x = require_field("georeferencing.origin_x", georeferencing_origin_x)?;
            let origin_y = require_field("georeferencing.origin_y", georeferencing_origin_y)?;

            Some(GeoreferencingMetadata {
                projection: ProjectionMetadata::Utm {
                    zone,
                    hemisphere,
                    epsg: georeferencing_epsg,
                },
                pixel_size_x,
                pixel_size_y,
                origin_x,
                origin_y,
            })
        }
        Some(other) => {
            return Err(Error::Metadata(format!(
                "Unsupported georeferencing projection: {other}"
            )));
        }
        None => None,
    };

    Ok(Metadata {
        general: GeneralMetadata {
            line_count,
            sample_count,
            band_count,
            data_type,
            x_pixel_size,
            y_pixel_size,
            no_data,
            sensor,
        },
        bands,
        statistics,
        source_geolocation,
        georeferencing,
    })
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

    if let Some(statistics) = &metadata.statistics {
        writeln!(text, "  statistics {{").unwrap();

        for band in &statistics.bands {
            writeln!(text, "    band {{").unwrap();
            writeln!(text, "      name: \"{}\"", band.name).unwrap();
            writeln!(text, "      valid_count: {}", band.valid_count).unwrap();
            writeln!(text, "      no_data_count: {}", band.no_data_count).unwrap();
            writeln!(text, "      min: {}", band.min).unwrap();
            writeln!(text, "      max: {}", band.max).unwrap();
            writeln!(text, "      mean: {}", band.mean).unwrap();
            writeln!(text, "      std_dev: {}", band.std_dev).unwrap();
            writeln!(text, "    }}").unwrap();
        }

        writeln!(text, "  }}").unwrap();
    }
    if let Some(source_geolocation) = &metadata.source_geolocation {
        writeln!(text, "  source_geolocation {{").unwrap();

        for point in &source_geolocation.points {
            writeln!(text, "    point {{").unwrap();
            writeln!(text, "      line: {}", point.line).unwrap();
            writeln!(text, "      pixel: {}", point.pixel).unwrap();
            writeln!(text, "      latitude: {}", point.latitude).unwrap();
            writeln!(text, "      longitude: {}", point.longitude).unwrap();

            if let Some(height) = point.height {
                writeln!(text, "      height: {}", height).unwrap();
            }

            if let Some(incidence_angle) = point.incidence_angle {
                writeln!(text, "      incidence_angle: {}", incidence_angle).unwrap();
            }

            writeln!(text, "    }}").unwrap();
        }

        writeln!(text, "  }}").unwrap();
    }

    if let Some(georeferencing) = &metadata.georeferencing {
        writeln!(text, "  georeferencing {{").unwrap();

        match &georeferencing.projection {
            ProjectionMetadata::Utm {
                zone,
                hemisphere,
                epsg,
            } => {
                writeln!(text, "    projection: \"utm\"").unwrap();
                writeln!(text, "    utm_zone: {}", zone).unwrap();
                writeln!(
                    text,
                    "    hemisphere: \"{}\"",
                    hemisphere.as_meta_str()
                )
                .unwrap();

                if let Some(epsg) = epsg {
                    writeln!(text, "    epsg: {}", epsg).unwrap();
                }
            }
        }

        writeln!(text, "    pixel_size_x: {}", georeferencing.pixel_size_x).unwrap();
        writeln!(text, "    pixel_size_y: {}", georeferencing.pixel_size_y).unwrap();
        writeln!(text, "    origin_x: {}", georeferencing.origin_x).unwrap();
        writeln!(text, "    origin_y: {}", georeferencing.origin_y).unwrap();
        writeln!(text, "  }}").unwrap();
    }

    writeln!(text, "}}").unwrap();

    fs::write(path, text)?;

    Ok(())
}

fn parse_usize_field(field_name: &str, value: &str) -> Result<usize> {
    value.parse::<usize>().map_err(|error| {
        Error::Metadata(format!(
            "Could not parse metadata field {field_name} as usize: {error}"
        ))
    })
}

fn parse_u8_field(field_name: &str, value: &str) -> Result<u8> {
    value.parse::<u8>().map_err(|error| {
        Error::Metadata(format!(
            "Could not parse metadata field {field_name} as u8: {error}"
        ))
    })
}

fn parse_u16_field(field_name: &str, value: &str) -> Result<u16> {
    value.parse::<u16>().map_err(|error| {
        Error::Metadata(format!(
            "Could not parse metadata field {field_name} as u16: {error}"
        ))
    })
}

fn parse_f64_field(field_name: &str, value: &str) -> Result<f64> {
    value.parse::<f64>().map_err(|error| {
        Error::Metadata(format!(
            "Could not parse metadata field {field_name} as f64: {error}"
        ))
    })
}

fn parse_string_field(value: &str) -> String {
    value
        .trim()
        .trim_start_matches('"')
        .trim_end_matches('"')
        .to_string()
}

fn parse_hemisphere_field(value: &str) -> Result<Hemisphere> {
    match parse_string_field(value).to_ascii_lowercase().as_str() {
        "north" | "n" => Ok(Hemisphere::North),
        "south" | "s" => Ok(Hemisphere::South),
        other => Err(Error::Metadata(format!(
            "Unsupported hemisphere value: {other}"
        ))),
    }
}

impl Hemisphere {
    fn as_meta_str(self) -> &'static str {
        match self {
            Self::North => "north",
            Self::South => "south",
        }
    }
}

fn require_field<T>(field_name: &str, value: Option<T>) -> Result<T> {
    value.ok_or_else(|| Error::Metadata(format!("Missing required metadata field: {field_name}")))
}
