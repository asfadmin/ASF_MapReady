use asf_core::{Error, Result};
use asf_meta::{
    DataType, GeolocationGridPoint, GeoreferencingMetadata, Hemisphere, ProjectionMetadata,
    SourceGeolocationMetadata,
};
use asf_raster::{create_raster, open_raster, read_f32_band, write_f32_band, ResampleMethod};

#[derive(Debug, Clone)]
pub struct GeocodeOptions {
    pub input_base: String,
    pub output_base: String,
    pub projection: Projection,
    pub pixel_size: Option<f64>,
    pub resample_method: ResampleMethod,
    pub background: Option<f32>,
    pub band: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Projection {
    LatLon,
    Utm { zone: u8, north: bool },
}

pub fn geocode(options: &GeocodeOptions) -> Result<()> {
    validate_options(options)?;

    let input_raster = open_raster(&options.input_base)?;
    let input_metadata = &input_raster.metadata;

    if input_metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "asf_geocode pass-through currently supports only REAL32 rasters, found {}",
            input_metadata.general.data_type.as_meta_str()
        )));
    }

    let source_geolocation = input_metadata.source_geolocation.as_ref().ok_or_else(|| {
        Error::InvalidInput(
            "asf_geocode requires source geolocation metadata from Sentinel import".to_string(),
        )
    })?;

    let pixel_size = options.pixel_size.ok_or_else(|| {
        Error::InvalidArgument(
            "--pixel-size is required for the current UTM georeferencing milestone".to_string(),
        )
    })?;

    let mut output_metadata = input_metadata.clone();

    output_metadata.general.x_pixel_size = pixel_size;
    output_metadata.general.y_pixel_size = pixel_size;
    output_metadata.georeferencing = Some(build_georeferencing(
        &options.projection,
        pixel_size,
        source_geolocation,
    )?);
    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;

    for band_index in 0..input_metadata.general.band_count {
        let samples = read_f32_band(&options.input_base, band_index)?;
        write_f32_band(&options.output_base, band_index, &samples)?;
    }

    Ok(())
}

fn validate_options(options: &GeocodeOptions) -> Result<()> {
    if let Some(pixel_size) = options.pixel_size {
        if !pixel_size.is_finite() || pixel_size <= 0.0 {
            return Err(Error::InvalidArgument(format!(
                "--pixel-size must be a positive finite value, got {pixel_size}"
            )));
        }
    }

    if options.background.is_some() {
        return Err(Error::InvalidArgument(
            "--background is not supported by the current pass-through asf_geocode milestone"
                .to_string(),
        ));
    }

    if options.band.is_some() {
        return Err(Error::InvalidArgument(
            "--band is not supported by the current pass-through asf_geocode milestone".to_string(),
        ));
    }

    match &options.projection {
        Projection::LatLon => {
            return Err(Error::InvalidArgument(
                "The current georeferencing milestone supports only -p utm".to_string(),
            ));
        }
        Projection::Utm { zone, .. } => {
            if !(1..=60).contains(zone) {
                return Err(Error::InvalidArgument(format!(
                    "UTM zone must be in the range 1..=60, got {zone}"
                )));
            }
        }
    }

    match options.resample_method {
        ResampleMethod::Nearest | ResampleMethod::Bilinear => {}
        ResampleMethod::Average | ResampleMethod::LogicalOr => {
            return Err(Error::InvalidArgument(format!(
                "Unsupported asf_geocode resample method for pass-through milestone: {:?}",
                options.resample_method
            )));
        }
    }

    Ok(())
}

fn build_georeferencing(
    projection: &Projection,
    pixel_size: f64,
    source_geolocation: &SourceGeolocationMetadata,
) -> Result<GeoreferencingMetadata> {
    let Projection::Utm { zone, .. } = projection else {
        return Err(Error::InvalidArgument(
            "The current georeferencing milestone supports only UTM output".to_string(),
        ));
    };

    let tie_point = select_tie_point(source_geolocation)?;
    let hemisphere = infer_hemisphere(source_geolocation)?;
    let epsg = utm_epsg(*zone, hemisphere);
    let (tie_easting, tie_northing) = lat_lon_to_utm(
        tie_point.latitude,
        tie_point.longitude,
        *zone,
        hemisphere,
    )?;

    let origin_x = tie_easting - (tie_point.pixel as f64 * pixel_size);
    let origin_y = tie_northing + (tie_point.line as f64 * pixel_size);

    Ok(GeoreferencingMetadata {
        projection: ProjectionMetadata::Utm {
            zone: *zone,
            hemisphere,
            epsg: Some(epsg),
        },
        pixel_size_x: pixel_size,
        pixel_size_y: pixel_size,
        origin_x,
        origin_y,
    })
}

fn select_tie_point(source_geolocation: &SourceGeolocationMetadata) -> Result<&GeolocationGridPoint> {
    source_geolocation
        .points
        .iter()
        .min_by_key(|point| point.line.saturating_add(point.pixel))
        .ok_or_else(|| {
            Error::InvalidInput(
                "Source geolocation metadata does not contain any grid points".to_string(),
            )
        })
}

fn infer_hemisphere(source_geolocation: &SourceGeolocationMetadata) -> Result<Hemisphere> {
    let mut latitude_sum = 0.0_f64;
    let mut count = 0_usize;

    for point in &source_geolocation.points {
        if point.latitude.is_finite() {
            latitude_sum += point.latitude;
            count += 1;
        }
    }

    if count == 0 {
        return Err(Error::InvalidInput(
            "Source geolocation metadata does not contain any finite latitudes".to_string(),
        ));
    }

    if latitude_sum / count as f64 >= 0.0 {
        Ok(Hemisphere::North)
    } else {
        Ok(Hemisphere::South)
    }
}

fn utm_epsg(zone: u8, hemisphere: Hemisphere) -> u16 {
    match hemisphere {
        Hemisphere::North => 32600 + zone as u16,
        Hemisphere::South => 32700 + zone as u16,
    }
}

fn lat_lon_to_utm(
    latitude_degrees: f64,
    longitude_degrees: f64,
    zone: u8,
    hemisphere: Hemisphere,
) -> Result<(f64, f64)> {
    if !latitude_degrees.is_finite()
        || !longitude_degrees.is_finite()
        || !(-90.0..=90.0).contains(&latitude_degrees)
        || !(-180.0..=180.0).contains(&longitude_degrees)
    {
        return Err(Error::InvalidInput(format!(
            "Cannot project invalid latitude/longitude to UTM: latitude={latitude_degrees}, longitude={longitude_degrees}"
        )));
    }

    if !(1..=60).contains(&zone) {
        return Err(Error::InvalidArgument(format!(
            "UTM zone must be in the range 1..=60, got {zone}"
        )));
    }

    // WGS84 ellipsoid constants.
    let semi_major_axis = 6378137.0_f64;
    let flattening = 1.0_f64 / 298.257223563_f64;
    let eccentricity_squared = flattening * (2.0 - flattening);
    let second_eccentricity_squared = eccentricity_squared / (1.0 - eccentricity_squared);
    let scale_factor = 0.9996_f64;

    let latitude = latitude_degrees.to_radians();
    let longitude = longitude_degrees.to_radians();
    let central_meridian = ((zone as f64 - 1.0) * 6.0 - 180.0 + 3.0).to_radians();

    let sin_latitude = latitude.sin();
    let cos_latitude = latitude.cos();
    let tan_latitude = latitude.tan();

    let radius_of_curvature =
        semi_major_axis / (1.0 - eccentricity_squared * sin_latitude * sin_latitude).sqrt();
    let t = tan_latitude * tan_latitude;
    let c = second_eccentricity_squared * cos_latitude * cos_latitude;
    let a = cos_latitude * (longitude - central_meridian);

    let meridional_arc = semi_major_axis
        * ((1.0
            - eccentricity_squared / 4.0
            - 3.0 * eccentricity_squared.powi(2) / 64.0
            - 5.0 * eccentricity_squared.powi(3) / 256.0)
            * latitude
            - (3.0 * eccentricity_squared / 8.0
                + 3.0 * eccentricity_squared.powi(2) / 32.0
                + 45.0 * eccentricity_squared.powi(3) / 1024.0)
                * (2.0 * latitude).sin()
            + (15.0 * eccentricity_squared.powi(2) / 256.0
                + 45.0 * eccentricity_squared.powi(3) / 1024.0)
                * (4.0 * latitude).sin()
            - (35.0 * eccentricity_squared.powi(3) / 3072.0) * (6.0 * latitude).sin());

    let easting = scale_factor
        * radius_of_curvature
        * (a
            + (1.0 - t + c) * a.powi(3) / 6.0
            + (5.0 - 18.0 * t + t * t + 72.0 * c - 58.0 * second_eccentricity_squared)
                * a.powi(5)
                / 120.0)
        + 500_000.0;

    let mut northing = scale_factor
        * (meridional_arc
            + radius_of_curvature
                * tan_latitude
                * (a.powi(2) / 2.0
                    + (5.0 - t + 9.0 * c + 4.0 * c * c) * a.powi(4) / 24.0
                    + (61.0 - 58.0 * t + t * t + 600.0 * c
                        - 330.0 * second_eccentricity_squared)
                        * a.powi(6)
                        / 720.0));

    if hemisphere == Hemisphere::South {
        northing += 10_000_000.0;
    }

    Ok((easting, northing))
}
