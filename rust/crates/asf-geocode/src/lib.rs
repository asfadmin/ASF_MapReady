use std::fs;

use asf_core::{path::image_path, Error, Result};
use asf_meta::{
    AffineTransform, DataType, GeolocationGridPoint, GeoreferencingMetadata, Hemisphere,
    ProjectionMetadata, SourceGeolocationMetadata,
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
    pub preserve_orientation: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Projection {
    LatLon,
    Utm { zone: Option<u8>, north: bool },
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct ProjectedPoint {
    source_line: f64,
    source_pixel: f64,
    x: f64,
    y: f64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct ProjectedBounds {
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
}

impl ProjectedBounds {
    fn width(self) -> f64 {
        self.max_x - self.min_x
    }

    fn height(self) -> f64 {
        self.max_y - self.min_y
    }
}

#[derive(Debug, Clone, Copy)]
struct GeolocationCell {
    top_left: ProjectedPoint,
    top_right: ProjectedPoint,
    bottom_left: ProjectedPoint,
    bottom_right: ProjectedPoint,
}

pub fn geocode(options: &GeocodeOptions) -> Result<()> {
    validate_options(options)?;

    let input_raster = open_raster(&options.input_base)?;
    let input_metadata = &input_raster.metadata;

    if input_metadata.general.data_type != DataType::F32 {
        return Err(Error::UnsupportedDataType(format!(
            "asf_geocode currently supports only REAL32 rasters, found {}",
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
            "--pixel-size is required for the current UTM geocoding milestone".to_string(),
        )
    })?;

    let Projection::Utm {
        zone: requested_zone,
        ..
    } = options.projection
    else {
        return Err(Error::InvalidArgument(
            "The current geocoding milestone supports only -p utm".to_string(),
        ));
    };

    let automatically_determined_zone = determine_zone(source_geolocation)?;
    let zone = match requested_zone {
        Some(zone) => {
            if zone != automatically_determined_zone {
                return Err(Error::InvalidArgument(format!(
                    "Requested UTM zone {zone} is incompatible with source geolocation. \
                     Automatically determined UTM zone is {automatically_determined_zone}."
                )));
            }

            zone
        }
        None => automatically_determined_zone,
    };

    let hemisphere = infer_hemisphere(source_geolocation)?;
    let epsg = utm_epsg(zone, hemisphere);
    let projected_points = project_geolocation_points(source_geolocation, zone, hemisphere)?;

    if options.preserve_orientation {
        return geocode_preserve_orientation(
            options,
            input_metadata,
            source_geolocation,
            &projected_points,
            zone,
            hemisphere,
            epsg,
            pixel_size,
        );
    }

    let bounds = projected_bounds(&projected_points)?;
    let cells = build_geolocation_cells(&projected_points)?;

    let output_sample_count = output_count(bounds.width(), pixel_size)?;
    let output_line_count = output_count(bounds.height(), pixel_size)?;

    validate_output_raster_size(
        &options.input_base,
        output_sample_count,
        output_line_count,
        input_metadata.general.band_count,
        input_metadata.general.data_type,
    )?;

    let mut output_metadata = input_metadata.clone();
    output_metadata.general.sample_count = output_sample_count;
    output_metadata.general.line_count = output_line_count;
    output_metadata.general.x_pixel_size = pixel_size;
    output_metadata.general.y_pixel_size = pixel_size;
    output_metadata.georeferencing = Some(GeoreferencingMetadata {
        projection: ProjectionMetadata::Utm {
            zone,
            hemisphere,
            epsg: Some(epsg),
        },
        pixel_size_x: pixel_size,
        pixel_size_y: pixel_size,
        origin_x: bounds.min_x,
        origin_y: bounds.max_y,
        affine: None,
    });
    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;

    let fill_value = input_metadata
        .general
        .no_data
        .map(|value| value as f32)
        .unwrap_or(0.0);

    for band_index in 0..input_metadata.general.band_count {
        let input_band = read_f32_band(&options.input_base, band_index)?;
        let output_band = geocode_band_nearest(
            &input_band,
            input_metadata.general.sample_count,
            input_metadata.general.line_count,
            output_sample_count,
            output_line_count,
            pixel_size,
            bounds,
            &cells,
            fill_value,
        )?;

        write_f32_band(&options.output_base, band_index, &output_band)?;
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
            "--background is not supported by the current Sentinel UTM geocoding milestone"
                .to_string(),
        ));
    }

    if options.band.is_some() {
        return Err(Error::InvalidArgument(
            "--band is not supported by the current Sentinel UTM geocoding milestone".to_string(),
        ));
    }

    match &options.projection {
        Projection::LatLon => {
            return Err(Error::InvalidArgument(
                "The current geocoding milestone supports only -p utm".to_string(),
            ));
        }
        Projection::Utm {
            zone: Some(zone), ..
        } => {
            if !(1..=60).contains(zone) {
                return Err(Error::InvalidArgument(format!(
                    "UTM zone must be in the range 1..=60, got {zone}"
                )));
            }
        }
        Projection::Utm { zone: None, .. } => {}
    }

    match options.resample_method {
        ResampleMethod::Nearest => {}
        ResampleMethod::Bilinear | ResampleMethod::Average | ResampleMethod::LogicalOr => {
            return Err(Error::InvalidArgument(format!(
                "Unsupported asf_geocode resample method for current Sentinel UTM milestone: {:?}",
                options.resample_method
            )));
        }
    }

    Ok(())
}


const MAX_GEOCODE_OUTPUT_TO_INPUT_RATIO: u64 = 16;
const MIN_MAX_GEOCODE_OUTPUT_BYTES: u64 = 1_073_741_824;

fn validate_output_raster_size(
    input_base: &str,
    output_width: usize,
    output_height: usize,
    band_count: usize,
    data_type: DataType,
) -> Result<()> {
    let output_bytes = raster_byte_count(output_width, output_height, band_count, data_type)?;
    let input_bytes = fs::metadata(image_path(input_base))?.len();

    let max_output_bytes = input_bytes
        .saturating_mul(MAX_GEOCODE_OUTPUT_TO_INPUT_RATIO)
        .max(MIN_MAX_GEOCODE_OUTPUT_BYTES);

    if output_bytes > max_output_bytes {
        return Err(Error::InvalidInput(format!(
            "Refusing to create geocoded output raster of {} bytes. \
             Input image is {} bytes, configured limit is {} bytes, and requested output dimensions are {}x{}x{} {}. \
             This usually indicates invalid projected bounds, an incompatible UTM zone, or antimeridian handling trouble.",
            output_bytes,
            input_bytes,
            max_output_bytes,
            output_width,
            output_height,
            band_count,
            data_type.as_meta_str()
        )));
    }

    Ok(())
}

fn raster_byte_count(
    width: usize,
    height: usize,
    band_count: usize,
    data_type: DataType,
) -> Result<u64> {
    let bytes = width
        .checked_mul(height)
        .and_then(|pixels| pixels.checked_mul(band_count))
        .and_then(|samples| samples.checked_mul(data_type.byte_size()))
        .ok_or_else(|| Error::InvalidInput("Output raster byte count overflows usize".to_string()))?;

    if bytes > u64::MAX as usize {
        return Err(Error::InvalidInput(format!(
            "Output raster byte count overflows u64: {bytes}"
        )));
    }

    Ok(bytes as u64)
}

fn geocode_preserve_orientation(
    options: &GeocodeOptions,
    input_metadata: &asf_meta::Metadata,
    source_geolocation: &SourceGeolocationMetadata,
    projected_points: &[ProjectedPoint],
    zone: u8,
    hemisphere: Hemisphere,
    epsg: u16,
    requested_pixel_size: f64,
) -> Result<()> {
    if options.resample_method != ResampleMethod::Nearest {
        return Err(Error::InvalidArgument(
            "--preserve-orientation currently supports only nearest-neighbor resampling".to_string(),
        ));
    }

    let x_scale = input_metadata.general.x_pixel_size / requested_pixel_size;
    let y_scale = input_metadata.general.y_pixel_size / requested_pixel_size;

    if !x_scale.is_finite() || !y_scale.is_finite() || x_scale <= 0.0 || y_scale <= 0.0 {
        return Err(Error::InvalidInput(format!(
            "Invalid preserve-orientation scale factors: x_scale={}, y_scale={}",
            x_scale, y_scale
        )));
    }

    let output_sample_count = scaled_count(input_metadata.general.sample_count, x_scale)?;
    let output_line_count = scaled_count(input_metadata.general.line_count, y_scale)?;

    validate_output_raster_size(
        &options.input_base,
        output_sample_count,
        output_line_count,
        input_metadata.general.band_count,
        input_metadata.general.data_type,
    )?;

    let affine = affine_from_projected_grid(projected_points)?;

    let output_affine = AffineTransform {
        origin_x: affine.origin_x,
        pixel_x: affine.pixel_x / x_scale,
        line_x: affine.line_x / y_scale,
        origin_y: affine.origin_y,
        pixel_y: affine.pixel_y / x_scale,
        line_y: affine.line_y / y_scale,
    };

    let mut output_metadata = input_metadata.clone();
    output_metadata.general.sample_count = output_sample_count;
    output_metadata.general.line_count = output_line_count;
    output_metadata.general.x_pixel_size = requested_pixel_size;
    output_metadata.general.y_pixel_size = requested_pixel_size;
    scale_source_geolocation(
        &mut output_metadata,
        input_metadata.general.sample_count,
        input_metadata.general.line_count,
        output_sample_count,
        output_line_count,
    );
    output_metadata.georeferencing = Some(GeoreferencingMetadata {
        projection: ProjectionMetadata::Utm {
            zone,
            hemisphere,
            epsg: Some(epsg),
        },
        pixel_size_x: requested_pixel_size,
        pixel_size_y: requested_pixel_size,
        origin_x: output_affine.origin_x,
        origin_y: output_affine.origin_y,
        affine: Some(output_affine),
    });
    output_metadata.statistics = None;

    create_raster(&options.output_base, &output_metadata)?;

    let fill_value = input_metadata
        .general
        .no_data
        .map(|value| value as f32)
        .unwrap_or(0.0);

    for band_index in 0..input_metadata.general.band_count {
        let input_band = read_f32_band(&options.input_base, band_index)?;
        let output_band = resample_preserve_orientation_nearest(
            &input_band,
            input_metadata.general.sample_count,
            input_metadata.general.line_count,
            output_sample_count,
            output_line_count,
            fill_value,
        )?;

        write_f32_band(&options.output_base, band_index, &output_band)?;
    }

    // source_geolocation is intentionally accepted as a parameter so this mode
    // documents that it still depends on Sentinel source geolocation metadata.
    let _ = source_geolocation;

    Ok(())
}

fn scaled_count(input_count: usize, scale: f64) -> Result<usize> {
    let output_count = ((input_count as f64) * scale).round();

    if output_count < 1.0 {
        return Ok(1);
    }

    if output_count > usize::MAX as f64 {
        return Err(Error::InvalidInput(format!(
            "Scaled preserve-orientation dimension overflows usize: {output_count}"
        )));
    }

    Ok(output_count as usize)
}

fn resample_preserve_orientation_nearest(
    input: &[f32],
    input_width: usize,
    input_height: usize,
    output_width: usize,
    output_height: usize,
    fill_value: f32,
) -> Result<Vec<f32>> {
    let expected_input_len = input_width
        .checked_mul(input_height)
        .ok_or_else(|| Error::InvalidInput("Input raster dimensions overflow usize".to_string()))?;

    if input.len() != expected_input_len {
        return Err(Error::InvalidInput(format!(
            "Input band expected {} samples, found {}",
            expected_input_len,
            input.len()
        )));
    }

    let output_len = output_width.checked_mul(output_height).ok_or_else(|| {
        Error::InvalidInput("Output raster dimensions overflow usize".to_string())
    })?;

    let mut output = vec![fill_value; output_len];

    let x_ratio = input_width as f64 / output_width as f64;
    let y_ratio = input_height as f64 / output_height as f64;

    for output_y in 0..output_height {
        let input_y = ((output_y as f64 + 0.5) * y_ratio - 0.5).round() as isize;

        for output_x in 0..output_width {
            let input_x = ((output_x as f64 + 0.5) * x_ratio - 0.5).round() as isize;

            if input_x < 0
                || input_y < 0
                || input_x >= input_width as isize
                || input_y >= input_height as isize
            {
                continue;
            }

            output[output_y * output_width + output_x] =
                input[input_y as usize * input_width + input_x as usize];
        }
    }

    Ok(output)
}

fn scale_source_geolocation(
    metadata: &mut asf_meta::Metadata,
    input_sample_count: usize,
    input_line_count: usize,
    output_sample_count: usize,
    output_line_count: usize,
) {
    let Some(source_geolocation) = &mut metadata.source_geolocation else {
        return;
    };

    if input_sample_count == 0 || input_line_count == 0 {
        return;
    }

    let x_scale = output_sample_count as f64 / input_sample_count as f64;
    let y_scale = output_line_count as f64 / input_line_count as f64;

    let max_output_pixel = output_sample_count.saturating_sub(1);
    let max_output_line = output_line_count.saturating_sub(1);

    for point in &mut source_geolocation.points {
        point.pixel = ((point.pixel as f64) * x_scale)
            .round()
            .clamp(0.0, max_output_pixel as f64) as usize;

        point.line = ((point.line as f64) * y_scale)
            .round()
            .clamp(0.0, max_output_line as f64) as usize;
    }
}

fn affine_from_projected_grid(points: &[ProjectedPoint]) -> Result<AffineTransform> {
    let top_left = points
        .iter()
        .min_by(|a, b| {
            a.source_line
                .total_cmp(&b.source_line)
                .then(a.source_pixel.total_cmp(&b.source_pixel))
        })
        .ok_or_else(|| Error::InvalidInput("Cannot build affine transform from empty grid".to_string()))?;

    let top_right = points
        .iter()
        .filter(|point| (point.source_line - top_left.source_line).abs() < f64::EPSILON)
        .max_by(|a, b| a.source_pixel.total_cmp(&b.source_pixel))
        .ok_or_else(|| Error::InvalidInput("Cannot find top-right geolocation point".to_string()))?;

    let bottom_left = points
        .iter()
        .filter(|point| (point.source_pixel - top_left.source_pixel).abs() < f64::EPSILON)
        .max_by(|a, b| a.source_line.total_cmp(&b.source_line))
        .ok_or_else(|| Error::InvalidInput("Cannot find bottom-left geolocation point".to_string()))?;

    let pixel_span = top_right.source_pixel - top_left.source_pixel;
    let line_span = bottom_left.source_line - top_left.source_line;

    if pixel_span == 0.0 || line_span == 0.0 {
        return Err(Error::InvalidInput(
            "Cannot build affine transform from degenerate geolocation grid".to_string(),
        ));
    }

    let pixel_x = (top_right.x - top_left.x) / pixel_span;
    let pixel_y = (top_right.y - top_left.y) / pixel_span;
    let line_x = (bottom_left.x - top_left.x) / line_span;
    let line_y = (bottom_left.y - top_left.y) / line_span;

    let origin_x = top_left.x - pixel_x * top_left.source_pixel - line_x * top_left.source_line;
    let origin_y = top_left.y - pixel_y * top_left.source_pixel - line_y * top_left.source_line;

    Ok(AffineTransform {
        origin_x,
        pixel_x,
        line_x,
        origin_y,
        pixel_y,
        line_y,
    })
}

fn project_geolocation_points(
    source_geolocation: &SourceGeolocationMetadata,
    zone: u8,
    hemisphere: Hemisphere,
) -> Result<Vec<ProjectedPoint>> {
    if source_geolocation.points.is_empty() {
        return Err(Error::InvalidInput(
            "Source geolocation metadata does not contain any grid points".to_string(),
        ));
    }

    source_geolocation
        .points
        .iter()
        .map(|point| project_geolocation_point(point, zone, hemisphere))
        .collect()
}

fn project_geolocation_point(
    point: &GeolocationGridPoint,
    zone: u8,
    hemisphere: Hemisphere,
) -> Result<ProjectedPoint> {
    let (x, y) = lat_lon_to_utm(point.latitude, point.longitude, zone, hemisphere)?;

    Ok(ProjectedPoint {
        source_line: point.line as f64,
        source_pixel: point.pixel as f64,
        x,
        y,
    })
}

fn projected_bounds(points: &[ProjectedPoint]) -> Result<ProjectedBounds> {
    let mut bounds: Option<ProjectedBounds> = None;

    for point in points {
        bounds = Some(match bounds {
            Some(bounds) => ProjectedBounds {
                min_x: bounds.min_x.min(point.x),
                max_x: bounds.max_x.max(point.x),
                min_y: bounds.min_y.min(point.y),
                max_y: bounds.max_y.max(point.y),
            },
            None => ProjectedBounds {
                min_x: point.x,
                max_x: point.x,
                min_y: point.y,
                max_y: point.y,
            },
        });
    }

    let bounds = bounds.ok_or_else(|| {
        Error::InvalidInput(
            "Source geolocation metadata does not contain any projected grid points".to_string(),
        )
    })?;

    if !bounds.min_x.is_finite()
        || !bounds.max_x.is_finite()
        || !bounds.min_y.is_finite()
        || !bounds.max_y.is_finite()
        || bounds.width() <= 0.0
        || bounds.height() <= 0.0
    {
        return Err(Error::InvalidInput(format!(
            "Projected source geolocation bounds are invalid: min_x={}, max_x={}, min_y={}, max_y={}",
            bounds.min_x, bounds.max_x, bounds.min_y, bounds.max_y
        )));
    }

    Ok(bounds)
}

fn output_count(projected_size: f64, pixel_size: f64) -> Result<usize> {
    if !projected_size.is_finite() || projected_size <= 0.0 {
        return Err(Error::InvalidInput(format!(
            "Invalid projected output size: {projected_size}"
        )));
    }

    let count = (projected_size / pixel_size).ceil();

    if count < 1.0 || count > usize::MAX as f64 {
        return Err(Error::InvalidInput(format!(
            "Projected output dimension is invalid: {count}"
        )));
    }

    Ok(count as usize)
}

fn build_geolocation_cells(points: &[ProjectedPoint]) -> Result<Vec<GeolocationCell>> {
    let mut sorted = points.to_vec();
    sorted.sort_by(|a, b| {
        a.source_line
            .total_cmp(&b.source_line)
            .then(a.source_pixel.total_cmp(&b.source_pixel))
    });

    let mut line_values = sorted
        .iter()
        .map(|point| point.source_line)
        .collect::<Vec<_>>();
    line_values.dedup_by(|a, b| (*a - *b).abs() < f64::EPSILON);

    if line_values.len() < 2 {
        return Err(Error::InvalidInput(
            "Source geolocation grid must contain at least two lines".to_string(),
        ));
    }

    let mut cells = Vec::new();

    for line_pair in line_values.windows(2) {
        let top_line = line_pair[0];
        let bottom_line = line_pair[1];

        let top_points = sorted
            .iter()
            .copied()
            .filter(|point| (point.source_line - top_line).abs() < f64::EPSILON)
            .collect::<Vec<_>>();

        let bottom_points = sorted
            .iter()
            .copied()
            .filter(|point| (point.source_line - bottom_line).abs() < f64::EPSILON)
            .collect::<Vec<_>>();

        if top_points.len() < 2 || bottom_points.len() < 2 {
            return Err(Error::InvalidInput(format!(
                "Source geolocation grid lines {} and {} must each contain at least two points",
                top_line, bottom_line
            )));
        }

        for top_pair in top_points.windows(2) {
            let top_left = top_pair[0];
            let top_right = top_pair[1];

            let Some(bottom_left) = find_point_on_line(&bottom_points, top_left.source_pixel) else {
                continue;
            };

            let Some(bottom_right) = find_point_on_line(&bottom_points, top_right.source_pixel) else {
                continue;
            };

            cells.push(GeolocationCell {
                top_left,
                top_right,
                bottom_left,
                bottom_right,
            });
        }
    }

    if cells.is_empty() {
        return Err(Error::InvalidInput(
            "Could not construct any cells from the source geolocation grid".to_string(),
        ));
    }

    Ok(cells)
}

fn find_point_on_line(points: &[ProjectedPoint], source_pixel: f64) -> Option<ProjectedPoint> {
    points
        .iter()
        .copied()
        .find(|point| (point.source_pixel - source_pixel).abs() < f64::EPSILON)
}

fn geocode_band_nearest(
    input: &[f32],
    input_width: usize,
    input_height: usize,
    output_width: usize,
    output_height: usize,
    pixel_size: f64,
    bounds: ProjectedBounds,
    cells: &[GeolocationCell],
    fill_value: f32,
) -> Result<Vec<f32>> {
    let expected_input_len = input_width
        .checked_mul(input_height)
        .ok_or_else(|| Error::InvalidInput("Input raster dimensions overflow usize".to_string()))?;

    if input.len() != expected_input_len {
        return Err(Error::InvalidInput(format!(
            "Input band expected {} samples, found {}",
            expected_input_len,
            input.len()
        )));
    }

    let output_len = output_width.checked_mul(output_height).ok_or_else(|| {
        Error::InvalidInput("Output raster dimensions overflow usize".to_string())
    })?;

    let mut output = vec![fill_value; output_len];

    for cell in cells {
        fill_output_pixels_for_cell(
            cell,
            input,
            input_width,
            input_height,
            &mut output,
            output_width,
            output_height,
            pixel_size,
            bounds,
        )?;
    }

    Ok(output)
}

fn fill_output_pixels_for_cell(
    cell: &GeolocationCell,
    input: &[f32],
    input_width: usize,
    input_height: usize,
    output: &mut [f32],
    output_width: usize,
    output_height: usize,
    pixel_size: f64,
    bounds: ProjectedBounds,
) -> Result<()> {
    let cell_min_x = cell
        .top_left
        .x
        .min(cell.top_right.x)
        .min(cell.bottom_left.x)
        .min(cell.bottom_right.x);
    let cell_max_x = cell
        .top_left
        .x
        .max(cell.top_right.x)
        .max(cell.bottom_left.x)
        .max(cell.bottom_right.x);
    let cell_min_y = cell
        .top_left
        .y
        .min(cell.top_right.y)
        .min(cell.bottom_left.y)
        .min(cell.bottom_right.y);
    let cell_max_y = cell
        .top_left
        .y
        .max(cell.top_right.y)
        .max(cell.bottom_left.y)
        .max(cell.bottom_right.y);

    let start_x = (((cell_min_x - bounds.min_x) / pixel_size).floor() as isize - 1).max(0) as usize;
    let end_x = (((cell_max_x - bounds.min_x) / pixel_size).ceil() as isize + 1)
        .min(output_width as isize) as usize;

    let start_y = (((bounds.max_y - cell_max_y) / pixel_size).floor() as isize - 1).max(0) as usize;
    let end_y = (((bounds.max_y - cell_min_y) / pixel_size).ceil() as isize + 1)
        .min(output_height as isize) as usize;

    for output_y in start_y..end_y {
        let map_y = bounds.max_y - ((output_y as f64 + 0.5) * pixel_size);

        for output_x in start_x..end_x {
            let map_x = bounds.min_x + ((output_x as f64 + 0.5) * pixel_size);

            let Some((u, v)) = solve_bilinear_inverse(cell, map_x, map_y) else {
                continue;
            };

            if !(-0.001..=1.001).contains(&u) || !(-0.001..=1.001).contains(&v) {
                continue;
            }

            let source_pixel = bilinear_value(
                cell.top_left.source_pixel,
                cell.top_right.source_pixel,
                cell.bottom_left.source_pixel,
                cell.bottom_right.source_pixel,
                u,
                v,
            );

            let source_line = bilinear_value(
                cell.top_left.source_line,
                cell.top_right.source_line,
                cell.bottom_left.source_line,
                cell.bottom_right.source_line,
                u,
                v,
            );

            let Some(value) = sample_nearest(input, input_width, input_height, source_pixel, source_line) else {
                continue;
            };

            output[output_y * output_width + output_x] = value;
        }
    }

    Ok(())
}

fn solve_bilinear_inverse(cell: &GeolocationCell, x: f64, y: f64) -> Option<(f64, f64)> {
    // The geolocation grid gives us a bilinear mapping from cell coordinates
    // (u, v) to projected coordinates (x, y):
    //
    // u = 0 left, u = 1 right
    // v = 0 top,  v = 1 bottom
    //
    // For geocoding we need the inverse: output UTM x/y -> source image line/pixel.
    // Newton iteration is narrow but adequate for Sentinel annotation grid cells.

    let mut u = 0.5_f64;
    let mut v = 0.5_f64;

    for _ in 0..12 {
        let current_x = bilinear_value(
            cell.top_left.x,
            cell.top_right.x,
            cell.bottom_left.x,
            cell.bottom_right.x,
            u,
            v,
        );

        let current_y = bilinear_value(
            cell.top_left.y,
            cell.top_right.y,
            cell.bottom_left.y,
            cell.bottom_right.y,
            u,
            v,
        );

        let error_x = current_x - x;
        let error_y = current_y - y;

        if error_x.hypot(error_y) < 0.01 {
            return Some((u, v));
        }

        let dx_du = bilinear_du(
            cell.top_left.x,
            cell.top_right.x,
            cell.bottom_left.x,
            cell.bottom_right.x,
            v,
        );
        let dx_dv = bilinear_dv(
            cell.top_left.x,
            cell.top_right.x,
            cell.bottom_left.x,
            cell.bottom_right.x,
            u,
        );
        let dy_du = bilinear_du(
            cell.top_left.y,
            cell.top_right.y,
            cell.bottom_left.y,
            cell.bottom_right.y,
            v,
        );
        let dy_dv = bilinear_dv(
            cell.top_left.y,
            cell.top_right.y,
            cell.bottom_left.y,
            cell.bottom_right.y,
            u,
        );

        let determinant = dx_du * dy_dv - dx_dv * dy_du;

        if !determinant.is_finite() || determinant.abs() < 1.0e-12 {
            return None;
        }

        let delta_u = (error_x * dy_dv - error_y * dx_dv) / determinant;
        let delta_v = (dx_du * error_y - dy_du * error_x) / determinant;

        u -= delta_u;
        v -= delta_v;

        if !u.is_finite() || !v.is_finite() {
            return None;
        }

        if !(-0.25..=1.25).contains(&u) || !(-0.25..=1.25).contains(&v) {
            return None;
        }
    }

    Some((u, v))
}

fn bilinear_value(top_left: f64, top_right: f64, bottom_left: f64, bottom_right: f64, u: f64, v: f64) -> f64 {
    top_left * (1.0 - u) * (1.0 - v)
        + top_right * u * (1.0 - v)
        + bottom_left * (1.0 - u) * v
        + bottom_right * u * v
}

fn bilinear_du(top_left: f64, top_right: f64, bottom_left: f64, bottom_right: f64, v: f64) -> f64 {
    (top_right - top_left) * (1.0 - v) + (bottom_right - bottom_left) * v
}

fn bilinear_dv(top_left: f64, top_right: f64, bottom_left: f64, bottom_right: f64, u: f64) -> f64 {
    (bottom_left - top_left) * (1.0 - u) + (bottom_right - top_right) * u
}

fn sample_nearest(
    input: &[f32],
    input_width: usize,
    input_height: usize,
    source_pixel: f64,
    source_line: f64,
) -> Option<f32> {
    if !source_pixel.is_finite() || !source_line.is_finite() {
        return None;
    }

    let x = source_pixel.round() as isize;
    let y = source_line.round() as isize;

    if x < 0 || y < 0 || x >= input_width as isize || y >= input_height as isize {
        return None;
    }

    Some(input[y as usize * input_width + x as usize])
}

fn determine_zone(source_geolocation: &SourceGeolocationMetadata) -> Result<u8> {
    let (center_latitude, center_longitude) = source_geolocation_center_lat_lon(source_geolocation)?;

    if !(-80.0..=84.0).contains(&center_latitude) {
        return Err(Error::InvalidInput(format!(
            "Cannot determine UTM zone for latitude {center_latitude}; UTM is valid only from 80°S to 84°N"
        )));
    }

    // Standard Norway exception.
    if (56.0..64.0).contains(&center_latitude) && (3.0..12.0).contains(&center_longitude) {
        return Ok(32);
    }

    // Standard Svalbard exceptions.
    if (72.0..84.0).contains(&center_latitude) {
        if (0.0..9.0).contains(&center_longitude) {
            return Ok(31);
        }

        if (9.0..21.0).contains(&center_longitude) {
            return Ok(33);
        }

        if (21.0..33.0).contains(&center_longitude) {
            return Ok(35);
        }

        if (33.0..42.0).contains(&center_longitude) {
            return Ok(37);
        }
    }

    let zone = (((center_longitude + 180.0) / 6.0).floor() as i32 + 1).clamp(1, 60);

    Ok(zone as u8)
}

fn source_geolocation_center_lat_lon(source_geolocation: &SourceGeolocationMetadata) -> Result<(f64, f64)> {
    let mut latitude_sum = 0.0_f64;
    let mut longitude_sum = 0.0_f64;
    let mut count = 0_usize;

    let mut min_longitude = f64::INFINITY;
    let mut max_longitude = f64::NEG_INFINITY;

    for point in &source_geolocation.points {
        if !point.latitude.is_finite()
            || !point.longitude.is_finite()
            || !(-90.0..=90.0).contains(&point.latitude)
            || !(-180.0..=180.0).contains(&point.longitude)
        {
            continue;
        }

        latitude_sum += point.latitude;
        longitude_sum += point.longitude;
        count += 1;

        min_longitude = min_longitude.min(point.longitude);
        max_longitude = max_longitude.max(point.longitude);
    }

    if count == 0 {
        return Err(Error::InvalidInput(
            "Cannot determine UTM zone because source geolocation contains no valid latitude/longitude points"
                .to_string(),
        ));
    }

    let mean_latitude = latitude_sum / count as f64;

    if max_longitude - min_longitude <= 180.0 {
        return Ok((mean_latitude, longitude_sum / count as f64));
    }

    let mut unwrapped_longitude_sum = 0.0_f64;

    for point in &source_geolocation.points {
        if !point.longitude.is_finite() || !(-180.0..=180.0).contains(&point.longitude) {
            continue;
        }

        let longitude = if point.longitude < 0.0 {
            point.longitude + 360.0
        } else {
            point.longitude
        };

        unwrapped_longitude_sum += longitude;
    }

    let mut mean_longitude = unwrapped_longitude_sum / count as f64;

    if mean_longitude > 180.0 {
        mean_longitude -= 360.0;
    }

    Ok((mean_latitude, mean_longitude))
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

fn normalize_longitude_delta_radians(delta: f64) -> f64 {
    let two_pi = std::f64::consts::TAU;
    let pi = std::f64::consts::PI;

    (delta + pi).rem_euclid(two_pi) - pi
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
    let longitude_delta = normalize_longitude_delta_radians(longitude - central_meridian);

    let sin_latitude = latitude.sin();
    let cos_latitude = latitude.cos();
    let tan_latitude = latitude.tan();

    let radius_of_curvature =
        semi_major_axis / (1.0 - eccentricity_squared * sin_latitude * sin_latitude).sqrt();
    let t = tan_latitude * tan_latitude;
    let c = second_eccentricity_squared * cos_latitude * cos_latitude;
    let a = cos_latitude * longitude_delta;

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
