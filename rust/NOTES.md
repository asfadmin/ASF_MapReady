
# ASF MapReady Rust Port Notes

## Project goal

Create browse images for Sentinel using a Rust-only port of the relevant MapReady functionality.

Do not modify the legacy C code. It is legacy and further development is no longer an option.

Legacy MapReady `.meta` compatibility is not required. The Rust tools may use a Rust-owned `.meta` format as long as the Rust Sentinel browse workflow works end-to-end.

Use modern clap command-line syntax. Do not preserve legacy single-dash long options unless there is a specific reason to do so.

The long-term Sentinel goal is broader than the immediate browse MVP. Implement the Sentinel path in coherent stages that follow the legacy Sentinel import/export behavior, instead of adding isolated post-processing fixes that compensate for incomplete upstream radiometry.

The legacy C code outputs exactly what we want. The resulting Rust implementation should strive for the same output.

## Target Sentinel browse workflow

The Rust implementation currently targets this workflow:

```shell script
asf_import --format sentinel --gamma SAFE_OR_INPUT imported
resample --square 125.0 imported resampled
asf_geocode -p utm --zone 6 --pixel-size 250 resampled geocoded
adjust_bands --bands VV geocoded vv_only
adjust_bands --bands VH geocoded vh_only
color_browse --noise-cal=-23.5 --tmpDir tmp vv_only vh_only color
stats --mask 0.0 --overmeta color
asf_export --format geotiff --byte truncate --rgb "Ps Pv Pd" color browse.tif
```


A second geocoding/export variant is now supported for preserving source image orientation while still writing spatial GeoTIFF metadata:

```shell script
asf_geocode -p utm --zone 6 --pixel-size 250 --preserve-orientation resampled geocoded
```


In this mode, the raster is not rotated into a north-up UTM grid. Instead, `asf_geocode` writes an affine transform into Rust metadata and `asf_export` serializes it as a GeoTIFF `ModelTransformationTag`.

Target options by tool:

- `asf_import`
  - `--format sentinel`
  - `--gamma` for the browse workflow
  - radiometry options:
    - `--amplitude`
    - `--sigma`
    - `--beta`
    - `--gamma`
    - `--power`
    - `--db`
- `resample`
  - `--square PIXEL_SIZE`
- `asf_geocode`
  - `-p utm`
  - `--zone ZONE`
  - `--pixel-size PIXEL_SIZE`
  - `--preserve-orientation`
- `adjust_bands`
  - `--bands BAND_NAME`
  - currently one band only
- `color_browse`
  - `--noise-cal VALUE`
  - `--tmpDir DIR`
- `stats`
  - `--mask VALUE`
  - `--overmeta`
- `asf_export`
  - `--format geotiff`
  - `--byte truncate`
  - `--byte sigma`
  - `--rgb "Ps Pv Pd"` for dual-pol color browse
  - no `--rgb` for single-band grayscale browse

The target workflow may eventually support:

```shell script
asf_export --format geotiff --byte sigma color browse.tif
```


but export scaling should not be tuned to compensate for incomplete upstream Sentinel import radiometry.

## Rust workspace status

A Rust workspace exists under `rust/`.

Implemented so far:

- workspace skeleton
- `asf-core`
- `asf-meta`
- `asf-raster`
- `asf-import`
- `asf-resample`
- `asf-geocode`
- `asf-adjust-bands`
- `asf-color-browse`
- `asf-stats`
- `sqrt_img` for applying square root to Rust-owned `REAL32` rasters
- `asf_export --format geotiff --byte sigma` for grayscale/single-band export
- `asf_export` can export one-band input without `--rgb`
- CLI tools for the target commands
- Sentinel-only `asf_import` path
- Rust-owned `.meta` reader/writer
- Rust-owned band-sequential little-endian `REAL32` raster path
- `stats --overmeta` writes per-band statistics into Rust metadata
- `resample --square PIXEL_SIZE` for Rust-owned `f32` rasters
- `adjust_bands --bands BAND_NAME` for selecting one named band
- `color_browse --noise-cal VALUE [--tmpDir DIR] CO_POL CROSS_POL OUTPUT`
  - for VV+VH, VV IS CO_POL AND VH IS CROSS_POL
  - for HV+HH, HH IS CO_POL AND HV IS CROSS_POL
- `asf_export --format geotiff --byte truncate --rgb "R G B"`
- Rust-owned source geolocation metadata
- Rust-owned projected georeferencing metadata
- Sentinel annotation geolocation grid parsing
- GeoTIFF spatial tag writing
- two narrow `asf_geocode` modes:
  - default north-up UTM geocoding
  - `--preserve-orientation` affine GeoTIFF mode

Current high-level status:

- The Rust Sentinel browse pipeline can produce a spatial GeoTIFF.
- The default geocode path now performs a narrow real UTM geocode using Sentinel annotation geolocation grid points.
- The `--preserve-orientation` path keeps the image visually source-oriented and writes a rotated/skewed affine GeoTIFF transform.
- Both default and preserve-orientation geocode paths have been manually regression-tested by the current development effort.
- The preserve-orientation output visually matches the desired non-rotated image behavior while still placing correctly in GIS software.

## Rust metadata status

Rust metadata is intentionally Rust-owned and does not attempt legacy MapReady `.meta` compatibility.

Current top-level metadata includes:

- general raster metadata
- band names
- per-band statistics
- optional Sentinel source geolocation metadata
- optional projected georeferencing metadata

Source geolocation metadata stores Sentinel annotation grid points:

- line
- pixel
- latitude
- longitude
- height, if present
- incidence angle, if present

Projected georeferencing metadata currently supports:

- UTM projection
- zone
- hemisphere
- EPSG code
- pixel size
- north-up origin
- optional affine transform for preserve-orientation GeoTIFFs

Affine georeferencing uses this model:

```plain text
map_x = origin_x + pixel_x * pixel + line_x * line
map_y = origin_y + pixel_y * pixel + line_y * line
```


`asf_export` writes:

- `ModelPixelScaleTag`
- `ModelTiepointTag`
- `GeoKeyDirectoryTag`

for north-up georeferencing, and writes:

- `ModelTransformationTag`
- `GeoKeyDirectoryTag`

when affine metadata is present.

## Current asf_import behavior

`asf_import` implements a narrow Sentinel-only import path aimed at the Sentinel browse workflow.

Current command shape:

```shell script
asf_import --format sentinel [--amplitude | --sigma | --beta | --gamma | --power] [--db] INPUT OUTPUT_BASE
```


Expected browse use:

```shell script
asf_import --format sentinel --gamma SAFE_OR_INPUT imported
```


Current behavior:

- supports only `--format sentinel`
- uses modern clap command-line parsing
- parses radiometry as an explicit mode rather than a boolean
- finds a Sentinel `.SAFE` directory or input directory
- builds an explicit Sentinel product model
- routes product types explicitly:
  - GRD is supported
  - SLC is intentionally unsupported
  - RAW is intentionally unsupported
  - OCN is intentionally unsupported
- links each measurement TIFF to expected:
  - annotation XML
  - calibration XML
  - noise XML
- reads Sentinel calibration XML using `roxmltree`
- reads Sentinel noise XML using `roxmltree`
- supports both known Sentinel noise XML forms:
  - `noiseVectorList` / `noiseVector` / `noiseLut`
  - `noiseRangeVectorList` / `noiseRangeVector` / `noiseRangeLut`
- interpolates calibration and noise LUTs over the image grid
- computes calibrated GRD detected-data output for calibrated radiometry:
```plain text
scaled_power = (dn * dn - noise_lut) / (cal_lut * cal_lut)
```

- for non-dB calibrated radiometry, writes:
```plain text
output = scaled_power
```

- for dB calibrated radiometry, writes:
```plain text
if scaled_power < 0: output = -40.0
  else: output = 10.0 * log10(scaled_power)
```

- writes Rust-owned band-sequential little-endian `REAL32` raster data
- infers band names such as `VV` and `VH` from measurement file names
- validates that measurement dimensions match
- parses Sentinel annotation geolocation grid points
- writes source geolocation metadata for later geocoding
- writes Rust-owned metadata with:
  - line count
  - sample count
  - band count
  - `REAL32` data type
  - pixel sizes, currently still narrow/hard-coded at import
  - no-data value `0.0`
  - sensor/mission
  - source geolocation grid

Known limitations:

- Sentinel GRD import performance may need continued optimization because calibrated import does per-pixel calibration/noise work
- `--power` remains intentionally not finalized where legacy Sentinel behavior is ambiguous
- pixel sizes are still narrow/hard-coded at import
- does not write incidence angle bands
- does not implement full legacy Sentinel metadata behavior
- SLC/RAW/OCN remain explicitly unsupported
- legacy MapReady `.meta` compatibility remains out of scope

Production scope decision:

- SLC/RAW/OCN are not needed for the current production Sentinel browse workflow.
- Unsupported Sentinel product types should continue to fail loudly unless production needs change.
- Incidence angle bands remain lower priority than the browse/georeferencing path.

## Current resample behavior

`resample` implements the narrow Sentinel browse subset.

It accepts:

```shell script
resample --square PIXEL_SIZE INPUT_BASE OUTPUT_BASE
```


Current behavior:

- supports Rust-owned `REAL32` rasters
- supports band-sequential little-endian raster layout
- writes Rust-owned `REAL32` rasters
- updates output raster dimensions
- updates pixel size metadata
- preserves band names
- clears output statistics
- clears projected georeferencing metadata
- scales source geolocation grid line/pixel coordinates so they continue to match the resampled raster

The geolocation scaling is important. Sentinel annotation geolocation grid points are tied to raster line/pixel coordinates. When `resample` changes raster dimensions, the grid line/pixel coordinates must be rescaled to remain valid for downstream `asf_geocode`.

Known limitations:

- only the narrow square-pixel resampling path is implemented
- general MapReady resampling behavior is not implemented
- resampling uses the current narrow averaging implementation

## Current asf_geocode behavior

`asf_geocode` now has two narrow Sentinel UTM modes.

Common accepted form:

```shell script
asf_geocode -p utm --zone ZONE --pixel-size PIXEL_SIZE INPUT_BASE OUTPUT_BASE
```


Preserve-orientation form:

```shell script
asf_geocode -p utm --zone ZONE --pixel-size PIXEL_SIZE --preserve-orientation INPUT_BASE OUTPUT_BASE
```


Current common behavior:

- supports Rust-owned `REAL32` rasters
- requires source geolocation metadata from Sentinel import
- supports UTM only
- determines hemisphere from source geolocation latitude
- computes EPSG:
  - northern hemisphere: `32600 + zone`
  - southern hemisphere: `32700 + zone`
- rejects unsupported projections and options loudly
- clears output statistics
- preserves band names

Default north-up UTM mode:

- projects Sentinel source geolocation grid points to UTM
- computes projected bounds
- creates a north-up output UTM grid from projected bounds and requested pixel size
- constructs geolocation grid cells
- maps output UTM pixels back to source image line/pixel coordinates using bilinear inversion per geolocation cell
- samples source raster values using nearest-neighbor sampling
- writes projected north-up georeferencing metadata
- output image content is rotated into the north-up UTM grid

Preserve-orientation mode:

- keeps the image visually source-oriented
- optionally resamples in image row/column space to the requested pixel size
- computes an affine transform from Sentinel geolocation grid points
- writes affine georeferencing metadata
- `asf_export` writes a GeoTIFF `ModelTransformationTag`
- GIS software can place the image rotated/skewed on the map while the TIFF image array remains visually unrotated

Known limitations:

- UTM only
- nearest-neighbor sampling only
- no DEM or terrain correction
- no broad MapReady geocoding behavior
- no explicit support for non-Sentinel source geolocation models
- default north-up geocode is a narrow grid-based approximation
- preserve-orientation affine transform is derived from representative geolocation grid edges and is not a full per-pixel geolocation model
- `--background` and `--band` remain unsupported
- `-p latlon` remains unsupported in the current milestone

Important note:

- `--preserve-orientation` is not the same as true north-up geocoding.
- Use default geocoding when a north-up projected raster is required.
- Use `--preserve-orientation` when visual similarity to the source-oriented/classic browse image is desired while still carrying spatial placement metadata.

## Current adjust_bands behavior

`adjust_bands` implements the narrow Sentinel browse subset.

It accepts:

```shell script
adjust_bands --bands BAND_NAME INPUT_BASE OUTPUT_BASE
```


Current behavior:

- supports Rust-owned `REAL32` rasters
- expects band-sequential little-endian raster layout
- interprets `--bands` as exactly one band name
- matches band names case-insensitively
- preserves the canonical selected band name from input metadata
- writes a new single-band Rust-owned `REAL32` raster
- preserves relevant metadata, including source geolocation/georeferencing when present
- clears output statistics
- rejects unknown band names with a clear error
- rejects comma-separated or whitespace-separated multiple requested bands

Known limitations:

- does not support multiple selected bands
- does not support arithmetic band expressions
- does not support color mapping

Future extension:

- If later Sentinel browse generation needs multiple selected bands, extend `--bands` to accept a comma-separated list while preserving the same option name.

## Current color_browse behavior

`color_browse` implements the narrow Sentinel `--noise-cal` browse-intermediate subset.

It accepts:

```shell script
color_browse --noise-cal VALUE [--tmpDir DIR] CO_POL_INPUT_BASE CROSS_POL_INPUT_BASE OUTPUT_BASE
```


Expected Sentinel browse use:

```shell script
color_browse --noise-cal=-23.5 --tmpDir tmp vv_only vh_only color
```


Current behavior:

- supports Rust-owned `REAL32` rasters
- supports only Sentinel-oriented `--noise-cal` mode
- accepts `--tmpDir` for workflow compatibility
- validates `--tmpDir` when supplied
- expects exactly two input rasters:
  - first input is co-pol single-band raster from `adjust_bands`
  - second input is cross-pol single-band raster from `adjust_bands`
- rejects inputs that are not single-band `REAL32`
- rejects mismatched input dimensions
- rejects mismatched input pixel sizes
- computes the narrow legacy-inspired noise/calibration color decomposition
- writes a Rust-owned three-band `REAL32` raster with bands:
  - `Ps`
  - `Pv`
  - `Pd`
- preserves relevant metadata from the co-pol input, including georeferencing
- clears output statistics because pixel values and band meanings changed

Known limitations:

- does not support one-input color browse
- does not support legacy PALSAR FBD or PLR modes
- does not support legacy `--sentinel` config-file mode
- does not support legacy `--dem`, `--teal`, or `--float`
- does not implement general-purpose color mapping
- does not write final image formats
- does not use temporary files internally

Important note:

- Do not tune `color_browse` channel math to compensate for upstream radiometry problems.

## Current stats behavior

`stats` implements the downstream subset needed before export.

It accepts:

```shell script
stats [--mask VALUE] [--overmeta] [--nometa] [--overstat] [--nostat] INPUT_BASE
```


Expected Sentinel browse use:

```shell script
stats --mask 0.0 --overmeta color
```


Current behavior:

- supports Rust-owned `REAL32` rasters through `asf-raster`
- reads all bands from the input raster
- uses `--mask VALUE` when supplied
- otherwise uses metadata `no_data` as the effective mask when present
- treats non-finite samples as no-data
- computes per-band:
  - valid sample count
  - no-data sample count
  - minimum
  - maximum
  - mean
  - standard deviation
- writes per-band statistics into Rust-owned `.meta` file when `--overmeta` is supplied and `--nometa` is not supplied
- preserves georeferencing metadata
- prints computed statistics to stdout

Known limitations:

- does not write a legacy `.stat` file
- does not compute histograms
- does not compute RMSE
- does not compute percent-valid
- does not support windowed statistics
- does not preserve legacy overwrite prompts or fallback behavior

The Sentinel browse path currently needs `.meta` statistics for downstream export scaling, so `.stat` file support remains deferred unless `asf_export` proves it is needed.

## Current asf_export behavior

`asf_export` has a narrow RGB GeoTIFF output path.

Target command shape:

```shell script
asf_export --format geotiff --byte truncate --rgb "Ps Pv Pd" INPUT_BASE OUTPUT
```


Current behavior:

- supports Rust-owned `REAL32` rasters
- supports `--format geotiff`
- supports `--byte truncate`
- requires `--rgb` for color export
- resolves the three RGB band names case-insensitively against Rust metadata
- reads selected `f32` bands
- interleaves them as RGB
- writes RGB TIFF/GeoTIFF output using the Rust `tiff` crate
- requires projected georeferencing metadata
- writes GeoTIFF spatial tags from Rust metadata
- writes north-up GeoTIFF tags when affine metadata is absent:
  - `ModelPixelScaleTag`
  - `ModelTiepointTag`
  - `GeoKeyDirectoryTag`
- writes affine GeoTIFF tags when affine metadata is present:
  - `ModelTransformationTag`
  - `GeoKeyDirectoryTag`

`--byte truncate` behavior:

- non-finite values become `0`
- values below `0` become `0`
- values above `255` become `255`
- other values are rounded using `value + 0.5`

Known limitations:

- supports `--byte truncate`
- supports `--byte sigma`
- supports RGB export with `--rgb`
- supports grayscale export for one-band input or with `--band BAND_NAME`
- still rejects `--byte minmax` and `--byte none`
- rejects formats other than `geotiff`
- does not support `--band`
- does not yet use metadata statistics for scaling
- does not implement general MapReady export behavior

Important note:

- `asf_export` should not invent georeferencing.
- It should serialize projected georeferencing already present in Rust metadata.

## Current correctness priorities

Completed georeferencing milestones:

- Rust-owned georeferencing metadata
- Sentinel annotation geolocation grid parsing
- source geolocation preservation through import
- source geolocation coordinate scaling through resample
- north-up UTM geocoding milestone
- preserve-orientation affine georeferencing mode
- GeoTIFF spatial tags in export

Remaining important correctness work:

- compare default north-up geocode output against classic MapReady more rigorously
- compare preserve-orientation output against classic browse output more rigorously
- improve geocoding sampling from nearest-neighbor to bilinear where appropriate
- improve edge coverage and no-data handling in north-up geocoding
- review affine transform accuracy for preserve-orientation mode across more products
- improve Sentinel import metadata beyond the current narrow/hard-coded pixel size assumptions
- continue validating Sentinel radiometry against known/classic outputs
- consider export scaling modes only after upstream radiometry is trusted

## Recommended next milestone

The next practical milestone is validation and cleanup, not broad new functionality.

Suggested next steps:

1. Run the full Sentinel browse workflow on more than one Sentinel GRD product.
2. Compare these outputs:
   - Rust default north-up geocode GeoTIFF
   - Rust `--preserve-orientation` GeoTIFF
   - classic MapReady output
3. Use `gdalinfo` or equivalent to verify:
   - CRS/EPSG
   - origin
   - pixel size
   - affine transform when `--preserve-orientation` is used
   - corner coordinates
4. Add small Rust tests around metadata round-tripping:
   - source geolocation metadata
   - north-up georeferencing metadata
   - affine georeferencing metadata
5. Clean up obvious duplicate/formatting issues in metadata writer code if present.
6. Decide whether the next functional improvement should be:
   - bilinear sampling in `asf_geocode`
   - better GeoTIFF/export scaling
   - more Sentinel annotation metadata
   - incidence angle band support

Recommended priority after validation:

- First: metadata/georeferencing cleanup and tests.
- Second: bilinear sampling for `asf_geocode`.
- Third: revisit export scaling, including possible `--byte sigma`.
- Later: incidence angle bands.

## Deferred: incidence angle bands

Incidence angle is now parsed from Sentinel geolocation grid points when present, but no incidence angle raster band is written.

Incidence angle bands remain useful future work, but they are lower priority than the Sentinel browse georeferencing/export path.

A later milestone can:

- reuse parsed annotation geolocation/incidence angle information
- interpolate incidence angle over the raster
- append an incidence angle band if needed by downstream processing

Do not implement incidence angle bands before validation of the current georeferencing and browse workflow unless production needs change.
