# ASF Tools Rust Workspace

This workspace contains a Rust replacement path for selected ASF MapReady
functionality. AI was used extensively in this effort.

Initial target tools:

- `asf_import`
- `resample`
- `asf_geocode`
- `adjust_bands`
- `color_browse`
- `stats`
- `asf_export`

The first `asf_import` implementation intentionally supports only Sentinel
input. Additional formats should be added after the shared raster and metadata
APIs stabilize.

## Build
