#include "asf.h"
#include "asf_view.h"
#include "asf_tiff.h"
#include "asf_nan.h"
#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include <geotiff_support.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

void read_tiff_colormap(const char *tiff_file, meta_colormap *mc);
int read_tiff_rgb_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                            uint32 row, uint32 scanlineSize, int sample_count,
                            int band_r, int band_g, int band_b,
                            tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf);
int ReadScanline_from_ContiguousRGB_TIFF(TIFF *tiff, uint32 row, uint32 sample_count,
                                         int band_r, int band_g, int band_b,
                                         tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf);
int read_tiff_greyscale_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                                  uint32 row, uint32 scanlineSize, int sample_count, int band,
                                  tdata_t *tif_buf);
int interleave_byte_rgbScanlines_to_byte_buff(unsigned char *dest,
                                              tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf,
                                              int band_r, int band_g, int band_b,
                                              uint32 row, uint32 sample_count,
                                              tiff_data_config_t *data_config);
int interleave_rgbScanlines_to_float_buff(float *dest,
                                          tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf,
                                          int band_r, int band_g, int band_b,
                                          uint32 row, uint32 sample_count, tiff_data_config_t *data_config,
                                          int empty);
int copy_byte_scanline_to_byte_buff(unsigned char *dest, tdata_t *tif_buf,
                                    uint32 row, uint32 sample_count, tiff_data_config_t *data_config);
int copy_scanline_to_float_buff(float *dest, tdata_t *tif_buf,
                                uint32 row, uint32 sample_count,
                                tiff_data_config_t *data_config, int empty);
void add_empties(const char *tiff_file, char *band_str, short *num_bands,
                 char *meta_bands, int meta_band_count, int *empty);

typedef struct {
    TIFF  *tiff;              // Data file pointer
    GTIF  *gtif;              // GeoKey data struct pointer
    int   is_rgb;             // Are we doing rgb compositing
    int   band_gs;            // Which band are we using (viewing as greyscale)
    int   band_r;             // Which band we are using for red (viewing as rgb)
    int   band_g;             // Which band we are using for green (viewing as rgb)
    int   band_b;             // Which band we are using for blue (viewing as rgb)
    int   ignore[MAX_BANDS];  // Array of which bands to ignore (blank in the TIFF file)
} ReadTiffClientInfo;

int try_tiff(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return strcmp_case(ext, ".tif") == 0 ||
               strcmp_case(ext, ".tiff") == 0;
    } else if (try_extensions) {
        return try_ext(filename, ".tif") || try_ext(filename, ".tiff");
    }
    return FALSE;
}

//----------------------------------------------------------------------
// handle_tiff_file()

// INPUT:  filename
//              name of a file that try_tiff() has indicated is of this type

// OUTPUT: meta_name
//              preallocated array - name of the metadata file, this will
//              be passed to read_tiff_meta().
//         data_name
//              preallocated array - data file name, this will be passed
//              to open_tiff_data().
//         err
//              not preallocated - only populate this if an error occurs
//              loading the file.

// 1. This function needs to generate the metadata and data file names
//    for the given file, check that they exist, and populate the metadata
//    and data file names.

// 2. When the files don't exist, allocate and populate the "err" string
//    with information about what file was being looked for and couldn't
//    be found.

int handle_tiff_file(const char *filename, char *meta_name, char *data_name,
                     char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_tif_ext = has_ext && strcmp_case(ext,".tif")==0;
    int has_tiff_ext = has_ext && strcmp_case(ext,".tiff")==0;

    if (!has_ext) {
        has_tif_ext = try_ext(filename, ".tif");
        if (!has_tif_ext)
            has_tiff_ext = try_ext(filename, ".tiff");
    }

    if (has_tif_ext || has_tiff_ext)
    {
        char *d=NULL;
        if (has_ext)
            d = STRDUP(filename);
        else if (has_tif_ext)
            d = appendExt(filename, ".tif");
        else if (has_tiff_ext)
            d = appendExt(filename, ".tiff");
        assert(d);

        strcpy(meta_name, d);
        strcpy(data_name, d);
        free(d);

        int ret;
        if (!fileExists(data_name)) {
            int l = sizeof(char)*strlen(filename)+255;
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening TIFF file: %s\n", data_name);
            ret = FALSE;
        }
        else
            ret = TRUE;

        return ret;
    }
    else {
        assert(!try_ext(filename, ".tif"));
        int l = sizeof(char)*strlen(filename)+255;
        *err = MALLOC(l);
        snprintf(*err, l, "Failed to open %s as a TIFF File.\n", filename);
        return FALSE;
    }

    *err = STRDUP("File type not supported.\n");
    return FALSE;
}

//----------------------------------------------------------------------
// read_tiff_meta()

// Create & return a meta_parameters structure from the given file.
// Return NULL if the metadata could not be read, or there is no
// metadata for this particular format.

// The only place this is called is in "read.c/read_file()", so you can
// customize the signature to fit your needs.  The read_jpeg client, for
// example, eliminated this function entirely, and returned metadata
// with the "open_" function, below.
meta_parameters *read_tiff_meta(const char *meta_name, ClientInterface *client, char *filename)
{
    ReadTiffClientInfo *info = (ReadTiffClientInfo *)client->read_client_info;
    int i;
    char band_str[256];
    meta_parameters *meta = NULL;
    TIFF *tiff = NULL;
    short sample_format;    // TIFFTAG_SAMPLEFORMAT
    short bits_per_sample;  // TIFFTAG_BITSPERSAMPLE
    short planar_config;    // TIFFTAG_PLANARCONFIG
    short num_bands=0;
    int is_scanline_format; // False if tiled or strips > 1 TIFF file format
    int is_palette_color_tiff;
    data_type_t data_type;

    tiff = XTIFFOpen(meta_name, "r");
    if (tiff) {
      get_tiff_data_config(tiff,
                           &sample_format, // TIFF type (uint, int, float)
                           &bits_per_sample, // 8, 16, or 32
                           &planar_config, // Contiguous (RGB or RGBA) or separate (band sequential)
                           &data_type, // BYTE, INTEGER16, INTEGER32, or REAL32 ...no complex
                           &num_bands, // Initial number of bands
                           &is_scanline_format,
                           &is_palette_color_tiff,
                           REPORT_LEVEL_NONE);

      XTIFFClose(tiff);
    }
    else {
      return NULL;
    }

    // Read the metadata from the tiff tags and geokeys
    for (i=0; i<MAX_BANDS; i++) info->ignore[i]=0; // Default to ignoring no bands
    if (isGeotiff(meta_name)) {
        // Read the GeoTIFF metadata from the GeoTIFF file
        meta = read_generic_geotiff_metadata(meta_name, info->ignore, NULL);
        if (meta->general->band_count > 1 &&
            strncmp(meta->general->bands, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) == 0)
        {
            int band;
            for (band=0; band < meta->general->band_count; band++) {
            if (band == 0) {
                sprintf(meta->general->bands, "%02d", band + 1);
            }
            else {
                sprintf(meta->general->bands, "%s,%02d", meta->general->bands, band + 1);
            }
            }
        }
        add_empties(meta_name, band_str, &num_bands, meta->general->bands,
                    meta->general->band_count, info->ignore);
        if (num_bands > meta->general->band_count && strlen(band_str) > strlen(meta->general->bands)) {
            strcpy(meta->general->bands, band_str);
            meta->general->band_count = num_bands;
        }

        meta->insar = populate_insar_metadata(meta_name);

    }
    else {
        // The TIFF is not a GeoTIFF, so populate the metadata with generic
        // information
        meta = raw_init ();
        meta->optical = NULL;
        meta->thermal = NULL;
        meta->projection = NULL;
        meta->stats = NULL;
        meta->state_vectors = NULL;
        meta->location = meta_location_init ();
        meta->stVec = NULL;
        meta->geo = NULL;
        meta->ifm = NULL;
        meta->info = NULL;
        meta->colormap = NULL;

        tiff = XTIFFOpen(meta_name, "r");
        if (tiff) {
            uint32 width;
            uint32 height;
            TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &height);
            TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &width);
            if (height <= 0 || width <= 0) {
                asfPrintError("Invalid height and width parameters in TIFF file,\n"
                        "Height = %ld, Width = %ld\n", height, width);
            }
            meta->general->data_type = data_type;
            meta->general->image_data_type = IMAGE;
            strcpy(meta->general->sensor, MAGIC_UNSET_STRING);
            strcpy(meta->general->basename, meta_name);
            meta->general->line_count = height;
            meta->general->sample_count = width;
            meta->general->start_line = 0;
            meta->general->start_sample = 0;
            meta->general->x_pixel_size = 1.0;
            meta->general->y_pixel_size = 1.0;
            meta->general->band_count = num_bands;

            XTIFFClose(tiff);
        }
    }

    // If the tiff file is a single-band image with RGB color map, then store
    // the color map as an ASF style look-up table
    if (is_palette_color_tiff) {
      meta->colormap = meta_colormap_init();
      meta_colormap *mc = meta->colormap;
      strcpy(mc->look_up_table, EMBEDDED_TIFF_COLORMAP_LUT_FILE);
      strcpy(mc->band_id, "01");
      mc->num_elements = 1<<bits_per_sample;
      mc->rgb = (meta_rgb *)CALLOC(mc->num_elements, sizeof(meta_rgb));
      read_tiff_colormap(meta_name, mc);
      char lut_file[256];
      char *lut_loc = (char *)MALLOC(sizeof(char)*(strlen(get_asf_share_dir())+64));
      sprintf(lut_loc, "%s%clook_up_tables", get_asf_share_dir(), DIR_SEPARATOR);
      sprintf(lut_file,"%s%c%s", lut_loc, DIR_SEPARATOR, EMBEDDED_TIFF_COLORMAP_LUT_FILE);
      FILE *lutFP = (FILE *)FOPEN(lut_file, "wt");
      fprintf(lutFP, "# Look up table type: %s\n", mc->look_up_table);
      fprintf(lutFP, "# Originating source: %s\n", meta_name);
      fprintf(lutFP, "# Index   Red   Green   Blue\n");
      for (i=0; i<mc->num_elements; i++) {
        fprintf(lutFP, "%03d    %03d    %03d    %03d\n",
                i, mc->rgb[i].red, mc->rgb[i].green, mc->rgb[i].blue);
      }
      fprintf(lutFP, "\n");
      FCLOSE(lutFP);
    }

    return meta;
}

//----------------------------------------------------------------------
// read_tiff_client()

// A function that will read the given rows from the file.

// [in] row_start: The row number of the desired row.
// [in] n_rows_to_get: How many rows to read in.
// [out] dest: where the data should be put.  Allocated by the caller.
//             this is an rgb buffer, [rgb|rgb|rgb|rgb...]
// [in] read_client_info: A pointer to the ReadTiffClientInfo struct you
//                        created in open_tiff_data().
// [in] meta: The metdata from read_tiff_meta().

// return TRUE on success, FALSE on failure.

// You aren't allowed to customize the interface, here -- any additional
// info you want needs to be placed into the ReadTiffClientInfo struct.
int read_tiff_client(int row_start, int n_rows_to_get,
                     void *dest_void, void *read_client_info,
                     meta_parameters *meta, int dest_data_type)
{
  data_type_t data_type;
  tiff_data_config_t data_config;
  int num_bands, is_scanline_format, is_palette_color_tiff;
  uint32 row;
  ReadTiffClientInfo *info = (ReadTiffClientInfo*)read_client_info;
  TIFF *tiff = info->tiff;
  meta_general *mg = meta->general;

  // Determine what type of TIFF this is (scanline/strip/tiled)
  if (get_tiff_data_config(tiff,
                           &data_config.sample_format,
                           &data_config.bits_per_sample,
                           &data_config.planar_config,
                           &data_type,
                           &data_config.samples_per_pixel,
                           &is_scanline_format,
                           &is_palette_color_tiff,
                           REPORT_LEVEL_NONE))
  {
    return FALSE;
  }
  num_bands = data_config.samples_per_pixel;

  tiff_type_t tiffInfo;
  get_tiff_type(tiff, &tiffInfo);
  if (tiffInfo.imageCount > 1) {
    ; // Only first image in multi-image files will be utilized - WARN the user here?
  }
  if (tiffInfo.imageCount < 1) {
    // TIFF contains zero images ...fail
    return FALSE;
  }
  if (tiffInfo.format != SCANLINE_TIFF &&
      tiffInfo.format != STRIP_TIFF    &&
      tiffInfo.format != TILED_TIFF)
  {
    // Unrecognized TIFF type
    return FALSE;
  }
  if (tiffInfo.volume_tiff) {
    // 3-dimensional (a 'volume tiff') found ...this is unsupported
    return FALSE;
  }
  if (num_bands > 1 &&
      data_config.planar_config != PLANARCONFIG_CONTIG &&
      data_config.planar_config != PLANARCONFIG_SEPARATE)
  {
    // Invalid planar configuration setting found in TIFF file...
    return FALSE;
  }

  uint32 scanlineSize = TIFFScanlineSize(tiff);
  if (scanlineSize <= 0) {
    // Invalid scanline size found in TIFF file...
    return FALSE;
  }
  tdata_t *tif_buf  = _TIFFmalloc(scanlineSize); // TIFF read buffer (interleaved bands)
  tdata_t *rtif_buf = _TIFFmalloc(scanlineSize); // TIFF read buffer (red band)
  tdata_t *gtif_buf = _TIFFmalloc(scanlineSize); // TIFF read buffer (green band)
  tdata_t *btif_buf = _TIFFmalloc(scanlineSize); // TIFF read buffer (blue band)
  if (!tif_buf  ||
      !rtif_buf ||
      !gtif_buf ||
      !btif_buf)
  {
    // Cannot allocate tiff read buffers...
    return FALSE;
  }

  // Convenience flags and values
  int is_rgb       = (dest_data_type == RGB_BYTE  ||
                      dest_data_type == RGB_FLOAT ||
                      info->is_rgb)       ? 1 : 0;
  int dest_is_byte = (dest_data_type == GREYSCALE_BYTE || dest_data_type == RGB_BYTE) ? 1 : 0;
  int band_r  = info->band_r;
  int band_g  = info->band_g;
  int band_b  = info->band_b;
  int band_gs = info->band_gs;

  if (dest_is_byte)
  {
    // FIXME: Resample if there are more than 8 bits per pixel.  But for now,
    // just return a failure
    if (data_config.bits_per_sample != 8) return FALSE;

    // Read byte image (8 bits per element)
    unsigned char *dest = (unsigned char*)dest_void;
    // Blackenize the buffer
    if (is_rgb)
      memset(dest, 0, n_rows_to_get*mg->sample_count*3);
    else
      memset(dest, 0, n_rows_to_get*mg->sample_count);
    // Populate the buffer with actual data
    uint32 row_offset=row_start;
    for (row=0; row < n_rows_to_get && row+row_offset < mg->line_count; row++) {
      if (is_rgb) {
        // Read a scanline and populate r, g, and b tiff buffers
        // NOTE: Empty bands will have the no_data value populated in the tiff buffer
        read_tiff_rgb_scanline(info->tiff, tiffInfo.format, &data_config,
                               row + row_offset, scanlineSize, mg->sample_count,
                               band_r, band_g, band_b,
                               rtif_buf, gtif_buf, btif_buf);
        // Interleave the rgb values into an rgb buffer
        interleave_byte_rgbScanlines_to_byte_buff(dest,
                                                  rtif_buf, gtif_buf, btif_buf,
                                                  band_r, band_g, band_b,
                                                  row, mg->sample_count, &data_config);
      }
      else { // is greyscale
        // Read a scanline into a tiff buffer (using first non-blank band as the greyscale image)
        // NOTE: Since displaying a greyscale band specifically selects a band, empty or not, the
        // selected band is read as-is.
        read_tiff_greyscale_scanline(info->tiff, tiffInfo.format, &data_config,
                                     row + row_offset, scanlineSize, mg->sample_count, band_gs,
                                     tif_buf);
        copy_byte_scanline_to_byte_buff(dest, tif_buf, row, mg->sample_count, &data_config);
      }
    }
  }
  else if (mg->data_type != COMPLEX_BYTE      &&
           mg->data_type != COMPLEX_INTEGER16 &&
           mg->data_type != COMPLEX_INTEGER32 &&
           mg->data_type != COMPLEX_REAL32    &&
           mg->data_type != COMPLEX_REAL64)
  {
    // Read image (any non-complex type)
    float *dest = (float*)dest_void;
    // Blackenize the buffer
    if (is_rgb)
      memset(dest, 0, n_rows_to_get*mg->sample_count*3*sizeof(float));
    else
      memset(dest, 0, n_rows_to_get*mg->sample_count*sizeof(float));
    // Populate the buffer with actual data
    uint32 row_offset=row_start;
    for (row=0; row < n_rows_to_get && row + row_offset < mg->line_count; row++) {
      if (is_rgb) {
        // Read a scanline and populate r, g, and b tiff buffers
        // NOTE: Empty bands will have the no_data value populated in the tiff buffer
        read_tiff_rgb_scanline(info->tiff, tiffInfo.format, &data_config,
                               row + row_offset, scanlineSize, mg->sample_count,
                               band_r, band_g, band_b,
                               rtif_buf, gtif_buf, btif_buf);
        // Interleave the rgb values into an rgb buffer
        interleave_rgbScanlines_to_float_buff(dest,
            rtif_buf, gtif_buf, btif_buf,
            band_r, band_g, band_b,
            row, mg->sample_count, &data_config, info->ignore[band_gs]);
      }
      else { // is greyscale
        // Read a scanline into a tiff buffer (using first non-blank band as the greyscale image)
        // NOTE: Since displaying a greyscale band specifically selects a band, empty or not, the
        // selected band is read as-is.
        read_tiff_greyscale_scanline(info->tiff, tiffInfo.format, &data_config,
                                     row + row_offset, scanlineSize, mg->sample_count, band_gs,
                                     tif_buf);
        copy_scanline_to_float_buff(dest, tif_buf, row, mg->sample_count, &data_config, info->ignore[band_gs]);
      }
    }
  } // End of reading a non-BYTE type TIFF
  else {
    // Complex data found (rare in a tiff, but possible) ...this is an unsupported type
    _TIFFfree(tif_buf);
    _TIFFfree(rtif_buf);
    _TIFFfree(gtif_buf);
    _TIFFfree(btif_buf);
    return FALSE;
  }

  _TIFFfree(tif_buf);
  _TIFFfree(rtif_buf);
  _TIFFfree(gtif_buf);
  _TIFFfree(btif_buf);
  return TRUE;
}

//----------------------------------------------------------------------
// get_tiff_thumbnail_data()

// This is a performance improvement function - reads in a subset
// of the data to generate a thumbnail.  Theoretically it isn't needed
// but we can get the program started up much faster by having
// dedicated code to reading in the subset of data necessary to
// generate the preview (thumbnail) image.

// Read in a thumb_size_x (columns) by thumb_size_y (rows)
// thumbnail of the data, return TRUE on success, FALSE on failure.

// [in] fp: An already open file pointer to the data file.
// [in] thumb_size_x: Columns
// [in] thumb_size_y: Rows
// [in] meta: The metdata from read_tiff_meta().
// [in] read_client_info: A pointer to the ReadTiffClientInfo struct you
//                        created in open_tiff_data().
// [out] dest: where the data should be put.  Allocated by the caller.

// You should use an asfPercentMeter while reading in the data.

// You aren't allowed to customize the interface, here -- any additional
// info you want needs to be placed into the ReadTiffClientInfo struct.

// However, you are allowed to completely eliminate this -- set the
// thumbnail function pointer to NULL.  In that case, the read_tiff code
// will be called to generate the thumbnail.

int get_tiff_thumbnail_data(FILE *fp, int thumb_size_x,
                            int thumb_size_y, meta_parameters *meta,
                            void *read_client_info, void *dest_void)
{
  // pick one of these and populate it:
  //  float *dest = (float*)dest_void;
  //  unsigned char *dest = (unsigned char*)dest_void;

//  ReadTiffClientInfo *info = (ReadTiffClientInfo*)read_client_info;
//  TIFF *tiff = info->tiff;

  // here is where you populate "dest"
  return FALSE;
}

//----------------------------------------------------------------------
// free_tiff_client_info()

// Frees the client info structure

// You aren't allowed to customize the interface, here -- the cache
// has a pointer to this function that is typedef'ed.
void free_tiff_client_info(void *read_client_info)
{
    ReadTiffClientInfo *info = (ReadTiffClientInfo*)read_client_info;
    if (info->gtif) GTIFFree(info->gtif);
    if (info->tiff) XTIFFClose(info->tiff);
    FREE(info);
}

//----------------------------------------------------------------------
// open_tiff_data()

// Open the data file, populate the read_client_info structure,
// and set pointers to the read_tiff_client and get_tiff_thumbnail_data
// functions.

// [in] data_name: the data file name, returned from handle_tiff_file().
// [in] meta_name: the meta file name, returned from handle_tiff_file().
// [in] band: Command-line band argument.  Ignore if you don't support
//            bands.  Will be NULL if no band argument was supplied, in
//            which case you should load the first band.
// [in] meta: The metdata from read_tiff_meta().
// [out] client: Structure of pointers to client functions,
//               including the read_client_info.  You should allocate
//               read_client_info, but ClientInterface is pre-allocated

// The only place this is called is in "read.c/read_file()"

int open_tiff_data(const char *data_name, const char *band, ClientInterface *client)
{
  int i;
  char *band_selection = STRDUP(band);
  ReadTiffClientInfo *info = MALLOC(sizeof(ReadTiffClientInfo));

  // ensure any custom tags are installed & recognized by libtiff
  _XTIFFInitialize();

  info->tiff = XTIFFOpen(data_name, "r");
  info->gtif = GTIFNew(info->tiff);
  info->is_rgb = FALSE; // Default to one band only
  info->band_gs = 0; // Default to first band
  info->band_r = 0; // Default to first band
  info->band_g = 0; // Default to first band
  info->band_b = 0; // Default to first band
  for (i=0; i<MAX_BANDS; i++) info->ignore[i] = 0;

  client->read_client_info = info;
  client->read_fn = read_tiff_client;
  client->thumb_fn = NULL;// get_tiff_thumbnail_data;
  client->free_fn = free_tiff_client_info;
  client->require_full_load = FALSE; // TIFF files are random-access.  No need to read it all if you don't want to
  client->data_type = UNDEFINED; // Populated below (REQUIRED)

  if (info->tiff == NULL) return FALSE;
  if (info->gtif == NULL) return FALSE;

  // Get the tiff data so we can figure out what we're looking at
  data_type_t data_type;
  tiff_data_config_t data_config;
  int is_scanline_format, is_palette_color_tiff;
  if (get_tiff_data_config(info->tiff,
      &data_config.sample_format,
      &data_config.bits_per_sample,
      &data_config.planar_config,
      &data_type,
      &data_config.samples_per_pixel,
      &is_scanline_format,
      &is_palette_color_tiff,
      REPORT_LEVEL_NONE))
  {
    return FALSE;
  }
  char *citation = NULL;
  int citation_length;
  int typeSize;
  tagtype_t citation_type;
  citation_length = GTIFKeyInfo(info->gtif, GTCitationGeoKey, &typeSize, &citation_type);
  if (citation_length > 0) {
    citation = MALLOC ((citation_length) * typeSize);
    GTIFKeyGet (info->gtif, GTCitationGeoKey, citation, 0, citation_length);
    asfPrintStatus("\nCitation: %s\n\n", citation);
  }
  else {
    citation_length = GTIFKeyInfo(info->gtif, PCSCitationGeoKey, &typeSize, &citation_type);
    if (citation_length > 0) {
      citation = MALLOC ((citation_length) * typeSize);
      GTIFKeyGet (info->gtif, PCSCitationGeoKey, citation, 0, citation_length);
      asfPrintStatus("\nCitation: %s\n\n", citation);
    }
    else {
      asfPrintStatus("\nCitation: The GeoTIFF citation string is MISSING (Not req'd)\n\n");
    }
  }
  char *band_str = (char *) MALLOC(100*sizeof(char));
  int num_found_bands=0;
  int *empty = (int*)CALLOC(data_config.samples_per_pixel, sizeof(int));
  if (citation_length > 0) {
    int i, num_empty;
    short tmp_num_bands;
    char tmp_band_str[256];
    char *tmp_citation = (citation != NULL) ? STRDUP(citation) : NULL;
    get_bands_from_citation(&num_found_bands, &band_str, empty, tmp_citation,
                             data_config.samples_per_pixel);
    if (num_found_bands < 1) {
      // No bands in citation string ...make some up instead
      if (data_config.samples_per_pixel == 1) {
        strcpy(band_str, "01");
      }
      else {
        for (i=0; i<data_config.samples_per_pixel; i++) {
          if (i==0) {
            sprintf(band_str, "%02d", i + 1);
          }
          else {
            sprintf(band_str, "%s,%02d", band_str, i + 1);
          }
        }
      }
    }
    for (i=0, num_empty=0; i < data_config.samples_per_pixel; i++) num_empty += empty[i] ? 1 : 0;
    if (num_empty > 0) {
      add_empties(data_name, tmp_band_str, &tmp_num_bands,
                  band_str, num_found_bands, empty);
      if (tmp_num_bands > num_found_bands && strlen(tmp_band_str) > strlen(band_str)) {
        strcpy(band_str, tmp_band_str);
        num_found_bands = tmp_num_bands;
      }
    }
    FREE(tmp_citation);
  }
  else {
    if (data_config.samples_per_pixel == 1) {
      strcpy(band_str, "01");
    }
    else {
      for (i=0; i<data_config.samples_per_pixel; i++) {
        if (i==0) {
          sprintf(band_str, "%02d", i + 1);
        }
        else {
          sprintf(band_str, "%s,%02d", band_str, i + 1);
        }
      }
    }
  }

  if (band_selection == NULL && data_config.samples_per_pixel == 3) {
    // If no band is given and it's a 3-band image, then assume it is a normal
    // RGB image of some kind ...display it in color
    FREE(band_selection);
    band_selection = STRDUP(band_str);
  }
  if (band_selection) {
    char *r, *g, *b;

    if (split3(band_selection, &r, &g, &b, ',')) {
      // Looks like we were given 3 bands -- so, we are doing rgb
      int tmp_num_bands = num_found_bands > 0 ? num_found_bands : data_config.samples_per_pixel;
      info->band_r = get_band_number(band_str,
                                     tmp_num_bands, r);
      if (info->band_r < 0)
        asfPrintWarning("Red band '%s' not found.\n", r);
      else
        asfPrintStatus("Red band is band #%d: %s\n",
                      info->band_r+1, r);

      info->band_g = get_band_number(band_str,
                                     tmp_num_bands, g);
      if (info->band_g < 0)
        asfPrintWarning("Green band '%s' not found.\n", g);
      else
        asfPrintStatus("Green band is band #%d: %s\n",
                      info->band_g+1, g);

      info->band_b = get_band_number(band_str,
                                     tmp_num_bands, b);
      if (info->band_b < 0)
        asfPrintWarning("Blue band '%s' not found.\n", b);
      else
        asfPrintStatus("Blue band is band #%d: %s\n",
                      info->band_b+1, b);

      if (info->band_r < 0 && info->band_g < 0 && info->band_b < 0) {
        // none of the bands were found
        if (band_selection) FREE (band_selection);
        return FALSE;
      }

      info->is_rgb = TRUE;
      FREE(r); FREE(g); FREE(b);

      set_bands_rgb(info->band_r, info->band_g, info->band_b);
    } else {
      // Single band name given
      if (strncmp(uc(band_selection), "EMPTY", 5) == 0) {
        // show the first BLANK band you find...
        info->band_gs = 0;
        while (!empty[info->band_gs] && info->band_gs < data_config.samples_per_pixel) info->band_gs++;
        if (info->band_gs == data_config.samples_per_pixel) {
          asfPrintWarning("Band '%s' not found.\n", band_selection);
          if (band_selection) FREE (band_selection);
          return FALSE;
        }
      }
      else {
        int tmp_num_bands = num_found_bands > 0 ? num_found_bands : data_config.samples_per_pixel;
        info->band_gs = get_band_number(band_str,
                                        tmp_num_bands, (char*)band);
      }
      if (info->band_gs < 0) {
        asfPrintWarning("Band '%s' not found.\n", band);
        if (band_selection) FREE (band_selection);
        return FALSE;
      } else {
        asfPrintStatus("Reading band #%d: %s\n",
                       info->band_gs+1, band_selection);
      }

      set_bands_greyscale(info->band_gs);
    }
  }

  if (data_type == BYTE)
    client->data_type = info->is_rgb ? RGB_BYTE : GREYSCALE_BYTE;
  else
    client->data_type = info->is_rgb ? RGB_FLOAT : GREYSCALE_FLOAT;
  FREE (band_selection);

  FREE(citation);
  FREE(empty);
  FREE(band_str);

  return TRUE;
}

int read_tiff_rgb_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                            uint32 row, uint32 scanlineSize, int sample_count,
                            int band_r, int band_g, int band_b,
                            tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf)
{
  // NOTE: num_bands may not be greater than 1 ...open_tiff_data() decides what to
  // assign into band_r, band_g, and band_b.  They may all be the same, all different,
  // or some combination depending on what the GUI asked for versus what was available
  // in the TIFF file.  All code called after open_tiff_data() should assume that if
  // RGB is desired that the 3 band assignments are taken care of appropriate to the
  // situation.
  int num_bands = data_config->samples_per_pixel;
  if (num_bands < 1 ||
      band_r < 0 || band_r > num_bands - 1 ||
      band_g < 0 || band_g > num_bands - 1 ||
      band_b < 0 || band_b > num_bands - 1)
  {
    return FALSE;
  }
  switch (format) {
    case SCANLINE_TIFF:
      if (data_config->planar_config == PLANARCONFIG_CONTIG) {
        ReadScanline_from_ContiguousRGB_TIFF(tiff, row, sample_count,
                                             band_r, band_g, band_b,
                                             rtif_buf, gtif_buf, btif_buf);
      }
      else {
        TIFFReadScanline(tiff, rtif_buf, row, band_r); // Red band
        TIFFReadScanline(tiff, gtif_buf, row, band_g); // Green band
        TIFFReadScanline(tiff, btif_buf, row, band_b); // Blue band
      }
      break;
    case STRIP_TIFF:
      ReadScanline_from_TIFF_Strip(tiff, rtif_buf, row, band_r); // Red band
      ReadScanline_from_TIFF_Strip(tiff, gtif_buf, row, band_g); // Green band
      ReadScanline_from_TIFF_Strip(tiff, btif_buf, row, band_b); // Blue band
      break;
    case TILED_TIFF:
      ReadScanline_from_TIFF_TileRow(tiff, rtif_buf, row, band_r); // Red band
      ReadScanline_from_TIFF_TileRow(tiff, gtif_buf, row, band_g); // Green band
      ReadScanline_from_TIFF_TileRow(tiff, btif_buf, row, band_b); // Blue band
      break;
    default:
      // This code should never execute
      return FALSE;
      break;
  }

  return TRUE;
}

int read_tiff_greyscale_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                                  uint32 row, uint32 scanlineSize, int sample_count, int band,
                                  tdata_t *tif_buf)
{
  switch (format) {
    case SCANLINE_TIFF:
      if (data_config->planar_config == PLANARCONFIG_CONTIG) {
        TIFFReadScanline(tiff, tif_buf, row, 0);
      }
      else {
        TIFFReadScanline(tiff, tif_buf, row, band);
      }
      break;
    case STRIP_TIFF:
      ReadScanline_from_TIFF_Strip(tiff, tif_buf, row, band);
      break;
    case TILED_TIFF:
      ReadScanline_from_TIFF_TileRow(tiff, tif_buf, row, band);
      break;
    default:
      // This code should never execute
      return FALSE;
      break;
  }

  return TRUE;
}

int interleave_byte_rgbScanlines_to_byte_buff(unsigned char *dest,
                                              tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf,
                                              int band_r, int band_g, int band_b,
                                              uint32 row, uint32 sample_count, tiff_data_config_t *data_config)
{
  if (data_config->bits_per_sample != 8) return FALSE; // Bail if not byte data

  int sample;
  int spp = 3;
  int ns = sample_count;
  for (sample=0; sample < sample_count; sample++) {
    switch(data_config->sample_format) {
      case SAMPLEFORMAT_UINT:
        dest[row*ns*spp+sample*spp]   = (unsigned char)(((uint8 *)rtif_buf)[sample]); // Red component
        dest[row*ns*spp+sample*spp+1] = (unsigned char)(((uint8 *)gtif_buf)[sample]); // Green component
        dest[row*ns*spp+sample*spp+2] = (unsigned char)(((uint8 *)btif_buf)[sample]); // Blue component
        break;
      case SAMPLEFORMAT_INT:
        dest[row*ns*spp+sample*spp]   = (unsigned char)(((int8 *)rtif_buf)[sample]); // Red component
        dest[row*ns*spp+sample*spp+1] = (unsigned char)(((int8 *)gtif_buf)[sample]); // Green component
        dest[row*ns*spp+sample*spp+2] = (unsigned char)(((int8 *)btif_buf)[sample]); // Blue component
        break;
      default:
        // No such thing as an 8-bit IEEE float
        return FALSE;
        break;
    }
  }

  return TRUE;
}

int interleave_rgbScanlines_to_float_buff(float *dest,
                                          tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf,
                                          int band_r, int band_g, int band_b,
                                          uint32 row, uint32 sample_count, tiff_data_config_t *data_config,
                                          int empty)
{
  int sample;
  int spp=3;
  int ns=sample_count;
  for (sample=0; sample < sample_count; sample++) {
    switch (data_config->bits_per_sample) {
      case 8:
        switch(data_config->sample_format) {
          case SAMPLEFORMAT_UINT:
            dest[row*ns*spp+sample*spp]   = (float)(((uint8 *)rtif_buf)[sample]); // Red component
            dest[row*ns*spp+sample*spp+1] = (float)(((uint8 *)gtif_buf)[sample]); // Green component
            dest[row*ns*spp+sample*spp+2] = (float)(((uint8 *)btif_buf)[sample]); // Blue component
            break;
          case SAMPLEFORMAT_INT:
            dest[row*ns*spp+sample*spp]   = (float)(((int8 *)rtif_buf)[sample]); // Red component
            dest[row*ns*spp+sample*spp+1] = (float)(((int8 *)gtif_buf)[sample]); // Green component
            dest[row*ns*spp+sample*spp+2] = (float)(((int8 *)btif_buf)[sample]); // Blue component
            break;
          default:
            // No such thing as an 8-bit IEEE float
            return FALSE;
            break;
        }
        break;
      case 16:
        switch(data_config->sample_format) {
          case SAMPLEFORMAT_UINT:
            dest[row*ns*spp+sample*spp]   = (float)(((uint16 *)rtif_buf)[sample]); // Red component
            dest[row*ns*spp+sample*spp+1] = (float)(((uint16 *)gtif_buf)[sample]); // Green component
            dest[row*ns*spp+sample*spp+2] = (float)(((uint16 *)btif_buf)[sample]); // Blue component
            break;
          case SAMPLEFORMAT_INT:
            dest[row*ns*spp+sample*spp]   = (float)(((int16 *)rtif_buf)[sample]); // Red component
            dest[row*ns*spp+sample*spp+1] = (float)(((int16 *)gtif_buf)[sample]); // Green component
            dest[row*ns*spp+sample*spp+2] = (float)(((int16 *)btif_buf)[sample]); // Blue component
            break;
          default:
            // No such thing as an 16-bit IEEE float
            return FALSE;
            break;
        }
        break;
      case 32:
        switch(data_config->sample_format) {
          case SAMPLEFORMAT_UINT:
            dest[row*ns*spp+sample*spp]   = (float)(((uint32 *)rtif_buf)[sample]); // Red component
            dest[row*ns*spp+sample*spp+1] = (float)(((uint32 *)gtif_buf)[sample]); // Green component
            dest[row*ns*spp+sample*spp+2] = (float)(((uint32 *)btif_buf)[sample]); // Blue component
            break;
          case SAMPLEFORMAT_INT:
            dest[row*ns*spp+sample*spp]   = (float)(((long *)rtif_buf)[sample]); // Red component
            dest[row*ns*spp+sample*spp+1] = (float)(((long *)gtif_buf)[sample]); // Green component
            dest[row*ns*spp+sample*spp+2] = (float)(((long *)btif_buf)[sample]); // Blue component
            break;
          case SAMPLEFORMAT_IEEEFP:
            dest[row*ns*spp+sample*spp]   = !empty ? (float)(((float *)rtif_buf)[sample]) : 0.0; // Red component
            dest[row*ns*spp+sample*spp+1] = !empty ? (float)(((float *)gtif_buf)[sample]) : 0.0; // Green component
            dest[row*ns*spp+sample*spp+2] = !empty ? (float)(((float *)btif_buf)[sample]) : 0.0; // Blue component
            break;
          default:
            return FALSE;
            break;
        }
        break;
      default:
        // Unsupported bits_per_sample encountered...
        return FALSE;
        break;
    }
  }

  return TRUE;
}

// copy_byte_scanline_to_byte_buff() is for greyscale only, i.e. tif_buff is an array of bytes,
// one greyscale byte per pixel.  The destination buffer is also a byte buffer.
int copy_byte_scanline_to_byte_buff(unsigned char *dest, tdata_t *tif_buf,
                                    uint32 row, uint32 sample_count, tiff_data_config_t *data_config)
{
  int sample;
  if (data_config->bits_per_sample != 8) return FALSE;

  for (sample=0; sample<sample_count; sample++) {
    switch (data_config->sample_format) {
      case SAMPLEFORMAT_UINT:
        dest[row*sample_count+sample] = (unsigned char)(((uint8 *)tif_buf)[sample]);
        break;
      case SAMPLEFORMAT_INT:
        dest[row*sample_count+sample] = (unsigned char)(((int8 *)tif_buf)[sample]);
        break;
      default:
        return FALSE;
        break;
      }
  }

  return TRUE;
}

// copy_scanline_to_float_buff() is for greyscale only, i.e. tif_buff is an array of sample of
// any supported data type, one data element per pixel, and the destination buffer is a float buffer.
int copy_scanline_to_float_buff(float *dest, tdata_t *tif_buf,
                                uint32 row, uint32 sample_count,
                                tiff_data_config_t *data_config, int empty)
{
  int sample;

  for (sample=0; sample<sample_count; sample++) {
    if (empty) {
      dest[row*sample_count+sample] = FLOAT_IMAGE_DEFAULT_MASK;
    }
    else {
      switch (data_config->bits_per_sample) {
        case 8:
          switch (data_config->sample_format) {
            case SAMPLEFORMAT_UINT:
              dest[row*sample_count+sample] = (float)(((uint8 *)tif_buf)[sample]);
              break;
            case SAMPLEFORMAT_INT:
              dest[row*sample_count+sample] = (float)(((int8 *)tif_buf)[sample]);
              break;
            default:
              return FALSE;
              break;
          }
          break;
        case 16:
          switch (data_config->sample_format) {
            case SAMPLEFORMAT_UINT:
              dest[row*sample_count+sample] = (float)(((uint16 *)tif_buf)[sample]);
              break;
            case SAMPLEFORMAT_INT:
              dest[row*sample_count+sample] = (float)(((int16 *)tif_buf)[sample]);
              break;
            default:
              return FALSE;
              break;
          }
          break;
        case 32:
          switch (data_config->sample_format) {
            case SAMPLEFORMAT_UINT:
              dest[row*sample_count+sample] = (float)(((uint32 *)tif_buf)[sample]);
              break;
            case SAMPLEFORMAT_INT:
              dest[row*sample_count+sample] = (float)(((long *)tif_buf)[sample]);
              break;
            case SAMPLEFORMAT_IEEEFP:
              dest[row*sample_count+sample] = (float)(((float *)tif_buf)[sample]);
              break;
            default:
              return FALSE;
              break;
          }
          break;
        default:
          return FALSE;
          break;
      }
    }
  }

  return TRUE;
}

// ReadScanline_from_ContiguousRGB_TIFF()
//
//   For scanline type tiffs in contiguous RGB format (scanlines are rgbrgb...) only
//   ...Not for striped or tiled tiffs.  See:
//
//        ReadScanline_from_TIFF_Strip() and
//        ReadScanline_from_TIFF_TileRow()
//
//   respectively for those needs.  For RGB tiffs with separate color planes, just
//   use:
//
//        TIFFReadScanline()
//
//   from the TIFF library directly.
//

int ReadScanline_from_ContiguousRGB_TIFF(TIFF *tiff, uint32 row, uint32 sample_count,
                                         int band_r, int band_g, int band_b,
                                         tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf)
{
  data_type_t data_type;
  tiff_data_config_t data_config;
  int num_bands, is_scanline_format, is_palette_color_tiff;

  // Determine what type of TIFF this is (scanline/strip/tiled)
  if (get_tiff_data_config(tiff,
      &data_config.sample_format,
      &data_config.bits_per_sample,
      &data_config.planar_config,
      &data_type,
      &data_config.samples_per_pixel,
      &is_scanline_format,
      &is_palette_color_tiff,
      REPORT_LEVEL_NONE))
  {
    return FALSE;
  }
  num_bands = data_config.samples_per_pixel;
  if (num_bands < 3) {
    return FALSE;
  }
  if (data_config.planar_config != PLANARCONFIG_CONTIG) {
    return FALSE;
  }
  uint32 scanlineSize = TIFFScanlineSize(tiff);
  if (scanlineSize <= 0) {
    return FALSE;
  }
  tdata_t *tif_buf = _TIFFmalloc(scanlineSize);

  TIFFReadScanline(tiff, tif_buf, row, 0); // Read RGB scanline (the band number is ignored for PLANARCONFIG_CONTIG)
  int s;
  for (s=0; s<sample_count; s++) {
    switch(data_config.bits_per_sample) {
      case 8:
        switch(data_config.sample_format) {
          case SAMPLEFORMAT_UINT:
            ((uint8*)rtif_buf)[s] = (uint8)(((uint8*)tif_buf)[s*num_bands+band_r]);
            ((uint8*)gtif_buf)[s] = (uint8)(((uint8*)tif_buf)[s*num_bands+band_g]);
            ((uint8*)btif_buf)[s] = (uint8)(((uint8*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_INT:
            ((int8*)rtif_buf)[s] = (int8)(((int8*)tif_buf)[s*num_bands+band_r]);
            ((int8*)gtif_buf)[s] = (int8)(((int8*)tif_buf)[s*num_bands+band_g]);
            ((int8*)btif_buf)[s] = (int8)(((int8*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_IEEEFP:
          default:
            _TIFFfree(tif_buf);
            return FALSE;
            break;
        }
        break;
      case 16:
        switch(data_config.sample_format) {
          case SAMPLEFORMAT_UINT:
            ((uint16*)rtif_buf)[s] = (uint16)(((uint16*)tif_buf)[s*num_bands+band_r]);
            ((uint16*)gtif_buf)[s] = (uint16)(((uint16*)tif_buf)[s*num_bands+band_g]);
            ((uint16*)btif_buf)[s] = (uint16)(((uint16*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_INT:
            ((int16*)rtif_buf)[s] = (int16)(((int16*)tif_buf)[s*num_bands+band_r]);
            ((int16*)gtif_buf)[s] = (int16)(((int16*)tif_buf)[s*num_bands+band_g]);
            ((int16*)btif_buf)[s] = (int16)(((int16*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_IEEEFP:
          default:
            _TIFFfree(tif_buf);
            return FALSE;
            break;
        }
        break;
      case 32:
        switch(data_config.sample_format) {
          case SAMPLEFORMAT_UINT:
            ((uint32*)rtif_buf)[s] = (uint32)(((uint32*)tif_buf)[s*num_bands+band_r]);
            ((uint32*)gtif_buf)[s] = (uint32)(((uint32*)tif_buf)[s*num_bands+band_g]);
            ((uint32*)btif_buf)[s] = (uint32)(((uint32*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_INT:
            ((long*)rtif_buf)[s] = (long)(((long*)tif_buf)[s*num_bands+band_r]);
            ((long*)gtif_buf)[s] = (long)(((long*)tif_buf)[s*num_bands+band_g]);
            ((long*)btif_buf)[s] = (long)(((long*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_IEEEFP:
            ((float*)rtif_buf)[s] = (float)(((float*)tif_buf)[s*num_bands+band_r]);
            ((float*)gtif_buf)[s] = (float)(((float*)tif_buf)[s*num_bands+band_g]);
            ((float*)btif_buf)[s] = (float)(((float*)tif_buf)[s*num_bands+band_b]);
            break;
          default:
            _TIFFfree(tif_buf);
            return FALSE;
            break;
        }
        break;
      default:
        _TIFFfree(tif_buf);
        return FALSE;
        break;
    }
  }
  _TIFFfree(tif_buf);

  return TRUE;
}

// If an input GeoTIFF a) has a citation string produced by ASF, and b) the
// citation string contains a list of band names, then the word 'Empty' will exist
// for files that are populated with a no-data value.  The normal import process
// reads the TIFF file to produce ASF metadata and all 'empty' bands are stripped
// out ...the right thing for ingest into ASF format.  The ASF data viewer however
// needs to use those 'empty' bands when displaying a color TIFF and the metadata
// parameters are used by the viewer... consequently, the 'Empty' bandname(s) and
// total band count must be corrected in the ingested metadata to reflect the
// actual contents of the TIFF file rather than the contents of an ingested ASF
// internal format file.  add_empties() produces a new (complete with 'Empty'
// bands) list of band names and a corrected band count ...if conditions a)
// and b) above are met.
void add_empties(const char *tiff_file, char *band_str, short *num_bands,
                 char *meta_bands, int meta_band_count, int *empty)
{
  int i, num_empty;
  char **band_names;

  TIFF *tiff = TIFFOpen(tiff_file, "r");
  if (tiff == NULL) asfPrintError("Cannot open tiff file %s\n", tiff_file);
  for (i=0, num_empty = 0; i<MAX_BANDS; i++) num_empty += empty[i] ? 1 : 0;
  if (num_empty > 0 && tiff) {
    short samplesPerPixel;
    *num_bands = 0;
    int read_count = TIFFGetField(tiff, TIFFTAG_SAMPLESPERPIXEL, &samplesPerPixel);
    if (read_count) {
      *num_bands = (int)samplesPerPixel;
      band_names = extract_band_names(meta_bands, meta_band_count);
      strcpy(band_str, "");
      int bn;
      for (i=0, bn=0; i < *num_bands; i++) {
        if (!empty[i]) {
          if (i==0) {
            sprintf(band_str, "%s", band_names[bn]);
          }
          else {
            sprintf(band_str, "%s,%s", band_str, band_names[bn]);
          }
          bn++;
        }
        else {
          if (i==0) {
            sprintf(band_str, "%s", "Empty");
          }
          else {
            sprintf(band_str, "%s,%s", band_str, "Empty");
          }
        }
      }
      if (band_names) {
        for (i=0; i<meta_band_count; i++) {
          if (band_names[i]) FREE(band_names[i]);
        }
        FREE(band_names);
      }
    }
  }
  if (tiff) TIFFClose(tiff);
}

// Assumes mc->num_elements has been set properly to 2^bits_per_sample
void read_tiff_colormap(const char *tiff_file, meta_colormap *mc)
{
  unsigned short *colors = NULL;
  unsigned short *red    = NULL;
  unsigned short *green  = NULL;
  unsigned short *blue   = NULL;
  TIFF *tiff = TIFFOpen(tiff_file, "r");
  if (tiff == NULL) asfPrintError("Cannot open tiff file %s\n", tiff_file);
  asfRequire(mc && mc->num_elements > 0, "Invalid colormap");

  colors = (unsigned short*)CALLOC(3 * mc->num_elements, sizeof(unsigned short));
  red    = &colors[                   0];
  green  = &colors[    mc->num_elements];
  blue   = &colors[2 * mc->num_elements];

  if (TIFFGetField(tiff, TIFFTAG_COLORMAP, &red, &green, &blue)) {
    int i;
    for (i=0; i<mc->num_elements; i++) {
      mc->rgb[i].red   = red[i]   >> 8;
      mc->rgb[i].green = green[i] >> 8;
      mc->rgb[i].blue  = blue[i]  >> 8;
    }
  }
  else {
    asfPrintError("Cannot read colormap from TIFF file %s\n", tiff_file);
  }

  if (tiff) TIFFClose(tiff);
  FREE(colors);
}

static xmlXPathObjectPtr
getnodeset(xmlDocPtr doc, xmlChar *xpath){
	
  xmlXPathContextPtr context;
  xmlXPathObjectPtr result;
  
  context = xmlXPathNewContext(doc);
  if (context == NULL) {
    printf("Error in xmlXPathNewContext\n");
    return NULL;
  }
  result = xmlXPathEvalExpression(xpath, context);
  xmlXPathFreeContext(context);
  if (result == NULL) {
    printf("Error in xmlXPathEvalExpression\n");
    return NULL;
  }
  if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
    xmlXPathFreeObject(result);
    printf("No result\n");
    return NULL;
  }
  return result;
}

static int get_meta_xml_item(xmlDocPtr doc, xmlChar *parameter)
{
  int ii, found=-1;
  xmlNodeSetPtr keyNode;
  xmlXPathObjectPtr key;
  xmlChar *keyword;
  key = getnodeset(doc, (xmlChar*)"/GDALMetadata/Item/@name");
  if (key) {
    keyNode = key->nodesetval;
    for (ii=0; ii<keyNode->nodeNr; ii++) {
      keyword = 
	xmlNodeListGetString(doc, keyNode->nodeTab[ii]->xmlChildrenNode, 1);
      if (strcmp_case((char *)keyword, (char *)parameter) == 0)
	found = ii;
      xmlFree(keyword);
    }
  }
  return found;
}

static xmlChar *get_meta_xml_units(xmlDocPtr doc, xmlChar *parameter)
{
  xmlNodeSetPtr unitsNode;
  xmlXPathObjectPtr units;
  int item = get_meta_xml_item(doc, parameter);
  units = getnodeset(doc, (xmlChar*)"/GDALMetadata/Item/@units");
  if (units && item >= 0) {
    unitsNode = units->nodesetval;
    return
      xmlNodeListGetString(doc, unitsNode->nodeTab[item]->xmlChildrenNode, 1);
  }
  return NULL;
}

static double get_meta_xml_double(xmlDocPtr doc, xmlChar *parameter)
{
  xmlNodeSetPtr valNode;
  xmlXPathObjectPtr val;
  xmlChar *value;
  int item = get_meta_xml_item(doc, parameter);
  val = getnodeset(doc, (xmlChar*)"/GDALMetadata/Item");
  if (val && item >= 0) {
    valNode = val->nodesetval;
    value = 
      xmlNodeListGetString(doc, valNode->nodeTab[item]->xmlChildrenNode, 1);
    return
      atof((char *)value);
  }
  return MAGIC_UNSET_DOUBLE;
}

static char *get_meta_xml_string(xmlDocPtr doc, xmlChar *parameter)
{
  xmlNodeSetPtr valNode;
  xmlXPathObjectPtr val;
  int item = get_meta_xml_item(doc, parameter);
  val = getnodeset(doc, (xmlChar*)"/GDALMetadata/Item");
  if (val && item >= 0) {
    valNode = val->nodesetval;
    return
      (char *) xmlNodeListGetString(doc, 
				    valNode->nodeTab[item]->xmlChildrenNode, 1);
  }
  return MAGIC_UNSET_STRING;
}

/**
 * Parse the in memory document and free the resulting tree
 */
static xmlDocPtr
get_insar_xml_tree_from_string(const char *document) {
    xmlDocPtr doc;

    /*
     * The document being in memory, it have no base per RFC 2396,
     * and the "noname.xml" argument will serve as its base.
     */
    doc = xmlReadMemory(document, strlen(document), "noname.xml", NULL, 0);
    if ( NULL == doc ) {
        asfPrintStatus("Failed to parse in-memory XML metadata document for InSAR product.\n");
    }
    return doc;
}

/**
 * gotchas: xmlDocPtr is passed by reference
 * returns: bool success
 */
static int
get_insar_xml_from_tiff_tag(const char *tiff_name, xmlDocPtr *doc)
{
    TIFF *tiff = NULL;
    char *insar_xml;
    
    asfPrintStatus("Checking for ASF InSAR metadata from GDAL metadata tag...");
    tiff = XTIFFOpen(tiff_name, "r");
    if (tiff) {
        if ( 0 == TIFFGetField(tiff, TIFFTAG_ASF_INSAR_METADATA, &insar_xml) )
        {
            asfPrintStatus("...didn't find it.\n");
            return FALSE;
        } else {
            asfPrintStatus("...found it!\n");
            *doc = get_insar_xml_tree_from_string(insar_xml);
            return TRUE;
        }   
        XTIFFClose(tiff);
    }
    return FALSE;
}

/**
 * assumptions: the metadata isn't more than 2kb in size.
 * precondition: meta_parameters meta is initialized
 * postcondition: meta->insar has a reference to a valid InSAR data structure
 *     (could be empty).
 */
meta_insar *populate_insar_metadata(const char *filename)
{
  meta_insar *insar = NULL;
  //LIBXML_TEST_VERSION - Check for version/compilation/libraries
  xmlDocPtr doc = NULL;
  
  if ( FALSE == get_insar_xml_from_tiff_tag(filename, &doc) ) {
      asfPrintStatus("Didn't find an InSAR metadata in embedded TIFF metadata.\n");
  } 
  else {
    insar = meta_insar_init();
    strcpy(insar->processor, 
	   get_meta_xml_string(doc, (xmlChar *)"INSAR_PROCESSOR"));
    strcpy(insar->master_image, 
	   get_meta_xml_string(doc, (xmlChar *)"INSAR_MASTER_IMAGE"));
    strcpy(insar->slave_image, 
	   get_meta_xml_string(doc, (xmlChar *)"INSAR_SLAVE_IMAGE"));
    strcpy(insar->master_acquisition_date, 
	   get_meta_xml_string(doc, 
			       (xmlChar *)"INSAR_MASTER_ACQUISITION_DATE"));
    strcpy(insar->slave_acquisition_date, 
	   get_meta_xml_string(doc, 
			       (xmlChar *)"INSAR_SLAVE_ACQUISITION_DATE"));
    insar->center_look_angle = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_CENTER_LOOK_ANGLE");
    char *units = (char*) get_meta_xml_units(doc,
                                     (xmlChar *)"INSAR_CENTER_LOOK_ANGLE");
    strcpy(insar->center_look_angle_units, units ? units : "");
    insar->doppler = get_meta_xml_double(doc, (xmlChar *)"INSAR_DOPPLER");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_DOPPLER");
    strcpy(insar->doppler_units, units ? units : "");
    insar->doppler_rate = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_DOPPLER_RATE");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_DOPPLER_RATE");
    strcpy(insar->doppler_rate_units, units ? units : "");
    insar->baseline_length = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_BASELINE_LENGTH");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_BASELINE_LENGTH");
    strcpy(insar->baseline_length_units, units ? units : "");
    insar->baseline_parallel = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_BASELINE_PARALLEL");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_BASELINE_PARALLEL");
    strcpy(insar->baseline_parallel_units, units ? units : "");
    insar->baseline_parallel_rate = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_BASELINE_PARALLEL_RATE");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_BASELINE_PARALLEL_RATE");
    strcpy(insar->baseline_parallel_rate_units, units ? units : "");
    insar->baseline_perpendicular = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_BASELINE_PERPENDICULAR");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_BASELINE_PERPENDICULAR");
    strcpy(insar->baseline_perpendicular_units, units ? units : "");
    insar->baseline_perpendicular_rate = 
      get_meta_xml_double(doc, 
			  (xmlChar *)"INSAR_BASELINE_PERPENDICULAR_RATE");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_BASELINE_PERPENDICULAR_RATE");
    strcpy(insar->baseline_perpendicular_rate_units, units ? units : "");
    insar->baseline_temporal = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_BASELINE_TEMPORAL");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_BASELINE_TEMPORAL");
    strcpy(insar->baseline_temporal_units, units ? units : "");
    insar->baseline_critical = 
      get_meta_xml_double(doc, (xmlChar *)"INSAR_BASELINE_CRITICAL");
    units = (char*) get_meta_xml_units(doc, (xmlChar *)"INSAR_BASELINE_CRITICAL");
    strcpy(insar->baseline_critical_units, units ? units : "");
      
    xmlFreeDoc(doc);
    xmlCleanupParser();
  }
  return insar;
}
