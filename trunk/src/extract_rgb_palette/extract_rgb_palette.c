/*******************************************************************************
NAME: extract_rgb_palette

ALGORITHM DESCRIPTION:
  Dumps RGB indexed color palette from graphics file to stdout

ISSUES:
        Only color indexed TIFF files currently supported

*******************************************************************************/
#include "asf.h"
#include "asf_nan.h"
#include <math.h>
#include <ctype.h>
#include "asf_raster.h"
#include "typlim.h"
#include <float_image.h>
#include <uint8_image.h>
#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include <gsl/gsl_math.h>
#include "geotiff_support.h"
#include "extract_rgb_palette_help.h"

/**** TYPES ****/
typedef enum {
  UNKNOWN_GRAPHICS_TYPE=0,
  ASF_IMG,
  JPEG,
  PGM,
  PPM,
  PBM,
  STD_TIFF,
  GEO_TIFF,
  BMP,
  GIF,
  PNG
} graphics_file_t;

#define MISSING_TIFF_DATA -1
typedef struct {
  uint32 width;
  uint32 height;
  short sample_format;
  short bits_per_sample;
  short planar_config;
  data_type_t data_type; // ASF data type
  short num_bands;
  int is_scanline_format;
  int is_palette_color_tiff;
} tiff_data_t;

#define MISSING_GTIF_DATA -1
typedef struct {
  int gtif_data_exists;
  char *GTcitation;
  char *PCScitation;
  int tie_point_elements; // Number of tie point elements (usually a multiple of 6)
  int num_tie_points; // Number of elements divided by 6 since (i,j,k) maps to (x,y,z)
  double *tie_point;  // Usually only 1, but who knows what evil lurks in the hearts of men?
  int pixel_scale_elements; // Usually a multiple of 3
  int num_pixel_scales;
  double *pixel_scale;  // Should always be 3 of these ...for ScaleX, ScaleY, and ScaleZ
  short model_type;
  short raster_type;
  short linear_units;
  double scale_factor;
  datum_type_t datum;
  char hemisphere;
  unsigned long pro_zone; // UTM zone (UTM only)
  short proj_coords_trans;
  short pcs;
  short geodetic_datum;
  short geographic_datum;
  double false_easting;
  double false_northing;
  double natLonOrigin;
  double lonCenter;
  double falseOriginLon;
  double falseOriginLat;
  double natLatOrigin;
  double latCenter;
  double stdParallel1;
  double stdParallel2;
  double lonPole;
} geotiff_data_t;

/**** PROTOTYPES ****/
graphics_file_t getGraphicsFileType (char *file);
void graphicsFileType_toStr (graphics_file_t type, char *type_str);
float get_maxval(data_type_t data_type);
char *data_type2str(data_type_t data_type);
void get_tiff_info_from_file(char *file, tiff_data_t *t);
void get_tiff_info(TIFF *tif, tiff_data_t *t);
void get_geotiff_keys(char *file, geotiff_data_t *g);
void dump_tiff_rgb_palette(char *inFile);

int main(int argc, char **argv)
{
  char *inFile;
  char msg[1024];
  char type_str[255];

  asfSplashScreen(argc, argv);

  if (argc != 2) {
      usage();
      exit(1);
  }
  else {
      check_for_help(argc, argv);
  }

  inFile=argv[1];

  if (!fileExists(inFile)) {
    sprintf(msg, "File not found: %s\n  => Did you forget to use the filename extension?\n", inFile);
    asfPrintError(msg);
  }
  graphics_file_t type;
  type = getGraphicsFileType(inFile);
  if (type != STD_TIFF &&
      type != GEO_TIFF )
  {
    graphicsFileType_toStr(type, type_str);
    sprintf(msg, "Graphics file type %s is not currently supported (Graphics file: %s)\n",
            type_str, inFile);
    asfPrintError(msg);
  }

  switch (type) {
    case STD_TIFF:
    case GEO_TIFF:
      dump_tiff_rgb_palette(inFile);
      break;
    default:
      sprintf(msg, "Unrecognized image file type found.\n");
      asfPrintError(msg);
      break;
  }

  return (0);
}

graphics_file_t getGraphicsFileType (char *file)
{
  FILE *fp = (FILE *)FOPEN(file, "rb");
  uint8 magic[4];
  graphics_file_t file_type = UNKNOWN_GRAPHICS_TYPE;
  TIFF *itif = NULL;
  GTIF *gtif = NULL;

  if (fp != NULL) {
    int i;

    // Read the magic number from the file header
    for (i=0; i<4; i++) {
      magic[i] = (uint8)fgetc(fp);
    }
    if(fp) FCLOSE(fp);

    // Check for a valid magic number combination
    if (magic[0] == 0xFF && magic[1] == 0xD8) {
      file_type = JPEG;
    }
    else if (magic[0] == 'P' && (magic[1] == '4' || magic[1] == '1')) {
      file_type = PBM;
    }
    else if (magic[0] == 'P' && (magic[1] == '5' || magic[1] == '2')) {
      file_type = PGM;
    }
    else if (magic[0] == 'P' && (magic[1] == '6' || magic[1] == '3')) {
      file_type = PPM;
    }
    else if ((magic[0] == 'I' && magic[1] == 'I') || (magic[0] == 'M' && magic[1] == 'M')) {
      file_type = STD_TIFF;

      itif = XTIFFOpen(file, "rb");
      if (itif != NULL) {
        gtif = GTIFNew(itif);
        if (gtif != NULL) {
          double *tie_point = NULL;
          double *pixel_scale = NULL;
          short model_type, raster_type, linear_units;
          int read_count, tie_points, pixel_scales;

          (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_TIEPOINTS, &tie_points, &tie_point);
          (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_PIXELSCALE, &pixel_scales, &pixel_scale);
          read_count = GTIFKeyGet(gtif, GTModelTypeGeoKey, &model_type, 0, 1);
          read_count += GTIFKeyGet(gtif, GTRasterTypeGeoKey, &raster_type, 0, 0);
          read_count += GTIFKeyGet(gtif, ProjLinearUnitsGeoKey, &linear_units, 0, 1);

          if (tie_points == 6 && pixel_scales == 3 && read_count == 3) {
            file_type = GEO_TIFF;
          }

          if (tie_point != NULL) free(tie_point);
          if (pixel_scale != NULL) free(pixel_scale);
          GTIFFree(gtif);
        }
        XTIFFClose(itif);
      }
    }
    else if (magic[0] == 'B' && magic[1] == 'M') {
      file_type = BMP;
    }
    else if (magic[0] == 'G' && magic[1] == 'I' && magic[2] == 'F') {
      file_type = GIF;
    }
    else if (magic[1] == 'P' && magic[2] == 'N' && magic[3] == 'G') {
      file_type = PNG;
    }
    else {
      file_type = UNKNOWN_GRAPHICS_TYPE;
    }
  }

  return file_type;
}

void graphicsFileType_toStr (graphics_file_t type, char *type_str)
{
  switch (type) {
    case ASF_IMG:
      strcpy (type_str, "ASF_IMG");
      break;
    case JPEG:
      strcpy (type_str, "JPEG");
      break;
    case PBM:
      strcpy (type_str, "PBM");
      break;
    case PGM:
      strcpy (type_str, "PGM");
      break;
    case PPM:
      strcpy (type_str, "PPM");
      break;
    case STD_TIFF:
      strcpy (type_str, "TIFF");
      break;
    case GEO_TIFF:
      strcpy (type_str, "GEOTIFF");
      break;
    case BMP:
      strcpy (type_str, "BMP");
      break;
    case GIF:
      strcpy (type_str, "GIF");
      break;
    case PNG:
      strcpy (type_str, "PNG");
      break;
    case UNKNOWN_GRAPHICS_TYPE:
    default:
      strcpy(type_str, "UNRECOGNIZED");
      break;
  }
}

float get_maxval(data_type_t data_type)
{
  float ret;

  // Only non-complex types with 32 bits or less are supported
  switch (data_type) {
    case BYTE:
      ret = powf(2, sizeof(unsigned char)) - 1.0;
      break;
    case INTEGER16:
      ret = powf(2, sizeof(short int)) - 1.0;
      break;
    case INTEGER32:
      ret = powf(2, sizeof(int)) - 1.0;
      break;
    case REAL32:
      ret = MAXREAL;
      break;
    default:
      ret = 0.0;
      break;
  }

  return ret;
}

// User must free the returned string
char *data_type2str(data_type_t data_type)
{
  char *retstr = (char*)CALLOC(64, sizeof(char));

  switch (data_type) {
    case BYTE:
      strcpy(retstr, "BYTE");
      break;
    case INTEGER16:
      strcpy(retstr, "INTEGER16");
      break;
    case INTEGER32:
      strcpy(retstr, "INTEGER32");
      break;
    case REAL32:
      strcpy(retstr, "REAL32");
      break;
    case REAL64:
      strcpy(retstr, "REAL64");
      break;
    case COMPLEX_BYTE:
      strcpy(retstr, "COMPLEX_BYTE");
      break;
    case COMPLEX_INTEGER16:
      strcpy(retstr, "COMPLEX_INTEGER16");
      break;
    case COMPLEX_INTEGER32:
      strcpy(retstr, "COMPLEX_INTEGER32");
      break;
    case COMPLEX_REAL32:
      strcpy(retstr, "COMPLEX_REAL32");
      break;
    case COMPLEX_REAL64:
      strcpy(retstr, "COMPLEX_REAL64");
      break;
    default:
      strcpy(retstr, "UNKNOWN");
      break;
  }

  return retstr;
}

void get_tiff_info_from_file(char *file, tiff_data_t *t)
{
  TIFF *tif;

  t->sample_format = MISSING_TIFF_DATA;
  t->bits_per_sample = MISSING_TIFF_DATA;
  t->planar_config = MISSING_TIFF_DATA;
  t->data_type = 0;
  t->num_bands = 0;
  t->is_scanline_format = 0;
  t->height = 0;
  t->width = 0;

  tif = XTIFFOpen(file, "rb");
  if (tif != NULL) {
    get_tiff_info(tif, t);
  }

  XTIFFClose(tif);
}

void get_tiff_info(TIFF *tif, tiff_data_t *t)
{
  get_tiff_data_config(tif,
                       &t->sample_format,
                       &t->bits_per_sample,
                       &t->planar_config,
                       &t->data_type,
                       &t->num_bands,
                       &t->is_scanline_format,
                       &t->is_palette_color_tiff,
                       REPORT_LEVEL_NONE);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &t->height);
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &t->width);
  if (t->planar_config != PLANARCONFIG_CONTIG &&
      t->planar_config != PLANARCONFIG_SEPARATE &&
      t->num_bands == 1)
  {
    t->planar_config = PLANARCONFIG_CONTIG;
  }
}

void get_geotiff_keys(char *file, geotiff_data_t *g)
{
  TIFF *tif = XTIFFOpen(file, "rb");
  GTIF *gtif = NULL;

  // Init values to 'missing'
  g->gtif_data_exists = 0;
  g->GTcitation = NULL;   // Should be unallocated
  g->PCScitation = NULL;  // Should be unallocated

  // Read geotiff info
  if (tif != NULL) {
    gtif = GTIFNew(tif);
    if (gtif != NULL) {
      int count, read_count;
      int citation_length;
      int typeSize;
      tagtype_t citation_type;

      // Get citations
      citation_length = GTIFKeyInfo(gtif, GTCitationGeoKey, &typeSize, &citation_type);
      if (citation_length > 0) {
        g->GTcitation = (char*)MALLOC(citation_length * typeSize);
        GTIFKeyGet(gtif, GTCitationGeoKey, g->GTcitation, 0, citation_length);
      }
      else {
        g->GTcitation = NULL;
      }
      citation_length = GTIFKeyInfo(gtif, PCSCitationGeoKey, &typeSize, &citation_type);
      if (citation_length > 0) {
        g->PCScitation = (char*)MALLOC(citation_length * typeSize);
        GTIFKeyGet(gtif, PCSCitationGeoKey, g->PCScitation, 0, citation_length);
      }
      else {
        g->PCScitation = NULL;
      }
      if ((g->GTcitation != NULL && strlen(g->GTcitation) > 0) ||
           (g->PCScitation != NULL && strlen(g->PCScitation) > 0))
      {
        g->gtif_data_exists = 1;
      }

      // Get tie points and pixel scale
      (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_TIEPOINTS, &count, &g->tie_point);
      if (count >= 6) {
        g->gtif_data_exists = 1;
        g->tie_point_elements = count;
        g->num_tie_points = count / 6;
      }
      (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_PIXELSCALE, &count, &g->pixel_scale);
      if (count >= 3) {
        g->gtif_data_exists = 1;
        g->pixel_scale_elements = count;
        g->num_pixel_scales = count / 3;
      }

      // Get model type, raster type, and linear units
      read_count = GTIFKeyGet (gtif, GTModelTypeGeoKey, &g->model_type, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      read_count = GTIFKeyGet (gtif, GTRasterTypeGeoKey, &g->raster_type, 0, 0);
      if (read_count >= 1) g->gtif_data_exists = 1;
      read_count = GTIFKeyGet (gtif, ProjLinearUnitsGeoKey, &g->linear_units, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;

      // Get UTM related info if it exists
      read_count = GTIFKeyGet(gtif, ProjectedCSTypeGeoKey, &g->pcs, 0, 1);
      if (read_count == 1 && PCS_2_UTM(g->pcs, &g->hemisphere, &g->datum, &g->pro_zone)) {
        g->gtif_data_exists = 1;
      }
      else {
        read_count = GTIFKeyGet(gtif, ProjectionGeoKey, &g->pcs, 0, 1);
        if (read_count == 1 && PCS_2_UTM(g->pcs, &g->hemisphere, &g->datum, &g->pro_zone)) {
          g->gtif_data_exists = 1;
        }
        else {
          g->hemisphere = '\0';
          g->datum = UNKNOWN_DATUM;
          g->pro_zone = MISSING_GTIF_DATA;
        }
      }

      // Get projection type (ProjCoordTransGeoKey) and other projection parameters
      read_count = GTIFKeyGet(gtif, ProjCoordTransGeoKey, &g->proj_coords_trans, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->proj_coords_trans = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet(gtif, GeographicTypeGeoKey, &g->geographic_datum, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->geographic_datum = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet(gtif, GeogGeodeticDatumGeoKey, &g->geodetic_datum, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->geodetic_datum = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet(gtif, ProjScaleAtNatOriginGeoKey, &g->scale_factor, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->scale_factor = MISSING_GTIF_DATA;

      // Get generic projection parameters (Note: projection type is defined by
      // the g->proj_coords_trans value)
      read_count = GTIFKeyGet (gtif, ProjFalseEastingGeoKey, &g->false_easting, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->false_easting = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjFalseNorthingGeoKey, &g->false_northing, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->false_northing = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjNatOriginLongGeoKey, &g->natLonOrigin, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->natLonOrigin = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjNatOriginLatGeoKey, &g->natLatOrigin, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->natLatOrigin = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjStdParallel1GeoKey, &g->stdParallel1, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->stdParallel1 = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjStdParallel2GeoKey, &g->stdParallel2, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->stdParallel2 = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjCenterLongGeoKey, &g->lonCenter, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->lonCenter = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjCenterLatGeoKey, &g->latCenter, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->latCenter = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjFalseOriginLongGeoKey, &g->falseOriginLon, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->falseOriginLon = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjFalseOriginLatGeoKey, &g->falseOriginLat, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->falseOriginLat = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjStraightVertPoleLongGeoKey, &g->lonPole, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->lonPole = MISSING_GTIF_DATA;
    }
  }
}

void tiff_get_float_line(TIFF *tif, float *buf, int row, int band_no)
{
  tiff_data_t t;

  get_tiff_info(tif, &t);
  // Note: For single-plane (greyscale) images, planar_config may remain unset so
  // we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0)
  {
    asfPrintError("Cannot read tif file\n");
  }

  // Read a scanline
  tsize_t scanlineSize = TIFFScanlineSize(tif);
  tdata_t *tif_buf = _TIFFmalloc(scanlineSize);
  if (t.planar_config == PLANARCONFIG_CONTIG || t.num_bands == 1) {
    TIFFReadScanline(tif, tif_buf, row, 0);
  }
  else {
    // Planar configuration is band-sequential
    TIFFReadScanline(tif, tif_buf, row, band_no);
  }

  int col;
  for (col=0; col<t.width; col++) {
    switch(t.bits_per_sample) {
      case 8:
        switch(t.sample_format) {
          case SAMPLEFORMAT_UINT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((uint8*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint8*)(tif_buf))[col]);
            }
            break;
          case SAMPLEFORMAT_INT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((int8*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((int8*)(tif_buf))[col]);   // Current sample.
            }
            break;
          default:
            // There is no such thing as an IEEE 8-bit floating point
            if (tif_buf) _TIFFfree(tif_buf);
            if (tif) XTIFFClose(tif);
            asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
            break;
        }
        break;
      case 16:
        switch(t.sample_format) {
          case SAMPLEFORMAT_UINT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((uint16*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint16*)(tif_buf))[col]);   // Current sample.
            }
            break;
          case SAMPLEFORMAT_INT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((int16*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint16*)(tif_buf))[col]);   // Current sample.
            }
            break;
          default:
            // There is no such thing as an IEEE 16-bit floating point
            if (tif_buf) _TIFFfree(tif_buf);
            if (tif) XTIFFClose(tif);
            asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
            break;
        }
        break;
      case 32:
        switch(t.sample_format) {
          case SAMPLEFORMAT_UINT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((uint32*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint32*)(tif_buf))[col]);   // Current sample.
            }
            break;
          case SAMPLEFORMAT_INT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((long*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((long*)(tif_buf))[col]);   // Current sample.
            }
            break;
          case SAMPLEFORMAT_IEEEFP:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((float*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((float*)(tif_buf))[col]);   // Current sample.
            }
            break;
          default:
            if (tif_buf) _TIFFfree(tif_buf);
            if (tif) XTIFFClose(tif);
            asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
            break;
        }
        break;
      default:
        if (tif_buf) _TIFFfree(tif_buf);
        if (tif) XTIFFClose(tif);
        asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
        break;
    }
  }
  if (tif_buf) {
    _TIFFfree(tif_buf);
  }
}

float tiff_image_get_float_pixel(TIFF *tif, int row, int col, int band_no)
{
  tiff_data_t t;
  float cs;

  get_tiff_info(tif, &t);
  // Note: For single-plane (greyscale) images, planar_config may remain unset so
  // we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0)
  {
    asfPrintError("Cannot read tif file\n");
  }

  // Get a float line from the file
  float *buf = (float*)MALLOC(t.width*sizeof(float));
  tiff_get_float_line(tif, buf, row, band_no);
  cs = buf[col];
  FREE(buf);

  // Return as float
  return cs;
}

void dump_tiff_rgb_palette(char *inFile)
{
    TIFF *tiff = NULL;
    tiff_data_t tiff_data;

    get_tiff_info_from_file(inFile, &tiff_data);

    tiff = XTIFFOpen(inFile, "rb");
    if (!tiff) asfPrintError("Cannot open TIFF file (%s)\n", inFile);

    int color;
    int num_colors=(int)powl(2,tiff_data.bits_per_sample);
    uint16* red=NULL;
    uint16* green=NULL;
    uint16* blue=NULL;
    uint16 rmin, gmin, bmin, min;
    uint16 rmax, gmax, bmax, max;

    TIFFGetField(tiff, TIFFTAG_COLORMAP, &red, &green, &blue);
    if (red == NULL || green == NULL || blue == NULL) asfPrintError("Cannot read palette from TIFF file (%s)\n", inFile);

    rmin=gmin=bmin=min=65535; // Max uint16
    rmax=gmax=bmax=max=0; // Min uint16
    for (color=0; color<num_colors; color++) {
        unsigned short r, g, b;

        r=red[color];
        g=green[color];
        b=blue[color];

        r = (unsigned short)(((float)r/65535)*255 + .5);
        g = (unsigned short)(((float)g/65535)*255 + .5);
        b = (unsigned short)(((float)b/65535)*255 + .5);

        rmin = (r < rmin) ? r : rmin;
        gmin = (g < gmin) ? g : gmin;
        bmin = (b < bmin) ? b : bmin;
        min = (r < min) ? r : min;
        min = (g < min) ? g : min;
        min = (b < min) ? b : min;

        rmax = (r > rmax) ? r : rmax;
        gmax = (g > gmax) ? g : gmax;
        bmax = (b > bmax) ? b : bmax;
        max = (r > max) ? r : max;
        max = (g > max) ? g : max;
        max = (b > max) ? b : max;
    }
    fprintf(stdout, "# red min = %03d, green min = %03d, blue min = %03d, overall min = %03d\n",
            rmin, gmin, bmin, min);
    fprintf(stdout, "# red max = %03d, green max = %03d, blue max = %03d, overall max = %03d\n",
            rmax, gmax, bmax, max);
    fprintf(stdout, "# \n# color\tred\tgreen\tblue\n");
    for (color=0; color<num_colors; color++) {
        unsigned short r, g, b;

        r=red[color];
        g=green[color];
        b=blue[color];

        r = (unsigned short)(((float)r/65535)*255 + .5);
        g = (unsigned short)(((float)g/65535)*255 + .5);
        b = (unsigned short)(((float)b/65535)*255 + .5);
        fprintf(stdout, "%03d\t%03d\t%03d\t%03d\n",
                color, r, g, b);
    }

    XTIFFClose(tiff);
}

