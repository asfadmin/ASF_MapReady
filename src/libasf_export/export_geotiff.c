#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <asf.h>
#include "asf_tiff.h"

#include <gsl/gsl_math.h>
#include <proj_api.h>

#include "asf_jpeg.h"
#include <png.h>
#include "envi.h"

#include "dateUtil.h"
#include <time.h>
#include "matrix.h"
#include <asf_nan.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>
#include <asf_raster.h>
#include <float_image.h>
#include <spheroids.h>
#include <typlim.h>

typedef struct {
  FILE *fp;
  meta_parameters *meta;
  float *line;
} image_info_t;

/* This constant is from the GeoTIFF spec.  It basically means that
   the system which would normally be specified by the field
   (projected coordinate system, datum, ellipsoid, whatever), in
   instead going to be specified by more detailed low level tags.  */
static const int user_defined_value_code = 32767;

void export_geotiff(const char *input_file_list, const char *output_file_name)
{
  int band = 0;
  char inFile[512];
  FILE *fpList = FOPEN(input_file_list, "r");
  while (fgets(inFile, 512, fpList))
    band++;
  FCLOSE(fpList);
  chomp(inFile);

  // Get the image metadata
  meta_parameters *md = meta_read(inFile);
  if (!md->projection)
    asfPrintError("Data is not map projected!\n");
  int sample_count = md->general->sample_count;
  int line_count = md->general->line_count;

  // Open GeoTIFF file
  _XTIFFInitialize();
  TIFF *otif = XTIFFOpen (output_file_name, "w");
  asfRequire(otif != NULL, "Error opening output TIFF file.\n");

  // Setting TIFF tags
  TIFFSetField(otif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
  TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, sample_count);
  TIFFSetField(otif, TIFFTAG_IMAGELENGTH, line_count);
  TIFFSetField(otif, TIFFTAG_BITSPERSAMPLE, 32);
  TIFFSetField(otif, TIFFTAG_COMPRESSION, COMPRESSION_LZW);
  TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, band);
  TIFFSetField(otif, TIFFTAG_ROWSPERSTRIP, 1);
  TIFFSetField(otif, TIFFTAG_XRESOLUTION, 1.0);
  TIFFSetField(otif, TIFFTAG_YRESOLUTION, 1.0);
  TIFFSetField(otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);

  // Write GeoTIFF tags
  GTIF *ogtif = GTIFNew (otif);
  asfRequire(ogtif != NULL, "Error opening output GeoKey file descriptor.\n");

  GTIFKeySet(ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
  if (md->projection->type != LAT_LONG_PSEUDO_PROJECTION)
    GTIFKeySet(ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
  else
    GTIFKeySet(ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeGeographic);
  GTIFKeySet(ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
  GTIFKeySet(ogtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
  GTIFKeySet(ogtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
  GTIFKeySet(ogtif, GeogPrimeMeridianGeoKey, TYPE_SHORT, 1, PM_Greenwich);
  double re_major = md->projection->re_major;
  double re_minor = md->projection->re_minor;

  if (!(meta_is_valid_double(md->projection->startX) &&
        meta_is_valid_double(md->projection->startY)))
    asfPrintWarning("Metadata projection block contains invalid startX "
        "or startY values\n");
        
  double tie_point[6];
  tie_point[0] = 0.0;
  tie_point[1] = 0.0;
  tie_point[2] = 0.0;
  tie_point[3] = md->projection->startX + 
                 md->general->start_sample * md->projection->perX;
  tie_point[4] = md->projection->startY +
                 md->general->start_line * md->projection->perY;
  tie_point[5] = 0.0;
  TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 6, tie_point);

  double pixel_scale[3];
  pixel_scale[0] = fabs(md->projection->perX);
  pixel_scale[1] = fabs(md->projection->perY);
  pixel_scale[2] = 0;
  TIFFSetField (otif, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);

  int max_citation_length = 2048;
  char *citation;
  int citation_length;

  // Set the background value
  if (meta_is_valid_double(md->general->no_data)) {
    char nd[64];
    sprintf(nd, "%.18g", md->general->no_data);
    TIFFSetField(otif, TIFFTAG_GDAL_NODATA, nd);
  }

  // For now, only support the Hughes ellipsoid for polar stereo
  if (md->projection->datum == HUGHES_DATUM &&
      md->projection->type != POLAR_STEREOGRAPHIC)
    asfPrintError("Hughes Ellipsoid is only supported for Polar Stereographic "
      "projections.\n");

  // Write the appropriate geotiff keys for the projection type
  switch (md->projection->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
    {
      short pcs;
      if (UTM_2_PCS(&pcs, md->projection->datum,
        md->projection->param.utm.zone, md->projection->hem) ) {
        GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, pcs);
        GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
          user_defined_value_code);
        GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
          CT_TransverseMercator);
  
        if (meta_is_valid_double(md->projection->param.utm.false_easting)) {
          GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.utm.false_easting);
        }
        else
          GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        if (meta_is_valid_double(md->projection->param.utm.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.utm.false_northing);
        }
        else
          GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        if (meta_is_valid_double(md->projection->param.utm.lat0)) {
          GTIFKeySet(ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.utm.lat0);
        }
        if (meta_is_valid_double(md->projection->param.utm.lon0)) {
          GTIFKeySet(ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.utm.lon0);
        }
        GTIFKeySet(ogtif, ProjScaleAtNatOriginGeoKey, TYPE_DOUBLE, 1,
        md->projection->param.utm.scale_factor);

        write_datum_key(ogtif, md->projection->datum);
        write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);

        // Write the citation
        char datum_str[256];
        if (md->projection->datum == ITRF97_DATUM)
          strcpy(datum_str, "ITRF97 (WGS 84)");
        else
          pcs_2_string (datum_str, pcs);
        citation = MALLOC((max_citation_length + 1) * sizeof (char));
        snprintf(citation, max_citation_length + 1,
          "UTM zone %d %c projected GeoTIFF on %s "
          "%s written by Alaska Satellite Facility tools.",
          md->projection->param.utm.zone, md->projection->hem, datum_str,
          md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
        citation_length = strlen(citation);
        asfRequire((citation_length >= 0) && 
          (citation_length <= max_citation_length), 
          "GeoTIFF citation too long");
        GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
        FREE(citation);
      }
    }
    break;
    case ALBERS_EQUAL_AREA:
    {
      short pcs;
      if (albers_2_pcs(md->projection, &pcs))
        GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, pcs);
      else
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1, 
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
        CT_AlbersEqualArea);
      if (meta_is_valid_double(md->projection->param.albers.std_parallel1))
        GTIFKeySet(ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
          md->projection->param.albers.std_parallel1);
      if (meta_is_valid_double(md->projection->param.albers.std_parallel2))
        GTIFKeySet(ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
          md->projection->param.albers.std_parallel2);
      if (meta_is_valid_double(md->projection->param.albers.false_easting))
        GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.albers.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.albers.false_northing))
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.albers.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.albers.orig_latitude))
        GTIFKeySet(ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.albers.orig_latitude);
      if (meta_is_valid_double(md->projection->param.albers.center_meridian)) {
        // The following is where ArcGIS looks for the center meridian
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.albers.center_meridian);
        // The following is where the center meridian _should_ be stored
        GTIFKeySet(ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.albers.center_meridian);
      }
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      char datum_str[256];
      datum_2_string (datum_str, md->projection->datum);
      citation = MALLOC ((max_citation_length + 1) * sizeof (char));
      snprintf(citation, max_citation_length + 1,
        "Albers equal-area conic projected GeoTIFF using %s "
        "%s written by Alaska Satellite Facility tools.", datum_str,
        md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
      citation_length = strlen(citation);
      asfRequire (citation_length >= 0 && 
        citation_length <= max_citation_length, "bad citation length");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case LAMBERT_CONFORMAL_CONIC:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
        CT_LambertConfConic_2SP);
      if (meta_is_valid_double(md->projection->param.lamcc.plat1))
        GTIFKeySet(ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamcc.plat1);
      if (meta_is_valid_double(md->projection->param.lamcc.plat2))
        GTIFKeySet(ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamcc.plat2);
      if (meta_is_valid_double(md->projection->param.lamcc.false_easting))
        GTIFKeySet(ogtif, ProjFalseOriginEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamcc.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseOriginEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.lamcc.false_northing))
        GTIFKeySet(ogtif, ProjFalseOriginNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamcc.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseOriginNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.lamcc.lon0))
        GTIFKeySet(ogtif, ProjFalseOriginLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamcc.lon0);
      if (meta_is_valid_double(md->projection->param.lamcc.lat0))
        GTIFKeySet(ogtif, ProjFalseOriginLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamcc.lat0);
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      char datum_str[256];
      datum_2_string(datum_str, md->projection->datum);
      citation = MALLOC ((max_citation_length + 1) * sizeof (char));
      snprintf (citation, max_citation_length + 1,
        "Lambert conformal conic projected GeoTIFF using %s "
        "%s written by Alaska Satellite Facility tools.", datum_str,
        md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
      citation_length = strlen(citation);
      asfRequire (citation_length >= 0 && 
        citation_length <= max_citation_length, "bad citation length");
      GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case POLAR_STEREOGRAPHIC:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
        CT_PolarStereographic);
      if (meta_is_valid_double(md->projection->param.ps.slon))
        GTIFKeySet(ogtif, ProjStraightVertPoleLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.ps.slon);
      if (meta_is_valid_double(md->projection->param.ps.slat))
        GTIFKeySet(ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.ps.slat);
      if (meta_is_valid_double(md->projection->param.ps.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.ps.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.ps.false_northing))
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.ps.false_northing);
      else
        GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      GTIFKeySet(ogtif, ProjScaleAtNatOriginGeoKey, TYPE_DOUBLE, 1, 1.0);
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      char datum_str[256];
      datum_2_string(datum_str, md->projection->datum);
      citation = MALLOC ((max_citation_length + 1) * sizeof (char));
      if (md->projection->datum != HUGHES_DATUM) {
        snprintf(citation, max_citation_length + 1,
          "Polar stereographic projected GeoTIFF using %s "
          "datum written by Alaska Satellite Facility tools", datum_str);
        citation_length = strlen(citation);
        asfRequire(citation_length >= 0 &&
          citation_length <= max_citation_length, "bad citation length");
      }
      else {
        // Hughes Datum
        GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 3411);
        GTIFKeySet(ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4054);
        sprintf(citation, "NSIDC Sea Ice Polar Stereographic North");
      }
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
        CT_LambertAzimEqualArea);
      if (meta_is_valid_double(md->projection->param.lamaz.center_lon))
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.center_lon);
      if (meta_is_valid_double(md->projection->param.lamaz.center_lat))
        GTIFKeySet(ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.center_lat);
      if (meta_is_valid_double(md->projection->param.lamaz.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.lamaz.false_northing))
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      char datum_str[256];
      datum_2_string (datum_str, md->projection->datum);
      citation = MALLOC ((max_citation_length + 1) * sizeof (char));
      snprintf (citation, max_citation_length + 1,
        "Lambert azimuthal equal area projected GeoTIFF using "
        "%s %s written by Alaska Satellite Facility tools.", datum_str,
        md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
      citation_length = strlen(citation);
      asfRequire (citation_length >= 0 &&
          citation_length <= max_citation_length, "bad citation length");
      GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case EQUI_RECTANGULAR:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
        CT_Equirectangular);
      GTIFKeySet(ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.eqr.central_meridian))
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.eqr.central_meridian);
      if (meta_is_valid_double(md->projection->param.eqr.orig_latitude))
        GTIFKeySet(ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.eqr.orig_latitude);
      if (meta_is_valid_double(md->projection->param.eqr.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.eqr.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.eqr.false_northing))
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.eqr.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      char datum_str[256];
      datum_2_string(datum_str, md->projection->datum);
      citation = MALLOC((max_citation_length + 1) * sizeof (char));
      snprintf (citation, max_citation_length + 1,
        "Equi-rectangular projected GeoTIFF using "
        "%s %s written by Alaska Satellite Facility tools.", datum_str,
        md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
      citation_length = strlen(citation);
      asfRequire(citation_length >= 0 &&
        citation_length <= max_citation_length, "bad citation length");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case EQUIDISTANT:
    {
      GTIFKeySet(ogtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1,
        md->projection->spheroid);
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, 32663);
      if (meta_is_valid_double(md->projection->param.eqc.central_meridian))
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.eqc.central_meridian);
      if (meta_is_valid_double(md->projection->param.eqc.orig_latitude))
        GTIFKeySet(ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.eqc.orig_latitude);
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      char datum_str[256];
      datum_2_string (datum_str, md->projection->datum);
      citation = MALLOC((max_citation_length + 1) * sizeof (char));
      snprintf(citation, max_citation_length + 1,
        "WGS 84 / World Equidistant Cylindrical");
      citation_length = strlen(citation);
      asfRequire(citation_length >= 0 &&
          citation_length <= max_citation_length, "bad citation length");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case MERCATOR:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1, 
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_Mercator);
      if (meta_is_valid_double(md->projection->param.mer.central_meridian))
        GTIFKeySet(ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.mer.central_meridian);
      if (meta_is_valid_double(md->projection->param.mer.orig_latitude))
        GTIFKeySet(ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.mer.orig_latitude);
      if (meta_is_valid_double(md->projection->param.mer.standard_parallel)) {
        // GeoTIFF only supports the scale factor version.
        // Hence some fancy calculation
        double lat = md->projection->param.mer.orig_latitude;
        double lat1 = md->projection->param.mer.standard_parallel;
        double re = md->projection->re_major;
        double rp = md->projection->re_minor;
        double e2 = sqrt(1.0 - rp*rp/(re*re));
        double scale = (sqrt(1.0 - e2*sin(lat)*sin(lat))/cos(lat)) * 
          (cos(lat1)/sqrt(1.0 - e2*sin(lat1)*sin(lat1)));
        GTIFKeySet(ogtif, ProjScaleAtNatOriginGeoKey, TYPE_DOUBLE, 1, scale);
      }
      if (meta_is_valid_double(md->projection->param.mer.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.mer.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.mer.false_northing))
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.mer.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      char datum_str[256];
      datum_2_string(datum_str, md->projection->datum);
      citation = MALLOC((max_citation_length + 1) * sizeof (char));
      snprintf(citation, max_citation_length + 1, "Mercator projected GeoTIFF "
        "using %s %s written by Alaska Satellite Facility tools.", datum_str,
        md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
      citation_length = strlen(citation);
      asfRequire(citation_length >= 0 &&
        citation_length <= max_citation_length, "bad citation length");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case SINUSOIDAL:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
        user_defined_value_code);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
        CT_Sinusoidal);
      if (meta_is_valid_double(md->projection->param.sin.longitude_center))
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.sin.longitude_center);
      if (meta_is_valid_double(md->projection->param.sin.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.sin.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.sin.false_northing)) 
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.sin.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      citation = MALLOC((max_citation_length + 1) * sizeof (char));
      snprintf (citation, max_citation_length + 1,
        "Sinusoidal projected GeoTIFF using sphere written by Alaska Satellite "
        "Facility tools.");
      citation_length = strlen(citation);
      asfRequire(citation_length >= 0 &&
        citation_length <= max_citation_length, "bad citation length");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE(citation);
    }
    break;
    case EASE_GRID_GLOBAL:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 3410);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, 28);
      GTIFKeySet(ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1, 6053);
      GTIFKeySet(ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4053);
      if (meta_is_valid_double(md->projection->param.cea.standard_parallel))
        GTIFKeySet(ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
          md->projection->param.cea.standard_parallel);
      if (meta_is_valid_double(md->projection->param.cea.central_meridian)) {
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.cea.central_meridian);
        GTIFKeySet(ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.cea.central_meridian);
      }
      if (meta_is_valid_double(md->projection->param.cea.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.cea.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.cea.false_northing)) 
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.cea.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      citation = MALLOC((max_citation_length + 1) * sizeof (char));
      snprintf(citation, max_citation_length + 1, "NSIDC EASE-Grid Global");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE (citation);
    }
    break;
    case EASE_GRID_NORTH:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 3408);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, 
        CT_LambertAzimEqualArea);
      GTIFKeySet(ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1, 6053);
      GTIFKeySet(ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4053);
      if (meta_is_valid_double(md->projection->param.lamaz.center_lat))
        GTIFKeySet(ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.center_lat);
      if (meta_is_valid_double(md->projection->param.lamaz.center_lon))
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.center_lon);
      if (meta_is_valid_double(md->projection->param.lamaz.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.lamaz.false_northing)) 
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      citation = MALLOC((max_citation_length + 1) * sizeof (char));
      snprintf(citation, max_citation_length + 1, "NSIDC EASE-Grid North");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE(citation);
    }
    break;
    case EASE_GRID_SOUTH:
    {
      GTIFKeySet(ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 3409);
      GTIFKeySet(ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, 
        CT_LambertAzimEqualArea);
      GTIFKeySet(ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1, 6053);
      GTIFKeySet(ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4053);
      if (meta_is_valid_double(md->projection->param.lamaz.center_lat))
        GTIFKeySet(ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.center_lat);
      if (meta_is_valid_double(md->projection->param.lamaz.center_lon))
        GTIFKeySet(ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.center_lon);
      if (meta_is_valid_double(md->projection->param.lamaz.false_easting))
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.false_easting);
      else
        GTIFKeySet(ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
      if (meta_is_valid_double(md->projection->param.lamaz.false_northing)) 
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
          md->projection->param.lamaz.false_northing);
      else
        GTIFKeySet(ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
      citation = MALLOC((max_citation_length + 1) * sizeof (char));
      snprintf(citation, max_citation_length + 1, "NSIDC EASE-Grid South");
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
      GTIFKeySet(ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
      FREE(citation);
    }
    break;
    case LAT_LONG_PSEUDO_PROJECTION:
    {
      write_datum_key(ogtif, md->projection->datum);
      write_spheroid_key(ogtif, md->projection->spheroid, re_major, re_minor);
    }
    break;
    default:
      asfPrintWarning ("Unsupported map projection found.  TIFF file will not\n"
        "contain projection information.\n");
    break;
  }
  
  // Get file information
  int ii, jj, kk;
  image_info_t *files = (image_info_t *) MALLOC(sizeof(image_info_t)*band);
  band = 0;
  fpList = FOPEN(input_file_list, "r");
  while (fgets(inFile, 512, fpList)) {
    chomp(inFile);
    files[band].fp = FOPEN(inFile, "rb");
    files[band].meta = meta_read(inFile);
    files[band].line = (float *) MALLOC(sizeof(float) * sample_count);
    band++;
  }
  FCLOSE(fpList);
  
  // Write the data to the file
  float *float_out_line = (float *) MALLOC(sizeof(float) * sample_count * band);
  for (ii=0; ii<md->general->line_count; ii++) {
    for (jj=0; jj<band; jj++)
      get_float_line(files[jj].fp, files[jj].meta, ii, files[jj].line);
    for (kk=0; kk<md->general->sample_count; kk++) {
      for (jj=0; jj<band; jj++)
        float_out_line[kk*band+jj] = files[jj].line[kk];
    }
    TIFFWriteScanline(otif, float_out_line, ii, 0);
    asfLineMeter(ii, md->general->line_count);
  }
  FREE(float_out_line);
  
  // Finalize GeoTIFF
  int ret = GTIFWriteKeys (ogtif);
  asfRequire(ret, "Error writing GeoTIFF keys.\n");
  GTIFFree(ogtif);
  XTIFFClose(otif);
  
  // Clean up
  meta_free(md);
  for (ii=0; ii<band; ii++) {
    FCLOSE(files[ii].fp);
    meta_free(files[ii].meta);
    FREE(files[ii].line);
  }
  FREE(files);
}
