#include "asf.h"
#include "asf_meta.h"
#include "geo_tiffp.h"
#include "geo_keyp.h"
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define ASF_EXPORT_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

typedef struct {
  // TIFF parameters
  long height;
  long width;
  short bits_per_sample;
  short sample_format;
  short color_space;
  short samples_per_pixel;
  // GeoTIFF Configuration Keys
  short model_type;
  short raster_type;
  // Geographic CS Parameter Keys
  short gcs;
  short geodetic_datum;
  short geog_prime_meridian;
  short geog_linear_units;
  short geog_angular_units;
  short geog_ellipsoid;
  double geog_semimajor_axis;
  double geog_semiminor_axis;
  double geog_inverse_flattening;
  // Projected CS Parameter Keys
  short pcs;
  short projection;
  short pct;
  short proj_linear_units;
  double proj_first_standard_parallel;
  double proj_second_standard_parallel;
  double proj_latitude_of_origin;
  double proj_central_meridian;
  double proj_false_easting;
  double proj_false_northing;
  double proj_center_latitude;
  double proj_center_longitude;
  double proj_center_easting;
  double proj_center_northing;
  double proj_scale_at_origin;
  double proj_scale_at_center;
  double proj_azimuth_angle;
  double proj_vertical_pole_longitude;
  int utm_zone;
  // Tie points
  double *tie_point;
  int tie_point_count;
  // Pixel scales
  double *pixel_scale;
  int pixel_scale_count;
} geotiff_info;

geotiff_info* initialize_geotiff_info(void)
{
  geotiff_info *ginfo = (geotiff_info *) MALLOC(sizeof(geotiff_info));
  
  // TIFF parameters
  ginfo->height = MAGIC_UNSET_INT;
  ginfo->width = MAGIC_UNSET_INT;
  ginfo->bits_per_sample = MAGIC_UNSET_SHORT;
  ginfo->sample_format = MAGIC_UNSET_SHORT;
  ginfo->color_space = MAGIC_UNSET_SHORT;
  ginfo->samples_per_pixel = MAGIC_UNSET_SHORT;
  // GeoTIFF Configuration Keys
  ginfo->model_type = MAGIC_UNSET_SHORT;
  ginfo->raster_type = MAGIC_UNSET_SHORT;
  // Geographic CS Parameter Keys
  ginfo->gcs = MAGIC_UNSET_SHORT;
  ginfo->geodetic_datum = MAGIC_UNSET_SHORT;
  ginfo->geog_prime_meridian = MAGIC_UNSET_SHORT;
  ginfo->geog_linear_units = MAGIC_UNSET_SHORT;
  ginfo->geog_angular_units = MAGIC_UNSET_SHORT;
  ginfo->geog_ellipsoid = MAGIC_UNSET_SHORT;
  ginfo->geog_semimajor_axis = MAGIC_UNSET_DOUBLE;
  ginfo->geog_semiminor_axis = MAGIC_UNSET_DOUBLE;
  ginfo->geog_inverse_flattening = MAGIC_UNSET_DOUBLE;
  // Projected CS Parameter Keys
  ginfo->pcs = MAGIC_UNSET_SHORT;
  ginfo->projection = MAGIC_UNSET_SHORT;
  ginfo->pct = MAGIC_UNSET_SHORT;
  ginfo->proj_linear_units = MAGIC_UNSET_SHORT;
  ginfo->proj_first_standard_parallel = MAGIC_UNSET_DOUBLE;
  ginfo->proj_second_standard_parallel = MAGIC_UNSET_DOUBLE;
  ginfo->proj_latitude_of_origin = MAGIC_UNSET_DOUBLE;
  ginfo->proj_central_meridian = MAGIC_UNSET_DOUBLE;
  ginfo->proj_false_easting = MAGIC_UNSET_DOUBLE;
  ginfo->proj_false_northing = MAGIC_UNSET_DOUBLE;
  ginfo->proj_center_latitude = MAGIC_UNSET_DOUBLE;
  ginfo->proj_center_longitude = MAGIC_UNSET_DOUBLE;
  ginfo->proj_center_easting = MAGIC_UNSET_DOUBLE;
  ginfo->proj_center_northing = MAGIC_UNSET_DOUBLE;
  ginfo->proj_scale_at_origin = MAGIC_UNSET_DOUBLE;
  ginfo->proj_scale_at_center = MAGIC_UNSET_DOUBLE;
  ginfo->proj_azimuth_angle = MAGIC_UNSET_DOUBLE;
  ginfo->proj_vertical_pole_longitude = MAGIC_UNSET_DOUBLE;
  ginfo->utm_zone = MAGIC_UNSET_INT;
  // Tie points
  ginfo->tie_point = NULL;
  ginfo->tie_point_count = MAGIC_UNSET_INT;
  // Pixel scale
  ginfo->pixel_scale = NULL;
  ginfo->pixel_scale_count = MAGIC_UNSET_INT;

  return ginfo;
}

int read_geotiff_info(char *in_file, geotiff_info *ginfo)
{
  TIFF *tif;
  TIFFErrorHandler oldHandler;
  oldHandler = TIFFSetWarningHandler(NULL);
  tif = XTIFFOpen (in_file, "r");
  if (tif == NULL)
    return FALSE;

  // Check the TIFF related tags 
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &ginfo->width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &ginfo->height);
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &ginfo->bits_per_sample);
  TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &ginfo->sample_format);
  TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &ginfo->color_space);
  TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &ginfo->samples_per_pixel);

  GTIF *gtif;
  int num_tie_points = 0;
  int num_pixel_scales = 0;
  double *tie_point = NULL;
  double *pixel_scale = NULL;
  gtif = GTIFNew (tif);
  if (gtif == NULL)
    return FALSE;
  else {
    (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_TIEPOINTS,
			   &num_tie_points, &tie_point);
    ginfo->tie_point = tie_point;
    ginfo->tie_point_count = num_tie_points;
    (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_PIXELSCALE,
			   &num_pixel_scales, &pixel_scale);
    ginfo->pixel_scale = pixel_scale;
    ginfo->pixel_scale_count = num_pixel_scales;
  }

  // GeoTIFF Configuration Keys
  // model type
  GTIFKeyGet(gtif, GTModelTypeGeoKey, &ginfo->model_type, 0, 1);
  // raster type
  GTIFKeyGet(gtif, GTRasterTypeGeoKey, &ginfo->raster_type, 0, 1);

  // Geographic CS Parameter Keys
  // geographic coordinate system
  GTIFKeyGet(gtif, GeographicTypeGeoKey, &ginfo->gcs, 0, 1);
  // geodetic datum
  GTIFKeyGet(gtif, GeogGeodeticDatumGeoKey, &ginfo->geodetic_datum, 0, 1);
  // prime meridian
  GTIFKeyGet(gtif, GeogPrimeMeridianGeoKey, &ginfo->geog_prime_meridian, 0, 1);
  // linear units
  GTIFKeyGet(gtif, GeogLinearUnitsGeoKey, &ginfo->geog_linear_units, 0, 1);
  // angular units
  GTIFKeyGet(gtif, GeogAngularUnitsGeoKey, &ginfo->geog_angular_units, 0, 1);
  // ellipsoid
  GTIFKeyGet(gtif, GeogEllipsoidGeoKey, &ginfo->geog_ellipsoid, 0, 1);
  // semi-major axis
  GTIFKeyGet(gtif, GeogSemiMajorAxisGeoKey, &ginfo->geog_semimajor_axis, 0, 1);
  // semi-minor axis
  GTIFKeyGet(gtif, GeogSemiMinorAxisGeoKey, &ginfo->geog_semiminor_axis, 0, 1);
  // inverse flattening
  GTIFKeyGet(gtif, GeogInvFlatteningGeoKey, 
	     &ginfo->geog_inverse_flattening, 0, 1);

  // Projected CS Parameter Keys
  // projected coordinate system
  GTIFKeyGet(gtif, ProjectedCSTypeGeoKey, &ginfo->pcs, 0, 1);
  // projection
  GTIFKeyGet(gtif, ProjectionGeoKey, &ginfo->projection, 0, 1);
  // projection coordinate transform
  GTIFKeyGet(gtif, ProjCoordTransGeoKey, &ginfo->pct, 0, 1);
  // linear units
  GTIFKeyGet(gtif, ProjLinearUnitsGeoKey, &ginfo->proj_linear_units, 0, 1);

  // Universal Transverse Mercator
  if (ginfo->pct == CT_TransverseMercator ||
      ginfo->pct == CT_TransvMercator_Modified_Alaska ||
      ginfo->pct == CT_TransvMercator_SouthOriented ||
      (ginfo->pcs >= 16001 && ginfo->pcs <= 16060) ||
      (ginfo->pcs >= 26703 && ginfo->pcs <= 26798) ||
      (ginfo->pcs >= 26903 && ginfo->pcs <= 26998) ||
      (ginfo->pcs >= 32601 && ginfo->pcs <= 32660) ||
      (ginfo->pcs >= 32701 && ginfo->pcs <= 32760)) {
    // latitude of origin
    GTIFKeyGet(gtif, ProjNatOriginLatGeoKey, 
	       &ginfo->proj_latitude_of_origin, 0, 1);
    // central meridian
    GTIFKeyGet(gtif, ProjNatOriginLongGeoKey, 
	       &ginfo->proj_central_meridian, 0, 1);
    // scale at origin
    GTIFKeyGet(gtif, ProjScaleAtNatOriginGeoKey, 
	       &ginfo->proj_scale_at_origin, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseEastingGeoKey, &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Albers Conical Equal Area
  else if (ginfo->pct == CT_AlbersEqualArea) {
    // first standard parallel
    GTIFKeyGet(gtif, ProjStdParallel1GeoKey, 
	       &ginfo->proj_first_standard_parallel, 0, 1);
    // second standard parallel
    GTIFKeyGet(gtif, ProjStdParallel2GeoKey, 
	       &ginfo->proj_second_standard_parallel, 0, 1);
    // latitude of origin
    GTIFKeyGet(gtif, ProjNatOriginLatGeoKey, 
	       &ginfo->proj_latitude_of_origin, 0, 1);
    // central meridian
    GTIFKeyGet(gtif, ProjNatOriginLongGeoKey, 
	       &ginfo->proj_central_meridian, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseEastingGeoKey, &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Lambert Conformal Conic
  else if (ginfo->pct == CT_LambertConfConic_2SP) {
    // first standard parallel
    GTIFKeyGet(gtif, ProjStdParallel1GeoKey, 
	       &ginfo->proj_first_standard_parallel, 0, 1);
    // second standard parallel
    GTIFKeyGet(gtif, ProjStdParallel2GeoKey, 
	       &ginfo->proj_second_standard_parallel, 0, 1);
    // latitude of origin
    GTIFKeyGet(gtif, ProjFalseOriginLatGeoKey, 
	       &ginfo->proj_latitude_of_origin, 0, 1);
    // central meridian
    GTIFKeyGet(gtif, ProjFalseOriginLongGeoKey, 
	       &ginfo->proj_central_meridian, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseOriginEastingGeoKey, 
	       &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseOriginNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Polar Stereographic projection
  else if (ginfo->pct == CT_PolarStereographic) {
    // latitude of origin
    GTIFKeyGet(gtif, ProjNatOriginLatGeoKey, 
	       &ginfo->proj_latitude_of_origin, 0, 1);
    // vertical pole longitude - central meridian
    GTIFKeyGet(gtif, ProjStraightVertPoleLongGeoKey, 
	       &ginfo->proj_central_meridian, 0, 1);
    // scale at origin
    GTIFKeyGet(gtif, ProjScaleAtNatOriginGeoKey, 
	       &ginfo->proj_scale_at_origin, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseEastingGeoKey, &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Lambert Azimuthal Equal Area
  else if (ginfo->pct == CT_LambertAzimEqualArea) {
    // center latitude
    GTIFKeyGet(gtif, ProjCenterLatGeoKey, &ginfo->proj_center_latitude, 0, 1);
    // center longitude
    GTIFKeyGet(gtif, ProjCenterLongGeoKey, &ginfo->proj_center_longitude, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseEastingGeoKey, &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Equirectangular projection
  else if (ginfo->pct == CT_Equirectangular) {
    // first standard parallel
    GTIFKeyGet(gtif, ProjStdParallel1GeoKey, 
	       &ginfo->proj_first_standard_parallel, 0, 1);
    // center latitude
    GTIFKeyGet(gtif, ProjCenterLatGeoKey, &ginfo->proj_center_latitude, 0, 1);
    // center longitude
    GTIFKeyGet(gtif, ProjCenterLongGeoKey, &ginfo->proj_center_longitude, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseEastingGeoKey, &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Equidistant projection
  else if (ginfo->pct == 32663) {
    // center latitude
    GTIFKeyGet(gtif, ProjCenterLatGeoKey, &ginfo->proj_center_latitude, 0, 1);
    // center longitude
    GTIFKeyGet(gtif, ProjCenterLongGeoKey, &ginfo->proj_center_longitude, 0, 1);
  }

  // Mercator projection
  else if (ginfo->pct == CT_Mercator) {
    // latitude of origin
    GTIFKeyGet(gtif, ProjNatOriginLatGeoKey, 
	       &ginfo->proj_latitude_of_origin, 0, 1);
    // central meridian
    GTIFKeyGet(gtif, ProjNatOriginLongGeoKey, 
	       &ginfo->proj_central_meridian, 0, 1);
    // scale at origin
    GTIFKeyGet(gtif, ProjScaleAtNatOriginGeoKey, 
	       &ginfo->proj_scale_at_origin, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseEastingGeoKey, &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Sinusoidal projection
  else if (ginfo->pct == CT_Sinusoidal) {
    // latitude of origin
    GTIFKeyGet(gtif, ProjCenterLongGeoKey, &ginfo->proj_center_longitude, 0, 1);
    // false easting
    GTIFKeyGet(gtif, ProjFalseEastingGeoKey, &ginfo->proj_false_easting, 0, 1);
    // false northing
    GTIFKeyGet(gtif, ProjFalseNorthingGeoKey, 
	       &ginfo->proj_false_northing, 0, 1);
  }

  // Clean up
  GTIFFree(gtif);
  XTIFFClose(tif);

  return TRUE;
}

void finalize_geotiff_info(geotiff_info *ginfo)
{
  FREE(ginfo->tie_point);
  FREE(ginfo->pixel_scale);
  FREE(ginfo);
}

static char *model_type2str(short model_type)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (model_type == ModelTypeProjected)
    strcpy(str, "Projection Coordinate System");
  else if (model_type == ModelTypeGeographic)
    strcpy(str, "Geographic latitude-longitude System");
  else if (model_type == ModelTypeGeocentric)
    strcpy(str, "Geocentric (X,Y,Z) Coordinate System");

  return str;
}

static char *raster_type2str(short raster_type)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (raster_type == RasterPixelIsArea)
    strcpy(str, "Area");
  else if (raster_type == RasterPixelIsPoint)
    strcpy(str, "Point");

  return str;
}

static char *gcs2str(short gcs)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (gcs == GCS_ED50)
    strcpy(str, "ED50");
  else if (gcs == GCS_NAD27)
    strcpy(str, "NAD27");
  else if (gcs == GCS_NAD83)
    strcpy(str, "NAD83");
  else if (gcs == GCS_SAD69)
    strcpy(str, "SAD69");
  else if (gcs == GCS_WGS_72)
    strcpy(str, "WGS 72");
  else if (gcs == GCS_WGS_84)
    strcpy(str, "WGS 84");
  else if (gcs == GCSE_Bessel1841)
    strcpy(str, "Bessel1841");
  else if (gcs == GCSE_Clarke1866)
    strcpy(str, "Clarke1866");
  else if (gcs == GCSE_GRS1980)
    strcpy(str, "GRS1980");
  else if (gcs == GCSE_WGS84)
    strcpy(str, "WGS 84");
  else if (gcs == GCSE_GEM10C)
    strcpy(str, "GEM10C");
  else if (gcs == GCSE_Sphere)
    strcpy(str, "Sphere");
  else if (gcs == 4054)
    strcpy(str, "Unspecified datum based upon the Hughes 1980 ellipsoid");
  else if (gcs == 32767)
    strcpy(str, "user defined");
  else if (gcs == 0 || gcs == -9999)
    strcpy(str, "unnamed");

  return str;
}

static char *geodetic_datum2str(short geodetic_datum)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (geodetic_datum == Datum_North_American_Datum_1927)
    strcpy(str, "North_American_Datum_1927");
  else if (geodetic_datum == Datum_North_American_Datum_1983)
    strcpy(str, "North_American_Datum_1983");
  else if (geodetic_datum == Datum_WGS72)
    strcpy(str, "WGS_1972");
  else if (geodetic_datum == Datum_WGS84)
    strcpy(str, "WGS_1984");
  else if (geodetic_datum == 6655)
    strcpy(str, "ITRF97");
  else if (geodetic_datum == 6054)
    strcpy(str, "Not_specified_based_on_Hughes_1980_ellipsoid");
  else if (geodetic_datum == 32767)
    strcpy(str, "user defined");
  else if (geodetic_datum == 0 || geodetic_datum == -9999)
    strcpy(str, "unknown");

  return str;
}

static char *prime_meridian2str(short prime_meridian)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (prime_meridian == PM_Greenwich)
    strcpy(str, "Greenwhich");
  else if (prime_meridian == 0 || prime_meridian == -9999)
    strcpy(str, "undefined");
  else if (prime_meridian == 32767)
    strcpy(str, "user-defined");

  return str;
}

static char *linear_units2str(short linear_units)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (linear_units == Linear_Meter)
    strcpy(str, "meters");
  else if (linear_units == Linear_Foot ||
	   linear_units == Linear_Foot_US_Survey ||
	   linear_units == Linear_Foot_Modified_American ||
	   linear_units == Linear_Foot_Clarke ||
	   linear_units == Linear_Foot_Indian)
    strcpy(str, "feet");
  else if (linear_units == 0 || linear_units == -9999)
    strcpy(str, "undefined");

  return str;
}

static char *angular_units2str(short angular_units)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (angular_units == Angular_Radian)
    strcpy(str, "radians");
  else if (angular_units == Angular_Degree)
    strcpy(str, "degrees");
  else if (angular_units == Angular_Arc_Minute)
    strcpy(str, "arc minutes");
  else if (angular_units == Angular_Arc_Second)
    strcpy(str, "arc seconds");
  else if (angular_units == 0 || angular_units == -9999)
    strcpy(str, "undefined");

  return str;
}

static char *ellipsoid2str(short ellipsoid)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (ellipsoid == Ellipse_Bessel_1841)
    strcpy(str, "Bessel 1841");
  else if (ellipsoid == Ellipse_Clarke_1866)
    strcpy(str, "Clarke 1866");
  else if (ellipsoid == Ellipse_GRS_1980)
    strcpy(str, "GRS 1980");
  else if (ellipsoid == Ellipse_WGS_84)
    strcpy(str, "WGS 84");
  else if (ellipsoid == Ellipse_GEM_10C)
    strcpy(str, "GEM 10C");
  else if (ellipsoid == Ellipse_Sphere)
    strcpy(str, "Sphere");
  else if (ellipsoid == 7058)
    strcpy(str, "Hughes 1980");
  else if (ellipsoid == 32767)
    strcpy(str, "unnamed");
  else if (ellipsoid == 0 || ellipsoid == -9999)
    strcpy(str, "undefined");

  return str;
}

static char *pcs2str(geotiff_info *ginfo)
{
  char *str = (char *) MALLOC(sizeof(char)*256);
  short pcs = ginfo->pcs;

  unsigned long zone = pcs - (pcs / 100) * 100;

  if (pcs >= 16001 && pcs <= 16060) {
    sprintf(str, "UTM zone %ldN", zone);
    ginfo->utm_zone = (int) zone;
  }
  else if (pcs >= 16101 && pcs <= 16160) {
    sprintf(str, "UTM zone %ldS", zone);
    ginfo->utm_zone = (int) zone;
  }
  else if (pcs >= 26703 && pcs <= 26798) {
    sprintf(str, "NAD 27 / UTM zone %ld", zone);
    ginfo->utm_zone = (int) zone;
    ginfo->geodetic_datum = Datum_North_American_Datum_1927;
  }
  else if (pcs >= 26903 && pcs <= 26998) {
    sprintf(str, "NAD 83 / UTM zone %ld", zone);
    ginfo->utm_zone = (int) zone;
    ginfo->geodetic_datum = Datum_North_American_Datum_1983;
  }
  else if (pcs >= 32601 && pcs <= 32660) {
    sprintf(str, "WGS 84 / UTM zone %ldN", zone);
    ginfo->utm_zone = (int) zone;
    ginfo->geodetic_datum = Datum_WGS84;
  }
  else if (pcs >= 32701 && pcs <= 32760) {
    sprintf(str, "WGS 84 / UTM zone %ldS", zone);
    ginfo->utm_zone = (int) zone;
    ginfo->geodetic_datum = Datum_WGS84;
  }
  else if (pcs == 3411)
    strcpy(str, "Hughes / NSIDC Polar Stereographic North");
  else if (pcs == 32767)
    strcpy(str, "user defined");
  else if (pcs == 0 || pcs == -9999)
    strcpy(str, "undefined");
    
  return str;
}

static char *projection2str(short projection)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (projection == 32767)
    strcpy(str, "user defined");
  else if (projection == 0 || projection == -9999)
    strcpy(str, "undefined");
    
  return str;
}

static char *pct2str(short pct)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (pct == CT_TransverseMercator)
    strcpy(str, "Transverse_Mercator");
  else if (pct == CT_Mercator)
    strcpy(str, "Mercator_1SP");
  else if (pct == CT_LambertConfConic_2SP)
    strcpy(str, "Lambert_Conformal_Conic_2SP");
  else if (pct == CT_LambertAzimEqualArea)
    strcpy(str, "Lambert_Azimuthal_Equal_Area");
  else if (pct == CT_AlbersEqualArea)
    strcpy(str, "Albers_Conic_Equal_Area");
  else if (pct == CT_AzimuthalEquidistant)
    strcpy(str, "Azimuthal_Equidistant");
  else if (pct == CT_PolarStereographic)
    strcpy(str, "Polar_Stereographic");
  else if (pct == CT_Equirectangular)
    strcpy(str, "Equirectangular");
  else if (pct == 32663)
    strcpy(str, "WGS 84 / World Equidistant Cylindrical");
  else if (pct == CT_Sinusoidal)
    strcpy(str, "Sinusoidal");
  else if (pct == 32767)
    strcpy(str, "user defined");
  else if (pct == 0 || pct == -9999)
    strcpy(str, "undefined");
    
  return str;
}

int geotiff_test(char *in_file, char *spec_file)
{
  return geotiff_test_ext(in_file, spec_file, REPORT_LEVEL_NONE);
}

int geotiff_test_ext(char *in_file, char *spec_file, report_level_t level)
{
  char *filename = NULL, line[1024], param[100], type[25], valid[500], *p;
  char strValue[100];
  int nMin, nMax, nValue, nValid, singleValue, map_param, passed = TRUE;
  double lfMin, lfMax, lfValue, lfValid;

  // First try to find the spec file locally
  FILE *fp = fopen(spec_file, "r");
  if (!fp) {
    filename = appendExt(spec_file, ".specs");
    fp = fopen(filename, "r");
  }

  // Second try the spec file from share directory
  if (!fp) {
    FREE(filename);
    filename = (char *) MALLOC(sizeof(char)*1024);
    sprintf(filename, "%s/geotiff_test/%s", get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp) {
    sprintf(filename, "%s/geotiff_test/%s.specs", 
	    get_asf_share_dir(), spec_file);
    fp = fopen(filename, "r");
  }
  if (!fp)
    asfPrintError("Could not find spec file (%s)\n", spec_file);
  if (filename)
    FREE(filename);

  geotiff_info *ginfo = initialize_geotiff_info();
  read_geotiff_info(in_file, ginfo);

  // Go through specs file
  while(fgets(line, 1024, fp)) {
    // skip comments
    if (strstr(line, "#"))
      continue;
    else
      line[strlen(line)-1] = '\0';

    // parameter
    p = strchr(line, ',');
    *p = '\0';
    sprintf(param, "%s", line);

    map_param = FALSE;

    if (strcmp_case(param, "image_width") == 0)
      nValue = ginfo->width;
    else if (strcmp_case(param, "image_height") == 0)
      nValue = ginfo->height;
    else if (strcmp_case(param, "bits_per_sample") == 0)
      nValue = ginfo->bits_per_sample;
    else if (strcmp_case(param, "sample_format") == 0)
      nValue = ginfo->sample_format;
    else if (strcmp_case(param, "color_space") == 0)
      nValue = ginfo->color_space;
    else if (strcmp_case(param, "samples_per_pixel") == 0)
      nValue = ginfo->samples_per_pixel;
    else if (strcmp_case(param, "model_type") == 0)
      strcpy(strValue, model_type2str(ginfo->model_type));
    else if (strcmp_case(param, "raster_type") == 0)
      strcpy(strValue, raster_type2str(ginfo->raster_type));
    else if (strcmp_case(param, "geographic_coordinate_system") == 0)
      strcpy(strValue, gcs2str(ginfo->gcs));
    else if (strcmp_case(param, "geodetic_datum") == 0)
      strcpy(strValue, geodetic_datum2str(ginfo->geodetic_datum));
    else if (strcmp_case(param, "geographic_prime_meridian") == 0)
      strcpy(strValue, prime_meridian2str(ginfo->geog_prime_meridian));
    else if (strcmp_case(param, "geographic_linear_units") == 0)
      strcpy(strValue, linear_units2str(ginfo->geog_linear_units));
    else if (strcmp_case(param, "geographic_angular_units") == 0)
      strcpy(strValue, angular_units2str(ginfo->geog_angular_units));
    else if (strcmp_case(param, "geographic_ellipsoid") == 0)
      strcpy(strValue, ellipsoid2str(ginfo->geog_ellipsoid));
    else if (strcmp_case(param, "geographic_semimajor_axis") == 0)
      lfValue = ginfo->geog_semimajor_axis;
    else if (strcmp_case(param, "geographic_semiminor_axis") == 0)
      lfValue = ginfo->geog_semiminor_axis;
    else if (strcmp_case(param, "geographic_inverse_flattening") == 0)
      lfValue = ginfo->geog_inverse_flattening;
    else if (strcmp_case(param, "projected_coordinate_system") == 0)
      strcpy(strValue, pcs2str(ginfo));
    else if (strcmp_case(param, "projection") == 0)
      strcpy(strValue, projection2str(ginfo->projection));
    else if (strcmp_case(param, "projection_coordinate_transformation") == 0)
      strcpy(strValue, pct2str(ginfo->pct));
    else if (strcmp_case(param, "projection_linear_units") == 0)
      strcpy(strValue, linear_units2str(ginfo->proj_linear_units));
    else if (strcmp_case(param, "projection_first_standard_parallel") == 0) {
      lfValue = ginfo->proj_first_standard_parallel;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_second_standard_parallel") == 0) {
      lfValue = ginfo->proj_second_standard_parallel;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_latitude_of_origin") == 0) {
      lfValue = ginfo->proj_latitude_of_origin;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_central_meridian") == 0) {
      lfValue = ginfo->proj_central_meridian;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_false_easting") == 0) {
      lfValue = ginfo->proj_false_easting;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_false_northing") == 0) {
      lfValue = ginfo->proj_false_northing;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_center_latitude") == 0) {
      lfValue = ginfo->proj_center_latitude;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_center_longitude") == 0) {
      lfValue = ginfo->proj_center_longitude;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_center_easting") == 0) {
      lfValue = ginfo->proj_center_easting;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_center_northing") == 0) {
      lfValue = ginfo->proj_center_northing;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_scale_at_origin") == 0) {
      lfValue = ginfo->proj_scale_at_origin;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_scale_at_center") == 0) {
      lfValue = ginfo->proj_scale_at_center;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_azimuth_angle") == 0) {
      lfValue = ginfo->proj_azimuth_angle;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "projection_vertical_pole_longitude") == 0) {
      lfValue = ginfo->proj_vertical_pole_longitude;
      map_param = TRUE;
    }
    else if (strcmp_case(param, "startX") == 0)
      lfValue = ginfo->tie_point[3];
    else if (strcmp_case(param, "startY") == 0)
      lfValue = ginfo->tie_point[4];
    else if (strcmp_case(param, "perX") == 0)
      lfValue = fabs(ginfo->pixel_scale[0]);
    else if (strcmp_case(param, "perY") == 0)
      lfValue = -(fabs(ginfo->pixel_scale[1]));

    // data type
    sprintf(line, "%s", p+1);
    p = strchr(line, ',');
    if (p)
      *p = '\0';
    sprintf(type, "%s", line);

    // valid
    // If no value is given, we just want to check the parameter is given in
    // the metadata file. If we find quotation marks then assume we have a list
    // of values to check. Otherwise, we assume to find a minimum and a maximum
    // value to check.
    if (p && !map_param) {
      sprintf(line, "%s", p+1);
      if (line[0] == '\'' && line[strlen(line)-1] == '\'') {
	sprintf(valid, "%s", line+1);
	valid[strlen(valid)-1] = '\0';
	if (strncmp_case(type, "CHAR", 4) == 0 &&
	    strlen(valid) > 0  && strcmp_case(strValue, valid) != 0) {
	  asfForcePrintStatus("\n   Param: %s, value: %s, valid: %s\n",
			 param, strValue, valid);
	  asfReport(level, "   %s failed\n", param);
	  passed = FALSE;
	}
      }
      else {
	if (strncmp_case(type, "INT", 3) == 0) {
	  if (strchr(line, ',')) {
	    singleValue = FALSE;
	    sscanf(line, "%d,%d", &nMin, &nMax);
	  }
	  else {
	    singleValue = TRUE;
	    sscanf(line, "%d", &nValid);
	  }
	  if (singleValue && nValue != nValid) {
	    asfForcePrintStatus("\n   Param: %s, value: %d, valid: %d\n",
			   param, nValue, nValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	  else if (!singleValue && (nValue < nMin || nValue > nMax)) {
	    asfForcePrintStatus("\n   Param: %s, value: %d, min: %d, max: %d\n",
			   param, nValue, nMin, nMax);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
	else if (strncmp_case(type, "DOUBLE", 6) == 0) {
	  if (strchr(line, ',')) {
	    singleValue = FALSE;
	    sscanf(line, "%lf,%lf", &lfMin, &lfMax);
	  }
	  else {
	    singleValue = TRUE;
	    sscanf(line, "%lf", &lfValid);
	  }
	  if (singleValue && !FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	  else if (!singleValue && (lfValue < lfMin || lfValue > lfMax)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, min: %lf, "
				"max: %lf\n", param, lfValue, lfMin, lfMax);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
    }
    else if (p && map_param) {
      sprintf(line, "%s", p+1);
      
      // Universal Transverse Mercator
      if (ginfo->pct == CT_TransverseMercator ||
	  ginfo->pct == CT_TransvMercator_Modified_Alaska ||
	  ginfo->pct == CT_TransvMercator_SouthOriented ||
	  (ginfo->pcs >= 16001 && ginfo->pcs <= 16060) ||
	  (ginfo->pcs >= 26703 && ginfo->pcs <= 26798) ||
	  (ginfo->pcs >= 26903 && ginfo->pcs <= 26998) ||
	  (ginfo->pcs >= 32601 && ginfo->pcs <= 32660) ||
	  (ginfo->pcs >= 32701 && ginfo->pcs <= 32760)) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_latitude_of_origin") == 0 ||
	    strcmp_case(param, "projection_central_meridian") == 0 ||
	    strcmp_case(param, "projection_scale_at_origin") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Albers Conical Equal Area
      else if (ginfo->pct == CT_AlbersEqualArea) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_first_standard_parallel") == 0 ||
	    strcmp_case(param, "projection_second_standard_parallel") == 0 ||
	    strcmp_case(param, "projection_latitude_of_origin") == 0 ||
	    strcmp_case(param, "projection_central_meridian") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Lambert Conformal Conic
      else if (ginfo->pct == CT_LambertConfConic_2SP) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_first_standard_parallel") == 0 ||
	    strcmp_case(param, "projection_second_standard_parallel") == 0 ||
	    strcmp_case(param, "projection_latitude_of_origin") == 0 ||
	    strcmp_case(param, "projection_central_meridian") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Polar Stereographic projection
      else if (ginfo->pct == CT_PolarStereographic) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_latitude_of_origin") == 0 ||
	    strcmp_case(param, "projection_central_meridian") == 0 ||
	    strcmp_case(param, "projection_scale_at_origin") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Lambert Azimuthal Equal Area
      else if (ginfo->pct == CT_LambertAzimEqualArea) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_center_latitude") == 0 ||
	    strcmp_case(param, "projection_center_longitude") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Equirectangular projection
      else if (ginfo->pct == CT_Equirectangular) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
      
	if (strcmp_case(param, "projection_first_standard_parallel") == 0 ||
	    strcmp_case(param, "projection_center_latitude") == 0 ||
	    strcmp_case(param, "projection_center_longitude") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Equidistant projection
      else if (ginfo->pct == 32663) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_center_latitude") == 0 ||
	    strcmp_case(param, "projection_center_longitude") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Mercator projection
      else if (ginfo->pct == CT_Mercator) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_central_meridian") == 0 ||
	    strcmp_case(param, "projection_scale_at_origin") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
      
      // Sinusoidal projection
      else if (ginfo->pct == CT_Sinusoidal) {
	
	double lfValid;
	sscanf(line, "%lf", &lfValid);
	
	if (strcmp_case(param, "projection_center_longitude") == 0 ||
	    strcmp_case(param, "projection_false_easting") == 0 ||
	    strcmp_case(param, "projection_false_northing") == 0) {
	  if (!FLOAT_EQUIVALENT(lfValue, lfValid)) {
	    asfForcePrintStatus("\n   Param: %s, value: %lf, valid: %lf\n",
			   param, lfValue, lfValid);
	    asfReport(level, "   %s failed\n", param);
	    passed = FALSE;
	  }
	}
      }
    }
  }
  finalize_geotiff_info(ginfo);

  return passed;
}

int geotiff_compare(char *in_file1, char *in_file2, report_level_t level)
{
  int passed = TRUE;

  // Read GeoTIFF metadata
  geotiff_info *ginfo1 = initialize_geotiff_info();
  read_geotiff_info(in_file1, ginfo1);
  geotiff_info *ginfo2 = initialize_geotiff_info();
  read_geotiff_info(in_file2, ginfo2);

  // Go through values
  if (ginfo1->width != ginfo2->width) {
    asfForcePrintStatus("image width - value(%s): %d, value(%s): %d\n",
			in_file1, ginfo1->width, in_file2, ginfo2->width);
    passed = FALSE;
  }
  if (ginfo1->height != ginfo2->height) {
    asfForcePrintStatus("image height - value(%s): %d, value(%s): %d\n",
			in_file1, ginfo1->height, in_file2, ginfo2->height);
    passed = FALSE;
  }
  if (ginfo1->bits_per_sample != ginfo2->bits_per_sample) {
    asfForcePrintStatus("bits per sample - value(%s): %d, value(%s): %d\n",
			in_file1, ginfo1->bits_per_sample, 
			in_file2, ginfo2->bits_per_sample);
    passed = FALSE;
  }
  if (ginfo1->sample_format != ginfo2->sample_format) {
    asfForcePrintStatus("sample format - value(%s): %d, value(%s): %d\n",
			in_file1, ginfo1->sample_format, 
			in_file2, ginfo2->sample_format);
    passed = FALSE;
  }
  if (ginfo1->color_space != ginfo2->color_space) {
    asfForcePrintStatus("color space - value(%s): %d, value(%s): %d\n",
			in_file1, ginfo1->color_space, 
			in_file2, ginfo2->color_space);
    passed = FALSE;
  }
  if (ginfo1->samples_per_pixel != ginfo2->samples_per_pixel) {
    asfForcePrintStatus("samples per pixel - value(%s): %d, value(%s): %d\n",
			in_file1, ginfo1->samples_per_pixel, 
			in_file2, ginfo2->samples_per_pixel);
    passed = FALSE;
  }
  if (ginfo1->model_type != ginfo2->model_type) {
    asfForcePrintStatus("model type - value(%s): %s, value(%s): %s\n",
			in_file1, model_type2str(ginfo1->model_type), 
			in_file2, model_type2str(ginfo2->model_type));
    passed = FALSE;
  }
  if (ginfo1->raster_type != ginfo2->raster_type) {
    asfForcePrintStatus("raster type - value(%s): %s, value(%s): %s\n",
			in_file1, raster_type2str(ginfo1->raster_type), 
			in_file2, raster_type2str(ginfo2->raster_type));
    passed = FALSE;
  }
  if (ginfo1->gcs != ginfo2->gcs) {
    asfForcePrintStatus("geographic coordinate system - value(%s): %s, "
			"value(%s): %s\n",
			in_file1, gcs2str(ginfo1->gcs), 
			in_file2, gcs2str(ginfo2->gcs));
    passed = FALSE;
  }
  if (ginfo1->geodetic_datum != ginfo2->geodetic_datum) {
    asfForcePrintStatus("geodetic datum - value(%s): %s, value(%s): %s\n",
			in_file1, geodetic_datum2str(ginfo1->geodetic_datum), 
			in_file2, geodetic_datum2str(ginfo2->geodetic_datum));
    passed = FALSE;
  }
  if (ginfo1->geog_prime_meridian != ginfo2->geog_prime_meridian) {
    asfForcePrintStatus("geographic prime meridian - value(%s): %s, "
			"value(%s): %s\n",
			in_file1, prime_meridian2str(ginfo1->geog_prime_meridian), 
			in_file2, prime_meridian2str(ginfo2->geog_prime_meridian));
    passed = FALSE;
  }
  if (ginfo1->geog_linear_units != ginfo2->geog_linear_units) {
    asfForcePrintStatus("geographic linear units - value(%s): %s, "
			"value(%s): %s\n",
			in_file1, linear_units2str(ginfo1->geog_linear_units), 
			in_file2, linear_units2str(ginfo2->geog_linear_units));
    passed = FALSE;
  }
  if (ginfo1->geog_angular_units != ginfo2->geog_angular_units) {
    asfForcePrintStatus("geographic angular units - value(%s): %s, "
			"value(%s): %s\n",
			in_file1, angular_units2str(ginfo1->geog_angular_units), 
			in_file2, angular_units2str(ginfo2->geog_angular_units));
    passed = FALSE;
  }
  if (ginfo1->geog_ellipsoid != ginfo2->geog_ellipsoid) {
    asfForcePrintStatus("ellipsoid - value(%s): %s, value(%s): %s\n",
			in_file1, ellipsoid2str(ginfo1->geog_ellipsoid), 
			in_file2, ellipsoid2str(ginfo2->geog_ellipsoid));
    passed = FALSE;
  }
  if (!FLOAT_EQUIVALENT(ginfo1->geog_semimajor_axis, 
			ginfo2->geog_semimajor_axis)) {
    asfForcePrintStatus("semimajor axis - value(%s): %lf, value(%s): %lf\n",
			in_file1, ginfo1->geog_semimajor_axis, 
			in_file2, ginfo2->geog_semimajor_axis);
    passed = FALSE;
  }
  if (!FLOAT_EQUIVALENT(ginfo1->geog_semiminor_axis, 
			ginfo2->geog_semiminor_axis)) {
    asfForcePrintStatus("semiminor axis - value(%s): %lf, value(%s): %lf\n",
			in_file1, ginfo1->geog_semiminor_axis, 
			in_file2, ginfo2->geog_semiminor_axis);
    passed = FALSE;
  }
  if (!FLOAT_EQUIVALENT(ginfo1->geog_inverse_flattening,
			ginfo2->geog_inverse_flattening)) {
    asfForcePrintStatus("inverse flattening - value(%s): %lf, value(%s): %lf\n",
			in_file1, ginfo1->geog_inverse_flattening, 
			in_file2, ginfo2->geog_inverse_flattening);
    passed = FALSE;
  }
  if (ginfo1->model_type == ModelTypeProjected) {

    if (strcmp_case(pcs2str(ginfo1), pcs2str(ginfo2)) != 0) {
      asfForcePrintStatus("projected coordinate system - value(%s): %s, "
			  "value(%s): %s\n",
			  in_file1, pcs2str(ginfo1), in_file2, pcs2str(ginfo2));
      passed = FALSE;
    }
    if (ginfo1->projection != ginfo2->projection) {
      asfForcePrintStatus("projection - value(%s): %s, value(%s): %s\n",
			  in_file1, projection2str(ginfo1->projection), 
			  in_file2, projection2str(ginfo2->projection));
      passed = FALSE;
    }
    if (ginfo1->pct != ginfo2->pct) {
      asfForcePrintStatus("projection coordinate transformation - "
			  "value(%s): %s, value(%s): %s\n",
			  in_file1, pct2str(ginfo1->pct), 
			  in_file2, pct2str(ginfo2->pct));
      passed = FALSE;
    }
    if (ginfo1->proj_linear_units != ginfo2->proj_linear_units) {
      asfForcePrintStatus("projection linear units - value(%s): %s, "
			  "value(%s): %s\n",
			  in_file1, linear_units2str(ginfo1->proj_linear_units), 
			  in_file2, linear_units2str(ginfo2->proj_linear_units));
      passed = FALSE;
    }
  }
  if (!FLOAT_EQUIVALENT(ginfo1->tie_point[3], ginfo2->tie_point[3])) {
    asfForcePrintStatus("startX - value(%s): %lf, value(%s): %lf\n",
			in_file1, ginfo1->tie_point[3], 
			in_file2, ginfo2->tie_point[3]);
    passed = FALSE;
  }
  if (!FLOAT_EQUIVALENT(ginfo1->tie_point[4], ginfo2->tie_point[4])) {
    asfForcePrintStatus("startY - value(%s): %lf, value(%s): %lf\n",
			in_file1, ginfo1->tie_point[4], 
			in_file2, ginfo2->tie_point[4]);
    passed = FALSE;
  }
  if (!FLOAT_EQUIVALENT(ginfo1->pixel_scale[0], ginfo2->pixel_scale[0])) {
    asfForcePrintStatus("perX - value(%s): %lf, value(%s): %lf\n",
			in_file1, fabs(ginfo1->pixel_scale[0]), 
			in_file2, fabs(ginfo2->pixel_scale[0]));
    passed = FALSE;
  }
  if (!FLOAT_EQUIVALENT(ginfo1->pixel_scale[1], ginfo2->pixel_scale[1])) {
    asfForcePrintStatus("perX - value(%s): %lf, value(%s): %lf\n",
			in_file1, -(fabs(ginfo1->pixel_scale[1])), 
			in_file2, -(fabs(ginfo2->pixel_scale[1])));
    passed = FALSE;
  }

  if (ginfo1->model_type == ModelTypeProjected) {
    
    // Universal Transverse Mercator
    if (ginfo1->pct == CT_TransverseMercator ||
	ginfo1->pct == CT_TransvMercator_Modified_Alaska ||
	ginfo1->pct == CT_TransvMercator_SouthOriented ||
	(ginfo1->pcs >= 16001 && ginfo1->pcs <= 16060) ||
	(ginfo1->pcs >= 26703 && ginfo1->pcs <= 26798) ||
	(ginfo1->pcs >= 26903 && ginfo1->pcs <= 26998) ||
	(ginfo1->pcs >= 32601 && ginfo1->pcs <= 32660) ||
	(ginfo1->pcs >= 32701 && ginfo1->pcs <= 32760)) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_latitude_of_origin,
			    ginfo2->proj_latitude_of_origin)) {
	asfForcePrintStatus("projection latitude of origin - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_latitude_of_origin, 
			    in_file2, ginfo2->proj_latitude_of_origin);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_central_meridian,
			    ginfo2->proj_central_meridian)) {
	asfForcePrintStatus("projection central meridian - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_central_meridian, 
			    in_file2, ginfo2->proj_central_meridian);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_scale_at_origin,
			    ginfo2->proj_scale_at_origin)) {
	asfForcePrintStatus("projection scale at origin - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_scale_at_origin, 
			    in_file2, ginfo2->proj_scale_at_origin);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }
    
    // Albers Conical Equal Area
    else if (ginfo1->pct == CT_AlbersEqualArea) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_first_standard_parallel,
			    ginfo2->proj_first_standard_parallel)) {
	asfForcePrintStatus("projection first standard parallel - "
			    "value(%s): %lf, value(%s): %lf\n",
			    in_file1, ginfo1->proj_first_standard_parallel, 
			    in_file2, ginfo2->proj_first_standard_parallel);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_second_standard_parallel,
			    ginfo2->proj_second_standard_parallel)) {
	asfForcePrintStatus("projection second standard parallel - "
			    "value(%s): %lf, value(%s): %lf\n",
			    in_file1, ginfo1->proj_second_standard_parallel, 
			    in_file2, ginfo2->proj_second_standard_parallel);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_latitude_of_origin,
			    ginfo2->proj_latitude_of_origin)) {
	asfForcePrintStatus("projection latitude of origin - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_latitude_of_origin, 
			    in_file2, ginfo2->proj_latitude_of_origin);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_central_meridian,
			    ginfo2->proj_central_meridian)) {
	asfForcePrintStatus("projection central meridian - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_central_meridian, 
			    in_file2, ginfo2->proj_central_meridian);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }
    
    // Lambert Conformal Conic
    else if (ginfo1->pct == CT_LambertConfConic_2SP) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_first_standard_parallel,
			    ginfo2->proj_first_standard_parallel)) {
	asfForcePrintStatus("projection first standard parallel - "
			    "value(%s): %lf, value(%s): %lf\n",
			    in_file1, ginfo1->proj_first_standard_parallel, 
			    in_file2, ginfo2->proj_first_standard_parallel);
	passed = FALSE;
    }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_second_standard_parallel,
			    ginfo2->proj_second_standard_parallel)) {
	asfForcePrintStatus("projection second standard parallel - "
			    "value(%s): %lf, value(%s): %lf\n",
			    in_file1, ginfo1->proj_second_standard_parallel, 
			    in_file2, ginfo2->proj_second_standard_parallel);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_latitude_of_origin,
			    ginfo2->proj_latitude_of_origin)) {
	asfForcePrintStatus("projection latitude of origin - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_latitude_of_origin, 
			    in_file2, ginfo2->proj_latitude_of_origin);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_central_meridian,
			    ginfo2->proj_central_meridian)) {
	asfForcePrintStatus("projection central meridian - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_central_meridian, 
			    in_file2, ginfo2->proj_central_meridian);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }
    
    // Polar Stereographic projection
    else if (ginfo1->pct == CT_PolarStereographic) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_latitude_of_origin,
			    ginfo2->proj_latitude_of_origin)) {
	asfForcePrintStatus("projection latitude of origin - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_latitude_of_origin, 
			    in_file2, ginfo2->proj_latitude_of_origin);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_central_meridian,
			    ginfo2->proj_central_meridian)) {
	asfForcePrintStatus("projection central meridian - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_central_meridian, 
			    in_file2, ginfo2->proj_central_meridian);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_scale_at_origin,
			    ginfo2->proj_scale_at_origin)) {
	asfForcePrintStatus("projection scale at origin - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_scale_at_origin, 
			    in_file2, ginfo2->proj_scale_at_origin);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }
    
    // Lambert Azimuthal Equal Area
    else if (ginfo1->pct == CT_LambertAzimEqualArea) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_center_latitude,
			    ginfo2->proj_center_latitude)) {
	asfForcePrintStatus("projection center latitude - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_center_latitude, 
			    in_file2, ginfo2->proj_center_latitude);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_center_longitude,
			    ginfo2->proj_center_longitude)) {
	asfForcePrintStatus("projection center longitude - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_center_longitude, 
			    in_file2, ginfo2->proj_center_longitude);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }
    
    // Equirectangular projection
    else if (ginfo1->pct == CT_Equirectangular) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_first_standard_parallel,
			    ginfo2->proj_first_standard_parallel)) {
	asfForcePrintStatus("projection first standard parallel - "
			    "value(%s): %lf, value(%s): %lf\n",
			    in_file1, ginfo1->proj_first_standard_parallel, 
			    in_file2, ginfo2->proj_first_standard_parallel);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_center_latitude,
			    ginfo2->proj_center_latitude)) {
	asfForcePrintStatus("projection center latitude - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_center_latitude, 
			    in_file2, ginfo2->proj_center_latitude);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_center_longitude,
			    ginfo2->proj_center_longitude)) {
	asfForcePrintStatus("projection center longitude - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_center_longitude, 
			    in_file2, ginfo2->proj_center_longitude);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }
    
    // Equidistant projection
    else if (ginfo1->pct == 32663) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_center_latitude,
			    ginfo2->proj_center_latitude)) {
	asfForcePrintStatus("projection center latitude - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_center_latitude, 
			    in_file2, ginfo2->proj_center_latitude);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_center_longitude,
			    ginfo2->proj_center_longitude)) {
	asfForcePrintStatus("projection center longitude - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_center_longitude, 
			    in_file2, ginfo2->proj_center_longitude);
	passed = FALSE;
      }
    }
    
    // Mercator projection
    else if (ginfo1->pct == CT_Mercator) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_latitude_of_origin,
			    ginfo2->proj_latitude_of_origin)) {
	asfForcePrintStatus("projection latitude of origin - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_latitude_of_origin, 
			    in_file2, ginfo2->proj_latitude_of_origin);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_central_meridian,
			    ginfo2->proj_central_meridian)) {
	asfForcePrintStatus("projection central meridian - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_central_meridian, 
			    in_file2, ginfo2->proj_central_meridian);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }
    
    // Sinusoidal projection
    else if (ginfo1->pct == CT_Sinusoidal) {
      
      if (!FLOAT_EQUIVALENT(ginfo1->proj_center_longitude,
			    ginfo2->proj_center_longitude)) {
	asfForcePrintStatus("projection center longitude - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_center_longitude, 
			    in_file2, ginfo2->proj_center_longitude);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_easting,
			    ginfo2->proj_false_easting)) {
	asfForcePrintStatus("projection false easting - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_easting, 
			    in_file2, ginfo2->proj_false_easting);
	passed = FALSE;
      }
      if (!FLOAT_EQUIVALENT(ginfo1->proj_false_northing,
			    ginfo2->proj_false_northing)) {
	asfForcePrintStatus("projection false northing - value(%s): %lf, "
			    "value(%s): %lf\n",
			    in_file1, ginfo1->proj_false_northing, 
			    in_file2, ginfo2->proj_false_northing);
	passed = FALSE;
      }
    }    
  }

  finalize_geotiff_info(ginfo1);
  finalize_geotiff_info(ginfo2);
  
  return passed;
}
