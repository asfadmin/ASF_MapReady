#include <ctype.h>
#include <stdio.h>

#include "asf_meta.h"
#include "asf_nan.h"
#include "caplib.h"
#include "err_die.h"

void meta_put_string(FILE *meta_file,char *name,char  *value,char *comment);
void meta_put_double(FILE *meta_file,char *name,double value,char *comment);
void meta_put_int   (FILE *meta_file,char *name,int    value,char *comment);
void meta_put_char  (FILE *meta_file,char *name,char   value,char *comment);

/* Given a meta_parameters structure pointer and a file name, write a
   metadata file for that structure.  */
void meta_write(meta_parameters *meta, const char *file_name)
{
  /* Maximum file name length, including trailing null.  */
#define FILE_NAME_MAX 1000
  char *file_name_with_extension = appendExt(file_name, ".meta");
  FILE *fp = FOPEN(file_name_with_extension, "w");
  char comment[256];

  FREE(file_name_with_extension);

  /* Write an 'about meta file' comment  */
  fprintf(fp,
  	"# This file contains the metadata for satellite capture file of the same base name.\n"
	"#      '%c' is likely an unknown single character value.\n"
	"#      '%s' is likely an unknown string of characters.\n"
	"#      '%d' is likely an unknown integer value.\n"
	"#      '%lf' is likely an unknown Real value.\n\n",
	MAGIC_UNSET_CHAR, MAGIC_UNSET_STRING, MAGIC_UNSET_INT, MAGIC_UNSET_DOUBLE);

  /* We always write out files corresponding to the latest meta version.  */
  fprintf(fp, "meta_version: %.2f\n\n", META_VERSION);

/* General block.  */
  meta_put_string(fp,"general {", "","Begin parameters generally used in remote sensing");
  meta_put_string(fp,"sensor:", meta->general->sensor, "Imaging sensor");
  meta_put_string(fp,"mode:",meta->general->mode,"Imaging mode");
  meta_put_string(fp,"processor:", meta->general->processor,"Name and Version of Processor");
  strcpy(comment,"Type of samples (e.g. REAL64)");
  switch (meta->general->data_type) {
    case BYTE:
      meta_put_string(fp,"data_type:","BYTE",comment);
      break;
    case INTEGER16:
      meta_put_string(fp,"data_type:","INTEGER16",comment);
      break;
    case INTEGER32:
      meta_put_string(fp,"data_type:","INTEGER32",comment);
      break;
    case REAL32:
      meta_put_string(fp,"data_type:","REAL32",comment);
      break;
    case REAL64:
      meta_put_string(fp,"data_type:","REAL64",comment);
      break;
    case COMPLEX_BYTE:
      meta_put_string(fp,"data_type:","COMPLEX_BYTE",comment);
      break;
    case COMPLEX_INTEGER16:
      meta_put_string(fp,"data_type:","COMPLEX_INTEGER16",comment);
      break;
    case COMPLEX_INTEGER32:
      meta_put_string(fp,"data_type:","COMPLEX_INTEGER32",comment);
      break;
    case COMPLEX_REAL32:
      meta_put_string(fp,"data_type:","COMPLEX_REAL32",comment);
      break;
    case COMPLEX_REAL64:
      meta_put_string(fp,"data_type:","COMPLEX_REAL64",comment);
      break;
    default:
      meta_put_string(fp,"data_type:",MAGIC_UNSET_STRING,comment);
      break;
  }
  if (META_VERSION >= 1.2) {
    strcpy(comment,"Image data type (e.g. AMPLITUDE_IMAGE)");
    switch (meta->general->image_data_type) {
      case RAW_IMAGE:
	meta_put_string(fp,"image_data_type:","RAW_IMAGE",comment);
	break;
      case COMPLEX_IMAGE:
	meta_put_string(fp,"image_data_type:","COMPLEX_IMAGE",comment);
	break;
      case AMPLITUDE_IMAGE:
	meta_put_string(fp,"image_data_type:","AMPLITUDE_IMAGE",comment);
	break;
      case PHASE_IMAGE:
	meta_put_string(fp,"image_data_type:","PHASE_IMAGE",comment);
	break;
      case POWER_IMAGE:
	meta_put_string(fp,"image_data_type:","POWER_IMAGE",comment);
	break;
      case SIGMA_IMAGE:
	meta_put_string(fp,"image_data_type:","SIGMA_IMAGE",comment);
	break;
      case GAMMA_IMAGE:
	meta_put_string(fp,"image_data_type:","GAMMA_IMAGE",comment);
	break;
      case BETA_IMAGE:
	meta_put_string(fp,"image_data_type:","BETA_IMAGE",comment);
	break;
      case COHERENCE_IMAGE:
	meta_put_string(fp,"image_data_type:","COHERENCE_IMAGE",comment);
	break;
      case GEOCODED_IMAGE:
	meta_put_string(fp,"image_data_type:","GEOCODED_IMAGE",comment);
	break;
      case LUT_IMAGE:
	meta_put_string(fp,"image_data_type:","LUT_IMAGE",comment);
	break;
      case ELEVATION:
	meta_put_string(fp,"image_data_type:","ELEVATION",comment);
	break;
      case DEM:
	meta_put_string(fp,"image_data_type:","DEM",comment);
	break;
      case IMAGE:
	meta_put_string(fp,"image_data_type:","IMAGE",comment);
	break;
      default:
	meta_put_string(fp,"image_data_type:","???",comment);
	break;
    }
  }
  meta_put_string(fp,"system:", meta->general->system,
		  "System of samples (e.g. big_ieee)");
  meta_put_int   (fp,"orbit:", meta->general->orbit,
		  "Orbit Number for this datatake");
  meta_put_char  (fp,"orbit_direction:", meta->general->orbit_direction,
		  "Ascending 'A', or descending 'D'");
  meta_put_int   (fp,"frame:", meta->general->frame,
		  "Frame for this image [-1 if n/a]");
  meta_put_int   (fp,"band_number:", meta->general->band_number,
		  "Band number; first band is 0");
  meta_put_int   (fp,"line_count:", meta->general->line_count,
		  "Number of lines in image");
  meta_put_int   (fp,"sample_count:", meta->general->sample_count,
		  "Number of samples in image");
  meta_put_int   (fp,"start_line:", meta->general->start_line,
		  "First line relative to original image");
  meta_put_int   (fp,"start_sample:", meta->general->start_sample,
		  "First sample relative to original image");
  meta_put_double(fp,"x_pixel_size:", meta->general->x_pixel_size,
		  "Range pixel size [m]");
  meta_put_double(fp,"y_pixel_size:", meta->general->y_pixel_size,
		  "Azimuth pixel size [m]");
  meta_put_double(fp,"center_latitude:", meta->general->center_latitude,
		  "Approximate image center latitude");
  meta_put_double(fp,"center_longitude:", meta->general->center_longitude,
		  "Approximate image center longitude");
  meta_put_double(fp,"re_major:", meta->general->re_major,
		  "Major (equator) Axis of earth [m]");
  meta_put_double(fp,"re_minor:", meta->general->re_minor,
		  "Minor (polar) Axis of earth [m]");
  meta_put_double(fp,"bit_error_rate:", meta->general->bit_error_rate,
		  "Fraction of bits which are in error");
  meta_put_int   (fp,"missing_lines:", meta->general->missing_lines,
		  "Number of missing lines in data take");
  meta_put_string(fp,"}", "","End general");

/* SAR block.  */
  meta_put_string(fp,"sar {","","Begin parameters used specifically in SAR imaging");
  meta_put_char  (fp,"image_type:", meta->sar->image_type,
		  "[S=slant range; G=ground range; P=map projected]");
  meta_put_char  (fp,"look_direction:",meta->sar->look_direction,
		  "SAR Satellite look direction [R=right; L=left]");
  meta_put_int   (fp,"look_count:",meta->sar->look_count,
		  "Number of looks to take from SLC");
  meta_put_int   (fp,"deskewed:",meta->sar->deskewed,
		  "Image moved to zero doppler? [1=yes; 0=no]");
  meta_put_int   (fp,"original_line_count:",meta->sar->original_line_count,
		  "Number of lines in original image");
  meta_put_int   (fp,"original_sample_count:",meta->sar->original_sample_count,
		  "Number of samples in original image");
  meta_put_double(fp,"line_increment:",meta->sar->line_increment,
		  "Line increment for sampling");
  meta_put_double(fp,"sample_increment:",meta->sar->sample_increment,
		  "Sample increment for sampling");
  meta_put_double(fp,"range_time_per_pixel:",meta->sar->range_time_per_pixel,
		  "Time per pixel in range [s]");
  meta_put_double(fp,"azimuth_time_per_pixel:",meta->sar->azimuth_time_per_pixel,
		  "Time per pixel in azimuth [s]");
  meta_put_double(fp,"slant_range_first_pixel:",meta->sar->slant_range_first_pixel,
		  "Slant range to first pixel [m]");
  meta_put_double(fp,"slant_shift:",meta->sar->slant_shift,
		  "Error correction factor, in slant range [m]");
  meta_put_double(fp,"time_shift:",meta->sar->time_shift,
		  "Error correction factor, in time [s]");
  meta_put_double(fp,"wavelength:",meta->sar->wavelength,
		  "SAR carrier wavelength [m]");
  meta_put_double(fp,"prf:",meta->sar->prf,"Pulse Repetition Frequency [Hz]");
  meta_put_double(fp,"earth_radius:",meta->sar->earth_radius,
		  "Earth radius at scene center [m]");
  meta_put_double(fp,"satellite_height:",meta->sar->satellite_height,
		  "Satellite height from earth's center [m]");
  meta_put_string(fp,"satellite_binary_time:",meta->sar->satellite_binary_time,
		  "Satellite Binary Time");
  meta_put_string(fp,"satellite_clock_time:",meta->sar->satellite_clock_time,
		  "Satellite Clock Time (UTC)");
  meta_put_double(fp,"dopRangeCen:",meta->sar->range_doppler_coefficients[0],
		  "Range doppler centroid [Hz]");
  meta_put_double(fp,"dopRangeLin:",meta->sar->range_doppler_coefficients[1],
		  "Range doppler per range pixel [Hz/pixel]");
  meta_put_double(fp,"dopRangeQuad:",meta->sar->range_doppler_coefficients[2],
		  "Range doppler per range pixel sq. [Hz/(pixel^2)]");
  meta_put_double(fp,"dopAzCen:",meta->sar->azimuth_doppler_coefficients[0],
		  "Azimuth doppler centroid [Hz]");
  meta_put_double(fp,"dopAzLin:",meta->sar->azimuth_doppler_coefficients[1],
		  "Azimuth doppler per azimuth pixel [Hz/pixel]");
  meta_put_double(fp,"dopAzQuad:",meta->sar->azimuth_doppler_coefficients[2],
		  "Azimuth doppler per azimuth pixel sq. [Hz/(pixel^2)]");
  meta_put_string(fp,"}","","End sar");

/* State block.  */
  if (meta->state_vectors) {
    meta_put_string(fp,"state {","",
		    "Begin list of state vectors for satellite, over image");
    meta_put_int   (fp,"year:",meta->state_vectors->year,"Year of image start");
    meta_put_int   (fp,"julDay:",meta->state_vectors->julDay,
		    "Julian day of the year for image start");
    meta_put_double(fp,"second:",meta->state_vectors->second,
		    "Second of the day for image start");
    meta_put_int   (fp,"vector_count:",meta->state_vectors->vector_count,
		    "Number of state vectors below");
    {
      int ii;
      for (ii = 0; ii < meta->state_vectors->vector_count; ii++ ) {
	meta_put_string(fp,"vector {","","Begin a single state vector");
	meta_put_double(fp,"time:",meta->state_vectors->vecs[ii].time,
			"Time, relative to image start [s]");
	meta_put_double(fp,"x:",meta->state_vectors->vecs[ii].vec.pos.x,
			"X Coordinate, earth-fixed [m]");
	meta_put_double(fp,"y:",meta->state_vectors->vecs[ii].vec.pos.y,
			"Y Coordinate, earth-fixed [m]");
	meta_put_double(fp,"z:",meta->state_vectors->vecs[ii].vec.pos.z,
			"Z Coordinate, earth-fixed [m]");
	meta_put_double(fp,"vx:",meta->state_vectors->vecs[ii].vec.vel.x,
			"X Velocity, earth-fixed [m/s]");
	meta_put_double(fp,"vy:",meta->state_vectors->vecs[ii].vec.vel.y,
			"Y Velocity, earth-fixed [m/s]");
	meta_put_double(fp,"vz:",meta->state_vectors->vecs[ii].vec.vel.z,
			"Z Velocity, earth-fixed [m/s]");
	meta_put_string(fp,"}","","End a single state vector");
      }
    }
    meta_put_string(fp,"}","","End the list of state vectors");
  }

/* Projection parameters block, if appropriate.  */
  if ( meta->sar->image_type == 'P' && meta->projection ) {
    meta_put_string(fp,"projection {","","Map Projection parameters");
    switch (meta->projection->type) {
      case UNIVERSAL_TRANSVERSE_MERCATOR:
	meta_put_string(fp,"type:","UNIVERSAL_TRANSVERSE_MERCATOR","Projection Type");
	break;
      case POLAR_STEREOGRAPHIC:
	meta_put_string(fp,"type:","POLAR_STEREOGRAPHIC","Projection Type");
        break;
      case ALBERS_EQUAL_AREA:
	meta_put_string(fp,"type:","ALBERS_EQUAL_AREA","Projection Type");
	break;
      case LAMBERT_CONFORMAL_CONIC:
	meta_put_string(fp,"type:","LAMBERT_CONFORMAL_CONIC","Projection Type");
	break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	meta_put_string(fp,"type:","LAMBERT_AZIMUTHAL_EQUAL_AREA","Projection Type");
	break;
      case STATE_PLANE:
	meta_put_string(fp,"type:","STATE_PLANE","Projection Type");
	break;
      case SCANSAR_PROJECTION:
	meta_put_string(fp,"type:","SCANSAR_PROJECTION","Projection Type");
	break;
    }
    meta_put_double(fp,"startX:",meta->projection->startX,
		    "Projection Coordinate at top-left, X direction");
    meta_put_double(fp,"startY:",meta->projection->startY,
		    "Projection Coordinate at top-left, Y direction");
    meta_put_double(fp,"perX:",meta->projection->perX,
		    "Projection Coordinate per pixel, X direction");
    meta_put_double(fp,"perY:",meta->projection->perY,
		    "Projection Coordinate per pixel, Y direction");
    meta_put_string(fp,"units:",meta->projection->units,
		    "Units of projection [meters, seconds]");
    meta_put_char  (fp,"hem:",meta->projection->hem,
		    "Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
    if (META_VERSION >= 1.3) {
      switch (meta->projection->spheroid) {
      case BESSEL_SPHEROID:
	meta_put_string(fp,"spheroid:","BESSEL","Spheroid");
	break;
      case CLARKE1866_SPHEROID:
	meta_put_string(fp,"spheroid:","CLARKE1866","Spheroid");
	break;
      case CLARKE1880_SPHEROID:
	meta_put_string(fp,"spheroid:","CLARKE1880","Spheroid");
	break;
      case GEM6_SPHEROID:
	meta_put_string(fp,"spheroid:","GEM6","Spheroid");
	break;
      case GEM10C_SPHEROID:
	meta_put_string(fp,"spheroid:","GEM10C","Spheroid");
	break;
      case GRS1980_SPHEROID:
	meta_put_string(fp,"spheroid:","GRS1980","Spheroid");
	break;
      case INTERNATIONAL1924_SPHEROID:
	meta_put_string(fp,"spheroid:","INTERNATIONAL1924","Spheroid");
	break;
      case INTERNATIONAL1967_SPHEROID:
	meta_put_string(fp,"spheroid:","INTERNATIONAL1967","Spheroid");
	break;
      case WGS72_SPHEROID:
	meta_put_string(fp,"spheroid:","WGS72","Spheroid");
	break;
      case WGS84_SPHEROID:
	meta_put_string(fp,"spheroid:","WGS84","Spheroid");
	break;
      }
    }
    meta_put_double(fp,"re_major:",meta->projection->re_major,
		    "Major Axis (equator) of earth [m]");
    meta_put_double(fp,"re_minor:",meta->projection->re_minor,
		    "Minor Axis (polar) of earth [m]");
    if (META_VERSION >= 1.3) {
      switch (meta->projection->datum) {
      case EGM96_DATUM:
	meta_put_string(fp,"datum:","EGM96","Geodetic Datum");
	break;
      case ED50_DATUM:
	meta_put_string(fp,"datum:","ED50","Geodetic Datum");
	break;
      case ETRF89_DATUM:
	meta_put_string(fp,"datum:","ETRF89","Geodetic Datum");
	break;
      case ETRS89_DATUM:
	meta_put_string(fp,"datum:","ETRS89","Geodetic Datum");
	break;
      case ITRF_DATUM:
	meta_put_string(fp,"datum:","ITRF","Geodetic Datum");
	break;
      case NAD27_DATUM:
	meta_put_string(fp,"datum:","NAD27","Geodetic Datum");
	break;
      case NAD83_DATUM:
	meta_put_string(fp,"datum:","NAD83","Geodetic Datum");
	break;
      case WGS72_DATUM:
	meta_put_string(fp,"datum:","WGS72","Geodetic Datum");
	break;
      case WGS84_DATUM:
	meta_put_string(fp,"datum:","WGS84","Geodetic Datum");
	break;
      }
    }
    meta_put_string(fp,"param {","","Projection specific parameters");
    switch ( meta->projection->type ) {
    case SCANSAR_PROJECTION: /* Along-track/cross-track projection.  */
      meta_put_string(fp,"atct {","","Begin along-track/cross-track projection");
      meta_put_double(fp,"rlocal:",meta->projection->param.atct.rlocal,
		      "Local earth radius [m]");
      meta_put_double(fp,"alpha1:",meta->projection->param.atct.alpha1,
		      "First rotation angle [degrees]");
      meta_put_double(fp,"alpha2:",meta->projection->param.atct.alpha2,
		      "Second rotation angle [degrees]");
      meta_put_double(fp,"alpha3:",meta->projection->param.atct.alpha3,
		      "Third rotation angle [degrees]");
      meta_put_string(fp,"}","","End atct");
      break;
    case ALBERS_EQUAL_AREA:
      meta_put_string(fp,"albers {","","Begin Albers Conical Equal Area projection");
      meta_put_double(fp,"std_parallel1:",
		      meta->projection->param.albers.std_parallel1,
		      "First standard parallel [degrees]");
      meta_put_double(fp,"std_parallel2:",
		      meta->projection->param.albers.std_parallel2,
		      "Second standard parallel [degrees]");
      meta_put_double(fp,"center_meridian:",
		      meta->projection->param.albers.center_meridian,
		      "Longitude of center meridian [degrees]");
      meta_put_double(fp,"orig_latitude:",
		      meta->projection->param.albers.orig_latitude,
		      "Latitude of the projection origin [degrees]");
      if (META_VERSION >= 1.3) {
	meta_put_double(fp,"false_easting:",
			meta->projection->param.albers.false_easting,
			"False easting [m]");
	meta_put_double(fp,"false_northing:",
			meta->projection->param.albers.false_northing,
			"False northing [m]");
      }
      meta_put_string(fp,"}","","End albers");
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      meta_put_string(fp,"lamaz {","","Begin Lambert Azimuthal Equal Area projection");
      meta_put_double(fp,"center_lon:",meta->projection->param.lamaz.center_lon,
		      "Longitude at center of projection");
      meta_put_double(fp,"center_lat:",meta->projection->param.lamaz.center_lat,
		      "Latitude at center of projection");
      if (META_VERSION >= 1.3) {
	meta_put_double(fp,"false_easting:",
			meta->projection->param.lamaz.false_easting,
			"False easting [m]");
	meta_put_double(fp,"false_northing:",
			meta->projection->param.lamaz.false_northing,
			"False northing [m]");
      }
      meta_put_string(fp,"}","","End lamaz");
      break;
    case LAMBERT_CONFORMAL_CONIC:/*Lambert conformal conic projection.*/
      meta_put_string(fp,"lamcc {","","Begin Lambert Conformal Conic projection");
      meta_put_double(fp,"plat1:",meta->projection->param.lamcc.plat1,
		      "First standard parallel");
      meta_put_double(fp,"plat2:",meta->projection->param.lamcc.plat2,
		      "Second standard parallel");
      meta_put_double(fp,"lat0:",meta->projection->param.lamcc.lat0,
		      "Original latitude");
      meta_put_double(fp,"lon0:",meta->projection->param.lamcc.lon0,
		      "Original longitude");
      if (META_VERSION >= 1.3) {
	meta_put_double(fp,"false_easting:",
			meta->projection->param.lamcc.false_easting,
			"False easting [m]");
	meta_put_double(fp,"false_northing:",
			meta->projection->param.lamcc.false_northing,
			"False northing [m]");
	meta_put_double(fp,"scale_factor:", 
			meta->projection->param.lamcc.scale_factor,
			"Scaling factor");
      }
      meta_put_string(fp,"}","","End lamcc");
      break;
    case POLAR_STEREOGRAPHIC:/*Polar stereographic projection.*/
      meta_put_string(fp,"ps {","","Begin Polar Stereographic Projection");
      meta_put_double(fp,"slat:",meta->projection->param.ps.slat,"Reference Latitude");
      meta_put_double(fp,"slon:",meta->projection->param.ps.slon,"Reference Longitude");
      if (META_VERSION >= 1.3) {
	meta_put_double(fp,"false_easting:",
			meta->projection->param.ps.false_easting, "False easting [m]");
	meta_put_double(fp,"false_northing:",
			meta->projection->param.ps.false_northing, 
			"False northing [m]");
      }
      meta_put_string(fp,"}","","End ps");
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR:/*Universal transverse mercator projection.*/
      meta_put_string(fp,"utm {","","Begin Universal Transverse Mercator Projection");
      meta_put_int   (fp,"zone:",meta->projection->param.utm.zone,"Zone Code");
      if (META_VERSION >= 1.3) {
	meta_put_double(fp,"false_easting:",
			meta->projection->param.utm.false_easting,
			"False easting [m]");
	meta_put_double(fp,"false_northing:",
			meta->projection->param.utm.false_northing,
			"False northing [m]");
	meta_put_double(fp,"latitude:",meta->projection->param.utm.lat0,
			"Latitude [degrees]");
	meta_put_double(fp,"longitude:",meta->projection->param.utm.lon0,
			"Longitude [degrees]");
	meta_put_double(fp,"scale_factor:", meta->projection->param.utm.scale_factor,
			"Scaling factor");
      }
      meta_put_string(fp,"}","","End utm");
      break;
    case STATE_PLANE:/*State plane coordinates projection.*/
      meta_put_string(fp,"state {","","Begin State Plane Coordinates Projection");
      meta_put_int   (fp,"zone:",meta->projection->param.state.zone,"Zone Code");
      meta_put_string(fp,"}","","End state");
      break;
   default:
      printf("WARNING in asf_meta library function '%s': unknown projection type '%c'.\n",
             "meta_write", meta->projection->type);
    }
    meta_put_string(fp,"}","","End param");
    meta_put_string(fp,"}","","End projection");
  }

/* Write out statistics block */
  if (meta->stats) {
    meta_put_string(fp,"stats {","","Block containing basic image statistics");
    meta_put_double(fp,"min:",meta->stats->min,"Minimum sample value");
    meta_put_double(fp,"max:",meta->stats->max,"Maximum sample value");
    meta_put_double(fp,"mean:",meta->stats->mean,"Mean average of sample values");
    meta_put_double(fp,"rmse:",meta->stats->rmse,"Root mean squared error");
    meta_put_double(fp,"std_deviation:",meta->stats->std_deviation,
		    "Standard deviation");
    meta_put_double(fp,"mask:",meta->stats->mask,
		    "Value ignored while taking statistics");
    meta_put_string(fp,"}","","End stats");
  }

  FCLOSE(fp);

  return;
}


/****************************************************************
 * meta_write_old:
 * Given a meta_parameters structure pointer and a file name,
 * write an old style metadata file for that structure.        */
void meta_write_old(meta_parameters *meta, const char *file_name)
{
  /* Maximum file name length, including trailing null.  */
#define FILE_NAME_MAX 1000
  char *file_name_with_extension = appendExt(file_name, ".meta");
  FILE *fp = FOPEN(file_name_with_extension, "w");
  geo_parameters *geo=meta->geo;
  ifm_parameters *ifm=meta->ifm;

  FREE(file_name_with_extension);

  /* Write an 'about meta file' comment  */
  fprintf(fp,
        "# This file contains the metadata for satellite capture file of the same base name.\n"
        "# It was created by meta2ddr, most likely from post version 0.9 data.\n\n");

  /* Meta version 0.9 since we're formatting it that way.  */
  meta_put_double(fp,"meta_version:",0.90,"ASF APD Metadata File.\n");

/*Geolocation parameters.*/
  meta_put_string(fp,"geo {","","begin parameters used in geolocating the image.");
  meta_put_char(fp,"type:",geo->type,
		"Image type: [S=slant range; G=ground range; P=map projected]");
  if (geo->type=='P')
  {/*Projection Parameters.*/
    proj_parameters *proj=meta->geo->proj;
    meta_put_string(fp,"proj {","","Map Projection parameters");
    meta_put_char(fp,"type:",proj->type,
		  "Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
    meta_put_double(fp,"startX:",proj->startX,
		    "Projection Coordinate at top-left, X direction");
    meta_put_double(fp,"startY:",proj->startY,
		    "Projection Coordinate at top-left, Y direction");
    meta_put_double(fp,"perX:",proj->perX,
		    "Projection Coordinate per pixel, X direction");
    meta_put_double(fp,"perY:",proj->perY,
		    "Projection Coordinate per pixel, X direction");
    meta_put_char(fp,"hem:",proj->hem,
		  "Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
    meta_put_double(fp,"re_major:",proj->re_major,
		    "Major (equator) Axis of earth (meters)");
    meta_put_double(fp,"re_minor:",proj->re_minor,
		    "Minor (polar) Axis of earth (meters)");
    switch(proj->type)
    {
      case 'A':/*Along-track/cross-track projection.*/
        meta_put_double(fp,"rlocal:",proj->param.atct.rlocal,"Local earth radius [m]");
        meta_put_double(fp,"atct_alpha1:",proj->param.atct.alpha1,
			"at/ct projection parameter");
        meta_put_double(fp,"atct_alpha2:",proj->param.atct.alpha2,
			"at/ct projection parameter");
        meta_put_double(fp,"atct_alpha3:",proj->param.atct.alpha3,
			"at/ct projection parameter");
        break;
      case 'L':/*Lambert Conformal Conic projection.*/
        meta_put_double(fp,"lam_plat1:",proj->param.lamcc.plat1,
			"Lambert first standard parallel");
        meta_put_double(fp,"lam_plat2:",proj->param.lamcc.plat2,
			"Lambert second standard parallel");
        meta_put_double(fp,"lam_lat:",proj->param.lamcc.lat0,
			"Lambert original latitude");
        meta_put_double(fp,"lam_lon:",proj->param.lamcc.lon0,
			"Lambert original longitude");
        break;
      case 'P':/*Polar Stereographic Projection.*/
        meta_put_double(fp,"ps_lat:",proj->param.ps.slat,
			"Polar Stereographic reference Latitude");
        meta_put_double(fp,"ps_lon:",proj->param.ps.slon,
			"Polar Stereographic reference Longitude");
        break;
      case 'U':/*Universal Trasnverse Mercator Projection.*/
        meta_put_int(fp,"utm_zone:",proj->param.utm.zone,"UTM Zone Code");
        break;
      default:
        printf("ERROR! Unrecognized map projection code '%c' in function '%s'; program exitting.\n",
                proj->type, "meta_write_old");
        exit(EXIT_FAILURE);
    }
    meta_put_string(fp,"}","","end proj");
  }
  meta_put_char(fp,"lookDir:",geo->lookDir,
		"SAR Satellite look direction (normally R) [R=right; L=left]");
  meta_put_int(fp,"deskew:",geo->deskew,"Image moved to zero doppler? [1=yes; 0=no]");
  meta_put_double(fp,"xPix:",geo->xPix,"Pixel size in X direction [m]");
  meta_put_double(fp,"yPix:",geo->yPix,"Pixel size in Y direction [m]");
  meta_put_double(fp,"rngPixTime:",geo->rngPixTime,
		  "Time/pixel, range (xPix/(c/2.0), or 1/fs) [s]");
  meta_put_double(fp,"azPixTime:",geo->azPixTime,
		  "Time/pixel, azimuth (yPix/swathVel, or 1/prf) [s]");
  meta_put_double(fp,"slantShift:",geo->slantShift,
		  "Error correction factor, in slant range [m]");
  meta_put_double(fp,"timeShift:",geo->timeShift,
		  "Error correction factor, in time [s]");
  meta_put_double(fp,"slantFirst:",geo->slantFirst,
		  "Slant range to first image pixel [m]");
  meta_put_double(fp,"wavelength:",geo->wavelen,"SAR Carrier Wavelength [m]");
  meta_put_double(fp,"dopRangeCen:",geo->dopRange[0],"Doppler centroid [Hz]");
  meta_put_double(fp,"dopRangeLin:",geo->dopRange[1],
		  "Doppler per range pixel [Hz/pixel]");
  meta_put_double(fp,"dopRangeQuad:",geo->dopRange[2],
		  "Doppler per range pixel sq. [Hz/(pixel^2)]");
  meta_put_double(fp,"dopAzCen:",geo->dopAz[0],"Doppler centroid [Hz]");
  meta_put_double(fp,"dopAzLin:",geo->dopAz[1],"Doppler per azimuth pixel [Hz/pixel]");
  meta_put_double(fp,"dopAzQuad:",geo->dopAz[2],
		  "Doppler per azimuth pixel sq. [Hz/(pixel^2)]");
  meta_put_string(fp,"}","","end geo");

/*Interferometry parameters:*/
  meta_put_string(fp,"ifm {","","begin interferometry-related parameters");
  meta_put_double(fp,"er:",ifm->er,"Local earth radius [m]");
  meta_put_double(fp,"ht:",ifm->ht,"Satellite height, from center of earth [m]");
  meta_put_int(fp,"nLooks:",ifm->nLooks,"Number of looks to take from SLC");
  meta_put_int(fp,"orig_lines:",ifm->orig_nLines,"Number of lines in original image");
  meta_put_int(fp,"orig_samples:",ifm->orig_nSamples,
	       "Number of samples in original image");
  meta_put_string(fp,"}","","end ifm");

/*State Vectors:*/
  if (meta->stVec!=NULL) {
    meta_put_string(fp,"state {","",
		    "begin list of state vectors for satellite, over image");
    meta_put_int   (fp,"year:",meta->state_vectors->year,"Year of image start");
    meta_put_int   (fp,"day:",meta->state_vectors->julDay,
		    "Julian day of the year for image start");
    meta_put_double(fp,"second:",meta->state_vectors->second,
		    "Second of the day for image start");
    meta_put_int   (fp,"number:",meta->state_vectors->vector_count,
		    "Number of state vectors below");
    {
      int ii;
      for (ii = 0; ii < meta->state_vectors->vector_count; ii++ ) {
        meta_put_string(fp,"vector {","","begin a single state vector");
        meta_put_double(fp,"time:",meta->state_vectors->vecs[ii].time,
			"Time, relative to image start [s]");
        meta_put_double(fp,"x:",meta->state_vectors->vecs[ii].vec.pos.x,
			"X Coordinate, earth-fixed [m]");
        meta_put_double(fp,"y:",meta->state_vectors->vecs[ii].vec.pos.y,
			"Y Coordinate, earth-fixed [m]");
        meta_put_double(fp,"z:",meta->state_vectors->vecs[ii].vec.pos.z,
			"Z Coordinate, earth-fixed [m]");
        meta_put_double(fp,"vx:",meta->state_vectors->vecs[ii].vec.vel.x,
			"X Velocity, earth-fixed [m/s]");
        meta_put_double(fp,"vy:",meta->state_vectors->vecs[ii].vec.vel.y,
			"Y Velocity, earth-fixed [m/s]");
        meta_put_double(fp,"vz:",meta->state_vectors->vecs[ii].vec.vel.z,
			"Z Velocity, earth-fixed [m/s]");
        meta_put_string(fp,"}","","end vector");
      }
    }
    meta_put_string(fp,"}","","end of list of state vectors");
  }

/*Extra Info:*/
  meta_put_string(fp,"extra {","","begin extra sensor information");
  meta_put_string(fp,"sensor:",meta->info->sensor,"Imaging sensor");
  meta_put_string(fp,"mode:",meta->info->mode,"Imaging mode");
  meta_put_string(fp,"processor:",meta->info->processor,
		  "Name & Version of SAR Processor");
  meta_put_int(fp,"orbit:",meta->info->orbit,"Orbit Number for this datatake");
  meta_put_double(fp,"bitErrorRate:",meta->info->bitErrorRate,"Bit Error Rate");
  meta_put_string(fp,"satBinTime:",meta->info->satBinTime,"Satellite Binary Time");
  meta_put_string(fp,"satClkTime:",meta->info->satClkTime,
		  "Satellite Clock Time (UTC)");
  meta_put_double(fp,"prf:",meta->info->prf,"Pulse Repition Frequency");
  meta_put_string(fp,"}","","end extra");

  FCLOSE(fp);

  return;
}


int is_empty(char *string)
{
	int ii;
	for (ii=0; ii<strlen(string); ii++) {
		if (!isspace(string[ii])) return 0;
	}
	return 1;
}
void meta_put_string(FILE *meta_file,char *name,char *value,char *comment)
{
	int ii;
	int malloc_flag=0;
	char line[255];/*The line to be written to the file.*/
	static int depth=0;
	strcpy(line,"");

/*Deal with indentation.*/
	if (strchr(name,'}'))/*If the string has a closing brace, indent less.*/
		depth--;
	if (depth<0)
		{printf("ERROR!  Too many '}' in meta file!\n"); exit(1);}

	for (ii=0; ii<depth; ii++)
		strcat(line,"    ");/*Indent the appropriate number of spaces.*/

/*Append parameter and value.*/
	strcat(line,name);/*Append parameter name*/
	strcat(line," ");
	if (is_empty(value) && !strchr(name,'{') && !strchr(name,'}')){
		value = (char*)MALLOC(sizeof(char)*4);
		malloc_flag=1;
		strcpy(value,MAGIC_UNSET_STRING);
	}
	strcat(line,value);/*Append parameter value.*/
	if (malloc_flag==1) {free(value);}

/* Append comment if applicable */
	if (comment!=NULL)
	{
	/*Space over to the comment section.*/
		ii=strlen(line);
		while (ii < 42+depth*4) /*Fill spaces out to about column 50.*/
			line[ii++]=' ';
		line[ii++]='\0';        /*Append trailing NULL.*/

	/*Add the comment.*/
		strcat(line," # ");     /*Signal beginning of comment.*/
		strcat(line,comment);   /*Append comment.*/
	}

/*If the string has a closing brace, append newline*/
	if (strchr(name,'}') && (depth==0))
		strcat(line,"\n");

/*More indentation.*/
	if (strchr(name,'{'))/*If the string has an opening brace, indent more.*/
		depth++;

/*Finally, write the line to the file.*/
	fprintf(meta_file,"%s\n",line);
}

void meta_put_double(FILE *meta_file,char *name,double value,char *comment)
{
	char param[64];
	sprintf(param,"%-16.11g",value);
	strtok(param," ");/*remove all trailing spaces */
	if (is_empty(param)) { strcpy(param,"NaN"); }
	meta_put_string(meta_file,name,param,comment);
}


void meta_put_int(FILE *meta_file,char *name,int value,char *comment)
{
	char param[64];
	sprintf(param,"%i",value);
	if (is_empty(param)) { strcpy(param,"-999999999"); }
	meta_put_string(meta_file,name,param,comment);
}

void meta_put_char(FILE *meta_file,char *name,char value,char *comment)
{
	char param[2];
	sprintf(param,"%c",value);
	if (is_empty(param)) { strcpy(param,"?"); }
	meta_put_string(meta_file,name,param,comment);
}

