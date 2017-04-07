#include <ctype.h>
#include <stdio.h>
#include <error.h>

#include "asf_meta.h"
#include "asf_nan.h"
#include "caplib.h"
#include "err_die.h"
#include "envi.h"
#include "dateUtil.h"

// Global flag for writing ENVI header files for all viewable images
int dump_envi_header = 0;

void meta_put_string(FILE *meta_file,char *name,char  *value,char *comment);
void meta_put_double(FILE *meta_file,char *name,double value,char *comment);
void meta_put_int   (FILE *meta_file,char *name,int    value,char *comment);
void meta_put_char  (FILE *meta_file,char *name,char   value,char *comment);
void meta_put_double_lf(FILE *meta_file,char *name,double value,int decimals,
                        char *comment);

// To avoid having to change link flags around or link against glib we
// have this compatability function.
static char *
static_strdup (const char *s)
{
  char *result = malloc (sizeof (char) * (strlen (s) + 1));

  int idx = 0;
  while ( s[idx] != '\0') {
    result[idx] = s[idx];
    idx++;
  }
  result[idx] = '\0';

  return result;
}

// To avoid having to change link flags around or link against glib we
// have this function.  Its like strcasecmp but only returns true or
// false (true if strings are not equal disregarding case, false
// otherwise).
static int
static_strcaseneq (const char *s1, const char *s2)
{
  return !!strcmp_case(s1,s2);
}

static void readline(FILE *f, char *buffer, size_t n)
{
  char *p;
  p = fgets(buffer, n, f);
  
  if (!p)
    strcpy(buffer, "");
  else {
    p = buffer + strlen(buffer) - 1;
    while(isspace(*p)) *p-- = '\0';
  }
}

//static int parse_val(char *inbuf, char *key, char **val)
static void parse_val(char *inbuf, char *key, char **val)
{
  char * p, * eq, * buf;
  //int match = FALSE;
  
  buf = static_strdup(inbuf);
  
  p = eq = strchr(buf, '=');
  if (!eq) {
    free(buf);
    //return FALSE;
  }
 
  *eq = '\0';
  --p;
  
  while (isspace((int)(*p)))
    *p-- = '\0';
  
  if (static_strcaseneq(buf, key) == 0) {
    p = eq + 1;
    while (isspace((int)(*p)))
			++p;
		if (*p) {
			*val = p;
			//match = TRUE;
		}
	}
      
  free(buf);
  //return match;
}

char *data_type2str(data_type_t data_type)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (data_type == ASF_BYTE)
    strcpy(str, "BYTE");
  else if (data_type == INTEGER16)
    strcpy(str, "INTEGER16");
  else if (data_type == INTEGER32)
    strcpy(str, "INTEGER32");
  else if (data_type == REAL32)
    strcpy(str, "REAL32");
  else if (data_type == REAL64)
    strcpy(str, "REAL64");
  else if (data_type == COMPLEX_BYTE)
    strcpy(str, "COMPLEX_BYTE");
  else if (data_type == COMPLEX_INTEGER16)
    strcpy(str, "COMPLEX_INTEGER16");
  else if (data_type == COMPLEX_INTEGER32)
    strcpy(str, "COMPLEX_INTEGER32");
  else if (data_type == COMPLEX_REAL32)
    strcpy(str, "COMPLEX_REAL32");
  else if (data_type == COMPLEX_REAL64)
    strcpy(str, "COMPLEX_REAL64");
  else
    strcpy(str, MAGIC_UNSET_STRING);
  
  return str;
}

char *image_data_type2str(image_data_type_t image_data_type)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (image_data_type == RAW_IMAGE)
    strcpy(str, "RAW_IMAGE");
  else if (image_data_type == COMPLEX_IMAGE)
    strcpy(str, "COMPLEX_IMAGE");
  else if (image_data_type == AMPLITUDE_IMAGE)
    strcpy(str, "AMPLITUDE_IMAGE");
  else if (image_data_type == PHASE_IMAGE)
    strcpy(str, "PHASE_IMAGE");
  else if (image_data_type == POWER_IMAGE)
    strcpy(str, "POWER_IMAGE");
  else if (image_data_type == SIGMA_IMAGE)
    strcpy(str, "SIGMA_IMAGE");
  else if (image_data_type == GAMMA_IMAGE)
    strcpy(str, "GAMMA_IMAGE");
  else if (image_data_type == BETA_IMAGE)
    strcpy(str, "BETA_IMAGE");
  else if (image_data_type == INTERFEROGRAM)
    strcpy(str, "INTERFEROGRAM");
  else if (image_data_type == UNWRAPPED_PHASE)
    strcpy(str, "UNWRAPPED_PHASE");
  else if (image_data_type == COHERENCE_IMAGE)
    strcpy(str, "COHERENCE_IMAGE");
  else if (image_data_type == GEOREFERENCED_IMAGE)
    strcpy(str, "GEOREFERENCED_IMAGE");
  else if (image_data_type == GEOCODED_IMAGE)
    strcpy(str, "GEOCODED_IMAGE");
  else if (image_data_type == POLARIMETRIC_IMAGE)
    strcpy(str, "POLARIMETRIC_IMAGE");
  else if (image_data_type == POLARIMETRIC_SEGMENTATION)
    strcpy(str, "POLARIMETRIC_SEGMENTATION");
  else if (image_data_type == POLARIMETRIC_DECOMPOSITION)
    strcpy(str, "POLARIMETRIC_DECOMPOSITION");
  else if (image_data_type == POLARIMETRIC_PARAMETER)
    strcpy(str, "POLARIMETRIC_PARAMETER");
  else if (image_data_type == POLARIMETRIC_S2_MATRIX)
    strcpy(str, "POLARIMETRIC_S2_MATRIX");
  else if (image_data_type == POLARIMETRIC_C2_MATRIX)
    strcpy(str, "POLARIMETRIC_C2_MATRIX");
  else if (image_data_type == POLARIMETRIC_C3_MATRIX)
    strcpy(str, "POLARIMETRIC_C3_MATRIX");
  else if (image_data_type == POLARIMETRIC_C4_MATRIX)
    strcpy(str, "POLARIMETRIC_C4_MATRIX");
  else if (image_data_type == POLARIMETRIC_T3_MATRIX)
    strcpy(str, "POLARIMETRIC_T3_MATRIX");
  else if (image_data_type == POLARIMETRIC_T4_MATRIX)
    strcpy(str, "POLARIMETRIC_T4_MATRIX");
  else if (image_data_type == POLARIMETRIC_STOKES_MATRIX)
    strcpy(str, "POLARIMETRIC_STOKES_MATRIX");
  else if (image_data_type == LUT_IMAGE)
    strcpy(str, "LUT_IMAGE");
  else if (image_data_type == ELEVATION)
    strcpy(str, "ELEVATION");
  else if (image_data_type == DEM)
    strcpy(str, "DEM");
  else if (image_data_type == IMAGE)
    strcpy(str, "IMAGE");
  else if (image_data_type == BROWSE_IMAGE)
    strcpy(str, "BROWSE_IMAGE");
  else if (image_data_type == MASK)
    strcpy(str, "MASK");
  else if (image_data_type == SIMULATED_IMAGE)
    strcpy(str, "SIMULATED_IMAGE");
  else if (image_data_type == IMAGE_LAYER_STACK)
    strcpy(str, "IMAGE_LAYER_STACK");
  else if (image_data_type == INSAR_STACK)
    strcpy(str, "INSAR_STACK");
  else if (image_data_type == RGB_STACK)
    strcpy(str, "RGB_STACK");
  else if (image_data_type == MOSAIC)
    strcpy(str, "MOSAIC");
  else if (image_data_type == ICE_AGE)
    strcpy(str, "ICE_AGE");
  else if (image_data_type == ICE_THICKNESS)
    strcpy(str, "ICE_THICKNESS");
  else if (image_data_type == BACKSCATTER_HISTOGRAM)
    strcpy(str, "BACKSCATTER_HISTOGRAM");
  else if (image_data_type == MULTIYEAR_ICE_FRACTION)
    strcpy(str, "MULTIYEAR_ICE_FRACTION");
  else if (image_data_type == DIVERGENCE)
    strcpy(str, "DIVERGENCE");
  else if (image_data_type == VORTICITY)
    strcpy(str, "VORTICITY");
  else if (image_data_type == SHEAR)
    strcpy(str, "SHEAR");
  else if (image_data_type == MODEL_OUTPUT)
    strcpy(str, "MODEL_OUTPUT");
  else
    strcpy(str, MAGIC_UNSET_STRING);

  return str;
}

char *radiometry2str(radiometry_t radiometry)
{
  char *str = (char *) MALLOC(sizeof(char)*256);

  if (radiometry == r_AMP)
    strcpy(str, "AMPLITUDE");
  else if (radiometry == r_SIGMA)
    strcpy(str, "SIGMA");
  else if (radiometry == r_GAMMA)
    strcpy(str, "GAMMA");
  else if (radiometry == r_BETA)
    strcpy(str, "BETA");
  else if (radiometry == r_SIGMA_DB)
    strcpy(str, "SIGMA_DB");
  else if (radiometry == r_GAMMA_DB)
    strcpy(str, "GAMMA_DB");
  else if (radiometry == r_BETA_DB)
    strcpy(str, "BETA_DB");
  else if (radiometry == r_POWER)
    strcpy(str, "POWER");
  else
    strcpy(str, MAGIC_UNSET_STRING);
  
  return str;
}

char *proj2str(projection_type_t type)
{
  char *str = (char *) MALLOC(sizeof(char)*256);
  if (type == UNIVERSAL_TRANSVERSE_MERCATOR)
    strcpy(str, "UNIVERSAL_TRANSVERSE_MERCATOR");
  else if (type == POLAR_STEREOGRAPHIC)
    strcpy(str, "POLAR_STEREOGRAPHIC");
  else if (type == ALBERS_EQUAL_AREA)
    strcpy(str, "ALBERS_EQUAL_AREA");
  else if (type == LAMBERT_CONFORMAL_CONIC)
    strcpy(str, "LAMBERT_CONFORMAL_CONIC");
  else if (type == LAMBERT_AZIMUTHAL_EQUAL_AREA)
    strcpy(str, "LAMBERT_AZIMUTHAL_EQUAL_AREA");
  else if (type == STATE_PLANE)
    strcpy(str, "STATE_PLANE");
  else if (type == SCANSAR_PROJECTION)
    strcpy(str, "SCANSAR_PROJECTION");
  else if (type == LAT_LONG_PSEUDO_PROJECTION)
    strcpy(str, "LAT_LONG_PSEUDO_PROJECTION");
  else if (type == MERCATOR)
    strcpy(str, "MERCATOR");
  else if (type == EQUI_RECTANGULAR)
    strcpy(str, "EQUI_RECTANGULAR");
  else if (type == EQUIDISTANT)
    strcpy(str, "EQUIDISTANT");
  else if (type == SINUSOIDAL)
    strcpy(str, "SINUSOIDAL");
  else if (type == EASE_GRID_NORTH)
    strcpy(str, "EASE_GRID_NORTH");
  else if (type == EASE_GRID_SOUTH)
    strcpy(str, "EASE_GRID_SOUTH");
  else if (type == EASE_GRID_GLOBAL)
    strcpy(str, "EASE_GRID_GLOBAL");
  else if (type == UNKNOWN_PROJECTION)
    strcpy(str, "UNKNOWN_PROJECTION");

  return str;
}

/* Given a meta_parameters structure pointer and a file name, write a
   metadata file for that structure.  */
void meta_write(meta_parameters *meta, const char *file_name)
{
  /* Maximum file name length, including trailing null.  */
#define FILE_NAME_MAX 1000
  char *file_name_with_extension = appendExt(file_name, ".meta");
  FILE *fp = FOPEN(file_name_with_extension, "w");
  char comment[256];

  // dump the envi header if we were told to do so, and envi supports
  // the type of data that we have
  if (dump_envi_header) {
      if (datatype2envi(meta->general->data_type) != -1) {
          char *hdr_file_name_with_extension = appendExt(file_name, ".hdr");
          envi_header *envi = meta2envi(meta);
          write_envi_header(hdr_file_name_with_extension, file_name, 
			    meta, envi);
          FREE(envi);
          FREE(hdr_file_name_with_extension);
      } else {
          // no need to get all "***WARNING!!!***" about this
          printf("Not dumping ENVI header for this data type.\n");
      }
  }

  FREE(file_name_with_extension);

  /* Write an 'about meta file' comment  */
  fprintf(fp,
  "# This file contains the metadata for satellite capture file of the same base name.\n"
  "#      '%c' is likely an unknown single character value.\n"
  "#      '%s' is likely an unknown string of characters.\n"
  "#      '%d' is likely an unknown integer value.\n"
  "#      'nan' is likely an unknown Real value.\n\n",
  MAGIC_UNSET_CHAR, MAGIC_UNSET_STRING, MAGIC_UNSET_INT);

  /* We always write out files corresponding to the latest meta version.  */
  fprintf(fp, "meta_version: %.2f\n\n", META_VERSION);

/* General block.  */
  meta_put_string(fp,"general {", "","Begin parameters generally used in remote sensing");
  meta_put_string(fp,"name:", meta->general->basename, "File name");
  meta_put_string(fp,"sensor:", meta->general->sensor, "Imaging satellite");
  meta_put_string(fp,"sensor_name:", meta->general->sensor_name, "Imaging sensor");
  meta_put_string(fp,"mode:",meta->general->mode,"Imaging mode");
  meta_put_string(fp,"receiving_station:",meta->general->receiving_station,"Downlinking ground station");
  meta_put_string(fp,"processor:", meta->general->processor,"Name and Version of Processor");
  strcpy(comment,"Type of samples (e.g. REAL64)");
  char *data_type = data_type2str(meta->general->data_type);
  meta_put_string(fp, "data_type:", data_type, comment);
  FREE(data_type);
  if (META_VERSION >= 1.2) {
    strcpy(comment,"Image data type (e.g. AMPLITUDE_IMAGE)");
    data_type = image_data_type2str(meta->general->image_data_type);
    meta_put_string(fp, "image_data_type:", data_type, comment);
    FREE(data_type);
  }
  if (META_VERSION >= 2.5) {
    strcpy(comment,"Radiometry (e.g. SIGMA)");
    char *rad_str = radiometry2str(meta->general->radiometry);
    meta_put_string(fp, "radiometry:", rad_str, comment);
    FREE(rad_str);
  }
  meta_put_string(fp,"acquisition_date:", meta->general->acquisition_date,
      "Acquisition date of the data");
  meta_put_int   (fp,"orbit:", meta->general->orbit,
      "Orbit Number for this datatake");
  meta_put_char  (fp,"orbit_direction:", meta->general->orbit_direction,
      "Ascending 'A', or descending 'D'");
  meta_put_int   (fp,"frame:", meta->general->frame,
      "Frame for this image [-1 if n/a]");
  meta_put_int   (fp,"band_count:", meta->general->band_count,
      "Number of bands in image");
  meta_put_string(fp,"bands:", meta->general->bands,
      "Band of the sensor");
  meta_put_int   (fp,"line_count:", meta->general->line_count,
      "Number of lines in image");
  meta_put_int   (fp,"sample_count:", meta->general->sample_count,
      "Number of samples in image");
  meta_put_int   (fp,"start_line:", meta->general->start_line,
      "First line relative to original image");
  meta_put_int   (fp,"start_sample:", meta->general->start_sample,
      "First sample relative to original image");
  // These are only used by ALOS & Airsar and for other image types it'll be
  // misleading -- so don't write it out
  if ((meta->sar && meta->transform) || meta->airsar || meta->uavsar) {
  //if (strcmp_case(meta->general->sensor, "ALOS") == 0 || meta->airsar) {
    meta_put_double(fp,"line_scaling:", meta->general->line_scaling,
                    "Scale factor relative to original image, y");
    meta_put_double(fp,"sample_scaling:", meta->general->sample_scaling,
                    "Scale factor relative to original image, x");
  }
  if (meta->projection && 
      meta->projection->type == LAT_LONG_PSEUDO_PROJECTION) {
    meta_put_double(fp,"x_pixel_size:", meta->general->x_pixel_size,
		    "Range pixel size [degrees]");
    meta_put_double(fp,"y_pixel_size:", meta->general->y_pixel_size,
		    "Azimuth pixel size [degrees]");
  }
  else {
    meta_put_double(fp,"x_pixel_size:", meta->general->x_pixel_size,
		    "Range pixel size [m]");
    meta_put_double(fp,"y_pixel_size:", meta->general->y_pixel_size,
		    "Azimuth pixel size [m]");
  }
  meta_put_double_lf(fp,"center_latitude:",
             meta->general->center_latitude, 4,
             "Approximate image center latitude");
  meta_put_double_lf(fp,"center_longitude:",
             meta->general->center_longitude, 4,
             "Approximate image center longitude");
  meta_put_double_lf(fp,"re_major:", meta->general->re_major, 3,
             "Major (equator) Axis of earth [m]");
  meta_put_double_lf(fp,"re_minor:", meta->general->re_minor, 3,
             "Minor (polar) Axis of earth [m]");
  meta_put_double(fp,"bit_error_rate:", meta->general->bit_error_rate,
      "Fraction of bits which are in error");
  meta_put_int   (fp,"missing_lines:", meta->general->missing_lines,
      "Number of missing lines in data take");
  meta_put_double_lf(fp,"no_data:", meta->general->no_data, 4,
      "Value indicating no data for a pixel");
  meta_put_string(fp,"}", "","End general");

  /* SAR block.  */
  if (meta->sar) {
    meta_put_string(fp,"sar {","","Begin parameters used specifically in SAR imaging");
    meta_put_string(fp,"polarization:",meta->sar->polarization,
        "Signal polarization");
    meta_put_char  (fp,"image_type:", meta->sar->image_type,
        "[S=slant range; G=ground range; P=map projected; R=georeferenced]");
    meta_put_char  (fp,"look_direction:",meta->sar->look_direction,
        "SAR Satellite look direction [R=right; L=left]");    
    meta_put_int   (fp,"azimuth_look_count:",meta->sar->azimuth_look_count,
        "Number of looks in azimuth direction");
    meta_put_int   (fp,"range_look_count:",meta->sar->range_look_count,
        "Number of looks in range direction");
    meta_put_int   (fp,"multilook:",meta->sar->multilook,
        "Image multilooked? [1=yes; 0=no]");
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
    meta_put_double(fp,"earth_radius_pp:",meta->sar->earth_radius_pp,
        "Earth radius used by the PP during L0 processsing. [m]");
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
    if (META_VERSION >= 2.7) {
      meta_put_double(fp,"pitch:",meta->sar->pitch,"Platform pitch [degrees]");
      meta_put_double(fp,"roll:",meta->sar->roll,"Platform roll [degrees]");
      meta_put_double(fp,"yaw:",meta->sar->yaw,"Platform yaw [degrees]");
    }
    if (META_VERSION >= 1.4) {
      meta_put_double(fp,"azimuth_bandwidth:",meta->sar->azimuth_processing_bandwidth,
          "Azimuth processing bandwidth [Hz]");
      meta_put_double(fp,"chirp_rate:",meta->sar->chirp_rate,
          "Chirp rate [Hz/sec]");
      meta_put_double(fp,"pulse_duration:",meta->sar->pulse_duration,
          "Pulse duration [s]");
      meta_put_double(fp,"range_samp_rate:",meta->sar->range_sampling_rate,
          "Range sampling rate [Hz]");
    }
    if (META_VERSION >=2.8) {
      int ii;
      char coeff[15];
      for (ii=0; ii<6; ++ii) {
    sprintf(coeff, "incid_a(%d):", ii);
    meta_put_double(fp, coeff, meta->sar->incid_a[ii],
            "Incidence angle transformation parameter");
      }
    }
    meta_put_string(fp,"}","","End sar");
  }

  // Doppler parameter block - version 3.1
  if (meta->doppler) {
    int ii, kk;
    char str[32];
    meta_put_string(fp,"doppler {","","Begin list of Doppler parameters");
    strcpy(comment,"Doppler type (TSX)");
    switch (meta->doppler->type)
      {
      case unknown_doppler:
	break;
      case tsx_doppler:
	meta_put_string(fp,"type:","TSX",comment);
	meta_put_int(fp,"year:",meta->doppler->tsx->year,
		     "Year for first Doppler estimate");
	meta_put_int(fp,"julDay:",meta->doppler->tsx->julDay,
		     "Julian day of year for first Doppler estimate)");
	meta_put_double(fp,"second:",meta->doppler->tsx->second,
			"Second of day for first Doppler estimate");
	meta_put_int(fp,"doppler_count:",meta->doppler->tsx->doppler_count,
		     "Number of Doppler estimates");
	for (ii=0; ii<meta->doppler->tsx->doppler_count; ii++) {
	  meta_put_string(fp,"estimate {","","Begin of single Doppler estimate");
	  meta_put_double(fp,"time:",meta->doppler->tsx->dop[ii].time,
			  "Time relative to first Doppler estimate time");
	  meta_put_double(fp,"first_range_time:",
			  meta->doppler->tsx->dop[ii].first_range_time,
			  "Time first range pixel");
	  meta_put_double(fp,"reference_time:",
			  meta->doppler->tsx->dop[ii].reference_time,
			  "Reference point time of polynomial");
	  meta_put_int(fp,"polynomial_degree:",
		       meta->doppler->tsx->dop[ii].poly_degree,
		       "Degree of polynomial");
	  for (kk=0; kk<=meta->doppler->tsx->dop[ii].poly_degree; kk++) {
	    sprintf(str, "coefficient[%d]:", kk);
	    meta_put_double(fp,str,meta->doppler->tsx->dop[ii].coefficient[kk],
			    "Polynomial coefficient");
	  }
	  meta_put_string(fp,"}","","End of single Doppler estimate");
	}
	break;
      case radarsat2_doppler:
	meta_put_string(fp,"type:","RADARSAT2",comment);
	meta_put_int(fp,"doppler_count:",meta->doppler->r2->doppler_count,
		     "Number of Doppler estimates");
	meta_put_double(fp,"centroid_time:",
			meta->doppler->r2->ref_time_centroid,
			"Reference time for centroid (t0)");
	for (kk=0; kk<meta->doppler->r2->doppler_count; kk++) {
	  sprintf(str, "centroid[%d]:", kk);
	  meta_put_double(fp,str,meta->doppler->r2->centroid[kk],
			  "Polynomial coefficients for Doppler centroid");
	}
	meta_put_double(fp, "rate_time:",meta->doppler->r2->ref_time_rate,
			"Reference time for rate (t0)");
	for (kk=0; kk<meta->doppler->r2->doppler_count; kk++) {
	  sprintf(str, "rate[%d]:", kk);
	  meta_put_double(fp,str,meta->doppler->r2->rate[kk],
			  "Polynomial coefficients for Doppler rate");
	}
	meta_put_double(fp, "time_first_sample:",
			meta->doppler->r2->time_first_sample,
			"Slant range time for first range sample");
      }
    meta_put_string(fp,"}","","End doppler");
  }

  if (meta->optical) {
    meta_put_string(fp,"optical {","","Begin parameters used specifically in "
        "optical imaging");
    meta_put_string(fp,"pointing_direction:",meta->optical->pointing_direction,
        "Pointing direction of the sensor");
    meta_put_double_lf(fp,"off_nadir_angle:",meta->optical->off_nadir_angle, 4,
        "Off-nadir angle of the sensor [degrees]");
    meta_put_string(fp,"correction_level:",meta->optical->correction_level,
        "N - uncorr, R - georef, G - geocoded, D - DEM corr");
    meta_put_double(fp,"cloud_percentage:",meta->optical->cloud_percentage,
        "Cloud cover percentage [%]");
    meta_put_double_lf(fp,"sun_azimuth_angle:",
               meta->optical->sun_azimuth_angle, 4,
               "Sun azimuth angle [degrees]");
    meta_put_double_lf(fp,"sun_elevation_angle:",
               meta->optical->sun_elevation_angle, 4,
               "Sun elevation angle [degrees]");
    meta_put_string(fp,"}","","End optical");
  }

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
  meta_put_double_lf(fp,"x:",meta->state_vectors->vecs[ii].vec.pos.x, 3,
      "X Coordinate, earth-fixed [m]");
  meta_put_double_lf(fp,"y:",meta->state_vectors->vecs[ii].vec.pos.y, 3,
      "Y Coordinate, earth-fixed [m]");
  meta_put_double_lf(fp,"z:",meta->state_vectors->vecs[ii].vec.pos.z, 3,
      "Z Coordinate, earth-fixed [m]");
  meta_put_double_lf(fp,"vx:",meta->state_vectors->vecs[ii].vec.vel.x, 3,
      "X Velocity, earth-fixed [m/s]");
  meta_put_double_lf(fp,"vy:",meta->state_vectors->vecs[ii].vec.vel.y, 3,
      "Y Velocity, earth-fixed [m/s]");
  meta_put_double_lf(fp,"vz:",meta->state_vectors->vecs[ii].vec.vel.z, 3,
      "Z Velocity, earth-fixed [m/s]");
  meta_put_string(fp,"}","","End a single state vector");
      }
    }
    meta_put_string(fp,"}","","End the list of state vectors");
  }

/* Projection parameters block, if appropriate.  */
//  if ( (meta->sar->image_type == 'P' || meta->general->image_data_type == DEM)
//       && meta->projection ) {
  if (meta->projection) {
    meta_put_string(fp,"projection {","","Map Projection parameters");
    strcpy(comment,"Projection Type");
    char *proj_str = proj2str(meta->projection->type);
    meta_put_string(fp, "type:", proj_str, comment);
    free(proj_str);
    meta_put_double_lf(fp,"startX:",meta->projection->startX, 6,
                       	  "Projection Coordinate at top-left, X direction");
    meta_put_double_lf(fp,"startY:",meta->projection->startY, 6,
                          "Projection Coordinate at top-left, Y direction");
    meta_put_double(fp,"perX:",meta->projection->perX,
		    "Projection Coordinate per pixel, X direction");
    meta_put_double(fp,"perY:",meta->projection->perY,
		    "Projection Coordinate per pixel, Y direction");
    meta_put_string(fp,"units:",meta->projection->units,
        "Units of projection [meters, seconds, degrees]");
    meta_put_char  (fp,"hem:",meta->projection->hem,
        "Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
    if (META_VERSION >= 1.3) {
        meta_put_string(fp,"spheroid:",
                        (char*)spheroid_toString(meta->projection->spheroid),"Spheroid");
    }
    meta_put_double_lf(fp,"re_major:",meta->projection->re_major, 3,
        "Major Axis (equator) of earth [m]");
    meta_put_double_lf(fp,"re_minor:",meta->projection->re_minor, 3,
        "Minor Axis (polar) of earth [m]");
    if (META_VERSION >= 1.3) {
      meta_put_string(fp,"datum:",(char*)datum_toString(meta->projection->datum),
                      "Geodetic Datum");
    }
    if (META_VERSION >= 1.6)
      meta_put_double_lf(fp, "height:", meta->projection->height, 3,
             "Height [m]");
    meta_put_string(fp,"param {","","Projection specific parameters");
    switch ( meta->projection->type ) {
    case SCANSAR_PROJECTION: /* Along-track/cross-track projection.  */
      meta_put_string(fp,"atct {","","Begin along-track/cross-track projection");
      meta_put_double_lf(fp,"rlocal:",meta->projection->param.atct.rlocal, 3,
             "Local earth radius [m]");
      meta_put_double_lf(fp,"alpha1:",meta->projection->param.atct.alpha1, 4,
             "First rotation angle [degrees]");
      meta_put_double_lf(fp,"alpha2:",meta->projection->param.atct.alpha2, 4,
             "Second rotation angle [degrees]");
      meta_put_double_lf(fp,"alpha3:",meta->projection->param.atct.alpha3, 4,
             "Third rotation angle [degrees]");
      meta_put_string(fp,"}","","End atct");
      break;
    case ALBERS_EQUAL_AREA:
      meta_put_string(fp,"albers {","","Begin Albers Conical Equal Area projection");
      meta_put_double_lf(fp,"std_parallel1:",
             meta->projection->param.albers.std_parallel1, 4,
             "First standard parallel [degrees]");
      meta_put_double_lf(fp,"std_parallel2:",
             meta->projection->param.albers.std_parallel2, 4,
             "Second standard parallel [degrees]");
      meta_put_double_lf(fp,"center_meridian:",
             meta->projection->param.albers.center_meridian, 4,
             "Longitude of center meridian [degrees]");
      meta_put_double_lf(fp,"orig_latitude:",
             meta->projection->param.albers.orig_latitude, 4,
             "Latitude of the projection origin [degrees]");
      if (META_VERSION >= 1.3) {
    meta_put_double_lf(fp,"false_easting:",
               meta->projection->param.albers.false_easting, 3,
               "False easting [m]");
    meta_put_double_lf(fp,"false_northing:",
               meta->projection->param.albers.false_northing, 3,
               "False northing [m]");
      }
      meta_put_string(fp,"}","","End albers");
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      meta_put_string(fp,"lamaz {","","Begin Lambert Azimuthal Equal Area projection");
      meta_put_double_lf(fp,"center_lat:",
             meta->projection->param.lamaz.center_lat, 4,
             "Latitude at center of projection");
      meta_put_double_lf(fp,"center_lon:",
             meta->projection->param.lamaz.center_lon, 4,
             "Longitude at center of projection");
      if (META_VERSION >= 1.3) {
    meta_put_double_lf(fp,"false_easting:",
               meta->projection->param.lamaz.false_easting, 3,
               "False easting [m]");
    meta_put_double_lf(fp,"false_northing:",
               meta->projection->param.lamaz.false_northing, 3,
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
  meta_put_double_lf(fp,"false_easting:",
             meta->projection->param.utm.false_easting, 3,
             "False easting [m]");
  meta_put_double_lf(fp,"false_northing:",
             meta->projection->param.utm.false_northing, 3,
             "False northing [m]");
  meta_put_double_lf(fp,"latitude:",meta->projection->param.utm.lat0, 4,
             "Latitude [degrees]");
  meta_put_double_lf(fp,"longitude:",meta->projection->param.utm.lon0, 4,
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
    case MERCATOR:
      meta_put_string(fp,"mer {","","Begin Mercator projection");
      meta_put_double_lf(fp,"standard_parallel:",
			 meta->projection->param.mer.standard_parallel, 4,
			 "First standard parallel [degrees]");
      meta_put_double_lf(fp,"central_meridian:",
			 meta->projection->param.mer.central_meridian, 4,
			 "Longitude of center meridian [degrees]");
      meta_put_double_lf(fp,"orig_latitude:",
			 meta->projection->param.mer.orig_latitude, 4,
			 "Latitude of the projection origin [degrees]");
      meta_put_double_lf(fp,"false_easting:",
			 meta->projection->param.mer.false_easting, 3,
			 "False easting [m]");
      meta_put_double_lf(fp,"false_northing:",
			 meta->projection->param.mer.false_northing, 3,
			 "False northing [m]");
      meta_put_string(fp,"}","","End mer");
      break;
    case EQUI_RECTANGULAR:
      meta_put_string(fp,"eqr {","","Begin Equirectangular projection");
      meta_put_double_lf(fp,"central_meridian:",
			 meta->projection->param.eqr.central_meridian, 4,
			 "Longitude of center meridian [degrees]");
      meta_put_double_lf(fp,"orig_latitude:",
			 meta->projection->param.eqr.orig_latitude, 4,
			 "Latitude of the projection origin [degrees]");
      meta_put_double_lf(fp,"false_easting:",
			 meta->projection->param.eqr.false_easting, 3,
			 "False easting [m]");
      meta_put_double_lf(fp,"false_northing:",
			 meta->projection->param.eqr.false_northing, 3,
			 "False northing [m]");
      meta_put_string(fp,"}","","End eqr");
      break;
    case EQUIDISTANT:
      meta_put_string(fp,"eqc {","","Begin Equidistant projection");
      meta_put_double_lf(fp,"central_meridian:",
			 meta->projection->param.eqc.central_meridian, 4,
			 "Longitude of center meridian [degrees]");
      meta_put_double_lf(fp,"orig_latitude:",
			 meta->projection->param.eqc.orig_latitude, 4,
			 "Latitude of the projection origin [degrees]");
      meta_put_string(fp,"}","","End eqc");
      break;
    case SINUSOIDAL:
      meta_put_string(fp,"sin {","","Begin Sinusoidal projection");
      meta_put_double_lf(fp,"longitude_center:",
			 meta->projection->param.sin.longitude_center, 4,
			 "Longitude of the projection center [degrees]");
      meta_put_double_lf(fp,"false_easting:",
			 meta->projection->param.sin.false_easting, 3,
			 "False easting [m]");
      meta_put_double_lf(fp,"false_northing:",
			 meta->projection->param.sin.false_northing, 3,
			 "False northing [m]");
      meta_put_string(fp,"}","","End sin");
      break;
    case EASE_GRID_GLOBAL:
      meta_put_string(fp,"cea {","","Begin Cylindrical Equal Area projection");
      meta_put_double_lf(fp,"stanard_parallel:",
			 meta->projection->param.cea.standard_parallel, 4,
			 "Standard parallel [degrees]");
      meta_put_double_lf(fp,"central_meridian:",
			 meta->projection->param.cea.central_meridian, 4,
			 "Longitude of central meridian [degrees]");
      meta_put_double_lf(fp,"false_easting:",
			 meta->projection->param.cea.false_easting, 3,
			 "False easting [m]");
      meta_put_double_lf(fp,"false_northing:",
			 meta->projection->param.cea.false_northing, 3,
			 "False northing [m]");
      meta_put_string(fp,"}","","End cea");
      break;
    case EASE_GRID_NORTH:
    case EASE_GRID_SOUTH:
      meta_put_string(fp,"lamaz {","","Begin Lambert Azimuthal Equal Area projection");
      meta_put_double_lf(fp,"center_lat:",
			 meta->projection->param.lamaz.center_lat, 4,
			 "Latitude at center of projection");
      meta_put_double_lf(fp,"center_lon:",
			 meta->projection->param.lamaz.center_lon, 4,
			 "Longitude at center of projection");
      meta_put_double_lf(fp,"false_easting:",
			 meta->projection->param.lamaz.false_easting, 3,
			 "False easting [m]");
      meta_put_double_lf(fp,"false_northing:",
			 meta->projection->param.lamaz.false_northing, 3,
			 "False northing [m]");
      meta_put_string(fp,"}","","End lamaz");
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
    case UNKNOWN_PROJECTION:
      /* This projection type doesn't need its own parameter block,
   since all its values are specified in the main projection
   structure.  */
      break;
      /*
    default:
      printf("WARNING in asf_meta library function '%s': unknown projection type '%c'.\n",
             "meta_write", meta->projection->type);
      */
    }
    meta_put_string(fp,"}","","End param");
    meta_put_string(fp,"}","","End projection");
  }

  // Write out coordinate transformation parameters (e.g. optical ALOS)
  if (meta->transform) {
    int ii;
    char coeff[15];
    meta_put_string(fp,"transform {","",
        "Block containing ALOS coordinate transformation parameters");
    meta_put_string(fp,"type:",meta->transform->type,
		    "Type: slant or ground (depending on geometry)");
    meta_put_double(fp,"source pixel size:", meta->transform->source_pixel_size,
          "Original pixel size of the l/s to lat/lon mapping");
    meta_put_double(fp,"target pixel size:", meta->transform->target_pixel_size,
          "New pixel size of the l/s to lat/lon mapping");
    meta_put_int(fp,"parameter_count:",meta->transform->parameter_count,
     "Number of transformation parameters");
    for (ii=0; ii<meta->transform->parameter_count; ii++) {
      sprintf(coeff, "phi(%d):", ii);
      meta_put_double(fp,coeff,meta->transform->y[ii],
          "Latitude transformation parameter");
    }
    for (ii=0; ii<meta->transform->parameter_count; ii++) {
      sprintf(coeff, "lambda(%d):", ii);
      meta_put_double(fp,coeff,meta->transform->x[ii],
          "Longitude transformation parameter");
    }
    if (meta->transform->parameter_count >= 25) {
      meta_put_double(fp, "origin pixel:", meta->transform->origin_pixel,
              "Origin pixel for transformation");
      meta_put_double(fp, "origin line:", meta->transform->origin_line,
              "Origin line for transformation");
    }
    for (ii=0; ii<meta->transform->parameter_count; ii++) {
      sprintf(coeff, "i(%d):", ii);
      meta_put_double(fp,coeff,meta->transform->s[ii],
          "Pixel transformation parameter");
    }
    for (ii=0; ii<meta->transform->parameter_count; ii++) {
      sprintf(coeff, "j(%d):", ii);
      meta_put_double(fp,coeff,meta->transform->l[ii],
                      "Line transformation parameter");
    }
    if (meta->transform->parameter_count >= 25) {
      meta_put_double(fp, "origin lat:", meta->transform->origin_lat,
              "Origin latitude [degrees]");
      meta_put_double(fp, "origin lon:", meta->transform->origin_lon,
              "Origin longitude [degrees]");
    }
    for (ii=0; ii<10; ++ii) {
      sprintf(coeff, "map_a(%d):", ii);
      meta_put_double(fp, coeff, meta->transform->map2ls_a[ii],
                      "Map to line/sample transformation parameter");
    }
    for (ii=0; ii<10; ++ii) {
      sprintf(coeff, "map_b(%d):", ii);
      meta_put_double(fp, coeff, meta->transform->map2ls_b[ii],
                      "Map to line/sample transformation parameter");
    }

    meta_put_string(fp,"}","","End transform");
  }

  // Write out airsar geocoding parameters
  if (meta->airsar) {
    meta_put_string(fp, "airsar {", "",
            "Block containing AirSAR parameters for geocoding");
    meta_put_double(fp, "scale_factor:", meta->airsar->scale_factor,
            "General scale factor");
    meta_put_double_lf(fp, "gps_altitude:", meta->airsar->gps_altitude, 3,
            "GPS altitude [m]");
    meta_put_double_lf(fp, "lat_peg_point:", meta->airsar->lat_peg_point, 4,
            "Latitude of peg point [degrees]");
    meta_put_double_lf(fp, "lon_peg_point:", meta->airsar->lon_peg_point, 4,
            "Longitude of peg point [degrees]");
    meta_put_double_lf(fp, "head_peg_point:", meta->airsar->head_peg_point, 4,
            "Heading at peg point [degrees]");
    meta_put_double_lf(fp, "along_track_offset:",
               meta->airsar->along_track_offset, 3,
               "Along-track offset S0 [m]");
    meta_put_double_lf(fp, "cross_track_offset:",
               meta->airsar->cross_track_offset, 3,
               "Cross-track offset C0 [m]");
    meta_put_string(fp, "}", "", "End airsar");
  }

  // Write out uavsar geocoding parameters
  if (meta->uavsar) {
    meta_put_string(fp, "uavsar {", "",
            "Block containing UAVSAR parameters for geocoding");
    meta_put_string(fp, "id:", meta->uavsar->id, "File name");
    meta_put_double(fp, "scale_factor:", meta->uavsar->scale_factor,
            "General scale factor");
    meta_put_double_lf(fp, "gps_altitude:", meta->uavsar->gps_altitude, 3,
            "GPS altitude [m]");
    meta_put_double_lf(fp, "lat_peg_point:", meta->uavsar->lat_peg_point, 4,
            "Latitude of peg point [degrees]");
    meta_put_double_lf(fp, "lon_peg_point:", meta->uavsar->lon_peg_point, 4,
            "Longitude of peg point [degrees]");
    meta_put_double_lf(fp, "head_peg_point:", meta->uavsar->head_peg_point, 4,
            "Heading at peg point [degrees]");
    meta_put_double_lf(fp, "along_track_offset:",
               meta->uavsar->along_track_offset, 4,
               "Along-track offset S0 [m]");
    meta_put_double_lf(fp, "cross_track_offset:",
               meta->uavsar->cross_track_offset, 4,
               "Cross-track offset C0 [m]");
    meta_put_string(fp, "}", "", "End uavsar");
  }

  // Write out DEM parameters
  if (meta->dem) {
    meta_put_string(fp, "dem {", "", "Block containing DEM parameters");
    meta_put_string(fp, "source:", meta->dem->source, "DEM source");
    meta_put_string(fp, "format:", meta->dem->format, "DEM format");
    meta_put_string(fp, "tiles:", meta->dem->tiles, "DEM tiles");
    meta_put_double(fp, "min_value:", meta->dem->min_value, "minimum value");
    meta_put_double(fp, "max_value:", meta->dem->max_value, "maximum value");
    meta_put_double(fp, "mean_value:", meta->dem->mean_value, "mean value");
    meta_put_double(fp, "standard_deviation:", meta->dem->standard_deviation,
		    "standard deviation");
    meta_put_string(fp, "unit_type:", meta->dem->unit_type, "unit type");
    meta_put_double(fp, "no_data:", meta->dem->no_data, "no data value");
    meta_put_string(fp, "}", "", "End dem");
  }

  /* Write out statistics block */
  if (meta->stats) {
    int ii;
    meta_put_string(fp,"statistics {","","Block containing basic image statistics");
    meta_put_int(fp, "band_count:", meta->stats->band_count,"Number of statistics blocks (1 per band)");
    for (ii=0; ii<meta->stats->band_count; ii++) {
      meta_put_string(fp,"band_stats {","","Block containing band statistics");
      meta_put_string(fp,"band_id:",meta->stats->band_stats[ii].band_id,"Band name");
      meta_put_double(fp,"min:",meta->stats->band_stats[ii].min,"Minimum sample value");
      meta_put_double(fp,"max:",meta->stats->band_stats[ii].max,"Maximum sample value");
      meta_put_double(fp,"mean:",meta->stats->band_stats[ii].mean,"Mean average of sample values");
      meta_put_double(fp,"rmse:",meta->stats->band_stats[ii].rmse,"Root mean squared error");
      meta_put_double(fp,"std_deviation:",meta->stats->band_stats[ii].std_deviation,
          "Standard deviation");
      meta_put_double(fp,"percent_valid:",meta->stats->band_stats[ii].percent_valid,
          "Percent of valid values");
      meta_put_double(fp,"mask:",meta->stats->band_stats[ii].mask,
          "Value ignored while taking statistics");
      meta_put_string(fp,"}","","End band statistics block");
    }
    meta_put_string(fp,"}","","End stats");
  }

  /* Write out location block - version 1.5 */
  if (meta->location) {
    meta_put_string(fp,"location {","","Block containing image corner coordinates");
    meta_put_double_lf(fp,"lat_start_near_range:",
               meta->location->lat_start_near_range, 4,
               "Latitude at image start in near range");
    meta_put_double_lf(fp,"lon_start_near_range:",
               meta->location->lon_start_near_range, 4,
               "Longitude at image start in near range");
    meta_put_double_lf(fp,"lat_start_far_range:",
               meta->location->lat_start_far_range, 4,
               "Latitude at image start in far range");
    meta_put_double_lf(fp,"lon_start_far_range:",
               meta->location->lon_start_far_range, 4,
               "Longitude at image start in far range");
    meta_put_double_lf(fp,"lat_end_near_range:",
               meta->location->lat_end_near_range, 4,
               "Latitude at image end in near range");
    meta_put_double_lf(fp,"lon_end_near_range:",
               meta->location->lon_end_near_range, 4,
               "Longitude at image end in near range");
    meta_put_double_lf(fp,"lat_end_far_range:",
               meta->location->lat_end_far_range, 4,
               "Latitude at image end in far range");
    meta_put_double_lf(fp,"lon_end_far_range:",
               meta->location->lon_end_far_range, 4,
               "Longitude at image end in far range");
    meta_put_string(fp,"}","","End location");
  }

  // Write out calibration block - version 2.8
  if (meta->calibration) {
    int ii;
    char str[15], gain[15], coeffs[50];

    meta_put_string(fp,"calibration {","",
            "Block containing calibration information");
    strcpy(comment,"Calibration type (ASF, ASF ScanSAR, ESA, RSAT, ALOS, TSX, RSAT2, UAVSAR)");
    switch (meta->calibration->type)
      {
      case unknown_cal:
    break;
      case asf_cal:
    meta_put_string(fp,"type:","ASF",comment);
    // scaledPower =
    //      (p->a1*(inDn*inDn-p->a0*noiseValue) + p->a2)*invIncAngle;
    meta_put_double(fp,"a(0):",meta->calibration->asf->a0,
            "Calibration coefficient");
    meta_put_double(fp,"a(1):",meta->calibration->asf->a1,
            "Calibration coefficient");
    meta_put_double(fp,"a(2):",meta->calibration->asf->a2,
            "Calibration coefficient");
    meta_put_int(fp,"sample_count:",meta->calibration->asf->sample_count,
             "Number of samples per line");
    for (ii=0; ii<256; ii++) {
      sprintf(str, "noise(%d):", ii);
      meta_put_double(fp,str,meta->calibration->asf->noise[ii],
              "Noise vector element");
    }
    break;
      case asf_scansar_cal:
    meta_put_string(fp,"type:","ASF_SCANSAR",comment);
    // scaledPower =
    //      (p->a1*(inDn*inDn-p->a0*noiseValue) + p->a2)*invIncAngle;
    meta_put_double(fp,"a(0):",meta->calibration->asf_scansar->a0,
            "Calibration coefficient");
    meta_put_double(fp,"a(1):",meta->calibration->asf_scansar->a1,
            "Calibration coefficient)");
    meta_put_double(fp,"a(2):",meta->calibration->asf_scansar->a2,
            "Calibration coefficient");
    for (ii=0; ii<256; ii++) {
      sprintf(str, "noise(%d):", ii);
      meta_put_double(fp,str,meta->calibration->asf_scansar->noise[ii],
              "Noise vector element");
    }
    break;
      case esa_cal:
    meta_put_string(fp,"type:","ESA",comment);
    // scaledPower =
    //  inDn*inDn/p->k*sin(p->ref_incid*D2R)/sin(incidence_angle*D2R);
    meta_put_double(fp,"k:",meta->calibration->esa->k,
            "Absolute calibration coefficient");
    meta_put_double(fp,"ref_incid:",meta->calibration->esa->ref_incid,
            "Reference incidence angle");
    break;
      case rsat_cal:
    meta_put_string(fp,"type:","RSAT",comment);
    // if (p->slc)
    //   scaledPower = (inDn*inDn)/(a2*a2)*invIncAngle;
    // else
    //   scaledPower = (inDn*inDn + p->a3)/a2*invIncAngle;
    meta_put_int(fp,"table_entries:",meta->calibration->rsat->n,
             "Number of table entries");
    for (ii=0; ii<meta->calibration->rsat->n; ii++) {
      sprintf(str, "lut(%d):", ii);
      meta_put_double(fp,str,meta->calibration->rsat->lut[ii],
              "Output scaling LUT entry");
    }
    meta_put_int(fp,"sample_inc:",meta->calibration->rsat->samp_inc,
             "Table entry sampling increment");
    meta_put_double(fp,"a3:",meta->calibration->rsat->a3,
            "Linear scaling offset");
    meta_put_int(fp,"slc:",meta->calibration->rsat->slc,
             "Flag: Data is SLC");
    meta_put_int(fp,"focus:",meta->calibration->rsat->focus,
             "Flag: Data processed by FOCUS");
    break;
      case alos_cal:
    meta_put_string(fp,"type:","ALOS",comment);
    // scaledPower = pow(10, p->cf/10.0)*inDn*inDn*invIncAngle;
    if (!ISNAN(meta->calibration->alos->cf_hh))
      meta_put_double(fp,"cf_hh:",meta->calibration->alos->cf_hh,
		      "Calibration factor: HH");
    if (!ISNAN(meta->calibration->alos->cf_hv))
      meta_put_double(fp,"cf_hv:",meta->calibration->alos->cf_hv,
		      "Calibration factor: HV");
    if (!ISNAN(meta->calibration->alos->cf_vh))
      meta_put_double(fp,"cf_vh:",meta->calibration->alos->cf_vh,
		      "Calibration factor: VH");
    if (!ISNAN(meta->calibration->alos->cf_vv))
      meta_put_double(fp,"cf_vv:",meta->calibration->alos->cf_vv,
		      "Calibration factor: VV");
    break;
      case tsx_cal:
	meta_put_string(fp,"type:","TSX",comment);
	// scaledPower = pow(10, p->cf/10.0)*inDn*inDn*invIncAngle;
	meta_put_double(fp,"k:",meta->calibration->tsx->k,
			"Calibration factor");
	break;
      case uavsar_cal:
	meta_put_string(fp,"type:","UAVSAR",comment);
	meta_put_double(fp,"semi_major:",meta->calibration->uavsar->semi_major,
			"Ellipsoid semi-major axis [m]");
	meta_put_double(fp, "slant_range_first_pixel:",
			meta->calibration->uavsar->slant_range_first_pixel,
			"Image starting range [m]");
	meta_put_double(fp, "range_spacing:",
			meta->calibration->uavsar->range_spacing,
			"Range spacing per bin [m]");
	meta_put_double(fp, "azimuth_spacing:",
			meta->calibration->uavsar->azimuth_spacing,
			"Azimuth spacing [m]");
	meta_put_double(fp, "pitch:",meta->calibration->uavsar->pitch,
			"Global average pitch [degrees]");
	meta_put_double(fp, "steering_angle:",
			meta->calibration->uavsar->steering_angle,
			"Global average electronic steering angle [degrees]");
	meta_put_double(fp, "altitude:",meta->calibration->uavsar->altitude,
			"Global average altitude [m]");
	meta_put_double(fp, "terrain_height:",
			meta->calibration->uavsar->terrain_height,
			"Global average terrain height [m]");
	break;
      case r2_cal:
	meta_put_string(fp,"type:","RSAT2",comment);
	meta_put_int(fp,"num_elements:",meta->calibration->r2->num_elements,
		     "Number of gain coefficients");
	for (ii=0; ii<meta->calibration->r2->num_elements; ii++) {
	  sprintf(gain, "gain(%d):", ii);
	  sprintf(coeffs, "%f %f %f", 
		  meta->calibration->r2->a_beta[ii],
		  meta->calibration->r2->a_gamma[ii],
		  meta->calibration->r2->a_sigma[ii]);
	  meta_put_string(fp, gain, coeffs, 
			  "Gain coefficient A: beta, gamma, sigma");
	}
	meta_put_double(fp,"offset:",meta->calibration->r2->b,
			"Constant offset B");
	meta_put_double(fp,"slc:",meta->calibration->r2->slc,"Data SLC?");
	break;
	    case sentinel_cal:
	    meta_put_string(fp,"type:","SENTINEL",comment);
	    meta_put_double(fp,"noise_mean:",meta->calibration->sentinel->noise_mean,
	      "Mean value of the noise floor");
	    break;
    }

    meta_put_string(fp,"}","","End calibration");
  }

  /* Write out color map block */
  if (meta->colormap) {
      int i;
      char rgb_row[256], idx[64];
      meta_put_string(fp,"colormap {","","Block containing Indexed RGB colormap");
      meta_put_string(fp,"look_up_table:",meta->colormap->look_up_table,
		      "Type of look up table");
      meta_put_string(fp,"band_id:",meta->colormap->band_id,
		      "Band that the look up table applies to");
      meta_put_int(fp, "num_elements:", meta->colormap->num_elements, 
		   "Number of elements in colormap");
      for (i=0; i < meta->colormap->num_elements; i++) {
          sprintf(idx, "idx(%03d):", i);
          sprintf(rgb_row, "%03d  %03d  %03d",
                  meta->colormap->rgb[i].red, 
		  meta->colormap->rgb[i].green, 
		  meta->colormap->rgb[i].blue);
          meta_put_string(fp,idx,rgb_row,"red_value green_value blue_value");
      }
      meta_put_string(fp,"}","","End colormap");
  }

  // Write out insar block
  if (meta->insar) {
    meta_put_string(fp, "insar {","","Block containing InSAR parameters");
    meta_put_string(fp, "processor:", meta->insar->processor,
		    "Name of processor: ROIPAC, GAMMA");
    meta_put_string(fp, "master_image:", meta->insar->master_image,
		    "Name of master image");
    meta_put_string(fp, "slave_image:", meta->insar->slave_image,
		    "Name of slave image");
    meta_put_string(fp, "master_acquisition_date:",
                    meta->insar->master_acquisition_date,
		    "Acquisition date of master image");
    meta_put_string(fp, "slave_acquisition_date:",
                    meta->insar->slave_acquisition_date,
		    "Acquisition date of slave image");
    meta_put_double_lf(fp, "center_look_angle:", 
		       meta->insar->center_look_angle, 4,
		       "Center look angle [degrees]");
    meta_put_double(fp, "doppler:", meta->insar->doppler,
		    "Doppler (constant term) [Hz]");
    meta_put_double(fp, "doppler_rate:", 
		    meta->insar->doppler_rate, 
		    "Doppler rate [Hz/m]");
    meta_put_double_lf(fp, "baseline_length:", meta->insar->baseline_length, 1,
		       "Length of baseline [m]");
    meta_put_double_lf(fp, "baseline_parallel:", 
		       meta->insar->baseline_parallel, 1,
		       "Parallel baseline component [m]");
    meta_put_double(fp, "baseline_parallel_rate:",
		    meta->insar->baseline_parallel_rate,
		    "Parallel baseline rate [m/s]");
    meta_put_double_lf(fp, "baseline_perpendicular:", 
		       meta->insar->baseline_perpendicular, 1,
		       "Perpendicular baseline component [m]");
    meta_put_double(fp, "baseline_perpendicular_rate:",
		    meta->insar->baseline_perpendicular_rate,
		    "Perpendicular baseline rate [m/s]");
    meta_put_int(fp, "baseline_temporal:", meta->insar->baseline_temporal,
		 "Temporal baseline [days]");
    meta_put_double_lf(fp, "baseline_critical:", 
		       meta->insar->baseline_critical, 1,
		       "Critical baseline [m]");
    meta_put_string(fp,"}","","End insar");
  }
  
  if (meta->quality) {
  	meta_put_string(fp,"quality {","","Block containing data quality parameters");
  	meta_put_double(fp, "bit_error_rate:", meta->quality->bit_error_rate,
  		"Bit error rate");
		meta_put_double(fp, "azimuth_resolution:", meta->quality->azimuth_resolution,
			"Nominal azimuth resolution [m]");
		meta_put_double(fp, "range_resolution:", meta->quality->range_resolution,
			"Nominal range resolution [m]");
		meta_put_double(fp, "signal_to_noise_ratio:", 
			meta->quality->signal_to_noise_ratio, "Signal to noise ratio [dB]");
		meta_put_double(fp, "peak_sidelobe_ratio:", 
			meta->quality->peak_sidelobe_ratio, "Peak to sidelobe ratio");
		meta_put_double(fp, "integrated_sidelobe_ratio:", 
			meta->quality->integrated_sidelobe_ratio, "Integrated sidelobe ratio");

  	meta_put_string(fp,"}","","End quality");
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
    char oldproj=0;
    proj_parameters *proj=meta->geo->proj;
    meta_put_string(fp,"proj {","","Map Projection parameters");
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
      case SCANSAR_PROJECTION:/*Along-track/cross-track projection.*/
        oldproj='A';
  meta_put_double(fp,"rlocal:",proj->param.atct.rlocal,"Local earth radius [m]");
        meta_put_double(fp,"atct_alpha1:",proj->param.atct.alpha1,
      "at/ct projection parameter");
        meta_put_double(fp,"atct_alpha2:",proj->param.atct.alpha2,
      "at/ct projection parameter");
        meta_put_double(fp,"atct_alpha3:",proj->param.atct.alpha3,
      "at/ct projection parameter");
        break;
      case LAMBERT_CONFORMAL_CONIC:/*Lambert Conformal Conic projection.*/
        oldproj='L';
        meta_put_double(fp,"lam_plat1:",proj->param.lamcc.plat1,
      "Lambert first standard parallel");
        meta_put_double(fp,"lam_plat2:",proj->param.lamcc.plat2,
      "Lambert second standard parallel");
        meta_put_double(fp,"lam_lat:",proj->param.lamcc.lat0,
      "Lambert original latitude");
        meta_put_double(fp,"lam_lon:",proj->param.lamcc.lon0,
      "Lambert original longitude");
        break;
      case POLAR_STEREOGRAPHIC:/*Polar Stereographic Projection.*/
        oldproj='P';
        meta_put_double(fp,"ps_lat:",proj->param.ps.slat,
      "Polar Stereographic reference Latitude");
        meta_put_double(fp,"ps_lon:",proj->param.ps.slon,
      "Polar Stereographic reference Longitude");
        break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:/*Universal Trasnverse Mercator Projection.*/
        oldproj='U';
        meta_put_int(fp,"utm_zone:",proj->param.utm.zone,"UTM Zone Code");
        break;
      case LAT_LONG_PSEUDO_PROJECTION:/*Geographic coordinates.*/
        oldproj='G';
        break;
      default: ; /* Don't worry about missing projection types--just leave field blank */
      /*
        printf("ERROR! Unrecognized map projection code '%c' in function '%s'; program exitting.\n",
                proj->type, "meta_write_old");
        exit(EXIT_FAILURE);*/
    }
    if (oldproj) {
      meta_put_char(fp,"type:",oldproj,
      "Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
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
  if (!string) return 1;
  for (ii=0; ii<strlen(string); ii++) {
    if (!isspace(string[ii])) return 0;
  }
  return 1;
}
void meta_put_string(FILE *meta_file,char *name,char *value,char *comment)
{
  int ii;
  int malloc_flag=0;
  char line[1024];/*The line to be written to the file.*/
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
  int n = fprintf(meta_file,"%s\n",line);
  if (n < 0) {
    if (errno == ENOMEM)
      asfPrintError("meta_put_string: "
                    "Insufficient storage space is available\n");
    else
      asfPrintError("fprint error: %d\n", strerror(errno));
  }
}

void meta_put_double(FILE *meta_file,char *name,double value,char *comment)
{
  char param[64];
  strcpy(param,"NaN");
  if (meta_is_valid_double(value))
  {
    sprintf(param,"%-16.11g",value);
    strtok(param," ");/*remove all trailing spaces */
    if (is_empty(param)) { strcpy(param,"NaN"); }
  }
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

void meta_put_double_lf(FILE *meta_file,char *name,double value,int decimals,
            char *comment)
{
  char param[250];
  strcpy(param, "NaN");
  if (meta_is_valid_double(value))
  {
    char format[64];
    sprintf(format, "%%-16.%if", decimals);
    snprintf(param,249,format,value);
    strtok(param," ");/*remove all trailing spaces */
    if (is_empty(param)) { strcpy(param,"nan"); }
  }
  meta_put_string(meta_file,name,param,comment);
}

void meta_write_xml(meta_parameters *meta, const char *file_name)
{
  FILE *fp = FOPEN(file_name, "w");
  int ii, kk;
  meta_general *mg = meta->general;
  fprintf(fp, "<metadata>\n");
  fprintf(fp, "  <metadata_type>Alaska Satellite Facility</metadata_type>\n");
  fprintf(fp, "  <meta_version>%.2f</meta_version>\n", meta->meta_version);
  fprintf(fp, "  <general>\n");
  fprintf(fp, "    <name>%s</name>\n", mg->basename);
  fprintf(fp, "    <sensor>%s</sensor>\n", mg->sensor);
  if (strcmp_case(mg->sensor, "ALOS") == 0 && 
  		strcmp_case(mg->sensor_name, "SAR") == 0)
  	fprintf(fp, "    <sensor_name>PALSAR</sensor_name>\n");
  else
  	fprintf(fp, "    <sensor_name>%s</sensor_name>\n", mg->sensor_name);
  fprintf(fp, "    <mode>%s</mode>\n", mg->mode);
  fprintf(fp, "    <processor>%s</processor>\n", mg->processor);
  char *data_type = data_type2str(mg->data_type);
  fprintf(fp, "    <data_type>%s</data_type>\n", data_type);
  FREE(data_type);
  data_type = image_data_type2str(mg->image_data_type);
  fprintf(fp, "    <image_data_type>%s</image_data_type>\n", data_type);
  FREE(data_type);
  char *rad_str = radiometry2str(mg->radiometry);
  fprintf(fp, "    <radiometry>%s</radiometry>\n", rad_str);
  FREE(rad_str);
  fprintf(fp, "    <acquisition_date>%s</acquisition_date>\n",
	  mg->acquisition_date);
  fprintf(fp, "    <orbit>%d</orbit>\n", mg->orbit);
  if (mg->orbit_direction == 'A')
    fprintf(fp, "    <orbit_direction>ascending</orbit_direction>\n");
  else if (mg->orbit_direction == 'D')
    fprintf(fp, "    <orbit_direction>descending</orbit_direction>\n");
  fprintf(fp, "    <frame>%d</frame>\n", mg->frame);
  fprintf(fp, "    <band_count>%d</band_count>\n", mg->band_count);
  fprintf(fp, "    <bands>%s</bands>\n", mg->bands);
  fprintf(fp, "    <line_count>%d</line_count>\n", mg->line_count);
  fprintf(fp, "    <sample_count>%d</sample_count>\n", mg->sample_count);
  fprintf(fp, "    <start_line>%d</start_line>\n", mg->start_line);
  fprintf(fp, "    <start_sample>%d</start_sample>\n", mg->start_sample);
  fprintf(fp, "    <x_pixel_size units=\"m\">%.11g</x_pixel_size>\n", 
	  mg->x_pixel_size);
  fprintf(fp, "    <y_pixel_size units=\"m\">%.11g</y_pixel_size>\n", 
	  mg->y_pixel_size);
  fprintf(fp, "    <center_latitude units=\"degrees\">%.4f</center_latitude>"
	  "\n", mg->center_latitude);
  fprintf(fp, "    <center_longitude units=\"degrees\">%.4f"
	  "</center_longitude>\n", mg->center_longitude);
  fprintf(fp, "    <re_major units=\"m\">%.3f</re_major>\n", mg->re_major);
  fprintf(fp, "    <re_minor units=\"m\">%.3f</re_minor>\n", mg->re_minor);
  fprintf(fp, "    <bit_error_rate>%g</bit_error_rate>\n", mg->bit_error_rate);
  fprintf(fp, "    <missing_lines>%i</missing_lines>\n", mg->missing_lines);
  fprintf(fp, "    <no_data>%.4f</no_data>\n", mg->no_data);
  fprintf(fp, "  </general>\n");

  if (meta->sar) {
    meta_sar *ms = meta->sar;
    fprintf(fp, "  <sar>\n");
    fprintf(fp, "    <polarization>%s</polarization>\n", ms->polarization);
    if (ms->image_type == 'S')
      fprintf(fp, "    <image_type>slant range</image_type>\n");
    else if (ms->image_type == 'G')
      fprintf(fp, "    <image_type>ground range</image_type>\n");
    else if (ms->image_type == 'P')
      fprintf(fp, "    <image_type>projected</image_type>\n");
    else if (ms->image_type == 'R')
      fprintf(fp, "    <image_type>georeferenced</image_type>\n");
    if (ms->look_direction == 'R')
      fprintf(fp, "    <look_direction>right</look_direction>\n");
    else if (ms->look_direction == 'L')
      fprintf(fp, "    <look_direction>left</look_direction>\n");
    fprintf(fp, "    <azimuth_look_count>%d</azimuth_look_count>\n", 
	    ms->azimuth_look_count);
    fprintf(fp, "    <range_look_count>%d</range_look_count>\n",
	    ms->range_look_count);
    fprintf(fp, "    <multilook>%d</multilook>\n", ms->multilook);
    fprintf(fp, "    <deskewed>%d</deskewed>\n", ms->deskewed);
    fprintf(fp, "    <original_line_count>%d</original_line_count>\n",
	    ms->original_line_count);
    fprintf(fp, "    <original_sample_count>%d</original_sample_count>\n",
	    ms->original_sample_count);
    fprintf(fp, "    <line_increment>%.11g</line_increment>\n", 
	    ms->line_increment);
    fprintf(fp, "    <sample_increment>%.11g</sample_increment>\n",
	    ms->sample_increment);
    fprintf(fp, "    <range_time_per_pixel units=\"s\">%.11g"
	    "</range_time_per_pixel>\n", ms->range_time_per_pixel);
    fprintf(fp, "    <azimuth_time_per_pixel units=\"s\">%.11g"
	    "</azimuth_time_per_pixel>\n", ms->azimuth_time_per_pixel);
    fprintf(fp, "    <slant_range_first_pixel units=\"m\">%.3f"
	    "</slant_range_first_pixel>\n", ms->slant_range_first_pixel);
    fprintf(fp, "    <slant_shift units=\"m\">%.11g</slant_shift>\n",
	    ms->slant_shift);
    fprintf(fp, "    <time_shift units=\"s\">%.11g</time_shift>\n", 
	    ms->time_shift);
    fprintf(fp, "    <wavelength units=\"m\">%.11g</wavelength>\n", 
	    ms->wavelength);
    fprintf(fp, "    <prf units=\"Hz\">%.11g</prf>\n", ms->prf);
    fprintf(fp, "    <earth_radius units=\"m\">%.3f</earth_radius>\n", 
	    ms->earth_radius);
    fprintf(fp, "    <earth_radius_pp units=\"m\">%.3f</earth_radius_pp>\n",
	    ms->earth_radius_pp);
    fprintf(fp, "    <satellite_height units=\"m\">%.3f</satellite_height>\n",
	    ms->satellite_height);
    fprintf(fp, "    <dopRangeCen units=\"Hz\">%.11g</dopRangeCen>\n", 
	    ms->range_doppler_coefficients[0]);
    fprintf(fp, "    <dopRangeLin units=\"Hz/pixel\">%.11g</dopRangeLin>"
	    "\n", ms->range_doppler_coefficients[1]);
    fprintf(fp, "    <dopRangeQuad units=\"Hz/(pixel^2)\">%.11g"
	    "</dopRangeQuad>\n", ms->range_doppler_coefficients[2]);
    fprintf(fp, "    <dopAzCen units=\"Hz\">%.11g</dopAzCen>\n",
	    ms->azimuth_doppler_coefficients[0]);
    fprintf(fp, "    <dopAzLin units=\"Hz/pixel\">%.11g</dopAzLin>\n",
	    ms->azimuth_doppler_coefficients[1]);
    fprintf(fp, "    <dopAzQuad units=\"Hz/(pixel^2)\">%.11g</dopAzQuad>\n",
	    ms->azimuth_doppler_coefficients[2]);
    fprintf(fp, "    <pitch units=\"degrees\">%.4f</pitch>\n", ms->pitch);
    fprintf(fp, "    <roll units=\"degrees\">%.4f</roll>\n", ms->roll);
    fprintf(fp, "    <yaw units=\"degrees\">%.4f</yaw>\n", ms->yaw);
    fprintf(fp, "    <azimuth_bandwidth units=\"Hz\">%.11g"
	    "</azimuth_bandwidth>\n", ms->azimuth_processing_bandwidth);
    fprintf(fp, "    <chirp_rate units=\"Hz/sec\">%.11g</chirp_rate>\n", 
	    ms->chirp_rate);
    fprintf(fp, "    <pulse_duration units=\"s\">%.11g</pulse_duration>\n",
	    ms->pulse_duration);
    fprintf(fp, "    <range_samp_rate units=\"Hz\">%.11g</range_samp_rate>"
	    "\n", ms->range_sampling_rate);
    for (ii=0; ii<6; ii++)
      fprintf(fp, "    <incid_a exponent=\"%d\">%.11g</incid_a>\n", ii, 
	      ms->incid_a[ii]);
    fprintf(fp, "  </sar>\n");
  }

  if (meta->doppler) {
    meta_doppler *md = meta->doppler;
    fprintf(fp, "  <doppler>\n");
    if (md->type == tsx_doppler) {
      fprintf(fp, "    <type>TSX</type>\n");
      fprintf(fp, "    <year>%d</year>\n", md->tsx->year);
      fprintf(fp, "    <julDay>%d</julDay>\n", md->tsx->julDay);
      fprintf(fp, "    <second>%f</second>\n", md->tsx->second);
      fprintf(fp, "    <doppler_count>%d</doppler_count>\n", 
	      md->tsx->doppler_count);
      for (ii=0; ii<md->tsx->doppler_count; ii++) {
	fprintf(fp,"    <estimate num=\"%d\">\n", ii+1);
	fprintf(fp,"    <time units=\"s\">%.11g</time>\n", 
		md->tsx->dop[ii].time);
	fprintf(fp,"    <first_range_time units=\"s\">%.11g"
		"</first_range_time>\n", md->tsx->dop[ii].first_range_time);
	fprintf(fp,"    <reference_time units=\"s\">%.11g</reference_time>"
		"\n", md->tsx->dop[ii].reference_time);
	fprintf(fp,"    <polynomial_degree>%d</polynomial_degree>\n",
		md->tsx->dop[ii].poly_degree);
	for (kk=0; kk<=md->tsx->dop[ii].poly_degree; kk++)
	  fprintf(fp, "    <coefficient num=\"%d\">%.11g</coefficient>\n",
		  kk, md->tsx->dop[ii].coefficient[kk]);
	fprintf(fp, "    </estimate>\n");
      }
    }
    else if (md->type == radarsat2_doppler) {
      fprintf(fp, "    <type>RADARSAT2</type>\n");
      fprintf(fp, "    <doppler_count>%d</doppler_count>\n",
	      md->r2->doppler_count);
      fprintf(fp, "    <centroid_time units=\"s\">%.11g</centroid_time>\n",
	      md->r2->ref_time_centroid);
      for (kk=0; kk<md->r2->doppler_count; kk++)
	fprintf(fp, "    <centroid exponent=\"%d\">%.11g</centroid>\n", 
		kk,md->r2->centroid[kk]);
      fprintf(fp, "    <rate_time units=\"s\">%.11g</rate_time>\n",
	      md->r2->ref_time_rate);
      for (kk=0; kk<md->r2->doppler_count; kk++)
	fprintf(fp, "    <rate exponent=\"%d\">%.11g</rate>\n", kk,
		md->r2->rate[kk]);
      fprintf(fp, "    <time_first_sample units=\"s\">%.11g"
	      "</time_first_sample>\n", md->r2->time_first_sample);
    }
    fprintf(fp, "  </doppler>\n");
  }

  if (meta->optical) {
    meta_optical *mo = meta->optical;
    fprintf(fp,"  <optical>\n");
    fprintf(fp,"    <pointing_direction>%s</pointing_direction>\n",
	    mo->pointing_direction);
    fprintf(fp,"    <off_nadir_angle units=\"degrees\">%.4f</off_nadir_angle>"
	    "\n", mo->off_nadir_angle);
    if (strcmp_case(mo->correction_level, "N") == 0)
      fprintf(fp,"    <correction_level>uncorrected</correction_level>\n");
    else if (strcmp_case(mo->correction_level, "R") == 0)
      fprintf(fp, "   <correction_level>georeferenced</correction_level>\n");
    else if (strcmp_case(mo->correction_level, "G") == 0)
      fprintf(fp, "   <correction_level>geocoded</correction_level>\n");
    else if (strcmp_case(mo->correction_level, "D") == 0)
      fprintf(fp, "   <correction_level>DEM corrected</correction_level>\n");
    fprintf(fp, "    <cloud_percentage units=\"percent\">%.1f"
	    "</cloud_percentage>\n", mo->cloud_percentage);
    fprintf(fp, "    <sun_azimuth_angle units=\"degrees\">%.4f"
	    "</sun_azimuth_angle>\n", mo->sun_azimuth_angle);
    fprintf(fp, "    <sun_elevation_angle units=\"degrees\">%.4f"
	    "</sun_elevation_angle>\n", mo->sun_elevation_angle);
    fprintf(fp,"  </optical>\n");
  }

  if (meta->state_vectors) {
    meta_state_vectors *mo = meta->state_vectors;
    fprintf(fp, "  <state>\n");
    fprintf(fp, "    <year>%d</year>\n", mo->year);
    fprintf(fp, "    <julDay>%d</julDay>\n", mo->julDay);
    fprintf(fp, "    <second units=\"s\">%.11g</second>\n", mo->second);
    fprintf(fp, "    <vector_count>%d</vector_count>\n", mo->vector_count);
    for (ii=0; ii<mo->vector_count; ii++) {
      fprintf(fp, "    <vector num=\"%d\">\n", ii+1);
      fprintf(fp, "      <time units=\"s\">%.11g</time>\n", 
	      mo->vecs[ii].time);
      fprintf(fp, "      <posX units=\"m\">%.3f</posX>\n",
	      mo->vecs[ii].vec.pos.x);
      fprintf(fp, "      <posY units=\"m\">%.3f</posY>\n",
	      mo->vecs[ii].vec.pos.y);
      fprintf(fp, "      <posZ units=\"m\">%.3f</posZ>\n",
	      mo->vecs[ii].vec.pos.z);
      fprintf(fp, "      <velX units=\"m/s\">%.3f</velX>\n",
	      mo->vecs[ii].vec.vel.x);
      fprintf(fp, "      <velY units=\"m/s\">%.3f</velY>\n",
	      mo->vecs[ii].vec.vel.y);
      fprintf(fp, "      <velZ units=\"m/s\">%.3f</velZ>\n",
	      mo->vecs[ii].vec.vel.z);
      fprintf(fp, "    </vector>\n");
    }
    fprintf(fp, "  </state>\n");
  }

  if (meta->projection) {
    meta_projection *mp = meta->projection;
    fprintf(fp, "  <projection>\n");
    if (mp->type == UNIVERSAL_TRANSVERSE_MERCATOR)
      fprintf(fp, "    <type>UNIVERSAL_TRANSVERSE_MERCATOR</type>\n");
    else if (mp->type == POLAR_STEREOGRAPHIC)
      fprintf(fp, "    <type>POLAR_STEREOGRAPHIC</type>\n");
    else if (mp->type == ALBERS_EQUAL_AREA)
      fprintf(fp, "    <type>ALBERS_CONICAL_EQUAL_AREA</type>\n");
    else if (mp->type == LAMBERT_CONFORMAL_CONIC)
      fprintf(fp, "    <type>LAMBERT_CONFORMAL_CONIC</type>\n");
    else if (mp->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)
      fprintf(fp, "    <type>LAMBERT_AZIMUTHAL_EQUAL_AREA</type>\n");
    else if (mp->type == STATE_PLANE)
      fprintf(fp, "    <type>STATE_PLANE</type>\n");
    else if (mp->type == SCANSAR_PROJECTION)
      fprintf(fp, "    <type>SCANSAR_PROJECTION</type>\n");
    else if (mp->type == LAT_LONG_PSEUDO_PROJECTION)
      fprintf(fp, "    <type>LAT_LONG_PSEUDO_PROJECTION</type>\n");
    else if (mp->type == MERCATOR)
      fprintf(fp, "    <type>MERCATOR</type>\n");
    else if (mp->type == EQUI_RECTANGULAR)
      fprintf(fp, "    <type>EQUI_RECTANGULAR</type>\n");
    else if (mp->type == SINUSOIDAL)
      fprintf(fp, "    <type>SINUSOIDAL</type>\n");
    else if (mp->type == UNKNOWN_PROJECTION)
      fprintf(fp, "    <type>UNKNOWN_PROJECTION</type>\n");
    if (mp->type != LAT_LONG_PSEUDO_PROJECTION) {
      fprintf(fp, "    <startX units=\"m\">%.8f</startX>\n", mp->startX);
      fprintf(fp, "    <startY units=\"m\">%.8f</startY>\n", mp->startY);
      fprintf(fp, "    <perX units=\"m\">%.8f</perX>\n", mp->perX);
      fprintf(fp, "    <perY units=\"m\">%.8f</perY>\n", mp->perY);
    }
    else {
      fprintf(fp, "    <startX units=\"degrees\">%.8f</startX>\n", 
	      mp->startX);
      fprintf(fp, "    <startY units=\"degrees\">%.8f</startY>\n", 
	      mp->startY);
      fprintf(fp, "    <perX units=\"degrees\">%.11g</perX>\n", mp->perX);
      fprintf(fp, "    <perY units=\"degrees\">%.11g</perY>\n", mp->perY);
    }
    fprintf(fp, "    <units>%s</units>\n", mp->units);
    if (mp->hem == 'N')
      fprintf(fp, "    <hemisphere>northern</hemisphere>\n");
    else if (mp->hem == 'S')
      fprintf(fp, "    <hemisphere>southern</hemisphere>\n");
    fprintf(fp, "    <spheroid>%s</spheroid>\n", 
	    spheroid_toString(mp->spheroid));
    fprintf(fp, "    <re_major units=\"m\">%.3f</re_major>\n", mp->re_major);
    fprintf(fp, "    <re_minor units=\"m\">%.3f</re_minor>\n", mp->re_minor);
    fprintf(fp, "    <datum>%s</datum>\n",
	    datum_toString(meta->projection->datum));
    fprintf(fp, "    <height units=\"m\">%.3f</height>\n", mp->height);
    if (mp->type == SCANSAR_PROJECTION) {
      fprintf(fp, "    <scansar_projection>\n");
      fprintf(fp, "      <rlocal units=\"m\">%.3f</rlocal>\n", 
	      mp->param.atct.rlocal);
      fprintf(fp, "      <alpha1 units=\"degrees\">%.4f</alpha1>\n", 
	      mp->param.atct.alpha1);
      fprintf(fp, "      <alpha2 units=\"degrees\">%.4f</alpha2>\n",
	      mp->param.atct.alpha2);
      fprintf(fp, "      <alpha3 units=\"degrees\">%.4f</alpha3>\n",
	      mp->param.atct.alpha3);
      fprintf(fp, "    </scansar_projection>\n");
    }
    else if (mp->type == ALBERS_EQUAL_AREA) {
      fprintf(fp, "    <albers_conical_equal_area>\n");
      fprintf(fp, "      <std_parallel1 units=\"degrees\">%.4f"
	      "</std_parallel1>\n", mp->param.albers.std_parallel1);
      fprintf(fp, "      <std_parallel2 units=\"degrees\">%.4f"
	      "</std_parallel2>\n", mp->param.albers.std_parallel2);
      fprintf(fp, "      <center_meridian units=\"degrees\">%.4f"
	      "</center_meridian>\n", mp->param.albers.center_meridian);
      fprintf(fp, "      <orig_latitude units=\"degrees\">%.4f"
	      "</orig_latitude>\n", mp->param.albers.orig_latitude);
      fprintf(fp, "      <false_easting units=\"m\">%.3f</false_easting>\n",
	      mp->param.albers.false_easting);
      fprintf(fp, "      <false_northing units=\"m\">%.3f</false_northing>\n",
	      mp->param.albers.false_northing);
      fprintf(fp,"    </albers_conical_equal_area>\n");
    }
    else if (mp->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
      fprintf(fp, "    <lambert_azimuthal_equal_area>\n");
      fprintf(fp, "      <center_lat units=\"degrees\">%.4f</center_lat>\n",
	      mp->param.lamaz.center_lat);
      fprintf(fp, "      <center_lon units=\"degrees\">%.4f</center_lon>\n",
	      mp->param.lamaz.center_lon);
      fprintf(fp, "      <false_easting units=\"m\">%.3f</false_easting>\n",
	      mp->param.lamaz.false_easting);
      fprintf(fp, "      <false_northing units=\"m\">%.3f</false_northing>\n",
	      mp->param.lamaz.false_northing);
      fprintf(fp, "    </lambert_azimuthal_equal_area>\n");
    }
    else if (mp->type == LAMBERT_CONFORMAL_CONIC) {
      fprintf(fp, "    <lambert_conformal_conic>\n");
      fprintf(fp, "      <plat1 units=\"degrees\">%.4f</plat1>\n",
	      mp->param.lamcc.plat1);
      fprintf(fp, "      <plat2 units=\"degrees\">%.4f</plat2>\n",
	      mp->param.lamcc.plat2);
      fprintf(fp, "      <lat0 units=\"degrees\">%.4f</lat0>\n",
	      mp->param.lamcc.lat0);
      fprintf(fp, "      <lon0 units=\"degrees\">%.4f</lon0>\n",
	      mp->param.lamcc.lon0);
      fprintf(fp, "      <false_easting units=\"m\">%.3f</false_easting>\n",
	      mp->param.lamcc.false_easting);
      fprintf(fp, "      <false_northing units=\"m\">%.3f</false_northing>\n",
	      mp->param.lamcc.false_northing);
      fprintf(fp, "      <scale_factor>%.11g</scale_factor>\n",
	      mp->param.lamcc.scale_factor);
      fprintf(fp, "    </lambert_conformal_conic>\n");
    }
    else if (mp->type == POLAR_STEREOGRAPHIC) {
      fprintf(fp, "    <polar_stereographic>\n");
      fprintf(fp, "      <slat units=\"degrees\">%.4f</slat>\n",
	      mp->param.ps.slat);
      fprintf(fp, "      <slon units=\"degrees\">%.4f</slon>\n",
	      mp->param.ps.slon);
      fprintf(fp, "      <false_easting units=\"m\">%.3f</false_easting>\n",
	      mp->param.ps.false_easting);
      fprintf(fp, "      <false_northing units=\"m\">%.3f</false_northing>\n",
	      mp->param.ps.false_northing);
      fprintf(fp, "    </polar_stereographic>\n");
    }
    else if (mp->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
      fprintf(fp, "    <universal_transverse_mercator>\n");
      fprintf(fp, "      <zone>%d</zone>\n", mp->param.utm.zone);
      fprintf(fp, "      <false_easting units=\"m\">%.3f</false_easting>\n",
	      mp->param.utm.false_easting);
      fprintf(fp, "      <false_northing units=\"m\">%.3f</false_northing>\n",
	      mp->param.utm.false_northing);
      fprintf(fp, "      <latitude units=\"degrees\">%.4f</latitude>\n",
	      mp->param.utm.lat0);
      fprintf(fp, "      <longitude units=\"degrees\">%.4f</longitude>\n",
	      mp->param.utm.lon0);
      fprintf(fp, "      <scale_factor>%.11g</scale_factor>\n", 
	      mp->param.utm.scale_factor);
      fprintf(fp, "    </universal_transverse_mercator>\n");
    }
    else if (mp->type == STATE_PLANE) {
      fprintf(fp, "    <state_plane>\n");
      fprintf(fp, "      <zone>%d</zone>\n", mp->param.state.zone);
      fprintf(fp, "    </state_plane>\n");
    }
    else if (mp->type == MERCATOR) {
      fprintf(fp, "    <mercator>\n");
      fprintf(fp, "      <standard_parallel units=\"degrees\">%.4f"
	      "</standard_parallel>\n", mp->param.mer.standard_parallel);
      fprintf(fp, "      <central_meridian units=\"degrees\">%.4f"
	      "</central_meridian>\n", mp->param.mer.central_meridian);
      fprintf(fp, "      <orig_latitude units=\"degress\">%.4f"
	      "</orig_latitude>\n", mp->param.mer.orig_latitude);
      fprintf(fp, "      <false_easting units=\"m\">%.3f</false_easting>\n",
	      mp->param.mer.false_easting);
      fprintf(fp, "      <false_northing units=\"m\">%.3f</false_northing>\n",
	      mp->param.mer.false_northing);
      fprintf(fp, "    </mercator>\n");
    }
    else if (mp->type == EQUI_RECTANGULAR) {
      fprintf(fp, "    <equi_rectangular>\n");
      fprintf(fp, "      <central_meridian units=\"degrees\">%.4f"
	      "</central_meridian>\n", mp->param.eqr.central_meridian);
      fprintf(fp, "      <orig_latitude units=\"degrees\">%.4f"
	      "</orig_latitude>\n", mp->param.eqr.orig_latitude);
      fprintf(fp, "      <false_easting units=\"m\">%.3f</false_easting>\n",
	      mp->param.eqr.false_easting);
      fprintf(fp, "      <false_northing units=\"m\">%.3f</false_northing>\n",
	      mp->param.eqr.false_northing);
      fprintf(fp, "    </equi_rectangular>\n");
    }
    fprintf(fp,"  </projection>\n");
  }

  if (meta->transform) {
    meta_transform *mt = meta->transform;
    fprintf(fp, "  <transform>\n");
    fprintf(fp, "    <parameter_count>%d</parameter_count>\n",
	    mt->parameter_count);
    for (ii=0; ii<mt->parameter_count; ii++)
      fprintf(fp, "    <phi coefficient=\"%d\">%.11g</phi>\n", ii,
	      mt->y[ii]);
    for (ii=0; ii<mt->parameter_count; ii++)
      fprintf(fp, "    <lambda coefficient=\"%d\">%.11g</lambda>\n", ii,
	      mt->x[ii]);
    if (mt->parameter_count >= 25) {
      fprintf(fp, "    <origin_pixel>%.11g</origin_pixel>\n", 
	      mt->origin_pixel);
      fprintf(fp, "    <origin_line>%.11g</origin_line>\n", 
	      mt->origin_line);
    }
    for (ii=0; ii<mt->parameter_count; ii++)
      fprintf(fp, "    <i coefficient=\"%d\">%.11g</i>\n", ii, mt->s[ii]);
    for (ii=0; ii<mt->parameter_count; ii++)
      fprintf(fp, "    <j coeeficient=\"%d\">%.11g</j>\n", ii, mt->l[ii]);
    if (mt->parameter_count >= 25) {
      fprintf(fp, "    <origin_lat units=\"degrees\">%.4f</origin_lat>\n", 
	      mt->origin_lat);
      fprintf(fp, "    <origin_lon units=\"degrees\">%.4f</origin_lon>\n", 
	      mt->origin_lon);
    }
    for (ii=0; ii<10; ++ii)
      fprintf(fp, "    <map_a coefficient=\"%d\">%.11g</map_a>\n", ii,
	      mt->map2ls_a[ii]);
    for (ii=0; ii<10; ++ii)
      fprintf(fp, "    <map_b coefficient=\"%d\">%.11g</map_b>\n", ii,
	      mt->map2ls_b[ii]);
    fprintf(fp, "  </transform>\n");
  }

  if (meta->airsar) {
    meta_airsar *ma = meta->airsar;
    fprintf(fp, "  <airsar>\n");
    fprintf(fp, "    <scale_factor>%.11g</scale_factor>\n", 
	    ma->scale_factor);
    fprintf(fp, "    <gps_altitude units=\"m\">%.3f</gps_altitude>\n", 
	    ma->gps_altitude);
    fprintf(fp, "    <lat_peg_point units=\"degrees\">%.4f</lat_peg_point>"
	    "\n", ma->lat_peg_point);
    fprintf(fp, "    <lon_peg_point units=\"degrees\">%.4f</lon_peg_point>"
	    "\n", ma->lon_peg_point);
    fprintf(fp, "    <head_peg_point units=\"degrees\">%.4f"
	    "</head_peg_point>\n", ma->head_peg_point);
    fprintf(fp, "    <along_track_offset units=\"m\">%.3f"
	    "</along_track_offset>\n", ma->along_track_offset);
    fprintf(fp, "    <cross_track_offset units=\"m\">%.3f"
	    "</cross_track_offset>\n", ma->cross_track_offset);
    fprintf(fp, "  </airsar>\n");
  }

  if (meta->uavsar) {
    meta_uavsar *mu = meta->uavsar;
    fprintf(fp, "  <uavsar>\n");
    fprintf(fp, "    <id>%s</id>\n", mu->id);
    fprintf(fp, "    <scale_factor>%.11g</scale_factor>\n", 
	    mu->scale_factor);
    fprintf(fp, "    <gps_altitude units=\"m\">%.3f</gps_altitude>\n", 
	    mu->gps_altitude);
    fprintf(fp, "    <lat_peg_point units=\"degrees\">%.4f</lat_peg_point>"
	    "\n", mu->lat_peg_point);
    fprintf(fp, "    <lon_peg_point units=\"degrees\">%.4f</lon_peg_point>"
	    "\n", mu->lon_peg_point);
    fprintf(fp, "    <head_peg_point units=\"degrees\">%.4f"
	    "</head_peg_point>\n", mu->head_peg_point);
    fprintf(fp, "    <along_track_offset units=\"m\">%.4f"
	    "</along_track_offset>\n", mu->along_track_offset);
    fprintf(fp, "    <cross_track_offset units=\"m\">%.4f"
	    "</cross_track_offset>\n", mu->cross_track_offset);
    fprintf(fp, "  </uavsar>\n");
  }

  if (meta->dem) {
    meta_dem *md = meta->dem;
    fprintf(fp, "  <dem>\n");
    fprintf(fp, "    <source>%s</source>\n", md->source);
    fprintf(fp, "    <format>%s</format>\n", md->format);
    fprintf(fp, "    <tiles>%s</tiles>\n", md->tiles);
    fprintf(fp, "    <min_value>%.3f</min_value>\n", md->min_value);
    fprintf(fp, "    <max_value>%.3f</max_value>\n", md->max_value);
    fprintf(fp, "    <mean_value>%.3f</mean_value>\n", md->mean_value);
    fprintf(fp, "    <standard_deviation>%.3f</standard_deviation>\n", 
            md->standard_deviation);
    fprintf(fp, "    <unit_type>%s</unit_type>\n", md->unit_type);
    fprintf(fp, "    <no_data>%.4f</no_data>\n", md->no_data);
    fprintf(fp, "  </dem>\n");
  }

  if (meta->stats) {
    meta_statistics *ms = meta->stats;
    fprintf(fp, "  <statistics>\n");
    fprintf(fp, "    <band_count>%d</band_count>\n", ms->band_count);
    for (ii=0; ii<ms->band_count; ii++) {
      fprintf(fp, "    <band_stats num=\"%d\">\n", ii+1);
      fprintf(fp, "      <band_id>%s</band_id>\n", ms->band_stats[ii].band_id);
      fprintf(fp, "      <min>%.11g</min>\n", ms->band_stats[ii].min);
      fprintf(fp, "      <max>%.11g</max>\n", ms->band_stats[ii].max);
      fprintf(fp, "      <mean>%.11g</mean>\n", ms->band_stats[ii].mean);
      fprintf(fp, "      <rmse>%.11g</rmse>\n", ms->band_stats[ii].rmse);
      fprintf(fp, "      <std_deviation>%.11g</std_deviation>\n",
	      ms->band_stats[ii].std_deviation);
	    fprintf(fp, "      <percent_valid>%.11g</percent_valid>\n",
	      ms->band_stats[ii].percent_valid);
      fprintf(fp, "      <mask>%.11g</mask>\n", ms->band_stats[ii].mask);
      fprintf(fp, "    </band_stats>\n");
    }
    fprintf(fp, "  </statistics>\n");
  }
    
  if (meta->location) {
    meta_location *ml = meta->location;
    fprintf(fp, "  <location>\n");
    fprintf(fp, "    <lat_start_near_range>%.4f</lat_start_near_range>\n",
	    ml->lat_start_near_range);
    fprintf(fp, "    <lon_start_near_range>%.4f</lon_start_near_range>\n",
	    ml->lon_start_near_range);
    fprintf(fp, "    <lat_start_far_range>%.4f</lat_start_far_range>\n",
	    ml->lat_start_far_range);
    fprintf(fp, "    <lon_start_far_range>%.4f</lon_start_far_range>\n",
	    ml->lon_start_far_range);
    fprintf(fp, "    <lat_end_near_range>%.4f</lat_end_near_range>\n",
	    ml->lat_end_near_range);
    fprintf(fp, "    <lon_end_near_range>%.4f</lon_end_near_range>\n",
	    ml->lon_end_near_range);
    fprintf(fp, "    <lat_end_far_range>%.4f</lat_end_far_range>\n",
	    ml->lat_end_far_range);
    fprintf(fp, "    <lon_end_far_range>%.4f</lon_end_far_range>\n",
	    ml->lon_end_far_range);
    fprintf(fp, "  </location>\n");
  }

  if (meta->calibration) {
    meta_calibration *mc = meta->calibration;
    fprintf(fp,"  <calibration>\n");
    if (mc->type == asf_cal) {
      fprintf(fp, "    <type>ASF</type>\n");
      fprintf(fp, "    <a coefficient=\"0\">%.11g</a>\n", mc->asf->a0);
      fprintf(fp, "    <a coefficient=\"1\">%.11g</a>\n", mc->asf->a1);
      fprintf(fp, "    <a coefficient=\"2\">%.11g</a>\n", mc->asf->a2);
      fprintf(fp, "    <sample_count>%d</sample_count>\n",
	      mc->asf->sample_count);
      for (ii=0; ii<256; ii++)
	fprintf(fp, "    <noise element=\"%d\">%.11g</noise>\n", 
		ii, mc->asf->noise[ii]);
    }
    else if (mc->type == asf_scansar_cal) {
      fprintf(fp, "    <type>ASF_SCANSAR</type>\n");
      fprintf(fp, "    <a coefficient=\"0\">%.11g</a>\n", 
	      mc->asf_scansar->a0);
      fprintf(fp, "    <a coefficient=\"1\">%.11g</a>\n", 
	      mc->asf_scansar->a1);
      fprintf(fp, "    <a coefficient=\"2\">%.11g</a>\n",
	      mc->asf_scansar->a2);
      for (ii=0; ii<256; ii++)
	fprintf(fp, "    <noise element=\"%d\">%.11g</noise>\n", 
		ii, mc->asf_scansar->noise[ii]);
    }
    else if (mc->type == esa_cal) {
      fprintf(fp, "    <type>ESA</type>\n");
      fprintf(fp, "    <k>%.11g</k>\n", mc->esa->k);
      fprintf(fp, "    <ref_incid>%.11g</ref_incid>\n", mc->esa->ref_incid);
    }
    else if (mc->type == rsat_cal) {
      fprintf(fp, "    <type>RSAT</type>\n");
      fprintf(fp, "    <table_entries>%d</table_entries>\n", mc->rsat->n);
      for (ii=0; ii<mc->rsat->n; ii++)
	fprintf(fp, "    <lut coefficient=\"%d\">%.11g</lut>\n", 
		ii, mc->rsat->lut[ii]);
      fprintf(fp, "    <sample_inc>%d</sample_inc>\n", mc->rsat->samp_inc);
      fprintf(fp, "    <a3>%.11g</a3>\n", mc->rsat->a3);
      fprintf(fp, "    <slc>%d</slc>\n", mc->rsat->slc);
      fprintf(fp, "    <focus>%d</focus>\n", mc->rsat->focus);
    }
    else if (mc->type == alos_cal) {
      fprintf(fp, "    <type>ALOS</type>\n");
      if (!ISNAN(mc->alos->cf_hh))
	fprintf(fp, "    <cf_hh>%.11g</cf_hh>\n", mc->alos->cf_hh);
      if (!ISNAN(mc->alos->cf_hv))
	fprintf(fp, "    <cf_hv>%.11g</cf_hv>\n", mc->alos->cf_hv);
      if (!ISNAN(mc->alos->cf_vh))
	fprintf(fp, "    <cf_vh>%.11g</cf_vh>\n", mc->alos->cf_vh);
      if (!ISNAN(mc->alos->cf_vv))
	fprintf(fp, "    <cf_vv>%.11g</cf_vv>\n", mc->alos->cf_vv);
    }
    else if (mc->type == uavsar_cal) {
      fprintf(fp, "    <type>UAVSAR</type>\n");
      fprintf(fp, "    <semi_major units=\"m\">%.3f</semi_major>\n", 
	      mc->uavsar->semi_major);
      fprintf(fp, "    <slant_range_first_pixel units=\"m\">%.3f"
	      "</slant_range_first_pixel>\n", 
	      mc->uavsar->slant_range_first_pixel);
      fprintf(fp, "    <range_spacing units=\"m\">%.6f</range_spacing>\n", 
	      mc->uavsar->range_spacing);
      fprintf(fp, "    <azimuth_spacing units=\"m\">%.6f</azimuth_spacing>\n",
	      mc->uavsar->azimuth_spacing);
      fprintf(fp, "    <pitch units=\"degrees\">%.6f</pitch>\n", 
	      mc->uavsar->pitch);
      fprintf(fp, "    <steering_angle units=\"degrees\">%.6f</steering_angle>"
	      "\n", mc->uavsar->steering_angle);
      fprintf(fp, "    <altitude units=\"m\">%.3f</altitude>\n", 
	      mc->uavsar->altitude);
      fprintf(fp, "    <terrain_height units=\"m\">%.3f</terrain_height>\n",
	      mc->uavsar->terrain_height);
    }
    else if (mc->type == sentinel_cal) {
      fprintf(fp, "    <type>SENTINEL</type>\n");
      fprintf(fp, "    <noise_mean>%.6f</noise_mean>\n", 
        mc->sentinel->noise_mean);
    }
    fprintf(fp, "  </calibration>\n");
  }

  if (meta->colormap) {
    meta_colormap *mc = meta->colormap;
    fprintf(fp, "  <colormap>\n");
    fprintf(fp, "    <look_up_table>%s</look_up_table>\n", mc->look_up_table);
    fprintf(fp, "    <band_id>%s</band_id>\n", mc->band_id);
    fprintf(fp, "    <num_elements>%d</num_elements>\n", mc->num_elements);
    for (ii=0; ii<mc->num_elements; ii++)
      fprintf(fp, "    <idx element=\"%03d\">%03d  %03d  %03d</idx>\n",
	      ii, mc->rgb[ii].red, mc->rgb[ii].green, mc->rgb[ii].blue);
    fprintf(fp, "  </colormap>\n");
  }

  if (meta->insar) {
    meta_insar *mi = meta->insar;
    fprintf(fp, "  <insar>\n");
    fprintf(fp, "    <processor>%s</processor>\n", mi->processor);
    fprintf(fp, "    <master_image>%s</master_image>\n", mi->master_image);
    fprintf(fp, "    <slave_image>%s</slave_image>\n", mi->slave_image);
    fprintf(fp, "    <master_acquisition_date>%s</master_acquisition_date>\n",
	    mi->master_acquisition_date);
    fprintf(fp, "    <slave_acquisition_date>%s</slave_acquisition_date>\n",
	    mi->slave_acquisition_date);
    fprintf(fp, "    <center_look_angle units=\"degrees\">%.4f"
	    "</center_look_angle>\n", mi->center_look_angle);
    fprintf(fp, "    <doppler units=\"Hz\">%.11g</doppler>\n", mi->doppler);
    fprintf(fp, "    <doppler_rate units=\"Hz/m\">%.11g</doppler_rate>\n",
	    mi->doppler_rate);
    fprintf(fp, "    <baseline_length units=\"m\">%.1f</baseline_length>\n", 
	    mi->baseline_length);
    fprintf(fp, "    <baseline_parallel units=\"m\">%.1f</baseline_parallel>\n",
	    mi->baseline_parallel);
    fprintf(fp, "    <baseline_parallel_rate units=\"m/s\">%.11g"
	    "</baseline_parallel_rate>\n", mi->baseline_parallel_rate);
    fprintf(fp, "    <baseline_perpendicular units=\"m\">%.1f"
	    "</baseline_perpendicular>\n", mi->baseline_perpendicular);
    fprintf(fp, "    <baseline_perpendicular_rate units=\"m/s\">%.11g"
	    "</baseline_perpendicular_rate>\n", 
	    mi->baseline_perpendicular_rate);
    fprintf(fp, "    <baseline_temporal units=\"days\">%d</baseline_temporal>"
	    "\n", mi->baseline_temporal);
    fprintf(fp, "    <baseline_critical units=\"m\">%.1f</baseline_critical>"
	    "\n", mi->baseline_critical);
    fprintf(fp, "  </insar>\n");
  }
  
  if (meta->quality) {
  	meta_quality *mq = meta->quality;
  	fprintf(fp, "  <quality>\n");
  	fprintf(fp, "    <bit_error_rate>%g</bit_error_rate>\n", 
  		mq->bit_error_rate);
		fprintf(fp, "    <azimuth_resolution units=\"m\">%g</azimuth_resolution>"
			"\n", mq->azimuth_resolution);
		fprintf(fp, "    <range_resolution units=\"m\">%g</range_resolution>\n",
			mq->range_resolution);
		fprintf(fp, "    <signal_to_noise_ratio units=\"dB\">%g</signal_to_noise_ratio>\n",
			mq->signal_to_noise_ratio);
		fprintf(fp, "    <peak_sidelobe_ratio units=\"dB\">%g</peak_sidelobe_ratio>\n",
			mq->peak_sidelobe_ratio);
		fprintf(fp, "    <integrated_sidelobe_ratio units=\"dB\">%g</integrated_sidelobe_ratio>"
			"\n", mq->integrated_sidelobe_ratio);
  	fprintf(fp, "  </quality>\n");
  }

  fprintf(fp, "</metadata>\n");
  FCLOSE(fp);
}
