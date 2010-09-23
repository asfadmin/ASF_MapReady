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
#include <hdf5.h>
#include <netcdf.h>

#define RES 16
#define MAX_PTS 256

struct double_meters {
  double value;
  char *units;
};

struct double_seconds {
  double value;
  char *units;
};

struct double_latitude {
  double value;
  char *units;
};

struct double_longitude {
  double value;
  char *units;
};

struct double_hertz {
  double value;
  char *units;
};

struct double_hertz_pixel {
  double value;
  char *units;
};

struct double_hertz_pixel2 {
  double value;
  char *units;
};

struct vector_time {
  int number;
  double time;
  char *units;
};

struct position {
  int number;
  double x;
  double y;
  double z;
  char *units;
};

struct velocity {
  int number;
  double x;
  double y;
  double z;
  char *units;
};

netcdf_t *initialize_netcdf_file(const char *output_file, 
				 meta_parameters *meta)
{
  int ii, status, ncid, var_id;
  int dim_xgrid_id, dim_ygrid_id, dim_lat_id, dim_lon_id, dim_time_id;
  char *spatial_ref=NULL, *datum=NULL, *spheroid=NULL;

  // Assign parameters
  int projected = FALSE;
  int band_count = meta->general->band_count;
  int variable_count = band_count + 3;
  if (meta->projection && meta->projection->type != SCANSAR_PROJECTION) {
    projected = TRUE;
    variable_count += 2;
  }
  size_t line_count = meta->general->line_count;
  size_t sample_count = meta->general->sample_count;

  // Assign data type
  nc_type datatype;
  if (meta->general->data_type == BYTE)
    datatype = NC_CHAR;
  else if (meta->general->data_type == REAL32)
    datatype = NC_FLOAT;

  // Initialize the netCDF pointer structure
  netcdf_t *netcdf = (netcdf_t *) MALLOC(sizeof(netcdf_t));
  netcdf->var_count = variable_count;
  netcdf->var_id = (int *) MALLOC(sizeof(int)*variable_count);

  // Create the actual file
  //status = nc_create(output_file, NC_CLOBBER|NC_NETCDF4|NC_CLASSIC_MODEL, 
  //	     &ncid);
  status = nc_create(output_file, NC_CLOBBER|NC_NETCDF4, &ncid);
  netcdf->ncid = ncid;
  if (status != NC_NOERR)
    asfPrintError("Could not open netCDF file (%s).\n", nc_strerror(status));

  // Define compound data type for metadata values with units
  nc_type met_id;
  nc_def_compound(ncid, sizeof(struct double_meters), "value_met", &met_id);
  nc_insert_compound(ncid, met_id, "value", 
		     NC_COMPOUND_OFFSET(struct double_meters, value), 
		     NC_DOUBLE);
  nc_insert_compound(ncid, met_id, "units", 
		     NC_COMPOUND_OFFSET(struct double_meters, units), 
		     NC_STRING);
  struct double_meters metCom[1];
  metCom[0].units = (char *) MALLOC(sizeof(char)*25);
  strcpy(metCom[0].units, "meters");

  nc_type sec_id;
  nc_def_compound(ncid, sizeof(struct double_seconds), "value_sec", &sec_id);
  nc_insert_compound(ncid, sec_id, "value", 
		     NC_COMPOUND_OFFSET(struct double_seconds, value), 
		     NC_DOUBLE);
  nc_insert_compound(ncid, sec_id, "units", 
		     NC_COMPOUND_OFFSET(struct double_seconds, units), 
		     NC_STRING);
  struct double_seconds secCom[1];
  secCom[0].units = (char *) MALLOC(sizeof(char)*25);
  strcpy(secCom[0].units, "seconds");

  nc_type lat_id;
  nc_def_compound(ncid, sizeof(struct double_latitude), "value_lat", &lat_id);
  nc_insert_compound(ncid, lat_id, "value", 
		     NC_COMPOUND_OFFSET(struct double_latitude, value), 
		     NC_DOUBLE);
  nc_insert_compound(ncid, lat_id, "units", 
		     NC_COMPOUND_OFFSET(struct double_latitude, units), 
		     NC_STRING);
  struct double_latitude latCom[1];
  latCom[0].units = (char *) MALLOC(sizeof(char)*25);
  strcpy(latCom[0].units, "degrees_north");

  nc_type lon_id;
  nc_def_compound(ncid, sizeof(struct double_longitude), "value_lon", &lon_id);
  nc_insert_compound(ncid, lon_id, "value", 
		     NC_COMPOUND_OFFSET(struct double_longitude, value), 
		     NC_DOUBLE);
  nc_insert_compound(ncid, lon_id, "units", 
		     NC_COMPOUND_OFFSET(struct double_longitude, units), 
		     NC_STRING);
  struct double_seconds lonCom[1];
  lonCom[0].units = (char *) MALLOC(sizeof(char)*25);
  strcpy(lonCom[0].units, "degrees_east");

  nc_type hz_id;
  nc_def_compound(ncid, sizeof(struct double_hertz), "value_hz", &hz_id);
  nc_insert_compound(ncid, hz_id, "value", 
		     NC_COMPOUND_OFFSET(struct double_hertz, value), 
		     NC_DOUBLE);
  nc_insert_compound(ncid, hz_id, "units", 
		     NC_COMPOUND_OFFSET(struct double_hertz, units), 
		     NC_STRING);
  struct double_hertz hzCom[1];
  hzCom[0].units = (char *) MALLOC(sizeof(char)*25);
  strcpy(hzCom[0].units, "hertz");

  nc_type hz2_id;
  nc_def_compound(ncid, sizeof(struct double_hertz_pixel), "value_hz_pix", 
		  &hz2_id);
  nc_insert_compound(ncid, hz2_id, "value", 
		     NC_COMPOUND_OFFSET(struct double_hertz_pixel, value), 
		     NC_DOUBLE);
  nc_insert_compound(ncid, hz2_id, "units", 
		     NC_COMPOUND_OFFSET(struct double_hertz_pixel, units), 
		     NC_STRING);
  struct double_hertz_pixel hz2Com[1];
  hz2Com[0].units = (char *) MALLOC(sizeof(char)*25);
  strcpy(hz2Com[0].units, "hertz per pixel");

  nc_type hz3_id;
  nc_def_compound(ncid, sizeof(struct double_hertz_pixel2), "value_hz_pix2", 
		  &hz3_id);
  nc_insert_compound(ncid, hz3_id, "value", 
		     NC_COMPOUND_OFFSET(struct double_hertz_pixel2, value), 
		     NC_DOUBLE);
  nc_insert_compound(ncid, hz3_id, "units", 
		     NC_COMPOUND_OFFSET(struct double_hertz_pixel2, units), 
		     NC_STRING);
  struct double_hertz_pixel2 hz3Com[1];
  hz3Com[0].units = (char *) MALLOC(sizeof(char)*25);
  strcpy(hz3Com[0].units, "hertz per square pixel");

  int vector_count;
  if (meta->state_vectors)
    vector_count = meta->state_vectors->vector_count;
  else
    vector_count = 1;
  nc_type vec_time_id, pos_id, vel_id;
  struct vector_time vecTimeCom[vector_count];
  struct position posCom[vector_count];
  struct position velCom[vector_count];
  if (meta->state_vectors) {
    nc_def_compound(ncid, sizeof(struct vector_time), "vector_time", 
		    &vec_time_id);
    nc_insert_compound(ncid, vec_time_id, "number",
		       NC_COMPOUND_OFFSET(struct vector_time, number), NC_INT);
    nc_insert_compound(ncid, vec_time_id, "time", 
		       NC_COMPOUND_OFFSET(struct vector_time, time), 
		       NC_DOUBLE);
    nc_insert_compound(ncid, vec_time_id, "units", 
		       NC_COMPOUND_OFFSET(struct vector_time, units), 
		       NC_STRING);
    for (ii=0; ii<vector_count; ii++) {
      vecTimeCom[ii].units = (char *) MALLOC(sizeof(char)*25);
      strcpy(vecTimeCom[ii].units, "seconds");
    }
    
    nc_def_compound(ncid, sizeof(struct position), "position", &pos_id);
    nc_insert_compound(ncid, pos_id, "number",
		       NC_COMPOUND_OFFSET(struct position, number), NC_INT);
    nc_insert_compound(ncid, pos_id, "x", 
		       NC_COMPOUND_OFFSET(struct position, x), NC_DOUBLE);
    nc_insert_compound(ncid, pos_id, "y", 
		       NC_COMPOUND_OFFSET(struct position, y), NC_DOUBLE);
    nc_insert_compound(ncid, pos_id, "z", 
		       NC_COMPOUND_OFFSET(struct position, z), NC_DOUBLE);
    nc_insert_compound(ncid, pos_id, "units", 
		       NC_COMPOUND_OFFSET(struct position, units), NC_STRING);
    for (ii=0; ii<meta->state_vectors->vector_count; ii++) {
      posCom[ii].units = (char *) MALLOC(sizeof(char)*25);
      strcpy(posCom[ii].units, "meters");
    }
    
    nc_def_compound(ncid, sizeof(struct velocity), "velocity", &vel_id);
    nc_insert_compound(ncid, vel_id, "number",
		       NC_COMPOUND_OFFSET(struct position, number), NC_INT);
    nc_insert_compound(ncid, vel_id, "x", 
		       NC_COMPOUND_OFFSET(struct position, x), NC_DOUBLE);
    nc_insert_compound(ncid, vel_id, "y", 
		       NC_COMPOUND_OFFSET(struct position, y), NC_DOUBLE);
    nc_insert_compound(ncid, vel_id, "z", 
		       NC_COMPOUND_OFFSET(struct position, z), NC_DOUBLE);
    nc_insert_compound(ncid, vel_id, "units", 
		       NC_COMPOUND_OFFSET(struct position, units), NC_STRING);
    for (ii=0; ii<meta->state_vectors->vector_count; ii++) {
      velCom[ii].units = (char *) MALLOC(sizeof(char)*30);
      strcpy(velCom[ii].units, "meters per second");
    }
  }

  // Define dimensions
  if (projected) {
    nc_def_dim(ncid, "xgrid", sample_count, &dim_xgrid_id);
    nc_def_dim(ncid, "ygrid", line_count, &dim_ygrid_id);
  }
  else {
    nc_def_dim(ncid, "longitude", sample_count, &dim_lon_id);
    nc_def_dim(ncid, "latitude", line_count, &dim_lat_id);
  }
  nc_def_dim(ncid, "time", 1, &dim_time_id);

  // Define projection
  char *str = (char *) MALLOC(sizeof(char)*1024);
  float *fValue = (float *) MALLOC(sizeof(float));
  if (projected) {
    nc_def_var(ncid, "projection", NC_CHAR, 0, 0, &var_id);
    if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {

      meta_projection *mp = meta->projection;
      strcpy(str, "transverse_mercator");
      nc_put_att_text(ncid, var_id, "grid_mapping_name", strlen(str), str);
      *fValue = (float) mp->param.utm.scale_factor;
      nc_put_att_float(ncid, var_id, "scale_factor_at_central_meridian", 
		       NC_FLOAT, 1, fValue);
      *fValue = (float) mp->param.utm.lon0;
      nc_put_att_float(ncid, var_id, "longitude_of_central_meridian",
		       NC_FLOAT, 1, fValue);
      *fValue = (float) mp->param.utm.lat0;
      nc_put_att_float(ncid, var_id, "latitude_of_projection_origin",
		       NC_FLOAT, 1, fValue);
      *fValue = (float) mp->param.utm.false_easting;
      nc_put_att_float(ncid, var_id, "false_easting", NC_FLOAT, 1, fValue);
      *fValue = (float) mp->param.utm.false_northing;
      nc_put_att_float(ncid, var_id, "false_northing", NC_FLOAT, 1, fValue);
      strcpy(str, "xgrid");
      nc_put_att_text(ncid, var_id, "projection_x_coordinate", strlen(str), 
		      str);
      strcpy(str, "ygrid");
      nc_put_att_text(ncid, var_id, "projection_y_coordinate", strlen(str), 
		      str);
      strcpy(str, "meters");
      nc_put_att_text(ncid, var_id, "units", strlen(str), str); 
      *fValue = (float) mp->startY;
      nc_put_att_float(ncid, var_id, "grid_boundary_top_projected_y",
		       NC_FLOAT, 1, fValue);
      *fValue = (float) (mp->startY + meta->general->line_count * mp->perY);
      nc_put_att_float(ncid, var_id, "grid_boundary_bottom_projected_y",
		       NC_FLOAT, 1, fValue);
      *fValue = (float) (mp->startX + meta->general->sample_count * mp->perX);
      nc_put_att_float(ncid, var_id, "grid_boundary_right_projected_x",
		       NC_FLOAT, 1, fValue);
      *fValue = (float) mp->startX;
      nc_put_att_float(ncid, var_id, "grid_boundary_left_projected_x",
		       NC_FLOAT, 1, fValue);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) MALLOC(sizeof(char)*25);
      if (mp->datum == WGS84_DATUM)
	strcpy(datum, "WGS_1984");
      else
	strcpy(datum, "");
      spheroid = (char *) MALLOC(sizeof(char)*25);
      if (mp->spheroid == WGS84_SPHEROID)
	strcpy(spheroid, "WGS_1984");
      else
	strcpy(spheroid, "");
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"%s_UTM_Zone_%d%c\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.1lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",%.1lf],PARAMETER[\"False_Northing\",%.1lf],PARAMETER[\"Central_Meridian\",%.1lf],PARAMETER[\"Scale_Factor\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.1lf],UNIT[\"Meter\",1]]",
	      spheroid, mp->param.utm.zone, mp->hem, spheroid, datum, 
	      spheroid, mp->re_major, flat, mp->param.utm.false_easting, 
	      mp->param.utm.false_northing, mp->param.utm.lon0, 
	      mp->param.utm.scale_factor, mp->param.utm.lat0);
      status = nc_put_att_text(ncid, var_id, "spatial_ref", 
			       strlen(spatial_ref), spatial_ref);
      sprintf(str, "+proj=utm +zone=%d", mp->param.utm.zone);
      if (meta->general->center_latitude < 0)
	strcat(str, " +south");
      nc_put_att_text(ncid, var_id, "proj4text", strlen(str), str);
      status = nc_put_att_int(ncid, var_id, "zone", NC_INT, 1, 
			      &mp->param.utm.zone);
      *fValue = (float) mp->re_major;
      nc_put_att_float(ncid, var_id, "semimajor_radius", NC_FLOAT, 1, fValue);
      *fValue = (float) mp->re_minor;
      nc_put_att_float(ncid, var_id, "semiminor_radius", NC_FLOAT, 1, fValue);
      sprintf(str, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      nc_put_att_text(ncid, var_id, "GeoTransform", strlen(str), str);
    }
    else if (meta->projection->type == POLAR_STEREOGRAPHIC) {
      /*
    projection:grid_mapping_name = "polar_stereographic" ;
    projection:straight_vertical_longitude_from_pole = 135. ;
    projection:latitude_of_projection_origin = 90. ;
    projection:standard_parallel = 70. ;
    projection:false_easting = 0. ;
    projection:false_northing = 0. ;
    projection:projection_x_coordinate = "xgrid" ;
    projection:projection_y_coordinate = "ygrid" ;
    projection:units = "m" ;
    projection:grid_boundary_top_projected_y = 3408500. ;
    projection:grid_boundary_bottom_projected_y = -3400000. ;
    projection:grid_boundary_right_projected_x = 3408500. ;
    projection:grid_boundary_left_projected_x = -3400000. ;
    projection:spatial_ref = "PROJCS[\"Stereographic_North_Pole\",GEOGCS[\"unnamed ellipse\",DATUM[\"D_unknown\",SPHEROID[\"Unknown\",6378273.000,298.7802446]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0002247191011236]],PROJECTION[\"Stereographic_North_Pole\"],PARAMETER[\"standard_parallel_1\",   70.00],PARAMETER[\"central_meridian\",  -45.00],PARAMETER[\"scale_factor\", 1],PARAMETER[\"false_easting\", 0],PARAMETER[\"false_northing\", 0],UNIT[\"Meter\",1, AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"3411\"]]" ;
    projection:proj4text = "+proj=stere +lat_0=90.000000 +lat_ts=70.000000 +lon_0=-45.000000 +k=1 +x_0=0 +y_0=0 +a=6378273.000 +b=6356889.449 +units=m +no_defs" ;
    projection:latitude_of_true_scale = 70. ;
    projection:longitude_of_projection_origin = -45. ;
    projection:scaling_factor = 1. ;
    projection:semimajor_radius = 6378273. ;
    projection:semiminor_radius = 6356889.44856411 ;
    projection:GeoTransform = "-3400000.000000 4450.000000 0 3408500.000000 0 -4450.000000" ; 
      */
    }
    else if (meta->projection->type == ALBERS_EQUAL_AREA) {
      /*
	grid_mapping_name = albers_conical_equal_area
	standard_parallel
	longitude_of_central_meridian
	latitude_of_projection_origin
	false_easting
	false_northing
      */
    }
    else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC) {
      /*
	grid_mapping_name = lambert_conformal_conic
	standard_parallel
	longitude_of_central_meridian
	latitude_of_projection_origin
	false_easting
	false_northing
      */
    }
    else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
      /*
	grid_mapping_name = lambert_azimuthal_equal_area
	longitude_of_projection_origin
	latitude_of_projection_origin
	false_easting
	false_northing
      */
    }
  }
  
  // Define variables and data attributes
  char **band_name = extract_band_names(meta->general->bands, band_count);
  int dims_bands[3];
  dims_bands[0] = dim_time_id;
  if (projected) {
    dims_bands[1] = dim_ygrid_id;
    dims_bands[2] = dim_xgrid_id;
  }
  else {
    dims_bands[1] = dim_lon_id;
    dims_bands[2] = dim_lat_id;
  }
  for (ii=0; ii<band_count; ii++) {
    
    nc_def_var(ncid, band_name[ii], datatype, 3, dims_bands, &var_id);
    netcdf->var_id[ii] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
    *fValue = -999.0;
    nc_put_att_float(ncid, var_id, "_FillValue", NC_FLOAT, 1, fValue);
    sprintf(str, "%s", meta->general->sensor);
    if (meta->general->image_data_type < 9)
      strcat(str, " radar backscatter");
    if (meta->general->radiometry >= r_SIGMA_DB &&
	meta->general->radiometry <= r_GAMMA_DB)
      strcat(str, " in dB");
    nc_put_att_text(ncid, var_id, "long_name", strlen(str), str);
    strcpy(str, "area: backcatter value");
    nc_put_att_text(ncid, var_id, "cell_methods", strlen(str), str);
    strcpy(str, "1");
    nc_put_att_text(ncid, var_id, "units", strlen(str), str);
    strcpy(str, "unitless normalized radar cross-section");
    if (meta->general->radiometry >= r_SIGMA &&
	meta->general->radiometry <= r_GAMMA)
      strcat(str, " stored as powerscale");
    else if (meta->general->radiometry >= r_SIGMA_DB &&
	     meta->general->radiometry <= r_GAMMA_DB)
      strcat(str, " stored as dB=10*log10(*)");
    nc_put_att_text(ncid, var_id, "units_description", strlen(str), str);
    strcpy(str, "longitude latitude");
    nc_put_att_text(ncid, var_id, "coordinates", strlen(str), str);
    if (projected) {
      strcpy(str, "projection");
      nc_put_att_text(ncid, var_id, "grid_mapping", strlen(str), str);
    } 
  }
  // Define other attributes
  ymd_date ymd;
  hms_time hms;
  parse_date(meta->general->acquisition_date, &ymd, &hms);

  // Time
  ii = band_count;
  int dims_time[1] = { dim_time_id };
  nc_def_var(ncid, "time", NC_FLOAT, 1, dims_time, &var_id);
  netcdf->var_id[ii] = var_id;
  strcpy(str, "seconds since 1900-01-01T00:00:00Z");
  nc_put_att_text(ncid, var_id, "units", strlen(str), str);
  strcpy(str, "scene center time");
  nc_put_att_text(ncid, var_id, "references", strlen(str), str);
  strcpy(str, "time");
  nc_put_att_text(ncid, var_id, "standard_name", strlen(str), str);
  strcpy(str, "T");
  nc_put_att_text(ncid, var_id, "axis", strlen(str), str);
  strcpy(str, "serial date");
  nc_put_att_text(ncid, var_id, "long_name", strlen(str), str);
  
  // Longitude
  ii++;
  if (projected) {
    int dims_lon[2] = { dim_ygrid_id, dim_xgrid_id };
    nc_def_var(ncid, "longitude", NC_FLOAT, 2, dims_lon, &var_id);
  }
  else {
    int dims_lon[2] = { dim_lon_id, dim_lat_id };
    nc_def_var(ncid, "longitude", NC_FLOAT, 2, dims_lon, &var_id);
  }
  netcdf->var_id[ii] = var_id;
  nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
  strcpy(str, "longitude");
  nc_put_att_text(ncid, var_id, "standard_name", strlen(str), str);
  strcpy(str, "longitude");
  nc_put_att_text(ncid, var_id, "long_name", strlen(str), str);
  strcpy(str, "degrees_east");
  nc_put_att_text(ncid, var_id, "units", strlen(str), str);
  float *valid_range = (float *) MALLOC(sizeof(float)*2);
  valid_range[0] = -180.0;
  valid_range[1] = 180.0;
  nc_put_att_float(ncid, var_id, "valid_range", NC_FLOAT, 2, valid_range);
  FREE(valid_range);
  *fValue = -999.0;
  nc_put_att_float(ncid, var_id, "_FillValue", NC_FLOAT, 1, fValue);

  // Latitude
  ii++;
  if (projected) {
    int dims_lat[2] = { dim_ygrid_id, dim_xgrid_id };
    nc_def_var(ncid, "latitude", NC_FLOAT, 2, dims_lat, &var_id);
  }
  else {
    int dims_lat[2] = { dim_lat_id, dim_lon_id };
    nc_def_var(ncid, "latitude", NC_FLOAT, 2, dims_lat, &var_id);
  }
  netcdf->var_id[ii] = var_id;
  nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
  strcpy(str, "latitude");
  nc_put_att_text(ncid, var_id, "standard_name", strlen(str), str);
  strcpy(str, "latitude");
  nc_put_att_text(ncid, var_id, "long_name", strlen(str), str);
  strcpy(str, "degrees_north");
  nc_put_att_text(ncid, var_id, "units", strlen(str), str);
  valid_range = (float *) MALLOC(sizeof(float)*2);
  valid_range[0] = -90.0;
  valid_range[1] = 90.0;
  nc_put_att_float(ncid, var_id, "valid_range", NC_FLOAT, 2, valid_range);
  FREE(valid_range);
  *fValue = -999.0;
  nc_put_att_float(ncid, var_id, "_FillValue", NC_FLOAT, 1, fValue);

  if (projected) {

    // ygrid
    ii++;
    int dims_ygrid[1] = { dim_ygrid_id };
    nc_def_var(ncid, "ygrid", NC_FLOAT, 1, dims_ygrid, &var_id);
    netcdf->var_id[ii] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
    strcpy(str, "projection_y_coordinates");
    nc_put_att_text(ncid, var_id, "standard_name", strlen(str), str);
    strcpy(str, "projection_grid_y_centers");
    nc_put_att_text(ncid, var_id, "long_name", strlen(str), str);
    strcpy(str, "meters");
    nc_put_att_text(ncid, var_id, "units", strlen(str), str);
    strcpy(str, "Y");
    nc_put_att_text(ncid, var_id, "axis", strlen(str), str);

    // xgrid
    ii++;
    int dims_xgrid[1] = { dim_xgrid_id };
    nc_def_var(ncid, "xgrid", NC_FLOAT, 1, dims_xgrid, &var_id);
    netcdf->var_id[ii] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
    strcpy(str, "projection_x_coordinates");
    nc_put_att_text(ncid, var_id, "standard_name", strlen(str), str);
    strcpy(str, "projection_grid_x_centers");
    nc_put_att_text(ncid, var_id, "long_name", strlen(str), str);
    strcpy(str, "meters");
    nc_put_att_text(ncid, var_id, "units", strlen(str), str);
    strcpy(str, "X");
    nc_put_att_text(ncid, var_id, "axis", strlen(str), str);
  }
  
  // Define global attributes
  strcpy(str, "CF-1.4");
  nc_put_att_text(ncid, NC_GLOBAL, "Conventions", strlen(str), str);
  strcpy(str, "Alaska Satellite Facility");
  nc_put_att_text(ncid, NC_GLOBAL, "institution", strlen(str), str);
  sprintf(str, "%s %s %s image", meta->general->sensor, 
	  meta->general->sensor_name, meta->general->mode);
  nc_put_att_text(ncid, NC_GLOBAL, "title", strlen(str), str);
  if (meta->general->image_data_type == AMPLITUDE_IMAGE)
    strcpy(str, "SAR backcatter image");
  nc_put_att_text(ncid, NC_GLOBAL, "source", strlen(str), str);
  sprintf(str, "%s", meta->general->basename);
  nc_put_att_text(ncid, NC_GLOBAL, "original_file", strlen(str), str);
  if (strcmp_case(meta->general->sensor, "RSAT-1") == 0)
    sprintf(str, "Copyright Canadian Space Agency, %d", ymd.year);
  else if (strncmp_case(meta->general->sensor, "ERS", 3) == 0)
    sprintf(str, "Copyright European Space Agency, %d", ymd.year);
  else if (strcmp_case(meta->general->sensor, "JERS-1") == 0 ||
	   strcmp_case(meta->general->sensor, "ALOS") == 0)
    sprintf(str, "Copyright Japan Aerospace Exploration Agency , %d", 
	    ymd.year);
  nc_put_att_text(ncid, NC_GLOBAL, "comment", strlen(str), str);
  strcpy(str, "Documentation available at: www.asf.alaska.edu");
  nc_put_att_text(ncid, NC_GLOBAL, "references", strlen(str), str);
  time_t t;
  struct tm *timeinfo;
  time(&t);
  timeinfo = gmtime(&t);
  sprintf(str, "%s", asctime(timeinfo));
  chomp(str);
  strcat(str, ", UTC: netCDF File created.");
  nc_put_att_text(ncid, NC_GLOBAL, "history", strlen(str), str);
  
  // Metadata - general block
  status = nc_def_var(ncid, "overview", NC_CHAR, 0, 0, &var_id);
  strcpy(str, meta->general->basename);
  nc_put_att_text(ncid, var_id, "name", strlen(str), str);
  strcpy(str, meta->general->sensor);
  nc_put_att_text(ncid, var_id, "sensor", strlen(str), str);
  strcpy(str, meta->general->sensor_name);
  nc_put_att_text(ncid, var_id, "sensor_name", strlen(str), str);
  strcpy(str, meta->general->mode);
  nc_put_att_text(ncid, var_id, "mode", strlen(str), str);
  strcpy(str, meta->general->processor);
  nc_put_att_text(ncid, var_id, "processor", strlen(str), str);
  strcpy(str, data_type2str(meta->general->data_type));
  nc_put_att_text(ncid, var_id, "data_type", strlen(str), str);
  strcpy(str, image_data_type2str(meta->general->image_data_type));
  nc_put_att_text(ncid, var_id, "image_data_type", strlen(str), str);
  strcpy(str, radiometry2str(meta->general->radiometry));
  nc_put_att_text(ncid, var_id, "radiometry", strlen(str), str);
  strcpy(str, meta->general->acquisition_date);
  nc_put_att_text(ncid, var_id, "acquisition_date", strlen(str), str);
  nc_put_att_int(ncid, var_id, "orbit", NC_INT, 1, &meta->general->orbit);
  if (meta->general->orbit_direction == 'A')
    strcpy(str, "Ascending");
  else
    strcpy(str, "Descending");
  nc_put_att_text(ncid, var_id, "orbit_direction", strlen(str), str);
  nc_put_att_int(ncid, var_id, "frame", NC_INT, 1, &meta->general->frame);
  nc_put_att_int(ncid, var_id, "band_count", NC_INT, 1, 
		 &meta->general->band_count);
  strcpy(str, meta->general->bands);
  nc_put_att_text(ncid, var_id, "bands", strlen(str), str);
  nc_put_att_int(ncid, var_id, "line_count", NC_INT, 1, 
		 &meta->general->line_count);
  nc_put_att_int(ncid, var_id, "sample_count", NC_INT, 1, 
		 &meta->general->sample_count);
  nc_put_att_int(ncid, var_id, "start_line", NC_INT, 1, 
		 &meta->general->start_line);
  nc_put_att_int(ncid, var_id, "start_sample", NC_INT, 1, 
		 &meta->general->start_sample);
  metCom[0].value = meta->general->x_pixel_size;
  nc_put_att(ncid, var_id, "x_pixel_size", met_id, 1, metCom);
  metCom[0].value = meta->general->y_pixel_size;
  nc_put_att(ncid, var_id, "y_pixel_size", met_id, 1, metCom);
  latCom[0].value = meta->general->center_latitude;
  nc_put_att(ncid, var_id, "center_latitude", lat_id, 1, latCom);
  lonCom[0].value = meta->general->center_longitude;
  nc_put_att(ncid, var_id, "center_longitude", lon_id, 1, lonCom);
  metCom[0].value = meta->general->re_major;
  nc_put_att(ncid, var_id, "re_major", met_id, 1, metCom);
  metCom[0].value = meta->general->re_minor;
  nc_put_att(ncid, var_id, "re_minor", met_id, 1, metCom);
  nc_put_att_double(ncid, var_id, "bit_error_rate", NC_DOUBLE, 1, 
		    &meta->general->bit_error_rate);
  nc_put_att_int(ncid, var_id, "missing_lines", NC_INT, 1, 
		 &meta->general->missing_lines);
  nc_put_att_float(ncid, var_id, "no_data", NC_FLOAT, 1, 
		   &meta->general->no_data);
  if (meta->sar) {
    // Metadata - SAR block
    nc_def_var(ncid, "SAR", NC_CHAR, 0, 0, &var_id);
    if (meta->sar->image_type == 'S')
      strcpy(str, "slant range");
    else if (meta->sar->image_type == 'G')
      strcpy(str, "ground range");
    else if (meta->sar->image_type == 'P')
      strcpy(str, "projected");
    else if (meta->sar->image_type == 'R')
      strcpy(str, "georeferenced");
    nc_put_att_text(ncid, var_id, "image_type", strlen(str), str);
    if (meta->sar->look_direction == 'R')
      strcpy(str, "right");
    else if (meta->sar->look_direction == 'L')
      strcpy(str, "left");
    nc_put_att_text(ncid, var_id, "look_direction", strlen(str), str);
    nc_put_att_int(ncid, var_id, "look_count", NC_INT, 1, 
		   &meta->sar->look_count);
    nc_put_att_int(ncid, var_id, "multilook", NC_INT, 1, 
		   &meta->sar->multilook);
    nc_put_att_int(ncid, var_id, "deskewed", NC_INT, 1, &meta->sar->deskewed);
    nc_put_att_int(ncid, var_id, "original_line_count", NC_INT, 1, 
		   &meta->sar->original_line_count);
    nc_put_att_int(ncid, var_id, "original_sample_count", NC_INT, 1, 
		   &meta->sar->original_sample_count);
    nc_put_att_double(ncid, var_id, "line_increment", NC_DOUBLE, 1, 
		      &meta->sar->line_increment);
    nc_put_att_double(ncid, var_id, "sample_increment", NC_DOUBLE, 1, 
		      &meta->sar->sample_increment);
    secCom[0].value = meta->sar->range_time_per_pixel;
    nc_put_att(ncid, var_id, "range_time_per_pixel", sec_id, 1, secCom);
    secCom[0].value = meta->sar->azimuth_time_per_pixel;
    nc_put_att(ncid, var_id, "azimuth_time_per_pixel", sec_id, 1, secCom);
    metCom[0].value = meta->sar->slant_range_first_pixel;
    nc_put_att(ncid, var_id, "slant_range_first_pixel", met_id, 1, metCom);
    metCom[0].value = meta->sar->slant_shift;
    nc_put_att(ncid, var_id, "slant_shift", met_id, 1, metCom);
    secCom[0].value = meta->sar->time_shift;
    nc_put_att(ncid, var_id, "time_shift", sec_id, 1, secCom);
    metCom[0].value = meta->sar->wavelength;
    nc_put_att(ncid, var_id, "wavelength", met_id, 1, metCom);
    hzCom[0].value = meta->sar->prf;
    nc_put_att(ncid, var_id, "pulse_repetition_frequency", hz_id, 1, hzCom);
    metCom[0].value = meta->sar->earth_radius;
    nc_put_att(ncid, var_id, "earth_radius", met_id, 1, metCom);
    metCom[0].value = meta->sar->satellite_height;
    nc_put_att(ncid, var_id, "satellite_height", met_id, 1, metCom);
    hzCom[0].value = meta->sar->range_doppler_coefficients[0];
    nc_put_att(ncid, var_id, "range_doppler_centroid", hz_id, 1, hzCom);
    hz2Com[0].value = meta->sar->range_doppler_coefficients[1];
    nc_put_att(ncid, var_id, "range_doppler_linear", hz2_id, 1, hz2Com);
    hz3Com[0].value = meta->sar->range_doppler_coefficients[2];
    nc_put_att(ncid, var_id, "range_doppler_quadratic", hz3_id, 1, hz3Com);
    hzCom[0].value = meta->sar->azimuth_doppler_coefficients[0];
    nc_put_att(ncid, var_id, "azimuth_doppler_centroid", hz_id, 1, hzCom);
    hz2Com[0].value = meta->sar->azimuth_doppler_coefficients[1];
    nc_put_att(ncid, var_id, "azimuth_doppler_linear", hz2_id, 1, hz2Com);
    hz3Com[0].value = meta->sar->azimuth_doppler_coefficients[2];
    nc_put_att(ncid, var_id, "azimuth_doppler_quadratic", hz3_id, 1, hz3Com);
  }

  if (meta->state_vectors) {
    // Metadata - state vector block
    nc_def_var(ncid, "orbit", NC_CHAR, 0, 0, &var_id);
    nc_put_att_int(ncid, var_id, "year", NC_INT, 1, 
		   &meta->state_vectors->year);
    nc_put_att_int(ncid, var_id, "day_of_year", NC_INT, 1, 
		   &meta->state_vectors->julDay);
    secCom[0].value = meta->state_vectors->second;
    nc_put_att(ncid, var_id, "second_of_day", sec_id, 1, secCom);
    nc_put_att_int(ncid, var_id, "vector_count", NC_INT, 1, 
		   &meta->state_vectors->vector_count);
    for (ii=0; ii<meta->state_vectors->vector_count; ii++) {
      vecTimeCom[ii].number = ii+1;
      vecTimeCom[ii].time = meta->state_vectors->vecs[ii].time;
      posCom[ii].number = ii+1;
      posCom[ii].x = meta->state_vectors->vecs[ii].vec.pos.x;
      posCom[ii].y = meta->state_vectors->vecs[ii].vec.pos.y;
      posCom[ii].z = meta->state_vectors->vecs[ii].vec.pos.z;
      velCom[ii].number = ii+1;
      velCom[ii].x = meta->state_vectors->vecs[ii].vec.vel.x;
      velCom[ii].y = meta->state_vectors->vecs[ii].vec.vel.y;
      velCom[ii].z = meta->state_vectors->vecs[ii].vec.vel.z;
    }
    nc_put_att(ncid, var_id, "vector_time", vec_time_id, 3, vecTimeCom);
    nc_put_att(ncid, var_id, "vector_position", pos_id, 3, posCom);
    nc_put_att(ncid, var_id, "vector_velocity", vel_id, 3, velCom);
  }

  // Finish off definition block
  nc_enddef(ncid);

  // Write ASF metadata to XML file
  char *output_file_name = 
    (char *) MALLOC(sizeof(char)*(strlen(output_file)+5));
  sprintf(output_file_name, "%s.xml", output_file);
  meta_write_xml(meta, output_file_name);
  FREE(output_file_name);

  // Clean up
  FREE(str);
  FREE(fValue);
  if (datum)
    FREE(datum);
  if (spheroid)
    FREE(spheroid);
  if (spatial_ref)
    FREE(spatial_ref);
  
  return netcdf;
}

void finalize_netcdf_file(netcdf_t *netcdf, meta_parameters *md)
{
  int ncid = netcdf->ncid;
  int n = md->general->band_count;
  int nl = md->general->line_count;
  int ns = md->general->sample_count;
  long pixel_count = md->general->line_count * md->general->sample_count;
  int projected = FALSE;
  if (md->projection && md->projection->type != SCANSAR_PROJECTION)
    projected = TRUE;

  // Extra bands - Time
  float time = (float) seconds_from_str(md->general->acquisition_date);
  asfPrintStatus("Storing band 'time' ...\n");
  nc_put_var_float(ncid, netcdf->var_id[n], &time);

  // Extra bands - longitude
  n++;
  int ii, kk;
  double *value = (double *) MALLOC(sizeof(double)*MAX_PTS);
  double *l = (double *) MALLOC(sizeof(double)*MAX_PTS);
  double *s = (double *) MALLOC(sizeof(double)*MAX_PTS);
  double line, sample, lat, lon, first_value;
  float *lons = (float *) MALLOC(sizeof(float)*pixel_count);
  asfPrintStatus("Generating band 'longitude' ...\n");
  meta_get_latLon(md, 0, 0, 0.0, &lat, &lon);
  if (lon < 0.0)
    first_value = lon + 360.0;
  else
    first_value = lon;
  asfPrintStatus("Calculating grid for quadratic fit ...\n");
  for (ii=0; ii<RES; ii++) {
    for (kk=0; kk<RES; kk++) {
      line = ii * nl / RES;
      sample = kk * ns / RES;
      meta_get_latLon(md, line, sample, 0.0, &lat, &lon);
      l[ii*RES+kk] = line;
      s[ii*RES+kk] = sample;
      if (lon < 0.0)
	value[ii*RES+kk] = lon + 360.0;
      else
	value[ii*RES+kk] = lon;
    }
    asfLineMeter(ii, nl);
  }
  quadratic_2d q = find_quadratic(value, l, s, MAX_PTS);
  q.A = first_value;
  for (ii=0; ii<nl; ii++) {
    for (kk=0; kk<ns; kk++) {
      lons[ii*ns+kk] = (float)
	(q.A + q.B*ii + q.C*kk + q.D*ii*ii + q.E*ii*kk + q.F*kk*kk +
	 q.G*ii*ii*kk + q.H*ii*kk*kk + q.I*ii*ii*kk*kk + q.J*ii*ii*ii +
	 q.K*kk*kk*kk) - 360.0;
      if (lons[ii*ns+kk] < -180.0)
	lons[ii*ns+kk] += 360.0;
    }
    asfLineMeter(ii, nl);
  }
  asfPrintStatus("Storing band 'longitude' ...\n");
  nc_put_var_float(ncid, netcdf->var_id[n], &lons[0]);
  FREE(lons);

  // Extra bands - Latitude
  n++;
  float *lats = (float *) MALLOC(sizeof(float)*pixel_count);
  asfPrintStatus("Generating band 'latitude' ...\n");
  meta_get_latLon(md, 0, 0, 0.0, &lat, &lon);
  first_value = lat + 180.0;
  asfPrintStatus("Calculating grid for quadratic fit ...\n");
  for (ii=0; ii<RES; ii++) {
    for (kk=0; kk<RES; kk++) {
      line = ii * nl / RES;
      sample = kk * ns / RES;
      meta_get_latLon(md, line, sample, 0.0, &lat, &lon);
      l[ii*RES+kk] = line;
      s[ii*RES+kk] = sample;
      value[ii*RES+kk] = lat + 180.0;
    }
    asfLineMeter(ii, nl);
  }
  q = find_quadratic(value, l, s, MAX_PTS);
  q.A = first_value;
  for (ii=0; ii<nl; ii++) {
    for (kk=0; kk<ns; kk++) {
      //if (projected || md->general->orbit_direction == 'A')
      if (md->general->orbit_direction == 'A')
	lats[(nl-ii-1)*ns+kk] = (float)
	  (q.A + q.B*ii + q.C*kk + q.D*ii*ii + q.E*ii*kk + q.F*kk*kk +
	   q.G*ii*ii*kk + q.H*ii*kk*kk + q.I*ii*ii*kk*kk + q.J*ii*ii*ii +
	   q.K*kk*kk*kk) - 180.0;
      else
	lats[ii*ns+kk] = (float)
	  (q.A + q.B*ii + q.C*kk + q.D*ii*ii + q.E*ii*kk + q.F*kk*kk +
	   q.G*ii*ii*kk + q.H*ii*kk*kk + q.I*ii*ii*kk*kk + q.J*ii*ii*ii +
	   q.K*kk*kk*kk) - 180.0;
    }
    asfLineMeter(ii, nl);
  }
  asfPrintStatus("Storing band 'latitude' ...\n");
  nc_put_var_float(ncid, netcdf->var_id[n], &lats[0]);
  FREE(lats);

  if (projected) {
    // Extra bands - ygrid
    n++;
    float *ygrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<nl; ii++) {
      for (kk=0; kk<ns; kk++)
	ygrids[ii*ns+kk] = 
	  md->projection->startY + kk*md->projection->perY;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'ygrid' ...\n");
    nc_put_var_float(ncid, netcdf->var_id[n], &ygrids[0]);
    FREE(ygrids);
    
    // Extra bands - xgrid
    n++;
    float *xgrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<nl; ii++) {
      for (kk=0; kk<ns; kk++) 
	xgrids[ii*ns+kk] = 
	  md->projection->startX + kk*md->projection->perX;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'xgrid' ...\n");
    nc_put_var_float(ncid, netcdf->var_id[n], &xgrids[0]);
    FREE(xgrids);
  }

  // Close file and clean up
  int status = nc_close(ncid);
  if (status != NC_NOERR)
    asfPrintError("Could not close netCDF file (%s).\n", nc_strerror(status));
  FREE(netcdf->var_id);
  FREE(netcdf);
}
