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

void nc_meta_double(int group_id, char *name, char *desc, char *units,
		    double *value)
{
  int var_id;
  char *str = (char *) MALLOC(sizeof(char)*1024);
  nc_def_var(group_id, name, NC_DOUBLE, 0, 0, &var_id);
  nc_put_att_text(group_id, var_id, "long_name", strlen(desc), desc);
  if (units && strlen(units) > 0) {
    strcpy(str, units);
    nc_put_att_text(group_id, var_id, "units", strlen(str), str);
  }
  nc_put_var_double(group_id, var_id, value);
}

void nc_meta_float(int group_id, char *name, char *desc, char *units, 
		   float *value)
{
  int var_id;
  char *str = (char *) MALLOC(sizeof(char)*1024);
  nc_def_var(group_id, name, NC_FLOAT, 0, 0, &var_id);
  nc_put_att_text(group_id, var_id, "long_name", strlen(desc), desc);
  if (units && strlen(units) > 0) {
    strcpy(str, units);
    nc_put_att_text(group_id, var_id, "units", strlen(str), str);
  }
  nc_put_var_float(group_id, var_id, value);
}

void nc_meta_int(int group_id, char *name, char *desc, char *units,
		 int *value)
{
  int var_id;
  char *str = (char *) MALLOC(sizeof(char)*1024);
  nc_def_var(group_id, name, NC_INT, 0, 0, &var_id);
  nc_put_att_text(group_id, var_id, "long_name", strlen(desc), desc);
  if (units && strlen(units) > 0) {
    strcpy(str, units);
    nc_put_att_text(group_id, var_id, "units", strlen(str), str);
  }
  nc_put_var_int(group_id, var_id, value);
}

void nc_meta_str(int group_id, char *name, char *desc, char *units,
		 char *value)
{
  int var_id;
  const char *str_value = (char *) MALLOC(sizeof(char)*strlen(value));
  strcpy(str_value, value);
  char *str = (char *) MALLOC(sizeof(char)*1024);
  nc_def_var(group_id, name, NC_STRING, 0, 0, &var_id);
  nc_put_att_text(group_id, var_id, "long_name", strlen(desc), desc);
  if (units && strlen(units) > 0) {
    strcpy(str, units);
    nc_put_att_text(group_id, var_id, "units", strlen(str), str);
  }
  nc_put_var_string(group_id, var_id, &str_value);
}

netcdf_t *initialize_netcdf_file(const char *output_file, 
				 meta_parameters *meta)
{
  int ii, status, ncid, var_id;
  int dim_xgrid_id, dim_ygrid_id, dim_lat_id, dim_lon_id, dim_time_id;
  char *spatial_ref=NULL, *datum=NULL, *spheroid=NULL;

  // Convenience variables
  meta_general *mg = meta->general;
  meta_sar *ms = meta->sar;
  meta_state_vectors *mo = meta->state_vectors;
  meta_projection *mp = meta->projection;

  // Assign parameters
  int projected = FALSE;
  int band_count = mg->band_count;
  int variable_count = band_count + 3;
  if (mp && mp->type != SCANSAR_PROJECTION) {
    projected = TRUE;
    variable_count += 2;
  }
  size_t line_count = mg->line_count;
  size_t sample_count = mg->sample_count;

  // Assign data type
  nc_type datatype;
  if (mg->data_type == BYTE)
    datatype = NC_CHAR;
  else if (mg->data_type == REAL32)
    datatype = NC_FLOAT;

  // Initialize the netCDF pointer structure
  netcdf_t *netcdf = (netcdf_t *) MALLOC(sizeof(netcdf_t));
  netcdf->var_count = variable_count;
  netcdf->var_id = (int *) MALLOC(sizeof(int)*variable_count);

  // Create the actual file
  status = nc_create(output_file, NC_CLOBBER|NC_NETCDF4, &ncid);
  netcdf->ncid = ncid;
  if (status != NC_NOERR)
    asfPrintError("Could not open netCDF file (%s).\n", nc_strerror(status));

  // Define dimensions
  if (projected) {
    nc_def_dim(ncid, "xgrid", sample_count, &dim_xgrid_id);
    nc_def_dim(ncid, "ygrid", line_count, &dim_ygrid_id);
  }
  else {
    status = nc_def_dim(ncid, "longitude", sample_count, &dim_lon_id);
    if (status != NC_NOERR)
      asfPrintError("Problem with longitude definition\n");
    status = nc_def_dim(ncid, "latitude", line_count, &dim_lat_id);
    if (status != NC_NOERR)
      asfPrintError("Problem with latitude definition\n");
  }
  status = nc_def_dim(ncid, "time", 1, &dim_time_id);
  if (status != NC_NOERR)
    asfPrintError("Problem with time definition\n");

  // Define projection
  char *str = (char *) MALLOC(sizeof(char)*1024);
  double lfValue;
  if (projected) {
    nc_def_var(ncid, "projection", NC_CHAR, 0, 0, &var_id);
    if (mp->type == UNIVERSAL_TRANSVERSE_MERCATOR) {

      strcpy(str, "transverse_mercator");
      nc_put_att_text(ncid, var_id, "grid_mapping_name", strlen(str), str);
      nc_put_att_double(ncid, var_id, "scale_factor_at_central_meridian", 
			NC_DOUBLE, 1, &mp->param.utm.scale_factor);
      nc_put_att_double(ncid, var_id, "longitude_of_central_meridian",
			NC_DOUBLE, 1, &mp->param.utm.lon0);
      nc_put_att_double(ncid, var_id, "latitude_of_projection_origin",
			NC_DOUBLE, 1, &mp->param.utm.lat0);
      nc_put_att_double(ncid, var_id, "false_easting", NC_DOUBLE, 1, 
			&mp->param.utm.false_easting);
      nc_put_att_double(ncid, var_id, "false_northing", NC_DOUBLE, 1, 
			&mp->param.utm.false_northing);
      strcpy(str, "xgrid");
      nc_put_att_text(ncid, var_id, "projection_x_coordinate", strlen(str), 
		      str);
      strcpy(str, "ygrid");
      nc_put_att_text(ncid, var_id, "projection_y_coordinate", strlen(str), 
		      str);
      strcpy(str, "m");
      nc_put_att_text(ncid, var_id, "units", strlen(str), str); 
      nc_put_att_double(ncid, var_id, "grid_boundary_top_projected_y",
			NC_DOUBLE, 1, &mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      nc_put_att_double(ncid, var_id, "grid_boundary_bottom_projected_y",
			NC_DOUBLE, 1, &lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      nc_put_att_double(ncid, var_id, "grid_boundary_right_projected_x",
			NC_DOUBLE, 1, &lfValue);
      nc_put_att_double(ncid, var_id, "grid_boundary_left_projected_x",
			NC_DOUBLE, 1, &mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"%s_UTM_Zone_%d%c\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.1lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",%.1lf],PARAMETER[\"False_Northing\",%.1lf],PARAMETER[\"Central_Meridian\",%.1lf],PARAMETER[\"Scale_Factor\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.1lf],UNIT[\"Meter\",1]]",
	      spheroid, mp->param.utm.zone, mp->hem, spheroid, datum, 
	      spheroid, mp->re_major, flat, mp->param.utm.false_easting, 
	      mp->param.utm.false_northing, mp->param.utm.lon0, 
	      mp->param.utm.scale_factor, mp->param.utm.lat0);
      nc_put_att_text(ncid, var_id, "spatial_ref", strlen(spatial_ref), 
		      spatial_ref);
      sprintf(str, "+proj=utm +zone=%d", mp->param.utm.zone);
      if (meta->general->center_latitude < 0)
	strcat(str, " +south");
      nc_put_att_text(ncid, var_id, "proj4text", strlen(str), str);
      nc_put_att_int(ncid, var_id, "zone", NC_INT, 1, &mp->param.utm.zone);
      nc_put_att_double(ncid, var_id, "semimajor_radius", NC_DOUBLE, 1, 
			&mp->re_major);
      nc_put_att_double(ncid, var_id, "semiminor_radius", NC_DOUBLE, 1, 
			&mp->re_minor);
      sprintf(str, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      nc_put_att_text(ncid, var_id, "GeoTransform", strlen(str), str);
    }
    else if (mp->type == POLAR_STEREOGRAPHIC) {

      strcpy(str, "polar_stereographic");
      nc_put_att_text(ncid, var_id, "grid_mapping_name", strlen(str), str);
      lfValue = 90.0;
      nc_put_att_double(ncid, var_id, "straight_vertical_longitude_from_pole", 
			NC_DOUBLE, 1, &mp->param.ps.slon);
      nc_put_att_double(ncid, var_id, "longitude_of_central_meridian",
			NC_DOUBLE, 1, &lfValue);
      nc_put_att_double(ncid, var_id, "standard_parallel",
			NC_DOUBLE, 1, &mp->param.ps.slat);
      nc_put_att_double(ncid, var_id, "false_easting", NC_DOUBLE, 1, 
			&mp->param.ps.false_easting);
      nc_put_att_double(ncid, var_id, "false_northing", NC_DOUBLE, 1, 
			&mp->param.ps.false_northing);
      strcpy(str, "xgrid");
      nc_put_att_text(ncid, var_id, "projection_x_coordinate", strlen(str), 
		      str);
      strcpy(str, "ygrid");
      nc_put_att_text(ncid, var_id, "projection_y_coordinate", strlen(str), 
		      str);
      strcpy(str, "m");
      nc_put_att_text(ncid, var_id, "units", strlen(str), str); 
      nc_put_att_double(ncid, var_id, "grid_boundary_top_projected_y",
			NC_DOUBLE, 1, &mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      nc_put_att_double(ncid, var_id, "grid_boundary_bottom_projected_y",
			NC_DOUBLE, 1, &lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      nc_put_att_double(ncid, var_id, "grid_boundary_right_projected_x",
			NC_DOUBLE, 1, &lfValue);
      nc_put_att_double(ncid, var_id, "grid_boundary_left_projected_x",
			NC_DOUBLE, 1, &mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Stereographic_North_Pole\",GEOGCS[\"unnamed ellipse\",DATUM[\"D_unknown\",SPHEROID[\"Unknown\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0002247191011236]],PROJECTION[\"Stereographic_North_Pole\"],PARAMETER[\"standard_parallel_1\",%.4lf],PARAMETER[\"central_meridian\",%.4lf],PARAMETER[\"scale_factor\",1],PARAMETER[\"false_easting\",%.3lf],PARAMETER[\"false_northing\",%.3lf],UNIT[\"Meter\",1,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"3411\"]]",
	      mp->re_major, flat, mp->param.ps.slat, mp->param.ps.slon,
	      mp->param.ps.false_easting, mp->param.ps.false_northing);
      nc_put_att_text(ncid, var_id, "spatial_ref", strlen(spatial_ref), 
		      spatial_ref);
      if (mp->param.ps.is_north_pole)
	sprintf(str, "+proj=stere +lat_0=90.0000 +lat_ts=%.4lf "
		"+lon_0=%.4lf +k=1 +x_0=%.3lf +y_0=%.3lf +a=%.3lf +b=%.3lf "
		"+units=m +no_defs", mp->param.ps.slat, mp->param.ps.slon,
		mp->param.ps.false_easting, mp->param.ps.false_northing,
		mp->re_major, mp->re_minor);
      else
	sprintf(str, "+proj=stere +lat_0=-90.0000 +lat_ts=%.4lf "
		"+lon_0=%.4lf +k=1 +x_0=%.3lf +y_0=%.3lf +a=%.3lf +b=%.3lf "
		"+units=m +no_defs", mp->param.ps.slat, mp->param.ps.slon,
		mp->param.ps.false_easting, mp->param.ps.false_northing,
		mp->re_major, mp->re_minor);
      nc_put_att_text(ncid, var_id, "proj4text", strlen(str), str);
      nc_put_att_double(ncid, var_id, "latitude_of_true_scale", NC_DOUBLE, 1,
			&mp->param.ps.slat);
      nc_put_att_double(ncid, var_id, "longitude_of_projection_origin", 
			NC_DOUBLE, 1, &mp->param.ps.slon);
      nc_put_att_double(ncid, var_id, "semimajor_radius", NC_DOUBLE, 1, 
			&mp->re_major);
      nc_put_att_double(ncid, var_id, "semiminor_radius", NC_DOUBLE, 1, 
			&mp->re_minor);
      sprintf(str, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      nc_put_att_text(ncid, var_id, "GeoTransform", strlen(str), str);
    }
    else if (mp->type == ALBERS_EQUAL_AREA) {

      strcpy(str, "albers_conical_equal_area");
      nc_put_att_text(ncid, var_id, "grid_mapping_name", strlen(str), str);
      lfValue = 90.0;
      nc_put_att_double(ncid, var_id, "standard_parallel_1", 
			NC_DOUBLE, 1, &mp->param.albers.std_parallel1);
      nc_put_att_double(ncid, var_id, "standard_parallel_2",
			NC_DOUBLE, 1, &mp->param.albers.std_parallel2);
      nc_put_att_double(ncid, var_id, "longitude_of_central_meridian",
			NC_DOUBLE, 1, &mp->param.albers.center_meridian);
      nc_put_att_double(ncid, var_id, "latitude_of_projection_origin",
			NC_DOUBLE, 1, &mp->param.albers.orig_latitude);
      nc_put_att_double(ncid, var_id, "false_easting", NC_DOUBLE, 1, 
			&mp->param.albers.false_easting);
      nc_put_att_double(ncid, var_id, "false_northing", NC_DOUBLE, 1, 
			&mp->param.albers.false_northing);
      strcpy(str, "xgrid");
      nc_put_att_text(ncid, var_id, "projection_x_coordinate", strlen(str), 
		      str);
      strcpy(str, "ygrid");
      nc_put_att_text(ncid, var_id, "projection_y_coordinate", strlen(str), 
		      str);
      strcpy(str, "m");
      nc_put_att_text(ncid, var_id, "units", strlen(str), str); 
      nc_put_att_double(ncid, var_id, "grid_boundary_top_projected_y",
			NC_DOUBLE, 1, &mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      nc_put_att_double(ncid, var_id, "grid_boundary_bottom_projected_y",
			NC_DOUBLE, 1, &lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      nc_put_att_double(ncid, var_id, "grid_boundary_right_projected_x",
			NC_DOUBLE, 1, &lfValue);
      nc_put_att_double(ncid, var_id, "grid_boundary_left_projected_x",
			NC_DOUBLE, 1, &mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Albers_Equal_Area_Conic\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",%.3lf],PARAMETER[\"False_Northing\",%.3lf],PARAMETER[\"Central_Meridian\",%.4lf],PARAMETER[\"Standard_Parallel_1\",%.4lf],PARAMETER[\"Standard_Parallel_2\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.4lf],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.albers.false_easting, mp->param.albers.false_northing,
	      mp->param.albers.center_meridian, mp->param.albers.std_parallel1,
	      mp->param.albers.std_parallel2, mp->param.albers.orig_latitude);
      nc_put_att_text(ncid, var_id, "spatial_ref", strlen(spatial_ref), 
		      spatial_ref);
      sprintf(str, "+proj=aea +lat_1=%.4lf +lat_2=%.4lf +lat_0=%.4lf "
	      "+lon_0=%.4lf +x_0=%.3lf +y_0=%.3lf", 
	      mp->param.albers.std_parallel1, mp->param.albers.std_parallel2, 
	      mp->param.albers.orig_latitude, mp->param.albers.center_meridian,
	      mp->param.albers.false_easting, mp->param.albers.false_northing);
      nc_put_att_text(ncid, var_id, "proj4text", strlen(str), str);
      nc_put_att_double(ncid, var_id, "latitude_of_true_scale", NC_DOUBLE, 1,
			&mp->param.ps.slat);
      nc_put_att_double(ncid, var_id, "longitude_of_projection_origin", 
			NC_DOUBLE, 1, &mp->param.ps.slon);
      nc_put_att_double(ncid, var_id, "semimajor_radius", NC_DOUBLE, 1, 
			&mp->re_major);
      nc_put_att_double(ncid, var_id, "semiminor_radius", NC_DOUBLE, 1, 
			&mp->re_minor);
      sprintf(str, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      nc_put_att_text(ncid, var_id, "GeoTransform", strlen(str), str);
    }
    else if (mp->type == LAMBERT_CONFORMAL_CONIC) {

      strcpy(str, "lambert_conformal_conic");
      nc_put_att_text(ncid, var_id, "grid_mapping_name", strlen(str), str);
      lfValue = 90.0;
      nc_put_att_double(ncid, var_id, "standard_parallel_1", 
			NC_DOUBLE, 1, &mp->param.lamcc.plat1);
      nc_put_att_double(ncid, var_id, "standard_parallel_2",
			NC_DOUBLE, 1, &mp->param.lamcc.plat2);
      nc_put_att_double(ncid, var_id, "longitude_of_central_meridian",
			NC_DOUBLE, 1, &mp->param.lamcc.lon0);
      nc_put_att_double(ncid, var_id, "latitude_of_projection_origin",
			NC_DOUBLE, 1, &mp->param.lamcc.lat0);
      nc_put_att_double(ncid, var_id, "false_easting", NC_DOUBLE, 1, 
			&mp->param.lamcc.false_easting);
      nc_put_att_double(ncid, var_id, "false_northing", NC_DOUBLE, 1, 
			&mp->param.lamcc.false_northing);
      strcpy(str, "xgrid");
      nc_put_att_text(ncid, var_id, "projection_x_coordinate", strlen(str), 
		      str);
      strcpy(str, "ygrid");
      nc_put_att_text(ncid, var_id, "projection_y_coordinate", strlen(str), 
		      str);
      strcpy(str, "m");
      nc_put_att_text(ncid, var_id, "units", strlen(str), str); 
      nc_put_att_double(ncid, var_id, "grid_boundary_top_projected_y",
			NC_DOUBLE, 1, &mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      nc_put_att_double(ncid, var_id, "grid_boundary_bottom_projected_y",
			NC_DOUBLE, 1, &lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      nc_put_att_double(ncid, var_id, "grid_boundary_right_projected_x",
			NC_DOUBLE, 1, &lfValue);
      nc_put_att_double(ncid, var_id, "grid_boundary_left_projected_x",
			NC_DOUBLE, 1, &mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Lambert_Conformal_Conic\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"False_Easting\",%.3lf],PARAMETER[\"False_Northing\",%.3lf],PARAMETER[\"Central_Meridian\",%.4lf],PARAMETER[\"Standard_Parallel_1\",%.4lf],PARAMETER[\"Standard_Parallel_2\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.4lf],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.lamcc.false_easting, mp->param.lamcc.false_northing,
	      mp->param.lamcc.lon0, mp->param.lamcc.plat1,
	      mp->param.lamcc.plat2, mp->param.lamcc.lat0);
      nc_put_att_text(ncid, var_id, "spatial_ref", strlen(spatial_ref), 
		      spatial_ref);
      sprintf(str, "+proj=lcc +lat_1=%.4lf +lat_2=%.4lf +lat_0=%.4lf "
	      "+lon_0=%.4lf +x_0=%.3lf +y_0=%.3lf", 
	      mp->param.lamcc.plat1, mp->param.lamcc.plat2,
	      mp->param.lamcc.lat0, mp->param.lamcc.lon0,
	      mp->param.lamcc.false_easting, mp->param.lamcc.false_northing);
      nc_put_att_text(ncid, var_id, "proj4text", strlen(str), str);
      nc_put_att_double(ncid, var_id, "semimajor_radius", NC_DOUBLE, 1, 
			&mp->re_major);
      nc_put_att_double(ncid, var_id, "semiminor_radius", NC_DOUBLE, 1, 
			&mp->re_minor);
      sprintf(str, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      nc_put_att_text(ncid, var_id, "GeoTransform", strlen(str), str);
    }
    else if (mp->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {

      strcpy(str, "lambert_azimuthal_equal_area");
      nc_put_att_text(ncid, var_id, "grid_mapping_name", strlen(str), str);
      nc_put_att_double(ncid, var_id, "longitude_of_projection_origin",
			NC_DOUBLE, 1, &mp->param.lamaz.center_lon);
      nc_put_att_double(ncid, var_id, "latitude_of_projection_origin",
			NC_DOUBLE, 1, &mp->param.lamaz.center_lat);
      nc_put_att_double(ncid, var_id, "false_easting", NC_DOUBLE, 1, 
			&mp->param.lamaz.false_easting);
      nc_put_att_double(ncid, var_id, "false_northing", NC_DOUBLE, 1, 
			&mp->param.lamaz.false_northing);
      strcpy(str, "xgrid");
      nc_put_att_text(ncid, var_id, "projection_x_coordinate", strlen(str), 
		      str);
      strcpy(str, "ygrid");
      nc_put_att_text(ncid, var_id, "projection_y_coordinate", strlen(str), 
		      str);
      strcpy(str, "m");
      nc_put_att_text(ncid, var_id, "units", strlen(str), str); 
      nc_put_att_double(ncid, var_id, "grid_boundary_top_projected_y",
			NC_DOUBLE, 1, &mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      nc_put_att_double(ncid, var_id, "grid_boundary_bottom_projected_y",
			NC_DOUBLE, 1, &lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      nc_put_att_double(ncid, var_id, "grid_boundary_right_projected_x",
			NC_DOUBLE, 1, &lfValue);
      nc_put_att_double(ncid, var_id, "grid_boundary_left_projected_x",
			NC_DOUBLE, 1, &mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Lambert_Azimuthal_Equal_Area\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"False_Easting\",%.3lf],PARAMETER[\"False_Northing\",%.3lf],PARAMETER[\"Central_Meridian\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.4lf],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.lamaz.false_easting, mp->param.lamaz.false_northing,
	      mp->param.lamaz.center_lon, mp->param.lamaz.center_lat);
      nc_put_att_text(ncid, var_id, "spatial_ref", strlen(spatial_ref), 
		      spatial_ref);
      sprintf(str, "+proj=laea +lat_0=%.4lf +lon_0=%.4lf +x_0=%.3lf "
	      "+y_0=%.3lf", 
	      mp->param.lamaz.center_lat, mp->param.lamaz.center_lon,
	      mp->param.lamaz.false_easting, mp->param.lamaz.false_northing);
      nc_put_att_text(ncid, var_id, "proj4text", strlen(str), str);
      nc_put_att_double(ncid, var_id, "semimajor_radius", NC_DOUBLE, 1, 
			&mp->re_major);
      nc_put_att_double(ncid, var_id, "semiminor_radius", NC_DOUBLE, 1, 
			&mp->re_minor);
      sprintf(str, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      nc_put_att_text(ncid, var_id, "GeoTransform", strlen(str), str);
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
    
    sprintf(str, "%s_AMPLITUDE_IMAGE", band_name[ii]);
    nc_def_var(ncid, str, datatype, 3, dims_bands, &var_id);
    netcdf->var_id[ii] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
    lfValue = -999.0;
    nc_put_att_double(ncid, var_id, "FillValue", NC_DOUBLE, 1, &lfValue);
    sprintf(str, "%s", mg->sensor);
    if (mg->image_data_type < 9)
      strcat(str, " radar backscatter");
    if (mg->radiometry >= r_SIGMA_DB && mg->radiometry <= r_GAMMA_DB)
      strcat(str, " in dB");
    nc_put_att_text(ncid, var_id, "long_name", strlen(str), str);
    strcpy(str, "area: backcatter value");
    nc_put_att_text(ncid, var_id, "cell_methods", strlen(str), str);
    strcpy(str, "1");
    nc_put_att_text(ncid, var_id, "units", strlen(str), str);
    strcpy(str, "unitless normalized radar cross-section");
    if (mg->radiometry >= r_SIGMA && mg->radiometry <= r_GAMMA)
      strcat(str, " stored as powerscale");
    else if (mg->radiometry >= r_SIGMA_DB && mg->radiometry <= r_GAMMA_DB)
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
  parse_date(mg->acquisition_date, &ymd, &hms);

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
  double *valid_range = (double *) MALLOC(sizeof(double)*2);
  valid_range[0] = -180.0;
  valid_range[1] = 180.0;
  nc_put_att_double(ncid, var_id, "valid_range", NC_DOUBLE, 2, valid_range);
  FREE(valid_range);
  lfValue = -999.0;
  nc_put_att_double(ncid, var_id, "FillValue", NC_DOUBLE, 1, &lfValue);

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
  valid_range = (double *) MALLOC(sizeof(double)*2);
  valid_range[0] = -90.0;
  valid_range[1] = 90.0;
  nc_put_att_double(ncid, var_id, "valid_range", NC_DOUBLE, 2, valid_range);
  FREE(valid_range);
  lfValue = -999.0;
  nc_put_att_double(ncid, var_id, "FillValue", NC_DOUBLE, 1, &lfValue);

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
  sprintf(str, "%s %s %s image", mg->sensor, mg->sensor_name, mg->mode);
  nc_put_att_text(ncid, NC_GLOBAL, "title", strlen(str), str);
  if (mg->image_data_type == AMPLITUDE_IMAGE)
    strcpy(str, "SAR backcatter image");
  nc_put_att_text(ncid, NC_GLOBAL, "source", strlen(str), str);
  sprintf(str, "%s", mg->basename);
  nc_put_att_text(ncid, NC_GLOBAL, "original_file", strlen(str), str);
  if (strcmp_case(mg->sensor, "RSAT-1") == 0)
    sprintf(str, "Copyright Canadian Space Agency, %d", ymd.year);
  else if (strncmp_case(mg->sensor, "ERS", 3) == 0)
    sprintf(str, "Copyright European Space Agency, %d", ymd.year);
  else if (strcmp_case(mg->sensor, "JERS-1") == 0 ||
	   strcmp_case(mg->sensor, "ALOS") == 0)
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

  // Metadata  
  int meta_id;
  nc_def_grp(ncid, "metadata", &meta_id);

  // Metadata - general block
  nc_meta_str(meta_id, "general_name", "file name", NULL, mg->basename);
  nc_meta_str(meta_id, "general_sensor", "imaging satellite", NULL, mg->sensor);
  nc_meta_str(meta_id, "general_sensor_name", "imaging sensor", NULL, 
	      mg->sensor_name);
  nc_meta_str(meta_id, "general_mode", "imaging mode", NULL, mg->mode);
  nc_meta_str(meta_id, "general_processor", "name and version of processor", 
	      NULL, mg->processor);
  nc_meta_str(meta_id, "general_data_type", "type of samples (e.g. REAL64)", 
	      NULL, data_type2str(mg->data_type));
  nc_meta_str(meta_id, "general_image_data_type", 
	      "image data type (e.g. AMPLITUDE_IMAGE)", NULL, 
	      image_data_type2str(mg->image_data_type));
  nc_meta_str(meta_id, "general_radiometry", "radiometry (e.g. SIGMA)", NULL, 
	      radiometry2str(mg->radiometry));
  nc_meta_str(meta_id, "general_acquisition_date", 
	      "acquisition date of the image", NULL, mg->acquisition_date);
  nc_meta_int(meta_id, "general_orbit", "orbit number of image", NULL, 
	      &mg->orbit);
  if (mg->orbit_direction == 'A')
    strcpy(str, "Ascending");
  else
    strcpy(str, "Descending");
  nc_meta_str(meta_id, "general_orbit_direction", "orbit direction", NULL, str);
  nc_meta_int(meta_id, "general_frame", "frame number of image", NULL, 
	      &mg->frame);
  nc_meta_int(meta_id, "general_band_count", "number of bands in image", NULL, 
	      &mg->band_count);
  nc_meta_str(meta_id, "general_bands", "bands of the sensor", NULL, 
	      &mg->bands);
  nc_meta_int(meta_id, "general_line_count", "number of lines in image", NULL, 
	      &mg->line_count);
  nc_meta_int(meta_id, "general_sample_count", "number of samples in image", 
	      NULL, &mg->sample_count);
  nc_meta_int(meta_id, "general_start_line", 
	      "first line relative to original image", NULL, &mg->start_line);
  nc_meta_int(meta_id, "general_start_sample", 
	      "first sample relative to original image", NULL, 
	      &mg->start_sample);
  nc_meta_double(meta_id, "general_x_pixel_size", "range pixel size", "m", 
		 &mg->x_pixel_size);
  nc_meta_double(meta_id, "general_y_pixel_size", "azimuth pixel size", "m", 
		 &mg->y_pixel_size);
  nc_meta_double(meta_id, "general_center_latitude", 
		 "approximate image center latitude", "degrees_north", 
		 &mg->center_latitude);
  nc_meta_double(meta_id, "general_center_longitude",
		 "approximate image center longitude", "degrees_east", 
		 &mg->center_longitude);
  nc_meta_double(meta_id,  "general_re_major", "major (equator) axis of earth",
		 "m", &mg->re_major);
  nc_meta_double(meta_id, "general_re_minor", "minor (polar) axis of earth", 
		 "m", &mg->re_minor);
  nc_meta_double(meta_id, "general_bit_error_rate", 
		 "fraction of bits which are in error", NULL, 
		 &mg->bit_error_rate);
  nc_meta_int(meta_id, "general_missing_lines", 
	      "number of missing lines in data take", NULL, &mg->missing_lines);
  nc_meta_float(meta_id, "general_no_data", 
		"value indicating no data for a pixel",	NULL, &mg->no_data);

  if (ms) {
    // Metadata - SAR block
    if (ms->image_type == 'S')
      sprintf(str, "slant range");
    else if (ms->image_type == 'G')
      sprintf(str, "ground range");
    else if (ms->image_type == 'P')
      sprintf(str, "projected");
    else if (ms->image_type == 'R')
      sprintf(str, "georeferenced");    
    nc_meta_str(meta_id, "sar_image_type", "image type", NULL, str);
    if (ms->look_direction == 'R')
      sprintf(str, "right");
    else if (ms->look_direction == 'L')
      sprintf(str, "left");
    nc_meta_str(meta_id, "sar_look_direction", "SAR satellite look direction", 
		NULL, str);
    nc_meta_int(meta_id, "sar_look_count", "number of looks to take from SLC", 
		NULL, &ms->look_count);
    nc_meta_int(meta_id, "sar_multilook", "multilooking flag", NULL, 
		&ms->multilook);
    nc_meta_int(meta_id, "sar_deskewed", "zero doppler deskew flag", NULL, 
		&ms->deskewed);
    nc_meta_int(meta_id, "sar_original_line_count", 
		"number of lines in original image", NULL, 
		&ms->original_line_count);
    nc_meta_int(meta_id, "sar_original_sample_count", 
		"number of samples in original image", NULL, 
		&ms->original_sample_count);
    nc_meta_double(meta_id, "sar_line_increment", 
		   "line increment for sampling", NULL, &ms->line_increment);
    nc_meta_double(meta_id, "sar_sample_increment", 
		   "sample increment for sampling", NULL, 
		   &ms->sample_increment);
    nc_meta_double(meta_id, "sar_range_time_per_pixel", 
		   "time per pixel in range", "s", 
		   &ms->range_time_per_pixel);
    nc_meta_double(meta_id, "sar_azimuth_time_per_pixel", 
		   "time per pixel in azimuth", "s", 
		   &ms->azimuth_time_per_pixel);
    nc_meta_double(meta_id, "sar_slant_range_first_pixel", 
		   "slant range to first pixel", "m", 
		   &ms->slant_range_first_pixel);
    nc_meta_double(meta_id, "sar_slant_shift", 
		   "error correction factor in slant range", "m", 
		   &ms->slant_shift);
    nc_meta_double(meta_id, "sar_time_shift", "error correction factor in time",
		   "s", &ms->time_shift);
    nc_meta_double(meta_id, "sar_wavelength", "SAR carrier wavelength", "m", 
		   &ms->wavelength);
    nc_meta_double(meta_id, "sar_pulse_repetition_frequency", 
		   "pulse repetition frequency", "Hz", &ms->prf);
    nc_meta_double(meta_id, "sar_earth_radius", "earth radius at scene center", 
		   "m", &ms->earth_radius);
    nc_meta_double(meta_id, "sar_satellite_height", 
		   "satellite height from earth's center", "m", 
		   &ms->satellite_height);
    nc_meta_double(meta_id, "sar_range_doppler_centroid", 
		   "range doppler centroid", "Hz", 
		   &ms->range_doppler_coefficients[0]);
    nc_meta_double(meta_id, "sar_range_doppler_linear", 
		   "range doppler per range pixel", "Hz/pixel", 
		   &ms->range_doppler_coefficients[1]);
    nc_meta_double(meta_id, "sar_range_doppler_quadratic", 
		   "range doppler per range pixel square", "Hz/pixel^2", 
		   &ms->range_doppler_coefficients[2]);
    nc_meta_double(meta_id, "sar_azimuth_doppler_centroid", 
		   "azimuth doppler centroid", "Hz", 
		   &ms->azimuth_doppler_coefficients[0]);
    nc_meta_double(meta_id, "sar_azimuth_doppler_linear", 
		   "azimuth doppler per azimuth pixel", "Hz/pixel", 
		   &ms->azimuth_doppler_coefficients[1]);
    nc_meta_double(meta_id, "sar_azimuth_doppler_quadratic", 
		   "azimuth doppler per azimuth per azimuth pixel square", 
		   "Hz/pixel^2", &ms->azimuth_doppler_coefficients[2]);
  }

  char tmp[50];
  if (mo) {
    // Metadata - state vector block
    nc_meta_int(meta_id, "orbit_year", "year of image start", NULL, &mo->year);
    nc_meta_int(meta_id, "orbit_day_of_year", "day of the year at image start",
		NULL, &mo->julDay);
    nc_meta_double(meta_id, "orbit_second_of_day", 
		   "second of the day at image start", "seconds", &mo->second);
    int vector_count = mo->vector_count;
    nc_meta_int(meta_id, "orbit_vector_count", "number of state vectors", NULL,
		&vector_count);
    for (ii=0; ii<vector_count; ii++) {
      sprintf(tmp, "orbit_vector[%d]_time", ii+1);
      nc_meta_double(meta_id, tmp, "time relative to image start", "s",
		     &mo->vecs[ii].time);
      sprintf(tmp, "orbit_vector[%d]_position_x", ii+1);
      nc_meta_double(meta_id, tmp, "x coordinate, earth-fixed", "m",
		     &mo->vecs[ii].vec.pos.x);
      sprintf(tmp, "orbit_vector[%d]_position_y", ii+1);
      nc_meta_double(meta_id, tmp, "y coordinate, earth-fixed", "m",
		     &mo->vecs[ii].vec.pos.y);
      sprintf(tmp, "orbit_vector[%d]_position_z", ii+1);
      nc_meta_double(meta_id, tmp, "z coordinate, earth-fixed", "m",
		     &mo->vecs[ii].vec.pos.z);
      sprintf(tmp, "orbit_vector[%d]_velocity_x", ii+1);
      nc_meta_double(meta_id, tmp, "x velocity, earth-fixed", "m/s",
		     &mo->vecs[ii].vec.vel.x);
      sprintf(tmp, "orbit_vector[%d]_velocity_y", ii+1);
      nc_meta_double(meta_id, tmp, "y velocity, earth-fixed", "m/s",
		     &mo->vecs[ii].vec.vel.y);
      sprintf(tmp, "orbit_vector[%d]_velocity_z", ii+1);
      nc_meta_double(meta_id, tmp, "z velocity, earth-fixed", "m/s",
		     &mo->vecs[ii].vec.vel.z);
    }    
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
