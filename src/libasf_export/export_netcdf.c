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

netcdf_t *initialize_netcdf_file(const char *output_file, 
				 meta_parameters *meta)
{
  int ii, status, ncid, var_id;
  int dim_xgrid_id, dim_ygrid_id, dim_lat_id, dim_lon_id, dim_time_id;

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
  status = nc_create(output_file, NC_CLOBBER|NC_NETCDF4|NC_CLASSIC_MODEL, 
		     &ncid);
  netcdf->ncid = ncid;
  if (status != NC_NOERR)
    asfPrintError("Could not open netCDF file (%s).\n", nc_strerror(status));

  // Define dimensions
  if (projected) {
    status = nc_def_dim(ncid, "xgrid", sample_count, &dim_xgrid_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define xgrid dimension (%s).\n", 
		    nc_strerror(status));
    status = nc_def_dim(ncid, "ygrid", line_count, &dim_ygrid_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define ygrid dimension (%s).\n", 
		    nc_strerror(status));
  }
  else {
    status = nc_def_dim(ncid, "longitude", sample_count, &dim_lon_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define longitude dimension (%s).\n", 
		    nc_strerror(status));
    status = nc_def_dim(ncid, "latitude", line_count, &dim_lat_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define latitude dimension (%s).\n", 
		    nc_strerror(status));
  }
  status = nc_def_dim(ncid, "time", 1, &dim_time_id);
  if (status != NC_NOERR)
    asfPrintError("Could not define time dimension (%s).\n", 
		  nc_strerror(status));

  // Define projection
  if (projected) {
    status = nc_def_var(ncid, "projection", NC_CHAR, 0, 0, &var_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define projection variable (%s),\n", 
		    nc_strerror(status));
    if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {

      proj_utm utm = meta->projection->param.utm;
      meta_projection *mp = meta->projection;
      char *grid_mapping_name = (char *) MALLOC(sizeof(char)*50);
      strcpy(grid_mapping_name, "transverse_mercator");
      status = nc_put_att_text(ncid, var_id, "grid_mapping_name", 
			       strlen(grid_mapping_name), grid_mapping_name);
      if (status != NC_NOERR)
	asfPrintError("Could not add grid mapping name attribute (%s)\n",
		      nc_strerror(status));
      FREE(grid_mapping_name);
      float *scale_factor_at_central_meridian = 
	(float *) MALLOC(sizeof(float));
      *scale_factor_at_central_meridian = (float) utm.scale_factor;
      status = nc_put_att_float(ncid, var_id,
				"scale_factor_at_central_meridian",
				NC_FLOAT, 1,
				scale_factor_at_central_meridian);
      if (status != NC_NOERR)
	asfPrintError("Could not add scale factor at central meridian "
		      "attribute (%s)\n", nc_strerror(status));
      FREE(scale_factor_at_central_meridian);
      float *longitude_of_central_meridian = (float *) MALLOC(sizeof(float));
      *longitude_of_central_meridian = (float) utm.lon0;
      status = nc_put_att_float(ncid, var_id, "longitude_of_central_meridian",
				NC_FLOAT, 1, longitude_of_central_meridian);
      if (status != NC_NOERR)
	asfPrintError("Could not add longitude of central meridian attribute "
		      "(%s)\n", nc_strerror(status));
      FREE(longitude_of_central_meridian);
      float *latitude_of_projection_origin = (float *) MALLOC(sizeof(float));
      *latitude_of_projection_origin = (float) utm.lat0;
      status = nc_put_att_float(ncid, var_id, "latitude_of_projection_origin",
				NC_FLOAT, 1, latitude_of_projection_origin);
      if (status != NC_NOERR)
	asfPrintError("Could not add latitude of projection origin attribute "
		      "(%s)\n", nc_strerror(status));
      FREE(latitude_of_projection_origin);
      float *false_easting = (float *) MALLOC(sizeof(float));
      *false_easting = (float) utm.false_easting;
      status = nc_put_att_float(ncid, var_id, "false_easting", NC_FLOAT, 1, 
				false_easting);
      if (status != NC_NOERR)
	asfPrintError("Could not add false easting attribute (%s)\n",
		      nc_strerror(status));
      FREE(false_easting);
      float *false_northing = (float *) MALLOC(sizeof(float));
      *false_northing = (float) utm.false_northing;
      status = nc_put_att_float(ncid, var_id, "false_northing", NC_FLOAT, 1, 
				false_northing);
      if (status != NC_NOERR)
	asfPrintError("Could not add false northing attribute (%s)\n",
		      nc_strerror(status));
      FREE(false_northing);
      char *projection_x_coordinate = (char *) MALLOC(sizeof(char)*25);
      strcpy(projection_x_coordinate, "xgrid");
      status = nc_put_att_text(ncid, var_id, "projection_x_coordinate",
			       strlen(projection_x_coordinate), 
			       projection_x_coordinate);
      if (status != NC_NOERR)
	asfPrintError("Could not add projection x coordinate attribute (%s)\n",
		      nc_strerror(status));
      FREE(projection_x_coordinate);
      char *projection_y_coordinate = (char *) MALLOC(sizeof(char)*25);
      strcpy(projection_y_coordinate, "ygrid");
      status = nc_put_att_text(ncid, var_id, "projection_y_coordinate",
			       strlen(projection_y_coordinate), 
			       projection_y_coordinate);
      if (status != NC_NOERR)
	asfPrintError("Could not add projection y coordinate attribute (%s)\n",
		      nc_strerror(status));
      FREE(projection_y_coordinate);
      char *units = (char *) MALLOC(sizeof(char)*25);
      strcpy(units, "m");
      status = nc_put_att_text(ncid, var_id, "units", strlen(units), units); 
      if (status != NC_NOERR)
	asfPrintError("Could not add units attribute (%s)\n",
		      nc_strerror(status));
      FREE(units);
      float *grid_boundary_top_projected_y = (float *) MALLOC(sizeof(float));
      *grid_boundary_top_projected_y = mp->startY;
      status = nc_put_att_float(ncid, var_id, "grid_boundary_top_projected_y",
				NC_FLOAT, 1, grid_boundary_top_projected_y);
      if (status != NC_NOERR)
	asfPrintError("Could not add grid boundary top projected y attribute "
		      "(%s)\n", nc_strerror(status));
      FREE(grid_boundary_top_projected_y);
      float *grid_boundary_bottom_projected_y = 
	(float *) MALLOC(sizeof(float));
      *grid_boundary_bottom_projected_y = 
	mp->startY + meta->general->line_count * mp->perY;
      status = nc_put_att_float(ncid, var_id, 
				"grid_boundary_bottom_projected_y",
				NC_FLOAT, 1, grid_boundary_bottom_projected_y);
      if (status != NC_NOERR)
	asfPrintError("Could not add grid boundary bottom projected y "
		      "attribute (%s)\n", nc_strerror(status));
      FREE(grid_boundary_bottom_projected_y);
      float *grid_boundary_right_projected_x = (float *) MALLOC(sizeof(float));
      *grid_boundary_right_projected_x = 
	mp->startX + meta->general->sample_count * mp->perX;
      status = nc_put_att_float(ncid, var_id, 
				"grid_boundary_right_projected_x",
				NC_FLOAT, 1, grid_boundary_right_projected_x);
      if (status != NC_NOERR)
	asfPrintError("Could not add grid boundary right projected x "
		      "attribute (%s)\n", nc_strerror(status));
      FREE(grid_boundary_right_projected_x);
      float *grid_boundary_left_projected_x = (float *) MALLOC(sizeof(float));
      *grid_boundary_left_projected_x = mp->startX;
      status = nc_put_att_float(ncid, var_id, "grid_boundary_left_projected_x",
				NC_FLOAT, 1, grid_boundary_left_projected_x);
      if (status != NC_NOERR)
	asfPrintError("Could not add grid boundary left projected x attribute "
		      "(%s)\n", nc_strerror(status));
      FREE(grid_boundary_left_projected_x);
      char *spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      char *datum = (char *) MALLOC(sizeof(char)*25);
      if (mp->datum == WGS84_DATUM)
	strcpy(datum, "WGS_1984");
      char *spheroid = (char *) MALLOC(sizeof(char)*25);
      if (mp->spheroid == WGS84_SPHEROID)
	strcpy(spheroid, "WGS_1984");
      sprintf(spatial_ref, "PROJCS[\"%s_UTM_Zone_%d%c\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.1lf,%.8lf]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",%.1lf],PARAMETER[\"False_Northing\",%.1lf],PARAMETER[\"Central_Meridian\",%.1lf],PARAMETER[\"Scale_Factor\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.1lf],UNIT[\"Meter\",1]]",
	      spheroid, utm.zone, mp->hem, spheroid, datum, spheroid,
	      mp->re_major, mp->re_major/(mp->re_major - mp->re_minor),
	      utm.false_easting, utm.false_northing, utm.lon0, 
	      utm.scale_factor, utm.lat0);
      status = nc_put_att_text(ncid, var_id, "spatial_ref", 
			       strlen(spatial_ref), spatial_ref);
      if (status != NC_NOERR)
	asfPrintError("Could not add spatial ref attribute (%s)\n",
		      nc_strerror(status));
      FREE(spatial_ref);
      char *proj4text = (char *) MALLOC(sizeof(char)*255);
      sprintf(proj4text, "+proj=utm +zone=%d", utm.zone);
      if (meta->general->center_latitude < 0)
	strcat(proj4text, " +south");
      status = nc_put_att_text(ncid, var_id, "proj4text", strlen(proj4text),
			       proj4text);
      if (status != NC_NOERR)
	asfPrintError("Could not add proj4text attribute (%s)\n",
		      nc_strerror(status));
      FREE(proj4text);
      int *zone = (int *) MALLOC(sizeof(int));
      *zone = utm.zone;
      status = nc_put_att_int(ncid, var_id, "zone", NC_INT, 1, zone);
      if (status != NC_NOERR)
	asfPrintError("Could not add zone attribute (%s)\n", 
		      nc_strerror(status));
      FREE(zone);
      float *semimajor_radius = (float *) MALLOC(sizeof(float));
      *semimajor_radius = mp->re_major;
      status = nc_put_att_float(ncid, var_id, "semimajor_radius", NC_FLOAT, 1,
				semimajor_radius);
      if (status != NC_NOERR)
	asfPrintError("Could not add semimajor radius attribute (%s)\n",
		      nc_strerror(status));
      FREE(semimajor_radius);
      float *semiminor_radius = (float *) MALLOC(sizeof(float));
      *semiminor_radius = mp->re_minor;
      status = nc_put_att_float(ncid, var_id, "semiminor_radius", NC_FLOAT, 1,
				semiminor_radius);
      if (status != NC_NOERR)
	asfPrintError("Could not add semiminor radius attribute (%s)\n",
		      nc_strerror(status));
      FREE(semiminor_radius);
      char *GeoTransform = (char *) MALLOC(sizeof(char)*255);
      sprintf(GeoTransform, "%.6lf %.6lf 0 %.6lf 0 %.6lf",
	      mp->startX, mp->perX, mp->startY, mp->perY); 
      status = nc_put_att_text(ncid, var_id, "GeoTransform", 
			       strlen(GeoTransform), GeoTransform);
      if (status != NC_NOERR)
	asfPrintError("Could not add GeoTransform attribute (%s)\n",
		      nc_strerror(status));
      FREE(GeoTransform);
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
    
    status = nc_def_var(ncid, band_name[ii], datatype, 3, dims_bands, &var_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define %s variable (%s),\n", 
		    band_name[ii], nc_strerror(status));
    netcdf->var_id[ii] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
    float *fillvalue = (float *) MALLOC(sizeof(float));
    *fillvalue = -999.0;
    status = nc_put_att_float(ncid, var_id, "_FillValue", NC_FLOAT, 1, 
			      fillvalue);
    if (status != NC_NOERR)
      asfPrintError("Could not add fill value attribute (%s)\n",
		    nc_strerror(status));
    FREE(fillvalue);
    char *long_name = (char *) MALLOC(sizeof(char)*255);
    sprintf(long_name, "%s", meta->general->sensor);
    if (meta->general->image_data_type < 9)
      strcat(long_name, " radar backscatter");
    if (meta->general->radiometry >= r_SIGMA_DB &&
	meta->general->radiometry <= r_GAMMA_DB)
      strcat(long_name, " in dB");
    status = nc_put_att_text(ncid, var_id, "long_name", strlen(long_name),
			     long_name);
    if (status != NC_NOERR)
      asfPrintError("Could not add long name attribute (%s)\n",
		    nc_strerror(status));
    FREE(long_name);
    char *cell_methods = (char *) MALLOC(sizeof(char)*25);
    strcpy(cell_methods, "area: backcatter value");
    status = nc_put_att_text(ncid, var_id, "cell_methods", 
			     strlen(cell_methods), cell_methods);
    if (status != NC_NOERR)
      asfPrintError("Could not add cell methods attribute (%s)\n",
		    nc_strerror(status));
    FREE(cell_methods);
    char *units = (char *) MALLOC(sizeof(char)*5);
    strcpy(units, "1");
    status = nc_put_att_text(ncid, var_id, "units", strlen(units), units);
    if (status != NC_NOERR)
      asfPrintError("Could not add units attribute (%s)\n",
		    nc_strerror(status));
    FREE(units);
    char *units_description = (char *) MALLOC(sizeof(char)*255);
    strcpy(units_description, "unitless normalized radar cross-section");
    if (meta->general->radiometry >= r_SIGMA &&
	meta->general->radiometry <= r_GAMMA)
      strcat(units_description, " stored as powerscale");
    else if (meta->general->radiometry >= r_SIGMA_DB &&
	     meta->general->radiometry <= r_GAMMA_DB)
      strcat(units_description, " stored as dB=10*log10(*)");
    status = nc_put_att_text(ncid, var_id, "units_description", 
			     strlen(units_description), units_description);
    if (status != NC_NOERR)
      asfPrintError("Could not add units description attribute (%s)\n",
		    nc_strerror(status));
    FREE(units_description);
    char *coordinates = (char *) MALLOC(sizeof(char)*25);
    strcpy(coordinates, "longitude latitude");
    status = nc_put_att_text(ncid, var_id, "coordinates", strlen(coordinates),
			     coordinates);
    if (status != NC_NOERR)
      asfPrintError("Could not add coordinates attribute (%s)\n",
		    nc_strerror(status));
    FREE(coordinates);
    if (projected) {
      char *grid_mapping = (char *) MALLOC(sizeof(char)*25);
      strcpy(grid_mapping, "projection");
      status = nc_put_att_text(ncid, var_id, "grid_mapping", 
			       strlen(grid_mapping), grid_mapping);
      if (status != NC_NOERR)
	asfPrintError("Could not add grid_mapping attribute (%s)\n",
		      nc_strerror(status));
      FREE(grid_mapping);
    } 
  }
  // Define other attributes
  ymd_date ymd;
  hms_time hms;
  parse_date(meta->general->acquisition_date, &ymd, &hms);

  // Time
  ii = band_count;
  int dims_time[1] = { dim_time_id };
  status = nc_def_var(ncid, "time", NC_FLOAT, 1, dims_time, &var_id);
  if (status != NC_NOERR)
    asfPrintError("Could not define time variable (%s),\n", 
		  nc_strerror(status));
  netcdf->var_id[ii] = var_id;
  char *units = (char *) MALLOC(sizeof(char)*50);
  strcpy(units, "seconds since 1900-01-01T00:00:00Z");
  status = nc_put_att_text(ncid, var_id, "units", strlen(units), units);
  if (status != NC_NOERR)
    asfPrintError("Could not add units attribute (%s)\n",
		  nc_strerror(status));
  FREE(units);
  char *reference = (char *) MALLOC(sizeof(char)*50);
  strcpy(reference, "scene center time");
  status = nc_put_att_text(ncid, var_id, "references", strlen(reference), 
			   reference);
  if (status != NC_NOERR)
    asfPrintError("Could not add references attribute (%s)\n",
		  nc_strerror(status));
  FREE(reference);
  char *standard_name = (char *) MALLOC(sizeof(char)*50);
  strcpy(standard_name, "time");
  status = nc_put_att_text(ncid, var_id, "standard_name", 
			   strlen(standard_name), standard_name);
  if (status != NC_NOERR)
    asfPrintError("Could not add standard name attribute (%s)\n",
		  nc_strerror(status));
  FREE(standard_name);
  char *axis = (char *) MALLOC(sizeof(char)*5);
  strcpy(axis, "T");
  status = nc_put_att_text(ncid, var_id, "axis", strlen(axis), axis);
  if (status != NC_NOERR)
    asfPrintError("Could not add axis attribute (%s)\n",
		  nc_strerror(status));
  FREE(axis);
  char *long_name = (char *) MALLOC(sizeof(char)*50);
  strcpy(long_name, "serial date");
  status = nc_put_att_text(ncid, var_id, "long_name", strlen(long_name), 
			   long_name);
  if (status != NC_NOERR)
    asfPrintError("Could not add long name attribute (%s)\n",
		  nc_strerror(status));
  FREE(long_name);
  
  // Longitude
  ii++;
  if (projected) {
    int dims_lon[2] = { dim_ygrid_id, dim_xgrid_id };
    status = nc_def_var(ncid, "longitude", NC_FLOAT, 2, dims_lon, &var_id);
  }
  else {
    int dims_lon[2] = { dim_lon_id, dim_lat_id };
    status = nc_def_var(ncid, "longitude", NC_FLOAT, 2, dims_lon, &var_id);
  }
  if (status != NC_NOERR)
    asfPrintError("Could not define longitude variable (%s),\n", 
		  nc_strerror(status));
  netcdf->var_id[ii] = var_id;
  nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
  standard_name = (char *) MALLOC(sizeof(char)*50);
  strcpy(standard_name, "longitude");
  status = nc_put_att_text(ncid, var_id, "standard_name", 
			   strlen(standard_name), standard_name);
  if (status != NC_NOERR)
    asfPrintError("Could not add standard name attribute (%s)\n",
		  nc_strerror(status));
  FREE(standard_name);  
  long_name = (char *) MALLOC(sizeof(char)*50);
  strcpy(long_name, "longitude");
  status = nc_put_att_text(ncid, var_id, "long_name", strlen(long_name), 
			   long_name);
  if (status != NC_NOERR)
    asfPrintError("Could not add long name attribute (%s)\n",
		  nc_strerror(status));
  FREE(long_name);  
  units = (char *) MALLOC(sizeof(char)*50);
  strcpy(units, "degrees_east");
  status = nc_put_att_text(ncid, var_id, "units", strlen(units), units);
  if (status != NC_NOERR)
    asfPrintError("Could not add units attribute (%s)\n",
		  nc_strerror(status));
  FREE(units);
  float *valid_range = (float *) MALLOC(sizeof(float)*2);
  valid_range[0] = -180.0;
  valid_range[1] = 180.0;
  status = nc_put_att_float(ncid, var_id, "valid_range", NC_FLOAT, 2, 
			    valid_range);
  if (status != NC_NOERR)
    asfPrintError("Could not add valid range attribute (%s)\n",
		  nc_strerror(status));
  FREE(valid_range);
  float *fillvalue = (float *) MALLOC(sizeof(float));
  *fillvalue = -999.0;
  status = nc_put_att_float(ncid, var_id, "_FillValue", NC_FLOAT, 1, 
			    fillvalue);
  if (status != NC_NOERR)
    asfPrintError("Could not add fill value attribute (%s)\n",
		  nc_strerror(status));
  FREE(fillvalue);

  // Latitude
  ii++;
  if (projected) {
    int dims_lat[2] = { dim_ygrid_id, dim_xgrid_id };
    status = nc_def_var(ncid, "latitude", NC_FLOAT, 2, dims_lat, &var_id);
  }
  else {
    int dims_lat[2] = { dim_lat_id, dim_lon_id };
    status = nc_def_var(ncid, "latitude", NC_FLOAT, 2, dims_lat, &var_id);
  }
  if (status != NC_NOERR)
    asfPrintError("Could not define latitude variable (%s),\n", 
		  nc_strerror(status));
  netcdf->var_id[ii] = var_id;
  nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
  standard_name = (char *) MALLOC(sizeof(char)*50);
  strcpy(standard_name, "latitude");
  status = nc_put_att_text(ncid, var_id, "standard_name", 
			   strlen(standard_name), standard_name);
  if (status != NC_NOERR)
    asfPrintError("Could not add standard name attribute (%s)\n",
		  nc_strerror(status));
  FREE(standard_name);  
  long_name = (char *) MALLOC(sizeof(char)*50);
  strcpy(long_name, "latitude");
  status = nc_put_att_text(ncid, var_id, "long_name", strlen(long_name), 
			   long_name);
  if (status != NC_NOERR)
    asfPrintError("Could not add long name attribute (%s)\n",
		  nc_strerror(status));
  FREE(long_name);  
  units = (char *) MALLOC(sizeof(char)*50);
  strcpy(units, "degrees_north");
  status = nc_put_att_text(ncid, var_id, "units", strlen(units), units);
  if (status != NC_NOERR)
    asfPrintError("Could not add units attribute (%s)\n",
		  nc_strerror(status));
  FREE(units);
  valid_range = (float *) MALLOC(sizeof(float)*2);
  valid_range[0] = -90.0;
  valid_range[1] = 90.0;
  status = nc_put_att_float(ncid, var_id, "valid_range", NC_FLOAT, 2, 
			    valid_range);
  if (status != NC_NOERR)
    asfPrintError("Could not add valid range attribute (%s)\n",
		  nc_strerror(status));
  FREE(valid_range);
  fillvalue = (float *) MALLOC(sizeof(float));
  *fillvalue = -999.0;
  status = nc_put_att_float(ncid, var_id, "_FillValue", NC_FLOAT, 1, 
			    fillvalue);
  if (status != NC_NOERR)
    asfPrintError("Could not add fill value attribute (%s)\n",
		  nc_strerror(status));
  FREE(fillvalue);

  if (projected) {

    // ygrid
    ii++;
    int dims_ygrid[1] = { dim_ygrid_id };
    status = nc_def_var(ncid, "ygrid", NC_FLOAT, 1, dims_ygrid, &var_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define ygrid variable (%s),\n", 
		    nc_strerror(status));
    netcdf->var_id[ii] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
    standard_name = (char *) MALLOC(sizeof(char)*50);
    strcpy(standard_name, "projection_y_coordinates");
    status = nc_put_att_text(ncid, var_id, "standard_name", 
			     strlen(standard_name), standard_name);
    if (status != NC_NOERR)
      asfPrintError("Could not add standard name attribute (%s)\n",
		    nc_strerror(status));
    FREE(standard_name);  
    long_name = (char *) MALLOC(sizeof(char)*50);
    strcpy(long_name, "projection_grid_y_centers");
    status = nc_put_att_text(ncid, var_id, "long_name", strlen(long_name), 
			     long_name);
    if (status != NC_NOERR)
      asfPrintError("Could not add long name attribute (%s)\n",
		    nc_strerror(status));
    FREE(long_name);  
    units = (char *) MALLOC(sizeof(char)*50);
    strcpy(units, "meters");
    status = nc_put_att_text(ncid, var_id, "units", strlen(units), units);
    if (status != NC_NOERR)
      asfPrintError("Could not add units attribute (%s)\n",
		    nc_strerror(status));
    FREE(units);
    axis = (char *) MALLOC(sizeof(char)*5);
    strcpy(axis, "Y");
    status = nc_put_att_text(ncid, var_id, "axis", strlen(axis), axis);
    if (status != NC_NOERR)
      asfPrintError("Could not add axis attribute (%s)\n",
		    nc_strerror(status));
    FREE(axis);

    // xgrid
    ii++;
    int dims_xgrid[1] = { dim_xgrid_id };
    status = nc_def_var(ncid, "xgrid", NC_FLOAT, 1, dims_xgrid, &var_id);
    if (status != NC_NOERR)
      asfPrintError("Could not define xgrid variable (%s),\n", 
		    nc_strerror(status));
    netcdf->var_id[ii] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);    
    standard_name = (char *) MALLOC(sizeof(char)*50);
    strcpy(standard_name, "projection_x_coordinates");
    status = nc_put_att_text(ncid, var_id, "standard_name", 
			     strlen(standard_name), standard_name);
    if (status != NC_NOERR)
      asfPrintError("Could not add standard name attribute (%s)\n",
		    nc_strerror(status));
    FREE(standard_name);  
    long_name = (char *) MALLOC(sizeof(char)*50);
    strcpy(long_name, "projection_grid_x_centers");
    status = nc_put_att_text(ncid, var_id, "long_name", strlen(long_name), 
			     long_name);
    if (status != NC_NOERR)
      asfPrintError("Could not add long name attribute (%s)\n",
		    nc_strerror(status));
    FREE(long_name);  
    units = (char *) MALLOC(sizeof(char)*50);
    strcpy(units, "meters");
    status = nc_put_att_text(ncid, var_id, "units", strlen(units), units);
    if (status != NC_NOERR)
      asfPrintError("Could not add units attribute (%s)\n",
		    nc_strerror(status));
    FREE(units);
    axis = (char *) MALLOC(sizeof(char)*5);
    strcpy(axis, "X");
    status = nc_put_att_text(ncid, var_id, "axis", strlen(axis), axis);
    if (status != NC_NOERR)
      asfPrintError("Could not add axis attribute (%s)\n",
		    nc_strerror(status));
    FREE(axis);    
  }
  
  // Define global attributes
  char *convention = (char *) MALLOC(sizeof(char)*25);
  strcpy(convention, "CF-1.4");
  status = nc_put_att_text(ncid, NC_GLOBAL, "Conventions", strlen(convention),
			   convention);
  if (status != NC_NOERR)
    asfPrintError("Could not add global conventions attribute (%s)\n", 
		  nc_strerror(status));
  FREE(convention);
  char *institution = (char *) MALLOC(sizeof(char)*255);
  strcpy(institution, "Alaska Satellite Facility");
  status = nc_put_att_text(ncid, NC_GLOBAL, "institution", strlen(institution),
			   institution);
  if (status != NC_NOERR)
    asfPrintError("Could not add global institution attribute (%s)\n",
		  nc_strerror(status));
  FREE(institution);
  char *title = (char *) MALLOC(sizeof(char)*255);
  sprintf(title, "%s %s %s image", 
	  meta->general->sensor, meta->general->sensor_name, 
	  meta->general->mode);
  status = nc_put_att_text(ncid, NC_GLOBAL, "title", strlen(title), title);
  if (status != NC_NOERR)
    asfPrintError("Could not add global title attribute (%s)\n", 
		  nc_strerror(status));
  FREE(title);
  char *source = (char *) MALLOC(sizeof(char)*255);
  if (meta->general->image_data_type == AMPLITUDE_IMAGE)
    strcpy(source, "SAR backcatter image");
  status = nc_put_att_text(ncid, NC_GLOBAL, "source", strlen(source), source);
  if (status != NC_NOERR)
    asfPrintError("Could not add global source attribute (%s)\n",
		  nc_strerror(status));
  FREE(source);
  char *original_file = (char *) MALLOC(sizeof(char)*255);
  sprintf(original_file, "%s", meta->general->basename);
  status = nc_put_att_text(ncid, NC_GLOBAL, "original_file", 
			   strlen(original_file), original_file);
  if (status != NC_NOERR)
    asfPrintError("Could not add global original file attribute (%s)\n",
		  nc_strerror(status));
  FREE(original_file);
  char *comment = (char *) MALLOC(sizeof(char)*255);
  if (strcmp_case(meta->general->sensor, "RSAT-1") == 0)
    sprintf(comment, "Copyright Canadian Space Agency, %d", ymd.year);
  else if (strncmp_case(meta->general->sensor, "ERS", 3) == 0)
    sprintf(comment, "Copyright European Space Agency, %d", ymd.year);
  else if (strcmp_case(meta->general->sensor, "JERS-1") == 0 ||
	   strcmp_case(meta->general->sensor, "ALOS") == 0)
    sprintf(comment, "Copyright Japan Aerospace Exploration Agency , %d", 
	    ymd.year);
  status = nc_put_att_text(ncid, NC_GLOBAL, "comment", strlen(comment), 
			   comment);
  if (status != NC_NOERR)
    asfPrintError("Could not add global comment attribute (%s)\n",
		  nc_strerror(status));
  FREE(comment);
  reference = (char *) MALLOC(sizeof(char)*255);
  strcpy(reference, "Documentation available at: www.asf.alaska.edu");
  status = nc_put_att_text(ncid, NC_GLOBAL, "references", strlen(reference),
			   reference);
  if (status != NC_NOERR)
    asfPrintError("Could not add global references attribute (%s)\n",
		  nc_strerror(status));
  FREE(reference);
  char *history = (char *) MALLOC(sizeof(char)*255);
  time_t t;
  struct tm *timeinfo;
  time(&t);
  timeinfo = gmtime(&t);
  sprintf(history, "%s", asctime(timeinfo));
  chomp(history);
  strcat(history, ", UTC: netCDF File created.");
  status = nc_put_att_text(ncid, NC_GLOBAL, "history", strlen(history), 
			   history);
  if (status != NC_NOERR)
    asfPrintError("Could not add global history attribute (%s)\n",
		  nc_strerror(status));
  FREE(history);

  // Finish off definition block
  status = nc_enddef(ncid);
  if (status != NC_NOERR)
    asfPrintError("Could not leave definition mode (%s)\n",
		  nc_strerror(status));

  return netcdf;
}

void finalize_netcdf_file(netcdf_t *netcdf, meta_parameters *md, float *nc)
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
  double time = seconds_from_str(md->general->acquisition_date);
  float *time_value = (float *) MALLOC(sizeof(float));
  *time_value = (float) time;
  asfPrintStatus("Storing band 'time' ...\n");
  nc_put_var_float(ncid, netcdf->var_id[n], &time_value[0]);
  FREE(time_value);

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
  FREE(nc);
}
