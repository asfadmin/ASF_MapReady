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

#define RES 16
#define MAX_PTS 256

void h5_att_double(hid_t data, hid_t space, char *name, double value)
{
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_DOUBLE, &value);
  H5Aclose(attr);
}

void h5_att_float(hid_t data, hid_t space, char *name, float value)
{
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_FLOAT, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_FLOAT, &value);
  H5Aclose(attr);
}

void h5_att_int(hid_t data, hid_t space, char *name, int value)
{
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_INT, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_INT, &value);
  H5Aclose(attr);
}

void h5_att_float2(hid_t data, hid_t space, char *name, float *value)
{
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_FLOAT, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_FLOAT, value);
  H5Aclose(attr);
}

void h5_att_str(hid_t data, hid_t space, char *name, char *value)
{
  hid_t str = H5Tcopy(H5T_C_S1);
  H5Tset_size(str, strlen(value));
  hid_t attr = H5Acreate(data, name, str, space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(attr, str, value);
  H5Aclose(attr);
}

void h5_value_double(hid_t file, char *group, char *name, 
		     double value, char *long_name, char *units)
{
  char meta[100];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_DOUBLE, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
  h5_att_str(h5_data, h5_space, "long_name", long_name);
  if (units && strlen(units) > 0) 
    h5_att_str(h5_data, h5_space, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_float(hid_t file, char *group, char *name, 
		    float value, char *long_name, char *units)
{
  char meta[100];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_FLOAT, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
  h5_att_str(h5_data, h5_space, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, h5_space, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_int(hid_t file, char *group, char *name, 
		  int value, char *long_name, char *units)
{
  char meta[100];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_INT, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
  h5_att_str(h5_data, h5_space, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, h5_space, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_str(hid_t file, char *group, char *name, 
		  char *value, char *long_name, char *units)
{
  char meta[100];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_str = H5Tcopy(H5T_C_S1);
  H5Tset_size(h5_str, strlen(value));
  hid_t h5_data = H5Dcreate(file, meta, h5_str, h5_space,
			 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, h5_str, H5S_ALL, H5S_ALL, H5P_DEFAULT, value);
  h5_att_str(h5_data, h5_space, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, h5_space, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

h5_t *initialize_h5_file(const char *output_file_name, meta_parameters *md)
{
  hid_t h5_file, h5_datagroup, h5_metagroup, h5_data, h5_proj;
  hid_t h5_array, h5_string, h5_time, h5_lat, h5_lon, h5_xgrid, h5_ygrid;
  int ii, kk, complex=FALSE, projected=FALSE;
  char *spatial_ref=NULL, *datum=NULL, *spheroid=NULL;
  char dataset[50], group[50], band[5], str_attr[50], tmp[50];
  double lfValue;

  // Convenience variables
  meta_general *mg = md->general;
  meta_sar *ms = md->sar;
  meta_state_vectors *mo = md->state_vectors;
  meta_projection *mp = md->projection;

  // Check whether data is map projected
  if (mp && mp->type != SCANSAR_PROJECTION)
    //asfPrintError("Image is map projected. Wrong initialization function!\n");
    projected = TRUE;

  // Initialize the HDF pointer structure
  h5_t *h5 = (h5_t *) MALLOC(sizeof(h5_t));
  int band_count = mg->band_count;
  h5->var_count = band_count;
  h5->var = (hid_t *) MALLOC(sizeof(hid_t)*h5->var_count);

  // Check for complex data
  if (mg->image_data_type == COMPLEX_IMAGE)
    complex = TRUE;

  // Create new HDF5 file
  h5_file = 
    H5Fcreate(output_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  h5->file = h5_file;

  // Create data space
  int samples = mg->sample_count;
  int lines = mg->line_count;
  hsize_t dims[2] = { lines, samples };
  hsize_t cdims[2] = { 100, samples };
  hsize_t rdims[2] = { 1, 2 };
  h5_array = H5Screate_simple(2, dims, NULL);
  h5->space = h5_array;
  h5_string = H5Screate(H5S_SCALAR);
  hid_t h5_range = H5Screate(H5S_SIMPLE);
  H5Sset_extent_simple(h5_range, 2, rdims, NULL); 
  
  // Create data structure
  char **band_name = extract_band_names(mg->bands, band_count);
  hid_t h5_plist = H5Pcreate(H5P_DATASET_CREATE);
  H5Pset_chunk(h5_plist, 2, cdims);
  H5Pset_deflate(h5_plist, 6);
  
  // Create a data group
  sprintf(group, "/data");
  h5_datagroup = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);
  // Projection information
  if (projected) { 
    sprintf(group, "/data/projection");
    h5_proj = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (mp->type == UNIVERSAL_TRANSVERSE_MERCATOR) {

      h5_att_str(h5_proj, h5_string, "grid_mapping_name", 
		 "transverse_mercator");
      h5_att_double(h5_proj, h5_string, "scale_factor_at_central_meridian", 
		    mp->param.utm.scale_factor);
      h5_att_double(h5_proj, h5_string, "longitude_of_central_meridian", 
		    mp->param.utm.lon0);
      h5_att_double(h5_proj, h5_string, "latitude_of_projection_origin",
		    mp->param.utm.lat0);
      h5_att_double(h5_proj, h5_string, "false_easting", 
		    mp->param.utm.false_easting);
      h5_att_double(h5_proj, h5_string, "false_northing", 
		    mp->param.utm.false_northing);
      h5_att_str(h5_proj, h5_string, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, h5_string, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, h5_string, "units", "meters"); 
      h5_att_double(h5_proj, h5_string, "grid_boundary_top_projected_y",
		    mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, h5_string, "grid_boundary_bottom_projected_y",
		    lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, h5_string, "grid_boundary_right_projected_x",
		    lfValue);
      h5_att_double(h5_proj, h5_string, "grid_boundary_left_projected_x",
		    mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"%s_UTM_Zone_%d%c\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.1lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",%.1lf],PARAMETER[\"False_Northing\",%.1lf],PARAMETER[\"Central_Meridian\",%.1lf],PARAMETER[\"Scale_Factor\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.1lf],UNIT[\"Meter\",1]]",
	      spheroid, mp->param.utm.zone, mp->hem, spheroid, datum, 
	      spheroid, mp->re_major, flat, mp->param.utm.false_easting, 
	      mp->param.utm.false_northing, mp->param.utm.lon0, 
	      mp->param.utm.scale_factor, mp->param.utm.lat0);
      h5_att_str(h5_proj, h5_string, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=utm +zone=%d", mp->param.utm.zone);
      if (mg->center_latitude < 0)
	strcat(str_attr, " +south");
      h5_att_str(h5_proj, h5_string, "proj4text", str_attr);
      h5_att_int(h5_proj, h5_string, "zone", mp->param.utm.zone);
      h5_att_double(h5_proj, h5_string, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, h5_string, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, h5_string, "GeoTransform", str_attr);
    }
    else if (mp->type == POLAR_STEREOGRAPHIC) {

      h5_att_str(h5_proj, h5_string, "grid_mapping_name", 
		 "polar_stereographic");
      h5_att_double(h5_proj, h5_string, "straight_vertical_longitude_from_pole",
		    mp->param.ps.slon);
      h5_att_double(h5_proj, h5_string, "latitude_of_projection_origin", 
		    mp->param.ps.slat);
      h5_att_double(h5_proj, h5_string, "scale_factor_at_projection_origin",
		    1.0);
      h5_att_double(h5_proj, h5_string, "false_easting", 
		    mp->param.ps.false_easting);
      h5_att_double(h5_proj, h5_string, "false_northing",
		    mp->param.ps.false_northing);
      h5_att_str(h5_proj, h5_string, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, h5_string, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, h5_string, "units", "meters");
      h5_att_double(h5_proj, h5_string, "grid_boundary_top_projected_y",
		    mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, h5_string, "grid_boundary_bottom_projected_y",
		    lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, h5_string, "grid_boundary_right_projected_x",
		    lfValue);
      h5_att_double(h5_proj, h5_string, "grid_boundary_left_projected_x",
		    mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Stereographic_North_Pole\",GEOGCS[\"unnamed ellipse\",DATUM[\"D_unknown\",SPHEROID[\"Unknown\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0002247191011236]],PROJECTION[\"Stereographic_North_Pole\"],PARAMETER[\"standard_parallel_1\",%.4lf],PARAMETER[\"central_meridian\",%.4lf],PARAMETER[\"scale_factor\",1],PARAMETER[\"false_easting\",%.1lf],PARAMETER[\"false_northing\",%.1lf],UNIT[\"Meter\",1, AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"3411\"]]",
	      mp->re_major, flat, mp->param.ps.slat, mp->param.ps.slon,
	      mp->param.ps.false_easting, mp->param.ps.false_northing);
      h5_att_str(h5_proj, h5_string, "spatial_ref", spatial_ref);
      if (mp->param.ps.is_north_pole)
	sprintf(str_attr, "+proj=stere +lat_0=90.0000 +lat_ts=%.4lf "
		"+lon_0=%.4lf +k=1 +x_0=%.3lf +y_0=%.3lf +a=%.3lf +b=%.3lf "
		"+units=m +no_defs", mp->param.ps.slat, mp->param.ps.slon,
		mp->param.ps.false_easting, mp->param.ps.false_northing,
		mp->re_major, mp->re_minor);
      else
	sprintf(str_attr, "+proj=stere +lat_0=-90.0000 +lat_ts=%.4lf "
		"+lon_0=%.4lf +k=1 +x_0=%.3lf +y_0=%.3lf +a=%.3lf +b=%.3lf "
		"+units=m +no_defs", mp->param.ps.slat, mp->param.ps.slon,
		mp->param.ps.false_easting, mp->param.ps.false_northing,
		mp->re_major, mp->re_minor);
      h5_att_str(h5_proj, h5_string, "proj4text", str_attr);
      h5_att_double(h5_proj, h5_string, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, h5_string, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, h5_string, "GeoTransform", str_attr);
    }
    else if (mp->type == ALBERS_EQUAL_AREA) {

      h5_att_str(h5_proj, h5_string, "grid_mapping_name", 
		 "albers_conical_equal_area");
      h5_att_double(h5_proj, h5_string, "standard_parallel_1",
		    mp->param.albers.std_parallel1);
      h5_att_double(h5_proj, h5_string, "standard_parallel_2",
		    mp->param.albers.std_parallel2);
      h5_att_double(h5_proj, h5_string, "longitude_of_central_meridian", 
		    mp->param.albers.center_meridian);
      h5_att_double(h5_proj, h5_string, "latitude_of_projection_origin",
		    mp->param.albers.orig_latitude);
      h5_att_double(h5_proj, h5_string, "false_easting", 
		    mp->param.albers.false_easting);
      h5_att_double(h5_proj, h5_string, "false_northing",
		    mp->param.albers.false_northing);
      h5_att_str(h5_proj, h5_string, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, h5_string, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, h5_string, "units", "meters");
      h5_att_double(h5_proj, h5_string, "grid_boundary_top_projected_y",
		    mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, h5_string, "grid_boundary_bottom_projected_y",
		    lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, h5_string, "grid_boundary_right_projected_x",
		    lfValue);
      h5_att_double(h5_proj, h5_string, "grid_boundary_left_projected_x",
		    mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Albers_Equal_Area_Conic\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",%.3lf],PARAMETER[\"False_Northing\",%.3lf],PARAMETER[\"Central_Meridian\",%.4lf],PARAMETER[\"Standard_Parallel_1\",%.4lf],PARAMETER[\"Standard_Parallel_2\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.4lf],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.albers.false_easting, mp->param.albers.false_northing,
	      mp->param.albers.center_meridian, mp->param.albers.std_parallel1,
	      mp->param.albers.std_parallel2, mp->param.albers.orig_latitude);
      h5_att_str(h5_proj, h5_string, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=aea +lat_1=%.4lf +lat_2=%.4lf +lat_0=%.4lf "
	      "+lon_0=%.4lf +x_0=%.3lf +y_0=%.3lf", 
	      mp->param.albers.std_parallel1, mp->param.albers.std_parallel2,
	      mp->param.albers.orig_latitude, mp->param.albers.center_meridian,
	      mp->param.albers.false_easting, mp->param.albers.false_northing);
      h5_att_str(h5_proj, h5_string, "proj4text", str_attr);
      h5_att_double(h5_proj, h5_string, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, h5_string, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, h5_string, "GeoTransform", str_attr);
    }
    else if (mp->type == LAMBERT_CONFORMAL_CONIC) {

      h5_att_str(h5_proj, h5_string, "grid_mapping_name", 
		 "lambert_conformal_conic");
      h5_att_double(h5_proj, h5_string, "standard_parallel_1",
		    mp->param.lamcc.plat1);
      h5_att_double(h5_proj, h5_string, "standard_parallel_2",
		    mp->param.lamcc.plat2);
      h5_att_double(h5_proj, h5_string, "longitude_of_central_meridian", 
		    mp->param.lamcc.lon0);
      h5_att_double(h5_proj, h5_string, "latitude_of_projection_origin",
		    mp->param.lamcc.lat0);
      h5_att_double(h5_proj, h5_string, "false_easting", 
		    mp->param.lamcc.false_easting);
      h5_att_double(h5_proj, h5_string, "false_northing",
		    mp->param.lamcc.false_northing);
      h5_att_str(h5_proj, h5_string, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, h5_string, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, h5_string, "units", "meters");
      h5_att_double(h5_proj, h5_string, "grid_boundary_top_projected_y",
		    mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, h5_string, "grid_boundary_bottom_projected_y",
		    lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, h5_string, "grid_boundary_right_projected_x",
		    lfValue);
      h5_att_double(h5_proj, h5_string, "grid_boundary_left_projected_x",
		    mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Lambert_Conformal_Conic\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"False_Easting\",%.3lf],PARAMETER[\"False_Northing\",%.3lf],PARAMETER[\"Central_Meridian\",%.4lf],PARAMETER[\"Standard_Parallel_1\",%.4lf],PARAMETER[\"Standard_Parallel_2\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.4lf],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.lamcc.false_easting, mp->param.lamcc.false_northing,
	      mp->param.lamcc.lon0, mp->param.lamcc.plat1,
	      mp->param.lamcc.plat2, mp->param.lamcc.lat0);
      h5_att_str(h5_proj, h5_string, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=lcc +lat_1=%.4lf +lat_2=%.4lf +lat_0=%.4lf "
	      "+lon_0=%.4lf +x_0=%.3lf +y_0=%.3lf", 
	      mp->param.lamcc.plat1, mp->param.lamcc.plat2,
	      mp->param.lamcc.lat0, mp->param.lamcc.lon0,
	      mp->param.lamcc.false_easting, mp->param.lamcc.false_northing);
      h5_att_str(h5_proj, h5_string, "proj4text", str_attr);
      h5_att_double(h5_proj, h5_string, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, h5_string, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, h5_string, "GeoTransform", str_attr);
    }
    else if (mp->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {

      h5_att_str(h5_proj, h5_string, "grid_mapping_name", 
		 "lambert_azimuthal_equal_area");
      h5_att_double(h5_proj, h5_string, "longitude_of_projection_origin", 
		    mp->param.lamaz.center_lon);
      h5_att_double(h5_proj, h5_string, "latitude_of_projection_origin",
		    mp->param.lamaz.center_lat);
      h5_att_double(h5_proj, h5_string, "false_easting", 
		    mp->param.lamaz.false_easting);
      h5_att_double(h5_proj, h5_string, "false_northing",
		    mp->param.lamaz.false_northing);
      h5_att_str(h5_proj, h5_string, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, h5_string, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, h5_string, "units", "meters");
      h5_att_double(h5_proj, h5_string, "grid_boundary_top_projected_y",
		    mp->startY);
      lfValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, h5_string, "grid_boundary_bottom_projected_y",
		    lfValue);
      lfValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, h5_string, "grid_boundary_right_projected_x",
		    lfValue);
      h5_att_double(h5_proj, h5_string, "grid_boundary_left_projected_x",
		    mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Lambert_Azimuthal_Equal_Area\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3lf,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"False_Easting\",%.3lf],PARAMETER[\"False_Northing\",%.3lf],PARAMETER[\"Central_Meridian\",%.4lf],PARAMETER[\"Latitude_Of_Origin\",%.4lf],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.lamaz.false_easting, mp->param.lamaz.false_northing,
	      mp->param.lamaz.center_lon, mp->param.lamaz.center_lat);
      h5_att_str(h5_proj, h5_string, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=laea +lat_0=%.4lf +lon_0=%.4lf +x_0=%.3lf "
	      "+y_0=%.3lf", 
	      mp->param.lamaz.center_lat, mp->param.lamaz.center_lon,
	      mp->param.lamaz.false_easting, mp->param.lamaz.false_northing);
      h5_att_str(h5_proj, h5_string, "proj4text", str_attr);
      h5_att_double(h5_proj, h5_string, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, h5_string, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, h5_string, "GeoTransform", str_attr);
    }
  }

  for (ii=0; ii<band_count; ii++) {

    // Create data set
    strncpy(band, band_name[ii], 2);
    band[2] = '\0';
    sprintf(dataset, "/data/%s_AMPLITUDE_IMAGE", band);
    h5_data = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
			H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    h5->var[ii] = h5_data;

    // Add attributes (from CF convention)
    sprintf(str_attr, "%s", mg->sensor);
    if (mg->image_data_type < 9)
      strcat(str_attr, " radar backscatter");
    if (mg->radiometry >= r_SIGMA_DB && mg->radiometry <= r_GAMMA_DB)
      strcat(str_attr, " in dB");
    h5_att_str(h5_data, h5_string, "long_name", str_attr);
    h5_att_str(h5_data, h5_string, "cell_methods", "area: backscatter value");
    h5_att_str(h5_data, h5_string, "units", "1");
    h5_att_str(h5_data, h5_string, "units_description",
	       "unitless normalized radar cross-section");
    if (mg->radiometry >= r_SIGMA && mg->radiometry <= r_GAMMA)
      strcat(str_attr, " stored as powerscale");
    else if (mg->radiometry >= r_SIGMA_DB && mg->radiometry <= r_GAMMA_DB)
      strcat(str_attr, " stored as dB=10*log10(*)");
    h5_att_float(h5_data, h5_string, "_FillValue", -999);
    h5_att_str(h5_data, h5_string, "coordinates", "longitude latitude");
    if (projected)
      h5_att_str(h5_data, h5_string, "grid_mapping", "projection");

    // Close up
    H5Dclose(h5_data);
  }

  // Extra bands - Time
  asfPrintStatus("Storing band 'time' ...\n");  
  float serial_date = seconds_from_str(mg->acquisition_date);
  sprintf(dataset, "/data/time");
  h5_time = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_string,
		      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_time, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, 
	   &serial_date);
  h5_att_str(h5_time, h5_string, "units", "seconds since 1900-01-01T00:00:00Z");
  h5_att_str(h5_time, h5_string, "references", "scene center time");
  h5_att_str(h5_time, h5_string, "standard_name", "time");
  h5_att_str(h5_time, h5_string, "axis", "T");
  h5_att_str(h5_time, h5_string, "long_name", "serial date");
  H5Dclose(h5_time);
  
  // Extra bands - Longitude
  int nl = mg->line_count;
  int ns = mg->sample_count;
  long pixel_count = mg->line_count * mg->sample_count;
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
  sprintf(dataset, "/data/longitude");
  h5_lon = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
		     H5P_DEFAULT, h5_plist, H5P_DEFAULT);
  H5Dwrite(h5_lon, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, lons);
  h5_att_str(h5_lon, h5_string, "units", "degrees_east");
  h5_att_str(h5_lon, h5_string, "long_name", "longitude");
  h5_att_str(h5_lon, h5_string, "standard_name", "longitude");
  float valid_range[2] = { -180.0, 180.0 };
  h5_att_float2(h5_lon, h5_range, "valid_range", valid_range);
  h5_att_float(h5_lon, h5_string, "_FillValue", -999);
  H5Dclose(h5_lon);
  FREE(lons);

  // Extra bands - Latitude
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
      if (mg->orbit_direction == 'A')
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
  sprintf(dataset, "/data/latitude");
  h5_lat = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
		     H5P_DEFAULT, h5_plist, H5P_DEFAULT);
  H5Dwrite(h5_lat, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, lats);
  h5_att_str(h5_lat, h5_string, "units", "degrees_north");
  h5_att_str(h5_lat, h5_string, "long_name", "latitude");
  h5_att_str(h5_lat, h5_string, "standard_name", "latitude");
  valid_range[0] = -90.0;
  valid_range[1] = 90.0;
  h5_att_float2(h5_lat, h5_range, "valid_range", valid_range);
  h5_att_float(h5_lat, h5_string, "_FillValue", -999);
  H5Dclose(h5_lat);
  FREE(lats);

  if (projected) {
    // Extra bands - ygrid
    float *ygrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<nl; ii++) {
      for (kk=0; kk<ns; kk++)
	ygrids[ii*ns+kk] = mp->startY + kk*mp->perY;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'ygrid' ...\n");
    sprintf(dataset, "/data/ygrid");
    h5_ygrid = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
			 H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    H5Dwrite(h5_ygrid, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ygrids);
    h5_att_str(h5_ygrid, h5_string, "units", "meters");
    h5_att_str(h5_ygrid, h5_string, "long_name", 
	       "projection_grid_y_coordinates");
    h5_att_str(h5_ygrid, h5_string, "standard_name", 
	       "projection_y_coordinates");
    h5_att_str(h5_ygrid, h5_string, "axis", "Y");
    H5Dclose(h5_ygrid);
    FREE(ygrids);
    
    // Extra bands - xgrid
    float *xgrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<nl; ii++) {
      for (kk=0; kk<ns; kk++) 
	xgrids[ii*ns+kk] = mp->startX + kk*mp->perX;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'xgrid' ...\n");
    sprintf(dataset, "/data/xgrid");
    h5_xgrid = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
			 H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    H5Dwrite(h5_xgrid, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, xgrids);
    h5_att_str(h5_xgrid, h5_string, "units", "meters");
    h5_att_str(h5_xgrid, h5_string, "long_name", 
	       "projection_grid_x_coordinates");
    h5_att_str(h5_xgrid, h5_string, "standard_name", 
	       "projection_x_coordinates");
    h5_att_str(h5_xgrid, h5_string, "axis", "X");
    H5Dclose(h5_xgrid);
    FREE(xgrids);
  }

  H5Gclose(h5_datagroup);

  // Adding global attributes
  hid_t h5_global = H5Gopen(h5_file, "/", H5P_DEFAULT);
  h5_att_str(h5_global, h5_string, "institution", "Alaska Satellite Facility");
  sprintf(str_attr, "%s %s %s image", mg->sensor, mg->sensor_name, mg->mode);
  h5_att_str(h5_global, h5_string, "title", str_attr); 
  if (mg->image_data_type == AMPLITUDE_IMAGE)
    strcpy(str_attr, "SAR backcatter image");
  h5_att_str(h5_global, h5_string, "source", str_attr);
  h5_att_str(h5_global, h5_string, "original_file", mg->basename);
  ymd_date ymd;
  hms_time hms;
  parse_date(mg->acquisition_date, &ymd, &hms);
  if (strcmp_case(mg->sensor, "RSAT-1") == 0)
    sprintf(str_attr, "Copyright Canadian Space Agency, %d", ymd.year);
  else if (strncmp_case(mg->sensor, "ERS", 3) == 0)
    sprintf(str_attr, "Copyright European Space Agency, %d", ymd.year);
  else if (strcmp_case(mg->sensor, "JERS-1") == 0 ||
	   strcmp_case(mg->sensor, "ALOS") == 0)
    sprintf(str_attr, "Copyright Japan Aerospace Exploration Agency , %d", 
	    ymd.year);
  h5_att_str(h5_global, h5_string, "comment", str_attr); 
  h5_att_str(h5_global, h5_string, "reference",
	     "Documentation available at: www.asf.alaska.edu");
  time_t t;
  struct tm *timeinfo;
  time(&t);
  timeinfo = gmtime(&t);
  sprintf(str_attr, "%s", asctime(timeinfo));
  chomp(str_attr);
  strcat(str_attr, ", UTC: H5 File created.");
  h5_att_str(h5_global, h5_string, "history", str_attr); 
  H5Gclose(h5_global);

  // Metadata
  sprintf(group, "/metadata");
  h5_metagroup = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);

  // Metadata - General block
  h5_value_str(h5_file, group, "general_name", mg->basename, "file_name", NULL);
  h5_value_str(h5_file, group, "general_sensor", mg->sensor,
	       "imaging satellite", NULL);
  h5_value_str(h5_file, group, "general_sensor_name", mg->sensor_name, 
	       "imaging sensor", NULL);
  h5_value_str(h5_file, group, "general_mode", mg->mode, "imaging mode", NULL);
  h5_value_str(h5_file, group, "general_processor", mg->processor, 
	       "name and version of processor", NULL);
  h5_value_str(h5_file, group, "general_data_type", 
	       data_type2str(mg->data_type), "type of samples (e.g. REAL64)", 
	       NULL);
  h5_value_str(h5_file, group, "general_image_data_type",
	       image_data_type2str(mg->image_data_type),
	       "image data type (e.g. AMPLITUDE_IMAGE)", NULL);
  h5_value_str(h5_file, group, "general_radiometry",
	       radiometry2str(mg->radiometry), "radiometry (e.g. SIGMA)", NULL);
  // FIXME: UDUNITS seconds since ...
  h5_value_str(h5_file, group, "general_acquisition_date", mg->acquisition_date,
	       "acquisition date of image", NULL);
  h5_value_int(h5_file, group, "general_orbit", mg->orbit,
	       "orbit number of image", NULL);
  if (mg->orbit_direction == 'A')
    strcpy(str_attr, "Ascending");
  else
    strcpy(str_attr, "Descending");
  h5_value_str(h5_file, group, "general_orbit_direction", str_attr,
	       "orbit direction", NULL);
  h5_value_int(h5_file, group, "general_frame", mg->frame,
	       "frame number of image", NULL);
  h5_value_int(h5_file, group, "general_band_count", mg->band_count,
	       "number of bands in image", NULL);
  h5_value_str(h5_file, group, "general_bands", mg->bands,
	       "bands of the sensor", NULL);
  h5_value_int(h5_file, group, "general_line_count", mg->line_count,
	       "number of lines in image", NULL);
  h5_value_int(h5_file, group, "general_sample_count", mg->sample_count, 
	       "number of samples in image", NULL);
  h5_value_int(h5_file, group, "general_start_line", mg->start_line,
	       "first line relative to original image", NULL);
  h5_value_int(h5_file, group, "general_start_sample", mg->start_sample, 
	       "first sample relative to original image", NULL);
  h5_value_double(h5_file, group, "general_x_pixel_size", mg->x_pixel_size, 
		  "range pixel size", "m");
  h5_value_double(h5_file, group, "general_y_pixel_size", mg->y_pixel_size, 
		  "azimuth pixel size", "m");
  h5_value_double(h5_file, group, "general_center_latitude", 
		  mg->center_latitude, "approximate image center latitude", 
		  "degrees_north");
  h5_value_double(h5_file, group, "general_center_longitude",
		  mg->center_longitude, "approximate image center longitude", 
		  "degrees_east");
  h5_value_double(h5_file, group, "general_re_major", mg->re_major,
		  "major (equator) axis of earth", "m");
  h5_value_double(h5_file, group, "general_re_minor", mg->re_minor,
		  "minor (polar) axis of earth", "m");
  h5_value_double(h5_file, group, "general_bit_error_rate", mg->bit_error_rate, 
		  "fraction of bits which are in error", NULL);
  h5_value_int(h5_file, group, "general_missing_lines", mg->missing_lines, 
	       "number of missing lines in image", NULL);
  h5_value_float(h5_file, group, "general_no_data", mg->no_data,
		 "value indicating no data for a pixel", NULL);

  if (ms) {
    if (ms->image_type == 'S')
      strcpy(str_attr, "slant range");
    else if (ms->image_type == 'G')
      strcpy(str_attr, "ground range");
    else if (ms->image_type == 'P')
      strcpy(str_attr, "projected");
    else if (ms->image_type == 'R')
      strcpy(str_attr, "georeferenced");
    h5_value_str(h5_file, group, "sar_image_type", str_attr, "image type", 
		 NULL);
    if (ms->look_direction == 'R')
      strcpy(str_attr, "right");
    else if (ms->look_direction == 'L')
      strcpy(str_attr, "left");
    h5_value_str(h5_file, group, "sar_look_direction", str_attr,
		 "SAR satellite look direction", NULL);
    h5_value_int(h5_file, group, "sar_look_count", ms->look_count,
		 "number of looks to take from SLC", NULL);
    h5_value_int(h5_file, group, "sar_multilook", ms->multilook,
		 "multilooking flag", NULL);
    h5_value_int(h5_file, group, "sar_deskewed", ms->deskewed,
		 "zero doppler deskew flag", NULL);
    h5_value_int(h5_file, group, "sar_original_line_count",
		 ms->original_line_count, "number of lines in original image", 
		 NULL);
    h5_value_int(h5_file, group, "sar_original_sample_count",
		 ms->original_sample_count,
		 "number of samples in original image", NULL);
    h5_value_double(h5_file, group, "sar_line_increment", 
		    ms->line_increment, "line increment for sampling", NULL);
    h5_value_double(h5_file, group, "sar_sample_increment",
		    ms->sample_increment, "sample increment for sampling",
		    NULL);
    h5_value_double(h5_file, group, "sar_range_time_per_pixel",
		    ms->range_time_per_pixel, "time per pixel in range", "s");
    h5_value_double(h5_file, group, "sar_azimuth_time_per_pixel",
		    ms->azimuth_time_per_pixel, "time per pixel in azimuth", 
		    "s");
    h5_value_double(h5_file, group, "sar_slant_range_first_pixel",
		    ms->slant_range_first_pixel, "slant range to first pixel", 
		    "m");
    h5_value_double(h5_file, group, "sar_slant_shift", ms->slant_shift, 
		    "error correction factor in slant range", "m");
    h5_value_double(h5_file, group, "sar_time_shift", ms->time_shift, 
		    "error correction factor in time", "s");
    h5_value_double(h5_file, group, "sar_wavelength", ms->wavelength,
		    "SAR carrier wavelength", "m");
    h5_value_double(h5_file, group, "sar_pulse_repetition_frequency", 
		    ms->prf, "pulse repetition frequency", "Hz");
    h5_value_double(h5_file, group, "sar_earth_radius", ms->earth_radius, 
		    "earth radius at image center", "m");
    h5_value_double(h5_file, group, "sar_satellite_height", 
		    ms->satellite_height, 
		    "satellite height from earth's center", "m");
    h5_value_double(h5_file, group, "sar_range_doppler_centroid",
		    ms->range_doppler_coefficients[0], "range doppler centroid",
		    "Hz");
    // FIXME: UDUNITS unit_description
    h5_value_double(h5_file, group, "sar_range_doppler_linear",
		    ms->range_doppler_coefficients[1],
		    "range doppler per range pixel", "Hz/pixel");
    // FIXME: UDUNITS unit description
    h5_value_double(h5_file, group, "sar_range_doppler_quadratic",
		    ms->range_doppler_coefficients[2],
		    "range doppler per range pixel square", "Hz/pixel^2");
    h5_value_double(h5_file, group, "sar_azimuth_doppler_centroid",
		    ms->azimuth_doppler_coefficients[0],
		    "azimuth doppler centroid", "Hz");
    h5_value_double(h5_file, group, "sar_azimuth_doppler_linear",
		    ms->azimuth_doppler_coefficients[1],
		    "azimuth doppler per azimuth pixel", "Hz/pixel");
    h5_value_double(h5_file, group, "sar_azimuth_doppler_quadratic",
		    ms->azimuth_doppler_coefficients[2],
		    "azimuth doppler per azimuth pixel square", "Hz/pixel^2");
  }

  if (mo) {
    int vector_count = mo->vector_count;
    h5_value_int(h5_file, group, "orbit_year", mo->year,
		 "year of image start", NULL);
    h5_value_int(h5_file, group, "orbit_day_of_year", mo->julDay, 
		 "day of year at image start", NULL);
    h5_value_double(h5_file, group, "orbit_second_of_day", mo->second,
		    "second of day at image start", "s");
    h5_value_int(h5_file, group, "orbit_vector_count", vector_count,
		 "number of state vectors", NULL);
    for (ii=0; ii<vector_count; ii++) {
      sprintf(tmp, "orbit_vector[%d]_time", ii+1);
      h5_value_double(h5_file, group, tmp, mo->vecs[ii].time,
		      "time relative to image start", "s");
      sprintf(tmp, "orbit_vector[%d]_position_x", ii+1);
      h5_value_double(h5_file, group, tmp, mo->vecs[ii].vec.pos.x,
		      "x coordinate, earth-fixed", "m");
      sprintf(tmp, "orbit_vector[%d]_position_y", ii+1);
      h5_value_double(h5_file, group, tmp, mo->vecs[ii].vec.pos.y,
		      "y coordinate, earth-fixed", "m");
      sprintf(tmp, "orbit_vector[%d]_position_z", ii+1);
      h5_value_double(h5_file, group, tmp, mo->vecs[ii].vec.pos.z,
		      "z coordinate, earth-fixed", "m");
      sprintf(tmp, "orbit_vector[%d]_velocity_x", ii+1);
      h5_value_double(h5_file, group, tmp, mo->vecs[ii].vec.vel.x,
		      "x velocity, earth-fixed", "m/s");
      sprintf(tmp, "orbit_vector[%d]_velocity_y", ii+1);
      h5_value_double(h5_file, group, tmp, mo->vecs[ii].vec.vel.y,
		      "y velocity, earth-fixed", "m/s");
      sprintf(tmp, "orbit_vector[%d]_velocity_z", ii+1);
      h5_value_double(h5_file, group, tmp, mo->vecs[ii].vec.vel.z,
		      "z velocity, earth-fixed", "m/s");
    }
  }
  H5Gclose(h5_metagroup);
  H5Sclose(h5_string);
  H5Sclose(h5_array);

  // Write ASF metadata to XML file
  char *output_file = 
    (char *) MALLOC(sizeof(char)*(strlen(output_file_name)+5));
  sprintf(output_file, "%s.xml", output_file_name);
  meta_write_xml(md, output_file);
  FREE(output_file);

  return h5;
}

void finalize_h5_file(h5_t *hdf)
{
  H5Fclose(hdf->file);
  
  // Clean up
  FREE(hdf->var);
  FREE(hdf);
}
