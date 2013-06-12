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
#include <asf_iso_meta.h>
#include <asf_export.h>
#include <asf_raster.h>
#include <float_image.h>
#include <spheroids.h>
#include <typlim.h>
#include <hdf5.h>

#define RES 16
#define MAX_PTS 256

void h5_att_double(hid_t data, char *name, double value)
{
  hid_t space = H5Screate(H5S_SCALAR);
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_DOUBLE, &value);
  H5Aclose(attr);
}

void h5_att_float(hid_t data, char *name, float value)
{
  hid_t space = H5Screate(H5S_SCALAR);
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_FLOAT, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_FLOAT, &value);
  H5Aclose(attr);
}

void h5_att_int(hid_t data, char *name, int value)
{
  hid_t space = H5Screate(H5S_SCALAR);
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_INT, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_INT, &value);
  H5Aclose(attr);
}

void h5_att_float2(hid_t data, char *name, float *value)
{
  hsize_t dims[2] = { 1, 2 };
  hid_t space = H5Screate(H5S_SIMPLE);
  H5Sset_extent_simple(space, 2, dims, NULL); 
  //hid_t space = H5Screate(H5S_SCALAR);
  hid_t attr = H5Acreate(data, name, H5T_NATIVE_FLOAT, space, H5P_DEFAULT, 
			 H5P_DEFAULT);
  H5Awrite(attr, H5T_NATIVE_FLOAT, value);
  H5Aclose(attr);
}

void h5_att_str(hid_t data, char *name, char *value)
{
  hid_t space = H5Screate(H5S_SCALAR);
  hid_t str = H5Tcopy(H5T_C_S1);
  H5Tset_size(str, strlen(value));
  hid_t attr = H5Acreate(data, name, str, space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(attr, str, value);
  H5Aclose(attr);
}

void h5_value_double(hid_t file, char *group, char *name, 
		     double value, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_DOUBLE, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0) 
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_double_array(hid_t file, char *group, char *name, double *value,
			   double numValues, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hsize_t dims[1] = { numValues };
  hid_t h5_space = H5Screate_simple(1, dims, NULL);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_DOUBLE, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0) 
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_float(hid_t file, char *group, char *name, 
		    float value, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_FLOAT, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_float_array(hid_t file, char *group, char *name, float *value,
			  float numValues, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hsize_t dims[1] = { numValues };
  hid_t h5_space = H5Screate_simple(1, dims, NULL);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_FLOAT, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_boolean(hid_t file, char *group, char *name, 
		      int value, char *long_name, char *units)
{
  char meta[255];
  char *str = (char *) MALLOC(sizeof(char)*10);
  if (value == 1)
    strcpy(str, "true");
  else
    strcpy(str, "false");
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_str = H5Tcopy(H5T_C_S1);
  H5Tset_size(h5_str, strlen(str));
  hid_t h5_data = H5Dcreate(file, meta, h5_str, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, h5_str, H5S_ALL, H5S_ALL, H5P_DEFAULT, str);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
  FREE(str);
}

void h5_value_int(hid_t file, char *group, char *name, 
		  int value, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_INT, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_int_array(hid_t file, char *group, char *name, int *value,
			int numValues, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hsize_t dims[1] = { numValues };
  hid_t h5_space = H5Screate_simple(1, dims, NULL);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_INT, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_long(hid_t file, char *group, char *name, 
		   long value, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_LONG, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_long_array(hid_t file, char *group, char *name, long *value, 
			 int numValues, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hsize_t dims[1] = { numValues };
  hid_t h5_space = H5Screate_simple(1, dims, NULL);
  hid_t h5_data = H5Dcreate(file, meta, H5T_NATIVE_LONG, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_str(hid_t file, char *group, char *name, 
		  char *value, char *long_name, char *units)
{
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hid_t h5_space = H5Screate(H5S_SCALAR);
  hid_t h5_str = H5Tcopy(H5T_C_S1);
  H5Tset_size(h5_str, strlen(value));
  hid_t h5_data = H5Dcreate(file, meta, h5_str, h5_space,
			 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, h5_str, H5S_ALL, H5S_ALL, H5P_DEFAULT, value);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

void h5_value_str_array(hid_t file, char *group, char *name, char **values, 
			int numValues, char *long_name, char *units)
{
  int ii, length = 0;
  char meta[255];
  sprintf(meta, "%s/%s", group, name);
  hsize_t dims[1] = { numValues };
  hid_t h5_space = H5Screate_simple(1, dims, NULL);
  hid_t h5_str = H5Tcopy(H5T_C_S1);
  for (ii=0; ii<numValues; ii++) {
    if (strlen(values[ii]) > length)
      length = strlen(values[ii]);
  }
  H5Tset_size(h5_str, length);
  hid_t h5_data = H5Dcreate(file, meta, h5_str, h5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_data, h5_str, H5S_ALL, H5S_ALL, H5P_DEFAULT, *values);
  h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

static h5_t *initialize_h5_file_meta(const char *output_file_name, 
				     meta_parameters *md)
{
  hid_t h5_file, h5_datagroup, h5_metagroup, h5_data, h5_proj;
  hid_t h5_array, h5_time, h5_lat, h5_lon, h5_xgrid, h5_ygrid;
  int ii, kk, complex=FALSE, projected=FALSE;
  char *spatial_ref=NULL, *datum=NULL, *spheroid=NULL;
  char dataset[50], group[50], str_attr[50], tmp[50];
  double fValue;

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
  h5_array = H5Screate_simple(2, dims, NULL);
  h5->space = h5_array;
  
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

      h5_att_str(h5_proj, "grid_mapping_name", "transverse_mercator");
      h5_att_double(h5_proj, "scale_factor_at_central_meridian", 
		    mp->param.utm.scale_factor);
      h5_att_double(h5_proj, "longitude_of_central_meridian", 
		    mp->param.utm.lon0);
      h5_att_double(h5_proj, "latitude_of_projection_origin",
		    mp->param.utm.lat0);
      h5_att_double(h5_proj, "false_easting", mp->param.utm.false_easting);
      h5_att_double(h5_proj, "false_northing", mp->param.utm.false_northing);
      h5_att_str(h5_proj, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, "units", "meters"); 
      h5_att_double(h5_proj, "grid_boundary_top_projected_y", mp->startY);
      fValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, "grid_boundary_bottom_projected_y", fValue);
      fValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, "grid_boundary_right_projected_x", fValue);
      h5_att_double(h5_proj, "grid_boundary_left_projected_x", mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"%s_UTM_Zone_%d%c\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.1f,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",%.1f],PARAMETER[\"False_Northing\",%.1f],PARAMETER[\"Central_Meridian\",%.1f],PARAMETER[\"Scale_Factor\",%.4f],PARAMETER[\"Latitude_Of_Origin\",%.1f],UNIT[\"Meter\",1]]",
	      spheroid, mp->param.utm.zone, mp->hem, spheroid, datum, 
	      spheroid, mp->re_major, flat, mp->param.utm.false_easting, 
	      mp->param.utm.false_northing, mp->param.utm.lon0, 
	      mp->param.utm.scale_factor, mp->param.utm.lat0);
      h5_att_str(h5_proj, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=utm +zone=%d", mp->param.utm.zone);
      if (mg->center_latitude < 0)
	strcat(str_attr, " +south");
      h5_att_str(h5_proj, "proj4text", str_attr);
      h5_att_int(h5_proj, "zone", mp->param.utm.zone);
      h5_att_double(h5_proj, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6f %.6f 0 %.6f 0 %.6f", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, "GeoTransform", str_attr);
    }
    else if (mp->type == POLAR_STEREOGRAPHIC) {

      h5_att_str(h5_proj, "grid_mapping_name", "polar_stereographic");
      h5_att_double(h5_proj, "straight_vertical_longitude_from_pole",
		    mp->param.ps.slon);
      h5_att_double(h5_proj, "latitude_of_projection_origin", 
		    mp->param.ps.slat);
      h5_att_double(h5_proj, "scale_factor_at_projection_origin", 1.0);
      h5_att_double(h5_proj, "false_easting", mp->param.ps.false_easting);
      h5_att_double(h5_proj, "false_northing", mp->param.ps.false_northing);
      h5_att_str(h5_proj, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, "units", "meters");
      h5_att_double(h5_proj, "grid_boundary_top_projected_y", mp->startY);
      fValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, "grid_boundary_bottom_projected_y", fValue);
      fValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, "grid_boundary_right_projected_x", fValue);
      h5_att_double(h5_proj, "grid_boundary_left_projected_x", mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Stereographic_North_Pole\",GEOGCS[\"unnamed ellipse\",DATUM[\"D_unknown\",SPHEROID[\"Unknown\",%.3f,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0002247191011236]],PROJECTION[\"Stereographic_North_Pole\"],PARAMETER[\"standard_parallel_1\",%.4f],PARAMETER[\"central_meridian\",%.4f],PARAMETER[\"scale_factor\",1],PARAMETER[\"false_easting\",%.1f],PARAMETER[\"false_northing\",%.1f],UNIT[\"Meter\",1, AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"3411\"]]",
	      mp->re_major, flat, mp->param.ps.slat, mp->param.ps.slon,
	      mp->param.ps.false_easting, mp->param.ps.false_northing);
      h5_att_str(h5_proj, "spatial_ref", spatial_ref);
      if (mp->param.ps.is_north_pole)
	sprintf(str_attr, "+proj=stere +lat_0=90.0000 +lat_ts=%.4f "
		"+lon_0=%.4f +k=1 +x_0=%.3f +y_0=%.3f +a=%.3f +b=%.3f "
		"+units=m +no_defs", mp->param.ps.slat, mp->param.ps.slon,
		mp->param.ps.false_easting, mp->param.ps.false_northing,
		mp->re_major, mp->re_minor);
      else
	sprintf(str_attr, "+proj=stere +lat_0=-90.0000 +lat_ts=%.4f "
		"+lon_0=%.4f +k=1 +x_0=%.3f +y_0=%.3f +a=%.3f +b=%.3f "
		"+units=m +no_defs", mp->param.ps.slat, mp->param.ps.slon,
		mp->param.ps.false_easting, mp->param.ps.false_northing,
		mp->re_major, mp->re_minor);
      h5_att_str(h5_proj, "proj4text", str_attr);
      h5_att_double(h5_proj, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6f %.6f 0 %.6f 0 %.6f", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, "GeoTransform", str_attr);
    }
    else if (mp->type == ALBERS_EQUAL_AREA) {

      h5_att_str(h5_proj, "grid_mapping_name", "albers_conical_equal_area");
      h5_att_double(h5_proj, "standard_parallel_1",
		    mp->param.albers.std_parallel1);
      h5_att_double(h5_proj, "standard_parallel_2",
		    mp->param.albers.std_parallel2);
      h5_att_double(h5_proj, "longitude_of_central_meridian", 
		    mp->param.albers.center_meridian);
      h5_att_double(h5_proj, "latitude_of_projection_origin",
		    mp->param.albers.orig_latitude);
      h5_att_double(h5_proj, "false_easting", mp->param.albers.false_easting);
      h5_att_double(h5_proj, "false_northing", mp->param.albers.false_northing);
      h5_att_str(h5_proj, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, "units", "meters");
      h5_att_double(h5_proj, "grid_boundary_top_projected_y", mp->startY);
      fValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, "grid_boundary_bottom_projected_y", fValue);
      fValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, "grid_boundary_right_projected_x", fValue);
      h5_att_double(h5_proj, "grid_boundary_left_projected_x", mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Albers_Equal_Area_Conic\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3f,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",%.3f],PARAMETER[\"False_Northing\",%.3f],PARAMETER[\"Central_Meridian\",%.4f],PARAMETER[\"Standard_Parallel_1\",%.4f],PARAMETER[\"Standard_Parallel_2\",%.4f],PARAMETER[\"Latitude_Of_Origin\",%.4f],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.albers.false_easting, mp->param.albers.false_northing,
	      mp->param.albers.center_meridian, mp->param.albers.std_parallel1,
	      mp->param.albers.std_parallel2, mp->param.albers.orig_latitude);
      h5_att_str(h5_proj, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=aea +lat_1=%.4f +lat_2=%.4f +lat_0=%.4f "
	      "+lon_0=%.4f +x_0=%.3f +y_0=%.3f", 
	      mp->param.albers.std_parallel1, mp->param.albers.std_parallel2,
	      mp->param.albers.orig_latitude, mp->param.albers.center_meridian,
	      mp->param.albers.false_easting, mp->param.albers.false_northing);
      h5_att_str(h5_proj, "proj4text", str_attr);
      h5_att_double(h5_proj, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6f %.6f 0 %.6f 0 %.6f", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, "GeoTransform", str_attr);
    }
    else if (mp->type == LAMBERT_CONFORMAL_CONIC) {

      h5_att_str(h5_proj, "grid_mapping_name", "lambert_conformal_conic");
      h5_att_double(h5_proj, "standard_parallel_1", mp->param.lamcc.plat1);
      h5_att_double(h5_proj, "standard_parallel_2", mp->param.lamcc.plat2);
      h5_att_double(h5_proj, "longitude_of_central_meridian", 
		    mp->param.lamcc.lon0);
      h5_att_double(h5_proj, "latitude_of_projection_origin",
		    mp->param.lamcc.lat0);
      h5_att_double(h5_proj, "false_easting", mp->param.lamcc.false_easting);
      h5_att_double(h5_proj, "false_northing", mp->param.lamcc.false_northing);
      h5_att_str(h5_proj, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, "units", "meters");
      h5_att_double(h5_proj, "grid_boundary_top_projected_y", mp->startY);
      fValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, "grid_boundary_bottom_projected_y", fValue);
      fValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, "grid_boundary_right_projected_x", fValue);
      h5_att_double(h5_proj, "grid_boundary_left_projected_x", mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Lambert_Conformal_Conic\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3f,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"False_Easting\",%.3f],PARAMETER[\"False_Northing\",%.3f],PARAMETER[\"Central_Meridian\",%.4f],PARAMETER[\"Standard_Parallel_1\",%.4f],PARAMETER[\"Standard_Parallel_2\",%.4f],PARAMETER[\"Latitude_Of_Origin\",%.4f],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.lamcc.false_easting, mp->param.lamcc.false_northing,
	      mp->param.lamcc.lon0, mp->param.lamcc.plat1,
	      mp->param.lamcc.plat2, mp->param.lamcc.lat0);
      h5_att_str(h5_proj, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=lcc +lat_1=%.4f +lat_2=%.4f +lat_0=%.4f "
	      "+lon_0=%.4f +x_0=%.3f +y_0=%.3f", 
	      mp->param.lamcc.plat1, mp->param.lamcc.plat2,
	      mp->param.lamcc.lat0, mp->param.lamcc.lon0,
	      mp->param.lamcc.false_easting, mp->param.lamcc.false_northing);
      h5_att_str(h5_proj, "proj4text", str_attr);
      h5_att_double(h5_proj, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6f %.6f 0 %.6f 0 %.6f", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, "GeoTransform", str_attr);
    }
    else if (mp->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {

      h5_att_str(h5_proj, "grid_mapping_name", "lambert_azimuthal_equal_area");
      h5_att_double(h5_proj, "longitude_of_projection_origin", 
		    mp->param.lamaz.center_lon);
      h5_att_double(h5_proj, "latitude_of_projection_origin",
		    mp->param.lamaz.center_lat);
      h5_att_double(h5_proj, "false_easting", mp->param.lamaz.false_easting);
      h5_att_double(h5_proj, "false_northing", mp->param.lamaz.false_northing);
      h5_att_str(h5_proj, "projection_x_coordinate", "xgrid");
      h5_att_str(h5_proj, "projection_y_coordinate", "ygrid");
      h5_att_str(h5_proj, "units", "meters");
      h5_att_double(h5_proj, "grid_boundary_top_projected_y", mp->startY);
      fValue = mp->startY + mg->line_count * mp->perY;
      h5_att_double(h5_proj, "grid_boundary_bottom_projected_y", fValue);
      fValue = mp->startX + mg->sample_count * mp->perX;
      h5_att_double(h5_proj, "grid_boundary_right_projected_x", fValue);
      h5_att_double(h5_proj, "grid_boundary_left_projected_x", mp->startX);
      spatial_ref = (char *) MALLOC(sizeof(char)*1024);
      datum = (char *) datum_toString(mp->datum);
      spheroid = (char *) spheroid_toString(mp->spheroid);
      double flat = mp->re_major/(mp->re_major - mp->re_minor);
      sprintf(spatial_ref, "PROJCS[\"Lambert_Azimuthal_Equal_Area\",GEOGCS[\"GCS_%s\",DATUM[\"D_%s\",SPHEROID[\"%s\",%.3f,%-16.11g]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.0174532925199432955]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"False_Easting\",%.3f],PARAMETER[\"False_Northing\",%.3f],PARAMETER[\"Central_Meridian\",%.4f],PARAMETER[\"Latitude_Of_Origin\",%.4f],UNIT[\"Meter\",1]]",
	      datum, datum, spheroid, mp->re_major, flat, 
	      mp->param.lamaz.false_easting, mp->param.lamaz.false_northing,
	      mp->param.lamaz.center_lon, mp->param.lamaz.center_lat);
      h5_att_str(h5_proj, "spatial_ref", spatial_ref);
      sprintf(str_attr, "+proj=laea +lat_0=%.4f +lon_0=%.4f +x_0=%.3f "
	      "+y_0=%.3f", 
	      mp->param.lamaz.center_lat, mp->param.lamaz.center_lon,
	      mp->param.lamaz.false_easting, mp->param.lamaz.false_northing);
      h5_att_str(h5_proj, "proj4text", str_attr);
      h5_att_double(h5_proj, "semimajor_radius", mp->re_major);
      h5_att_double(h5_proj, "semiminor_radius", mp->re_minor);
      sprintf(str_attr, "%.6f %.6f 0 %.6f 0 %.6f", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      h5_att_str(h5_proj, "GeoTransform", str_attr);
    }
  }

  for (ii=0; ii<band_count; ii++) {

    // Create data set
    sprintf(dataset, "/data/%s", band_name[ii]);
    h5_data = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
			H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    h5->var[ii] = h5_data;

    // Add attributes (from CF convention)
    sprintf(str_attr, "%s", mg->sensor);
    if (mg->image_data_type < 9)
      strcat(str_attr, " radar backscatter");
    if (mg->radiometry >= r_SIGMA_DB && mg->radiometry <= r_GAMMA_DB)
      strcat(str_attr, " in dB");
    h5_att_str(h5_data, "long_name", str_attr);
    h5_att_str(h5_data, "cell_methods", "area: backscatter value");
    h5_att_str(h5_data, "units", "1");
    h5_att_str(h5_data, "units_description",
	       "unitless normalized radar cross-section");
    if (mg->radiometry >= r_SIGMA && mg->radiometry <= r_GAMMA)
      strcat(str_attr, " stored as powerscale");
    else if (mg->radiometry >= r_SIGMA_DB && mg->radiometry <= r_GAMMA_DB)
      strcat(str_attr, " stored as dB=10*log10(*)");
    h5_att_float(h5_data, "_FillValue", -999);
    h5_att_str(h5_data, "coordinates", "longitude latitude");
    if (projected)
      h5_att_str(h5_data, "grid_mapping", "projection");

    // Close up
    H5Dclose(h5_data);
  }

  // Extra bands - Time
  asfPrintStatus("Storing band 'time' ...\n");  
  float serial_date = seconds_from_str(mg->acquisition_date);
  sprintf(dataset, "/data/time");
  hid_t h5_string = H5Screate(H5S_SCALAR);
  h5_time = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_string,
		      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_time, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, 
	   &serial_date);
  h5_att_str(h5_time, "units", "seconds since 1900-01-01T00:00:00Z");
  h5_att_str(h5_time, "references", "scene center time");
  h5_att_str(h5_time, "standard_name", "time");
  h5_att_str(h5_time, "axis", "T");
  h5_att_str(h5_time, "long_name", "serial date");
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
  h5_att_str(h5_lon, "units", "degrees_east");
  h5_att_str(h5_lon, "long_name", "longitude");
  h5_att_str(h5_lon, "standard_name", "longitude");
  float valid_range[2] = { -180.0, 180.0 };
  h5_att_float2(h5_lon, "valid_range", valid_range);
  h5_att_float(h5_lon, "_FillValue", -999);
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
  h5_att_str(h5_lat, "units", "degrees_north");
  h5_att_str(h5_lat, "long_name", "latitude");
  h5_att_str(h5_lat, "standard_name", "latitude");
  valid_range[0] = -90.0;
  valid_range[1] = 90.0;
  h5_att_float2(h5_lat, "valid_range", valid_range);
  h5_att_float(h5_lat, "_FillValue", -999);
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
    h5_att_str(h5_ygrid, "units", "meters");
    h5_att_str(h5_ygrid, "long_name", 
	       "projection_grid_y_coordinates");
    h5_att_str(h5_ygrid, "standard_name", 
	       "projection_y_coordinates");
    h5_att_str(h5_ygrid, "axis", "Y");
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
    h5_att_str(h5_xgrid, "units", "meters");
    h5_att_str(h5_xgrid, "long_name", "projection_grid_x_coordinates");
    h5_att_str(h5_xgrid, "standard_name", "projection_x_coordinates");
    h5_att_str(h5_xgrid, "axis", "X");
    H5Dclose(h5_xgrid);
    FREE(xgrids);
  }

  H5Gclose(h5_datagroup);

  // Adding global attributes
  hid_t h5_global = H5Gopen(h5_file, "/", H5P_DEFAULT);
  h5_att_str(h5_global, "institution", "Alaska Satellite Facility");
  sprintf(str_attr, "%s %s %s image", mg->sensor, mg->sensor_name, mg->mode);
  h5_att_str(h5_global, "title", str_attr); 
  if (mg->image_data_type == AMPLITUDE_IMAGE)
    strcpy(str_attr, "SAR backcatter image");
  h5_att_str(h5_global, "source", str_attr);
  h5_att_str(h5_global, "original_file", mg->basename);
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
  h5_att_str(h5_global, "comment", str_attr); 
  h5_att_str(h5_global, "reference",
	     "Documentation available at: www.asf.alaska.edu");
  time_t t;
  struct tm *timeinfo;
  time(&t);
  timeinfo = gmtime(&t);
  sprintf(str_attr, "%s", asctime(timeinfo));
  chomp(str_attr);
  strcat(str_attr, ", UTC: H5 File created.");
  h5_att_str(h5_global, "history", str_attr); 
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
    h5_value_int(h5_file, group, "sar_azimuth_look_count", 
		 ms->azimuth_look_count,
		 "number of looks in azimuth direction", NULL);
    h5_value_int(h5_file, group, "sar_range_look_count", ms->range_look_count,
		 "number of looks in range direction", NULL);
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
  H5Sclose(h5_array);

  // Write ASF metadata to XML file
  char *output_file = 
    (char *) MALLOC(sizeof(char)*(strlen(output_file_name)+5));
  sprintf(output_file, "%s.xml", output_file_name);
  meta_write_xml(md, output_file);
  FREE(output_file);

  return h5;
}


// input is ISO time stamp
// output is seconds since midnight, Jan 1, 1900 (as from date2sec())
static double seconds_from_isotime(iso_dateTime *it)
{
  ymd_date d;
  hms_time t;
  julian_date jd;

  d.year = it->year;
  d.month = it->month;
  d.day = it->day;
  t.hour = it->hour;
  t.min = it->min;
  t.sec = it->second;

  date_ymd2jd(&d, &jd);

  return date2sec(&jd, &t);
}

static void dateTime2str(iso_dateTime timeUTC, char *str)
{
  int year = timeUTC.year;
  int month = timeUTC.month;
  int day = timeUTC.day;
  int hour = timeUTC.hour;
  int min = timeUTC.min;
  double sec = timeUTC.second;

  // UTC time stamp: 2008-03-13T22:19:55.140975
  if (year < 0 || month < 0 || day < 0 || hour < 0 || min < 0 || sec < 0.0)
    strcpy(str, "1900-01-01T00:00:00.000000");
  else
    sprintf(str, "%4d-%02d-%02dT%02d:%02d:%09.6f", 
	    year, month, day, hour, min, sec);
}

static char *imagingMode2str(iso_imageMode_t mode)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (mode == FINE_BEAM)
    strcpy(str, "FINE BEAM");
  else if (mode == STANDARD_BEAM)
    strcpy(str, "STANDARD BEAM");
  else if (mode == STRIPMAP_IMAGE)
    strcpy(str, "STRIPMAP");
  else if (mode == SCANSAR_IMAGE)
    strcpy(str, "SCANSAR");
  else if (mode == SPOTLIGHT_IMAGE)
    strcpy(str, "SPOTLIGHT");
  else if (mode == UNDEF_IMAGE_MODE)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *polLayer2str(iso_polLayer_t pol)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (pol == HH_POL)
    strcpy(str, "HH");
  else if (pol == HV_POL)
    strcpy(str, "HV");
  else if (pol == VH_POL)
    strcpy(str, "VH");
  else if (pol == VV_POL)
    strcpy(str, "VV");
  else if (pol == UNDEF_POL_LAYER)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *polMode2str(iso_polMode_t mode)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (mode == SINGLE_POL)
    strcpy(str, "SINGLE");
  else if (mode == DUAL_POL)
    strcpy(str, "DUAL");
  else if (mode == QUAD_POL)
    strcpy(str, "QUAD");
  else if (mode == UNDEF_POL_MODE)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *lookDir2str(iso_lookDir_t dir)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (dir == RIGHT_LOOK)
    strcpy(str, "RIGHT");
  else if (dir == LEFT_LOOK)
    strcpy(str, "LEFT");
  else if (dir == UNDEF_LOOK)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *product2str(iso_product_t pro)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (pro == SLC_PRODUCT)
    strcpy(str, "SLC");
  else if (pro == STD_PRODUCT)
    strcpy(str, "STD");
  else if (pro == TC_PRODUCT)
    strcpy(str, "TC");
  else if (pro == RTC_PRODUCT)
    strcpy(str, "RTC");
  else if (pro == GEO_PRODUCT)
    strcpy(str, "GEO");
  else if (pro == SSC_PRODUCT)
    strcpy(str, "SSC");
  else if (pro == MGD_PRODUCT)
    strcpy(str, "MGD");
  else if (pro == GEC_PRODUCT)
    strcpy(str, "GEC");
  else if (pro == EEC_PRODUCT)
    strcpy(str, "EEC");
  else if (pro == UNDEF_PRODUCT)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *projection2str(iso_proj_t proj)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (proj == SLANTRANGE_PROJ)
    strcpy(str, "SLANTRANGE");
  else if (proj == GROUNDRANGE_PROJ)
    strcpy(str, "GROUNDRANGE");
  else if (proj == MAP_PROJ)
    strcpy(str, "MAP");
  else if (proj == UNDEF_PROJ)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *resolution2str(iso_resolution_t res)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (res == SE_RES)
    strcpy(str, "SE");
  else if (res == RE_RES)
    strcpy(str, "RE");
  else if (res == UNDEF_RES)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *imageCoord2str(iso_imageCoord_t coord)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (coord == RAW_COORD)
    strcpy(str, "RAW");
  else if (coord == ZERODOPPLER)
    strcpy(str, "ZERODOPPLER");
  else if (coord == UNDEF_COORD)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *dataStart2str(iso_dataStart_t start)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (start == EARLYAZNEARRG)
    strcpy(str, "EARLYAZNEARRG");
  else if (start == EARLYAZFARRG)
    strcpy(str, "EARLYAZFARRG");
  else if (start == LATEAZNEARRG)
    strcpy(str, "LATEAZNEARRG");
  else if (start == LATEAZFARRG)
    strcpy(str, "LATEAZFARRG");
  else if (start == UNDEF_DATA_START)
    strcpy(str, "UNDEFINED");

  return str;
}

static char *orbitAcc2str(iso_orbitAcc_t orbit)
{
  char *str = (char *) MALLOC(sizeof(char)*20);
  
  if (orbit == PREDICTED_ORBIT)
    strcpy(str, "PREDICTED");
  else if (orbit == RESTITUTED_ORBIT)
    strcpy(str, "RESTITUTED");
  else if (orbit == PRECISE_ORBIT)
    strcpy(str, "PRECISE");
  else if (orbit == TLE)
    strcpy(str, "TLE");
  else if (orbit == UNDEF_ORBIT_ACC)
    strcpy(str, "UNDEFINED");

  return str;
}

static h5_t *initialize_h5_file_iso(const char *output_file_name,
				    meta_parameters *md,
				    iso_meta *iso)
{
  hid_t h5_file, h5_datagroup, h5_metagroup, h5_data, h5_proj;
  hid_t h5_array, h5_time, h5_lat, h5_lon, h5_xgrid, h5_ygrid;
  hid_t h5_section, h5_level1, h5_level2, h5_level3, h5_level4;
  int ii, kk, complex=FALSE, projected=FALSE;
  char dataset[50], group[50], str[50], str_attr[50];
  char level1[255], level2[255], level3[255], level4[255];
  char **str_array;
  int *int_array;
  long *long_array;
  double *double_array;
  
  // Convenience pointers
  iso_generalHeader *header = iso->generalHeader;
  iso_productComponents *comps = iso->productComponents;
  iso_productInfo *info = iso->productInfo;
  iso_productSpecific *spec = iso->productSpecific;
  iso_setup *set = iso->setup;
  iso_processing *pro = iso->processing;
  iso_instrument *inst = iso->instrument;
  iso_platform *platform = iso->platform;
  iso_productQuality *quality = iso->productQuality;
  
  // Initialize the HDF pointer structure
  h5_t *h5 = (h5_t *) MALLOC(sizeof(h5_t));
  int band_count = info->numberOfLayers;
  h5->var_count = band_count;
  h5->var = (hid_t *) MALLOC(sizeof(hid_t)*h5->var_count);
  
  // Check for complex data
  if (info->imageDataType == COMPLEX_DATA_TYPE)
    complex = TRUE;
  
  // Create new HDF5 file
  h5_file = 
    H5Fcreate(output_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  h5->file = h5_file;
  
  // Create data space
  int samples = info->numberOfColumns;
  int lines = info->numberOfRows;
  hsize_t dims[2] = { lines, samples };
  hsize_t cdims[2] = { 100, samples };
  h5_array = H5Screate_simple(2, dims, NULL);
  h5->space = h5_array;
  
  // Create data structure
  hid_t h5_plist = H5Pcreate(H5P_DATASET_CREATE);
  H5Pset_chunk(h5_plist, 2, cdims);
  H5Pset_deflate(h5_plist, 6);
  
  // Create a data group
  strcpy(group, "/data");
  h5_datagroup = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);
  for (ii=0; ii<band_count; ii++) {
    
    // Create data set
    sprintf(dataset, "/data/%s", polLayer2str(info->polLayer[ii]));
    h5_data = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
			H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    h5->var[ii] = h5_data;
    
    // Add attributes (from CF convention)
    h5_att_str(h5_data, "long_name", info->pixelValueID);
    h5_att_str(h5_data, "cell_methods", "area: backscatter value");
    h5_att_str(h5_data, "units", "1");
    h5_att_str(h5_data, "units_description",
	       "unitless normalized radar cross-section");
    /* FIXME: where is the radiometry going to be stored?
       if (mg->radiometry >= r_SIGMA && mg->radiometry <= r_GAMMA)
       strcat(str_attr, " stored as powerscale");
       else if (mg->radiometry >= r_SIGMA_DB && mg->radiometry <= r_GAMMA_DB)
       strcat(str_attr, " stored as dB=10*log10(*)");
    */
    h5_att_float(h5_data, "_FillValue", -999);
    h5_att_str(h5_data, "coordinates", "longitude latitude");
    if (projected)
      h5_att_str(h5_data, "grid_mapping", "projection");

    // Close up
    H5Dclose(h5_data);
  }
  // Extra bands - Time
  asfPrintStatus("Storing band 'time' ...\n");  
  float serial_date = 
    seconds_from_isotime(&info->sceneCenterCoord.azimuthTimeUTC);
  strcpy(dataset, "/data/time");
  hid_t h5_string = H5Screate(H5S_SCALAR);
  h5_time = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_string,
		      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(h5_time, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, 
	   &serial_date);
  h5_att_str(h5_time, "units", "seconds since 1900-01-01T00:00:00Z");
  h5_att_str(h5_time, "references", "scene center time");
  h5_att_str(h5_time, "standard_name", "time");
  h5_att_str(h5_time, "axis", "T");
  h5_att_str(h5_time, "long_name", "serial date");
  H5Dclose(h5_time);

  // Extra bands - Longitude
  int nl = info->numberOfRows;
  int ns = info->numberOfColumns;
  long pixel_count = nl*ns;
  double *value = (double *) CALLOC(MAX_PTS, sizeof(double));
  double *l = (double *) CALLOC(MAX_PTS, sizeof(double));
  double *s = (double *) CALLOC(MAX_PTS, sizeof(double));
  double line, sample, lat=0, lon=0, first_value;
  float *lons = (float *) CALLOC(pixel_count, sizeof(float));
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
	 q.K*kk*kk*kk);
      if (lons[ii*ns+kk] > 180.0)
	lons[ii*ns+kk] -= 360.0;
    }
    asfLineMeter(ii, nl);
  }
  asfPrintStatus("Storing band 'longitude' ...\n");
  strcpy(dataset, "/data/longitude");
  h5_lon = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
		     H5P_DEFAULT, h5_plist, H5P_DEFAULT);
  H5Dwrite(h5_lon, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, lons);
  h5_att_str(h5_lon, "units", "degrees_east");
  h5_att_str(h5_lon, "long_name", "longitude");
  h5_att_str(h5_lon, "standard_name", "longitude");
  float valid_range[2] = { -180.0, 180.0 };
  h5_att_float2(h5_lon, "valid_range", valid_range);
  h5_att_float(h5_lon, "_FillValue", -999);
  H5Dclose(h5_lon);
  FREE(lons);
  
  // Extra bands - Latitude
  float *lats = (float *) MALLOC(sizeof(float)*pixel_count);
  asfPrintStatus("Generating band 'latitude' ...\n");
  meta_get_latLon(md, 0, 0, 0.0, &lat, &lon);
  if (lat < 0.0)
    first_value = lat + 180.0;
  else
    first_value = lat;
  asfPrintStatus("Calculating grid for quadratic fit ...\n");
  for (ii=0; ii<RES; ii++) {
    for (kk=0; kk<RES; kk++) {
      line = ii * nl / RES;
      sample = kk * ns / RES;
      meta_get_latLon(md, line, sample, 0.0, &lat, &lon);
      l[ii*RES+kk] = line;
      s[ii*RES+kk] = sample;
      if (lat < 0.0)
	value[ii*RES+kk] = lat + 180.0;
      else
	value[ii*RES+kk] = lat;
    }
    asfLineMeter(ii, nl);
  }
  q = find_quadratic(value, l, s, MAX_PTS);
  q.A = first_value;
  for (ii=0; ii<nl; ii++) {
    for (kk=0; kk<ns; kk++) {
      lats[ii*ns+kk] = (float)
	(q.A + q.B*ii + q.C*kk + q.D*ii*ii + q.E*ii*kk + q.F*kk*kk +
	 q.G*ii*ii*kk + q.H*ii*kk*kk + q.I*ii*ii*kk*kk + q.J*ii*ii*ii +
	 q.K*kk*kk*kk);
      if (lats[ii*ns+kk] > 90)
	lats[ii*ns+kk] -= 180.0;
    }
    asfLineMeter(ii, nl);
  }
  asfPrintStatus("Storing band 'latitude' ...\n");
  strcpy(dataset, "/data/latitude");
  h5_lat = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
		     H5P_DEFAULT, h5_plist, H5P_DEFAULT);
  H5Dwrite(h5_lat, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, lats);
  h5_att_str(h5_lat, "units", "degrees_north");
  h5_att_str(h5_lat, "long_name", "latitude");
  h5_att_str(h5_lat, "standard_name", "latitude");
  valid_range[0] = -90.0;
  valid_range[1] = 90.0;
  h5_att_float2(h5_lat, "valid_range", valid_range);
  h5_att_float(h5_lat, "_FillValue", -999);
  H5Dclose(h5_lat);
  FREE(lats);
  
  if (projected) {
    // Extra bands - ygrid
    float *ygrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<nl; ii++) {
      for (kk=0; kk<ns; kk++)
	ygrids[ii*ns+kk] = md->projection->startY + kk*md->projection->perY;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'ygrid' ...\n");
    strcpy(dataset, "/data/ygrid");
    h5_ygrid = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
			 H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    H5Dwrite(h5_ygrid, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ygrids);
    h5_att_str(h5_ygrid, "units", "meters");
    h5_att_str(h5_ygrid, "long_name", "projection_grid_y_coordinates");
    h5_att_str(h5_ygrid, "standard_name", "projection_y_coordinates");
    h5_att_str(h5_ygrid, "axis", "Y");
    H5Dclose(h5_ygrid);
    FREE(ygrids);
    
    // Extra bands - xgrid
    float *xgrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<nl; ii++) {
      for (kk=0; kk<ns; kk++) 
	xgrids[ii*ns+kk] = md->projection->startX + kk*md->projection->perX;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'xgrid' ...\n");
    strcpy(dataset, "/data/xgrid");
    h5_xgrid = H5Dcreate(h5_file, dataset, H5T_NATIVE_FLOAT, h5_array,
			 H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    H5Dwrite(h5_xgrid, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, xgrids);
    h5_att_str(h5_xgrid, "units", "meters");
    h5_att_str(h5_xgrid, "long_name", "projection_grid_x_coordinates");
    h5_att_str(h5_xgrid, "standard_name", "projection_x_coordinates");
    h5_att_str(h5_xgrid, "axis", "X");
    H5Dclose(h5_xgrid);
    FREE(xgrids);
  }
  H5Gclose(h5_datagroup);
  
  // Adding global attributes
  hid_t h5_global = H5Gopen(h5_file, "/", H5P_DEFAULT);
  h5_att_str(h5_global, "institution", "Alaska Satellite Facility");
  sprintf(str_attr, "%s %s %s image", info->mission, info->sensor, 
	  imagingMode2str(info->imageMode));
  h5_att_str(h5_global, "title", str_attr); 
  if (info->imageDataType == DETECTED_DATA_TYPE ||
      info->imageDataType == COMPLEX_DATA_TYPE)
    strcpy(str_attr, "SAR backcatter image");
  h5_att_str(h5_global, "source", str_attr);
  h5_att_str(h5_global, "original_file", info->productType);
  h5_att_str(h5_global, "comment", info->copyrightInfo); 
  h5_att_str(h5_global, "reference",
	     "Documentation available at: www.asf.alaska.edu");
  time_t t;
  struct tm *timeinfo;
  time(&t);
  timeinfo = gmtime(&t);
  sprintf(str_attr, "%s", asctime(timeinfo));
  chomp(str_attr);
  strcat(str_attr, ", UTC: H5 File created.");
  h5_att_str(h5_global, "history", str_attr); 
  H5Gclose(h5_global);
  
  // Create a metadata group
  sprintf(group, "/metadata");
  h5_metagroup = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);
  
  // On this level we will write the ISO XML metadata string
  
  // General Header
  strcpy(group, "/metadata/generalHeader");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_att_str(h5_section, "file", comps->annotation[0].file.name);
  h5_att_str(h5_section, "fileVersion", "1.0");
  h5_att_str(h5_section, "status", "PRELIMINARY");
  h5_att_str(h5_section, "long_name", 
	     "ground segment general header structure");
  h5_value_str(h5_file, group, "itemName", header->itemName,
	       "contains the name of the exchanged product", NULL);
  h5_value_str(h5_file, group, "mission", header->mission, 
	       "indicates the mission, for which the product is provided", 
	       NULL);
  h5_value_str(h5_file, group, "source", header->source,
	       "specifies the sensor, source or originator for the provided product", 
	       NULL);
  h5_value_str(h5_file, group, "destination", header->destination,
	       "specifies the destination to which provided the product goes", 
	       NULL);
  h5_value_str(h5_file, group, "generationSystem", header->generationSystem,
	       "specifies the system which generated the product", NULL);
  dateTime2str(header->generationTime, str);
  h5_value_str(h5_file, group, "generationTime", str, 
	       "reports UTC time at which the product was generated", NULL);
  if (header->referenceDocument)
    h5_value_str(h5_file, group, "referenceDocument", 
		 header->referenceDocument,
		 "specifies the reference document, in which the product is specified with respect to contents and format", 
		 NULL);
  if (header->revision)
    h5_value_str(h5_file, group, "revision", header->revision,
		 "handle to distinguish between project phases (e.g. testing, commissioning phase, operating)", 
		 NULL);
  if (header->revisionComment)
    h5_value_str(h5_file, group, "revisionComment", header->revisionComment,
		 "remark field for a narrative description of the revision field", 
		 NULL);
  H5Gclose(h5_section);
  
  // Product Components
  strcpy(group, "/metadata/productComponents");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			 H5P_DEFAULT);
  h5_att_str(h5_section, "long_name",
	     "lists and points to the product components");
  if (comps->annotation) {
    strcpy(level1, "/metadata/productComponents/annotation");
    h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    h5_att_str(h5_level1, "long_name", "pointer to the annoation file(s)");
    
    str_array = (char **) MALLOC(sizeof(char *)*comps->numAnnotations);
    for (ii=0; ii<comps->numAnnotations; ii++)
      str_array[ii] = (char *) MALLOC(sizeof(char)*1024);
    long_array = (long *) MALLOC(sizeof(long)*comps->numAnnotations);
    
    for (ii=0; ii<comps->numAnnotations; ii++) {
      if (comps->annotation[ii].type == MAIN_TYPE)
	strcpy(str_array[ii], "MAIN");
      else if (comps->annotation[ii].type == GEOREF_TYPE)
	strcpy(str_array[ii], "GEOREF");
      else if (comps->annotation[ii].type == GEOCODE_TYPE)
	strcpy(str_array[ii], "GEOCODE");
      else if (comps->annotation[ii].type == OTHER_TYPE)
	strcpy(str_array[ii], "OTHER");
      else if (comps->annotation[ii].type == UNDEF_TYPE)
	strcpy(str_array[ii], "UNDEFINED");
    }
    h5_value_str_array(h5_file, level1, "type", str_array, 
		       comps->numAnnotations, "annotation file type", NULL);
    strcpy(level2, "/metadata/productComponents/annotation/file");
    h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    strcpy(level3, "/metadata/productComponents/annotation/file/location");
    h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    for (ii=0; ii<comps->numAnnotations; ii++)
      strcpy(str_array[ii], comps->annotation[ii].file.host);
    h5_value_str_array(h5_file, level3, "host", str_array, 
		       comps->numAnnotations,
		       "host where the file is located; default to: .", 
		       NULL);
    for (ii=0; ii<comps->numAnnotations; ii++)
      strcpy(str_array[ii], comps->annotation[ii].file.path);
    h5_value_str_array(h5_file, level3, "path", str_array, 
		       comps->numAnnotations,
		       "path where the file is located; default to: .", 
		       NULL);
    for (ii=0; ii<comps->numAnnotations; ii++)
      strcpy(str_array[ii], comps->annotation[ii].file.name);
    h5_value_str_array(h5_file, level3, "filename", str_array, 
		       comps->numAnnotations, "name of the file", NULL);
    H5Gclose(h5_level3);
    for (ii=0; ii<comps->numAnnotations; ii++)
      long_array[ii] = comps->annotation[ii].file.size;
    h5_value_long_array(h5_file, level2, "size", long_array, 
			comps->numAnnotations, "size of the file", "bytes");
    H5Gclose(h5_level2);
    H5Gclose(h5_level1);
    for (ii=0; ii<comps->numAnnotations; ii++)
      FREE(str_array[ii]);
    FREE(str_array);
    FREE(long_array);
  }
  
  if (comps->imageData) {
    for (ii=0; ii<comps->numLayers; ii++) {
      strcpy(level1, "/metadata/productComponents/imageData");
      h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_att_int(h5_level1, "layerIndex", ii+1);
      h5_att_str(h5_level1, "long_name", 
		 "containing one or more polarimetric channels in separate binary data matrices");
      h5_value_str(h5_file, level1, "polLayer", 
		   polLayer2str(comps->imageData[ii].polLayer),
		   "pointer to product components", NULL);
      sprintf(level2, "/metadata/productComponents/imageData/file");
      h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      sprintf(level3, "/metadata/productComponents/imageData/file/location");
      h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_value_str(h5_file, level3, "host", comps->imageData[ii].file.host,
		   "host where the file is located; default to: .", NULL);
      h5_value_str(h5_file, level3, "path", comps->imageData[ii].file.path,
		   "path where the file is located; default to: .", NULL);
      h5_value_str(h5_file, level3, "filename", 
		   comps->imageData[ii].file.name, "name of the file", NULL);
      H5Gclose(h5_level3);
      h5_value_long(h5_file, level2, "size", comps->imageData[ii].file.size,
		    "size of the file", "bytes");
      H5Gclose(h5_level2);
      H5Gclose(h5_level1);
    }
  }
  
  if (comps->quicklooks) {
    for (ii=0; ii<comps->numLayers; ii++) {
      strcpy(level1, "/metadata/productComponents/quicklooks");
      h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_att_int(h5_level1, "layerIndex", ii+1);
      h5_att_str(h5_level1, "long_name", 
		 "rescaled images for each image layer");
      h5_value_str(h5_file, level1, "polLayer", 
		   polLayer2str(comps->quicklooks[ii].polLayer),
		   "pointer to product components", NULL);
      strcpy(level2, "/metadata/productComponents/quicklooks/file");
      h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      strcpy(level3, "/metadata/productComponents/quicklooks/file/location");
      h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_value_str(h5_file, level3, "host", comps->quicklooks[ii].file.host,
		   "host where the file is located; default to: .", NULL);
      h5_value_str(h5_file, level3, "path", comps->quicklooks[ii].file.path,
		   "path where the file is located; default to: .", NULL);
      h5_value_str(h5_file, level3, "filename", 
		   comps->quicklooks[ii].file.name, "name of the file", 
		   NULL);
      H5Gclose(h5_level3);
      h5_value_long(h5_file, level2, "size", comps->quicklooks[ii].file.size,
		    "size of the file", "bytes");
      H5Gclose(h5_level2);
      H5Gclose(h5_level1);
    }
  }
  
  strcpy(level1, "/metadata/productComponents/browseImage");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", "one low resolution quicklook");
  strcpy(level2, "/metadata/productComponents/browseImage/file");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  strcpy(level3, "/metadata/productComponents/browseImage/file/location");
  h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_value_str(h5_file, level3, "host", comps->browseImage.host, 
	       "host where the file is located; default to: .", NULL);
  h5_value_str(h5_file, level3, "path", comps->browseImage.path, 
	       "path where the file is located; default to: .", NULL);
  h5_value_str(h5_file, level3, "filename", comps->browseImage.name, 
	       "name of the file", NULL);
  H5Gclose(h5_level3);
  h5_value_long(h5_file, level2, "size", comps->browseImage.size, 
		"size of the file", "bytes");
  H5Gclose(h5_level2);
  H5Gclose(h5_level1);
  
  strcpy(level1, "/metadata/productComponents/mapPlot");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", 
	     "geographic map showing the image footprint");
  strcpy(level2, "/metadata/productComponents/mapPlot/file");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  strcpy(level3, "/metadata/productComponents/mapPlot/file/location");
  h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_value_str(h5_file, level3, "host", comps->mapPlot.host, 
	       "host where the file is located; default to: .", NULL);
  h5_value_str(h5_file, level3, "path", comps->mapPlot.path, 
	       "path where the file is located; default to: .", NULL);
  h5_value_str(h5_file, level3, "filename", comps->mapPlot.name, 
	       "name of the file", NULL);
  H5Gclose(h5_level3);
  h5_value_long(h5_file, level2, "size", comps->mapPlot.size, 
		"size of the file", "bytes");
  H5Gclose(h5_level2);
  H5Gclose(h5_level1);
  
  H5Gclose(h5_section);
  
  // Product Info
  strcpy(group, "/metadata/productInfo");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_att_str(h5_section, "long_name",
	     "general mission, scene, imaging mode related parameters");
  strcpy(level1, "/metadata/productInfo/generationInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name",
	     "key parameters of the product generation and delivery");
  h5_value_str(h5_file, level1, "logicalProductID", info->logicalProductID,
	       "logical ID", NULL);
  h5_value_str(h5_file, level1, "receivingStation", info->receivingStation,
	       "receiving ground station", NULL);
  h5_value_str(h5_file, level1, "level0ProcessingFacility", 
	       info->level0ProcessingFacility, 
	       "facility processing the level zero data", NULL);
  h5_value_str(h5_file, level1, "level1ProcessingFacility",
	       info->level1ProcessingFacility, 
	       "facility processing the level one data", NULL);
  if (info->groundOperationsType == OPERATIONAL)
    strcpy(str, "OPERATIONAL");
  else if (info->groundOperationsType == PREOPERATIONAL)
    strcpy(str, "PRE-OPERATIONAL");
  else if (info->groundOperationsType == INSTRUMENT)
    strcpy(str, "INSTRUMENT");
  else if (info->groundOperationsType == TEST_OPS)
    strcpy(str, "TEST");
  else if (info->groundOperationsType == UNDEF_OPS)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level1, "groundOperationsType", str,
	       "OPERATIONAL, PRE-OPERATIONAL, INSTRUMENT or TEST", NULL);
  h5_value_str(h5_file, level1, "deliveryInfo", info->deliveryInfo, 
	       "delivery information", NULL);
  h5_value_str(h5_file, level1, "copyrightInfo", info->copyrightInfo, 
	       "general copyright remark", NULL);
  strcpy(level2, "/metadata/productInfo/generationInfo/qualityInfo");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
H5P_DEFAULT);
  if (info->qualityInspection == AUTO_APPROVED)
    strcpy(str, "AUTO APPROVED");
  else if (info->qualityInspection == OPERATOR_APPROVED)
    strcpy(str, "OPERATOR APPROVED");
  else if (info->qualityInspection == NOT_APPROVED)
    strcpy(str, "NOT APPROVED");
  else if (info->qualityInspection == UNDEF_QUALITY)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level2, "qualityInspection", str, 
	       "AUTO APPROVED, OPERATOR APPROVED, NOT APPROVED or UNDEFINED",
	       NULL);
  H5Gclose(h5_level2);
  if (info->qualityRemark)
    h5_value_str(h5_file, level1, "qualityRemark", info->qualityRemark, 
		 "Additional information on quality", NULL);
  H5Gclose(h5_level1);
  
  strcpy(level1, "/metadata/productInfo/missionInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", "mission and orbit parameters");
  h5_value_str(h5_file, level1, "mission", info->mission, 
	       "mission abbreviation", NULL);
  h5_value_int(h5_file, level1, "orbitPhase", info->orbitPhase,
	       "orbit phase: 1 prelaunch phase, 0 launch phase, 1 nominal orbit", NULL);
  h5_value_int(h5_file, level1, "orbitCycle", info->orbitCycle, 
	       "cycle number", NULL);
  h5_value_int(h5_file, level1, "absOrbit", info->absOrbit,
	       "absolute orbit number at the start of the scene", NULL);
  h5_value_int(h5_file, level1, "relOrbit", info->relOrbit, 
	       "relative orbit", NULL);
  h5_value_int(h5_file, level1, "numOrbitsInCycle", info->numOrbitsInCycle,
	       "nominal number of orbits per cycle", NULL);
  if (info->orbitDirection == ASCENDING)
    strcpy(str, "ASCENDING");
  else if (info->orbitDirection == DESCENDING)
    strcpy(str, "DESCENDING");
  else if (info->orbitDirection == UNDEF_ORBIT)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level1, "orbitDirection", str, 
	       "ASCENDING or DESCENDING", NULL);
  H5Gclose(h5_level1);
  
  sprintf(level1, "/metadata/productInfo/acquisitionInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name",
	     "SAR sensor configuration and instrument modes during acquisition");
  h5_value_str(h5_file, level1, "sensor", info->sensor, 
	       "sensor identifier: SAR", NULL);
  h5_value_str(h5_file, level1, "imagingMode", 
	       imagingMode2str(info->imageMode), 
	       "FINE BEAM, STANDARD BEAM, STRIPMAP, SCANSAR or SPOTLIGHT", 
	       NULL);
  h5_value_str(h5_file, level1, "lookDirection", 
	       lookDir2str(info->lookDirection),
	       "LEFT or RIGHT", NULL);
  h5_value_str(h5_file, level1, "polarizationMode", 
	       polMode2str(info->polarizationMode), "SINGLE, DUAL or QUAD", 
	       NULL);
  strcpy(level2, "/metadata/productInfo/acquisitionInfo/polarizationList");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  str_array = (char **) MALLOC(sizeof(char *)*comps->numLayers);
  for (ii=0; ii<comps->numLayers; ii++)
    str_array[ii] = (char *) MALLOC(sizeof(char)*1024);
  for (ii=0; ii<comps->numLayers; ii++) 
    strcpy(str_array[ii], polLayer2str(info->polLayer[ii]));
  h5_value_str_array(h5_file, level2, "polLayer", str_array, 
		     comps->numLayers, "HH, HV, VH or VV", NULL);
  for (ii=0; ii<comps->numLayers; ii++)
    FREE(str_array[ii]);
  FREE(str_array);
  H5Gclose(h5_level2);  
  h5_value_str(h5_file, level1, "elevationBeamConfiguration", 
	       info->elevationBeamConfiguration, 
	       "beam identification as taken from the order file", NULL);
  strcpy(level2, "/metadata/productInfo/acquisitionInfo/imagingModeSpecificInfo");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  if (info->imageMode == FINE_BEAM ||
      info->imageMode == STANDARD_BEAM ||
      info->imageMode == STRIPMAP_IMAGE) {
    if (info->imageMode == FINE_BEAM)
      strcpy(level3, "/metadata/productInfo/acquisitionInfo/imagingModeSpecificInfo/fineBeam");
    else if (info->imageMode == STANDARD_BEAM)
      strcpy(level3, "/metadata/productInfo/acquisitionInfo/imagingModeSpecificInfo/standardBeam");
    else if (info->imageMode == STRIPMAP_IMAGE)
      strcpy(level3, "/metadata/productInfo/acquisitionInfo/imagingModeSpecificInfo/stripMap");
    h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    h5_value_str(h5_file, level3, "azimuthBeamID", info->azimuthBeamID, 
		 "azimuth beam ID", NULL);
  }
  else if (info->imageMode == SCANSAR_IMAGE) {
    strcpy(level3, "/metadata/productInfo/acquisitionInfo/imagingModeSpecificInfo/scanSAR");
    h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    h5_value_int(h5_file, level3, "numberOfBeams", info->numberOfBeams,
		 "number of beams", NULL);
    strcpy(level4, "/metadata/productInfo/acquisitionInfo/imagingModeSpecificInfo/scanSAR/beamList");
    h5_level4 = H5Gcreate(h5_file, level4, H5P_DEFAULT, H5P_DEFAULT,
			  H5P_DEFAULT);
    for (ii=0; ii<info->numberOfBeams; ii++)
      strcpy(str_array[ii], info->beamID[ii]);
    h5_value_str_array(h5_file, level4, "beamID", str_array, 
		       info->numberOfBeams, "beam ID", NULL);
    H5Gclose(h5_level4);
    h5_value_str(h5_file, level1, "azimuthBeamID", info->azimuthBeamID,
		 "azimuth beam ID", NULL);
    h5_value_int(h5_file, level1, "numberOfBursts", info->numberOfBursts,
		 "number of bursts", NULL);
    H5Gclose(h5_level3);
  }
  else if (info->imageMode == SPOTLIGHT_IMAGE) {
    strcpy(level3, "/metadata/productInfo/acquisitionInfo/imagingModeSpecificInfo/spotLight");
    h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    h5_value_int(h5_file, level1, "numberOfAzimuthBeams", 
		 info->numberOfAzimuthBeams, "number of azimuth beams", 
		 NULL);
    h5_value_str(h5_file, level1, "azimuthBeamIDFirst", 
		 info->azimuthBeamIDFirst, 
		 "azimuth beam ID of first azimuth pattern used", NULL);
    h5_value_str(h5_file, level1, "azimuthBeamIDLast", 
		 info->azimuthBeamIDLast, 
		 "azimuth beam ID of last azimuth pattern used", NULL);
    h5_value_double(h5_file, level1, "azimuthSteeringAngleFirst",
		   info->azimuthSteeringAngleFirst, 
		   "azimuth steering angle of first azimuth beam", 
		    "degrees");
    h5_value_double(h5_file, level1, "azimuthSteeringAngleLast",
		   info->azimuthSteeringAngleLast, 
		   "azimuth steering angle of last azimuth beam", "degrees");
    H5Gclose(h5_level3);    
  }
  H5Gclose(h5_level2);    
  H5Gclose(h5_level1);
  
  strcpy(level1, "/metadata/productInfo/productVariantInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", "product variant description");
  h5_value_str(h5_file, level1, "productType", info->productType,
	       "mnemonic", NULL);
  h5_value_str(h5_file, level1, "productVariant", 
	       product2str(info->productVariant), 
	       "SLC, STD, TC, RTC, GEO, SSC, MGD, GEC or EEC", NULL);
  h5_value_str(h5_file, level1, "projection", 
	       projection2str(info->projection), 
	       "GROUNDRANGE, SLANTRANGE or MAP", NULL);
  if (info->projection == MAP_PROJ) {
    if (info->mapProjection == UTM_PROJ)
      strcpy(str, "UTM");
    else if (info->mapProjection == PS_PROJ)
      strcpy(str, "POLARSTEREOGRAPHIC");
    else if (info->mapProjection == GEOG_PROJ)
      strcpy(str, "GEOGRAPHIC");
    else if (info->mapProjection == UNDEF_MAP)
      strcpy(str, "UNDEFINED");
    h5_value_str(h5_file, level1, "mapProjection", str, 
		 "for geocoding only: UTM, POLARSTEREOGRAPHIC or GEOGRAPHIC",
		 NULL);
  }
  h5_value_str(h5_file, level1, "resolutionVariant", 
	       resolution2str(info->resolutionVariant), "SE or RE", NULL);
  if (info->radiometricCorrection == CALIBRATED)
    strcpy(str, "CALIBRATED");
  else if (info->radiometricCorrection == RELCALIBRATED)
    strcpy(str, "RELATIVE CALIBRATED");
  else if (info->radiometricCorrection == NOTCALIBRATED)
    strcpy(str, "NOT CALIBRATED");
  else if (info->radiometricCorrection == UNDEF_CAL)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level1, "radiometricCorrection", str, 
	       "CALIBRATED, RELATIVE CALIBRATED or NOT CALIBRATED", NULL);
  H5Gclose(h5_level1);
  
  strcpy(level1, "/metadata/productInfo/imageDataInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", "image layer format");
  h5_value_str(h5_file, level1, "pixelValueID", info->pixelValueID,
	       "complex amplitude and phase, radar brightness (beta nought), sigma nought", NULL);
  if (info->imageDataType == DETECTED_DATA_TYPE)
    strcpy(str, "DETECTED");
  else if (info->imageDataType == COMPLEX_DATA_TYPE)
    strcpy(str, "COMPLEX");
  else if (info->imageDataType == RAW_DATA_TYPE)
    strcpy(str, "RAW");
  else if (info->imageDataType == UNDEF_DATA_TYPE)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level1, "imageDataType", str, 
	       "DETECTED, COMPLEX or RAW", NULL);
  if (info->imageDataFormat == CEOS_DATA_FORMAT)
    strcpy(str, "CEOS");
  else if (info->imageDataFormat == GEOTIFF_DATA_FORMAT)
    strcpy(str, "GEOTIFF");
  else if (info->imageDataFormat == HDF5_DATA_FORMAT)
    strcpy(str, "HDF5");
  else if (info->imageDataFormat == COSAR_DATA_FORMAT)
    strcpy(str, "COSAR");
  else if (info->imageDataFormat == UNDEF_DATA_FORMAT)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level1, "imageDataFormat", str, 
	       "CEOS, GEOTIFF, HDF5 or COSAR", NULL);
  h5_value_int(h5_file, level1, "numberOfLayers", info->numberOfLayers, 
	       "number of polarizations + elevation beams", NULL);
  h5_value_int(h5_file, level1, "imageDataDepth", info->imageDataDepth, 
	       "bits per pixel", NULL);
  if (info->imageStorageOrder == ROWBYROW)
    strcpy(str, "ROWBYROW");
  else if (info->imageStorageOrder == COLBYCOL)
    strcpy(str, "COLBYCOL");
  else if (info->imageStorageOrder == UNDEF_STORE)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level1, "imageStoreOrder", str, 
	       "ROWBYROW or COLBYCOL", NULL);
  h5_value_str(h5_file, level1, "rowContent", info->rowContent, 
	       "rangelines, northing (for geocoded products), x", NULL);
  h5_value_str(h5_file, level1, "columnContent", info->columnContent, 
	       "azimuth, y", NULL);
  strcpy(level2, "/metadata/productInfo/imageDataInfo/imageRaster");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_int(h5_file, level2, "numberOfRows", info->numberOfRows, 
	       "line count", NULL);
  h5_value_int(h5_file, level2, "numberOfColumns", info->numberOfColumns, 
	       "sample count", NULL);
  h5_value_int(h5_file, level2, "startRow", info->startRow, 
	       "start row of a subset", NULL);
  h5_value_int(h5_file, level2, "startColumn", info->startColumn, 
	       "start column of a subset", NULL);
  h5_value_double(h5_file, level2, "rowScaling", info->rowScaling, 
		  "row scaling factor, in case the image has been resampled", 
		  NULL);
  h5_value_double(h5_file, level2, "columnScaling", info->columnScaling, 
		  "column scaling factor, in case the image has been resampled",
		  NULL);
  if (info->projection == MAP_PROJ)
    h5_value_double(h5_file, level2, "rowSpacing", info->rowSpacing, 
		   "spacing of samples within a row from common raster [s or m]", "m");  
  else
    h5_value_double(h5_file, level2, "rowSpacing", info->rowSpacing,
		   "spacing of samples within a row from common raster [s or m]", "s");
  if (info->projection == MAP_PROJ)
    h5_value_double(h5_file, level2, "columnSpacing", info->columnSpacing, 
		   "spacing within a column [s or m]", "m");
  else
    h5_value_double(h5_file, level2, "columnSpacing", info->columnSpacing, 
		   "spacing within a column [s or m]", "s");
  h5_value_double(h5_file, level2, "groundRangeResolution", 
		  info->groundRangeResolution, 
		  "nominal worst case (at near range) [m]", "m");
  h5_value_double(h5_file, level2, "azimuthResolution",
		  info->azimuthResolution, "nominal [m]", "m");
  h5_value_double(h5_file, level2, "azimuthLooks", info->azimuthLooks, 
		  "effective number of looks", NULL);
  h5_value_double(h5_file, level2, "rangeLooks", info->rangeLooks, 
		  "number of range looks", NULL);
  H5Gclose(h5_level2);
  H5Gclose(h5_level1);
  
  strcpy(level1, "/metadata/productInfo/sceneInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_time", "time and scene location information");
  h5_value_str(h5_file, level1, "sceneID",  info->sceneID, 
	       "orbit + time information", NULL);
  strcpy(level2, "/metadata/productInfo/sceneInfo/start");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  dateTime2str(info->startTimeUTC, str);
  h5_value_str(h5_file, level2, "timeUTC", str, "time stamp in UTC", NULL);
  H5Gclose(h5_level2);
  strcpy(level2, "/metadata/productInfo/sceneInfo/stop");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  dateTime2str(info->stopTimeUTC, str);
  h5_value_str(h5_file, level2, "timeUTC", str, "time stamp in UTC", NULL);
  H5Gclose(h5_level2);
  strcpy(level2, "/metadata/productInfo/sceneInfo/rangeTime");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_double(h5_file, level2, "firstPixel", info->rangeTimeFirstPixel,
		  "minimum range time of entire scene [s]", "s");
  h5_value_double(h5_file, level2, "lastPixel", info->rangeTimeLastPixel,
		  "maximum range time of entire scene [s]", "s");
  H5Gclose(h5_level2);
  h5_value_double(h5_file, level1, "sceneAzimuthExtent",
		  info->sceneAzimuthExtent, 
		  "(approximate and at mid-range) in [m]", "m");
  h5_value_double(h5_file, level1, "sceneRangeExtent", 
		  info->sceneRangeExtent, "(approximate and at mid-range) in [m]",
		  "m");
  strcpy(level2, "/metadata/productInfo/sceneInfo/sceneCenterCoord");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  if (info->sceneCenterCoord.refRow)
    h5_value_int(h5_file, level2, "refRow", info->sceneCenterCoord.refRow,
		 "position in image row", NULL);
  if (info->sceneCenterCoord.refColumn)
    h5_value_int(h5_file, level2, "refColumn", 
		 info->sceneCenterCoord.refColumn,
		 "position in image column", NULL);
  h5_value_double(h5_file, level2, "lat", info->sceneCenterCoord.lat,
		 "geographical latitude [degrees]", "degrees");
  h5_value_double(h5_file, level2, "lon", info->sceneCenterCoord.lon,
		 "geographical longitude [degrees]", "degrees");
  dateTime2str(info->sceneCenterCoord.azimuthTimeUTC, str);
  h5_value_str(h5_file, level2, "azimuthTimeUTC", str, 
	       "time stamp at UTC for coordinate", NULL);
  h5_value_double(h5_file, level2, "rangeTime", 
		  info->sceneCenterCoord.rangeTime, "range time [s]", "s");
  h5_value_double(h5_file, level2, "incidenceAngle",
		  info->sceneCenterCoord.incidenceAngle, 
		  "incidence angle [degrees]", "degrees");
  H5Gclose(h5_level2);
  h5_value_double(h5_file, level1, "sceneAverageHeight", 
		  info->sceneAverageHeight,
		  "with respect to reference frame [m]", "m");
  
  int_array = (int *) MALLOC(sizeof(int)*4);
  double_array = (double *) MALLOC(sizeof(double)*4);
  str_array = (char **) MALLOC(sizeof(char *)*4);
  for (ii=0; ii<4; ii++)
    str_array[ii] = (char *) MALLOC(sizeof(char)*30);
  
  strcpy(level2, "/metadata/productInfo/sceneInfo/sceneCornerCoord");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  if (info->sceneCornerCoord[ii].refRow) {
    for (ii=0; ii<4; ii++) 
      int_array[ii] = info->sceneCornerCoord[ii].refRow;
    h5_value_int_array(h5_file, level2, "refRow", int_array, 4, 
		       "position in image row", NULL);
  }
  if (info->sceneCornerCoord[ii].refColumn) {
    for (ii=0; ii<4; ii++)
      int_array[ii] = info->sceneCornerCoord[ii].refColumn;
    h5_value_int_array(h5_file, level2, "refColumn", int_array, 4, 
		       "position in image column", NULL);
  }
  for (ii=0; ii<4; ii++)
    double_array[ii] = info->sceneCornerCoord[ii].lat;
  h5_value_double_array(h5_file, level2, "lat", double_array, 4,
		       "geographical latitude [degrees]", "degrees");
  for (ii=0; ii<4; ii++)
    double_array[ii] = info->sceneCornerCoord[ii].lon;
  h5_value_double_array(h5_file, level2, "lon", double_array, 4,
		       "geographical longitude [degrees]", "degrees");
  for (ii=0; ii<4; ii++)
    dateTime2str(info->sceneCornerCoord[ii].azimuthTimeUTC, str_array[ii]);
  h5_value_str_array(h5_file, level2, "azimuthTimeUTC", str_array, 4,
		     "time stamp at UTC for coordinate", NULL);
  for (ii=0; ii<4; ii++)
    double_array[ii] = info->sceneCornerCoord[ii].rangeTime;
  h5_value_double_array(h5_file, level2, "rangeTime", double_array, 4,
			"range time [s]", "s");
  for (ii=0; ii<4; ii++)
    double_array[ii] = info->sceneCornerCoord[ii].incidenceAngle;
  h5_value_double_array(h5_file, level2, "incidenceAngle", double_array, 4,
			"incidence angle [degrees]", "degrees");
  H5Gclose(h5_level2);
  h5_value_double(h5_file, level1, "yaw", info->yaw,
		 "yaw angle of the satellite [degrees]", "degrees");
  h5_value_double(h5_file, level1, "pitch", info->pitch,
		 "pitch angle of the satellite [degrees]", "degrees");
  h5_value_double(h5_file, level1, "roll", info->roll,
		 "roll angle of the satellite [degrees]", "degrees");
  h5_value_double(h5_file, level1, "headingAngle", info->headingAngle,
		 "rotation of azimuth heading clockwise with respect to north [degrees]", "degrees");
  h5_value_double(h5_file, level1, "earthRadius", info->earthRadius,
		 "earth radius at the scene center [m]", "m");
  h5_value_double(h5_file, level1, "satelliteHeight", info->satelliteHeight,
		 "satellite height at the scene center from earth center [m]", "m");
  FREE(int_array);
  FREE(double_array);
  for (ii=0; ii<4; ii++)
    FREE(str_array[ii]);
  FREE(str_array);
  H5Gclose(h5_level1);
  
  strcpy(level1, "/metadata/productInfo/previewInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", "quicklook information"); 
  strcpy(level2, "/metadata/productInfo/previewInfo/quicklooks");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_str(h5_file, level2, "imageDataFormat", 
	       info->quicklooks.imageDataFormat, 
	       "image data format (TIFF greyscale)", NULL);
  h5_value_int(h5_file, level2, "imageDataDepth", 
	       info->quicklooks.imageDataDepth, "image data depth (16 bit)", 
	       NULL);
  strcpy(level3, "/metadata/productInfo/previewInfo/quicklooks/imageRaster");
  h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_int(h5_file, level3, "numberOfRows", 
	       info->quicklooks.numberOfRows, "line count", NULL);
  h5_value_int(h5_file, level3, "numberOfColumns", 
	       info->quicklooks.numberOfColumns, "sample count", NULL);
  h5_value_double(h5_file, level3, "columnBlockLength",
		  info->quicklooks.columnBlockLength, 
		  "resampling factor [pixels]", "pixels");
  h5_value_double(h5_file, level3, "rowBlockLength",
		  info->quicklooks.rowBlockLength, 
		  "resampling factor [pixels]", "pixels");
  h5_value_double(h5_file, level3, "rowSpacing", info->quicklooks.rowSpacing,
		 "spacing of samples within row from common raster [s or m]", 
		 "m");
  h5_value_double(h5_file, level3, "columnSpacing", 
		  info->quicklooks.columnSpacing,
		 "spacing within a column [s or m]", "m");
  H5Gclose(h5_level2);
  strcpy(level2, "/metadata/productInfo/previewInfo/browseImage");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_str(h5_file, level2, "imageDataFormat", 
	       info->browseImageDataFormat,
	       "image data format (TIFF or JPEG)", NULL);
  h5_value_int(h5_file, level2, "imageDataDepth", info->browseImageDataDepth,
	       "image data depth (24 bit)", NULL);
  H5Gclose(h5_level2);
  h5_value_str(h5_file, level1, "mapPlotFormat", info->mapPlotFormat,
	       "map plot format (PNG, GIF or KML)", NULL);
  H5Gclose(h5_level1);
  H5Gclose(h5_section);
  
  // Product Specific
  strcpy(group, "/metadata/productSpecific");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			 H5P_DEFAULT);
  h5_att_str(h5_section, "long_name", "specific information for product");
  strcpy(level1, "/metadata/productSpecific/complexImageInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_double(h5_file, level1, "commonPRF", spec->commonPRF, 
		  "intermediate \"output PRF\" used during processing [Hz]", 
		  "Hz");
  h5_value_double(h5_file, level1, "commonRSF", spec->commonRSF, 
		  "range sampling frequency [Hz]", "Hz");
  h5_value_double(h5_file, level1, "slantRangeResolution", 
		  spec->slantRangeResolution, "slant range resolution [m]", 
		  "m");
  h5_value_double(h5_file, level1, "projectedSpacingAzimuth",
		 spec->projectedSpacingAzimuth, 
		 "nominal projected pixel spacing on ground at scene center [m]", "m");
  strcpy(level2, "/metadata/productSpecific/complexImageInfo/projectedSpacingRange");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_double(h5_file, level2, "groundNear", 
		  spec->projectedSpacingGroundNearRange, 
		  "ground near range [m]", "m");
  h5_value_double(h5_file, level2, "groundFar",
		 spec->projectedSpacingGroundFarRange, "ground far range [m]", 
		 "m");
  h5_value_double(h5_file, level2, "slantRange",
		 spec->projectedSpacingSlantRange, "slant range [m]", "m");
  H5Gclose(h5_level2);
  h5_value_str(h5_file, level1, "imageCoordinateType", 
	       imageCoord2str(spec->imageCoordinateType), 
	       "RAW or ZERODOPPLER", NULL);
  h5_value_str(h5_file, level1, "imageDataStartWith", 
	       dataStart2str(spec->imageDataStartWith), 
	       "EARLYAZNEARRG, EARLYAZFARRG, LATEAZNEARRG or LATEAZFARRG", 
	       NULL);
  h5_value_str(h5_file, level1, "quicklookDataStartWith", 
	       dataStart2str(spec->quicklookDataStartWith), 
	       "EARLYAZNEARRG, EARLYAZFARRG, LATEAZNEARRG or LATEAZFARRG", 
	       NULL);
  
  H5Gclose(h5_level1);
  H5Gclose(h5_section);
  
  // Setup
  strcpy(group, "/metadata/setup");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			 H5P_DEFAULT);
  h5_att_str(h5_section, "long_name",
	     "screening and processing chain setup and control parameters etc. used");
  strcpy(level1, "/metadata/setup/orderInfo");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_str(h5_file, level1, "orderType", set->orderType, 
	       "order type (e.g. L1B-SAR)", NULL);
  h5_value_str(h5_file, level1, "processingPriority", 
	       set->processingPriority,
	       "processing priority (e.g. NRT)", NULL);
  h5_value_str(h5_file, level1, "orbitAccuracy", 
	       orbitAcc2str(set->orbitAccuracy), 
	       "PREDICTED, RESTITUTED, PRECISE or TLE", NULL);
  if (set->sceneSpecification == FRAME_SPEC)
    strcpy(str, "FRAME");
  else if (set->sceneSpecification == TIME_SPEC)
    strcpy(str, "TIME");
  else if (set->sceneSpecification == CENTERCOORDS_SPEC)
    strcpy(str, "CENTERCOORDS");
  else if (set->sceneSpecification == UNDEF_SCENE_SPEC)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level1, "sceneSpecification", str, 
	       "FRAME, TIME or CENTERCOORDS", NULL);
  strcpy(level2, "/metadata/setup/orderInfo/orderedScene");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  if (set->sceneSpecification == FRAME_SPEC)
    h5_value_int(h5_file, level2, "frameID", set->frameID, "frame number", 
		 NULL);
  else if (set->sceneSpecification == TIME_SPEC) {
    strcpy(level3, "/metadata/setup/orderInfo/orderedScene/sceneExtent");
    h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    dateTime2str(set->sceneStartTimeUTC, str);
    h5_value_str(h5_file, level3, "startTimeUTC", str, 
		 "scene start time in UTC", NULL);
    dateTime2str(set->sceneStopTimeUTC, str);
    h5_value_str(h5_file, level3, "stopTimeUTC", str, 
		 "scene stop time in UTC", NULL);
    H5Gclose(h5_level2);
  }
  else if (set->sceneSpecification == CENTERCOORDS_SPEC) {
    strcpy(level3, "/metadata/setup/orderInfo/orderedScene/sceneCenterCoord");
    h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    h5_value_double(h5_file, level3, "lat", set->sceneCenterLatitude, 
		   "scene geographical center latitude [degrees]", 
		    "degrees");
    h5_value_double(h5_file, level3, "lon", set->sceneCenterLongitude,
		   "scene geographical center longitude [degrees]", 
		    "degrees");
    H5Gclose(h5_level2);
  }
  h5_value_str(h5_file, level2, "imagingMode", 
	       imagingMode2str(set->imagingMode), 
	       "FINE BEAM, STANDARD BEAM, STRIPMAP, SCANSAR or SPOTLIGHT", 
	       NULL);
  h5_value_str(h5_file, level2, "lookDirection", 
	       lookDir2str(set->lookDirection), "LEFT or RIGHT", NULL);
  h5_value_str(h5_file, level2, "polarizationMode", 
	       polMode2str(set->polarizationMode), 
	       "SINGLE, DUAL or QUAD", NULL);
  strcpy(level3, "/metadata/setup/orderInfo/orderedScene/polList");
  h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  str_array = (char **) MALLOC(sizeof(char *)*comps->numLayers);
  for (ii=0; ii<comps->numLayers; ii++)
    str_array[ii] = (char *) MALLOC(sizeof(char)*1024);
  for (ii=0; ii<comps->numLayers; ii++) 
    strcpy(str_array[ii], polLayer2str(info->polLayer[ii]));
  h5_value_str_array(h5_file, level3, "polLayer", str_array, 
		     comps->numLayers, "HH, HV, VH or VV", NULL);
  for (ii=0; ii<comps->numLayers; ii++)
    FREE(str_array[ii]);
  FREE(str_array);
  H5Gclose(h5_level3);  
  h5_value_str(h5_file, level2, "elevationBeamConfiguration", 
	       set->elevationBeamConfiguration, "beam identification", NULL);
  h5_value_str(h5_file, level2, "productVariant", 
	       product2str(set->productVariant), 
	       "SLC, STD, TC, RTC, GEO, SSC, MGD, GEC or EEC", NULL);
  h5_value_str(h5_file, level2, "resolutionVariant", 
	       resolution2str(set->resolutionVariant), "SE or RE", NULL);
  h5_value_str(h5_file, level1, "projection", 
	       projection2str(set->projection), 
	       "GROUNDRANGE, SLANTRANGE or MAP", NULL);
  H5Gclose(h5_level2);
  H5Gclose(h5_level1);
  strcpy(level1, "/metadata/setup/inputData");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_str(h5_file, level1, "logicalDataTakeID", set->logicalDataTakeID,
	       "logical datatake ID", NULL);
  h5_value_str(h5_file, level1, "level0ProductID", set->level0ProductID,
	       "level zero product ID", NULL);
  dateTime2str(set->L0SARGenerationTimeUTC, str);
  h5_value_str(h5_file, level1, "L0SARGenerationTimeUTC", str, 
	       "level zero SAR generation time UTC", NULL);
  H5Gclose(h5_level1);
  if (set->numProcessingSteps > 0) {
    strcpy(level1, "/metadata/setup/processingSteps");
    h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    strcpy(level2, "/metadata/setup/processingSteps/software");
    h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			  H5P_DEFAULT);
    h5_att_str(h5_level2, "long_name", 
	       "major components of processing chain");
    str_array = (char **) MALLOC(sizeof(char *)*comps->numLayers);
    for (ii=0; ii<set->numProcessingSteps; ii++)
      str_array[ii] = (char *) MALLOC(sizeof(char)*1024);
    for (ii=0; ii<set->numProcessingSteps; ii++) 
      strcpy(str_array[ii], set->processingStep[ii].softwareID);
    h5_value_str_array(h5_file, level2, "softwareID", str_array, 
		       set->numProcessingSteps, "software ID", NULL);
    for (ii=0; ii<set->numProcessingSteps; ii++)
      strcpy(str_array[ii], set->processingStep[ii].softwareVersion);
    h5_value_str_array(h5_file, level2, "softwareVersion", str_array,
		       set->numProcessingSteps, "version number", NULL);
    for (ii=0; ii<set->numProcessingSteps; ii++)
      dateTime2str(set->processingStep[ii].processingTimeUTC, str_array[ii]);
    h5_value_str_array(h5_file, level2, "processingTimeUTC", str_array,
		       set->numProcessingSteps, "processing time in UTC", NULL);
    int algorithmFlag = FALSE;
    for (ii=0; ii<set->numProcessingSteps; ii++)    
      if (set->processingStep[ii].algorithm)
	algorithmFlag = TRUE;
    if (algorithmFlag) {
      for (ii=0; ii<set->numProcessingSteps; ii++) {    
	if (set->processingStep[ii].algorithm)
	  strcpy(str_array[ii], set->processingStep[ii].algorithm);
	else
	  strcpy(str_array[ii], "");
      }
      h5_value_str_array(h5_file, level2, "algorithm", str_array,
			 set->numProcessingSteps, "main algorithm", NULL);
    }
    for (ii=0; ii<set->numProcessingSteps; ii++)
      FREE(str_array[ii]);
    FREE(str_array);
    H5Gclose(h5_level1);
  }
  H5Gclose(h5_section);
  
  // Processing
  strcpy(group, "/metadata/processing");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			 H5P_DEFAULT);
  h5_att_str(h5_section, "long_name", 
	     "processor and product configuration parameters");
  strcpy(level1, "/metadata/processing/doppler");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_str(h5_file, level1, "dopplerBasebandEstimationMethod",
	       pro->dopplerBasebandEstimationMethod, 
	       "azimuth cross correlation", NULL);
  h5_value_str(h5_file, level1, "dopplerCentroidCoordinateType", 
	       imageCoord2str(pro->dopplerCentroidCoordinateType), 
	       "RAW or ZERODOPPLER", NULL);
  if (pro->doppler) {
    int numDoppler = 1;
    for (ii=0; ii<numDoppler; ii++) {
      strcpy(level2, "/metadata/processing/doppler/dopplerCentroid");
      h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_value_str(h5_file, level2, "polLayer", 
		   polLayer2str(pro->doppler[ii].polLayer), 
		   "HH, HV, VH or VV", NULL);
      h5_value_int(h5_file, level2, "numberOfBlocks", 
		   pro->doppler[ii].numberOfBlocks, "number of blocks", 
		   NULL);
      h5_value_int(h5_file, level2, "numberOfRejectedBlocks", 
		   pro->doppler[ii].numberOfRejectedBlocks,
		   "number of rejected blocks", NULL);
      h5_value_int(h5_file, level2, "numberOfDopplerRecords",
		   pro->doppler[ii].numberOfDopperRecords, 
		   "number of Doppler records", NULL);
      strcpy(level3, "/metadata/processing/doppler/dopplerCentroid/dopplerEstimate");
      h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      dateTime2str(pro->doppler[ii].timeUTC, str);
      h5_value_str(h5_file, level3, "timeUTC", str, "time stamp in UTC", NULL);
      h5_value_double(h5_file, level3, "dopplerAtMidRange",
		      pro->doppler[ii].dopplerAtMidRange, 
		      "Doppler at mid range [Hz]", "Hz");
      int degree = pro->doppler[ii].polynomialDegree;
      h5_value_int(h5_file, level3, "polynomialDegree", degree,
		   "polynomial degree", NULL);
      strcpy(level4, "/metadata/processing/doppler/dopplerCentroid/dopplerEstimate/basebandDoppler");
      h5_level4 = H5Gcreate(h5_file, level4, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      double_array = (double *) CALLOC(degree+1, sizeof(double));
      for (kk=0; kk<=degree; kk++)
	double_array[kk] = pro->doppler[ii].coefficient[kk];
      h5_value_double_array(h5_file, level4, "coefficient", double_array, 
			    degree+1, "Doppler coefficient", NULL);
      FREE(double_array);
      H5Gclose(h5_level4);
      H5Gclose(h5_level3);
      H5Gclose(h5_level2);
    }
  }
  H5Gclose(h5_level1);
  strcpy(level1, "/metadata/processing/processingParameter");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", 
	     "range and azimuth processing parameters");
  h5_value_str(h5_file, level1, "processingInfoCoordinateType", 
	       imageCoord2str(pro->processingParameter[0].processingInfoCoordinateType), 
	       "RAW or ZERODOPPLER", NULL);
  h5_value_double(h5_file, level1, "rangeLooks",
		  pro->processingParameter[0].rangeLooks, 
		  "number of range looks", NULL);
  h5_value_double(h5_file, level1, "azimuthLooks",
		  pro->processingParameter[0].azimuthLooks, 
		  "number of azimuth looks", NULL);
  h5_value_double(h5_file, level1, "rangeLookBandwidth",
		  pro->processingParameter[0].rangeLookBandwidth, 
		  "range look bandwidth [Hz]", "Hz");
  h5_value_double(h5_file, level1, "azimuthLookBandwidth",
		  pro->processingParameter[0].azimuthLookBandwidth, 
		  "azimuth look bandwidth [Hz]", "Hz");
  h5_value_double(h5_file, level1, "totalProcessedRangeBandwidth",
		  pro->processingParameter[0].totalProcessedRangeBandwidth,
		  "total processed range bandwidth [Hz]", "Hz");
  h5_value_double(h5_file, level1, "totalProcessedAzimuthBandwidth",
		  pro->processingParameter[0].totalProcessedAzimuthBandwidth,
		  "total processed azimuth bandwidth [Hz]", "Hz");
  h5_value_double(h5_file, level1, "chirpRate",
		  pro->processingParameter[0].chirpRate, 
		  "chirp rate [Hz/sec]", "Hz/sec");
  h5_value_double(h5_file, level1, "pulseDuration",
		  pro->processingParameter[0].pulseDuration, 
		  "pulse duration [Hz]", "Hz");
  H5Gclose(h5_level1);
  
  strcpy(level1, "/metadata/processing/processingFlags");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", 
	     "flags indicating which processing steps have been performed");
  h5_value_boolean(h5_file, level1, "chirpReplicaUsedFlag",
		   pro->chirpReplicaUsedFlag, "reconstructed chirp used", 
		   NULL);
  h5_value_boolean(h5_file, level1, "geometricDopplerUsedFlag",
		   pro->geometricDopplerUsedFlag, 
		   "geometric Doppler centroid estimate has been used", 
		   NULL);
  h5_value_boolean(h5_file, level1, "azimuthPatternCorrectedFlag",
		   pro->azimuthPatternCorrectedFlag, 
		   "stripmap azimuth antenna pattern correction applied", 
		   NULL);
  h5_value_boolean(h5_file, level1, "elevationPatternCorrectedFlag",
		   pro->elevationPatternCorrectedFlag, 
		   "antenna elevation pattern correction applied", NULL);
  h5_value_boolean(h5_file, level1, "detectedFlag", pro->detectedFlag, 
		   "detection performed", NULL);
  h5_value_boolean(h5_file, level1, "multiLookedFlag", pro->multiLookedFlag, 
		   "multilooking performed", NULL);
  h5_value_boolean(h5_file, level1, "polarimetricProcessedFlag",
		   pro->polarimetricProcessedFlag, 
		   "polarimetric processing performed", NULL);
  h5_value_boolean(h5_file, level1, "terrainCorrectedFlag",
		   pro->terrainCorrectedFlag, 
		   "terrain correction performed", NULL);
  h5_value_boolean(h5_file, level1, "layoverShadowMaskGeneratedFlag",
		   pro->layoverShadowMaskGeneratedFlag, 
		   "layover/shadow mask generated", NULL);
  h5_value_boolean(h5_file, level1, "geocodedFlag", pro->geocodedFlag,
		   "geocoding performed", NULL);
  h5_value_boolean(h5_file, level1, "nominalProcessingPerformedFlag",
		   pro->nominalProcessingPerformedFlag, 
		   "nominal processing steps used", NULL);
  H5Gclose(h5_level1);
  H5Gclose(h5_section);
  
  // Instrument
  strcpy(group, "/metadata/instrument");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			 H5P_DEFAULT);
  h5_att_str(h5_section, "long_name", 
	     "sensor specific parameters at the time of the data acquisition");
  h5_value_str(h5_file, group, "instrumentInfoCoordinateType", 
	       imageCoord2str(inst->instrumentInfoCoordinateType), 
	       "RAW or ZERODOPPLER", NULL);
  strcpy(level1, "/metadata/instrument/radarParameters");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_double(h5_file, level1, "centerFrequency", inst->centerFrequency,
		  "center frequency [Hz]", "Hz");
  H5Gclose(h5_level1);
  if (inst->settings) {
    for (ii=0; ii<comps->numLayers; ii++) {
      strcpy(level1, "/metadata/instrument/settings");
      h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_value_str(h5_file, level1, "polLayer", 
		   polLayer2str(inst->settings[ii].polLayer), 
		   "HH, HV, VH or VV", NULL);
      h5_value_str(h5_file, level1, "beamID", inst->settings[ii].beamID,
		   "beam identifier", NULL);
      h5_value_double(h5_file, level1, "rxBandwidth", 
		      inst->settings[ii].rxBandwidth, "range bandwidth [Hz]", 
		      "Hz");
      h5_value_double(h5_file, level1, "RSF", inst->settings[ii].rsf,
		      "range sampling frequency [Hz]", "Hz");
      
      inst->settings[ii].numberOfPRFChanges = 0;
      inst->settings[ii].numberOfEchoWindowPositionChanges = 0;
      inst->settings[ii].numberOfEchoWindowLengthChanges = 0;
      inst->settings[ii].numberOfSettingRecords = 1;
      
      h5_value_int(h5_file, level1, "numberOfPRFChanges", 
		   inst->settings[ii].numberOfPRFChanges, 
		   "number of PRF changes", NULL);
      h5_value_int(h5_file, level1, "numberOfEchoWindowPositionChanges",
		   inst->settings[ii].numberOfEchoWindowPositionChanges, 
		   "number of echo window position changes", NULL);
      h5_value_int(h5_file, level1, "numberOfEchoWindowLengthChanges",
		   inst->settings[ii].numberOfEchoWindowLengthChanges,
		   "number of echo window length changes", NULL);
      h5_value_int(h5_file, level1, "numberOfSettingRecords",
		   inst->settings[ii].numberOfSettingRecords, 
		   "number of setting records", NULL);
      strcpy(level2, "/metadata/instrument/settings/settingRecords");
      h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      iso_settingRecord *rec = inst->settings[ii].settingRecord;
      int numRecords = inst->settings[ii].numberOfSettingRecords;
      int_array = (int *) MALLOC(sizeof(int)*numRecords);
      double_array = (double *) MALLOC(sizeof(double)*numRecords);
      str_array = (char **) MALLOC(sizeof(char *)*numRecords);
      for (kk=0; kk<numRecords; kk++)
	str_array[kk] = (char *) MALLOC(sizeof(char)*30);
      strcpy(level3, "/metadata/instrument/settings/settingRecords/dataSegment");
      h5_level3 = H5Gcreate(h5_file, level3, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      for (kk=0; kk<numRecords; kk++) 
	dateTime2str(rec[kk].startTimeUTC, str_array[kk]);
      h5_value_str_array(h5_file, level3, "startTimeUTC", str_array, 
			 numRecords, "start time stamp in UTC", NULL);
      for (kk=0; kk<numRecords; kk++)
	dateTime2str(rec[kk].stopTimeUTC, str_array[kk]);
      h5_value_str_array(h5_file, level3, "stopTimeUTC", str_array, 
			 numRecords, "stop time stamp in UTC", NULL);
      for (kk=0; kk<numRecords; kk++)
	int_array[kk] = rec[kk].numberOfRows;
      h5_value_int_array(h5_file, level3, "numberOfRows", int_array, 
			 numRecords, "line count", NULL);
      H5Gclose(h5_level3);
      for (kk=0; kk<numRecords; kk++)
	double_array[kk] = rec[kk].prf;
      h5_value_double_array(h5_file, level2, "PRF", double_array, 
			    numRecords, "pulse repetition frequency [Hz]", 
			    "Hz");
      for (kk=0; kk<numRecords; kk++)
	double_array[kk] = rec[kk].echoWindowPosition;
      h5_value_double_array(h5_file, level2, "echoWindowPosition", 
			    double_array, numRecords, 
			    "sampling window start time", NULL);
      for (kk=0; kk<numRecords; kk++)
	double_array[kk] = rec[kk].echoWindowLength;
      h5_value_double_array(h5_file, level2, "echoWindowLength", 
			    double_array, numRecords, "number of samples", 
			    NULL);
      for (kk=0; kk<numRecords; kk++)
	strcpy(str_array[kk], rec[kk].pulseType); 
      h5_value_str_array(h5_file, level2, "pulseType", str_array, numRecords,
			 "pulse type", NULL);
      FREE(int_array);
      FREE(double_array);
      for (kk=0; kk<numRecords; kk++)
	FREE(str_array[kk]);
      FREE(str_array);
      H5Gclose(h5_level2);
      H5Gclose(h5_level1);
    }
  }
  H5Gclose(h5_section);
  
  // Platform
  strcpy(group, "/metadata/platform");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			 H5P_DEFAULT);
  h5_att_str(h5_section, "long_name", 
	     "state vectors and geometric layout of the platform");
  strcpy(level1, "/metadata/platform/orbit");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  strcpy(level2, "/metadata/platform/orbit/orbitHeader");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  if (platform->sensor == PREDICTED_SENSOR)
    strcpy(str, "PREDICTED SENSOR");
  else if (platform->sensor == SINGLE_GPS)
    strcpy(str, "SINGLE GPS");
  else if (platform->sensor == DIFFERENTIAL_GPS)
    strcpy(str, "DIFFERENTIAL GPS");
  else if (platform->sensor == UNDEF_ORBIT_SENSOR)
    strcpy(str, "UNDEFINED");
  h5_value_str(h5_file, level2, "sensor", str, 
	       "PREDICTED SENSOR, SINGLE GPS or DIFFERENTIAL GPS", NULL);
  h5_value_str(h5_file, level2, "accuracy", 
	       orbitAcc2str(platform->accuracy), 
	       "PREDICTED, RESTITUTED, PRECISE or TLE", NULL);
  h5_value_int(h5_file, level2, "numStateVectors", platform->numStateVectors,
	       "number of state vectors", NULL);
  dateTime2str(platform->firstStateTimeUTC, str);
  h5_value_str(h5_file, level2, "firstStateTimeUTC", str,
	       "UTC time of first state vector", NULL);
  dateTime2str(platform->lastStateTimeUTC, str);
  h5_value_str(h5_file, level2, "lastStateTimeUTC", str,
	       "UTC time of last state vector", NULL);
  h5_value_str(h5_file, level2, "stateVectorRefFrame", 
	       platform->stateVectorRefFrame,
	       "state vector reference frame", NULL);
  h5_value_double(h5_file, level2, "stateVectorTimeSpacing", 
		  platform->stateVectorTimeSpacing, 
		  "state vector time spacing [s]", "s");
  H5Gclose(h5_level2);
  strcpy(level2, "/metadata/platform/orbit/stateVec");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  int numVectors = platform->numStateVectors;
  double_array = (double *) MALLOC(sizeof(double)*numVectors);
  str_array = (char **) MALLOC(sizeof(char *)*numVectors);
  for (kk=0; kk<numVectors; kk++)
    str_array[kk] = (char *) MALLOC(sizeof(char)*50);
  for (ii=0; ii<numVectors; ii++)
    dateTime2str(platform->stateVec[ii].timeUTC, str_array[ii]);
  h5_value_str_array(h5_file, level2, "timeUTC", str_array, numVectors,
		     "UTC time of state vector", NULL);
  for (ii=0; ii<numVectors; ii++)
    double_array[ii] = platform->stateVec[ii].posX;
  h5_value_double_array(h5_file, level2, "posX", double_array, numVectors,
			"ECR coordinate position in x [m]", "m");
  for (ii=0; ii<numVectors; ii++)
    double_array[ii] = platform->stateVec[ii].posY;
  h5_value_double_array(h5_file, level2, "posY", double_array, numVectors,
			"ECR coordinate position in y [m]", "m");
  for (ii=0; ii<numVectors; ii++)
    double_array[ii] = platform->stateVec[ii].posZ;
  h5_value_double_array(h5_file, level2, "posZ", double_array, numVectors,
			"ECR coordinate position in z [m]", "m");
  for (ii=0; ii<numVectors; ii++)
    double_array[ii] = platform->stateVec[ii].velX;
  h5_value_double_array(h5_file, level2, "velX", double_array, numVectors,
			"ECR coordinate velocity in x [m/s]", "m/s");
  for (ii=0; ii<numVectors; ii++)
    double_array[ii] = platform->stateVec[ii].velY;
  h5_value_double_array(h5_file, level2, "velY", double_array, numVectors,
			"ECR coordinate velocity in y [m/s]", "m/s");
  for (ii=0; ii<numVectors; ii++)
    double_array[ii] = platform->stateVec[ii].velZ;
  h5_value_double_array(h5_file, level2, "velZ", double_array, numVectors,
			"ECR coordinate velocity in z [m/s]", "m/s");
  FREE(double_array);
  for (kk=0; kk<numVectors; kk++)
    FREE(str_array[kk]);
  FREE(str_array);
  H5Gclose(h5_level2);  
  H5Gclose(h5_level1);
  H5Gclose(h5_section);

  // Product Quality
  strcpy(group, "/metadata/productQuality");
  h5_section = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			 H5P_DEFAULT);
  h5_att_str(h5_section, "long_name", "summarizes image and data quality");
  int numGaps;
  if (quality->rawDataQuality) {
    for (ii=0; ii<info->numberOfLayers; ii++) {
      strcpy(level1, "/metadata/productQuality/rawDataQuality");
      h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_att_str(h5_level1, "long_name", 
		 "assessment of raw data quality (per layer)");
      h5_value_str(h5_file, level1, "polLayer", 
		   polLayer2str(quality->rawDataQuality[ii].polLayer),
		   "HH, HV, VH or VV", NULL);
      if (quality->rawDataQuality[ii].beamID)
	h5_value_str(h5_file, level1, "beamID", 
		     quality->rawDataQuality[ii].beamID,
		     "beam identifier", NULL);
      numGaps = quality->rawDataQuality[ii].numGaps;
      h5_value_int(h5_file, level1, "numGaps", numGaps, 
		   "number of data gaps", NULL);
      if (numGaps > 0) {
	strcpy(level2, "/metadata/productQuality/rawDataQuality/gap");
	h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			      H5P_DEFAULT);
	h5_att_str(h5_level2, "long_name", "data gap information");
	long_array = (long *) MALLOC(sizeof(long)*numGaps);
	int_array = (int *) MALLOC(sizeof(int)*numGaps);
	str_array = (char **) MALLOC(sizeof(char *)*numGaps);
	for (kk=0; kk<numGaps; kk++)
	  str_array[kk] = (char *) MALLOC(sizeof(char)*30);
	for (kk=0; kk<numGaps; kk++)
	  long_array[kk] = quality->rawDataQuality[ii].gap[kk].start;
	h5_value_long_array(h5_file, level2, "start", long_array, numGaps,
			    "start line of data gap", NULL);
	for (kk=0; kk<numGaps; kk++)
	  int_array[kk] = quality->rawDataQuality[ii].gap[kk].length;
	h5_value_int_array(h5_file, level2, "length", int_array, numGaps,
			   "number of lines forming data gap", NULL);
	for (kk=0; kk<numGaps; kk++) {
	  if (quality->rawDataQuality[ii].gap[kk].fill == RANDOM_FILL)
	    strcpy(str_array[kk], "RANDOM");
	  else if (quality->rawDataQuality[ii].gap[kk].fill == ZERO_FILL)
	    strcpy(str_array[kk], "ZERO");
	  else if (quality->rawDataQuality[ii].gap[kk].fill == UNDEF_FILL)
	    strcpy(str_array[kk], "UNDEFINED");
	}
	h5_value_str_array(h5_file, level2, "fill", str_array, numGaps,
			   "RANDOM or ZERO", NULL);
	H5Gclose(h5_level2);
	FREE(long_array);
	FREE(int_array);
	for (kk=0; kk<numGaps; kk++)
	  FREE(str_array[kk]);
	FREE(str_array);
      }
      h5_value_boolean(h5_file, level1, "gapSignificanceFlag",
		       quality->rawDataQuality[ii].gapSignificanceFlag,
		       "gaps above the tolerance level?", NULL);
      h5_value_boolean(h5_file, level1, "missingLinesSignificanceFlag",
		       quality->rawDataQuality[ii].missingLinesSignificanceFlag,
		       "missing lines outside tolerance level?", NULL);
      h5_value_boolean(h5_file, level1, "bitErrorSignificanceFlag",
		       quality->rawDataQuality[ii].bitErrorSignificanceFlag,
		       "bit error outside tolerance limits?", NULL);
      h5_value_boolean(h5_file, level1, "timeReconstructionSignificanceFlag",
		       quality->rawDataQuality[ii].timeReconstructionSignificanceFlag,
		       "time reconstruction outside tolerance limits?", NULL);
      H5Gclose(h5_level1);
    }
  }
  strcpy(level1, "/metadata/productQuality/processingParameterQuality");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_att_str(h5_level1, "long_name", "quality of processing parameters");
  h5_value_boolean(h5_file, level1, "dopplerAmbiguityNotZeroFlag",
		   quality->dopplerAmbiguityNotZeroFlag,
		   "Doppler ambiguities?", NULL);
  h5_value_boolean(h5_file, level1, "dopplerOutsideLimitsFlag",
		   quality->dopplerOutsideLimitsFlag,
		   "Doppler outside tolerance limits?", NULL);
  h5_value_boolean(h5_file, level1, "geolocationQualityLowFlag",
		   quality->geolocationQualityLowFlag,
		   "orbit/attitude/DEM/Doppler quality problems?", NULL);
  if (quality->imageDataQuality) {
    for (ii=0; ii<info->numberOfLayers; ii++) {
      strcpy(level1, "/metadata/productQuality/imageDataQuality");
      h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_att_int(h5_level1, "layerIndex", ii+1);
      h5_value_str(h5_file, level1, "polLayer", 
		   polLayer2str(quality->imageDataQuality[ii].polLayer),
		   "HH, HV, VH or VV", NULL);
      if (quality->imageDataQuality[ii].beamID)
	h5_value_str(h5_file, level1, "beamID", 
		     quality->imageDataQuality[ii].beamID,
		     "beam identifier", NULL);
      strcpy(level2, "/metadata/productQuality/imageDataQuality/imageDataStatistics");
      h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			    H5P_DEFAULT);
      h5_att_str(h5_level2, "long_name", "image data statistics");
      h5_value_double(h5_file, level2, "min", 
		      quality->imageDataQuality[ii].min, "minimum value", 
		      NULL);
      h5_value_double(h5_file, level2, "max", 
		      quality->imageDataQuality[ii].max, "maximum value", 
		      NULL);
      h5_value_double(h5_file, level2, "mean", 
		      quality->imageDataQuality[ii].mean, 
		      "mean value", NULL);
      h5_value_double(h5_file, level2, "standardDeviation", 
		      quality->imageDataQuality[ii].stdDev, 
		      "standard deviation", NULL);
      h5_value_int(h5_file, level2, "missingLines", 
		   quality->imageDataQuality[ii].missingLines, 
		   "missing lines", NULL);
      h5_value_double(h5_file, level2, "bitErrorRate", 
		      quality->imageDataQuality[ii].bitErrorRate, 
		      "bit error rate", NULL);
      h5_value_double(h5_file, level2, "noData", 
		      quality->imageDataQuality[ii].noData, "no data value", 
		      NULL);
      H5Gclose(h5_level2);
      H5Gclose(h5_level1);
    }
  }
  strcpy(level1, "/metadata/productQuality/limits");
  h5_level1 = H5Gcreate(h5_file, level1, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  strcpy(level2, "/metadata/productQuality/limits/raw");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_int(h5_file, level2, "gapDefinition", quality->gapDefinition,
	       "number of consecutive missing lines considered a gap", NULL);
  h5_value_double(h5_file, level2, "gapPercentageLimit", 
		  quality->gapPercentageLimit, 
		 "percentage of gaps considered significant", NULL);
  h5_value_double(h5_file, level2, "missingLinePercentageLimit",
		 quality->missingLinePercentageLimit, 
		 "percentage of missing lines considered significant", NULL);
  h5_value_double(h5_file, level2, "bitErrorLimit", 
		 quality->bitErrorLimit, 
		 "bit error rate considered significant", NULL);
  h5_value_double(h5_file, level2, "timeReconstructionPercentageLimit",
		 quality->timeReconstructionPercentageLimit, 
		 "percentage of time reconstruction considered significant", 
		 NULL);
  H5Gclose(h5_level2);
  strcpy(level2, "/metadata/productQuality/limits/processing");
  h5_level2 = H5Gcreate(h5_file, level2, H5P_DEFAULT, H5P_DEFAULT, 
			H5P_DEFAULT);
  h5_value_double(h5_file, level2, "dopplerCentroidLimit", 
		 quality->dopplerCentroidLimit, "Doppler value limit [Hz]", 
		  "Hz");
  h5_value_double(h5_file, level2, "geolocationQualityLimit",
		 quality->geolocationQualityLimit,
		 "minimum posting required for accurate geolocation [arcsec]", "arcsec");
  H5Gclose(h5_level2);
  H5Gclose(h5_level1);
  quality->instrumentStateRemark = (char *) MALLOC(sizeof(char)*1024);
  strcpy(quality->instrumentStateRemark, MAGIC_UNSET_STRING);
  if (quality->instrumentStateRemark)
    h5_value_str(h5_file, group, "instrumentStateRemark", 
		 quality->instrumentStateRemark, 
		 "steering or antenna problems", NULL);
  H5Gclose(h5_section);

  H5Gclose(h5_metagroup);
  H5Sclose(h5_string);
  H5Sclose(h5_array);

  return h5;

}

void finalize_h5_file(h5_t *hdf)
{
  H5Fclose(hdf->file);
  
  // Clean up
  FREE(hdf->var);
  FREE(hdf);
}

void export_hdf(const char *metadata_file_name, 
		const char *image_data_file_name,
		char *output_file_name, char **band_name,
		int *noutputs,char ***output_names)
{
  int ii, jj, kk, channel;
  meta_parameters *md = NULL;
  iso_meta *iso = NULL;
  h5_t *h5 = NULL;

  append_ext_if_needed(output_file_name, ".h5", NULL);
  char *ext = findExt(metadata_file_name);
  if (strcmp_case(ext, ".XML") == 0) {
    md = meta_read(metadata_file_name);
    iso = iso_meta_read(metadata_file_name);
    h5 = initialize_h5_file_iso(output_file_name, md, iso);
  }
  else if (strcmp_case(ext, ".META") == 0) {
    md = meta_read (metadata_file_name); 
    h5 = initialize_h5_file_meta(output_file_name, md);
  }
  else
    asfPrintError("Metadata format (%s) not supported!\n", ext);
  int band_count = md->general->band_count;
  int sample_count = md->general->sample_count;
  int line_count = md->general->line_count;
  float *hdf = (float *) MALLOC(sizeof(float)*line_count*sample_count);
  FILE *fp = FOPEN(image_data_file_name, "rb");
  float *float_line = (float *) MALLOC(sizeof(float)*sample_count);
  if (!band_name)
    band_name = extract_band_names(md->general->bands, band_count);

  for (kk=0; kk<band_count; kk++) {
    for (ii=0; ii<line_count; ii++ ) {
      channel = get_band_number(md->general->bands, band_count, band_name[kk]);
      get_float_line(fp, md, ii+channel*line_count, float_line);
      for (jj=0; jj<sample_count; jj++)
	hdf[ii*sample_count+jj] = float_line[jj];
      asfLineMeter(ii, line_count);
    }
    asfPrintStatus("Storing band '%s' ...\n", band_name[kk]);
    char dataset[50];
    sprintf(dataset, "/data/%s", band_name[kk]);
    hid_t h5_data = H5Dopen(h5->file, dataset, H5P_DEFAULT);
    H5Dwrite(h5_data, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, hdf);
    H5Dclose(h5_data);
  }

  finalize_h5_file(h5);
  FREE(hdf);
  FREE(float_line);
  meta_free(md);

  *noutputs = 1;
  char **outs = MALLOC(sizeof(char*));
  outs[0] = STRDUP(output_file_name);
  *output_names = outs;  
}
