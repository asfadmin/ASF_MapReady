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
#include <HE5_HdfEosDef.h>

typedef struct vector_time_t {
  int number;
  double time;
} vector_time_t;

typedef struct vector_xyz_t {
  int number;
  double x;
  double y;
  double z;
} vector_xyz_t;

hdf_t *initialize_hdf5_file(const char *output_file_name, meta_parameters *md)
{
  hid_t hdf5_file, hdf5_group, hdf5_data, hdf5_attr;
  herr_t status;
  int ii, complex=FALSE, projected=FALSE;
  char dataset[50], groupname[50], band[5], str_attr[50];
  char *spatial_ref=NULL, *datum=NULL, *spheroid=NULL;

  // Initialize the HDF pointer structure
  hdf_t *hdf = (hdf_t *) MALLOC(sizeof(hdf_t));
  hdf->band_count = md->general->band_count;
  hdf->data_amp = (hid_t *) MALLOC(sizeof(hid_t)*hdf->band_count);
  if (complex)
    hdf->data_phase = (hid_t *) MALLOC(sizeof(hid_t)*hdf->band_count);
  else
    hdf->data_phase = NULL;

  // Check for complex data
  if (md->general->image_data_type == COMPLEX_IMAGE)
    complex = TRUE;

  // Check whether data is map projected
  if (md->projection && md->projection->type != SCANSAR_PROJECTION) {
    projected = TRUE;
    hdf->projected = TRUE;
  }
  else
    hdf->projected = FALSE;

  // Create new HDF5 file
  if (projected)
    hdf5_file = HE5_GDopen(output_file_name, H5F_ACC_TRUNC);
  else
    hdf5_file = 
      H5Fcreate(output_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  hdf->file = hdf5_file;

  // Create data space
  hsize_t adims[1] = { 1 };
  hsize_t dims[2] = { md->general->line_count, md->general->sample_count };
  hsize_t cdims[2] = { 100, md->general->sample_count };
  hid_t hdf5_space = H5Screate_simple(2, dims, NULL);
  hdf->space = hdf5_space;
  
  // Create data structure
  char **band_name = extract_band_names(md->general->bands, hdf->band_count);
  hid_t hdf5_plist = H5Pcreate(H5P_DATASET_CREATE);
  H5Pset_chunk(hdf5_plist, 2, cdims);
  H5Pset_deflate(hdf5_plist, 6);

  for (ii=0; ii<hdf->band_count; ii++) {
    // Create a group per band
    strncpy(band, band_name[ii], 2);
    band[2] = '\0';
    sprintf(groupname, "/%s", band);
    hdf5_group = H5Gcreate(hdf5_file, groupname, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);
    
    // Create data set
    // Might want to consider big endian floating point - H5T_IEEE_F32BE
    sprintf(dataset, "%s/AMPLITUDE_IMAGE", groupname);
    hdf5_data = H5Dcreate(hdf5_file, dataset, H5T_NATIVE_FLOAT, hdf5_space,
			  H5P_DEFAULT, hdf5_plist, H5P_DEFAULT);
    hdf->data_amp[ii] = hdf5_data;
    if (complex) {
      sprintf(dataset, "%s/PHASE_IMAGE", groupname);
      hdf5_data = H5Dcreate(hdf5_file, dataset, H5T_NATIVE_FLOAT, hdf5_space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      hdf->data_phase[ii] = hdf5_data;
    }

    // Close group
    status = H5Gclose(hdf5_group);
    if (status == -1)
      asfPrintError("Could not close group!\n");
  }

  // Define units
  hid_t hdf5_units = H5Gcreate(hdf5_file, "/units", H5P_DEFAULT,
			      H5P_DEFAULT, H5P_DEFAULT);
  hdf5_space = H5Screate_simple(1, adims, NULL);
  hid_t met_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "meters", met_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t sec_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "seconds", sec_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t lat_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "degrees_north", lat_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t lon_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "degrees_east", lon_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t hz_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "hertz", hz_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t hz2_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "hertz per pixel", hz2_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t hz3_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "hertz per square pixel", hz3_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t met_sec_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
  H5Tcommit(hdf5_units, "meters per second", met_sec_type_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  hid_t vec_time_id = H5Tcreate(H5T_COMPOUND, sizeof(vector_time_t));
  H5Tinsert(vec_time_id, "number []", HOFFSET(vector_time_t, number), 
	    H5T_NATIVE_INT);
  H5Tinsert(vec_time_id, "time [seconds]", HOFFSET(vector_time_t, time), 
	    sec_type_id);
  H5Tcommit(hdf5_units, "vector_time", vec_time_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t vec_pos_id = H5Tcreate(H5T_COMPOUND, sizeof(vector_xyz_t));
  H5Tinsert(vec_pos_id, "number []", HOFFSET(vector_time_t, number), 
	    H5T_NATIVE_INT);
  H5Tinsert(vec_pos_id, "x [meters]", HOFFSET(vector_xyz_t, x), met_type_id);
  H5Tinsert(vec_pos_id, "y [meters]", HOFFSET(vector_xyz_t, y), met_type_id);
  H5Tinsert(vec_pos_id, "z [meters]", HOFFSET(vector_xyz_t, z), met_type_id);
  H5Tcommit(hdf5_units, "vector_position", vec_pos_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t vec_vel_id = H5Tcreate(H5T_COMPOUND, sizeof(vector_xyz_t));
  H5Tinsert(vec_vel_id, "number []", HOFFSET(vector_time_t, number), 
	    H5T_NATIVE_INT);
  H5Tinsert(vec_vel_id, "x [meters per second]", HOFFSET(vector_xyz_t, x), 
	    met_sec_type_id);
  H5Tinsert(vec_vel_id, "y [meters per second]", HOFFSET(vector_xyz_t, y), 
	    met_sec_type_id);
  H5Tinsert(vec_vel_id, "z [meters per second]", HOFFSET(vector_xyz_t, z), 
	    met_sec_type_id);
  H5Tcommit(hdf5_units, "vector_velocity", vec_vel_id, 
	    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  
  H5Sclose(hdf5_space);
  H5Gclose(hdf5_units);
  
  // Adding global attributes
  hid_t hdf5_global = H5Gopen(hdf5_file, "/", H5P_DEFAULT);
  hdf5_space = H5Screate_simple(1, adims, NULL);
  hid_t hdf5_str = H5Tcopy(H5T_C_S1);
  sprintf(str_attr, "Alaska Satellite Facility");
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_global, "institution", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  sprintf(str_attr, "%s %s %s image", 
	  md->general->sensor, md->general->sensor_name, md->general->mode);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_global, "title", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  if (md->general->image_data_type == AMPLITUDE_IMAGE)
    strcpy(str_attr, "SAR backcatter image");
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_global, "source", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  sprintf(str_attr, "%s", md->general->basename);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_global, "original_file", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  ymd_date ymd;
  hms_time hms;
  parse_date(md->general->acquisition_date, &ymd, &hms);
  if (strcmp_case(md->general->sensor, "RSAT-1") == 0)
    sprintf(str_attr, "Copyright Canadian Space Agency, %d", ymd.year);
  else if (strncmp_case(md->general->sensor, "ERS", 3) == 0)
    sprintf(str_attr, "Copyright European Space Agency, %d", ymd.year);
  else if (strcmp_case(md->general->sensor, "JERS-1") == 0 ||
	   strcmp_case(md->general->sensor, "ALOS") == 0)
    sprintf(str_attr, "Copyright Japan Aerospace Exploration Agency , %d", 
	    ymd.year);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_global, "comment", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  strcpy(str_attr, "Documentation available at: www.asf.alaska.edu");
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_global, "reference", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  time_t t;
  struct tm *timeinfo;
  time(&t);
  timeinfo = gmtime(&t);
  sprintf(str_attr, "%s", asctime(timeinfo));
  chomp(str_attr);
  strcat(str_attr, ", UTC: HDF5 File created.");
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_global, "history", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  H5Sclose(hdf5_space);
  H5Gclose(hdf5_global);

  // Add projection parameters
  double *lfValue = (double *) MALLOC(sizeof(double));
  if (md->projection && md->projection->type != SCANSAR_PROJECTION)
    projected = TRUE;
  if (projected) {
    hid_t hdf5_proj = H5Gcreate(hdf5_file, "/projection", H5P_DEFAULT,
				H5P_DEFAULT, H5P_DEFAULT);
    hdf5_space = H5Screate_simple(1, adims, NULL);
    if (md->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {

      meta_projection *mp = md->projection;
      strcpy(str_attr, "transverse_mercator");
      H5Tset_size(hdf5_str, strlen(str_attr));
      hdf5_attr = H5Acreate(hdf5_proj, "grid_mapping_name", hdf5_str, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, hdf5_str, str_attr);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "scale_factor_at_central_meridian", 
			    H5T_NATIVE_DOUBLE, hdf5_space, 
			    H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &mp->param.utm.scale_factor);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "longitude_of_central_meridian", 
			    lon_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, lon_type_id, &mp->param.utm.lon0);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "latitude_of_projection_origin", 
			    lat_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, lat_type_id, &mp->param.utm.lat0);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "false_easting", met_type_id, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, &mp->param.utm.false_easting);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "false_northing", met_type_id,
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, &mp->param.utm.false_northing);
      H5Aclose(hdf5_attr);
      strcpy(str_attr, "xgrid");
      H5Tset_size(hdf5_str, strlen(str_attr));
      hdf5_attr = H5Acreate(hdf5_proj, "projection_x_coordinate", hdf5_str, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, hdf5_str, str_attr);
      H5Aclose(hdf5_attr);
      strcpy(str_attr, "ygrid");
      H5Tset_size(hdf5_str, strlen(str_attr));
      hdf5_attr = H5Acreate(hdf5_proj, "projection_y_coordinate", hdf5_str, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, hdf5_str, str_attr);
      H5Aclose(hdf5_attr);
      strcpy(str_attr, "meters");
      H5Tset_size(hdf5_str, strlen(str_attr));
      hdf5_attr = H5Acreate(hdf5_proj, "units", hdf5_str, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, hdf5_str, str_attr);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "grid_boundary_top_projected_y", 
			    met_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, &mp->startY);
      H5Aclose(hdf5_attr);
      *lfValue = mp->startY + md->general->line_count * mp->perY;
      hdf5_attr = H5Acreate(hdf5_proj, "grid_boundary_bottom_projected_y", 
			    met_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, lfValue);
      H5Aclose(hdf5_attr);
      *lfValue = mp->startX + md->general->sample_count * mp->perX;
      hdf5_attr = H5Acreate(hdf5_proj, "grid_boundary_right_projected_x", 
			    met_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, lfValue);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "grid_boundary_left_projected_x", 
			    met_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, &mp->startX);
      H5Aclose(hdf5_attr);
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
      H5Tset_size(hdf5_str, strlen(spatial_ref));
      hdf5_attr = H5Acreate(hdf5_proj, "spatial_ref", hdf5_str, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, hdf5_str, spatial_ref);
      H5Aclose(hdf5_attr);
      sprintf(str_attr, "+proj=utm +zone=%d", mp->param.utm.zone);
      if (md->general->center_latitude < 0)
	strcat(str_attr, " +south");
      H5Tset_size(hdf5_str, strlen(str_attr));
      hdf5_attr = H5Acreate(hdf5_proj, "proj4text", hdf5_str, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, hdf5_str, str_attr);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "zone", H5T_NATIVE_INT, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, H5T_NATIVE_INT, &mp->param.utm.zone);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "semimajor_radius", 
			    met_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, &mp->re_major);
      H5Aclose(hdf5_attr);
      hdf5_attr = H5Acreate(hdf5_proj, "semiminor_radius", 
			    met_type_id, hdf5_space, 
			    H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, met_type_id, &mp->re_minor);
      H5Aclose(hdf5_attr);
      sprintf(str_attr, "%.6lf %.6lf 0 %.6lf 0 %.6lf", mp->startX, mp->perX, 
	      mp->startY, mp->perY); 
      H5Tset_size(hdf5_str, strlen(str_attr));
      hdf5_attr = H5Acreate(hdf5_proj, "GeoTransform", hdf5_str, 
			    hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
      H5Awrite(hdf5_attr, hdf5_str, str_attr);
      H5Aclose(hdf5_attr);
    }
    else if (md->projection->type == POLAR_STEREOGRAPHIC) {
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
    else if (md->projection->type == ALBERS_EQUAL_AREA) {
      /*
	grid_mapping_name = albers_conical_equal_area
	standard_parallel
	longitude_of_central_meridian
	latitude_of_projection_origin
	false_easting
	false_northing
      */
    }
    else if (md->projection->type == LAMBERT_CONFORMAL_CONIC) {
      /*
	grid_mapping_name = lambert_conformal_conic
	standard_parallel
	longitude_of_central_meridian
	latitude_of_projection_origin
	false_easting
	false_northing
      */
    }
    else if (md->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
      /*
	grid_mapping_name = lambert_azimuthal_equal_area
	longitude_of_projection_origin
	latitude_of_projection_origin
	false_easting
	false_northing
      */
    }
    H5Sclose(hdf5_space);
    H5Gclose(hdf5_proj);
  }
  FREE(lfValue);

  // Metadata - General block
  hid_t hdf5_meta = H5Gcreate(hdf5_file, "/overview", H5P_DEFAULT,
			      H5P_DEFAULT, H5P_DEFAULT);
  hdf5_space = H5Screate_simple(1, adims, NULL);
  strcpy(str_attr, md->general->basename);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "name", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  strcpy(str_attr, md->general->sensor);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "sensor", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  strcpy(str_attr, md->general->sensor_name);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "sensor_name", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  strcpy(str_attr, md->general->mode);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "mode", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  strcpy(str_attr, md->general->processor);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "processor", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  strcpy(str_attr, data_type2str(md->general->data_type));
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "data_type", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  strcpy(str_attr, image_data_type2str(md->general->image_data_type));
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "image_data_type", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  strcpy(str_attr, radiometry2str(md->general->radiometry));
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "radiometry", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  strcpy(str_attr, md->general->acquisition_date);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "acquisition_date", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  hdf5_attr = H5Acreate(hdf5_meta, "orbit", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->orbit);
  H5Aclose(hdf5_attr);
  if (md->general->orbit_direction == 'A')
    strcpy(str_attr, "Ascending");
  else
    strcpy(str_attr, "Descending");
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "orbit_direction", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "frame", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->frame);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "band_count", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->band_count);
  H5Aclose(hdf5_attr);
  strcpy(str_attr, md->general->bands);
  H5Tset_size(hdf5_str, strlen(str_attr));
  hdf5_attr = H5Acreate(hdf5_meta, "bands", hdf5_str, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, hdf5_str, str_attr);
  H5Aclose(hdf5_attr);  
  hdf5_attr = H5Acreate(hdf5_meta, "line_count", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->line_count);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "sample_count", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->sample_count);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "start_line", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->start_line);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "start_sample", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->start_sample);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "x_pixel_size", met_type_id, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, met_type_id, &md->general->x_pixel_size);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "y_pixel_size", met_type_id, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, met_type_id, &md->general->y_pixel_size);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "center_latitude", lat_type_id, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, lat_type_id, &md->general->center_latitude);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "center_longitude", lon_type_id, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, lon_type_id, &md->general->center_longitude);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "re_major", met_type_id, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, met_type_id, &md->general->re_major);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "re_minor", met_type_id, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, met_type_id, &md->general->re_minor);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "bit_error_rate", H5T_NATIVE_DOUBLE, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->general->bit_error_rate);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "missing_lines", H5T_NATIVE_INT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->general->missing_lines);
  H5Aclose(hdf5_attr);
  hdf5_attr = H5Acreate(hdf5_meta, "no_data", H5T_NATIVE_FLOAT, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_FLOAT, &md->general->no_data);
  H5Aclose(hdf5_attr);
  H5Sclose(hdf5_space);
  H5Gclose(hdf5_meta);

  if (md->sar) {
    // Metadata - SAR block
    hid_t hdf5_sar = H5Gcreate(hdf5_file, "/sar", H5P_DEFAULT,
			       H5P_DEFAULT, H5P_DEFAULT);
    hdf5_space = H5Screate_simple(1, adims, NULL);
    if (md->sar->image_type == 'S')
      strcpy(str_attr, "slant range");
    else if (md->sar->image_type == 'G')
      strcpy(str_attr, "ground range");
    else if (md->sar->image_type == 'P')
      strcpy(str_attr, "projected");
    else if (md->sar->image_type == 'R')
      strcpy(str_attr, "georeferenced");
    H5Tset_size(hdf5_str, strlen(str_attr));
    hdf5_attr = H5Acreate(hdf5_meta, "image_type", hdf5_str, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hdf5_str, str_attr);
    H5Aclose(hdf5_attr);
    if (md->sar->look_direction == 'R')
      strcpy(str_attr, "right");
    else if (md->sar->look_direction == 'L')
      strcpy(str_attr, "left");
    H5Tset_size(hdf5_str, strlen(str_attr));
    hdf5_attr = H5Acreate(hdf5_meta, "look_direction", hdf5_str, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hdf5_str, str_attr);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "look_count", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->sar->look_count);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "multilook", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->sar->multilook);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "deskewed", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->sar->deskewed);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "original_line_count", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->sar->original_line_count);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "original_sample_count", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->sar->original_sample_count);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "line_increment", H5T_NATIVE_DOUBLE, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->sar->line_increment);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "sample_increment", H5T_NATIVE_DOUBLE, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->sar->sample_increment);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "range_time_per_pixel", sec_type_id,
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, sec_type_id, &md->sar->range_time_per_pixel);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "azimuth_time_per_pixel", 
			  sec_type_id, hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, sec_type_id, &md->sar->azimuth_time_per_pixel);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "slant_range_first_pixel", met_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, met_type_id, &md->sar->slant_range_first_pixel);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "slant_shift", met_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, met_type_id, &md->sar->slant_shift);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "time_shift", sec_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, sec_type_id, &md->sar->time_shift);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "wavelength", met_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, met_type_id, &md->sar->wavelength);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "pulse_repetition_frequency", hz_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hz_type_id, &md->sar->prf);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "earth_radius", met_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, met_type_id, &md->sar->earth_radius);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "satellite_height", met_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, met_type_id, &md->sar->satellite_height);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "range_doppler_centroid", hz_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hz_type_id, &md->sar->range_doppler_coefficients[0]);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "range_doppler_linear", hz2_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hz2_type_id, &md->sar->range_doppler_coefficients[1]);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "range_doppler_quadratic", hz3_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hz3_type_id, &md->sar->range_doppler_coefficients[2]);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "azimuth_doppler_centroid", hz_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hz_type_id, &md->sar->azimuth_doppler_coefficients[0]);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "azimuth_doppler_linear", hz2_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hz2_type_id, 
	     &md->sar->azimuth_doppler_coefficients[1]);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "azimuth_doppler_quadratic", hz3_type_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, hz3_type_id, 
	     &md->sar->azimuth_doppler_coefficients[2]);
    H5Aclose(hdf5_attr);
    H5Sclose(hdf5_space);
    H5Gclose(hdf5_sar);
  }

  if (md->state_vectors) {
    // Metadata - state vector block
    int vector_count = md->state_vectors->vector_count;
    hid_t hdf5_orbit = H5Gcreate(hdf5_file, "/orbit", H5P_DEFAULT,
				 H5P_DEFAULT, H5P_DEFAULT);
    hdf5_space = H5Screate_simple(1, adims, NULL);
    hdf5_attr = H5Acreate(hdf5_meta, "year", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->state_vectors->year);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "day_of_year", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &md->state_vectors->julDay);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "second_of_day", sec_type_id,
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, sec_type_id, &md->state_vectors->second);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_meta, "vector_count", H5T_NATIVE_INT, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, H5T_NATIVE_INT, &vector_count);
    H5Aclose(hdf5_attr);
    H5Sclose(hdf5_space);

    hsize_t vdims[1] = { vector_count };
    hdf5_space = H5Screate_simple(1, vdims, NULL);
    vector_time_t *vector_time =
      (vector_time_t *) MALLOC(sizeof(vector_time_t)*vector_count);
    vector_xyz_t *vector_xyz =
      (vector_xyz_t *) MALLOC(sizeof(vector_xyz_t)*vector_count);
    for (ii=0; ii<vector_count; ii++) {
      vector_time[ii].number = ii+1;
      vector_time[ii].time = md->state_vectors->vecs[ii].time;
      vector_xyz[ii].number = ii+1;
      vector_xyz[ii].x = md->state_vectors->vecs[ii].vec.pos.x;
      vector_xyz[ii].y = md->state_vectors->vecs[ii].vec.pos.y;
      vector_xyz[ii].z = md->state_vectors->vecs[ii].vec.pos.z;
    }
    hdf5_attr = H5Acreate(hdf5_orbit, "vector_time", vec_time_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, vec_time_id, vector_time);
    H5Aclose(hdf5_attr);
    hdf5_attr = H5Acreate(hdf5_orbit, "vector_position", vec_pos_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, vec_pos_id, vector_xyz);
    H5Aclose(hdf5_attr);  
    for (ii=0; ii<vector_count; ii++) {
      vector_xyz[ii].number = ii+1;
      vector_xyz[ii].x = md->state_vectors->vecs[ii].vec.vel.x;
      vector_xyz[ii].y = md->state_vectors->vecs[ii].vec.vel.y;
      vector_xyz[ii].z = md->state_vectors->vecs[ii].vec.vel.z;
    
    }
    hdf5_attr = H5Acreate(hdf5_orbit, "vector_velocity", vec_vel_id, 
			  hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(hdf5_attr, vec_vel_id, vector_xyz);
    H5Aclose(hdf5_attr);
    H5Tclose(vec_time_id);
    H5Tclose(vec_pos_id);
    H5Tclose(vec_vel_id);
    H5Sclose(hdf5_space);
    H5Gclose(hdf5_orbit);
    FREE(vector_time);
    FREE(vector_xyz);
  }

  // Write ASF metadata to XML file
  char *output_file = 
    (char *) MALLOC(sizeof(char)*(strlen(output_file_name)+5));
  sprintf(output_file, "%s.xml", output_file_name);
  meta_write_xml(md, output_file);
  FREE(output_file);

  return hdf;
}

void finalize_hdf5_file(hdf_t *hdf)
{
  int ii;
  herr_t status;
  for (ii=0; ii<hdf->band_count; ii++) {
    status = H5Dclose(hdf->data_amp[ii]);
    if (status == -1)
      asfPrintError("Could not close HDF5 amp data set!\n");
    if (hdf->data_phase) {
      status = H5Dclose(hdf->data_phase[ii]);
      if (status == -1)
	asfPrintError("Could not close HDF5 phase data set!\n");
    }
  }
  status = H5Sclose(hdf->space);
  if (status == -1)
    asfPrintError("Could not close HDF5 data space!\n");
  if (hdf->projected)
    status = HE5_GDclose(hdf->file);
  else
    status = H5Fclose(hdf->file);
  if (status == -1)
    asfPrintError("Could not close HDF5 file!\n");

  // Clean up
  FREE(hdf->data_amp);
  if (hdf->data_phase)
    FREE(hdf->data_phase);
  FREE(hdf);
}
