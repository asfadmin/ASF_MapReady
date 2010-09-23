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

hdf_t *initialize_hdf5_file(const char *output_file_name, meta_parameters *md)
{
  hid_t hdf5_group, hdf5_data, hdf5_attr;
  herr_t status;
  int ii, complex=FALSE;
  char dataset[50], groupname[50], band[5], str_attr[50];

  // Check for complex data
  if (md->general->image_data_type == COMPLEX_IMAGE)
    complex = TRUE;

  // Initialize the HDF pointer structure
  hdf_t *hdf = (hdf_t *) MALLOC(sizeof(hdf_t));
  hdf->band_count = md->general->band_count;
  hdf->data_amp = (hid_t *) MALLOC(sizeof(hid_t)*hdf->band_count);
  if (complex)
    hdf->data_phase = (hid_t *) MALLOC(sizeof(hid_t)*hdf->band_count);
  else
    hdf->data_phase = NULL;

  // Create new HDF5 file
  hid_t hdf5_file = 
    H5Fcreate(output_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  hdf->file = hdf5_file;

  // Create data space
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
  
  // Adding global attributes
  hid_t hdf5_global = H5Gopen(hdf5_file, "/", H5P_DEFAULT);
  dims[0] = 1;
  dims[1] = 1;
  hdf5_space = H5Screate_simple(2, dims, NULL);
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

  // Metadata - General block
  hid_t hdf5_meta = H5Gcreate(hdf5_file, "/overview", H5P_DEFAULT,
			      H5P_DEFAULT, H5P_DEFAULT);
  hdf5_space = H5Screate_simple(2, dims, NULL);

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

  hdf5_attr = H5Acreate(hdf5_meta, "x_pixel_size", H5T_NATIVE_DOUBLE, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->general->x_pixel_size);
  H5Aclose(hdf5_attr);

  hdf5_attr = H5Acreate(hdf5_meta, "y_pixel_size", H5T_NATIVE_DOUBLE, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->general->y_pixel_size);
  H5Aclose(hdf5_attr);

  hdf5_attr = H5Acreate(hdf5_meta, "center_latitude", H5T_NATIVE_DOUBLE, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->general->center_latitude);
  H5Aclose(hdf5_attr);

  hdf5_attr = H5Acreate(hdf5_meta, "center_longitude", H5T_NATIVE_DOUBLE, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->general->center_longitude);
  H5Aclose(hdf5_attr);

  hdf5_attr = H5Acreate(hdf5_meta, "re_major", H5T_NATIVE_DOUBLE, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->general->re_major);
  H5Aclose(hdf5_attr);

  hdf5_attr = H5Acreate(hdf5_meta, "re_minor", H5T_NATIVE_DOUBLE, 
			hdf5_space, H5P_DEFAULT, H5P_DEFAULT);
  H5Awrite(hdf5_attr, H5T_NATIVE_DOUBLE, &md->general->re_minor);
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

  meta_write_xml(md, output_file_name);

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
  status = H5Fclose(hdf->file);
  if (status == -1)
    asfPrintError("Could not close HDF5 file!\n");

  // Clean up
  FREE(hdf->data_amp);
  if (hdf->data_phase)
    FREE(hdf->data_phase);
  FREE(hdf);
}
