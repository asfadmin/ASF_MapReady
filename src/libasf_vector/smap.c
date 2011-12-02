#include "asf_vector.h"
#include "shapefil.h"
#include "asf_nan.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include "dateUtil.h"
#include "hdf5.h"

void shape_smap_init(char *inFile)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape;

  // Open database for initialization
  dbaseFile = appendExt(inFile, ".dbf");
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  if (DBFAddField(dbase, "POLYGON", FTString, 5, 0) == -1)
    asfPrintError("Could not add POLYGON field to database file\n");

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);

  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

static void check_smap_file(char *inDataName, 
			    int *line_count, int *sample_count)
{
  hsize_t dims[2];
  hid_t file, group, dataset, dataspace;

  // Check whether the file is actually a real HDF5 data set
  if (!H5Fis_hdf5(inDataName))
    asfPrintError("File (%s) is not in HDF5 format!\n", inDataName);

  // Check dimensions
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  H5Sget_simple_extent_dims(dataspace, dims, NULL);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Gclose(group);
  H5Fclose(file);

  *line_count = dims[0];
  *sample_count = dims[1];
}

static void read_smap_outline(char *inDataName, int *vertex_count, 
			      float *lat, float *lon)
{
  hsize_t dims[2], count[2], pixels[1], offset[2];
  hid_t file, group, dataset, datatype, dataspace, memspace;

  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);

  // Read latitude first
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  H5Sget_simple_extent_dims(dataspace, dims, NULL);
  int nl = dims[0];
  int ns = dims[1];

  // Determine dimensions
  offset[0] = 0;
  offset[1] = 0;
  count[0] = 1;
  count[1] = ns;
  pixels[0] = ns;
  memspace = H5Screate_simple(1, pixels, NULL);
  int ii, kk, counts=0;
  float *values = (float *) MALLOC(sizeof(float)*ns);

  // Read first line
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  for (ii=0; ii<ns; ii++) {
    if (meta_is_valid_double(values[ii])) {
      lat[counts] = values[ii];
      counts++;
    }
  }
  // Walk along right boundary
  for (kk=1; kk<nl; kk++) {
    offset[0] = kk;
    H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
    H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
    ii = ns - 1;
    while (!meta_is_valid_double(values[ii]))
      ii--;
    lat[counts] = values[ii];
    counts++;
  }
  // Read last line
  offset[0] = nl - 1;
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  for (ii=ns-1; ii>0; ii--) {
    if (meta_is_valid_double(values[ii])) {
      lat[counts] = values[ii];
      counts++;
    }
  }
  // Walk along left boundary
  for (kk=nl-1; kk>0; kk--) {
    offset[0] = kk;
    H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
    H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
    ii = 0;
    while (!meta_is_valid_double(values[ii]))
      ii++;
    lat[counts] = values[ii];
    counts++;
  }

  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);

  // Read longitudes next
  counts = 0;
  dataset = H5Dopen(group, "cell_lon", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  
  // Read first line
  offset[0] = 0;
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  for (ii=0; ii<ns; ii++) {
    if (meta_is_valid_double(values[ii])) {
      lon[counts] = values[ii];
      counts++;
    }
  }
  // Walk along right boundary
  for (kk=1; kk<nl; kk++) {
    offset[0] = kk;
    H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
    H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
    ii = ns - 1;
    while (!meta_is_valid_double(values[ii]))
      ii--;
    lon[counts] = values[ii];
    counts++;
  }
  // Read last line
  offset[0] = nl - 1;
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  for (ii=ns-1; ii>0; ii--) {
    if (meta_is_valid_double(values[ii])) {
      lon[counts] = values[ii];
      counts++;
    }
  }
  // Walk along left boundary
  for (kk=nl-1; kk>0; kk--) {
    offset[0] = kk;
    H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
    H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
    ii = 0;
    while (!meta_is_valid_double(values[ii]))
      ii++;
    lon[counts] = values[ii];
    counts++;
  }

  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);

  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);

  *vertex_count = counts;
}


int smap2shape(char *inFile, char *outFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  float *lat, *lon;
  int line_count, sample_count, vertex_count;

  // Initalize the database file
  shape_smap_init(outFile);
  open_shape(outFile, &dbase, &shape);

  // Read lat/lon for boundary of SMAP data set
  check_smap_file(inFile, &line_count, &sample_count);
  lat = (float *) MALLOC(sizeof(float)*line_count*sample_count*2);
  lon = (float *) MALLOC(sizeof(float)*line_count*sample_count*2);
  read_smap_outline(inFile, &vertex_count, lat, lon);

  // Create temporary processing directory
  char *tmpDir = (char *) MALLOC(sizeof(char)*(strlen(outFile)+25));
  sprintf(tmpDir, "%s-", outFile);
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);
  char *tmpFile = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+25));

  // Check for vertices crossing the date lines
  int ii, crosses_dateline = FALSE;
  float diff;
  for (ii=1; ii<vertex_count; ii++) {
    diff = fabs(lon[ii-1] - lon[ii]);
    if (diff > 1.0)
      crosses_dateline = TRUE;
  }
  if (crosses_dateline) {
    sprintf(tmpFile, "%s%cboundary1.csv", tmpDir, DIR_SEPARATOR);
    FILE *fp = FOPEN(tmpFile, "w");
    fprintf(fp, "# Format: POLYGON\n");
    fprintf(fp, "# ID,latitude,longitude\n");
    for (ii=0; ii<vertex_count; ii++)
      if (lon[ii] < 0.0)
	fprintf(fp, "%5d,%.4f,%.4f\n", ii, lat[ii], lon[ii]);
    FCLOSE(fp);
    sprintf(tmpFile, "%s%cboundary2.csv", tmpDir, DIR_SEPARATOR);
    fp = FOPEN(tmpFile, "w");
    fprintf(fp, "# Format: POLYGON\n");
    fprintf(fp, "# ID,latitude,longitude\n");
    for (ii=0; ii<vertex_count; ii++)
      if (lon[ii] >= 0.0)
	fprintf(fp, "%5d,%.4f,%.4f\n", ii, lat[ii], lon[ii]);
    FCLOSE(fp);
    sprintf(tmpFile, "%s%cboundary.lst", tmpDir, DIR_SEPARATOR);
    fp = FOPEN(tmpFile, "w");
    fprintf(fp, "%s%cboundary1.csv\n", tmpDir, DIR_SEPARATOR);
    fprintf(fp, "%s%cboundary2.csv\n", tmpDir, DIR_SEPARATOR);
    FCLOSE(fp);
    polygon2shape(tmpFile, outFile, 1);
  }
  else {
    sprintf(tmpFile, "%s%cboundary.csv", tmpDir, DIR_SEPARATOR);
    FILE *fp = FOPEN(tmpFile, "w");
    fprintf(fp, "# Format: POLYGON\n");
    fprintf(fp, "# ID,latitude,longitude\n");
    for (ii=0; ii<vertex_count; ii++)
      fprintf(fp, "%5d,%.4f,%.4f\n", ii, lat[ii], lon[ii]);
    FCLOSE(fp);
    polygon2shape(tmpFile, outFile, 0);
  }
  
  // Clean up
  remove_dir(tmpDir);
  FREE(tmpDir);
  FREE(tmpFile);
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return 1;
}
