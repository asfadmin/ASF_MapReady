#include "asf_vector.h"
#include "shapefil.h"
#include "asf_nan.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include "dateUtil.h"
#include "hdf5.h"
#include "smap.h"

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
  if (DBFAddField(dbase, "GRANULE", FTString, 100, 0) == -1)
    asfPrintError("Could not add GRANULE field to database file\n");
  if (DBFAddField(dbase, "START_TIME", FTString, 30, 0) == -1)
    asfPrintError("Could not add START_TIME field to database file\n");
  if (DBFAddField(dbase, "END_TIME", FTString, 30, 0) == -1)
    asfPrintError("Could not add STOP_TIME field to database file\n");
  if (DBFAddField(dbase, "ORBIT_DIR", FTString, 15, 0) == -1)
    asfPrintError("Could not add ORBIT_DIR field to database file\n");

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

static void check_smap_file(char *inDataName, int *line_count, int *sample_count)
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
			      double *lat, double *lon)
{
  hsize_t dims[2], count[2], offset[2], pixels[1];
  hid_t file, group, dataset, datatype, dataspace, memspace;

  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);

  // Look up latitude
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  H5Sget_simple_extent_dims(dataspace, dims, NULL);
  int nl = dims[0];
  int ns = dims[1];

  // Determine dimensions
  offset[0] = 0;
  offset[1] = 0;
  count[0] = nl;
  count[1] = ns;
  pixels[0] = nl*ns;
  int ii, kk, counts=0;
  float *values = (float *) MALLOC(sizeof(float)*ns*nl);

  // Read latitude
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  memspace = H5Screate_simple(1, pixels, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  
  // First line
  kk = 0;
  for (ii=0; ii<ns; ii++) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lat[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Right boundary
  for (kk=1; kk<nl-1; kk++) {
    ii = ns - 1;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii--;
    lat[counts] = (double) values[kk*ns+ii];
    counts++;
  }
  // Last line
  kk = nl - 1;
  for (ii=ns-1; ii>0; ii--) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lat[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Left boundary
  for (kk=nl-1; kk>0; kk--) {
    ii = 0;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii++;
    lat[counts] = (double) values[kk*ns+ii];
    counts++;
  }

  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);

  // Read longitudes
  counts = 0;
  dataset = H5Dopen(group, "cell_lon", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  offset[0] = 0;
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  
  // First line
  kk = 0;
  for (ii=0; ii<ns; ii++) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lon[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Right boundary
  for (kk=1; kk<nl-1; kk++) {
    ii = ns - 1;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii--;
    lon[counts] = (double) values[kk*ns+ii];
    counts++;
  }
  // Last line
  kk = nl - 1;
  for (ii=ns-1; ii>0; ii--) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lon[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Left boundary
  for (kk=nl-1; kk>0; kk--) {
    ii = 0;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii++;
    lon[counts] = (double) values[kk*ns+ii];
    counts++;
  }

  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);

  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);
  
	FREE(values);

  *vertex_count = counts;
}

void smap2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords)
{
  // Read header information
  dbf_header_t *header;
  int n;
  char shape_type[25];
  read_header_config("SMAP", &header, &n, shape_type);

  // Assign values
  smap_meta *smap = read_smap_meta(inFile);	
  int ii;
  for (ii=0; ii<n; ii++) {
    if (strcmp_case(header[ii].meta, "file_name") == 0)
      header[ii].sValue = STRDUP(smap->file_name);
    else if (strcmp_case(header[ii].meta, "start_time") == 0)
      header[ii].sValue = STRDUP(smap->orbit_start_date_time);
    else if (strcmp_case(header[ii].meta, "stop_time") == 0)
      header[ii].sValue = STRDUP(smap->orbit_stop_date_time);
    else if (strcmp_case(header[ii].meta, "direction") == 0)
      header[ii].sValue = STRDUP(smap->orbit_direction);
  }
  FREE(smap);

  // Read lat/lon for boundary of SMAP data set
  double *lat, *lon;
  int line_count, sample_count, vertex_count;
  check_smap_file(inFile, &line_count, &sample_count);
  lat = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  lon = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  read_smap_outline(inFile, &vertex_count, lat, lon);  
  lat[vertex_count] = lat[0];
  lon[vertex_count] = lon[0];

  *dbf = header;  
  *nAttr = n;
  *latArray = lat;
  *lonArray = lon;
  *nCoords = vertex_count+1;
}

int smap2shape(char *inFile, char *outFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  double *lat, *lon;
  int line_count, sample_count, vertex_count;

  // Initalize the database file
  shape_smap_init(outFile);
  open_shape(outFile, &dbase, &shape);

  // Read lat/lon for boundary of SMAP data set
  check_smap_file(inFile, &line_count, &sample_count);
  lat = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  lon = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  read_smap_outline(inFile, &vertex_count, lat, lon);  
  lat[vertex_count] = lat[0];
  lon[vertex_count] = lon[0];

	// Read metadata
  smap_meta *smap = read_smap_meta(inFile);	

  // Write attributes
  DBFWriteStringAttribute(dbase, 0, 0, smap->file_name);
  DBFWriteStringAttribute(dbase, 0, 1, smap->orbit_start_date_time);
  DBFWriteStringAttribute(dbase, 0, 2, smap->orbit_stop_date_time);
  DBFWriteStringAttribute(dbase, 0, 3, smap->orbit_direction);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject =
    SHPCreateSimpleObject(SHPT_POLYGON, vertex_count+1, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);  
  
  // Clean up
  FREE(lat);
  FREE(lon);
  FREE(smap);
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return 1;
}
