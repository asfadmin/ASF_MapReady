#include "smap.h"
#include "doppler.h"
#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_endian.h"
#include "dateUtil.h"
#include <ctype.h>
#include <hdf5.h>

smap_meta *smap_meta_init(void)
{
  smap_meta *smap;
  smap = (smap_meta *) CALLOC(1, sizeof(smap_meta));
  strcpy(smap->algorithm_descriptor, MAGIC_UNSET_STRING);
  strcpy(smap->collection_description, MAGIC_UNSET_STRING);
  smap->east_bounding_coordinate = MAGIC_UNSET_INT;
  strcpy(smap->equator_crossing_date, MAGIC_UNSET_STRING);
  smap->equator_crossing_longitude = MAGIC_UNSET_DOUBLE;
  strcpy(smap->equator_crossing_time, MAGIC_UNSET_STRING);
  strcpy(smap->granule_name, MAGIC_UNSET_STRING);
  smap->grid_spacing = MAGIC_UNSET_DOUBLE;
  strcpy(smap->input_name, MAGIC_UNSET_STRING);
  smap->north_bounding_coordinate = MAGIC_UNSET_INT;
  strcpy(smap->orbit_direction, MAGIC_UNSET_STRING);
  smap->orbit_inclination = MAGIC_UNSET_DOUBLE;
  smap->orbit_period = MAGIC_UNSET_DOUBLE;
  strcpy(smap->orbit_start_date, MAGIC_UNSET_STRING);
  strcpy(smap->orbit_start_time, MAGIC_UNSET_STRING);
  strcpy(smap->orbit_stop_date, MAGIC_UNSET_STRING);
  strcpy(smap->orbit_stop_time, MAGIC_UNSET_STRING);
  strcpy(smap->production_date_time, MAGIC_UNSET_STRING);
  strcpy(smap->project_id, MAGIC_UNSET_STRING);
  smap->radar_resolution = MAGIC_UNSET_DOUBLE;
  strcpy(smap->range_beginning_date, MAGIC_UNSET_STRING);
  strcpy(smap->range_beginning_time, MAGIC_UNSET_STRING);
  strcpy(smap->range_ending_date, MAGIC_UNSET_STRING);
  strcpy(smap->range_ending_time, MAGIC_UNSET_STRING);
  strcpy(smap->short_name, MAGIC_UNSET_STRING);
  smap->south_bounding_coordinate = MAGIC_UNSET_INT;
  smap->start_orbit_number = MAGIC_UNSET_INT;
  smap->stop_orbit_number = MAGIC_UNSET_INT;
  smap->version_id = MAGIC_UNSET_INT;
  smap->west_bounding_coordinate = MAGIC_UNSET_INT;

  return smap;
}

static int get_float_attr(hid_t object, char *name)
{
  float value;
  hid_t attr = H5Aopen(object, name, H5P_DEFAULT);
  hid_t type = H5Aget_type(attr);
  hid_t space = H5Aget_space(attr);
  if (H5Tget_class(type) == H5T_FLOAT)
    H5Aread(attr, H5Tget_native_type(type, H5T_DIR_DEFAULT), &value);
  H5Tclose(type);
  H5Sclose(space);
  H5Aclose(attr);

  return value;
}

static int get_int_attr(hid_t object, char *name)
{
  int value;
  hid_t attr = H5Aopen(object, name, H5P_DEFAULT);
  hid_t type = H5Aget_type(attr);
  hid_t space = H5Aget_space(attr);
  if (H5Tget_class(type) == H5T_INTEGER)
    H5Aread(attr, H5Tget_native_type(type, H5T_DIR_DEFAULT), &value);
  H5Tclose(type);
  H5Sclose(space);
  H5Aclose(attr);

  return value;
}

static char *get_string_attr(hid_t object, char *name)
{
  char *str = NULL;
  hid_t attr = H5Aopen(object, name, H5P_DEFAULT);
  hid_t type = H5Aget_type(attr);
  hid_t space = H5Aget_space(attr);
  if (H5Tget_class(type) == H5T_STRING) {
    int size = H5Tget_size(type);
    str = (char *) MALLOC(sizeof(char)*size);
    H5Aread(attr, H5Tget_native_type(type, H5T_DIR_DEFAULT), str);
  }
  H5Tclose(type);
  H5Sclose(space);
  H5Aclose(attr);

  return str;
}

smap_meta *read_smap_meta(const char *dataFile)
{
  smap_meta *smap = smap_meta_init();
  hid_t hFile, hMeta;
  hFile = H5Fopen(dataFile, H5F_ACC_RDONLY, H5P_DEFAULT);
  hMeta = H5Gopen(hFile, "Metadata", H5P_DEFAULT);
  strcpy(smap->algorithm_descriptor, 
	 get_string_attr(hMeta, "AlgorithmDescriptor"));
  strcpy(smap->collection_description,
	 get_string_attr(hMeta, "CollectionDescription"));
  smap->east_bounding_coordinate = 
    get_int_attr(hMeta, "EastBoundingCoordinate");
  strcpy(smap->equator_crossing_date,
	 get_string_attr(hMeta, "EquatorCrossingDate"));
  smap->equator_crossing_longitude = 
    get_float_attr(hMeta, "EquatorCrossingLongitude");
  strcpy(smap->equator_crossing_time,
	 get_string_attr(hMeta, "EquatorCrossingTime"));
  strcpy(smap->granule_name, get_string_attr(hMeta, "GranuleName"));
  smap->grid_spacing = get_float_attr(hMeta, "GridSpacing");
  strcpy(smap->input_name, get_string_attr(hMeta, "InputName"));
  smap->north_bounding_coordinate = 
    get_int_attr(hMeta, "NorthBoundingCoordinate");
  strcpy(smap->orbit_direction, get_string_attr(hMeta, "OrbitDirection"));
  smap->orbit_inclination = get_float_attr(hMeta, "OrbitInclination");
  smap->orbit_period = get_float_attr(hMeta, "OrbitPeriod");
  strcpy(smap->orbit_start_date, get_string_attr(hMeta, "OrbitStartDate"));
  strcpy(smap->orbit_start_time, get_string_attr(hMeta, "OrbitStartTime"));
  strcpy(smap->orbit_stop_date, get_string_attr(hMeta, "OrbitStopDate"));
  strcpy(smap->orbit_stop_time, get_string_attr(hMeta, "OrbitStopTime"));
  strcpy(smap->production_date_time, 
	 get_string_attr(hMeta, "ProductionDateTime"));
  strcpy(smap->project_id, get_string_attr(hMeta, "ProjectID"));
  smap->radar_resolution = get_float_attr(hMeta, "RadarResolution");
  strcpy(smap->range_beginning_date, 
	 get_string_attr(hMeta, "RangeBeginningDate"));
  strcpy(smap->range_beginning_time,
	 get_string_attr(hMeta, "RangeBeginningTime"));
  strcpy(smap->range_ending_date, get_string_attr(hMeta, "RangeEndingDate"));
  strcpy(smap->range_ending_time, get_string_attr(hMeta, "RangeEndingTime"));
  strcpy(smap->short_name, get_string_attr(hMeta, "ShortName"));
  smap->south_bounding_coordinate = 
    get_int_attr(hMeta, "SouthBoundingCoordinate");
  smap->start_orbit_number = get_int_attr(hMeta, "StartOrbitNumber");
  smap->stop_orbit_number = get_int_attr(hMeta, "StopOrbitNumber");
  smap->version_id = get_int_attr(hMeta, "VersionID");
  smap->west_bounding_coordinate = 
    get_int_attr(hMeta, "WestBoundingCoordinate");
  H5Gclose(hMeta);
  H5Fclose(hFile);

  return smap;
}

static int compare_values(const int *a, const int *b)
{
  int tmp = *a - *b;
  if (tmp > 0)
    return 1;
  else if (tmp < 0)
    return -1;
  else
    return 0;
}

static void read_smap_bounds(char *inDataName, float latUL, float lonUL,
			     float latLR, float lonLR, int *line, int *sample,
			     int *height, int *width)
{
  meta_parameters *meta = meta_read(inDataName);
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;
  float *lats = (float *) MALLOC(sizeof(float)*ns*nl);
  float *lons = (float *) MALLOC(sizeof(float)*ns*nl);
  hid_t file, group, dataset, datatype, dataspace, memspace;
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  hsize_t count[2], pixels[1], offset[2];
  offset[0] = 0;
  offset[1] = 0;
  count[0] = nl;
  count[1] = ns;
  pixels[0] = ns*nl;
  memspace = H5Screate_simple(1, pixels, NULL);

  // Latitude
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, lats);
  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  
  // Longitude
  dataset = H5Dopen(group, "cell_lon", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, lons);
  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);

  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);

  float latUR = latUL;
  float lonUR = lonLR;
  float latLL = latLR;
  float lonLL = lonUL;
  float lat, lon, diff, minUL, minUR, minLL, minLR;
  int lines[4], samples[4];
  minUL = minUR = minLL = minLR = 9999999;
  // We are looking for the nearest neighbor for the moment
  int ii, kk;
  for (ii=0; ii<nl; ii++) {
    for (kk=0; kk<ns; kk++) {
      lat = lats[ii*ns+kk];
      lon = lons[ii*ns+kk];
      diff = fabs(lat - latUL) + fabs(lon - lonUL);
      if (diff < minUL) {
	minUL = diff;
	lines[0] = ii;
	samples[0] = kk;
      }
      diff = fabs(lat - latUR) + fabs(lon - lonUR);
      if (diff < minUR) {
	minUR = diff;
	lines[1] = ii;
	samples[1] = kk;
      }
      diff = fabs(lat - latLL) + fabs(lon - lonLL);
      if (diff < minLL) {
	minLL = diff;
	lines[2] = ii;
	samples[2] = kk;
      }
      diff = fabs(lat - latLR) + fabs(lon - lonLR);
      if (diff < minLR) {
	minLR = diff;
	lines[3] = ii;
	samples[3] = kk;
      }
    }
  }
  meta_free(meta);
  qsort(lines, 4, sizeof(int), compare_values);
  qsort(samples, 4, sizeof(int), compare_values);
  float latSubUL = lats[lines[0]*ns+samples[0]];
  float lonSubUL = lons[lines[0]*ns+samples[0]];
  float latSubLR = lats[lines[3]*ns+samples[3]];
  float lonSubLR = lons[lines[3]*ns+samples[3]];
  if (!FLOAT_EQUIVALENT(latUL, latSubUL) || 
      !FLOAT_EQUIVALENT(lonUL, lonSubUL) ||
      !FLOAT_EQUIVALENT(latLR, latSubLR) ||
      !FLOAT_EQUIVALENT(lonLR, lonSubLR)) {
    asfPrintStatus("Subset exceeded imaged area!\nMaximum subset area:\n");
    asfPrintStatus("Upper left - Lat: %.4lf, Lon: %.4lf\n", latSubUL, lonSubUL);
    asfPrintStatus("Lower right - Lat: %.4lf, Lon: %.4lf\n", latSubLR, lonSubLR);
  }
  FREE(lats);
  FREE(lons);
  *line = lines[0];
  *sample = samples[0];
  *height = lines[3] - lines[0];
  *width = samples[3] - samples[0];
}

static void read_smap_subset(char *dataName, int band, 
			     char *inDataName, char *outDataName,
			     float latUL, float lonUL, float latLR, float lonLR)
{
  FILE *fp;
  meta_parameters *meta = meta_read(inDataName);
  int line, sample, height, width;
  read_smap_bounds(inDataName, latUL, lonUL, latLR, lonLR,
		   &line, &sample, &height, &width);
  hid_t file, group, dataset, dataspace, memspace;
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  dataset = H5Dopen(group, dataName, H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  hsize_t count[2], pixels[1], offset[2];
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;
  meta_get_latLon(meta, line+height/2, sample+width/2, 0.0,
		  &meta->general->center_latitude, 
		  &meta->general->center_longitude);
  meta->general->sample_count = width;
  meta->general->line_count = height;
  meta->general->start_line = line;
  meta->general->start_sample = sample;

  // Latitude
  offset[0] = 0;
  offset[1] = 0;
  count[0] = nl;
  count[1] = ns;
  pixels[0] = ns*nl;
  float *lats = (float *) MALLOC(sizeof(float)*ns*nl);
  memspace = H5Screate_simple(1, pixels, NULL);
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, lats);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  
  // Longitude
  float *lons = (float *) MALLOC(sizeof(float)*ns*nl);
  dataset = H5Dopen(group, "cell_lon", H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, lons);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Sclose(memspace);
  
  // Band
  offset[1] = sample;
  count[0] = 1;
  count[1] = width;
  pixels[0] = width;
  dataset = H5Dopen(group, dataName, H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  memspace = H5Screate_simple(1, pixels, NULL);
  float *amp = (float *) MALLOC(sizeof(float)*width);
  if (band == 0)
    fp = FOPEN(outDataName, "wb");
  else
    fp = FOPEN(outDataName, "ab");
  asfPrintStatus("   Band: %s\n", dataName);
  int ii, kk;
  for (ii=line; ii<line+height; ii++) {
    offset[0] = ii;
    H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
    H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, amp);
    /*
    if (strcmp_case(dataName, "cell_lat") != 0 &&
	strcmp_case(dataName, "cell_lon") != 0) {
      for (kk=sample; kk<sample+width; kk++) {
	if (lats[ii*ns+kk] >= latLR && lats[ii*ns+kk] <= latUL &&
	    lons[ii*ns+kk] >= lonUL && lons[ii*ns+kk] <= lonLR)
	  ;
	else
	  amp[kk-sample] = MAGIC_UNSET_DOUBLE;
      }
    }
    */
    put_band_float_line(fp, meta, band, ii-line, amp);
    asfLineMeter(ii-line, height);
  }
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);
  FCLOSE(fp);
  FREE(amp);
  meta_write(meta, outDataName);
  meta_free(meta);
}

static void read_smap_data(char *dataName, int band, 
			   char *inDataName, char *outDataName)
{
  FILE *fp;
  meta_parameters *meta = meta_read(inDataName);
  hid_t file, group, dataset, dataspace, memspace;
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  dataset = H5Dopen(group, dataName, H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  hsize_t count[2], line[1];
  hsize_t offset[2];
  int ns = meta->general->sample_count;
  offset[0] = 0;
  offset[1] = 0;
  count[0] = 1;
  count[1] = ns;
  line[0] = ns;
  memspace = H5Screate_simple(1, line, NULL);
  float *amp = (float *) MALLOC(sizeof(float)*ns);
  if (band == 0)
    fp = FOPEN(outDataName, "wb");
  else
    fp = FOPEN(outDataName, "ab");
  asfPrintStatus("   Band: %s\n", dataName);
  int ii;
  for (ii=0; ii<meta->general->line_count; ii++) {
    offset[0] = ii;
    H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
    H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, amp);
    put_band_float_line(fp, meta, band, ii, amp);
    asfLineMeter(ii, meta->general->line_count);
  }
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);
  FCLOSE(fp);
  FREE(amp);
  meta_free(meta);
}

static void read_smap_pixel(meta_parameters *meta, char *inDataName,
			    int line, int sample, float *lat, float *lon)
{
  hsize_t count[2], pixel[1];
  hsize_t offset[2];
  hid_t file, group, dataset, datatype, dataspace, memspace;
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  offset[0] = line;
  offset[1] = 0;
  count[0] = 1;
  count[1] = meta->general->sample_count;
  pixel[0] = meta->general->sample_count;
  memspace = H5Screate_simple(1, pixel, NULL);
  int ii;
  int ns = meta->general->sample_count;
  float *values = (float *) MALLOC(sizeof(float)*ns);
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  if (sample == 0) {
    ii = 0;
    while (!meta_is_valid_double(values[ii]) && (ii < ns))
      ii++;
    *lat = values[ii];
  }
  else if (sample == ns-1) {
    ii = ns - 1;
    while (!meta_is_valid_double(values[ii]) && (ii >= 0))
      ii--;
    *lat = values[ii];
  }
  else
    *lat = values[sample];
  dataset = H5Dopen(group, "cell_lon", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  if (sample == 0) {
    ii = 0;
    while (!meta_is_valid_double(values[ii]) && (ii < ns))
      ii++;
    *lon = values[ii];
  }
  else if (sample == ns-1) {
    ii = ns - 1;
    while (!meta_is_valid_double(values[ii]) && (ii >= 0))
      ii--;
    *lon = values[ii];
  }
  else
    *lon = values[sample];
  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);
}

void import_smap(const char *inBaseName, const char *outBaseName,
		 float latUL, float lonUL, float latLR, float lonLR)
{
  smap_meta *smap;
  meta_parameters *meta;
  char *inDataName=NULL, *outDataName=NULL;
  hsize_t dims[2];
  hid_t file, group, class, dataset, datatype, dataspace;
  float lat, lon;
  int subset = FALSE;
  
  // Take care of file names
  if (!fileExists(inBaseName))
    inDataName = appendExt(inBaseName, ".h5");
  else {
    inDataName = (char *) MALLOC(sizeof(char)*1024);
    strcpy(inDataName, inBaseName);
  }
  
  // Check whether the file is actually a real HDF5 data set
  if (!H5Fis_hdf5(inDataName))
    asfPrintError("File (%s) is not in HDF5 format!\n", inDataName);

  if (meta_is_valid_double(latUL) && meta_is_valid_double(lonUL) &&
      meta_is_valid_double(latLR) && meta_is_valid_double(lonLR))
    subset = TRUE;

  // Determine the image dimensions
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  dataset = H5Dopen(group, "cell_sigma0_hh_aft", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  class = H5Tget_class(datatype);
  if (class != H5T_FLOAT) 
    asfPrintError("Data is supposed to be floating point!\n");
  dataspace = H5Dget_space(dataset);
  H5Sget_simple_extent_dims(dataspace, dims, NULL);
  int inRows = dims[0];
  int inCols = dims[1];
  
  // Read metadata
  smap = read_smap_meta(inDataName);
  meta = smap2meta(smap);
  meta->general->line_count = meta->sar->original_line_count = inRows;
  meta->general->sample_count = meta->sar->original_sample_count = inCols;
  meta->general->band_count = 13;
  strcpy(meta->general->bands, "HH_aft,HH_fore,HH_kp,HV_aft,HV_fore,HV_kp,"
	 "VV_aft,VV_fore,VV_kp,LAT,LON,grid_lat,grid_lon");
  read_smap_pixel(meta, inDataName, meta->general->line_count/2, 
		  meta->general->sample_count/2, &lat, &lon);
  meta->general->center_latitude = (double) lat;
  meta->general->center_longitude = (double) lon;

  // Determine location block
  meta->location = meta_location_init();
  if (subset) {
    meta->location->lat_start_near_range = (double) latUL;
    meta->location->lon_start_near_range = (double) lonUL;
    meta->location->lat_start_far_range = (double) latUL;
    meta->location->lon_start_far_range = (double) lonLR;
    meta->location->lat_end_near_range = (double) latLR;
    meta->location->lon_end_near_range = (double) lonUL;
    meta->location->lat_end_far_range = (double) latLR;
    meta->location->lon_end_far_range = (double) lonLR;
  }
  else {
    read_smap_pixel(meta, inDataName, 0, 0, &lat, &lon);
    meta->location->lat_start_near_range = (double) lat;
    meta->location->lon_start_near_range = (double) lon;
    read_smap_pixel(meta, inDataName, 0, meta->general->sample_count-1, 
		    &lat, &lon);
    meta->location->lat_start_far_range = (double) lat;
    meta->location->lon_start_far_range = (double) lon;
    read_smap_pixel(meta, inDataName, meta->general->line_count-1, 0, 
		    &lat, &lon);
    meta->location->lat_end_near_range = (double) lat;
    meta->location->lon_end_near_range = (double) lon;
    read_smap_pixel(meta, inDataName, 
		    meta->general->line_count-1, meta->general->sample_count-1,
		    &lat, &lon);
    meta->location->lat_end_far_range = (double) lat;
    meta->location->lon_end_far_range = (double) lon;
  }
  meta_write(meta, inDataName);
  
  // Read data
  outDataName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName+25)));
  sprintf(outDataName, "%s.img", outBaseName);
  meta_write(meta, outDataName);

  if (subset) {
    read_smap_subset("cell_sigma0_hh_aft", 0, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_sigma0_hh_fore", 1, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_kp_hh", 2, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_sigma0_hv_aft", 3, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_sigma0_hv_fore", 4, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_kp_hv", 5, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_sigma0_vv_aft", 6, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_sigma0_vv_fore", 7, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_kp_vv", 8, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_lat", 9, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cell_lon", 10, inDataName, outDataName,
		     latUL, lonUL, latLR, lonLR);
    read_smap_subset("cylindrical_grid_latitude_index", 11, inDataName, 
		     outDataName, latUL, lonUL, latLR, lonLR);
    read_smap_subset("cylindrical_grid_longitude_index", 12, inDataName, 
		     outDataName, latUL, lonUL, latLR, lonLR);
  }
  else {
    read_smap_data("cell_sigma0_hh_aft", 0, inDataName, outDataName);
    read_smap_data("cell_sigma0_hh_fore", 1, inDataName, outDataName);
    read_smap_data("cell_kp_hh", 2, inDataName, outDataName);
    read_smap_data("cell_sigma0_hv_aft", 3, inDataName, outDataName);
    read_smap_data("cell_sigma0_hv_fore", 4, inDataName, outDataName);
    read_smap_data("cell_kp_hv", 5, inDataName, outDataName);
    read_smap_data("cell_sigma0_vv_aft", 6, inDataName, outDataName);
    read_smap_data("cell_sigma0_vv_fore", 7, inDataName, outDataName);
    read_smap_data("cell_kp_vv", 8, inDataName, outDataName);
    read_smap_data("cell_lat", 9, inDataName, outDataName);
    read_smap_data("cell_lon", 10, inDataName, outDataName);
    read_smap_data("cylindrical_grid_latitude_index", 
		   11, inDataName, outDataName);
    read_smap_data("cylindrical_grid_longitude_index", 
		   12, inDataName, outDataName);
  }

  // Clean up
  FREE(smap);
  FREE(inDataName);
  FREE(outDataName);
  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Gclose(group);
  H5Fclose(file);
  meta_free(meta);
}
