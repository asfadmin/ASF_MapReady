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

#define MAX_NAME 1024

static void appendBand(int band, char *bandStr, char *bands)
{
	if (band == 0)
		strcpy(bands, bandStr);
	else {
		strcat(bands, ",");
		strcat(bands, bandStr);
	}
}

smap_meta *smap_meta_init(void)
{
  smap_meta *smap;
  smap = (smap_meta *) CALLOC(1, sizeof(smap_meta));

	// dataset identification
  strcpy(smap->file_name, MAGIC_UNSET_STRING);
  strcpy(smap->short_name, MAGIC_UNSET_STRING);
  strcpy(smap->creation_date_time, MAGIC_UNSET_STRING);
    
  // extent
  smap->north_bounding_coordinate = MAGIC_UNSET_DOUBLE;
  smap->south_bounding_coordinate = MAGIC_UNSET_DOUBLE;
  smap->east_bounding_coordinate = MAGIC_UNSET_DOUBLE;
  smap->west_bounding_coordinate = MAGIC_UNSET_DOUBLE;
  strcpy(smap->range_beginning_date_time, MAGIC_UNSET_STRING);
  strcpy(smap->range_ending_date_time, MAGIC_UNSET_STRING);

	// orbital information
	smap->argument_of_perigee = MAGIC_UNSET_DOUBLE;
  strcpy(smap->equator_crossing_date_time, MAGIC_UNSET_STRING);
  smap->equator_crossing_longitude = MAGIC_UNSET_DOUBLE;
  smap->inclination = MAGIC_UNSET_DOUBLE;
  smap->mean_motion = MAGIC_UNSET_DOUBLE;
  smap->argument_of_perigee = MAGIC_UNSET_DOUBLE;
  smap->right_ascension_ascending_node = MAGIC_UNSET_DOUBLE;
  smap->orbit_period = MAGIC_UNSET_DOUBLE;
  smap->eccentricity = MAGIC_UNSET_DOUBLE;
  strcpy(smap->orbit_direction, MAGIC_UNSET_STRING);
  strcpy(smap->orbit_start_date_time, MAGIC_UNSET_STRING);
  strcpy(smap->orbit_stop_date_time, MAGIC_UNSET_STRING);

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
  
  hid_t hFile = H5Fopen(dataFile, H5F_ACC_RDONLY, H5P_DEFAULT);
  hid_t hMeta = H5Gopen(hFile, "Metadata", H5P_DEFAULT);
  
  // Dataset identification
  hid_t hData = H5Gopen(hMeta, "DatasetIdentification", H5P_DEFAULT);
  strcpy(smap->short_name, get_string_attr(hData, "SMAPShortName"));
  strcpy(smap->file_name, get_string_attr(hData, "fileName"));
  strcpy(smap->creation_date_time, get_string_attr(hData, "creationDate"));
  H5Gclose(hData);
	 
	// Extent
	hid_t hExtent = H5Gopen(hMeta, "Extent", H5P_DEFAULT);
	/*
  smap->north_bounding_coordinate = 
    get_int_attr(hExtent, "northBoundingCoordinate");
  smap->south_bounding_coordinate = 
    get_int_attr(hExtent, "southBoundingCoordinate");
  smap->east_bounding_coordinate = 
    get_int_attr(hExtent, "eastBoundingCoordinate");
  smap->west_bounding_coordinate = 
    get_int_attr(hExtent, "westBoundingCoordinate");
  */
  strcpy(smap->range_beginning_date_time, 
	  get_string_attr(hExtent, "rangeBeginningDateTime"));
  strcpy(smap->range_ending_date_time, 
  	get_string_attr(hExtent, "rangeEndingDateTime"));
  H5Gclose(hExtent);
    
  // Orbital information
  hid_t hOrbit = H5Gopen(hMeta, "OrbitMeasuredLocation", H5P_DEFAULT);
  smap->argument_of_perigee = get_float_attr(hOrbit, "argumentOfPerigee");
	smap->eccentricity = get_float_attr(hOrbit, "eccentricity");
	smap->epoch = get_float_attr(hOrbit, "epoch");
  strcpy(smap->equator_crossing_date_time, 
  	get_string_attr(hOrbit, "equatorCrossingDateTime"));
  smap->equator_crossing_longitude = 
    get_float_attr(hOrbit, "equatorCrossingLongitude");
  strcpy(smap->orbit_start_date_time, 
  	get_string_attr(hOrbit, "halfOrbitStartDateTime"));
  strcpy(smap->orbit_stop_date_time, 
  	get_string_attr(hOrbit, "halfOrbitStopDateTime"));
  smap->inclination = get_float_attr(hOrbit, "inclination");
  smap->mean_motion = get_float_attr(hOrbit, "meanMotion");
  strcpy(smap->orbit_direction, get_string_attr(hOrbit, "orbitDirection"));
  smap->orbit_period = get_float_attr(hOrbit, "orbitPeriod");
  H5Gclose(hOrbit);
  
  H5Gclose(hMeta);
  H5Fclose(hFile);

  return smap;
}

static int find_dataset(char *inDataName, char *dataset)
{
	int ii, otype, ret=FALSE;
	ssize_t len;
	hsize_t num_objs;
	char name[MAX_NAME];
	
  hid_t file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  hid_t group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);	
	H5Gget_num_objs(group, &num_objs);
	for (ii=0; ii<num_objs; ii++) {
		len = H5Gget_objname_by_idx(group, (hsize_t) ii, name, (size_t) MAX_NAME);
		otype = H5Gget_objtype_by_idx(group, (size_t) ii);
		if (strcmp_case(dataset, name) == 0 && otype == H5G_DATASET)
			ret = TRUE;
	}
  H5Gclose(group);
  H5Fclose(file);	
	
	return ret;
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

static void read_smap_bounds(char *inDataName, float latUL, float lonUL,
			     float latLR, float lonLR, int *line, int *sample,
			     int *height, int *width)
{
  meta_parameters *meta = meta_read(inDataName);
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;
  int subset;
  if (meta_is_valid_double(latUL) && meta_is_valid_double(lonUL) &&
      meta_is_valid_double(latLR) && meta_is_valid_double(lonLR))
    subset = TRUE;
  else if (meta_is_valid_double(latUL) && meta_is_valid_double(latLR))
  	subset = FALSE;
  else
  	asfPrintStatus("Can't determine bounding box!\n");
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
  float lat, lon, diff, minUL, minUR, minLL, minLR, minLat, maxLat;
  int lines[4], samples[4];
  minUL = minUR = minLL = minLR = minLat = maxLat = 9999999;
  int ii, kk;

  // Upper constraints
  int found = FALSE;
  for (ii=0; ii<nl; ii++) {
    for (kk=0; kk<ns; kk++) {
      lat = lats[ii*ns+kk];
      lon = lons[ii*ns+kk];
      if (subset) {
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
      else {
      	diff = lat - latUL;
      	if (diff < 0 && !found) {
      		found = TRUE;
      		lines[0] = ii-1;
      	}
      }
    }
  }
  // Lower constraints
  found = FALSE;
  for (ii=nl-1; ii>=0; ii--) {
  	for (kk=0; kk<ns; kk++) {
      lat = lats[ii*ns+kk];
      lon = lons[ii*ns+kk];
      if (subset) {
			}
			else {
				diff = latLR - lat;
				if (diff < 0 && !found) {
					found = TRUE;
					lines[1] = ii+1;
				}
			}  		
  	}
  }
  meta_free(meta);
  if (subset) {
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
    	asfPrintStatus("Upper left - Lat: %.4f, Lon: %.4f\n", latSubUL, lonSubUL);
    	asfPrintStatus("Lower right - Lat: %.4f, Lon: %.4f\n", latSubLR, lonSubLR);
  	}
  	FREE(lats);
  	FREE(lons);
  	*line = lines[0];
  	*sample = samples[0];
  	*height = lines[3] - lines[0];
  	*width = samples[3] - samples[0];
  }
  else {
  	FREE(lats);
  	FREE(lons);
  	*line = lines[0];
  	*height = (lines[1] - lines[0]);
  }
}

static update_geolocation(char *inDataName, char *outDataName) 
{
	float lat, lon;
	meta_parameters *meta = meta_read(outDataName);
	int startLine = meta->general->start_line;
	int startSample = meta->general->start_sample;
	int endLine = startLine + meta->general->line_count - 1;
	int endSample = startSample + meta->general->sample_count - 1;
	int centerLine = (endLine - startLine) / 2;
	int centerSample = (endSample - startSample) / 2;

	// Update center coordinates
	read_smap_pixel(meta, inDataName, centerLine, centerSample, &lat, &lon);
	meta->general->center_latitude = (double) lat;
	meta->general->center_longitude = (double) lon;

	// Update location block
  if (!meta->location)
  	meta->location = meta_location_init();
  read_smap_pixel(meta, inDataName, startLine, startSample, &lat, &lon);
  meta->location->lat_start_near_range = (double) lat;
  meta->location->lon_start_near_range = (double) lon;
  read_smap_pixel(meta, inDataName, startLine, endSample, &lat, &lon);
  meta->location->lat_start_far_range = (double) lat;
  meta->location->lon_start_far_range = (double) lon;
  read_smap_pixel(meta, inDataName, endLine, startSample, &lat, &lon);
  meta->location->lat_end_near_range = (double) lat;
  meta->location->lon_end_near_range = (double) lon;
  read_smap_pixel(meta, inDataName, endLine, endSample, &lat, &lon);
  meta->location->lat_end_far_range = (double) lat;
  meta->location->lon_end_far_range = (double) lon;
  /*
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
  */
  
  meta_write(meta, outDataName);
  meta_free(meta);
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

static void read_smap_constraint(char *dataName, int band, 
			     char *inDataName, char *outDataName,
			     float latLR, float latUL)
{
	
  FILE *fp;
  meta_parameters *meta = meta_read(inDataName);
  int ii, kk, line, sample, height, width;
  float lonUL = MAGIC_UNSET_DOUBLE;
  float lonLR = MAGIC_UNSET_DOUBLE;
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
  meta_get_latLon(meta, line+height/2, ns/2, 0.0,
		  &meta->general->center_latitude, 
		  &meta->general->center_longitude);

  // Latitude
  offset[0] = line;
  offset[1] = 0;
  count[0] = height;
  count[1] = ns;
  pixels[0] = ns*height;
  float *lats = (float *) MALLOC(sizeof(float)*ns*height);
  memspace = H5Screate_simple(1, pixels, NULL);
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, lats);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  
  // Band
  dataset = H5Dopen(group, dataName, H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  float *amp = (float *) MALLOC(sizeof(float)*ns*height);
  if (band == 0)
    fp = FOPEN(outDataName, "wb");
  else
    fp = FOPEN(outDataName, "ab");
  asfPrintStatus("   Band: %s\n", dataName);
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, amp);
  for (ii=0; ii<height; ii++) 
  	for (kk=0; kk<ns; kk++)
			if (lats[ii*ns+kk] < latLR || lats[ii*ns+kk] > latUL)
	  		amp[ii*ns+kk] = MAGIC_UNSET_DOUBLE;
  meta->general->band_count = band+1;
  meta->general->line_count = height;
  put_band_float_lines(fp, meta, band, 0, height, amp);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);
  FCLOSE(fp);
  FREE(lats);
  FREE(amp);
  meta_free(meta);
}

static int read_smap_data(char *dataName, int band, 
			   char *inDataName, char *outDataName)
{
	int ret = FALSE;
  FILE *fp;
  meta_parameters *meta = meta_read(inDataName);
  hid_t file, group, dataset, dataspace, memspace;
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  dataset = H5Dopen(group, dataName, H5P_DEFAULT);
  if (dataset >= 0) {
  	dataspace = H5Dget_space(dataset);
  	hsize_t count[2], offset[2], line[1];
  	int ns = meta->general->sample_count;
  	int nl = meta->general->line_count;
  	offset[0] = 0;
  	offset[1] = 0;
  	count[0] = nl;
  	count[1] = ns;
  	line[0] = ns*nl;
  	memspace = H5Screate_simple(1, line, NULL);
  	float *amp = (float *) MALLOC(sizeof(float)*ns*nl);
  	if (band == 0)
    	fp = FOPEN(outDataName, "wb");
  	else
    	fp = FOPEN(outDataName, "ab");
  	asfPrintStatus("   Band: %s\n", dataName);
    H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
    H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, amp);
    meta->general->band_count = band+1;
    put_band_float_lines(fp, meta, band, 0, nl, amp);
  	H5Sclose(dataspace);
  	H5Sclose(memspace);
	  H5Dclose(dataset);
	  FREE(amp);
  	ret = TRUE;
  }
  H5Gclose(group);
  H5Fclose(file);
  FCLOSE(fp);
  meta_free(meta);
  
  return ret;
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
  int subset = FALSE, latConstraint = FALSE;
  
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
  else if (meta_is_valid_double(latUL) && meta_is_valid_double(latLR))
  	latConstraint = TRUE;

  // Determine the image dimensions
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
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
  read_smap_pixel(meta, inDataName, meta->general->line_count/2, 
		  meta->general->sample_count/2, &lat, &lon);
  meta->general->center_latitude = (double) lat;
  meta->general->center_longitude = (double) lon;
	strcpy(meta->general->bands, "");
	
  // Read data
  outDataName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+25));
  sprintf(outDataName, "%s.img", outBaseName);
	int band = 0;

  if (subset) {
  	if (find_dataset(inDataName, "cell_sigma0_hh_fore")) {
    	read_smap_subset("cell_sigma0_hh_fore", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "HH_fore", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cell_sigma0_hh_aft")) {
	    read_smap_subset("cell_sigma0_hh_aft", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "HH_aft", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cell_sigma0_xy_fore")) {
	    read_smap_subset("cell_sigma0_xy_fore", band, inDataName, outDataName,
	    	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "HV_fore", meta->general->bands);
    	band++;
	  }
	  if (find_dataset(inDataName, "cell_sigma0_xy_aft")) {
	    read_smap_subset("cell_sigma0_xy_aft", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "HV_aft", meta->general->bands);
    	band++;
	  }
		if (find_dataset(inDataName, "cell_sigma0_xpol_fore")) {
	    read_smap_subset("cell_sigma0_xpol_fore", band, inDataName, outDataName,
	    	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "HV_fore", meta->general->bands);
    	band++;
	  }
	  if (find_dataset(inDataName, "cell_sigma0_xpol_aft")) {
	    read_smap_subset("cell_sigma0_xpol_aft", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "HV_aft", meta->general->bands);
    	band++;
	  }
	  if (find_dataset(inDataName, "cell_sigma0_vv_fore")) {
	    read_smap_subset("cell_sigma0_vv_fore", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "VV_fore", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cell_sigma0_vv_aft")) {
	    read_smap_subset("cell_sigma0_vv_aft", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "VV_aft", meta->general->bands);
    	band++;
		}
    if (find_dataset(inDataName, "faraday_rotation_angle")) {
    	read_smap_subset("faraday_rotation_angle", band, inDataName, outDataName,
    		latUL, lonUL, latLR, lonLR);
    	appendBand(band, "faraday", meta->general->bands);
    	band++;
    }
		if (find_dataset(inDataName, "cell_lat")) {
	    read_smap_subset("cell_lat", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "lat", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cell_lon")) {
	    read_smap_subset("cell_lon", band, inDataName, outDataName,
		  	latUL, lonUL, latLR, lonLR);
    	appendBand(band, "lon", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cylindrical_grid_row_index")) {
    	read_smap_subset("cylindrical_grid_row_index", band, inDataName, 
		  	outDataName, latUL, lonUL, latLR, lonLR);
    	appendBand(band, "cyl_row", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cylindrical_grid_column_index")) {
	    read_smap_subset("cylindrical_grid_column_index", band, inDataName, 
		  	outDataName, latUL, lonUL, latLR, lonLR);
    	appendBand(band, "lon", meta->general->bands);
    	band++;
		}		  	
    if (find_dataset(inDataName, "polar_grid_row_index")) {
    	read_smap_subset("polar_grid_row_index", band, inDataName, outDataName,
    		latUL, lonUL, latLR, lonLR);
    	appendBand(band, "pol_row", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "polar_grid_column_index")) {
    	read_smap_subset("polar_grid_column_index", band, inDataName, outDataName,
    		latUL, lonUL, latLR, lonLR);
    	appendBand(band, "pol_col", meta->general->bands);
    	band++;
    }
  }
  else if (latConstraint) {
  	if (find_dataset(inDataName, "cell_sigma0_hh_fore")) {
    	read_smap_constraint("cell_sigma0_hh_fore", band, inDataName, outDataName,
		  	latLR, latUL);
    	appendBand(band, "HH_fore", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cell_lat")) {
	    read_smap_constraint("cell_lat", band, inDataName, outDataName,
		  	latLR, latUL);
    	appendBand(band, "lat", meta->general->bands);
    	band++;
		}
		if (find_dataset(inDataName, "cell_lon")) {
	    read_smap_constraint("cell_lon", band, inDataName, outDataName,
	    	latLR, latUL);
    	appendBand(band, "lon", meta->general->bands);
    	band++;
		}
  	int line, sample, height, width;
	  read_smap_bounds(inDataName, latUL, lonUL, latLR, lonLR,
		  &line, &sample, &height, &width);
  	meta->general->line_count = height;
  	meta->general->start_line = line;
  }
  else {
    if (find_dataset(inDataName, "cell_sigma0_hh_fore")) {
    	read_smap_data("cell_sigma0_hh_fore", band, inDataName, outDataName);
    	appendBand(band, "HH_fore", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_sigma0_hh_aft")) {
    	read_smap_data("cell_sigma0_hh_aft", band, inDataName, outDataName);
    	appendBand(band, "HH_aft", meta->general->bands);    
    	band++;
    }
    if (find_dataset(inDataName, "cell_sigma0_xy_fore")) {
    	read_smap_data("cell_sigma0_xy_fore", band, inDataName, outDataName);
    	appendBand(band, "HV_fore", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_sigma0_xy_aft")) {
    	read_smap_data("cell_sigma0_xy_aft", band, inDataName, outDataName);
    	appendBand(band, "HV_aft", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_sigma0_xpol_fore")) {
    	read_smap_data("cell_sigma0_xpol_fore", band, inDataName, outDataName);
    	appendBand(band, "HV_fore", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_sigma0_xpol_aft")) {
    	read_smap_data("cell_sigma0_xpol_aft", band, inDataName, outDataName);
    	appendBand(band, "HV_aft", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_sigma0_vv_fore")) {
    	read_smap_data("cell_sigma0_vv_fore", band, inDataName, outDataName);
    	appendBand(band, "VV_fore", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_sigma0_vv_aft")) {
    	read_smap_data("cell_sigma0_vv_aft", band, inDataName, outDataName);
    	appendBand(band, "VV_aft", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "faraday_rotation_angle")) {
    	read_smap_data("faraday_rotation_angle", band, inDataName, outDataName);
    	appendBand(band, "faraday", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_lat")) {
    	read_smap_data("cell_lat", band, inDataName, outDataName);
    	appendBand(band, "lat", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cell_lon")) {
    	read_smap_data("cell_lon", band, inDataName, outDataName);
    	appendBand(band, "lon", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cylindrical_grid_row_index")) {
    	read_smap_data("cylindrical_grid_row_index", band, inDataName, 
    	outDataName);
    	appendBand(band, "cyl_row", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "cylindrical_grid_column_index")) {
    	read_smap_data("cylindrical_grid_column_index", band, inDataName, 
    	outDataName);
    	appendBand(band, "cyl_col", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "polar_grid_row_index")) {
    	read_smap_data("polar_grid_row_index", band, inDataName, outDataName);
    	appendBand(band, "pol_row", meta->general->bands);
    	band++;
    }
    if (find_dataset(inDataName, "polar_grid_column_index")) {
    	read_smap_data("polar_grid_column_index", band, inDataName, outDataName);
    	appendBand(band, "pol_col", meta->general->bands);
    	band++;
    }
  }
  meta->general->band_count = band;
  meta_write(meta, outDataName);
  meta_free(meta);
	update_geolocation(inDataName, outDataName);

  // Clean up
  FREE(smap);
  FREE(inDataName);
  FREE(outDataName);
  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Gclose(group);
  H5Fclose(file);
}
