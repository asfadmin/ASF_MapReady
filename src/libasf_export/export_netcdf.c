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
#include <netcdf.h>
#include <xml_util.h>

#define RES 16
#define MAX_PTS 256

static xmlNode *findNode(xmlNode *node, char *name)
{
  xmlNode *cur = node->xmlChildrenNode;
  while (cur != NULL) {
    if (!xmlStrcmp(cur->name, (const xmlChar *)name))
      return cur;
    cur = cur->next;
  }
  return NULL;
}

static xmlNode *findXmlPtr(xmlDoc *doc, char *str)
{
  int ii, n;
  char **array;
  xmlNode *cur, *next;
  split_into_array(str, '.', &n, &array);
  cur = xmlDocGetRootElement(doc);
  for (ii=1; ii<n; ii++) {
    next = findNode(cur, array[ii]);
    cur = next;
  }
  free_char_array(&array, n);
  return cur;
}

static void xml_get_children(xmlNode *node, char **children)
{
  int ii=0;
  xmlNode *cur = node->xmlChildrenNode;
  for (cur = node; cur; cur = cur->next) {
    if (cur->type == XML_ELEMENT_NODE) {
      sprintf(children[ii], "%s", (char *)cur->name);
      ii++;
    }
  }
}

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
  char *str_value = (char *) MALLOC(sizeof(char)*(1+strlen(value)));
  strcpy(str_value, value);
  char *str = (char *) MALLOC(sizeof(char)*1024);
  nc_def_var(group_id, name, NC_STRING, 0, 0, &var_id);
  nc_put_att_text(group_id, var_id, "long_name", strlen(desc), desc);
  if (units && strlen(units) > 0) {
    strcpy(str, units);
    nc_put_att_text(group_id, var_id, "units", strlen(str), str);
  }
  nc_put_var_string(group_id, var_id, (const char **)&str_value);
}

static void add_var_attr(xmlDoc *doc, int ncid, int var_id, char *xml_id)
{
  int ii;
  int meta_count = xml_get_children_count(doc, xml_id);
  char **meta_param = (char **) MALLOC(sizeof(char *)*meta_count);
  for (ii=0; ii<meta_count; ii++)
    meta_param[ii] = (char *) MALLOC(sizeof(char)*50);
  xmlNodePtr node = findXmlPtr(doc, xml_id);
  xml_get_children(node->children, meta_param);
  char type[10], xmlStr[512], str[512];
  int nValue;
  double fValue;
  for (ii=0; ii<meta_count; ii++) {
    sprintf(xmlStr, "%s.%s.type", xml_id, meta_param[ii]);
    strcpy(type, xml_get_string_attribute(doc, xmlStr));
    sprintf(xmlStr, "%s.%s", xml_id, meta_param[ii]);
    if (strcmp_case(type, "STRING") == 0) {
      strcpy(str, xml_get_string_value(doc, xmlStr));
      nc_put_att_text(ncid, var_id, meta_param[ii], strlen(str), str);
    }
    else if (strcmp_case(type, "INTEGER") == 0) {
      nValue = xml_get_int_value(doc, xmlStr);
      nc_put_att_int(ncid, var_id, meta_param[ii], NC_INT, 1, &nValue);
    }
    else if (strcmp_case(type, "DOUBLE") == 0) {
      fValue = xml_get_double_value(doc, xmlStr);
      nc_put_att_double(ncid, var_id, meta_param[ii], NC_DOUBLE, 1, &fValue);
    }
  }
}

void export_netcdf(const char *in_base_name, char *output_file_name,
  int *noutputs, char ***output_names)
{
  int hh, ii, jj, kk;
  char image_file_name[1024], data_file_name[1024], xmlStr[512];
  
  // Check out the general setup
  xmlDoc *doc = xmlReadFile(in_base_name, NULL, 0);
  int variable_count = xml_get_children_count(doc, "hdf5.data");
  if (variable_count == 0)
    asfPrintError("Could not find any data entries!\n");
    
  // Extract names of data sets and get metadata
  char **data_set = (char **) MALLOC(sizeof(char *)*variable_count);
  for (ii=0; ii<variable_count; ii++)
    data_set[ii] = (char *) MALLOC(sizeof(char)*50);
  xmlNode *node = findXmlPtr(doc, "hdf5.data");
  xml_get_children(node->children, data_set);
  
  int found_lat = FALSE, found_lon = FALSE;
  for (ii=0; ii<variable_count; ii++) {
    if (strcmp_case(data_set[ii], "LATITUDE") == 0)
      found_lat = TRUE;
    if (strcmp_case(data_set[ii], "LONGITUDE") == 0)
      found_lon = TRUE;
  }
  if (!found_lat)
    asfPrintError("Could not find latitude information!\n");
  if (!found_lon)
    asfPrintError("Could not find longitude information!\n");

  sprintf(xmlStr, "hdf5.data.%s", data_set[0]);
  strcpy(image_file_name, xml_get_string_value(doc, xmlStr));
  char *p = strstr(image_file_name, ":");
  if (p)
    *p = '\0';
  strcpy(data_file_name, image_file_name);
  meta_parameters *meta = meta_read(image_file_name);

  // Assign parameters
  int var_count = variable_count + 1;
  int projected = FALSE;
  if (meta->projection && meta->projection->type != SCANSAR_PROJECTION) {
    projected = TRUE;
    var_count += 3;
  }
  size_t line_count = meta->general->line_count;
  size_t sample_count = meta->general->sample_count;
  int band_count = meta->general->band_count;
  
  // Assign data type
  nc_type datatype;
  if (meta->general->data_type == ASF_BYTE)
    datatype = NC_CHAR;
  else if (meta->general->data_type == REAL32)
    datatype = NC_FLOAT;

  // Initialize the netCDF pointer structure
  netcdf_t *netcdf = (netcdf_t *) MALLOC(sizeof(netcdf_t));
  netcdf->var_count = var_count;
  netcdf->var_id = (int *) MALLOC(sizeof(int)*var_count);

  // Create output file
  int ncid;
  append_ext_if_needed(output_file_name, ".nc", NULL);
  int status = nc_create(output_file_name, NC_CLOBBER|NC_NETCDF4, &ncid);
  netcdf->ncid = ncid;
  if (status != NC_NOERR)
    asfPrintError("Could not open netCDF file (%s)!\n", nc_strerror(status));
    
  // Define dimensions
  int dim_xgrid_id, dim_ygrid_id, dim_lat_id, dim_lon_id, dim_time_id;
  if (projected) {
    status = nc_def_dim(ncid, "xgrid", sample_count, &dim_xgrid_id);
    if (status != NC_NOERR)
      asfPrintError("Problem with xgrid variable definition!\n");
    status = nc_def_dim(ncid, "ygrid", line_count, &dim_ygrid_id);
    if (status != NC_NOERR)
      asfPrintError("Problem with ygrid variable definition!\n");
  }
  status = nc_def_dim(ncid, "longitude", sample_count, &dim_lon_id);
  if (status != NC_NOERR)
    asfPrintError("Problem with longitude variable definition!\n");
  status = nc_def_dim(ncid, "latitude", line_count, &dim_lat_id);
  if (status != NC_NOERR)
    asfPrintError("Problem with latitude variable definition!\n");
  // FIXME - currently only covers single time
  status = nc_def_dim(ncid, "time", 1, &dim_time_id);
  if (status != NC_NOERR)
    asfPrintError("Problem with time variable definition!\n");
    
  int dims_bands[3];
  dims_bands[2] = dim_time_id;
  if (projected) {
    dims_bands[0] = dim_ygrid_id;
    dims_bands[1] = dim_xgrid_id;
  }
  else {
    dims_bands[0] = dim_lon_id;
    dims_bands[1] = dim_lat_id;
  }

  // Define projection
  double fValue;
  int nn = 0, var_id;
  char str[512];
  int meta_count = xml_get_children_count(doc, "hdf5.projection");
  if (projected && meta_count > 0) {
    nc_def_var(ncid, "projection", NC_INT, 0, 0, &var_id);
    netcdf->var_id[nn] = var_id;
    char **proj_param = (char **) MALLOC(sizeof(char *)*meta_count);
    for (ii=0; ii<meta_count; ii++)
      proj_param[ii] = (char *) MALLOC(sizeof(char)*50);
    node = findXmlPtr(doc, "hdf5.projection");
    xml_get_children(node->children, proj_param);
    for (ii=0; ii<meta_count; ii++) {
      sprintf(xmlStr, "hdf5.projection.%s", proj_param[ii]);
      if (strcmp_case(proj_param[ii], "false_easting") == 0 ||
        strcmp_case(proj_param[ii], "false_northing") == 0 ||
        strcmp_case(proj_param[ii], "latitude_of_projection_origin") == 0 ||
        strcmp_case(proj_param[ii], "longitude_of_central_meridian") == 0 ||
        strcmp_case(proj_param[ii], "longitude_of_projection_origin") == 0 ||
        strcmp_case(proj_param[ii], "scale_factor_at_central_meridian") == 0 ||
        strcmp_case(proj_param[ii], "semi_major_axis") == 0 ||
        strcmp_case(proj_param[ii], "semi_minor_axis") == 0 ||
        strcmp_case(proj_param[ii], "standard_parallel") == 0 ||
        strcmp_case(proj_param[ii], "standard_parallel_1") == 0 ||
        strcmp_case(proj_param[ii], "standard_parallel_2") == 0 ||
        strcmp_case(proj_param[ii], 
          "straight_vertical_longitude_from_pole") == 0) {
        fValue = xml_get_double_value(doc, xmlStr);
        nc_put_att_double(ncid, var_id, proj_param[ii], NC_DOUBLE, 1, &fValue);
      }
      else if (strcmp_case(proj_param[ii], "grid_mapping_name") == 0) {
        strcpy(str, xml_get_string_value(doc, xmlStr));
        nc_put_att_text(ncid, var_id, proj_param[ii], strlen(str), str);
      }
      else
        asfPrintError("Undefined projection parameter (%s)!\n", proj_param[ii]);
      FREE(proj_param[ii]);
    }
    FREE(proj_param);
  }

  if (projected) {

    // Define ygrid
    nn++;
    int dims_ygrid[2] = { dim_ygrid_id, dim_xgrid_id };
    nc_def_var(ncid, "ygrid", NC_FLOAT, 2, dims_ygrid, &var_id);
    netcdf->var_id[nn] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);
    add_var_attr(doc, ncid, var_id, "hdf5.metadata.ygrid");
    
    // Define xgrid
    nn++;
    int dims_xgrid[2] = { dim_ygrid_id, dim_xgrid_id };
    nc_def_var(ncid, "xgrid", NC_FLOAT, 2, dims_xgrid, &var_id);
    netcdf->var_id[nn] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);
    add_var_attr(doc, ncid, var_id, "hdf5.metadata.xgrid");
  }

  // Define longitude
  nn++;
  if (projected) {
    int dims_lon[2] = { dim_ygrid_id, dim_xgrid_id };
    nc_def_var(ncid, "longitude", NC_FLOAT, 2, dims_lon, &var_id);
  }
  else {
    int dims_lon[2] = { dim_lon_id, dim_lat_id };
    nc_def_var(ncid, "longitude", NC_FLOAT, 2, dims_lon, &var_id);
  }
  netcdf->var_id[nn] = var_id;
  nc_def_var_deflate(ncid, var_id, 0, 1, 6);
  add_var_attr(doc, ncid, var_id, "hdf5.metadata.lon");
  
  // Define latitude
  nn++;
  if (projected) {
    int dims_lat[2] = { dim_ygrid_id, dim_xgrid_id };
    nc_def_var(ncid, "latitude", NC_FLOAT, 2, dims_lat, &var_id);
  }
  else {
    int dims_lat[2] = { dim_lat_id, dim_lon_id };
    nc_def_var(ncid, "latitude", NC_FLOAT, 2, dims_lat, &var_id);
  }
  netcdf->var_id[nn] = var_id;
  nc_def_var_deflate(ncid, var_id, 0, 1, 6);
  add_var_attr(doc, ncid, var_id, "hdf5.metadata.lat");

  // Define time
  nn++;
  int dims_time[1] = { dim_time_id };
  nc_def_var(ncid, "time", NC_FLOAT, 1, dims_time, &var_id);
  netcdf->var_id[nn] = var_id;
  add_var_attr(doc, ncid, var_id, "hdf5.metadata.time");

  // Define image variables
  char band[25];
  for (ii=0; ii<variable_count; ii++) {
  
    // Skip lat/lon layers
    if (strcmp_case(data_set[ii], "LATITUDE") == 0 ||
      strcmp_case(data_set[ii], "LONGITUDE") == 0)
      continue;
      
    // Take care of all other layers
    nn++;
    nc_def_var(ncid, data_set[ii], datatype, 3, dims_bands, &var_id);
    netcdf->var_id[nn] = var_id;
    nc_def_var_deflate(ncid, var_id, 0, 1, 6);
    sprintf(xmlStr, "hdf5.data.%s", data_set[ii]);
    strcpy(image_file_name, xml_get_string_value(doc, xmlStr));

    // check whether metadata is stored by band name or not
    char *p = strstr(image_file_name, ":");
    if (p)
      sprintf(band, "hdf5.metadata.%s", p+1);
    else
      sprintf(band, "hdf5.metadata.%s", data_set[ii]);
    add_var_attr(doc, ncid, var_id, band);
  }
    
  // Define global attributes
  meta_count = xml_get_children_count(doc, "hdf5.root");
  if (meta_count == 0)
    asfPrintError("No metadata for the root level (hdf5.root) defined!\n");
  strcpy(str, xml_get_string_value(doc, "hdf5.root.conventions"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "Conventions", strlen(str), str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.institution"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "institution", strlen(str), str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.title"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "title", strlen(str), str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.source"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "source", strlen(str), str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.original_file"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "original_file", strlen(str), str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.comment"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "comment", strlen(str), str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.reference"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "reference", strlen(str), str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.history"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    nc_put_att_text(ncid, NC_GLOBAL, "history", strlen(str), str);

  // Finish off definition block
  nc_enddef(ncid); 

  // Writing data - extra layers first
  nn = 0;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  long pixel_count = line_count*sample_count;

  if (projected) {
  
    // Extra bands - ygrid
    nn++;
    float *ygrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<line_count; ii++) {
      for (kk=0; kk<sample_count; kk++)
	      ygrids[ii*ns+kk] = 
	        meta->projection->startY + kk*meta->projection->perY;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'ygrid' ...\n");
    nc_put_var_float(ncid, netcdf->var_id[nn], &ygrids[0]);
    FREE(ygrids);
    
    // Extra bands - xgrid
    nn++;
    float *xgrids = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ii=0; ii<line_count; ii++) {
      for (kk=0; kk<sample_count; kk++) 
	      xgrids[ii*ns+kk] = 
	        meta->projection->startX + kk*meta->projection->perX;
      asfLineMeter(ii, nl);
    }
    asfPrintStatus("Storing band 'xgrid' ...\n");
    nc_put_var_float(ncid, netcdf->var_id[nn], &xgrids[0]);
    FREE(xgrids);
  }

  // Extra bands - longitude
  double line, sample, lat, lon, first_value;
  double *value, *l, *s;
  quadratic_2d q;
  nn++;
  float *lons = NULL;
  if (meta->latlon)
    lons = meta->latlon->lon;
  else {
    value = (double *) MALLOC(sizeof(double)*MAX_PTS);
    l = (double *) MALLOC(sizeof(double)*MAX_PTS);
    s = (double *) MALLOC(sizeof(double)*MAX_PTS);
    lons = (float *) MALLOC(sizeof(float)*pixel_count);
    asfPrintStatus("Generating band 'longitude' ...\n");
    meta_get_latLon(meta, 0, 0, 0.0, &lat, &lon);

    if (lon < 0.0)
      first_value = lon + 360.0;
    else
      first_value = lon;
    asfPrintStatus("Calculating grid for quadratic fit ...\n");
    for (ii=0; ii<RES; ii++) {
      for (kk=0; kk<RES; kk++) {
        line = ii * nl / RES;
        sample = kk * ns / RES;
        meta_get_latLon(meta, line, sample, 0.0, &lat, &lon);
        l[ii*RES+kk] = line;
        s[ii*RES+kk] = sample;
        if (lon < 0.0)
          value[ii*RES+kk] = lon + 360.0;
        else
          value[ii*RES+kk] = lon;
      }
      asfLineMeter(ii, nl);
    }
    q = find_quadratic(value, l, s, MAX_PTS);
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
    FREE(lons);
  }
  asfPrintStatus("Storing band 'longitude' ...\n");
  nc_put_var_float(ncid, netcdf->var_id[nn], &lons[0]);

  // Extra bands - Latitude
  nn++;
  float *lats;
  if (meta->latlon)
    lats = meta->latlon->lat;
  else {
    lats = (float *) MALLOC(sizeof(float)*pixel_count);
    asfPrintStatus("Generating band 'latitude' ...\n");
    meta_get_latLon(meta, 0, 0, 0.0, &lat, &lon);
    first_value = lat + 180.0;
    asfPrintStatus("Calculating grid for quadratic fit ...\n");
    for (ii=0; ii<RES; ii++) {
      for (kk=0; kk<RES; kk++) {
        line = ii * nl / RES;
        sample = kk * ns / RES;
        meta_get_latLon(meta, line, sample, 0.0, &lat, &lon);
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
        if (meta->general->orbit_direction == 'A')
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
    FREE(lats);
  }
  asfPrintStatus("Storing band 'latitude' ...\n");
  nc_put_var_float(ncid, netcdf->var_id[nn], &lats[0]);

  // Extra bands - Time
  nn++;
  float time = (float) seconds_from_str(meta->general->acquisition_date);
  asfPrintStatus("Storing band 'time' ...\n");
  nc_put_var_float(ncid, netcdf->var_id[nn], &time);

  // Writing image bands
  float *nc = (float *) MALLOC(sizeof(float)*pixel_count);
  float *float_line = (float *) MALLOC(sizeof(float)*sample_count);
  FILE *fp = FOPEN(data_file_name, "rb");
  char **band_name = extract_band_names(meta->general->bands, band_count);
  int channel;
  for (kk=0; kk<band_count; kk++) {
    for (jj=0; jj<variable_count; jj++) {
      if (strcmp_case(data_set[jj], "LATITUDE") == 0 ||
        strcmp_case(data_set[jj], "LONGITUDE") == 0)
        continue;
      sprintf(xmlStr, "hdf5.data.%s", data_set[jj]);
      strcpy(image_file_name, xml_get_string_value(doc, xmlStr));
      char *p = strstr(image_file_name, ":");
      if (strcmp_case(band_name[kk], p+1) == 0) {
        nn++;
        channel = get_band_number(meta->general->bands, band_count, p+1);
        for (ii=0; ii<line_count; ii++) {
          get_band_float_line(fp, meta, channel, ii, float_line);
          for (hh=0; hh<sample_count; hh++)
            nc[ii*sample_count+hh] = float_line[hh];
          asfLineMeter(ii, line_count);
        }
        asfPrintStatus("Storing band '%s' ...\n", band_name[kk]);
        nc_put_var_float(ncid, netcdf->var_id[nn], &nc[0]);
      }
    }
  }
  
  FCLOSE(fp);
  
  // Close file and clean up
  status = nc_close(ncid);
  if (status != NC_NOERR)
    asfPrintError("Could not close netCDF file (%s).\n", nc_strerror(status));
  FREE(netcdf->var_id);
  FREE(netcdf);
  FREE(nc);
  FREE(float_line);
  meta_free(meta);
  xmlFreeDoc(doc);
  
  *noutputs = 1;
  char **outs = MALLOC(sizeof(char *));
  outs[0] = STRDUP(output_file_name);
  *output_names = outs;
}