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
#include <hdf5_hl.h>
#include <xml_util.h>

#define RES 16
#define MAX_PTS 256
#define DIM_WITHOUT_VAR "This is a netCDF dimension but not a netCDF variable."

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

void h5_att_str(hid_t data, char *name, const char *value)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
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
  if (long_name && strlen(long_name) > 0)
    h5_att_str(h5_data, "long_name", long_name);
  if (units && strlen(units) > 0)
    h5_att_str(h5_data, "units", units);
  H5Dclose(h5_data);
  H5Sclose(h5_space);
}

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

static void xml_meta2hdf(xmlDoc *doc, xmlNode *node, char *group, char *xmlStr, 
  hid_t h5)
{
  char str[512], element[50], value[200], type[25], definition[255], units[25];
  xmlNode *cur = NULL;
  cur = node->xmlChildrenNode;
  for (cur = node; cur; cur = cur->next) {
    if (cur->type == XML_ELEMENT_NODE) {
      sprintf(element, "%s", (char *)cur->name);
      sprintf(str, "%s.%s", xmlStr, element);
      sprintf(value, "%s", xml_get_string_value(doc, str));
      sprintf(str, "%s.%s.type", xmlStr, element);
      sprintf(type, "%s", xml_get_string_attribute(doc, str));
      sprintf(str, "%s.%s.definition", xmlStr, element);
      sprintf(definition, "%s", xml_get_string_attribute(doc, str));
      if (strcmp(definition, MAGIC_UNSET_STRING) == 0)
        strcpy(definition, "");
      sprintf(str, "%s.%s.units", xmlStr, element);
      sprintf(units, "%s", xml_get_string_attribute(doc, str));
      if (strcmp(units, MAGIC_UNSET_STRING) == 0)
        strcpy(units, "");

      if (strcmp_case(type, "STRING") == 0)
        h5_value_str(h5, group, element, value, definition, units);
      else if (strcmp_case(type, "INT") == 0)
        h5_value_int(h5, group, element, atoi(value), definition, units);
      else if (strcmp_case(type, "DOUBLE") == 0)
        h5_value_double(h5, group, element, atof(value), definition, units);
      else {
        asfPrintWarning("Unknown type (%s) for element (%s)!\nWriting element "
          "as string!\n", type, element);
        h5_value_str(h5, group, element, value, definition, units);
      }
    }
  }
}

static void xml_data2hdf(xmlDoc *doc, char *dataFile, char *group, 
  char *dataXml, h5_t *h5)
{
  char str[512], imgFile[512], metaFile[512], dataset[512];
  sprintf(str, "%s.%s", dataXml, dataFile);
  sprintf(imgFile, "%s.img", xml_get_string_value(doc, str));
  sprintf(metaFile, "%s.meta", xml_get_string_value(doc, str));
  sprintf(dataset, "%s/%s", group, dataFile);
  int found = TRUE;
  if (!fileExists(imgFile)) {
    asfPrintWarning("Image file (%s) does not exist! Skipping export to HDF5!"
      "\n", imgFile);
    found = FALSE;
  }
  if (!fileExists(metaFile)) {
    asfPrintWarning("Metadata file (%s) does not exist! Skipping export to "
      "HDF5!\n", metaFile);
    found = FALSE;
  }
  if (found) {
    meta_parameters *meta = meta_read(metaFile);
    int lines = meta->general->line_count;
    int samples = meta->general->sample_count;
    float *hdf = (float *) MALLOC(sizeof(float)*lines*samples);
    FILE *fp = FOPEN(imgFile, "rb");
    get_float_lines(fp, meta, 0, lines, hdf);
    FCLOSE(fp);
    
    // Write the data to the HDF5 file
    hsize_t dims[2] = { lines, samples };
    hsize_t cdims[2] = { 100, samples };
    hid_t h5_array = H5Screate_simple(2, dims, NULL);
    h5->space = h5_array;
    hid_t h5_plist = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_chunk(h5_plist, 2, cdims);
    H5Pset_deflate(h5_plist, 6);
    hid_t h5_data = H5Dcreate(h5->file, dataset, H5T_NATIVE_FLOAT, h5_array,
      H5P_DEFAULT, h5_plist, H5P_DEFAULT);
    H5Dwrite(h5_data, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, hdf);
    H5Dclose(h5_data);
    FREE(hdf);
    meta_free(meta);
  }
}

static void xml_datameta2hdf(xmlDoc *doc, xmlNode *node, char *group, 
  char *metaXml, char *parameter, hid_t h5)
{
  char str[512], element[512], value[200], units[25], valueStr[512];
  xmlNode *cur = NULL;
  cur = node->xmlChildrenNode;
  for (cur = node; cur; cur = cur->next) {
    if (cur->type == XML_ELEMENT_NODE) {
      sprintf(element, "%s", (char *)cur->name);
      sprintf(str, "%s.%s", metaXml, element);
      sprintf(value, "%s", xml_get_string_value(doc, str));
      sprintf(str, "%s.%s.units", metaXml, element);
      sprintf(units, "%s", xml_get_string_attribute(doc, str));
      if (strcmp(units, MAGIC_UNSET_STRING) == 0)
        sprintf(valueStr, "%s", value);
      else
        sprintf(valueStr, "%s [%s]", value, units);
      hid_t h5_data = H5Dopen(h5, group, H5P_DEFAULT);
      h5_att_str(h5_data, element, valueStr);
      H5Dclose(h5_data);
    }
  }
}

void export_hdf(const char *in_base_name, char *output_file_name,
								int *noutputs,char ***output_names)
{
  int ii, kk, dataCount, metaCount;
  char group[512], granule[100], xmlStr[512], datagroup[512];
  char **data_sets=NULL, **meta_params=NULL;

  // Read XML file
  xmlDoc *doc = xmlReadFile(in_base_name, NULL, 0);

  // Generate HDF5 file
  append_ext_if_needed(output_file_name, ".h5", NULL);
  h5_t *h5 = (h5_t *) MALLOC(sizeof(h5_t));
  hid_t h5_file = 
    H5Fcreate(output_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  h5->file = h5_file;
  
  // Create a granule group
  strcpy(granule, xml_get_string_value(doc, "hdf5.granule"));
  if (strcmp_case(granule, MAGIC_UNSET_STRING) == 0)
    asfPrintError("Granule parameter (hdf5.granule) needs to be defined!\n");
  sprintf(group, "/%s", granule);
  hid_t h5_granule = H5Gcreate(h5_file, group, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);
  
  // Work through the metadata
  sprintf(group, "/%s/metadata", granule);
  hid_t h5_metagroup = H5Gcreate(h5_granule, group, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);
  metaCount = xml_get_children_count(doc, "hdf5.metadata");
  if (metaCount == 0)
    asfPrintError("Metadata section (hdf.metadata) does not exist or does not "
      "have any children!\n");
  meta_params = (char **) MALLOC(sizeof(char *)*metaCount);
  for (ii=0; ii<metaCount; ii++)
    meta_params[ii] = (char *) MALLOC(sizeof(char)*50);
  xmlNode *node = findXmlPtr(doc, "hdf5.metadata");
  xml_get_children(node->children, meta_params);
  for (ii=0; ii<metaCount; ii++) {
    sprintf(xmlStr, "hdf5.metadata.%s", meta_params[ii]);
    node = findXmlPtr(doc, xmlStr);
    sprintf(group, "/%s/metadata/%s", granule, meta_params[ii]);
    hid_t h5_product = 
      H5Gcreate(h5_metagroup, group, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    xml_meta2hdf(doc, node->children, group, xmlStr, h5_file);
    H5Gclose(h5_product);
  }
  for (ii=0; ii<metaCount; ii++)
    FREE(meta_params[ii]);
  FREE(meta_params);
  H5Gclose(h5_metagroup);

  // Add data products
  sprintf(group, "/%s/data", granule);
  hid_t h5_datagroup = H5Gcreate(h5_granule, group, H5P_DEFAULT, H5P_DEFAULT, 
			   H5P_DEFAULT);
  dataCount = xml_get_children_count(doc, "hdf5.data");
  if (dataCount == 0)
    asfPrintError("Data section (hdf.data) does not exist or does not have any "
      "children!\n");
  data_sets = (char **) MALLOC(sizeof(char *)*dataCount);
  for (ii=0; ii<dataCount; ii++)
    data_sets[ii] = (char *) MALLOC(sizeof(char)*50);
  node = findXmlPtr(doc, "hdf5.data");
  xml_get_children(node->children, data_sets);
  for (ii=0; ii<dataCount; ii++) {
    xml_data2hdf(doc, data_sets[ii], group, "hdf5.data", h5);
    sprintf(datagroup, "/%s/data/%s", granule, data_sets[ii]);
    
    metaCount = xml_get_children_count(doc, "hdf5.metadata");
    meta_params = (char **) MALLOC(sizeof(char *)*metaCount);
    for (kk=0; kk<metaCount; kk++)
      meta_params[kk] = (char *) MALLOC(sizeof(char)*50);
    node = findXmlPtr(doc, "hdf5.metadata");
    xml_get_children(node->children, meta_params);
    for (kk=0; kk<metaCount; kk++) {
      sprintf(xmlStr, "hdf5.metadata.%s", meta_params[kk]);
      node = findXmlPtr(doc, xmlStr);
      if (strcmp_case(data_sets[ii], meta_params[kk]) == 0)
        xml_datameta2hdf(doc, node->children, datagroup, xmlStr, meta_params[kk],
          h5_file);
      FREE(meta_params[kk]);
    }
    FREE(meta_params);
    
    FREE(data_sets[ii]);
  }
  FREE(data_sets);
  H5Gclose(h5_datagroup);

  H5Gclose(h5_granule);

  // Adding global attributes
  char *str = (char *) MALLOC(sizeof(char)*1024);
  metaCount = xml_get_children_count(doc, "hdf5.root");
  if (metaCount == 0)
    asfPrintError("No metadata for the root level (hdf5.root) defined!\n");
  hid_t h5_global = H5Gopen(h5_file, "/", H5P_DEFAULT);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.institution"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    h5_att_str(h5_global, "institution", str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.title"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    h5_att_str(h5_global, "title", str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.source"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    h5_att_str(h5_global, "source", str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.original_file"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    h5_att_str(h5_global, "original_file", str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.comment"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    h5_att_str(h5_global, "comment", str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.reference")); 
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    h5_att_str(h5_global, "reference", str);
  strcpy(str, xml_get_string_value(doc, "hdf5.root.history"));
  if (strcmp_case(str, MAGIC_UNSET_STRING) != 0)
    h5_att_str(h5_global, "history", str); 
  H5Gclose(h5_global);
  
  H5Fclose(h5->file);
  FREE(h5);
  FREE(str);
  xmlFreeDoc(doc);

  *noutputs = 1;
  char **outs = MALLOC(sizeof(char*));
  outs[0] = STRDUP(output_file_name);
  *output_names = outs;
}
