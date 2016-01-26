#include <asf_meta.h>
#include <netcdf.h>
#include <xml_util.h>
#include <dateUtil.h>

typedef struct {
  char name[NC_MAX_NAME];
  size_t size;
} dim_t;

typedef struct {
  char name[NC_MAX_NAME];
  nc_type type;
  int nDims;
  int dims[NC_MAX_VAR_DIMS];
  int nAttrs;
} var_t;

typedef struct {
  char name[NC_MAX_NAME];
  nc_type type;
  size_t value_count;
} attr_t;

double *nc_get_double_attribute(int ncid, int var_id, char *name) {
  nc_type type;
  size_t value_count;
  double *value = NULL;
  int status = nc_inq_att(ncid, var_id, name, &type, &value_count);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve attribute information for %s!\n", name);
  if (type == NC_DOUBLE) {
    value = (double *) MALLOC(sizeof(double)*value_count);
    status = nc_get_att_double(ncid, var_id, name, value);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve double attribute value for %s!\n", name);
  }
  else
    asfPrintError("Could not retrieve double value for %s. Wrong data type.\n", name);
    
  return value;
}

float *nc_get_float_attribute(int ncid, int var_id, char *name) {
  nc_type type;
  size_t value_count;
  float *value = NULL;
  int status = nc_inq_att(ncid, var_id, name, &type, &value_count);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve attribute information for %s!\n", name);
  if (type == NC_FLOAT) {
    value = (float *) MALLOC(sizeof(float)*value_count);
    status = nc_get_att_float(ncid, var_id, name, value);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve float attribute value for %s!\n", name);
  }
  else
    asfPrintError("Could not retrieve float value for %s. Wrong data type.\n", name);
    
  return value;
}

int *nc_get_int_attribute(int ncid, int var_id, char *name) {
  nc_type type;
  size_t value_count;
  int *value = NULL;
  int status = nc_inq_att(ncid, var_id, name, &type, &value_count);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve attribute information for %s!\n", name);
  if (type == NC_INT) {
    value = (float *) MALLOC(sizeof(float)*value_count);
    status = nc_get_att_int(ncid, var_id, name, value);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve integer attribute value for %s!\n", name);
  }
  else
    asfPrintError("Could not retrieve integer value for %s. Wrong data type.\n", name);
    
  return value;
}

char *nc_get_string_attribute(int ncid, int var_id, char *name) {
  nc_type type;
  size_t length;
  char *value;
  int status = nc_inq_att(ncid, var_id, name, &type, &length);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve attribute information for %s!\n", name);
  if (type == NC_STRING || type == NC_CHAR) {
    value = (char *) MALLOC(sizeof(char)*255);
    status = nc_get_att_text(ncid, var_id, name, value);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve string attribute value for %s!\n", name);
  }
  else
      asfPrintError("Could not retrieve string value for %s. Wrong data type.\n", name);
      
  return value;
}

double *nc_get_var_double_attribute(int ncid, char *var_name, char *attr_name) {
  int var_id;
  int status = nc_inq_varid(ncid, var_name, &var_id);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve variable ID for variable (%s)!\n", var_name);
  return nc_get_double_attribute(ncid, var_id, attr_name);  
}

float *nc_get_var_float_attribute(int ncid, char *var_name, char *attr_name) {
  int var_id;
  int status = nc_inq_varid(ncid, var_name, &var_id);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve variable ID for variable (%s)!\n", var_name);
  return nc_get_float_attribute(ncid, var_id, attr_name);  
}

int *nc_get_var_int_attribute(int ncid, char *var_name, char *attr_name) {
  int var_id;
  int status = nc_inq_varid(ncid, var_name, &var_id);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve variable ID for variable (%s)!\n", var_name);
  return nc_get_int_attribute(ncid, var_id, attr_name);  
}

char *nc_get_var_string_attribute(int ncid, char *var_name, char *attr_name) {
  int var_id;
  int status = nc_inq_varid(ncid, var_name, &var_id);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve variable ID for variable (%s)!\n", var_name);
  return nc_get_string_attribute(ncid, var_id, attr_name);
}

xmlDoc *netcdf2xml(const char *ncFile)
{
  int ii, kk, ll, n, ncid, nDims, nVars, nAttrs, dims[NC_MAX_VAR_DIMS];
  nc_type type;
  size_t value_count;
  xmlNode *node, *subNode;
  char **arr, name[512], granule[512], absPath[1024];
  split_dir_and_file(ncFile, absPath, granule);
  split_into_array(absPath, '/', &n, &arr);
  snprintf(granule, strlen(arr[n-3])-4, "%s", arr[n-3]);

  // Set up netCDF file
  int status = nc_open(ncFile, NC_NOWRITE, &ncid);
  if (status != NC_NOERR)
    asfPrintError("Could not open netCDF file (%s)!\n", nc_strerror(status));    
  
  // Set up XML tree
  xmlDoc *doc = xmlNewDoc(BAD_CAST "1.0");
  xmlNode *root = xmlNewNode(NULL, BAD_CAST "netcdf");
  xmlDocSetRootElement(doc, root);
  
  // Fill in contents
  char valueStr[512], tmpStr[512], attrStr[512];
  xmlNewChild(root, NULL, BAD_CAST "granule", BAD_CAST granule);
  xmlNewChild(root, NULL, BAD_CAST "metadata_creation", BAD_CAST iso_date());
  
  // Dimensions
  xmlNode *dimension = xmlNewChild(root, NULL, BAD_CAST "dimension", NULL);
  status = nc_inq_ndims (ncid, &nDims);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve number of  dimensions!\n");
  dim_t *dim = (dim_t *) MALLOC(sizeof(dim_t)*nDims);  
  for (ii=0; ii<nDims; ii++) {
    status = nc_inq_dim(ncid, ii, dim[ii].name, &dim[ii].size);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve dimension information!\n");
    sprintf(valueStr, "%d", (int) dim[ii].size);
    xmlNewChild(dimension, NULL, BAD_CAST dim[ii].name, BAD_CAST valueStr);
  }
  
  // Variables
  xmlNode *data = xmlNewChild(root, NULL, BAD_CAST "data", NULL);
  status = nc_inq_nvars(ncid, &nVars);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve number of variables!\n");
  var_t *var = (var_t *) MALLOC(sizeof(var_t)*nVars);
  for (ii=0; ii<nVars; ii++) {
    status = nc_inq_var(ncid, ii, name, &type, &nDims, &dims, &nAttrs);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve variable information!\n");
    node = xmlNewChild(data, NULL, BAD_CAST name, NULL);
    if (type == NC_STRING || type == NC_CHAR)
      strcpy(valueStr, "string");
    else if (type == NC_FLOAT)
      strcpy(valueStr, "float");
    else if (type == NC_DOUBLE)
      strcpy(valueStr, "double");
    else if (type == NC_INT)
      strcpy(valueStr, "integer");
    xmlNewChild(node, NULL, BAD_CAST "data_type", BAD_CAST valueStr);
    sprintf(valueStr, "%d", nDims);
    subNode = xmlNewChild(node, NULL, BAD_CAST "dimensions", BAD_CAST valueStr);
    for (kk=0; kk<nDims; kk++) {
      sprintf(valueStr, "%d", (int) dim[kk].size);
      xmlNewProp(subNode, BAD_CAST dim[kk].name, BAD_CAST valueStr);
    }
    for (kk=0; kk<nAttrs; kk++) {
      strcpy(valueStr, "");
      status = nc_inq_attname(ncid, ii, kk, name);
      if (status != NC_NOERR)
        asfPrintError("Could not retrieve attribute name!\n");
      status = nc_inq_att(ncid, ii, name, &type, &value_count);
      if (status != NC_NOERR)
        asfPrintError("Could not retrieve attribute information!\n");
      if (type == NC_STRING || type == NC_CHAR) {
        sprintf(valueStr, "%s", nc_get_string_attribute(ncid, ii, name));
        strcpy(attrStr, "string");
      }
      else if (type == NC_DOUBLE) {
        double *value = (double *) MALLOC(sizeof(double)*(int)value_count);
        value = nc_get_double_attribute(ncid, ii, name);
        sprintf(valueStr, "%.6f", value[0]);
        for (ll=1; ll<(int)value_count; ll++) {
          sprintf(tmpStr, ",%.6f", value[ll]);
          strcat(valueStr, tmpStr);
        }
        strcpy(attrStr, "double");
      }
      else if (type == NC_FLOAT) {
        float *value = (float *) MALLOC(sizeof(float)*(int)value_count);
        value = nc_get_float_attribute(ncid, ii, name);
        sprintf(valueStr, "%.6f", value[0]);
        for (ll=1; ll<(int)value_count; ll++) {
          sprintf(tmpStr, ",%.6f", value[ll]);
          strcat(valueStr, tmpStr);
        }
        strcpy(attrStr, "float");
      }
      else if (type == NC_INT) {
        int *value = (int *) MALLOC(sizeof(int)*(int)value_count);
        value = nc_get_int_attribute(ncid, ii, name);
        sprintf(valueStr, "%d", value[0]);
        for (ll=1; ll<(int)value_count; ll++) {
          sprintf(tmpStr, ",%d", value[ll]);
          strcat(valueStr, tmpStr);
        }
        strcpy(attrStr, "int");
      }
      if (strcmp_case(name, "_FillValue") == 0)
        strcpy(name, "fill_value");
      else if (strcmp_case(name, "_Unsigned") == 0)
        strcpy(name, "unsigned");
      subNode = xmlNewChild(node, NULL, BAD_CAST name, BAD_CAST valueStr);
      xmlNewProp(subNode, BAD_CAST "data_type", BAD_CAST attrStr);
    }
  }

  // Global attributes
  xmlNode *metadata = xmlNewChild(root, NULL, BAD_CAST "metadata", NULL);
  status = nc_inq_natts(ncid, &nAttrs);
  if (status != NC_NOERR)
    asfPrintError("Could not retrieve number of global attributes");
  for (ii=0; ii<nAttrs; ii++) {
    strcpy(valueStr, "");
    status = nc_inq_attname(ncid, NC_GLOBAL, ii, name);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve attribute name!\n");
    status = nc_inq_att(ncid, NC_GLOBAL, name, &type, &value_count);
    if (status != NC_NOERR)
      asfPrintError("Could not retrieve attribute information!\n");
    if (type == NC_STRING || type == NC_CHAR)
      sprintf(valueStr, "%s", nc_get_string_attribute(ncid, NC_GLOBAL, name));
    else if (type == NC_DOUBLE) {
      double *value = (double *) MALLOC(sizeof(double)*(int)value_count);
      value = nc_get_double_attribute(ncid, NC_GLOBAL, name);
      sprintf(valueStr, "%.6f", value[0]);
      for (kk=1; kk<(int)value_count; kk++) {
        sprintf(tmpStr, ",%.6f", value[kk]);
        strcat(valueStr, tmpStr);
      }
    }
    else if (type == NC_FLOAT) {
      float *value = (float *) MALLOC(sizeof(float)*(int)value_count);
      value = nc_get_float_attribute(ncid, NC_GLOBAL, name);
      sprintf(valueStr, "%.6f", value[0]);
      for (kk=1; kk<(int)value_count; kk++) {
        sprintf(tmpStr, ",%.6f", value[kk]);
        strcat(valueStr, tmpStr);
      }
    }
    else if (type == NC_INT) {
      int *value = (int *) MALLOC(sizeof(int)*(int)value_count);
      value = nc_get_int_attribute(ncid, NC_GLOBAL, name);
      sprintf(valueStr, "%d", value[0]);
      for (kk=1; kk<(int)value_count; kk++) {
        sprintf(tmpStr, ",%d", value[kk]);
        strcat(valueStr, tmpStr);
      }
    }
    xmlNewChild(metadata, NULL, BAD_CAST name, BAD_CAST valueStr);
  }
    
  nc_close(ncid);
  
  return doc;
}

void import_netcdf_xml(const char *ncFile, char *xmlFile)
{
  FILE *fp = FOPEN(xmlFile, "w");
  xmlDoc *doc = netcdf2xml(ncFile);
  xmlDocFormatDump(fp, doc, 1);
  xmlFreeDoc(doc);
  xmlCleanupParser();
  FCLOSE(fp);
}