#include "asf_view.h"
#include "terrasar.h"
#include "xml_util.h"
#include "asf_import.h"
#include "asf_endian.h"

typedef struct {
  FILE *fp;
  int multilook;
  int asfv, aslv, rsfv, rslv;
  int header;
  int width;
} ReadTerrasarClientInfo;

static int is_valid_meta_terrasar_ext(const char *ext)
{
    return ext && strcmp_case(ext, ".xml") == 0;
}

int try_terrasar(const char *filename)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return is_valid_meta_terrasar_ext(ext);
    }
    else {
        return FALSE;
    }
}

int handle_terrasar_file(const char *filename, char *meta_name, char *data_name,
			 char **err)
{
  // allocate error buffer, just in case we need it
  *err = (char *) MALLOC(sizeof(char)*1024);
  
  // check for metadata
  if (!fileExists(filename))
    meta_name = appendExt(filename, ".xml");
  else
    strcpy(meta_name, filename);
  if (!fileExists(meta_name)) {
    sprintf(*err, "Error opening TerraSAR-X file, metadata file (%s) does "
	    "not exist!\n", meta_name);
    return FALSE;
  }
  xmlDoc *doc = xmlReadFile(meta_name, NULL, 0);
  if (!doc) {
    sprintf(*err, "Could not parse file %s\n", meta_name);
    return FALSE;
  }
  
  // path from the xml (metadata) file
  char *path = get_dirname(filename);
  if (strlen(path)>0) {
    strcpy(data_name, path);
    if (data_name[strlen(data_name)-1] != '/')
      strcat(data_name, "/");
  }
  else
    strcpy(data_name, "");
  free(path);
  
  // strcat() on the path & file from the XML entry
  strcat(data_name, xml_get_string_value(doc, 
    "level1Product.productComponents.imageData[0].file.location.path"));
  strcat(data_name, "/");
  strcat(data_name, xml_get_string_value(doc, 
    "level1Product.productComponents.imageData[0].file.location.filename"));
  if (!fileExists(data_name)) {
    sprintf(*err, "Data file (%s) does not exist!\n", data_name);
    return FALSE;
  }

  // check data type
  terrasar_meta *terrasar = read_terrasar_meta(meta_name);
  if (!strcmp_case(terrasar->imageDataType, "COMPLEX") == 0 &&
      !strcmp_case(terrasar->imageDataFormat, "COSAR") == 0) {
    sprintf(*err, "Data type (%s) and data format (%s) currently not "
	    "supported!\n", 
	    terrasar->imageDataType, terrasar->imageDataFormat);
    return FALSE;
  }

  FREE(*err);
  err = NULL;
  return TRUE;
}

int read_terrasar_client(int row_start, int n_rows_to_get,
			 void *dest_void, void *read_client_info,
			 meta_parameters *meta, int data_type)
{
  ReadTerrasarClientInfo *info = (ReadTerrasarClientInfo*) read_client_info;
  float *dest = (float*)dest_void;
  int ii, jj, ns = meta->general->sample_count;
  int skip = 1;
  if (info->multilook) {
    skip = meta->sar->look_count;
    row_start *= skip;
  }
  // Read in the image
  short int *shorts = MALLOC(sizeof(short int)*ns*2);
  for (ii=0; ii<n_rows_to_get; ii++) {
    long long offset = 
      (long long) (info->header + (ii*skip + row_start) * info->width);
    FSEEK(info->fp, offset, SEEK_SET);
    FREAD(shorts, sizeof(short int), ns*2, info->fp);
    for (jj=0; jj<ns; jj++) 
      dest[jj + ii*ns] = hypot(shorts[jj*2], shorts[jj*2+1]);
  }
  free(shorts);

  return TRUE;
}

int get_terrasar_thumbnail_data(int thumb_size_x, int thumb_size_y,
                              meta_parameters *meta, void *read_client_info,
                              void *dest_void, int data_type)
{
  ReadTerrasarClientInfo *info = (ReadTerrasarClientInfo*) read_client_info;
  
  int ii, jj;
  int ns = meta->general->sample_count;
  int sf = meta->general->line_count / thumb_size_y;
  
  if (info->multilook)
    sf *= meta->sar->look_count;
  
  float *dest = (float*)dest_void;

  // Read in the image
  short int *shorts = MALLOC(sizeof(short int)*ns*2);
  for (ii=0; ii<thumb_size_y; ii++) {
    int line = ii*sf;
    long long offset = (long long) (info->header + line*info->width);
    FSEEK(info->fp, offset, SEEK_SET);
    FREAD(shorts, sizeof(short int), ns*2, info->fp);
    for (jj=0; jj<thumb_size_x; jj++)
      dest[jj + ii*thumb_size_x] = hypot(shorts[jj*sf*2], shorts[jj*sf*2+1]);
    asfPercentMeter((float)ii/(thumb_size_y - 1));
  }
  free(shorts);
  
  return TRUE;
}

void free_terrasar_client_info(void *read_client_info)
{
    ReadTerrasarClientInfo *info = (ReadTerrasarClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

meta_parameters *open_terrasar(const char *data_name, const char *meta_name,
			       const char *band, int multilook,
			       ClientInterface *client)
{
  ReadTerrasarClientInfo *info = MALLOC(sizeof(ReadTerrasarClientInfo));
  
  terrasar_meta *terrasar = read_terrasar_meta(meta_name);
  meta_parameters *meta = terrasar2meta(terrasar);
  if (!meta)
    return NULL;
  
  client->read_client_info = info;
  client->read_fn = read_terrasar_client;
  client->thumb_fn = get_terrasar_thumbnail_data;
  client->free_fn = free_terrasar_client_info;
  client->data_type = GREYSCALE_FLOAT;
  
  info->fp = fopen(data_name, "rb");
  if (!info->fp) {
    asfPrintWarning("Failed to open Terrasar file %s: %s\n",
		    data_name, strerror(errno));
    return FALSE;
  }

  FILE *fpIn = info->fp;
  
  unsigned char intValue[4];
  int asfv, aslv, rsfv, rslv;
  FREAD(&intValue, 1, 4, fpIn);
  //int bytes_in_burst = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  //int range_sample_relative_index = bigInt32(intValue);;
  FREAD(&intValue, 1, 4, fpIn);
  int range_samples = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int azimuth_samples = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  //int burst_index = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int rangeline_total_number_bytes = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  int total_number_lines = bigInt32(intValue);
  FREAD(&intValue, 1, 4, fpIn);
  
  // Check for the first and last azimuth and range pixel
  int kk;
  asfv = 1; // azimuth sample first valid
  FSEEK(fpIn, 2*rangeline_total_number_bytes+8, SEEK_SET);
  for (kk=2; kk<range_samples; kk++) {
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) > asfv)
      asfv = bigInt32(intValue);
  }
  
  aslv = azimuth_samples; // azimuth sample last valid
  FSEEK(fpIn, 3*rangeline_total_number_bytes+8, SEEK_SET);
  for (kk=2; kk<range_samples; kk++) {
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) < aslv)
      aslv = bigInt32(intValue);
  }
  
  rsfv = 1; // range sample first valid
  rslv = range_samples; // range sample last valid
  for (kk=4; kk<total_number_lines; kk++) {
    FSEEK(fpIn, 4*rangeline_total_number_bytes, SEEK_SET);
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) > rsfv)
      rsfv = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    if (bigInt32(intValue) < rslv)
      rslv = bigInt32(intValue);
  }
 
  info->aslv = aslv;
  info->asfv = asfv;
  info->rslv = rslv;
  info->rsfv = rsfv;
  info->header = (asfv+2)*rangeline_total_number_bytes + 8;
  info->width = rangeline_total_number_bytes;
  info->multilook = multilook;
 
  meta->general->line_count = aslv - asfv + 1;
  meta->general->sample_count = rslv - rsfv + 1;

  return meta;
}
