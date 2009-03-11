#include "asf_view.h"

typedef struct {
  FILE *fp;
  int is_rgb;
  int band_gs;
  int band_r, band_g, band_b;
  int big_endian;
} ReadEnviClientInfo;

int try_envi(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
      return strcmp_case(ext, ".hdr") == 0;
    } else if (try_extensions) {
      int ret = try_ext(filename, ".hdr");
      if (!ret)
        ret = try_ext(filename, ".hdr");
      return ret;
    }
    return FALSE;
}

int handle_envi_file(const char *filename, char *meta_name, 
			  char *data_name, char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_envi_ext = has_ext && strcmp_case(ext,".hdr")==0;
    char *file;

    if (!has_ext) {
        has_envi_ext = try_ext(filename, ".hdr");
        if (!has_envi_ext) {
            has_envi_ext = try_ext(filename, ".hdr");
            if (has_envi_ext)
                file = appendExt(filename, ".hdr");
        }
        else {
            file = appendExt(filename, ".hdr");
        }
    }
    else
      file = STRDUP(filename);

    if (has_envi_ext)
    {
      char *dirName = (char *) MALLOC(sizeof(char) * 512);
      char *fileName = (char *) MALLOC(sizeof(char) * 512);
      split_dir_and_file(file, dirName, fileName);
      strcpy(meta_name, file);
      envi_header *hdr = read_envi((char *)meta_name);
      meta_parameters *meta = envi2meta(hdr);
      
      sprintf(data_name, "%s%s", dirName, meta->general->basename);
      meta_free(meta);
      if (hdr->band_name)
	FREE(hdr->band_name);
      FREE(hdr);
      
      int ret;
      if (!fileExists(data_name)) {
	// I don't think this will actually ever run
	int l = sizeof(char)*strlen(filename)+255;
	*err = MALLOC(l);
	snprintf(*err, l, "Error opening ENVI file: %s\n", data_name);
	ret = FALSE;
      }
      else
	ret = TRUE;
      
      free(file);
      return ret;
    }
    else {
        int l = sizeof(char)*strlen(filename)+255;
        *err = MALLOC(l);
        snprintf(*err, l, "Failed to open %s as an ENVI File.\n", filename);
        free(file);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

int read_envi_client(int row_start, int n_rows_to_get,
		     void *dest_void, void *read_client_info,
		     meta_parameters *meta, int data_type)
{
  int ii, jj, kk;
  int ns = meta->general->sample_count;
  //int nl = meta->general->line_count;
  float *dest = (float*)dest_void;
  ReadEnviClientInfo *info = (ReadEnviClientInfo*)read_client_info;
  
  assert(info->fp);

  if (data_type == GREYSCALE_FLOAT) {
    get_float_lines(info->fp, meta, row_start, n_rows_to_get, dest);
    if (!info->big_endian)
      for ( ii = row_start; ii < n_rows_to_get; ii++ )
	for (jj = 0 ; jj < ns; jj++ )
	  ieee_big32(dest[ii*ns + jj]);
  }
  else if (data_type == RGB_FLOAT) {
    memset(dest, 0, n_rows_to_get*ns*3);
    float *floats = (float *) MALLOC(sizeof(float)*ns);
    if (info->band_r >= 0) {
      for ( ii = 0; ii < n_rows_to_get; ii++ ) {
	get_band_float_line(info->fp, meta, info->band_r, ii, floats);
	kk = ii*ns*3;
	for ( jj = 0; jj < ns; jj++, kk += 3 ) {
	  dest[kk] = floats[jj];
	  if (!info->big_endian)
	    ieee_big32(dest[kk]);
	}
      }
    }
    if (info->band_g >= 0) {
      for ( ii = 0; ii < n_rows_to_get; ii++ ) {
	get_band_float_line(info->fp, meta, info->band_g, ii, floats);
	kk = ii*ns*3 + 1;
	for ( jj = 0; jj < ns; jj++, kk += 3 ) {
	  dest[kk] = floats[jj];
	  if (!info->big_endian)
	    ieee_big32(dest[kk]);
	}
      }
    }
    if (info->band_b >= 0) {
      for ( ii = 0; ii < n_rows_to_get; ii++ ) {
	get_band_float_line(info->fp, meta, info->band_b, ii, floats);
	kk = ii*ns*3 + 2;
	for ( jj = 0; jj < ns; jj++, kk += 3 ) {
	  dest[kk] = floats[jj];
	  if (!info->big_endian)
	    ieee_big32(dest[kk]);
	}
      }
    }
  }
  
  return TRUE;
}

void free_envi_client_info(void *read_client_info)
{
    ReadEnviClientInfo *info = (ReadEnviClientInfo*)read_client_info;
    if (info->fp) fclose(info->fp); // should never be still open
    free(info);
}

// for brs, combined the "open_meta" and "open_data" functions -- both
// will be opening the same file.
meta_parameters* open_envi(const char *meta_name, const char *band_str,
				ClientInterface *client)
{
  int ii;
  char data_name[1024], str[5];
  ReadEnviClientInfo *info = MALLOC(sizeof(ReadEnviClientInfo));
  envi_header *hdr = read_envi((char *)meta_name);
  info->big_endian = hdr->byte_order;
  meta_parameters *meta = envi2meta(hdr);
  char *dirName = (char *) MALLOC(sizeof(char) * 512);
  char *fileName = (char *) MALLOC(sizeof(char) * 512);
  split_dir_and_file(meta_name, dirName, fileName);
  sprintf(data_name, "%s%s", dirName, meta->general->basename);
  if (hdr->band_name)
    FREE(hdr->band_name);
  FREE(hdr);
  
  info->fp = FOPEN(data_name, "rb");
  strcpy(meta->general->bands, "");
  for (ii=0; ii<meta->general->band_count; ii++) {
    sprintf(str, "%d,", ii+1);
    strcat(meta->general->bands, str);
  }
  meta->general->bands[(int)strlen(meta->general->bands)-1] = '\0';
  int nBands = meta->general->band_count;
  info->band_gs = info->band_r = info->band_g = info->band_b = 0;
  if (band_str && strchr(band_str, ',')) {
    char *r, *g, *b;
    info->is_rgb = TRUE;
    split3(band_str, &r, &g, &b, ',');
    info->band_r = get_band_number(meta->general->bands, nBands, r);
    info->band_g = get_band_number(meta->general->bands, nBands, g);
    info->band_b = get_band_number(meta->general->bands, nBands, b);
    set_bands_rgb(info->band_r, info->band_g, info->band_b);
    client->data_type = RGB_FLOAT;
    FREE(r);
    FREE(g);
    FREE(b);
  }
  else {
    info->is_rgb = FALSE;
    info->band_gs = band_str ?
      get_band_number(meta->general->bands, nBands, (char *)band_str) : 0;
    set_bands_greyscale(info->band_gs);
    client->data_type = GREYSCALE_FLOAT;
  }  
  client->read_client_info = info;
  client->read_fn = read_envi_client;
  client->thumb_fn = NULL;
  client->free_fn = free_envi_client_info;
  
  return meta;
}
