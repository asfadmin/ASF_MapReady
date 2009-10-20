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
      if (strcmp_case(ext, ".hdr") == 0)
        return TRUE;
      else if (strcmp_case(ext, ".bin") == 0) {
        // check for a corresponding .bin.hdr
        char *hdr = appendExt(filename, ".bin.hdr");
        int ret = fileExists(hdr);
        free(hdr);
        if (!ret) {
          // try just plain .hdr
          hdr = appendExt(filename, ".hdr");
          ret = fileExists(hdr);
          free(hdr);
        }
        return ret;
      }
    }
    else if (try_extensions) {
      int ret = try_ext(filename, ".bin");
      if (!ret)
        ret = try_ext(filename, ".hdr");
      return ret;
    }
    return FALSE;
}

int handle_envi_file(const char *filename, char *meta_name, 
			  char *data_name, char **err)
{
  // six cases:
  // case (1) user gave us the basename 'base', and we need to find
  //          "base.hdr" and "base.bin"
  // case (2) user have us the basename 'base', and we need to find
  //          "base.bin.hdr" and "base.bin"
  // case (3) user gave us 'base.bin' and we need to find 'base.hdr'
  // case (4) user gave us 'base.bin' and we need to find 'base.bin.hdr'
  // case (5) user gave us 'base.hdr' and we need to find 'base.bin'
  // case (6) user gave us 'base.bin.hdr' and we need to find 'base.bin'

    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int l = sizeof(char)*strlen(filename)+255;
    int ret = FALSE;

    if (!has_ext) {
      // case (1) or case (2) ??
      // both cases should have "base.bin" though
      char *file = appendExt(filename, ".bin");
      if (!fileExists(file)) {
	*err = MALLOC(l);
	snprintf(*err, l, "Error opening ENVI data file: %s\n", file);
      }
      else {
        // found data file, try to find the .hdr file
        char *file2 = appendExt(filename, ".hdr");
        if (fileExists(file2)) {
          // case (1), success
          strcpy(meta_name, file2);
          ret = TRUE;
        }
        else {
          free(file2);
          file2 = appendExt(filename, ".bin.hdr");
          if (fileExists(file2)) {
            // case(2), success
            strcpy(meta_name, file2);
            ret = TRUE;
          }
          else {
            // neither (1) nor (2), fail.
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening ENVI header file.\n"
                     "Expected '%s.hdr' or '%s.bin.hdr'\n", filename, filename);
          }
        }
        free(file2);
      }
      if (ret)
        strcpy(data_name, file);
      free(file);
    }
    else if (strcmp_case(ext, ".bin")==0) {
      if (!fileExists(filename)) {
        *err = MALLOC(l);
        snprintf(*err, l, "Error opening ENVI data file: %s\n", filename);
      }
      else {
        // case (3) or case (4) ??
        char *file = appendExt(filename, ".hdr");
        if (fileExists(file)) {
          // case (3), success
          strcpy(meta_name, file);
          ret = TRUE;
        }
        else {
          char *file2 = appendExt(filename, ".bin.hdr");
          if (fileExists(file2)) {
            // case (4), success
            strcpy(meta_name, file2);
            ret = TRUE;
          }
          else {
            // neither (3) nor (4), fail.
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening ENVI header file.\n"
                     "Expected '%s' or '%s'\n", file, file2);
          }
          free(file2);
        }
        if (ret)
          strcpy(data_name, filename);
        free(file);
      }
    }
    else if (strcmp_case(ext, ".hdr")==0) {
      if (!fileExists(filename)) {
        *err = MALLOC(l);
        snprintf(*err, l, "Error opening ENVI header file: %s\n", filename);
      }
      else {
        // case (5) or case (6) ??
        char *datafile=NULL;
        if (endsWith(filename, ".bin.hdr")) {
          datafile = stripExt(filename); // strips .hdr, left with .bin
        }
        else {
          datafile = appendExt(filename, ".bin");
        }
        if (fileExists(datafile)) {
          strcpy(meta_name, filename);
          strcpy(data_name, datafile);
          ret = TRUE;
        }
        else {
          char *base = stripExt(filename);
          *err = MALLOC(l);
          snprintf(*err, l, "Error opening ENVI data file.\n"
                   "Expected '%s.bin' or '%s.bin.hdr'\n", base, base);
          free(base);
        }
        if (ret)
          strcpy(meta_name, filename);
      }
    }
    else {
      // I don't think this will actually ever run
      *err = MALLOC(l);
      snprintf(*err, l, "Error opening ENVI file: %s\n", filename);
      ret = FALSE;
    }

    return ret;
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

meta_parameters* open_envi(const char *meta_name,
                           const char *data_name,
                           const char *band_str,
                           ClientInterface *client)
{
  int ii;
  char str[5];
  ReadEnviClientInfo *info = MALLOC(sizeof(ReadEnviClientInfo));
  envi_header *hdr = read_envi((char *)meta_name);
  info->big_endian = hdr->byte_order;
  meta_parameters *meta = envi2meta(hdr);
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
