#include "asf_view.h"
#include "airsar.h"
#include "asf_import.h"

typedef enum {
  AIRSAR_UNKNOWN=0,
  AIRSAR_AMPLITUDE,
  AIRSAR_DEM,
  AIRSAR_COHERENCE,
  AIRSAR_POLARIMETRIC
} airsar_data_type_t;

typedef struct {
    FILE *fp;
    airsar_data_type_t airsar_data_type;
    int is_rgb;
    int band_gs;
    int band_r;
    int band_g;
    int band_b;
    airsar_header *header;
    airsar_dem_header *dem_header;
} ReadAirsarClientInfo;

static int is_valid_data_airsar_ext(const char *ext)
{
    return ext &&
        (strcmp_case(ext, ".vvi2") == 0
         || strcmp_case(ext, ".demi2") == 0
         || strcmp_case(ext, ".datgr") == 0
	 || strcmp_case(ext, ".dat") == 0
         //|| strcmp_case(ext, ".incgr") == 0
         || strcmp_case(ext, ".corgr") == 0
          );
}

static int is_valid_meta_airsar_ext(const char *ext)
{
    return ext && strcmp_case(ext, ".airsar") == 0;
}

static int is_valid_airsar_ext(const char *ext)
{
    return is_valid_data_airsar_ext(ext) || is_valid_meta_airsar_ext(ext);
}

// airsar never tries any extensions -- user must provide full name
// of the data file, so we don't have the "try_extensions" parameters that
// all of the other try_ functions have
int try_airsar(const char *filename)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return is_valid_airsar_ext(ext);
    }
    else {
        return FALSE;
    }
}

int handle_airsar_file(const char *filename, char *meta_name, char *data_name,
                       char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_airsar_ext = has_ext && is_valid_airsar_ext(ext);
    int exists = fileExists(filename);
    int is_data_file = is_valid_data_airsar_ext(ext);
    int is_meta_file = is_valid_meta_airsar_ext(ext);
    int l = sizeof(char)*strlen(filename)*2+255;

    if (has_airsar_ext && is_meta_file && exists)
    {
        // we were given a data file -- but which data file should we show?
        // MapReady gui tries <basename>_c.vvi2, then <basename>_l.vvi2,
        // then gives up
        int ret;

        char *d = MALLOC(sizeof(char)*(strlen(filename)+25));
        strcpy(d, filename);

        char *p = findExt(d);
        assert(p);

        // ensure we have correctly figured out the basename
        if (p-d<=5 || strcmp_case(p-5, "_meta.airsar") != 0) {
            *err = MALLOC(l);
            snprintf(*err, l,
                     "Error opening AirSAR file, filename does not appear\n"
                     "to have the form <basename>_meta.airsar:\n"
                     "      %s\n", filename);
            ret = FALSE;
        }
        else {
            p -= 5;  // back up over, then delete "_meta"
            *p = '\0';

            strcat(d, "_c.vvi2");
            if (!fileExists(d)) {
              *p = '\0';
              strcat(d, "_l.vvi2");
              if (!fileExists(d)) {
                *err = MALLOC(l);
                snprintf(*err, l,
                         "Couldn't find a data file for the metadata file:\n"
                         "  %s\n", filename);
                free(d);
                return FALSE;
              }
            }

            strcpy(meta_name, filename);
            strcpy(data_name, d);

            // these tests should always pass, we've already ensured
            // that the data & meta files exist
            if (!fileExists(meta_name) || !fileExists(data_name)) {
                *err = MALLOC(l);
                snprintf(*err, l,
                     "Error opening AirSAR file, not all required files\n"
                     "were found:\n"
                     "      Data file: %s - %s\n"
                     "      Metadata file: %s - %s\n",
                     d, fileExists(d) ? "Found" : "NOT FOUND",
                     filename, fileExists(filename) ? "Found" : "NOT FOUND");

                ret = FALSE;
            }
            else {
                ret = TRUE;
            }
        }

        free(d);
        return ret;
    }
    else if (has_airsar_ext && is_data_file && exists)
    {
        // we were given a data file -- this is easier, just need to figure
        // out the metadata file name, which is just "<basename>_meta.airsar"
        int ret=FALSE;

        char *m = MALLOC(sizeof(char)*(strlen(filename)+25));
        strcpy(m, filename);

        char *p = findExt(m);
        assert(p);

        p -= 2;  // back up over "_l" or "_c"
        if (*p != '_') {
            *err = MALLOC(l);
            snprintf(*err, l,
                     "Error opening AirSAR file, filename does not appear\n"
                     "to have the form <basename>_<band>.<type>i2:\n"
                     "      %s\n", filename);
        }
        else {
            *p = '\0';
            strcat(m, "_meta.airsar");

            strcpy(meta_name, m);
            strcpy(data_name, filename);

            if (!fileExists(meta_name) || !fileExists(data_name)) {
                *err = MALLOC(l);
                snprintf(*err, l,
                     "Error opening AirSAR file, not all required files\n"
                     "were found:\n"
                     "      Data file: %s - %s\n"
                     "      Metadata file: %s - %s\n",
                     filename, fileExists(filename) ? "Found" : "NOT FOUND",
                     m, fileExists(m) ? "Found" : "NOT FOUND");
            }
            else {
                // it can be done!
                ret = TRUE;
            }
        }

        free(m);
        return ret;
    }
    else {
        *err = MALLOC(l);
        if (!exists) {
            snprintf(*err, l, "File not found: %s\n", filename);
        }
        else {
          // this one should never happen, if try_airsar() is working right
          snprintf(*err, l, "Failed to open airsar file: %s\n", filename);
        }
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

static char *get_airsar_basename(const char *meta_name)
{
    char *airsar_basename = STRDUP(meta_name);
    char *p = strstr(airsar_basename, "_meta.airsar");
    if (!p) {
      p = strstr(airsar_basename, "_META.AIRSAR");
      if (!p) {
        p = strstr(airsar_basename, "_meta.AIRSAR");
        if (!p) {
          p = strstr(airsar_basename, "_META.airsar");
          if (!p) {
            // this should never happen, if handle_airsar() works properly
            assert(FALSE);
            free(airsar_basename);
            return NULL;
          }
        }
      }
    }
    *p = '\0';

    return airsar_basename;
}

static float get_airsar_polarimetric(unsigned char *byteBuf, float scale, int band)
{
  float ret = 0.0;
  float cal = (float)byteBuf[1]/254. + 1.5;
  float total_power = scale * cal * pow(2, byteBuf[0]);
  float ysca = 2. * sqrt(total_power);
  complexFloat cpx;

  switch (band) {
    case 0: // POWER
      ret = total_power;
      break;
    case 1: // SHH_AMP
      cpx.real = (float)byteBuf[2] * ysca / 127.0;
      cpx.imag = (float)byteBuf[3] * ysca / 127.0;
      ret = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      break;
    case 2: // SHH_PHASE
      cpx.real = (float)byteBuf[2] * ysca / 127.0;
      cpx.imag = (float)byteBuf[3] * ysca / 127.0;
      ret = atan2(cpx.imag, cpx.real);
      break;
    case 3: // SHV_AMP
      cpx.real = (float)byteBuf[4] * ysca / 127.0;
      cpx.imag = (float)byteBuf[5] * ysca / 127.0;
      ret = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      break;
    case 4: // SHV_PHASE
      cpx.real = (float)byteBuf[4] * ysca / 127.0;
      cpx.imag = (float)byteBuf[5] * ysca / 127.0;
      ret = atan2(cpx.imag, cpx.real);
      break;
    case 5: // SVH_AMP
      cpx.real = (float)byteBuf[6] * ysca / 127.0;
      cpx.imag = (float)byteBuf[7] * ysca / 127.0;
      ret = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      break;
    case 6: // SVH_PHASE
      cpx.real = (float)byteBuf[6] * ysca / 127.0;
      cpx.imag = (float)byteBuf[7] * ysca / 127.0;
      ret = atan2(cpx.imag, cpx.real);
      break;
    case 7: // SVV_AMP
      cpx.real = (float)byteBuf[8] * ysca / 127.0;
      cpx.imag = (float)byteBuf[9] * ysca / 127.0;
      ret = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      break;
    case 8: // SVV_PHASE
      cpx.real = (float)byteBuf[8] * ysca / 127.0;
      cpx.imag = (float)byteBuf[9] * ysca / 127.0;
      ret = atan2(cpx.imag, cpx.real);
      break;
  }

  return ret;
}

int read_airsar_client(int row_start, int n_rows_to_get,
                       void *dest_void, void *read_client_info,
                       meta_parameters *meta, int data_type)
{
    ReadAirsarClientInfo *info = (ReadAirsarClientInfo*) read_client_info;
    assert(data_type == GREYSCALE_FLOAT || data_type == RGB_FLOAT);
    float *dest = (float*)dest_void;

    int band_gs = info->band_gs;
    int band_r = info->band_r;
    int band_g = info->band_g;
    int band_b = info->band_b;

    if (info->airsar_data_type == AIRSAR_POLARIMETRIC) {
      float scale = (float) meta->airsar->scale_factor;
      // Scale is constant - according to Bruce Chapman from JPL
      // Apparently it is set to zero in the header, so we need to fix that
      scale = 1.0;
      unsigned char *byte_buf = MALLOC(sizeof(char)*10);
      FILE *fpIn = info->fp;
      int ns = meta->general->sample_count;
      int cal_off = info->header->calibration_header_offset;
      long offset = cal_off*10 + row_start*ns*10;
      FSEEK(fpIn,offset,SEEK_SET);
      int ii,kk,nn=0;

      if (!info->is_rgb) {
        for (ii=0; ii<n_rows_to_get; ++ii) {
          for (kk=0; kk<ns; ++kk) {
            FREAD(byte_buf, sizeof(char), 10, fpIn);
            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_gs);
          }
        }
      }
      else {
        for (ii=0; ii<n_rows_to_get; ++ii) {
          for (kk=0; kk<ns; ++kk) {
            FREAD(byte_buf, sizeof(char), 10, fpIn);
            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_r);
            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_g);
            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_b);
          }
        }
      }
    }
    else if (info->airsar_data_type == AIRSAR_DEM) {
      float incr = info->dem_header->elevation_increment;
      float off = info->dem_header->elevation_offset;
      int i,ns = meta->general->sample_count;
      long line_offset = info->header->first_data_offset / ns / 2;
      meta->general->line_count += line_offset;

      get_float_lines(info->fp, meta, row_start+line_offset, n_rows_to_get, dest);
      for (i=0; i<n_rows_to_get*ns; ++i)
        dest[i] = dest[i] * incr + off;
    }
    else {
      assert(data_type == GREYSCALE_FLOAT);
      int ns = meta->general->sample_count;
      long line_offset = info->header->first_data_offset / ns;
      if (info->airsar_data_type == AIRSAR_AMPLITUDE)
	line_offset /= 2;
      meta->general->line_count += line_offset;
      get_float_lines(info->fp, meta, row_start+line_offset, n_rows_to_get, dest);
    }

    return TRUE;
}

int get_airsar_thumbnail_data(int thumb_size_x, int thumb_size_y,
                              meta_parameters *meta, void *read_client_info,
                              void *dest_void, int data_type)
{
    ReadAirsarClientInfo *info = (ReadAirsarClientInfo*) read_client_info;

    int ns = meta->general->sample_count;
    int sf = meta->general->line_count / thumb_size_y;

    int band_gs = info->band_gs;
    int band_r = info->band_r;
    int band_g = info->band_g;
    int band_b = info->band_b;

    // temporary storage
    float *buf = MALLOC(sizeof(float)*ns);

    float *dest = (float*)dest_void;
    if (info->airsar_data_type == AIRSAR_POLARIMETRIC) {
      float scale = (float) meta->airsar->scale_factor;
      unsigned char *byte_buf = MALLOC(sizeof(char)*10);
      FILE *fpIn = info->fp;
      int cal_off = info->header->calibration_header_offset;
      int ii,kk,nn=0;

      if (!info->is_rgb) {
        for (ii=0; ii<thumb_size_y; ++ii) {
          for (kk=0; kk<thumb_size_x; ++kk) {
            long offset = cal_off*10 + (ii*ns + kk)*10;
            FSEEK(fpIn,offset,1);
            FREAD(byte_buf, sizeof(unsigned char), 10, fpIn);

            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_gs);
          }
          asfPercentMeter((float)ii/(thumb_size_y-1));
        }
      }
      else {
        for (ii=0; ii<thumb_size_y; ++ii) {
          for (kk=0; kk<thumb_size_x; ++kk) {
            long offset = cal_off*10 + (ii*ns + kk)*10;
            FSEEK(fpIn,offset,1);
            FREAD(byte_buf, sizeof(unsigned char), 10, fpIn);

            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_r);
            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_g);
            dest[nn++] = get_airsar_polarimetric(byte_buf, scale, band_b);
          }
        }
        asfPercentMeter((float)ii/(thumb_size_y-1));
      }
    }
    else {
      if (data_type == GREYSCALE_FLOAT) {
        int i,j;
        for (i=0; i<thumb_size_y; ++i) {
            get_float_line(info->fp, meta, i*sf, buf);
            for (j=0; j<thumb_size_x; ++j)
                dest[i*thumb_size_x+j] = buf[j*sf];
            asfPercentMeter((float)i/(thumb_size_y-1));
        }
      } else if (data_type == RGB_FLOAT) {
        // not possible
        assert(FALSE);
      }
    }

    free(buf);
    return TRUE;
}

void free_airsar_client_info(void *read_client_info)
{
    ReadAirsarClientInfo *info = (ReadAirsarClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    if (info->header) free(info->header);
    if (info->dem_header) free(info->dem_header);
    free(info);
}

meta_parameters *open_airsar(const char *data_name, const char *meta_name,
                             const char *band, ClientInterface *client)
{
    ReadAirsarClientInfo *info = MALLOC(sizeof(ReadAirsarClientInfo));

    char *airsar_basename = get_airsar_basename(meta_name);

    meta_parameters *meta = 
      import_airsar_meta(data_name, airsar_basename, FALSE);
    if (!meta)
      return NULL;
    info->header = NULL;
    info->dem_header = NULL;

    char *ext = findExt(data_name);
    if (strcmp_case(ext, ".corgr")==0) {
      asfPrintStatus("AirSAR: Coherence Image\n");
      info->airsar_data_type = AIRSAR_COHERENCE;
    }
    else if (strcmp_case(ext, ".datgr")==0 ||
	     strcmp_case(ext, ".dat")==0) {
      asfPrintStatus("AirSAR: Polarimetric Image (9 bands)\n");
      info->airsar_data_type = AIRSAR_POLARIMETRIC;
    }
    else if (strcmp_case(ext, ".demi2")==0) {
      asfPrintStatus("AirSAR: DEM Image\n");
      info->airsar_data_type = AIRSAR_DEM;
      asfPrintStatus("Reading AirSAR DEM header information...\n");
      info->dem_header = read_airsar_dem(data_name);
    }
    else if (strcmp_case(ext, ".vvi2")==0) {
      asfPrintStatus("AirSAR: Amplitude Image\n");
      info->airsar_data_type = AIRSAR_AMPLITUDE;
    }
    else {
      asfPrintStatus("AirSAR: (unknown image type)\n");
      info->airsar_data_type = AIRSAR_UNKNOWN;
    }

    if (info->airsar_data_type == AIRSAR_COHERENCE)
      meta->general->data_type = BYTE;
    else if (info->airsar_data_type == AIRSAR_POLARIMETRIC) {
      meta->general->data_type = REAL32;
      meta->general->image_data_type = POLARIMETRIC_IMAGE;
      meta->general->band_count = 9;
      strcpy(meta->general->bands,
          "POWER,SHH_AMP,SHH_PHASE,SHV_AMP,SHV_PHASE,SVH_AMP,SVH_PHASE,"
          "SVV_AMP,SVV_PHASE");
    }
    else if (info->airsar_data_type == AIRSAR_DEM)
      meta->general->data_type = INTEGER16;
    else if (info->airsar_data_type == AIRSAR_AMPLITUDE)
      meta->general->data_type = INTEGER16;
    else
      meta->general->data_type = INTEGER16;

    asfPrintStatus("Reading AirSAR General header information...\n");
    info->header = read_airsar_header(data_name);

    info->is_rgb = FALSE;
    info->band_gs = info->band_r = info->band_g = info->band_b = 0;

    if (band) {
        char *r, *b, *g;
        if (split3(band, &r, &g, &b, ',')) {
            // Looks like we were given 3 bands -- so, we are doing rgb
            if (info->airsar_data_type != AIRSAR_POLARIMETRIC) {
              asfPrintWarning("Cannot use RGB with non-Polarimetric data.\n");
              return FALSE;
            }

            info->band_r = get_band_number(meta->general->bands,
                    meta->general->band_count, r);
            if (info->band_r < 0)
                asfPrintWarning("Red band '%s' not found.\n", r);
            else
                asfPrintStatus("Red band is band #%d: %s\n",
                    info->band_r+1, r);

            info->band_g = get_band_number(meta->general->bands,
                    meta->general->band_count, g);
            if (info->band_g < 0)
                asfPrintWarning("Green band '%s' not found.\n", g);
            else
                asfPrintStatus("Green band is band #%d: %s\n",
                    info->band_g+1, g);

            info->band_b = get_band_number(meta->general->bands,
                    meta->general->band_count, b);
            if (info->band_b < 0)
                asfPrintWarning("Blue band '%s' not found.\n", b);
            else
                asfPrintStatus("Blue band is band #%d: %s\n",
                    info->band_b+1, b);

            if (info->band_r < 0 && info->band_g < 0 && info->band_b < 0) {
                // none of the bands were found
                return FALSE;
            }

            info->is_rgb = TRUE;
            FREE(r); FREE(g); FREE(b);

            set_bands_rgb(info->band_r, info->band_g, info->band_b);
        } else {
            // Single band name given
            info->band_gs = get_band_number(meta->general->bands,
                    meta->general->band_count, (char*)band);
            if (info->band_gs < 0) {
                asfPrintWarning("Band '%s' not found.\n", band);
                return FALSE;
            } else {
                asfPrintStatus("Reading band #%d: %s\n",
                    info->band_gs+1, band);
            }

            set_bands_greyscale(info->band_gs);
        }
    }

    client->read_client_info = info;
    client->read_fn = read_airsar_client;
    client->thumb_fn = get_airsar_thumbnail_data;
    client->free_fn = free_airsar_client_info;
    client->data_type = info->is_rgb ? RGB_FLOAT : GREYSCALE_FLOAT;

    info->fp = fopen(data_name, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open AirSAR file %s: %s\n",
            data_name, strerror(errno));
        return FALSE;
    }

    free(airsar_basename);
    return meta;
}
