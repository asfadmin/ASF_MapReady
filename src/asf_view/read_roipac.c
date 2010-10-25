#include "asf_view.h"
#include "asf_import.h"

static const int ROIPAC_CPX=1;
static const int ROIPAC_RMG=2;
static const int ROIPAC_SLC=3;

typedef struct {
    FILE *fp;    // data file pointer
    int roipac_file_type;
    int amp;
    int ml;      // should data be multilooked for display?
} ReadRoipacClientInfo;

int try_roipac(const char *filename)
{
    if (endsWith(filename, ".cor") ||
	endsWith(filename, ".cor.rsc") ||
        endsWith(filename, ".amp") ||
        endsWith(filename, ".amp.rsc") ||
        endsWith(filename, ".int") ||
        endsWith(filename, ".int.rsc") ||
        endsWith(filename, ".slc") ||
        endsWith(filename, ".slc.rsc") ||
        endsWith(filename, ".unw") ||
        endsWith(filename, ".unw.rsc") ||
        endsWith(filename, ".hgt") ||
        endsWith(filename, ".hgt.rsc"))
    {
      return TRUE;
    }

    return FALSE;
}

int handle_roipac_file(const char *filename, char *meta_name, char *data_name,
                       char **err)
{
    char *image_file = STRDUP(filename);
    if (endsWith(image_file, ".rsc")) 
       image_file[strlen(image_file)-4] = '\0';
    char *rsc_file = appendStr(image_file, ".rsc");

    if (fileExists(image_file) && fileExists(rsc_file)) {
      strcpy(meta_name, rsc_file);
      strcpy(data_name, image_file);
      return TRUE;
    }

    int l = sizeof(char)*strlen(filename)*3+255;
    *err = MALLOC(l);
    snprintf(*err, l,
             "Error opening ROI_PAC file: %s\n"
             "Tried to open %s (%s) and %s (%s)\n",
             filename,
             image_file, fileExists(image_file) ? "OK" : "NOT FOUND",
             rsc_file, fileExists(rsc_file) ? "OK" : "NOT FOUND");

    return FALSE;
}

static meta_parameters *read_roipac_meta(const char *meta_name)
{
  return meta_read_roipac(meta_name,NULL);
}

int read_roipac_client(int row_start, int n_rows_to_get,
                       void *dest_void, void *read_client_info,
                       meta_parameters *meta, int data_type)
{
    ReadRoipacClientInfo *info = (ReadRoipacClientInfo*) read_client_info;
    int ns = meta->general->sample_count;
    int len = n_rows_to_get*ns*2;
    float *dest = (float *)dest_void;
    int ii;

    // note that we will not use the capitalized versions of seek and read,
    // roipac files are bad for not being long enough -- we'll pad with zeros
    // We are treating the SLC files as CPX -- this means the "multilook"
    // checkbox won't be available, that can be added later (FIXME) if at all
    if (info->roipac_file_type == ROIPAC_CPX ||
        info->roipac_file_type == ROIPAC_SLC) {
      // alternating pixel values of I and Q
      float *buf = CALLOC(sizeof(float), len);
      int ret = fseek(info->fp, row_start*ns*2*sizeof(float), SEEK_SET);
      if (ret != 0) {
        memset(dest, 0, len/2 * sizeof(float));
      }
      else {
        fread(buf, sizeof(float), len, info->fp);
        if (info->amp) {
          for (ii=0; ii<len; ii+=2) {
            dest[ii/2] = hypot(buf[ii], buf[ii+1]);
          }
        }
        else {
          for (ii=0; ii<len; ii+=2) {
            dest[ii/2] = atan2_check(buf[ii+1], buf[ii]);
          }
        }
      }
    }
    else if (info->roipac_file_type == ROIPAC_RMG) {
      // alternating lines of amplitude and phase
      memset(dest, 0, len/2 * sizeof(float));
      int off = info->amp ? 0 : 1;
      int ret = fseek(info->fp, (row_start*2+off)*ns*sizeof(float), SEEK_SET);
      if (ret == 0) {
        for (ii=0; ii<n_rows_to_get; ++ii) {
          fread(dest+ns*ii, sizeof(float), ns, info->fp);
          fseek(info->fp, ns*sizeof(float), SEEK_CUR);
        }
      }
    }
    else {
      // can't happen
      assert(FALSE);
    }

    return TRUE;
}

void free_roipac_client_info(void *read_client_info)
{
    ReadRoipacClientInfo *info = (ReadRoipacClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

meta_parameters *open_roipac(const char *filename, const char *band,
                             const char *dataname, const char *metaname,
                             int multilook, ClientInterface *client)
{
    ReadRoipacClientInfo *info = MALLOC(sizeof(ReadRoipacClientInfo));

    if (!band)
      info->amp = !strstr(filename, ".int") && !strstr(filename, ".cor");
    else
      info->amp = strncmp_case(band, "AMP", 3)==0;

    info->ml = FALSE;

    if (endsWith(filename, ".cor") ||
	endsWith(filename, ".cor.rsc") ||
        endsWith(filename, ".unw") ||
	endsWith(filename, ".unw.rsc") ||
        endsWith(filename, ".hgt") ||
	endsWith(filename, ".hgt.rsc")) {
	info->roipac_file_type = ROIPAC_RMG;
    }
    else if (endsWith(filename, ".amp") ||
             endsWith(filename, ".amp.rsc") ||
             endsWith(filename, ".int") ||
             endsWith(filename, ".int.rsc")) {
        info->roipac_file_type = ROIPAC_CPX;
    }
    else if (endsWith(filename, ".slc") ||
             endsWith(filename, ".slc.rsc")) {
        info->roipac_file_type = ROIPAC_SLC;
    }
    else {
        assert(FALSE);
    }

    info->fp = fopen(filename, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open ROI_PAC file %s: %s\n",
            filename, strerror(errno));
        return NULL;
    }

    client->read_client_info = info;
    client->read_fn = read_roipac_client;
    client->thumb_fn = NULL;
    client->free_fn = free_roipac_client_info;
    client->data_type = GREYSCALE_FLOAT;

    meta_parameters *meta = read_roipac_meta(metaname);

    strcpy(meta->general->bands, "AMP,PHASE");
    meta->general->band_count = 2;

    set_bands_greyscale(info->amp ? 0 : 1);
    return meta;
}
