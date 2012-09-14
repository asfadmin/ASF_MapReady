#include "asf_view.h"
#include "asf_import.h"

typedef struct rasterfile {
               int  ras_magic;
               int  ras_width;
               int  ras_height;
               int  ras_depth;
               int  ras_length;
               int  ras_type;
               int  ras_maptype;
               int  ras_maplength;
} rasterfile_t;

typedef struct {
    FILE *fp;    // data file pointer
    rasterfile_t rasterfile;
    int big;
} ReadRasClientInfo;

int try_ras(const char *filename)
{
    return endsWith(filename, ".ras");
}

int handle_ras_file(const char *filename, char *meta_name, char *data_name,
                       char **err)
{
    char *image_file = STRDUP(filename);
    if (endsWith(image_file, ".ras") && fileExists(image_file)) {
      strcpy(meta_name, image_file);
      strcpy(data_name, image_file);
      return TRUE;
    }

    int l = sizeof(char)*strlen(filename)*3+255;
    *err = MALLOC(l);
    snprintf(*err, l,
             "Error opening SUN RAS file: %s\n",
             filename);

    return FALSE;
}

int read_ras_client(int row_start, int n_rows_to_get,
                    void *dest_void, void *read_client_info,
                    meta_parameters *meta, int data_type)
{
    ReadRasClientInfo *info = (ReadRasClientInfo*) read_client_info;
    int ns = meta->general->sample_count;
    int len = n_rows_to_get*ns;
    float *dest = (float *)dest_void;
    int ii;

    // alternating pixel values of I and Q
    unsigned char *buf = CALLOC(sizeof(unsigned char), len);
    int offset = info->rasterfile.ras_maplength + 8;
    int ret = fseek(info->fp, (offset+row_start*ns)*sizeof(unsigned char), SEEK_SET);
    if (ret != 0) {
      memset(dest, 0, len * sizeof(float));
    }
    else {
      fread(buf, sizeof(unsigned char), len, info->fp);
      for (ii=0; ii<len; ++ii) {
        dest[ii] = (float)buf[ii];
      }
    }

    return TRUE;
}

void free_ras_client_info(void *read_client_info)
{
    ReadRasClientInfo *info = (ReadRasClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

meta_parameters *open_ras(const char *filename, const char *band,
                          const char *dataname, const char *metaname,
                          int multilook, ClientInterface *client)
{
    ReadRasClientInfo *info = MALLOC(sizeof(ReadRasClientInfo));

    info->fp = fopen(filename, "rb");

    if (!info->fp) {
        asfPrintWarning("Failed to open SUN RAS file %s: %s\n",
            filename, strerror(errno));
        return NULL;
    }

    client->read_client_info = info;
    client->read_fn = read_ras_client;
    client->thumb_fn = NULL;
    client->free_fn = free_ras_client_info;
    client->data_type = GREYSCALE_FLOAT;

    FILE *ras = fopen(filename,"rb");
    fread(&info->rasterfile, 32, 1, ras);

    if (info->rasterfile.ras_magic == 0x59a66a95) {
      info->big = TRUE;
    }
    else if (info->rasterfile.ras_magic == 0x956aa659) {
      big32(info->rasterfile.ras_width);
      big32(info->rasterfile.ras_height);
      big32(info->rasterfile.ras_maplength);
      info->big = FALSE;
    } else {
      asfPrintWarning("Unexpected magic number in SUN RAS file: %x\n", info->rasterfile.ras_magic);
      free_ras_client_info(info);
      return NULL;
    }
    fclose(ras);

    meta_parameters *meta = raw_init();
    meta->general->line_count = info->rasterfile.ras_height;
    meta->general->sample_count = info->rasterfile.ras_width;
    //printf("RAS: height=%d width=%d\n", info->rasterfile.ras_height,info->rasterfile.ras_width);

    strcpy(meta->general->bands, "???");
    meta->general->band_count = 1;

    set_bands_greyscale(TRUE);
    return meta;
}
