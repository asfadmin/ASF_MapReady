#include "asf_view.h"

typedef struct {
    FILE *fp;    // data file pointer
    int byteswap;
} ReadGenericClientInfo;

int handle_generic_file(const char *filename, char **err)
{
    if (!fileExists(filename)) {
        int l = sizeof(char)*strlen(filename)*2+255;
        *err = MALLOC(l);
        snprintf(*err, l, "Error opening generic binary file.\n"
                          "File not found: %s\n", filename);
        return FALSE;
    }
    return TRUE;
}

meta_parameters *read_generic_meta(const char *filename)
{
  assert(generic_specified);
  meta_parameters *meta = raw_init();
  meta->general->line_count = generic_bin_height;
  meta->general->sample_count = generic_bin_width;
  meta->general->data_type = generic_bin_datatype;
  strcpy(meta->general->basename, filename);
  return meta;
}

static void get_generic_line(ReadGenericClientInfo *info, meta_parameters *meta,
                             int row, float *buf)
{
    get_float_line(info->fp, meta, row, buf);
    if (info->byteswap) {
        int i;
        for (i=0; i<meta->general->sample_count; ++i)
            ieee_big32(buf[i]);
    }
}

static void get_generic_lines(ReadGenericClientInfo *info, meta_parameters *meta,
                              int row, int n, float *buf)
{
    get_float_lines(info->fp, meta, row, n, buf);
    if (info->byteswap) {
        int i;
        for (i=0; i<n*meta->general->sample_count; ++i)
            ieee_big32(buf[i]);
    }
}

int read_generic_client(int row_start, int n_rows_to_get,
                        void *dest_void, void *read_client_info,
                        meta_parameters *meta, int data_type)
{
    ReadGenericClientInfo *info = (ReadGenericClientInfo*) read_client_info;
    int ns = meta->general->sample_count;

    if (meta->general->data_type == BYTE) {
        unsigned char *dest = (unsigned char*)dest_void;
        if (data_type==GREYSCALE_BYTE) {
            // reading byte data directly into the byte cache
            FSEEK64(info->fp, ns*row_start, SEEK_SET);
            FREAD(dest, sizeof(unsigned char), n_rows_to_get*ns, info->fp);
        }
        else {
            assert(FALSE);
        }
    } else {
        float *dest = (float*)dest_void;
        if (data_type==GREYSCALE_FLOAT) {
            // this is the normal case, just reading in a strip of lines
            // from the file directly into the floating point cache
            get_generic_lines(info, meta, row_start, n_rows_to_get, dest);
        } else {
            assert(FALSE);
        }
    }

    return TRUE;
}

int get_generic_thumbnail_data(int thumb_size_x, int thumb_size_y,
                               meta_parameters *meta, void *read_client_info,
                               void *dest_void, int data_type)
{
    ReadGenericClientInfo *info = (ReadGenericClientInfo*) read_client_info;

    int sf = meta->general->line_count / thumb_size_y;
    //assert(sf==meta->general->sample_count / thumb_size_x);
    int i,j,ns = meta->general->sample_count;

    // temporary storage
    float *buf = MALLOC(sizeof(float)*ns);

    if (meta->general->data_type == BYTE) {
        // BYTE case -- data file contains bytes.
        unsigned char *dest = (unsigned char*)dest_void;
        if (data_type == GREYSCALE_BYTE) {
            // data file contains byte data, and we are just pulling out
            // one band to display.
            for (i=0; i<thumb_size_y; ++i) {
                get_generic_line(info, meta, i*sf, buf);
                for (j=0; j<thumb_size_x; ++j)
                    dest[i*thumb_size_x+j] = (unsigned char)(buf[j*sf]);
                asfPercentMeter((float)i/(thumb_size_y-1));
            }
        }
        else {
            assert(FALSE);
        }
    } else {
        // this is the normal case -- regular old floating point data,
        // we just read with get_float_line and populate directly into
        // a floating point array
        float *dest = (float*)dest_void;
        if (data_type == GREYSCALE_FLOAT) {
            for (i=0; i<thumb_size_y; ++i) {
                get_generic_line(info, meta, i*sf, buf);
                for (j=0; j<thumb_size_x; ++j)
                    dest[i*thumb_size_x+j] = buf[j*sf];
                asfPercentMeter((float)i/(thumb_size_y-1));
            }
        } else {
            assert(FALSE);
        }
    }

    free(buf);
    return TRUE;
}

void free_generic_client_info(void *read_client_info)
{
    ReadGenericClientInfo *info = (ReadGenericClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

int open_generic_data(const char *filename,
                      meta_parameters *meta, ClientInterface *client)
{
    ReadGenericClientInfo *info = MALLOC(sizeof(ReadGenericClientInfo));

    info->fp = fopen(filename, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open Generic binary file %s: %s\n",
            filename, strerror(errno));
        return FALSE;
    }

    info->byteswap = generic_bin_byteswap;

    client->read_client_info = info;
    client->read_fn = read_generic_client;
    client->thumb_fn = get_generic_thumbnail_data;
    client->free_fn = free_generic_client_info;

    if (meta->general->data_type == BYTE)
        client->data_type = GREYSCALE_BYTE;
    else
        client->data_type = GREYSCALE_FLOAT;

    return TRUE;
}
