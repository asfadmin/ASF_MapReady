#include "asf_view.h"
#include "asf_import.h"
#include "get_ceos_names.h"
#include "asf_nan.h"
#include "asf_endian.h"

typedef struct {
    FILE *fp;
    int headerBytes;
    int reclen;
} ReadCeosClientInfo;

int try_ceos(const char *filename)
{
    char **dataName=NULL, **metaName=NULL, *baseName;
    int nBands, trailer;

    baseName = MALLOC(sizeof(char)*(strlen(filename)+5));

    ceos_file_pairs_t ret = 
        get_ceos_names(filename, baseName, &dataName, &metaName,
        &nBands, &trailer);

    // we don't even care about these -- just interested in the return val
    free_ceos_names(dataName, metaName);
    free(baseName);

    return ret != NO_CEOS_FILE_PAIR;
}

int handle_ceos_file(const char *filename, char *meta_name, char *data_name,
                     char **err)
{
    char **dataName=NULL, **metaName=NULL, *baseName;
    int nBands, trailer;

    baseName = MALLOC(sizeof(char)*(strlen(filename)+5));

    ceos_file_pairs_t ret = 
        get_ceos_names(filename, baseName, &dataName, &metaName,
        &nBands, &trailer);

    // we should not be in here if try_ceos(), above, failed
    assert(ret != NO_CEOS_FILE_PAIR);

    // which of all the dataName to put into data_name?
    // default to the first band, unless one of the returned values matches
    // what we were given -- in that case, the user has asked for a specific
    // band, so that's what we need to use.
    int i, found=FALSE;
    for (i=0; i<nBands; ++i) {
        if (strcmp(filename, dataName[i]) == 0) {
            // match -- user asked for specific band
            strcpy(data_name, dataName[i]);
            found = TRUE;
            break;
        }
    }

    // default to first band
    if (!found)
        strcpy(data_name, dataName[0]);

    strcpy(meta_name, metaName[0]);

    free_ceos_names(dataName, metaName);
    free(baseName);

    return TRUE;
}

meta_parameters *read_ceos_meta(const char *meta_name)
{
    // meta_create() likes the basename -- so we have to jump through a
    // few hoops here before we can call it.
    char **dataName=NULL, **metaName=NULL, *baseName;
    int nBands, trailer;

    baseName = MALLOC(sizeof(char)*(strlen(filename)+5));

    get_ceos_names(meta_name, baseName, &dataName, &metaName, &nBands, &trailer);

    free_ceos_names(dataName, metaName);
    meta_parameters *meta = meta_create(baseName);
    free(baseName);

    return meta;
}

int read_ceos_client(int row_start, int n_rows_to_get,
                     void *dest_void, void *read_client_info,
                     meta_parameters *meta)
{
    float *dest = (float*)dest_void;

    ReadCeosClientInfo *info = (ReadCeosClientInfo*)read_client_info;
    int ii, jj, ns = meta->general->sample_count;

    if (meta->general->data_type == INTEGER16)
    {
        unsigned short *shorts = MALLOC(sizeof(unsigned short)*ns);
        for (ii=0; ii<n_rows_to_get; ++ii) {
            long long offset = (long long)(info->headerBytes +
                (ii+row_start)*info->reclen);

            FSEEK64(info->fp, offset, SEEK_SET);
            FREAD(shorts, sizeof(unsigned short), ns, info->fp);

            for (jj = 0; jj < ns; ++jj) {
                big16(shorts[jj]);
                dest[jj + ii*ns] = (float)(shorts[jj]);
            }
        }
        free(shorts);
    }
    else if (meta->general->data_type == BYTE)
    {
        unsigned char *bytes = MALLOC(sizeof(unsigned char)*ns);
        for (ii=0; ii<n_rows_to_get; ++ii) {
            long long offset = (long long)(info->headerBytes +
                (ii+row_start)*info->reclen);

            FSEEK64(info->fp, offset, SEEK_SET);
            FREAD(bytes, sizeof(unsigned char), ns, info->fp);

            for (jj = 0; jj < ns; ++jj)
                dest[jj + ii*ns] = (float)(bytes[jj]);
        }
        free(bytes);
    }
    else {
        asfPrintError("Unsupported data type in CEOS data: %d\n",
            meta->general->data_type);
    }

    return TRUE;
}

int get_ceos_thumbnail_data(int thumb_size_x, int thumb_size_y,
                            meta_parameters *meta, void *read_client_info,
                            void *dest_void)
{
    float *dest = (float*)dest_void;

    ReadCeosClientInfo *info = (ReadCeosClientInfo*)read_client_info;
    int ii, jj, ns = meta->general->sample_count;

    int sf = meta->general->line_count / thumb_size_y;
    assert(sf==meta->general->sample_count / thumb_size_x);

    if (meta->general->data_type == INTEGER16)
    {
        unsigned short *shorts = MALLOC(sizeof(unsigned short)*ns);
        for (ii=0; ii<thumb_size_y; ++ii) {
            long long offset = (long long)(info->headerBytes + ii*sf*info->reclen);

            FSEEK64(info->fp, offset, SEEK_SET);
            FREAD(shorts, sizeof(unsigned short), ns, info->fp);

            for (jj = 0; jj < thumb_size_x; ++jj) {
                big16(shorts[jj]);
                dest[jj + ii*thumb_size_x] = (float)(shorts[jj*sf]);
            }

            asfPercentMeter((float)ii/(thumb_size_y-1));
        }
        free(shorts);
    }
    else if (meta->general->data_type == BYTE)
    {
        unsigned char *bytes = MALLOC(sizeof(unsigned char)*ns);
        for (ii=0; ii<thumb_size_y; ++ii) {
            long long offset = (long long)(info->headerBytes + ii*sf*info->reclen);

            FSEEK64(info->fp, offset, SEEK_SET);
            FREAD(bytes, sizeof(unsigned char), ns, info->fp);

            for (jj = 0; jj < thumb_size_x; ++jj)
                dest[jj + ii*thumb_size_x] = (float)(bytes[jj*sf]);

            asfPercentMeter((float)ii/(thumb_size_y-1));
        }
        free(bytes);
    }
    else {
        asfPrintError("Unsupported data type in CEOS data: %d\n",
            meta->general->data_type);
    }

    return TRUE;
}

void free_ceos_client_info(void *read_client_info)
{
    ReadCeosClientInfo *info = (ReadCeosClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

int open_ceos_data(const char *data_name, const char *meta_name,
                   const char *band, meta_parameters *meta,
                   ClientInterface *client)
{
    ReadCeosClientInfo *info = MALLOC(sizeof(ReadCeosClientInfo));

    int ns = meta->general->sample_count;

    struct IOF_VFDR image_fdr;
    get_ifiledr(meta_name, &image_fdr);

    int leftFill = image_fdr.lbrdrpxl;
    int rightFill = image_fdr.rbrdrpxl;

    info->headerBytes = firstRecordLen((char*)data_name) +
        (image_fdr.reclen - (ns + leftFill + rightFill)*image_fdr.bytgroup);

    info->reclen = image_fdr.reclen;

    info->fp = fopen(data_name, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open CEOS file %s: %s\n",
            data_name, strerror(errno));
        return FALSE;
    }

    client->read_client_info = info;
    client->read_fn = read_ceos_client;
    client->thumb_fn = get_ceos_thumbnail_data;
    client->free_fn = free_ceos_client_info;

    client->data_type = GREYSCALE_FLOAT;

    return TRUE;
}
