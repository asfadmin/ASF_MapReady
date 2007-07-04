#include "ssv.h"

#include "asf_import.h"
#include "get_ceos_names.h"
#include "asf_nan.h"
#include "asf_endian.h"

typedef struct {
    int headerBytes;
    int reclen;
} ReadCeosClientInfo;

int try_ceos(const char *filename)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return strcmp_case(ext, ".D") == 0 ||
               strcmp_case(ext, ".L") == 0;
    } else {
        return try_ext(filename, ".D");
    }
}

int handle_ceos_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_asf_ext = has_ext &&
        (strcmp_case(ext,".D")==0 || strcmp_case(ext,".L")==0);

    // either they gave us an CEOS extension, or we try adding it
    // to a user-provided basename, and that file does exist
    if (has_asf_ext || try_ext(filename, ".D"))
    {
        char *m = appendExt(filename, ".L");
        char *d = appendExt(filename, ".D");

        strcpy(meta_name, m);
        strcpy(data_name, d);

        int ret;
        if (!fileExists(meta_name) || !fileExists(data_name)) {
            int l = sizeof(char)*strlen(filename)*2+255;
            *err = MALLOC(l);
            snprintf(*err, l,
                "Error opening CEOS file.\n"
                "  Metadata file: %s - %s\n"
                "      Data file: %s - %s\n",
                m, fileExists(m) ? "Found" : "NOT FOUND",
                d, fileExists(d) ? "Found" : "NOT FOUND");

            ret = FALSE;
        }
        else
            ret = TRUE;

        free(m);
        free(d);

        return ret;
    }
    else {
        // in theory this shouldn't happen, if try_ext is working
        assert(!try_ext(filename, ".D"));
        int l = sizeof(char)*strlen(filename)*2+255;
        *err = MALLOC(l);
        snprintf(*err, l,
            "Failed to open %s as a CEOS File.\n", filename);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

meta_parameters *read_ceos_meta(const char *meta_name)
{
    return meta_create(meta_name);
}

int read_ceos_client(FILE *fp, int row_start, int n_rows_to_get,
                    float *dest, void *read_client_info,
                    meta_parameters *meta)
{
    ReadCeosClientInfo *info = (ReadCeosClientInfo*)read_client_info;
    int ii, jj, ns = meta->general->sample_count;

    if (meta->general->data_type == INTEGER16)
    {
        unsigned short *shorts = MALLOC(sizeof(unsigned short)*ns);
        for (ii=0; ii<n_rows_to_get; ++ii) {
            long long offset = (long long)(info->headerBytes +
                (ii+row_start)*info->reclen);

            FSEEK64(fp, offset, SEEK_SET);
            FREAD(shorts, sizeof(unsigned short), ns, fp);

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

            FSEEK64(fp, offset, SEEK_SET);
            FREAD(bytes, sizeof(unsigned char), ns, fp);

            for (jj = 0; jj < ns; ++jj)
                dest[jj + ii*ns] = (float)(bytes[jj]);
        }
        free(bytes);
    }

    return TRUE;
}

int get_ceos_thumbnail_data(FILE *fp, int thumb_size_x,
                            int thumb_size_y, meta_parameters *meta,
                            void *read_client_info, float *dest)
{
    ReadCeosClientInfo *info = (ReadCeosClientInfo*)read_client_info;
    int ii, jj, ns = meta->general->sample_count;

    int sf = meta->general->line_count / thumb_size_y;
    assert(sf==meta->general->sample_count / thumb_size_x);

    if (meta->general->data_type == INTEGER16)
    {
        unsigned short *shorts = MALLOC(sizeof(unsigned short)*ns);
        for (ii=0; ii<thumb_size_y; ++ii) {
            long long offset = (long long)(info->headerBytes + ii*sf*info->reclen);

            FSEEK64(fp, offset, SEEK_SET);
            FREAD(shorts, sizeof(unsigned short), ns, fp);

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

            FSEEK64(fp, offset, SEEK_SET);
            FREAD(bytes, sizeof(unsigned char), ns, fp);

            for (jj = 0; jj < thumb_size_x; ++jj)
                dest[jj + ii*thumb_size_x] = (float)(bytes[jj*sf]);

            asfPercentMeter((float)ii/(thumb_size_y-1));
        }
        free(bytes);
    }

    return TRUE;
}

int open_ceos_data(const char *data_name, const char *meta_name,
                   const char *band, meta_parameters *meta, 
                   ReadClientFn **read_fn, ThumbFn **thumb_fn,
                   void **read_client_info)
{
    ReadCeosClientInfo *info = MALLOC(sizeof(ReadCeosClientInfo));

    int ns = meta->general->sample_count;

    *read_client_info = info;
    *read_fn = read_ceos_client;
    *thumb_fn = get_ceos_thumbnail_data;

    struct IOF_VFDR image_fdr;
    get_ifiledr(meta_name, &image_fdr);

    int leftFill = image_fdr.lbrdrpxl;
    int rightFill = image_fdr.rbrdrpxl;

    info->headerBytes = firstRecordLen((char*)data_name) +
        (image_fdr.reclen - (ns + leftFill + rightFill)*image_fdr.bytgroup);

    info->reclen = image_fdr.reclen;

    return TRUE;
}

//static void read_alos(const char *basename, const char *img_name,
//                      const char *meta_name)
//{
//    printf("Reading ALOS: %s\n", img_name);
//    struct IOF_VFDR image_fdr;
//    get_ifiledr(basename, &image_fdr);
//    read_ceos(&image_fdr, img_name);
//}
