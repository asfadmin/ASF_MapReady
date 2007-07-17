#include "asf_view.h"

typedef struct {
    FILE *fp; // data file pointer
    int band; // which band we are using
} ReadAsfClientInfo;

int try_asf(const char *filename)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return strcmp_case(ext, ".img") == 0 ||
               strcmp_case(ext, ".meta") == 0;
    } else {
        return try_ext(filename, ".img");
    }
}

int handle_asf_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_asf_ext = has_ext &&
        (strcmp_case(ext,".img")==0 || strcmp_case(ext,".meta")==0);

    // either they gave us an ASF Internal extension, or we try adding it
    // to a user-provided basename, and that file does exist
    if (has_asf_ext || try_ext(filename, ".img"))
    {
        char *m = appendExt(filename, ".meta");
        char *d = appendExt(filename, ".img");

        strcpy(meta_name, m);
        strcpy(data_name, d);

        int ret;
        if (!fileExists(meta_name) || !fileExists(data_name)) {
            int l = sizeof(char)*strlen(filename)*2+255;
            *err = MALLOC(l);
            snprintf(*err, l,
                "Error opening ASF Internal Format file.\n"
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
        assert(!try_ext(filename, ".img"));
        int l = sizeof(char)*strlen(filename)*2+255;
        *err = MALLOC(l);
        snprintf(*err, l,
            "Failed to open %s as an ASF Internal Format File.\n", filename);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

meta_parameters *read_asf_meta(const char *meta_name)
{
    return meta_read(meta_name);
}

int read_asf_client(int row_start, int n_rows_to_get,
                    void *dest_void, void *read_client_info,
                    meta_parameters *meta)
{
    float *dest = (float*)dest_void;
    ReadAsfClientInfo *info = (ReadAsfClientInfo*) read_client_info;

    get_float_lines(info->fp, meta, row_start + nl*info->band,
                    n_rows_to_get, dest);

    return TRUE;
}

int get_asf_thumbnail_data(int thumb_size_x, int thumb_size_y,
                           meta_parameters *meta, void *read_client_info,
                           void *dest_void)
{
    float *dest = (float*)dest_void;
    ReadAsfClientInfo *info = (ReadAsfClientInfo*) read_client_info;

    float *buf = MALLOC(sizeof(float)*meta->general->sample_count);

    int sf = meta->general->line_count / thumb_size_y;
    assert(sf==meta->general->sample_count / thumb_size_x);

    int off = meta->general->line_count * info->band;

    int i,j;
    for (i=0; i<thumb_size_y; ++i) {
        get_float_line(info->fp, meta, i*sf + off, buf);
        for (j=0; j<thumb_size_x; ++j)
            dest[i*thumb_size_x+j] = buf[j*sf];
        asfPercentMeter((float)i/(thumb_size_y-1));
    }

    free(buf);
    return TRUE;
}

void free_asf_client_info(void *read_client_info)
{
    ReadAsfClientInfo *info = (ReadAsfClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

int open_asf_data(const char *filename, const char *band,
                  meta_parameters *meta, ClientInterface *client)
{
    ReadAsfClientInfo *info = MALLOC(sizeof(ReadAsfClientInfo));

    int b = 0;
    if (band)
        b = get_band_number(meta->general->bands,
                meta->general->band_count, (char*)band);
    if (b<0) {
        // we should really have the capability of returning this
        // error back to the caller, however we currently only have
        // the capability of choosing the band at the command line,
        // where it is ok to error out, so for now we'll just leave
        // this
        asfPrintError("Band '%s' not found.\n");
    } else if (band)
        asfPrintStatus("Reading band #%d: %s\n", b+1, band);

    info->band = b;
    info->fp = fopen(filename, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open ASF Internal file %s: %s\n",
            filename, strerror(errno));
        return FALSE;
    }

    client->read_client_info = info;
    client->read_fn = read_asf_client;
    client->thumb_fn = get_asf_thumbnail_data;
    client->free_fn = free_asf_client_info;

    client->data_type = GREYSCALE_FLOAT;

    return TRUE;
}
