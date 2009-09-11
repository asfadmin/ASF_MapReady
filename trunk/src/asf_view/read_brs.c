#include "asf_view.h"

typedef struct {
    FILE *fp;
} ReadBrsClientInfo;

int try_brs(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
      return strcmp_case(ext, ".brs") == 0;
    } else if (try_extensions) {
      int ret = try_ext(filename, ".brs");
      if (!ret)
        ret = try_ext(filename, ".BRS");
      return ret;
    }
    return FALSE;
}

int handle_brs_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_brs_ext = has_ext && strcmp_case(ext,".brs")==0;
    char *file;

    if (!has_ext) {
        has_brs_ext = try_ext(filename, ".brs");
        if (!has_brs_ext) {
            has_brs_ext = try_ext(filename, ".BRS");
            if (has_brs_ext)
                file = appendExt(filename, ".BRS");
        }
        else {
            file = appendExt(filename, ".brs");
        }
    }
    else
      file = STRDUP(filename);

    if (has_brs_ext)
    {
        strcpy(meta_name, file);
        strcpy(data_name, file);

        int ret;
        if (!fileExists(data_name)) {
            // I don't think this will actually ever run
            int l = sizeof(char)*strlen(filename)+255;
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening BRS file: %s\n", data_name);
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
        snprintf(*err, l, "Failed to open %s as a BRS File.\n", filename);
        free(file);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

int read_brs_client(int row_start, int n_rows_to_get,
                    void *dest_void, void *read_client_info,
                    meta_parameters *meta, int data_type)
{
    // since we set "require_full_load", we should be reading in the
    // entire image
    assert(row_start == 0);
    assert(n_rows_to_get == meta->general->line_count);

    unsigned char *dest = (unsigned char*)dest_void;
    ReadBrsClientInfo *info = (ReadBrsClientInfo*)read_client_info;

    // these will explode if we ever call this fn a second time on
    // the same data.  Shouldn't happen, because we set "require_full_load"
    assert(info->fp);

    int ns = meta->general->sample_count;
    //unsigned char *brs_buf = MALLOC(ns * sizeof(unsigned char));
    int ii;

    // iterate over all rows in the brs
    for ( ii = 0; ii < n_rows_to_get; ii++ )
      FREAD(dest + ii*ns, sizeof(unsigned char), ns, info->fp);

    fclose(info->fp);
    info->fp = NULL;
    //FREE(brs_buf);

    return TRUE;
}

void free_brs_client_info(void *read_client_info)
{
    ReadBrsClientInfo *info = (ReadBrsClientInfo*)read_client_info;
    if (info->fp) fclose(info->fp); // should never be still open
    free(info);
}

// for brs, combined the "open_meta" and "open_data" functions -- both
// will be opening the same file.
meta_parameters* open_brs(const char *data_name, ClientInterface *client)
{
    ReadBrsClientInfo *info = MALLOC(sizeof(ReadBrsClientInfo));

    info->fp = FOPEN(data_name, "rb");

    client->read_client_info = info;
    client->read_fn = read_brs_client;
    client->thumb_fn = NULL;
    client->free_fn = free_brs_client_info;
    client->require_full_load = TRUE;
    client->data_type = GREYSCALE_BYTE;

    meta_parameters *meta = raw_init();

    // 700x700 size is hard-coded... I think this is ok
    int fs = fileSize(data_name);
    meta->general->line_count = 600;
    meta->general->sample_count = fs/meta->general->line_count;
    meta->general->band_count = 1;
    strcpy(meta->general->bands, "");

    return meta;
}
