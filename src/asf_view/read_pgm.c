#include "asf_view.h"
#include <ctype.h>

typedef struct {
    FILE *fp;
    int rgb_flag;
} ReadPgmClientInfo;

int try_pgm(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
      return strcmp_case(ext, ".pgm") == 0 ||
             strcmp_case(ext, ".ppm") == 0;
    } else if (try_extensions) {
      return try_ext(filename, ".pgm") || try_ext(filename, ".ppm");
    }
    return FALSE;
}

int handle_pgm_file(const char *filename, char *meta_name, char *data_name,
                    char **err)
{
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_pgm_ext = has_ext && strcmp_case(ext,".pgm")==0;
    int has_ppm_ext = has_ext && strcmp_case(ext,".ppm")==0;

    if (!has_ext) {
        has_pgm_ext = try_ext(filename, ".pgm");
        if (!has_pgm_ext)
            has_ppm_ext = try_ext(filename, ".ppm");
    }

    if (has_pgm_ext || has_ppm_ext)
    {
        char *d=NULL;
        if (has_ext)
            d = STRDUP(filename);
        else if (has_pgm_ext)
            d = appendExt(filename, ".pgm");
        else if (has_ppm_ext)
            d = appendExt(filename, ".ppm");
        assert(d);

        strcpy(meta_name, d);
        strcpy(data_name, d);
        free(d);

        int ret;
        if (!fileExists(data_name)) {
            // I don't think this will actually ever run
            int l = sizeof(char)*strlen(filename)+255;
            *err = MALLOC(l);
            snprintf(*err, l, "Error opening PGM/PPM file: %s\n", data_name);
            ret = FALSE;
        }
        else
            ret = TRUE;

        return ret;
    }
    else {
        // in theory this shouldn't happen, if try_ext is working
        assert(!try_ext(filename, ".pgm"));
        int l = sizeof(char)*strlen(filename)+255;
        *err = MALLOC(l);
        snprintf(*err, l, "Failed to open %s as a PGM File.\n", filename);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

int read_pgm_client(int row_start, int n_rows_to_get,
                    void *dest_void, void *read_client_info,
                    meta_parameters *meta, int data_type)
{
    // since we set "require_full_load", we should be reading in the
    // entire image
    assert(row_start == 0);
    assert(n_rows_to_get == meta->general->line_count);

    unsigned char *dest = (unsigned char*)dest_void;
    ReadPgmClientInfo *info = (ReadPgmClientInfo*)read_client_info;

    // these will explode if we ever call this fn a second time on
    // the same data.  Shouldn't happen, because we set "require_full_load"
    assert(info->fp);

    int ns = meta->general->sample_count;
    int nchan = info->rgb_flag ? 3 : 1;
    unsigned char *buf = MALLOC(ns * sizeof(unsigned char) * nchan);
    int ii, jj, b, k=0;

    // iterate over all rows in the pgm
    for ( ii = 0; ii < n_rows_to_get; ii++ )
    {
      FREAD(buf, sizeof(unsigned char), ns*nchan, info->fp);
      for (jj = 0 ; jj < ns; jj++ ) {
        for (b = 0; b<nchan; ++b)
          dest[k+b] = buf[nchan*jj+b];

        k += nchan;
      }
    }

    fclose(info->fp);
    info->fp = NULL;

    FREE(buf);

    return TRUE;
}

void free_pgm_client_info(void *read_client_info)
{
    ReadPgmClientInfo *info = (ReadPgmClientInfo*)read_client_info;
    if (info->fp) fclose(info->fp); // should never be still open
    free(info);
}

// for pgm/ppm, combined the "open_meta" and "open_data" functions -- both
// will be opening the same file.
meta_parameters* open_pgm(const char *data_name, ClientInterface *client)
{
    ReadPgmClientInfo *info = MALLOC(sizeof(ReadPgmClientInfo));

    client->read_client_info = info;
    client->read_fn = read_pgm_client;
    client->thumb_fn = NULL;
    client->free_fn = free_pgm_client_info;
    client->require_full_load = TRUE;

    info->fp = fopen(data_name, "rb");

    unsigned char magic[3];
    FREAD(magic, sizeof(unsigned char), 3, info->fp);
    if (magic[0] == 'P' && (magic[1] == '5' || magic[1] == '6')) {
      info->rgb_flag = magic[1] == '6';
    }
    else {
      asfPrintWarning("Invalid ppm/pgm header!\n");
      return NULL;
    }

    // skip any "comment" lines
    // rest of the line (or, the next two lines) contains "width height"
    char line[255];
    while (1) {
      fgets(line, 255, info->fp);
      if (line[0] != '#' || feof(info->fp)) break;
    }

    int width, height, marker;
    int n = sscanf(line, "%d %d %d\n", &width, &height, &marker);
    if (n == 3) {
      if (marker != 255) {
        printf("Suspicious: %d != 255\n", marker);
      }
    } else if (n == 2 || n == 1) {
      if (n == 1) {
        fgets(line, 255, info->fp);
        sscanf(line, "%d\n", &height);
      }
      // read "255b" ('b' could be newline or just a space)
      FREAD(line, sizeof(unsigned char), 4, info->fp);
      if (line[0] != '2' || line[1] != '5' || line[2] != '5' ||
          !isspace(line[3])) {
        printf("Suspicious: '%c%c%c%c' != '255 '\n",
               line[0], line[1], line[2], line[3]);
      }
    } else {
      printf("Did not find start of data marker...\n");
    }

    client->data_type = info->rgb_flag ? RGB_BYTE : GREYSCALE_BYTE;

    meta_parameters *meta = raw_init();
    meta->general->line_count = height;
    meta->general->sample_count = width;
    meta->general->band_count = 1;
    strcpy(meta->general->bands, "");
    meta->general->data_type = BYTE;

    return meta;
}
