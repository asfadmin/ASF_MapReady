#include "asf_view.h"
#include "asf_import.h"

typedef struct {
    FILE *fp;    // data file pointer
} ReadAirsarClientInfo;

static int is_valid_data_airsar_ext(const char *ext)
{
    return ext &&
        (strcmp_case(ext, ".vvi2") == 0
         || strcmp_case(ext, ".demi2") == 0
         //|| strcmp_case(ext, ".datgr") == 0
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

        char *d = STRDUP(filename);
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

        char *m = STRDUP(filename);
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

meta_parameters *read_airsar_meta(const char *meta_name, char *data_name)
{
    char *airsar_basename = get_airsar_basename(meta_name);

    meta_parameters *meta = import_airsar_meta(airsar_basename);

    char *ext = findExt(data_name);
    if (strcmp_case(ext, ".corgr")==0)
      meta->general->data_type = BYTE;
    else
      meta->general->data_type = INTEGER16;

    free(airsar_basename);
    return meta;
}

int read_airsar_client(int row_start, int n_rows_to_get,
                       void *dest_void, void *read_client_info,
                       meta_parameters *meta, int data_type)
{
    ReadAirsarClientInfo *info = (ReadAirsarClientInfo*) read_client_info;

    //assert(meta->general->data_type == INTEGER16);
    assert(data_type == GREYSCALE_FLOAT);

    float *dest = (float*)dest_void;
    get_float_lines(info->fp, meta, row_start, n_rows_to_get, dest);

    return TRUE;
}

int get_airsar_thumbnail_data(int thumb_size_x, int thumb_size_y,
                              meta_parameters *meta, void *read_client_info,
                              void *dest_void, int data_type)
{
    ReadAirsarClientInfo *info = (ReadAirsarClientInfo*) read_client_info;

    int i,j;
    int ns = meta->general->sample_count;
    int sf = meta->general->line_count / thumb_size_y;

    //assert(sf==meta->general->sample_count / thumb_size_x);

    // temporary storage
    float *buf = MALLOC(sizeof(float)*ns);

    float *dest = (float*)dest_void;
    if (data_type == GREYSCALE_FLOAT) {
        for (i=0; i<thumb_size_y; ++i) {
            get_float_line(info->fp, meta, i*sf, buf);
            for (j=0; j<thumb_size_x; ++j)
                dest[i*thumb_size_x+j] = buf[j*sf];
            asfPercentMeter((float)i/(thumb_size_y-1));
        }
    } else if (data_type == RGB_FLOAT) {
        // airsar is only greyscale as of yet...
        assert(FALSE);
    }

    free(buf);
    return TRUE;
}

void free_airsar_client_info(void *read_client_info)
{
    ReadAirsarClientInfo *info = (ReadAirsarClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

int open_airsar_data(const char *filename, meta_parameters *meta,
                     ClientInterface *client)
{
    ReadAirsarClientInfo *info = MALLOC(sizeof(ReadAirsarClientInfo));

    info->fp = fopen(filename, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open AirSAR file %s: %s\n",
            filename, strerror(errno));
        return FALSE;
    }

    client->read_client_info = info;
    client->read_fn = read_airsar_client;
    client->thumb_fn = get_airsar_thumbnail_data;
    client->free_fn = free_airsar_client_info;
    client->data_type = GREYSCALE_FLOAT;

    return TRUE;
}
