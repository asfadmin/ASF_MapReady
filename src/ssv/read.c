#include "ssv.h"
#include "asf_import.h"
#include "get_ceos_names.h"
#include "asf_nan.h"
#include "asf_endian.h"

#include <errno.h>

int try_ext(const char *filename, const char *ext)
{
    char *buf = MALLOC(sizeof(char)*(strlen(filename)+strlen(ext)+5));
    if (ext[0]=='.')
        sprintf(buf, "%s%s", filename, ext);
    else
        sprintf(buf, "%s.%s", filename, ext);

    int ret = fileExists(buf);
    free(buf);

    return ret;
}

int read_file(const char *filename, const char *band, int on_fail_abort)
{
    if (meta)
        meta_free(meta);

    void (*err_func) (const char *format, ...);
    err_func = on_fail_abort ? asfPrintError : message_box;

    char *basename = get_filename(filename);

    void *read_client_info = NULL;
    ReadClientFn *read_fn = NULL;
    ThumbFn *thumb_fn = NULL;

    char *meta_name = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *data_name = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *err = NULL;

    if (try_asf(basename)) {
        if (handle_asf_file(filename, meta_name, data_name, &err)) {
            meta = read_asf_meta(meta_name);
            open_asf_data(data_name, band, meta,
                &read_fn, &thumb_fn, &read_client_info);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else {
        err_func("Don't know how to load file: %s\n", filename);
        return FALSE;
    }

    assert(meta);

    nl = meta->general->line_count;
    ns = meta->general->sample_count;

    assert(read_fn && read_client_info);

    data_ci = cached_image_new_from_file(data_name, meta, read_fn,
        thumb_fn, read_client_info);

    assert(data_ci);

    FREE(basename);

    center_samp = crosshair_samp = (double)ns/2.;
    center_line = crosshair_line = (double)nl/2.;

    g_meta_name = STRDUP(meta_name);
    free(meta_name);

    g_data_name = STRDUP(data_name);
    free(data_name);

    return TRUE;
}



/*
    char *filename = STRDUP(filename_in);
    char *basename = get_basename(filename);


    // first need to figure out what kind of file this is
    // we will do that based on the extension
    // user may have just given basename, so we may need to hunt
    char *img_file=NULL;

    char *ext = findExt(filename);

    if (!ext) {
        if (fileExists(filename)) {
            // no extension (e.g., ALOS)
            img_file = STRDUP(filename);
            ext = "";
        } else if (try_ext(filename, ".img")) {
            img_file = appendExt(filename, ".img");
            ext = ".img";
        } else if (try_ext(filename, ".D")) {
            img_file = appendExt(filename, ".D");
            ext = ".D";
        } else {
            // could be an ALOS basename...
            ext = "";
        }
    } else {
        // user gave extension
        img_file = STRDUP(filename);
    }

    if (strcmp_case(ext, ".img") == 0) {
        assert(img_file);
        char *meta_filename = appendExt(filename, ".meta");
        if (fileExists(meta_filename)) {
            meta = meta_read(meta_filename);
        } else {
            asfPrintError("Cannot find metadata for: %s\n", meta_filename);
        }
        free(meta_filename);
        read_asf(img_file, band);
    } else if (strcmp_case(ext, ".D") == 0) {
        assert(img_file);
        char *meta_filename = appendExt(filename, ".L");
        if (fileExists(meta_filename)) {
            meta = meta_create(meta_filename);
        } else {
            asfPrintError("Cannot find metadata: %s\n", meta_filename);
        }
        free(meta_filename);
        if (band)
            asfPrintWarning("Band specification ignored.\n");
        read_D(img_file);
    } else if (strncmp_case(filename, "IMG-", 4) == 0) {
        assert(img_file);
        char *meta_filename = MALLOC(sizeof(char)*strlen(filename));
        strcpy(meta_filename, "LED-");
        char *p = strchr(filename+5, '-') + 1;
        strcat(meta_filename, p);
        if (fileExists(meta_filename)) {
            printf("Creating metadata: %s\n", p);
            meta = meta_create(p);
        } else {
            asfPrintError("Cannot find metadata: %s\n", meta_filename);
        }
        if (band)
            asfPrintWarning("Band specification ignored.\n");
        read_alos(p, img_file, meta_filename);
        free(meta_filename);
    } else {
        // possibly an alos basename -- prepend "LED-" (if needed) and see
        char *meta_filename=NULL;
        if (strncmp_case(basename, "LED-", 4) == 0) {
            if (!fileExists(filename))
                asfPrintError("Cannot find: %s\n", filename);
            meta_filename = STRDUP(filename);
        } else {
            char *dir = get_dirname(filename);
            if (strlen(dir) == 0) {
                meta_filename = MALLOC(sizeof(char)*(10+strlen(filename)));
                strcpy(meta_filename, "LED-");
                strcat(meta_filename, filename);
            } else {
                char *file = get_filename(filename);
                meta_filename = MALLOC(sizeof(char)*(10+strlen(filename)));
                sprintf(meta_filename, "%s/LED-%s", dir, file);
                free(file);
            }
            free(dir);
        }
        if (meta_filename && fileExists(meta_filename)) {
            char **dataName = MALLOC(sizeof(char*)*MAX_BANDS);
            int i,nBands;
            for (i=0; i<MAX_BANDS; ++i)
                dataName[i] = MALLOC(sizeof(char)*256);
            char *p = meta_filename;
            if (strncmp_case(p, "LED-", 4) == 0) p += 4;
            get_ceos_data_name(p, dataName, &nBands);
            int which_band=-1;
            if (band) {
                for (i=0; i<nBands; ++i) {
                    if (strcmp(dataName[i], band) == 0) {
                        which_band=i;
                        break;
                    }
                }
            } else
                which_band = 0;
            if (which_band < 0)
                asfPrintError("Band '%s' not found.\n");
            meta = meta_create(p);
            read_alos(p, dataName[which_band], meta_filename);
            FREE_BANDS(dataName);
        } else {
            asfPrintError("Unknown image type: %s\n", img_file);
        }
        free(meta_filename);
    }

    FREE(img_file);
    FREE(filename);
}
*/
