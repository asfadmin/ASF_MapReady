#include "asf_view.h"

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

int try_prepension(const char *filename, const char *prepension)
{
    char *dir = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *file = MALLOC(sizeof(char)*(strlen(filename)+10));
    split_dir_and_file(filename, dir, file);

    char *buf = MALLOC(sizeof(char)*(strlen(filename)+
        strlen(prepension)+5));

    if (strlen(dir) > 0)
        sprintf(buf, "%s/%s%s", dir, prepension, file);
    else
        sprintf(buf, "%s%s", prepension, file);

    int ret = fileExists(buf);

    free(buf);
    free(dir);
    free(file);

    return ret;
}

int read_file(const char *filename, const char *band, int on_fail_abort)
{
    if (meta)
        meta_free(meta);

    void (*err_func) (const char *format, ...);
    err_func = on_fail_abort ? asfPrintError : message_box;

    ClientInterface *client = MALLOC(sizeof(ClientInterface));

    // these defaults should be overridden by the client if necessary
    client->data_type = UNDEFINED;
    client->require_full_load = FALSE;

    char *meta_name = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *data_name = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *err = NULL;

    // If you are considering adding support for another data type,
    // see the comments in read_template.c
    if (try_asf(filename)) {
        if (handle_asf_file(filename, meta_name, data_name, &err)) {
            meta = read_asf_meta(meta_name);
            open_asf_data(data_name, band, meta, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else if (try_ceos(filename)) {
        if (handle_ceos_file(filename, meta_name, data_name, &err)) {
            meta = read_ceos_meta(meta_name);
            open_ceos_data(data_name, meta_name, band, meta, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else if (try_alos(filename)) {
        if (handle_alos_file(filename, band, meta_name, data_name, &err)) {
            meta = read_ceos_meta(meta_name);
            open_ceos_data(data_name, meta_name, band, meta, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else if (try_jpeg(filename)) {
        if (handle_jpeg_file(filename, meta_name, data_name, &err)) {
            meta = open_jpeg(data_name, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else {
        err_func("Don't know how to load file: %s\n", filename);
        return FALSE;
    }

    data_ci = cached_image_new_from_file(data_name, meta, client);

    assert(data_ci);

    nl = meta->general->line_count;
    ns = meta->general->sample_count;

    center_samp = crosshair_samp = (double)ns/2.;
    center_line = crosshair_line = (double)nl/2.;

    g_meta_name = STRDUP(meta_name);
    free(meta_name);

    g_data_name = STRDUP(data_name);
    free(data_name);

    return TRUE;
}
