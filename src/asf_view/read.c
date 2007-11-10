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

int read_file(const char *filename, const char *band, int multilook,
              int on_fail_abort)
{
    if (curr->meta)
        meta_free(curr->meta);
    if (curr->data_ci)
        cached_image_free(curr->data_ci);

    void (*err_func) (const char *format, ...);
    err_func = on_fail_abort ? asfPrintError : message_box;

    ClientInterface *client = MALLOC(sizeof(ClientInterface));

    // these defaults should be overridden by the client if necessary
    client->data_type = UNDEFINED;
    client->require_full_load = FALSE;

    char *meta_name = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *data_name = MALLOC(sizeof(char)*(strlen(filename)+10));
    char *err = NULL;
    meta_parameters *meta;

    // If you are considering adding support for another data type,
    // see the comments in read_template.c
    if (try_asf(filename)) {
        if (handle_asf_file(filename, meta_name, data_name, &err)) {
            meta = read_asf_meta(meta_name);
            open_asf_data(data_name, band, multilook, meta, client);
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
    } else if (try_jpeg(filename)) {
        if (handle_jpeg_file(filename, meta_name, data_name, &err)) {
            meta = open_jpeg(data_name, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else if (try_tiff(filename)) {
        if (handle_tiff_file(filename, meta_name, data_name, &err)) {
          open_tiff_data(data_name, band, client); // Must be called before read_tiff_meta()
          meta = read_tiff_meta(meta_name, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else if (try_png(filename)) {
        if (handle_png_file(filename, meta_name, data_name, &err)) {
            meta = open_png(data_name, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else if (try_pgm(filename)) {
        if (handle_pgm_file(filename, meta_name, data_name, &err)) {
            meta = open_pgm(data_name, client);
        } else {
            err_func(err);
            free(err);
            return FALSE;
        }
    } else {
        err_func("Don't know how to load file: %s\n", filename);
        return FALSE;
    }

    if (!meta) {
        char err_str[1024];
        snprintf(err_str, 1024, "Error creating metadata for: %s\n",
                 filename);
        err_func(err_str);
        return FALSE;
    }

    g_saved_line_count = meta->general->line_count;
    if (multilook && meta->sar) {
        // change the stored metadata!
        meta->general->line_count /= meta->sar->look_count;
        meta->sar->azimuth_time_per_pixel *= meta->sar->look_count;
        meta->general->y_pixel_size *= meta->sar->look_count;
    }

    // set up the ImageInfo for this image
    curr->meta = meta;
    curr->data_ci = cached_image_new_from_file(data_name, meta, client,
                                               &(curr->stats));
    assert(curr->data_ci);

    int nl = meta->general->line_count;
    curr->nl = nl;

    int ns = meta->general->sample_count;
    curr->ns = ns;

    if (center_samp < 0 || center_samp >= ns ||
        center_line < 0 || center_line >= nl)
    {
        center_samp = (double)ns/2.;
        center_line = (double)nl/2.;
    }
    if (crosshair_samp < 0 || crosshair_samp >= ns ||
        crosshair_line < 0 || crosshair_line >= nl)
    {
        crosshair_samp = (double)ns/2.;
        crosshair_line = (double)nl/2.;
    }

    curr->meta_name = STRDUP(meta_name);
    free(meta_name);

    curr->data_name = STRDUP(data_name);
    free(data_name);

    return TRUE;
}
