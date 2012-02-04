#include "asf_view.h"
#include "uavsar.h"

typedef struct {
    FILE *fp;       // data file pointer
    int ml;         // multilook flag
    int is_complex; // true if data is real/imag pairs
} ReadUavsarClientInfo;

static char * find_uavsar_annotation_file(const char *data_file_name)
{
    char *ret = NULL;
    char *after_mission_name = STRDUP(data_file_name + 7);
    char *buf1 = asf_strReplace(after_mission_name, "HH", "");
    char *buf2 = asf_strReplace(buf1, "HV", "");
    char *polarization_stripped = asf_strReplace(buf2, "VV", "");
    char *fixed_name = STRDUP(data_file_name);
    fixed_name[7] = '\0';
    strcat(fixed_name, polarization_stripped);
    char *ann_try = appendExt(fixed_name, ".ann");
    if (fileExists(ann_try)) {
        ret = STRDUP(ann_try);
    }
    FREE(fixed_name);
    FREE(ann_try);
    FREE(polarization_stripped);
    FREE(buf2);
    FREE(buf1);
    FREE(after_mission_name);
    return ret;
}

int try_uavsar(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        if (strcmp_case(ext, ".grd") == 0 ||
            strcmp_case(ext, ".hgt") == 0 ||
            strcmp_case(ext, ".mlc") == 0)
        {
           char *ann_file = find_uavsar_annotation_file(filename);
           int ret = ann_file != NULL;
           FREE(ann_file);
           return ret;
        }
    }   
    return FALSE;
}

int handle_uavsar_file(const char *filename, char *meta_name, char *data_name,
                       char **err)
{
    // for uavsar data, require that they specify the full name of the data
    // file they want to look at, and we'll find the .ann file.
    char *ext = findExt(filename);
    int has_ext = ext && strlen(ext) > 0;
    int has_uavsar_ext = has_ext &&
        (strcmp_case(ext,".grd")==0 ||
         strcmp_case(ext,".mlc")==0 ||
         strcmp_case(ext,".hgt")==0);

    if (has_uavsar_ext)
    {
        int ret;

        char *m = find_uavsar_annotation_file(filename);
        if (!m) {
            int l = sizeof(char)*strlen(filename)*2+255;
            *err = MALLOC(l);
            snprintf(*err, l, "Could not find annotation file for UAVSAR "
                "data file '%s'.\n", filename);
            ret = FALSE;
        }
        else {
            strcpy(meta_name, m);
            strcpy(data_name, filename);
            const char *d = filename;

            if (!fileExists(meta_name) || !fileExists(data_name)) {
                int l = sizeof(char)*strlen(filename)*2+255;
                *err = MALLOC(l);
                snprintf(*err, l,
                    "Error opening UAVSAR data file.\n"
                    "  Annotation file: %s - %s\n"
                    "        Data file: %s - %s\n",
                    m, fileExists(m) ? "Found" : "NOT FOUND",
                    d, fileExists(d) ? "Found" : "NOT FOUND");

                ret = FALSE;
            }
            else
                ret = TRUE;
        
            free(m);
        }

        return ret;
    }
    else {
        // in theory this shouldn't happen, if try_ext is working
        int l = sizeof(char)*strlen(filename)*2+255;
        *err = MALLOC(l);
        snprintf(*err, l,
            "Failed to open %s as an UAVSAR Format File.\n", filename);
        return FALSE;
    }

    // not reached
    assert(FALSE);
    return FALSE;
}

meta_parameters *read_uavsar_meta(const char *meta_name, const char *data_name)
{
  char *ext = findExt(data_name);
  assert(ext);

  uavsar_polsar *polsar_params = NULL;
  if (strcmp_case(ext, ".mlc") == 0)
    polsar_params = read_uavsar_polsar_params(meta_name, POLSAR_MLC);
  else if (strcmp_case(ext, ".grd") == 0)
    polsar_params = read_uavsar_polsar_params(meta_name, POLSAR_GRD);
  else if (strcmp_case(ext, ".hgt") == 0)
    polsar_params = read_uavsar_polsar_params(meta_name, POLSAR_HGT);
  else
    asfPrintError("Unexpected UAVSAR extension: %s\n", ext);

  assert(polsar_params);
  return uavsar_polsar2meta(polsar_params);
}

static void get_uavsar_line(ReadUavsarClientInfo *info, meta_parameters *meta,
                            int row, float *buf)
{
    // wrapper for get_float_line() that multilooks if needed
    int j,ns=meta->general->sample_count;
    if (info->ml) {
        assert(meta->sar);
        int k,nlooks = meta->sar->look_count;
        row *= nlooks;

        // we fudged the line count in the metadata for the
        // viewer (which is displaying a multilooked image), we must
        // put the correct value back for the reader
        int lc = meta->general->line_count;
        meta->general->line_count = g_saved_line_count;

        // FIXME: figure a nice way to avoid allocating every time we read a line
        get_float_line(info->fp, meta, row, buf);
        float *tmp = MALLOC(sizeof(float)*meta->general->sample_count);
        for (k=1; k<nlooks; ++k) {
            get_float_line(info->fp, meta, row+k, tmp);
            for (j=0; j<ns; ++j)
                ieee_big32(tmp[j]);
            for (j=0; j<ns; ++j)
                buf[j] += tmp[j];
        }
        for (j=0; j<ns; ++j)
            buf[j] /= nlooks;
        free(tmp);

        // restore fudged value
        meta->general->line_count = lc;
    }
    else {
        // no multilooking case
        if (info->is_complex) {
            complexFloat *cf_buf = MALLOC(sizeof(complexFloat)*ns);
            get_complexFloat_line(info->fp, meta, row, cf_buf);
            for (j=0; j<ns; ++j) {
                ieee_big32(cf_buf[j].real);
                ieee_big32(cf_buf[j].imag);
                buf[j] = hypot(cf_buf[j].real, cf_buf[j].imag);
                //buf[j] = atan2_check(cf_buf[j].imag, cf_buf[j].real);
            }
            FREE(cf_buf);
        }
        else {
            get_float_line(info->fp, meta, row, buf);
            for (j=0; j<ns; ++j)
                ieee_big32(buf[j]);
        }
    }
}

static void get_uavsar_lines(ReadUavsarClientInfo *info, meta_parameters *meta,
                             int row, int n, float *buf)
{
    // wrapper for get_float_line() that multilooks if needed
    int j,ns=meta->general->sample_count;
    if (info->ml) {
        assert(meta->sar);
        int i,k;
        int nlooks = meta->sar->look_count;
        row *= nlooks;

        // we fudged the line count in the metadata for the
        // viewer (which is displaying a multilooked image), we must
        // put the correct value back for the reader
        int lc = meta->general->line_count;
        meta->general->line_count = g_saved_line_count;

        float *tmp = MALLOC(sizeof(float)*ns);
        for (i=0; i<n; ++i) {
            float *this_row = buf + i*ns;
            get_float_line(info->fp, meta, row+i*nlooks, this_row);
            for (j=0; j<ns; ++j)
                ieee_big32(this_row[j]);
            int n_read = nlooks;
            for (k=1; k<nlooks; ++k) {
                if (row+n*nlooks+k >= meta->general->line_count) {
                    --n_read;
                } else {
                    get_float_line(info->fp, meta, row+i*nlooks+k, tmp);
                    for (j=0; j<ns; ++j)
                        ieee_big32(tmp[j]);
                    for (j=0; j<ns; ++j)
                        this_row[j] += tmp[j];
                }
            }
            for (j=0; j<meta->general->sample_count; ++j)
                this_row[j] /= n_read;
        }
        free(tmp);

        // restore the fudged value
        meta->general->line_count = lc;
    }
    else {
        // no multilooking case
        if (info->is_complex) {
            complexFloat *cf_buf = MALLOC(sizeof(complexFloat)*ns*n);
            get_complexFloat_lines(info->fp, meta, row, n, cf_buf);
            for (j=0; j<n*ns; ++j) {
                ieee_big32(cf_buf[j].real);
                ieee_big32(cf_buf[j].imag);
                buf[j] = hypot(cf_buf[j].real, cf_buf[j].imag);
                //buf[j] = atan2_check(cf_buf[j].imag, cf_buf[j].real);
            }
            FREE(cf_buf);
        }
        else {
            get_float_lines(info->fp, meta, row, n, buf);
            for (j=0; j<n*ns; ++j)
                ieee_big32(buf[j]);
        }
    }
}

int read_uavsar_client(int row_start, int n_rows_to_get,
                       void *dest_void, void *read_client_info,
                       meta_parameters *meta, int data_type)
{
    ReadUavsarClientInfo *info = (ReadUavsarClientInfo*) read_client_info;
    int ns = meta->general->sample_count;

    if (meta->general->data_type == BYTE) {
        unsigned char *dest = (unsigned char*)dest_void;
        if (data_type==GREYSCALE_BYTE) {
            FSEEK64(info->fp, ns*row_start, SEEK_SET);
            FREAD(dest, sizeof(unsigned char), n_rows_to_get*ns, info->fp);
        }
        else {
            assert(FALSE);
        }
    } else {
        float *dest = (float*)dest_void;
        if (data_type==GREYSCALE_FLOAT) {
            get_uavsar_lines(info, meta, row_start, n_rows_to_get, dest);
        } else {
            assert(FALSE);
        }
    }

    return TRUE;
}

int get_uavsar_thumbnail_data(int thumb_size_x, int thumb_size_y,
                              meta_parameters *meta, void *read_client_info,
                              void *dest_void, int data_type)
{
    ReadUavsarClientInfo *info = (ReadUavsarClientInfo*) read_client_info;

    int i, j;

    int sf = meta->general->line_count / thumb_size_y;
    int ns = meta->general->sample_count;

    // temporary storage
    float *buf = MALLOC(sizeof(float)*ns);

    if (meta->general->data_type == BYTE) {
        unsigned char *dest = (unsigned char*)dest_void;
        if (data_type == GREYSCALE_BYTE) {
            for (i=0; i<thumb_size_y; ++i) {
                get_uavsar_line(info, meta, i*sf, buf);
                for (j=0; j<thumb_size_x; ++j)
                    dest[i*thumb_size_x+j] = (unsigned char)(buf[j*sf]);
                asfPercentMeter((float)i/(thumb_size_y-1));
            }
        }
        else {
            assert(FALSE);
        }
    } else {
        float *dest = (float*)dest_void;
        if (data_type == GREYSCALE_FLOAT) {
            for (i=0; i<thumb_size_y; ++i) {
                get_uavsar_line(info, meta, i*sf, buf);
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

void free_uavsar_client_info(void *read_client_info)
{
    ReadUavsarClientInfo *info = (ReadUavsarClientInfo*) read_client_info;
    if (info->fp) fclose(info->fp);
    free(info);
}

int open_uavsar_data(const char *filename, int multilook,
                     meta_parameters *meta, ClientInterface *client)
{
    ReadUavsarClientInfo *info = MALLOC(sizeof(ReadUavsarClientInfo));
    info->ml = multilook;

    char *ext = findExt(filename);
    if (strcmp_case(ext, ".grd") == 0 ||
        strcmp_case(ext, ".mlc") == 0)
    {
        // UAVSAR .grd/.mlc files are real for HHHH, HVHV, and VVVV
        info->is_complex = strstr(filename, "HHHV_") != NULL ||
                           strstr(filename, "HHVV_") != NULL ||
                           strstr(filename, "HVVV_") != NULL;
    }
    else if (strcmp_case(ext, ".hgt") == 0) {
        // UAVSAR .hgt files are real
        info->is_complex = FALSE;
    }

    asfPrintStatus("Reading UAVSAR data as %s\n",
                   info->is_complex ? "complex" : "real");

    if (info->is_complex)
        meta->general->data_type = COMPLEX_REAL32;

    info->fp = fopen(filename, "rb");
    if (!info->fp) {
        asfPrintWarning("Failed to open UAVSAR data file %s: %s\n",
            filename, strerror(errno));
        return FALSE;
    }

    client->read_client_info = info;
    client->read_fn = read_uavsar_client;
    client->thumb_fn = get_uavsar_thumbnail_data;
    client->free_fn = free_uavsar_client_info;

    if (meta->general->data_type == BYTE)
        client->data_type = GREYSCALE_BYTE;
    else
        client->data_type = GREYSCALE_FLOAT;

    return TRUE;
}
