#include "asf_view.h"

typedef struct {
    FILE *fp;    // data file pointer
    int is_rgb;  // are we doing rgb compositing
    int band_gs; // which band we are using (when viewing as greyscale)
    int band_r;  // which band we are using for red (when rgb compositing)
    int band_g;  // which band we are using for green (when rgb compositing)
    int band_b;  // which band we are using for blue (when rgb compositing)
    int ml;      // should data be multilooked for display?
} ReadAsfClientInfo;

int try_asf(const char *filename, int try_extensions)
{
    char *ext = findExt(filename);

    if (ext && strlen(ext) > 0) {
        return strcmp_case(ext, ".img") == 0 ||
               strcmp_case(ext, ".meta") == 0;
    } else if (try_extensions) {
        return try_ext(filename, ".img");
    } else {
        return FALSE;
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
  meta_parameters *meta = meta_read(meta_name);

  // If the ASF internal format file is a single-band image with RGB color map
  // in the metadata, then store the color map as an ASF style look-up table
  if (meta->colormap) {
    int i;
    meta_colormap *mc = meta->colormap;
    char lut_file[256];
    char *lut_loc = (char *)MALLOC(sizeof(char)*(strlen(get_asf_share_dir())+128));
    sprintf(lut_loc, "%s%clook_up_tables", get_asf_share_dir(), DIR_SEPARATOR);
    sprintf(lut_file,"%s%c%s", lut_loc, DIR_SEPARATOR, EMBEDDED_ASF_COLORMAP_LUT_FILE);
    FILE *lutFP = (FILE *)FOPEN(lut_file, "wt");
    fprintf(lutFP, "# Look up table type: %s\n", mc->look_up_table);
    fprintf(lutFP, "# Originating source: %s\n", meta_name);
    fprintf(lutFP, "# Index   Red   Green   Blue\n");
    for (i=0; i<mc->num_elements; i++) {
      fprintf(lutFP, "%03d    %03d    %03d    %03d\n",
              i, mc->rgb[i].red, mc->rgb[i].green, mc->rgb[i].blue);
    }
    fprintf(lutFP, "\n");
    FCLOSE(lutFP);
  }

  return meta;
}

static void get_asf_line(ReadAsfClientInfo *info, meta_parameters *meta,
                         int row, float *buf)
{
    // wrapper for get_float_line() that multilooks if needed
    if (info->ml) {
        assert(meta->sar);
        int k,j,nlooks = meta->sar->look_count;
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
            for (j=0; j<meta->general->sample_count; ++j)
                buf[j] += tmp[j];
        }
        for (j=0; j<meta->general->sample_count; ++j)
            buf[j] /= nlooks;
        free(tmp);

        meta->general->line_count = lc;
    }
    else {
        // no multilooking case
        get_float_line(info->fp, meta, row, buf);
    }
}

static void get_asf_lines(ReadAsfClientInfo *info, meta_parameters *meta,
                          int row, int n, float *buf)
{
    // wrapper for get_float_line() that multilooks if needed
    if (info->ml) {
        assert(meta->sar);
        int i,j,k;
        int nlooks = meta->sar->look_count;
        int ns = meta->general->sample_count;
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
            int n_read = nlooks;
            for (k=1; k<nlooks; ++k) {
                if (row+n*nlooks+k >= meta->general->line_count) {
                    --n_read;
                } else {
                    get_float_line(info->fp, meta, row+i*nlooks+k, tmp);
                    for (j=0; j<meta->general->sample_count; ++j)
                        this_row[j] += tmp[j];
                }
            }
            for (j=0; j<meta->general->sample_count; ++j)
                this_row[j] /= n_read;
        }
        free(tmp);

        meta->general->line_count = lc;
    }
    else {
        // no multilooking case
        get_float_lines(info->fp, meta, row, n, buf);
    }
}

int read_asf_client(int row_start, int n_rows_to_get,
                    void *dest_void, void *read_client_info,
                    meta_parameters *meta, int data_type)
{
    ReadAsfClientInfo *info = (ReadAsfClientInfo*) read_client_info;
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    if (meta->general->data_type == BYTE) {
        unsigned char *dest = (unsigned char*)dest_void;
        if (data_type==GREYSCALE_BYTE) {
            // reading byte data directly into the byte cache
            FSEEK64(info->fp, ns*(row_start + nl*info->band_gs), SEEK_SET);
            FREAD(dest, sizeof(unsigned char), n_rows_to_get*ns, info->fp);
        }
        else {
            // will have to figure this one out
            assert(!info->ml);

            // Here we have to read three separate strips of the
            // file to compose into the byte cache (which has interleaved
            // rgb values -- red vals coming from the first strip we read,
            // green from the second, and blues from the third.
            // So, we need a temporary buffer to place the values, so they
            // can be interleaved (i.e., we can't read directly into the
            // cache's memory)
            unsigned char *buf = MALLOC(sizeof(unsigned char)*ns);

            // first set the cache's memory to all zeros, this way any
            // rgb channels we don't populate will end up black
            memset(dest, 0, n_rows_to_get*ns*3);

            // red
            if (info->band_r >= 0) {
                int i,j,off = ns*(row_start + nl*info->band_r);
                for (i=0; i<n_rows_to_get; ++i) {
                    int k = 3*ns*i;
                    FSEEK64(info->fp, off + i*ns, SEEK_SET);
                    FREAD(buf, sizeof(unsigned char), ns, info->fp);
                    for (j=0; j<ns; ++j, k += 3)
                        dest[k] = buf[j];
                }
            }

            // green
            if (info->band_g >= 0) {
                int i,j,off = ns*(row_start + nl*info->band_g);
                for (i=0; i<n_rows_to_get; ++i) {
                    int k = 3*ns*i+1;
                    FSEEK64(info->fp, off + i*ns, SEEK_SET);
                    FREAD(buf, sizeof(unsigned char), ns, info->fp);
                    for (j=0; j<ns; ++j, k += 3)
                        dest[k] = buf[j];
                }
            }

            // blue
            if (info->band_b >= 0) {
                int i,j,off = ns*(row_start + nl*info->band_b);
                for (i=0; i<n_rows_to_get; ++i) {
                    int k = 3*ns*i+2;
                    FSEEK64(info->fp, off + i*ns, SEEK_SET);
                    FREAD(buf, sizeof(unsigned char), ns, info->fp);
                    for (j=0; j<ns; ++j, k += 3)
                        dest[k] = buf[j];
                }
            }

            free(buf);
        }
    } else {
        float *dest = (float*)dest_void;
        if (data_type==GREYSCALE_FLOAT) {
            // this is the normal case, just reading in a strip of lines
            // from the file directly into the floating point cache
            get_asf_lines(info, meta, row_start + nl*info->band_gs,
                          n_rows_to_get, dest);
        } else {
            // grabbing 3 channels floating point data
            assert(data_type==RGB_FLOAT);

            // first set the cache's memory to all zeros, this way any
            // rgb channels we don't populate will end up black
            memset(dest, 0, n_rows_to_get*ns*3*sizeof(float));
            float *buf = MALLOC(sizeof(float)*ns);
            int i,j,k;

            if (info->band_r >= 0) {
                for (i=0; i<n_rows_to_get; ++i) {
                    get_asf_line(info, meta, row_start + nl*info->band_r + i, buf);
                    for (j=0, k=3*ns*i; j<ns; ++j, k += 3)
                        dest[k] = buf[j];
                }
            }

            if (info->band_g >= 0) {
                for (i=0; i<n_rows_to_get; ++i) {
                    get_asf_line(info, meta, row_start + nl*info->band_g + i, buf);
                    for (j=0, k=3*ns*i+1; j<ns; ++j, k += 3)
                        dest[k] = buf[j];
                }
            }

            if (info->band_b >= 0) {
                for (i=0; i<n_rows_to_get; ++i) {
                    get_asf_line(info, meta, row_start + nl*info->band_b + i, buf);
                    for (j=0, k=3*ns*i+2; j<ns; ++j, k += 3)
                        dest[k] = buf[j];
                }
            }

            free(buf);
        }
    }

    return TRUE;
}

int get_asf_thumbnail_data(int thumb_size_x, int thumb_size_y,
                           meta_parameters *meta, void *read_client_info,
                           void *dest_void, int data_type)
{
    ReadAsfClientInfo *info = (ReadAsfClientInfo*) read_client_info;

    int sf = meta->general->line_count / thumb_size_y;
    //assert(sf==meta->general->sample_count / thumb_size_x);
    int i,j;

    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    // temporary storage
    float *buf = MALLOC(sizeof(float)*ns);

    if (meta->general->data_type == BYTE) {
        // BYTE case -- data file contains bytes.
        unsigned char *dest = (unsigned char*)dest_void;
        if (data_type == GREYSCALE_BYTE) {
            // data file contains byte data, and we are just pulling out
            // one band to display.
            int off = nl*info->band_gs;
            for (i=0; i<thumb_size_y; ++i) {
                get_asf_line(info, meta, i*sf + off, buf);
                for (j=0; j<thumb_size_x; ++j)
                    dest[i*thumb_size_x+j] = (unsigned char)(buf[j*sf]);
                asfPercentMeter((float)i/(thumb_size_y-1));
            }
        }
        else {
            // rgb case -- we have to read up to 3 bands from the file,
            // and put them together
            assert(data_type == RGB_BYTE);
            int off, k;

            // first set dest buffer to all zeros.  This way, any bands that
            // aren't set up will just come out black
            memset(dest, 0, 3*thumb_size_x*thumb_size_y);

            // "tot" is to help with the PercentMeter -- the total number
            // of lines that we will need to read.  "l" is the counter
            int tot = (info->band_r>=0) + (info->band_g>=0) + (info->band_b>=0);
            tot *= thumb_size_y;
            int l=0;

            // to do each of the bands, we read the data into a float array,
            // then cast (back) to byte into the interleaved "dest" array
            // (interleaved in the sense that we only populate every 3rd item
            // each time through)

            // red band
            if (info->band_r >= 0) {
                off = nl*info->band_r;
                for (i=0; i<thumb_size_y; ++i) {
                    k=3*i*thumb_size_x; // starting point in dest array
                    get_asf_line(info, meta, i*sf + off, buf);
                    for (j=0; j<thumb_size_x; ++j, k += 3)
                        dest[k] = (unsigned char)(buf[j*sf]);
                    asfPercentMeter((float)(l++)/(tot-1));
                }
            }

            // green band
            if (info->band_g >= 0) {
                off = nl*info->band_g;
                for (i=0; i<thumb_size_y; ++i) {
                    k=3*i*thumb_size_x+1;
                    get_asf_line(info, meta, i*sf + off, buf);
                    for (j=0; j<thumb_size_x; ++j, k += 3)
                        dest[k] = (unsigned char)(buf[j*sf]);
                    asfPercentMeter((float)(l++)/(tot-1));
                }
            }

            // blue band
            if (info->band_b >= 0) {
                off = nl*info->band_b;
                for (i=0; i<thumb_size_y; ++i) {
                    k=3*i*thumb_size_x+2;
                    get_asf_line(info, meta, i*sf + off, buf);
                    for (j=0; j<thumb_size_x; ++j, k += 3)
                        dest[k] = (unsigned char)(buf[j*sf]);
                    asfPercentMeter((float)(l++)/(tot-1));
                }
            }

            //assert(l==tot);
            if (l!=tot) printf("These are supposed to be equal: %d %d\n",
                l, tot);
        }
    } else {
        // this is the normal case -- regular old floating point data,
        // we just read with get_float_line and populate directly into
        // a floating point array
        float *dest = (float*)dest_void;
        if (data_type == GREYSCALE_FLOAT) {
            int off = nl*info->band_gs;
            for (i=0; i<thumb_size_y; ++i) {
                get_asf_line(info, meta, i*sf + off, buf);
                for (j=0; j<thumb_size_x; ++j)
                    dest[i*thumb_size_x+j] = buf[j*sf];
                asfPercentMeter((float)i/(thumb_size_y-1));
            }
        } else if (data_type == RGB_FLOAT) {
            int off, k;

            // first set dest buffer to all zeros.  This way, any bands that
            // aren't set up will just come out black.
            memset(dest, 0, 3*sizeof(float)*thumb_size_x*thumb_size_y);

            // "tot" is to help with the PercentMeter -- the total number
            // of lines that we will need to read.  "l" is the counter
            int tot = (info->band_r>=0) + (info->band_g>=0) + (info->band_b>=0);
            tot *= thumb_size_y;
            int l=0;

            // to do each of the bands, we read the data into a float array,
            // then cast (back) to byte into the interleaved "dest" array
            // (interleaved in the sense that we only populate every 3rd item
            // each time through)

            // red band
            if (info->band_r >= 0) {
                off = nl*info->band_r;
                for (i=0; i<thumb_size_y; ++i) {
                    k=3*i*thumb_size_x; // starting point in dest array
                    get_asf_line(info, meta, i*sf + off, buf);
                    for (j=0; j<thumb_size_x; ++j, k += 3)
                        dest[k] = buf[j*sf];
                    asfPercentMeter((float)(l++)/(tot-1));
                }
            }

            // green band
            if (info->band_g >= 0) {
                off = nl*info->band_g;
                for (i=0; i<thumb_size_y; ++i) {
                    k=3*i*thumb_size_x+1;
                    get_asf_line(info, meta, i*sf + off, buf);
                    for (j=0; j<thumb_size_x; ++j, k += 3)
                        dest[k] = buf[j*sf];
                    asfPercentMeter((float)(l++)/(tot-1));
                }
            }

            // blue band
            if (info->band_b >= 0) {
                off = nl*info->band_b;
                for (i=0; i<thumb_size_y; ++i) {
                    k=3*i*thumb_size_x+2;
                    get_asf_line(info, meta, i*sf + off, buf);
                    for (j=0; j<thumb_size_x; ++j, k += 3)
                        dest[k] = buf[j*sf];
                    asfPercentMeter((float)(l++)/(tot-1));
                }
            }

            //assert(l==tot);
            if (l!=tot) printf("These are supposed to be equal: %d %d\n",
                l, tot);
        } else {
            assert(FALSE);
        }
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

int open_asf_data(const char *filename, const char *band, int multilook,
                  meta_parameters *meta, ClientInterface *client)
{
    ReadAsfClientInfo *info = MALLOC(sizeof(ReadAsfClientInfo));

    info->is_rgb = FALSE;
    info->band_gs = info->band_r = info->band_g = info->band_b = 0;
    info->ml = multilook;

    // special hack for Avnir data!
    if (!band                                                   &&
        strcmp_case(meta->general->sensor_name, "AVNIR") == 0   &&
        meta->general->band_count >= 3)
    {
        // no band was specifed -- show true color (3,2,1)
        asfPrintStatus("Avnir data: defaulting to TRUE color -- "
                       "Red=3, Green=2, Blue=1\n");
        band = "03,02,01";
    }

    if (band) {
        char *r, *b, *g;
        if (split3(band, &r, &g, &b, ',')) {
            // Looks like we were given 3 bands -- so, we are doing rgb
            info->band_r = get_band_number(meta->general->bands,
                    meta->general->band_count, r);
            if (info->band_r < 0)
                asfPrintWarning("Red band '%s' not found.\n", r);
            else
                asfPrintStatus("Red band is band #%d: %s\n",
                    info->band_r+1, r);

            info->band_g = get_band_number(meta->general->bands,
                    meta->general->band_count, g);
            if (info->band_g < 0)
                asfPrintWarning("Green band '%s' not found.\n", g);
            else
                asfPrintStatus("Green band is band #%d: %s\n",
                    info->band_g+1, g);

            info->band_b = get_band_number(meta->general->bands,
                    meta->general->band_count, b);
            if (info->band_b < 0)
                asfPrintWarning("Blue band '%s' not found.\n", b);
            else
                asfPrintStatus("Blue band is band #%d: %s\n",
                    info->band_b+1, b);

            if (info->band_r < 0 && info->band_g < 0 && info->band_b < 0) {
                // none of the bands were found
                return FALSE;
            }

            info->is_rgb = TRUE;
            FREE(r); FREE(g); FREE(b);

            set_bands_rgb(info->band_r, info->band_g, info->band_b);
        } else {
            // Single band name given
            info->band_gs = get_band_number(meta->general->bands,
                    meta->general->band_count, (char*)band);
            if (info->band_gs < 0) {
                asfPrintWarning("Band '%s' not found.\n", band);
                return FALSE;
            } else {
                asfPrintStatus("Reading band #%d: %s\n",
                    info->band_gs+1, band);
            }

            set_bands_greyscale(info->band_gs);
        }
    }


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

    if (meta->general->data_type == BYTE)
        client->data_type = info->is_rgb ? RGB_BYTE : GREYSCALE_BYTE;
    else
        client->data_type = info->is_rgb ? RGB_FLOAT : GREYSCALE_FLOAT;

    return TRUE;
}
