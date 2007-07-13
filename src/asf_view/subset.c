#include "asf_view.h"

void save_subset(int what_to_save)
{
    if (g_poly.n > 0) {
        if (crosshair_line > 0 && crosshair_samp > 0) {
            // fixme: ask user name of the subset file
            char *out_filename = "subset.img";
            char *out_metaname = "subset.meta";

            // find the extent of the selected points in line/sample space
            int line_min = crosshair_line, line_max = crosshair_line;
            int samp_min = crosshair_samp, samp_max = crosshair_samp;

            int i,j;
            for (i=0; i<g_poly.n; ++i) {
                int l = g_poly.line[i];
                int s = g_poly.samp[i];

                if (l < line_min) line_min = l;
                if (l > line_max) line_max = l;

                if (s < samp_min) samp_min = s;
                if (s > samp_max) samp_max = s;
            }

            printf("Extent is line: %d-%d, samp: %d-%d\n",
                line_min, line_max, samp_min, samp_max);

            return;
            int ns = samp_max - samp_min + 1;
            int nl = line_max - line_min + 1;

            // generate the metadata
            printf("Generating %s...\n", out_metaname);
            meta_parameters *out_meta = meta_copy(meta);
            out_meta->general->line_count = nl;
            out_meta->general->sample_count = ns;
            out_meta->general->data_type = REAL32;
            out_meta->general->band_count = 1;
            strcpy(out_meta->general->bands, "");
            meta_write(out_meta, out_metaname);

            // generate the output line-by-line
            printf("Generating %s...\n", out_filename);
            FILE *outFp = fopenImage(out_filename, "wb");
            float *buf = MALLOC(sizeof(float)*ns);
            for (i=0; i<nl; ++i) {
                for (j=0; j<ns; ++j) {
                    if (what_to_save == PIXEL_VALUES) {
                        buf[j] = cached_image_get_pixel(data_ci,
                            line_min+i, samp_min+j);
                    } else if (what_to_save == INCIDENCE_ANGLES) {
                    } else if (what_to_save == LOOK_ANGLES) {
                    } else if (what_to_save == SLANT_RANGES) {
                    }
                }
                put_float_line(outFp, out_meta, i, buf);
                asfLineMeter(i,nl);
            }
            fclose(outFp);

            free(buf);
        } else {
            // shouldn't ever get in here...
            asfPrintWarning("Can't save subset: No crosshair.\n");
        }
    } else {
        // shouldn't ever get in here...
        asfPrintWarning("Can't save subset: Polygon not defined.\n");
    }
}

SIGNAL_CALLBACK void on_save_subset_button_clicked(GtkWidget *w)
{
    save_subset(0);
}
