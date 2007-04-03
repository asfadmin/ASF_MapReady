#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"

static int nl = -1;
static int ns = -1;
static float *data = NULL;

static float get_pixel(int i, int j)
{
    return data[i*ns + j];
}

static void set_pixel(int i, int j, float value)
{
    data[i*ns + j] = value;
}

void interp_dem_holes_file(const char *infile, const char *outfile,
                           float cutoff, int verbose)
{
    char *inFile = MALLOC(sizeof(char)*(strlen(infile)+10));
    char *outFile = MALLOC(sizeof(char)*(strlen(outfile)+10));

    create_name(inFile, infile, ".img");
    create_name(outFile, outfile, ".img");

    meta_parameters *meta = meta_read(inFile);
    nl = meta->general->line_count;
    ns = meta->general->sample_count;

    data = MALLOC(sizeof(float)*nl*ns);
    FILE *fp = fopenImage(inFile,"rb");
    
    int i;
    for (i=0; i<nl; ++i)
        get_float_line(fp, meta, i, &data[i*ns]);
    
    fclose(fp);
    
    interp_dem_holes_data(meta, data, cutoff, verbose);
    
    fp = fopenImage(outFile,"wb");
    for (i=0; i<nl; ++i)
        put_float_line(fp, meta, i, &data[i*ns]);
    fclose(fp);

    meta_write(meta, outFile);
    meta_free(meta);

    FREE(data);
    FREE(inFile);
    FREE(outFile);
}

void interp_dem_holes_data(meta_parameters *meta, float *dem_data,
                           float cutoff, int verbose)
{
    nl = meta->general->line_count;
    ns = meta->general->sample_count;
    data = dem_data;

    int i, j;
    int count = 0;
    float pad = fabs(cutoff) + 100;

    if (verbose) asfPrintStatus("Height cutoff is: %7.1f m\n", cutoff);
    if (verbose) asfPrintStatus("Performing interpolations...\n");
    for (i=0; i<nl; ++i) {
        for (j=0; j<ns; ++j) {
            if (get_pixel(i,j) < cutoff) {
                
                // we found a hole
                // scan up/down/left/right to find nearest good data
                // then set this pixel to be a weighted average

                int right = j+1;
                while (right < ns && get_pixel(i,right) < cutoff)
                    ++right;

                if (right-j > 250) {
                    // huge giant hole... skip head to the end of it
                    // we will just leave it alone
                    j = right;
                    continue;
                }

                int left = j-1;
                while (left >= 0 && get_pixel(i,left) < cutoff)
                    --left;

                int up = i-1;
                while (up >= 0 && get_pixel(up,j) < cutoff)
                    --up;

                int down = i+1;
                while (down < nl && get_pixel(down,j) < cutoff)
                    ++down;

                float n = (i-up) + (down-i) + (j-left) + (right-j);
                float pixel_value =
                    get_pixel(up,j) * (float)(i-up)/n +
                    get_pixel(down,j) * (float)(down-i)/n +
                    get_pixel(i,left) * (float)(j-left)/n +
                    get_pixel(i,right) * (float)(right-j)/n;

                /*
                asfPrintStatus("Hole at [%d,%d] %f\n", i, j, get_pixel(i,j));
                asfPrintStatus("Up: %d %f\n", up, get_pixel(up,j));
                asfPrintStatus("Down: %d %f\n", down, get_pixel(down,j));
                asfPrintStatus("Left: %d %f\n", left, get_pixel(i,left));
                asfPrintStatus("Right: %d %f\n", right, get_pixel(i, right));
                asfPrintStatus("Total Distance: %f\n", n);
                asfPrintStatus("Result --> %f (%f)\n\n", pixel_value,
                               cutoff-100-pixel_value);
                */

                // HACK: save the pixel value in the incoming array
                // do this without screwing up the hole detection
                // by setting to a value below the cutoff

                set_pixel(i, j, cutoff - pad - pixel_value);
                ++count;
            }
        }
        asfLineMeter(i,nl);
    }

    if (verbose) asfPrintStatus("Found %d hole pixels.\n", count);
    if (verbose) asfPrintStatus("Cleaning up...\n");

    for (i=0; i<nl; ++i) {
        for (j=0; j<ns; ++j) {
            float v = get_pixel(i,j);
            if (v < cutoff) {
                set_pixel(i, j, -v + cutoff - pad);
                --count;
            }
        }
        if (verbose) asfLineMeter(i,nl);
    }
}
