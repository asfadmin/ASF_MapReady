#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "float_image.h"

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

                int right = j;
                while (right < ns-1 && get_pixel(i,right) < cutoff)
                    ++right;

                if (right-j > 1000) {
                    // huge giant hole... skip head to the end of it
                    // we will just leave it alone
                    set_pixel(i,j,cutoff-pad-get_pixel(i,j));
                    j = right;
                    continue;
                }

                int left = j;
                while (left > 0 && get_pixel(i,left) < cutoff)
                    --left;

                int up = i;
                while (up > 0 && get_pixel(up,j) < cutoff)
                    --up;

                int down = i;
                while (down < nl-1 && get_pixel(down,j) < cutoff)
                    ++down;

                // if hole is along the edge, do nothing
                if (left==0 || right==ns-1 || up==0 || down==nl-1) {
                  set_pixel(i,j,cutoff-pad-get_pixel(i,j));
                  continue;
                }

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

void interp_dem_holes_float_image(meta_parameters *meta, FloatImage *img,
                                  float cutoff, int verbose,
                                  int max_hole_width, double max_slope_angle)
{
#define pixel_at(y,x) float_image_get_pixel(img,y,x)

    nl = img->size_y;
    ns = img->size_x;

    int i, j, k;
    int count = 0;
    float pad = fabs(cutoff) + 100;

    double xps = meta->general->x_pixel_size;
    double yps = meta->general->y_pixel_size;

    if (verbose) {
      asfPrintStatus("Height cutoff is: %7.1f m\n", cutoff);
      if (max_slope_angle > 0) {
        asfPrintStatus("Maximum angle   : %7.1f degrees\n", max_slope_angle);
        if (xps != yps) {
          asfPrintStatus("                 (%8.2fm max change per pixel, x)\n",
                         fabs(xps*tan(D2R*max_slope_angle)));
          asfPrintStatus("                 (%8.2fm max change per pixel, y)\n",
                         fabs(yps*tan(D2R*max_slope_angle)));
        }
        else {
          asfPrintStatus("                 (%8.2fm max change per pixel)\n",
                         fabs(xps*tan(D2R*max_slope_angle)));
        }
      }
      else {
        asfPrintStatus("Interpolated values not slope-constrained.\n");
      }
      if (max_hole_width > 0)
        asfPrintStatus("Max hole size: %d pixels (width)\n",
                       max_hole_width);
    }

    max_slope_angle *= D2R;
    float max_dx = fabs(xps*tan(max_slope_angle));
    float max_dy = fabs(yps*tan(max_slope_angle));
    printf("max_dx = %f, max_dy = %f\n", max_dx, max_dy);

    if (verbose)
      asfPrintStatus("Scanning for bad pixels...\n");

    int n_bad_pixels = 0;
    for (k=0; k<2; ++k) {
      if (k==1) asfPrintStatus("Second pass...\n");
    for (i=1; i<nl-1; ++i) {
      for (j=1; j<ns-1; ++j) {
        float v = pixel_at(j,i);
        if (v>cutoff) {
          if (pixel_at(j-1,i)>cutoff && pixel_at(j-1,i)-v>max_dy) {
            //printf("(y-1) Bad pixel at: %d,%d (%f,%f)\n", i, j,
            //       pixel_at(j-1,i),v);
            float_image_set_pixel(img, j, i, cutoff-1);
            ++n_bad_pixels;
          }
          else if (pixel_at(j+1,i)>cutoff && pixel_at(j+1,i)-v>max_dy) {
            //printf("(y+1) Bad pixel at: %d,%d (%f,%f)\n", i, j,
            //       pixel_at(j+1,i),v);
            float_image_set_pixel(img, j, i, cutoff-1);
            ++n_bad_pixels;
          }
          else if (pixel_at(j,i+1)>cutoff && pixel_at(j,i+1)-v>max_dx) {
            //printf("(x+1) Bad pixel at: %d,%d (%f,%f)\n", i, j,
            //       pixel_at(j,i+1),v);
            float_image_set_pixel(img, j, i, cutoff-1);
            ++n_bad_pixels;
          }
          else if (pixel_at(j+1,i+1)>cutoff && pixel_at(j+1,i+1)-v>max_dx*1.4) {
            //printf("(x+1) Bad pixel at: %d,%d (%f,%f)\n", i, j,
            //       pixel_at(j,i+1),v);
            float_image_set_pixel(img, j, i, cutoff-1);
            ++n_bad_pixels;
          }
          else if (pixel_at(j,i-1)>cutoff && pixel_at(j,i-1)-v>max_dx) {
            //printf("(x-1) Bad pixel at: %d,%d (%f,%f)\n", i, j,
            //       pixel_at(j,i+1),v);
            float_image_set_pixel(img, j, i, cutoff-1);
            ++n_bad_pixels;
          }

          if (pixel_at(j,i-1)<cutoff && pixel_at(j,i+1)<cutoff &&
              pixel_at(j-1,i)<cutoff && pixel_at(j+1,i)<cutoff) {
            float_image_set_pixel(img, j, i, cutoff-1);
            ++n_bad_pixels;
          }
        }
      }
      asfLineMeter(i+1,nl);
    }
    }

    if (verbose && n_bad_pixels>0)
      asfPrintStatus("Found %d pixels in the input data which violate the "
                     "slope constraint.\n",
                     n_bad_pixels);
/*
    if (verbose) {
      asfPrintStatus("Creating byte mask of dem (with bad pixels removed).\n");
      asfPrintStatus("   [0 = No data, 255 = Valid Data]\n");

      data_type_t saved_dt = meta->general->data_type;
      meta->general->data_type=BYTE;

      FILE *ofp = fopenImage("good_data_mask.img", "wb");
      float *line = MALLOC(sizeof(float)*ns);

      for (i=0; i<nl; ++i) {
        for (j=0; j<ns; ++j)
          line[j] = pixel_at(j,i) < cutoff ? 0 : 255;
        put_float_line(ofp, meta, i, line);
        asfLineMeter(i,nl);
      }

      FREE(line);
      FCLOSE(ofp);

      meta_write(meta, "good_data_mask.meta");
      meta->general->data_type = saved_dt;      
    }
*/

    asfPrintStatus("Performing interpolations...\n");
    for (i=0; i<nl; ++i) {
      for (j=0; j<ns; ++j) {
        if (pixel_at(j,i) < cutoff) {
          
          // we found a hole
          // scan up/down/left/right to find nearest good data
          // then set this pixel to be a weighted average
          
          int right = j;
          while (right<ns-1 && pixel_at(right,i) < cutoff)
            ++right;

          if (max_hole_width>0 && right-j > max_hole_width) {
            // huge giant hole... skip head to the end of it
            // we will just leave it alone
            int m;
            for (m=j; m<right; ++m)
              float_image_set_pixel(img,m,i,cutoff-pad-pixel_at(m,i));
            j = right;
            continue;
          }

          int left = j;
          while (left>0 && pixel_at(left,i) < cutoff)
            --left;

          int up = i;
          while (up > 0 && pixel_at(j,up) < cutoff)
            --up;

          int down = i;
          while (down < nl-1 && pixel_at(j,down) < cutoff)
            ++down;

          float uv = pixel_at(j,up);
          float dv = pixel_at(j,down);
          float lv = pixel_at(left,i);
          float rv = pixel_at(right,i);

          // iterate until slope requirement is met...
          int n_iter=0;
          int ok=TRUE; // FALSE
/*
          while (n_iter < 10) {

            // if hole is along the edge, we're DOOMED
            if (left==0 || right==ns-1 || up==0 || down==nl-1)
              break;

            double horizontal_slope = atan2(lv - rv, (right-left)*xps);
            double vertical_slope = atan2(uv - dv, (down-up)*yps);

            if (fabs(horizontal_slope) > max_slope_angle ||
                fabs(vertical_slope) > max_slope_angle)
            {
              // which one is the outlier!?  we will do at most one rescan
              float avg = 0.25 * (uv + dv + lv + rv);
              float udiff = fabs(uv - avg);
              float ddiff = fabs(dv - avg);
              float ldiff = fabs(lv - avg);
              float rdiff = fabs(rv - avg);
              // require that an outlier a bit further out...
              float s = .9;
              if (udiff*s > ddiff && udiff*s > ldiff && udiff*s > rdiff) {
                // "up" value is the outlier
                --up;
                while (up > 0 && pixel_at(j,up) < cutoff)
                  --up;
                uv = pixel_at(j,up);
              }
              else if (ddiff*s > udiff && ddiff*s > ldiff && ddiff*s > rdiff) {
                // "down" is the outlier
                ++down;
                while (down < nl-1 && pixel_at(j,down) < cutoff)
                  ++down;
                dv = pixel_at(j,down);
                float_image_set_pixel(img,j,down,cutoff-1);
              }
              else if (ldiff*s > udiff && ldiff*s > ddiff && ldiff*s > rdiff) {
                // "left" is the outlier
                --left;
                while (left>0 && pixel_at(left,i) < cutoff)
                  --left;
                lv = pixel_at(left,i);
              }
              else if (rdiff*s > udiff && rdiff*s > ldiff && rdiff*s > ldiff) {
                // "right" is the outlier
                ++right;
                while (right<ns-1 && pixel_at(right,i) < cutoff)
                  ++right;
                rv = pixel_at(right,i);
                float_image_set_pixel(img,right,i,cutoff-1);
              }
              else {
                // no outlier...
                printf("No outlier found for: %d,%d (%.0f,%.0f,%.0f,%.0f => %.0f) (%.0f,%.0f,%.0f,%.0f)\n", 
                       i, j, uv, dv, lv, rv, avg, udiff, ddiff, ldiff, rdiff);
                break;
              }
            }
            else {
              // does not violate slope requirement!!
              ok=TRUE;
              break;
            }
            if (++n_iter > 10) {
              printf("Failed to find good data for: %d,%d\n", i, j);
              break;
            }
            else if (n_iter > 7) {
              printf("iter: %d at %d,%d\n", n_iter, i, j);
            }
          }
*/
          if (ok) {
            float interp_value;
            if (left==0 || right==ns-1 || up==0) {
              // leave pixel unchanged
              interp_value = pixel_at(j,i);
            }
            else if (down==nl-1) {
              float n = 1./(i-up) + 1./(j-left) + 1./(right-j);
              interp_value =
                uv * 1./(float)(i-up)/n +
                lv * 1./(float)(j-left)/n + rv * 1./(float)(right-j)/n;
            }
            else {
              float n = 1./(i-up) + 1./(down-i) + 1./(j-left) + 1./(right-j);
              interp_value =
                uv * 1./(float)(i-up)/n +   dv * 1./(float)(down-i)/n +
                lv * 1./(float)(j-left)/n + rv * 1./(float)(right-j)/n;
            }

            // HACK: save the pixel value in the incoming array
            // do this without screwing up the hole detection
            // by setting to a value below the cutoff using a kludgey
            // formula.  This way, we will still have the real hole
            // boundaries intact for more interpolations within this
            // hole          

            float_image_set_pixel(img, j, i, cutoff - pad - interp_value);
            ++count;
          }
          else {
            // could not figure out what to do, do not change pixel value
            float_image_set_pixel(img,j,i,cutoff-pad-pixel_at(j,i));
          }
/*
                asfPrintStatus("Hole at [%d,%d] %f\n",
                  i, j, float_image_get_pixel(img,j,i));
                asfPrintStatus("Up:    %2d %8.2f (w: %.2f)\n", i-up,
                  float_image_get_pixel(img,j,up), 1./(float)(i-up)/n);
                asfPrintStatus("Down:  %2d %8.2f (w: %.2f)\n", down-i,
                  float_image_get_pixel(img,j,down), 1./(float)(down-i)/n);
                asfPrintStatus("Left:  %2d %8.2f (w: %.2f)\n", j-left,
                  float_image_get_pixel(img,left,i), 1./(float)(j-left)/n);
                asfPrintStatus("Right: %2d %8.2f (w: %.2f)\n", right-j,
                  float_image_get_pixel(img,right,i), 1./(float)(right-j)/n);
                asfPrintStatus("Result --> %f (%f)\n\n", pixel_value,
                               cutoff-pad-pixel_value);
*/
        }
      }
      asfLineMeter(i,nl);
    }

    if (verbose) asfPrintStatus("Found %d hole pixels.\n", count);
    if (verbose) asfPrintStatus("Cleaning up...\n");

    for (i=0; i<nl; ++i) {
      for (j=0; j<ns; ++j) {
        float v = pixel_at(j,i);
        if (v < cutoff) {
          // undo the kludgey formula from the above "HACK"
          // to get the actual interpolated value in the DEM
          float_image_set_pixel(img, j, i, cutoff - pad - v);
          --count;
        }
      }
      if (verbose) asfLineMeter(i,nl);
    }
#undef pixel_at
}

void interp_dem_holes_file(const char *infile, const char *outfile,
                           float cutoff, int verbose,
                           int max_hole_width, double max_slope_angle)
{
    char *inFile = MALLOC(sizeof(char)*(strlen(infile)+10));
    char *outFile = MALLOC(sizeof(char)*(strlen(outfile)+10));

    create_name(inFile, infile, ".img");
    create_name(outFile, outfile, ".img");

    meta_parameters *meta = meta_read(inFile);
    if (verbose) asfPrintStatus("Tiling input image...\n");
    int oldQuietFlag = quietflag;
    if (!verbose) quietflag = TRUE;
    FloatImage *fi = float_image_new_from_metadata(meta, inFile);
    quietflag = oldQuietFlag;

    meta_write(meta, outFile);

    interp_dem_holes_float_image(meta, fi, cutoff, verbose, max_hole_width,
                                 max_slope_angle);
    meta_free(meta);
    
    float_image_store(fi, outFile, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
    float_image_free(fi);

    FREE(inFile);
    FREE(outFile);
}
