/*
	ok so the point of these functions is to find the largest n^2
        squares within an image with no more than certain % overlay of mask.
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "asf.h"
#include "asf_sar.h"

static const int verbose = FALSE;

static void seed_points(int *x, int *y, int ns, int nl)
{   // generates new random location seed points to start from
    *x = ns * (double)rand() / ((double)(RAND_MAX) + 1.0);
    *y = nl * (double)rand() / ((double)(RAND_MAX) + 1.0);
}

static 
void check_square(int size,int x_pos, int y_pos, float  *mask,
                  int ns, int nl, long *total_pixels, float *good_pct)
{ 
    // go to the mask and work out the number of good and bad pixels 
    // within a square
    long loc,pmp,gmp;
    int TLx,TRx,TLy,BLy;
    int x,y;
    TLx = x_pos - size;
    TRx = x_pos + size;
    TLy = y_pos - size;
    BLy = y_pos + size;
    
    if (TLx < 0) TLx = 0; if (TLx >= ns) TLx = ns-1;
    if (TRx < 0) TRx = 0; if (TRx >= ns) TRx = ns-1;
    
    if (TLy < 0) TLy = 0; if (TLy >= nl) TLy = nl-1;
    if (BLy < 0) BLy = 0; if (BLy >= nl) BLy = nl-1;
    
    pmp = 0; gmp = 0; // set the counters to zero

    for(y=TLy; y < BLy; ++y)
        for(x=TLx; x < TRx; ++x)
        {   // step through each pixel and find out how good it is
            loc = y*ns + x;
            //printf("  mask[%d,%d] = %f\n", x,y, mask[loc]);
            if (is_masked(mask[loc]))
            {   // masked pixel
                pmp++;
            }
            else
            {   // good pixel
                gmp++;
            }
        }

    *total_pixels = gmp + pmp;
    *good_pct = ((float)gmp)/(*total_pixels);

    if (verbose)
      printf("  In samp:%d-%d, line:%d-%d, total: %ld, pct: %.1f%%\n", 
             TLx, TRx, TLy, BLy, *total_pixels, *good_pct*100);
}

static int grab_more(int size, int nl, int ns)
{
    if (size < 300)
        return size * 2;
    else if (size < 1000)
        return size + 100;
    else if (size < 10000)
        return size + 1000;
    else
        return size + 2000;
}

// puts the seeds onto the mask and see if they can grow
int lay_seeds(int num_seeds, float *mask, long ns, long nl,
              int *x_tl_list, int *y_tl_list, 
              int *x_br_list, int *y_br_list,
              float *good_pct_list)
{
    int ii,seed;

    //seed = (int)time(0);	/* choose a seed value */
    seed = 42;                  /* choose a seed value */
    srand(seed);		/* initialize random number generator */

    asfPrintStatus("Searching for suitable seed points...\n");
    if (verbose)
      asfPrintStatus("Image size: %dx%d LxS\n", nl, ns);

    // Minimum size we require for the seed regions.  This is an emprical
    // number, based on how big we're required to be to have a chance at
    // a reasonable fft match.
    const int size_cutoff = 256; 

    // Percentage of pixels in a square that we require to be "good" --
    // i.e., actual data (not masked).
    const float good_cutoff = 0.8;

    // image not big enough?
    if (size_cutoff*size_cutoff > nl*ns) 
        return 1;

    const long max_size = ((long)nl-1)*((long)ns-1);

    long pixels_cutoff = size_cutoff*size_cutoff;
    asfPrintStatus("Seed region size cutoff: %d pixels (%dx%d).\n",
                   pixels_cutoff, size_cutoff, size_cutoff);
    asfRequire(pixels_cutoff <= max_size, "Impossible!\n");

    // initialize the lists
    for (ii=0; ii<num_seeds; ii++) {
        x_tl_list[ii] = y_tl_list[ii] = x_br_list[ii] = y_br_list[ii] = 0;
        good_pct_list[ii] = 0;
    }

    for (ii=0; ii<num_seeds; ii++)
    {
        // note that "size" here is really half of a side
        int size = 4;

        // determine an initial point, it is ok if it isn't good
        int center_x, center_y;
        seed_points(&center_x, &center_y, ns, nl);
        if (verbose)
          printf("Initial point: Line: %d, Sample: %d\n", center_y, center_x);

        int n_try = 0;
        long prev_total_pixels = 0;
        float prev_good = 0;
        while (TRUE)
        {
            long total_pixels;
            float good;

            // check a larger square
            int next_size = grab_more(size, nl, ns);
            check_square(next_size, center_x, center_y, mask, ns, nl,
                         &total_pixels, &good);

            if (verbose)
              printf("Seed %d -- ratio of %f and pixels %ld  size %d \n",
                     ii+1, good, total_pixels, next_size);

                                           // if we ...
            if (good >= good_cutoff &&     // still have enough unmasked pixels
                total_pixels < max_size &&   // haven't gotten the entire image
                prev_total_pixels < total_pixels)       // still getting bigger
            {
                // this square meets the requirement
                // but be greedy -- try to get a bigger square
                size = next_size;
                prev_total_pixels = total_pixels;
                prev_good = good;
                if (verbose)
                  printf("  Enlarging...\n");
            }
            else
            {
                // this square does not meet the requirements
                // it is either because (i) we got too greedy--the previous
                // square did meet the requirements--or (ii) because we picked
                // a bad starting point.  In case (i) we can save the
                // previous square, in case (ii) we do not.
                if (verbose) {
                    if (good < good_cutoff) {
                        printf("  Too many masked pixels. prev_pixels=%ld, "
                               "pixels_cutoff=%ld\n",
                               prev_total_pixels, pixels_cutoff);
                    }
                    else {
                        printf("  Reached maximum size. prev_pixels=%ld, "
                               "pixels_cutoff=%ld\n",
                               prev_total_pixels, pixels_cutoff);
                    }
                }

                // this square didn't meet the requirement
                // if previous square did, and is large enough, we'll take it
                if (prev_total_pixels > pixels_cutoff)
                {
                    // We found a good region!  Save this info.

                    // convert from center-based to corner-based info
                    x_tl_list[ii] = center_x - size;
                    y_tl_list[ii] = center_y - size;

                    x_br_list[ii] = center_x + size;
                    y_br_list[ii] = center_y + size;

                    // clip to the image
                    if (x_tl_list[ii] < 0)  x_tl_list[ii] = 0;
                    if (y_tl_list[ii] < 0)  y_tl_list[ii] = 0;

                    if (x_br_list[ii] > ns) x_br_list[ii] = ns;
                    if (y_br_list[ii] > nl) y_br_list[ii] = nl;

                    long check_sz = (y_br_list[ii]-y_tl_list[ii])*
                                    (x_br_list[ii]-x_tl_list[ii]);

                    // check_square also discards pixels out-of-image, so
                    // the "good" pct correctly reflects the clipped area
                    good_pct_list[ii] = prev_good;

                    if (verbose)
                      printf("Found %d: %d %d %ld==%ld %d %f\n",
                             ii+1, center_x, center_y, 
                             prev_total_pixels, check_sz, size, prev_good);

                    //asfRequire(check_sz==prev_total_pixels, "Impossible!\n");
                    asfRequire(prev_good > good_cutoff, "Impossible!\n");

                    break;
                } 
                else
                {
                    // new seed point needed
                    if (++n_try > 2500) {
                        // too many failures -- return success (0) if we found
                        // at least one possible seed region, otherwise fail.
                        if (ii>0)
                            asfPrintStatus("Only found %d seed point%s.\n", 
                                           ii, ii==1?"":"s");
                        else
                            asfPrintStatus("Couldn't find any seed points!\n");

                        return ii==0;
                    }

                    size = 4;
                    seed_points(&center_x, &center_y, ns, nl);

                    prev_total_pixels = 0;
                    if (verbose) {
                      printf("New seed point needed.\n");
                      printf("Initial point: Line: %d, Sample: %d\n",
                             center_y, center_x);
                    }
                }
            }
        }
    }

    asfPrintStatus("Found %d seed point%s.\n", num_seeds, num_seeds==1?"":"s");
    return 0;
}
