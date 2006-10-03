/*
	ok so the point of these functions is to find the largest n^2 squares within an image with no more than certain % overlay
	of mask.
	
	ideally squares should not overlap, but well worry about this later
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "asf.h"

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
    
    if (TLx < 0) TLx = 0; if (TLx > ns) TLx = ns;
    if (TRx < 0) TRx = 0; if (TRx > ns) TRx = ns;
    
    if (TLy < 0) TLy = 0; if (TLy > nl) TLy = nl;
    if (BLy < 0) BLy = 0; if (BLy > nl) BLy = nl;
    
    pmp = 0; gmp = 0; // set the counters to zero
    
    for(y=TLy; y < BLy; ++y)
        for(x=TLx; x < TRx; ++x)
        {   //  step through each pixel and find out how good it is
            loc = y*ns + x;
            //printf("mask[%d,%d] = %f\n", x,y, mask[loc]);
            if (mask[loc] != 0)
            {   // we have something masked_pixels
                pmp++;
            }
            else
            {
                gmp++;
            }
        }

    *total_pixels = gmp + pmp;
    *good_pct = ((float)gmp)/(*total_pixels);
}

static int grab_more(int size, int nl, int ns)
{
    if (size < 1000)
        return size * 2;
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
    long pixels_cutoff;

    seed = (int)time(0);	/* choose a seed value */
    srand(seed);		/* initialize random number generator */

    asfPrintStatus("Searching for suitable seed points...\n");
    
    // Minimum size we require for the seed regions.  This is an emprical
    // number, based on how big we're required to be to have a chance at
    // a reasonable fft match.
    const int size_cutoff = 512; 

    // Percentage of pixels in a square that we require to be "good" --
    // i.e., actual data (not masked).
    const float good_cutoff = 0.8;

    // image not big enough?
    if (size_cutoff*size_cutoff > nl*ns) 
        return 1;
    
    pixels_cutoff = size_cutoff*size_cutoff;
    asfPrintStatus("Seed region size cutoff: %d pixels.\n",
                   pixels_cutoff);
    asfRequire(pixels_cutoff <= nl*ns, "Impossible!\n");

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
            
            //printf("Seed %d -- ratio of %f and pixels %ld  size %d \n",
            //       ii+1, good, total_pixels, next_size);

            if (good >= good_cutoff && total_pixels < (long)nl*ns)
            {
                // this square meets the requirement
                // but be greedy -- try to get a bigger square
                size = next_size;
                prev_total_pixels = total_pixels;
                prev_good = good;
                //printf("      -- enlarging\n");
            }
            else
            {
                //printf("      -- didn't meet.  prev_pixels=%ld, "
                //       "pixels_cutoff=%ld\n",
                //       prev_total_pixels, pixels_cutoff);

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

                    //printf("Found %d: %d %d %ld==%ld %d %f\n",
                    //       ii+1, center_x, center_y, 
                    //       prev_total_pixels, check_sz, size, prev_good);

                    asfRequire(check_sz == prev_total_pixels, "Impossible!\n");
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
                            asfPrintStatus("Only found %d seed points.\n", ii);
                        else
                            asfPrintStatus("Couldn't find any seed points!\n");

                        return ii==0;
                    }

                    size = 4;
                    seed_points(&center_x, &center_y, ns, nl);
                    prev_total_pixels = 0;
                    //printf("New seed point needed.\n");
                }
            }
        }
    }

    asfPrintStatus("Found %d seed points \n", num_seeds);
    return 0;
}
