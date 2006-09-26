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
                  int ns, int nl, long *masked_pixels, long *good_pixels)
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
    
    if (TLx < 0) TLx = 0; if (TLx > ns) TLx = ns-1;
    if (TRx < 0) TRx = 0; if (TRx > ns) TRx = ns-1;
    
    if (TLy < 0) TLy = 0; if (TLy > nl) TLy = nl-1;
    if (BLy < 0) BLy = 0; if (BLy > nl) BLy = nl-1;
    
    pmp = 0; gmp = 0; // set the counters to zero
    
    for(y=TLy; y <= BLy; ++y)
        for(x=TLx; x <= TRx; ++x)
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
    
    //printf(" -- size: %d: good %ld bad %ld \n",size,gmp,pmp);
    *masked_pixels = pmp;
    *good_pixels = gmp;
}

// puts the seeds onto the mask and see if they can grow
int lay_seeds(int num_seeds, float *mask, long ns, long nl,
              int *x_tl_list, int *y_tl_list, 
              int *x_br_list, int *y_br_list,
              float *good_pct_list)
{
    int ii,seed,size_cutoff;
    long masked_pixels, good_pixels, pixels_cutoff;
    const int size_min = 512;

    seed =(int)time(0);		/* choose a seed value */
    srand(seed);		/* initialize random number generator */

    asfPrintStatus("Searching for suitable seed points. "
                   "This may take some time.\n");
    
    size_cutoff = size_min; // minimum size we require for the seed regions
    if (size_cutoff*size_cutoff > nl*ns) // image not big enough?
        return 1;
    
    // Try to make the chips bigger, if we can.
    while (size_cutoff*size_cutoff < nl*ns/64.0)
        size_cutoff *= 2;
    
    pixels_cutoff = size_cutoff*size_cutoff;
    asfPrintStatus("Seed region size cutoff: %d pixels.\n",
                   pixels_cutoff);
    asfRequire(pixels_cutoff <= nl*ns, "Impossible!\n");

    for (ii=0; ii<num_seeds; ii++)
    {
        int n_attempt = 0;

        // note that "size" here is really half of a side
        int size = 4;

        // determine an initial point and go!
        int center_x, center_y;
        seed_points(&center_x, &center_y, ns, nl);

        while (TRUE)
        {
            if (++n_attempt > 50) {
                // 50 tries max before we give up
                return 1;
            }
            
            check_square(size, center_x, center_y, mask, ns, nl,
                         &masked_pixels, &good_pixels);

            long total_pixels = good_pixels + masked_pixels;
            float good = ((float)good_pixels)/total_pixels;

            //printf("Seed %d -- good %ld  bad %ld  .... ratio of %f "
            //       "and pixels %ld  size %d \n",
            //       ii,good_pixels,masked_pixels,good,total_pixels,size);

            if (good > 0.8)
            {
                if (total_pixels > pixels_cutoff) 
                {
                    // found a good region!  Save this info.

                    // convert from center-based to corner-based info
                    x_tl_list[ii] = center_x - size;
                    y_tl_list[ii] = center_y - size;

                    x_br_list[ii] = center_x + size;
                    y_br_list[ii] = center_y + size;

                    // clip to the image
                    if (x_tl_list[ii] < 0)  x_tl_list[ii] = 0;
                    if (y_tl_list[ii] < 0)  y_tl_list[ii] = 0;

                    if (x_br_list[ii] > ns) x_br_list[ii] = ns-1;
                    if (y_br_list[ii] > nl) y_br_list[ii] = nl-1;

                    // check_square also discards pixels out-of-image, so
                    // the "good" pct correctly reflects the clipped area
                    good_pct_list[ii] = good;

                    //printf("\n found %d \n",ii);
                    break;
                } else {
                    size*=2;
                }
            }
            else
            {
                // new seed point needed
                size = 4;
                seed_points(&center_x, &center_y, ns, nl);
                //printf(".");
                //printf(" pct=%f seed point quality failed trying again \n",
                //       good);
            }	 
        }
    }

    printf("Found all seed points \n");    
    return 0;
}
