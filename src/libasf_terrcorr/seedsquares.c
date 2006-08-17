/*
	ok so the point of these functions is to find the largest n^2 squares within an image with no more than certain % overlay
	of mask.
	
	ideally squares should not overlap, but well worry about this later
	
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void seed_points(int *x, int *y, int ns, int nl)
{ // generates new random location seed points to start from

    *x = ns * (double)rand() / ((double)(RAND_MAX) + 1.0);
    *y = nl * (double)rand() / ((double)(RAND_MAX) + 1.0);
}

void check_square(int size,int x_pos, int y_pos, float  *mask, int ns, int nl, long *masked_pixels, long *good_pixels)
{ // go to the mask and work out the number of good and bad pixels within a square
	long loc;
	int TLx,BLx,TRx,BRx;
	int TLy,BLy,TRy,BRy;
	int x,y;
	long pmp;
        long gmp;
	TLx = x_pos - size;
	BLx = x_pos - size;
	TRx = x_pos + size;
	BRx = x_pos + size;
	TLy = y_pos - size;
	BLy = y_pos - size;
	TRy = y_pos + size;
	BRy = y_pos + size;
	
	if (TLx < 0) TLx = 0; if (TLx > ns) TLx = ns-1;
	if (TRx < 0) TRx = 0; if (TRx > ns) TRx = ns-1;
	if (BLx < 0) BLx = 0; if (BLx > ns) BLx = ns-1;
	if (BRx < 0) BRx = 0; if (BRx > ns) BRx = ns-1;
	
	if (TLy < 0) TLy = 0; if (TLy > nl) TLy = nl-1;
	if (TRy < 0) TRy = 0; if (TRy > nl) TRy = nl-1;
	if (BLy < 0) BLy = 0; if (BLy > nl) BLy = nl-1;
	if (BRy < 0) BRy = 0; if (BRy > nl) BRy = nl-1;
	
	

	pmp = 0; gmp = 0; // set the counters to zero
	
	for(y=TLy; y <= BLy; ++y)
		for(x=TLx; x <= TRx; ++x)
			{ //  step through each pixel and find out how good it is
			loc = (y*ns) + x;
			if (mask[loc] != 0)
				{ // we have something masked_pixels
					pmp++;
				}
				else
				{
					gmp++;
				}
			}
	
	// printf(" good %ld bad %ld \n",gmp,pmp);
	*masked_pixels = pmp;
	*good_pixels = gmp;
	return;
}

void lay_seeds(int num_seeds, float *mask,long ns, long nl, int *x_pos_list, int *y_pos_list, int *size_list, long *clipped_pixels)
		/* 
		int num_seeds, number of random seed points to generate
		 float *mask, the mask image
		long ns, the number of samples in the mask image
		int *x_pos_list,  the list of 1D x positions
		int *y_pos_list,  the list of 1D y positions 
		int *size_list, the size of each seed point achieves
		
		*/
{// puts the seeds onto the mask and see if they can grow
	int x;
	int size;
	long  masked_pixels, good_pixels;
	float good;
	int seed;
	int *pxl = x_pos_list;
	int *pyl = y_pos_list;
	int *psl = size_list;
	long *ppl = clipped_pixels;
	int leave;
	seed =(int)time(0);		/* choose a seed value */
	srand(seed);		/*initialize random number generator*/


	for (x=1;x<num_seeds; x++)
	{ // put the seed points onto the map
			seed_points(&pxl[x],&pyl[x], ns,nl);
			psl[x] = 4;
	}
	printf(" searching for suitable seed points, this may take some time \n ");
	for(x=1;x<num_seeds;x++)
	{
		size = 4;
		leave  = 0;
		while (leave == 0)
			{
				check_square(size,pxl[x],pyl[x], mask,ns,nl, &masked_pixels, &good_pixels);
				ppl[x] = (good_pixels + masked_pixels);
				good =((double) good_pixels )/((double) (good_pixels + masked_pixels));
			//	printf(" seed %d good %ld  bad %ld  .... ratio of %f and pixels %d  size %d \n "
			//					,x,good_pixels,masked_pixels,good,ppl[x],size);
				if ( good > 0.8)
					{
						psl[x] = size;
						if (size > 4096) 
						{
								leave=1;
								printf("\n found %d \n",x);
						}else{
								size=size*2;
						}
					}
				 else
					{
						if (size <= 512)
						{
						 size = 4;
						 seed_points(&pxl[x],&pyl[x], ns,nl);
						 printf(".");
				//		 printf(" seed point quality failed  trying again \n");
						} else
						{
							printf("\n found %d \n",x);
							leave = 1;
						}
				 	}	 
			}
			
	}
	printf(" found all seed points \n");
	for(x=1;x<num_seeds;++x)
		printf(" seed %d positions %d %d  and size  %ld (total pixels) \n ",x,pxl[x],pyl[x],ppl[x]);
	//*x_pos_list = pxl;
	//*y_pos_list = pyl;
	//*size_list = psl;
	//*clipped_pixels = ppl;
}
