/* SccsId[]= @(#)overlay_32.c	2.41 3/24/98 */
static char sccsid_overlay_32[]= "@(#)PPoverlay_32.c:2.41";

#include "pp.h"

void overlay_32(int *image_buffer, int ix_st, int ir_st, double *scale)
{
  int header_offset, offset, i, j, k;
  int nx_total,ns_total,num1,num3;

  header_offset = 192/(2*sizeof(short));

  /* perform local overlay */
  ns_total = rng_in - plen*fs;
  nx_total = az_in+nadd;
  if(ir_st -1 + ns_total >= nfr) ns_total = nfr-ir_st+1;
  if(ix_st -1 + nx_total >= nfx) nx_total = nfx-ix_st+1;

  for(k = 0; k < nx_total; k++) {
    for(i = 0; i < ns_total; i++) {
      /*jump (ir_st-1+i)+(ix_st-1+1)*(nfr+header_offset) pixels.
	the first row is left blank. we substract 1 from ir_st 
	and ix_st because they start at one. 
      */
      offset = (ir_st-1+i)+(ix_st-1+1)*(nfr+header_offset);
      /* the first 4 bytes of buff_in contains the real component 
	 of the data and the second 4 bytes of buff_in contains the 
	 imaginary component of the data. we assign them to the first
	 2 byte and the second 2 bytes of an integer, so that we can
	 deal with real and imaginary components together.
       */
      ((short*)&num1)[0] = (short)(((float*)&buff_in[i+k*rng_in])[0]*
				   scale[ir_st-1+i]); 
      ((short*)&num1)[1] = (short)(((float*)&buff_in[i+k*rng_in])[1]*
				   scale[ir_st-1+i]); 
      num3 = image_buffer[header_offset+offset+k*(nfr+header_offset)];
      if(num3 == 0 && num1 != 0) { 
	image_buffer[header_offset+offset+k*(nfr+header_offset)] = num1;
      }
    }
  }

}

