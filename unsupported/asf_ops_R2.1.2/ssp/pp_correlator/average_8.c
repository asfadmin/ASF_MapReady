/* SccsId[]= @(#)average_8.c	2.41 3/24/98 */
static char sccsid_average_8[]= "@(#)PPaverage_8.c:2.41";

void average_8(char *image_buffer,int nfx,int nfr,int ratio) 
{
  int i, j, m, n, ii, jj;
  float sum;
  for(j = 0; j <= nfr-ratio; j+=ratio) {
    n = j/ratio;
    for(i = 0; i <= nfx-ratio; i+=ratio) {
      m = i/ratio;
      sum = 0.0;
      for(ii = 0; ii < ratio; ii++) {
	for(jj = 0; jj < ratio; jj++) {
	  sum += image_buffer[(i+ii) + (j+jj)*nfx];
	}
      }
      sum /= ratio*ratio;
      image_buffer[m + n*nfx] = (int)sum;
    }
  }
}
