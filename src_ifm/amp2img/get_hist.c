/*
   find the optimum range for DN compression
*/
#include "asf.h"

float calc_hist(FILE *fp, int n_cols, int n_rows, float avg)
{
    int    hist[256];
    int     i, j;
    int     n_tmp;
    float   *ibuff;
    float   accum, up_cut_off, n_total;
    float   mean=0, ratio=1, f_tmp, min, max;

    min = 100.0; 
    max = 0.0;

    ibuff = (float *)MALLOC(n_cols*sizeof(float));
    
    /* initialize the histogram array */
    for (i = 0; i < 256; i++) hist[i] = 0;

    /* get compressed histogram of the data */
    n_total = 0.;
    for (i = 0; i < n_rows; i+=1+n_rows/30)
      {
        FSEEK64 (fp, sizeof(float) * n_cols*i, 0);        
	if ((fread(ibuff, sizeof(float)*n_cols, 1, fp)) <= 0)
          { fprintf (stderr, "Read ends for input file\n"); break; }
        for (j = 0; j < n_cols; j++)
          {
            f_tmp = (float) (ibuff[j] / avg);
            f_tmp = fabs(f_tmp);

            if (f_tmp < min) min = f_tmp;
            if (f_tmp > max) max = f_tmp;
            
            n_tmp = (int) (f_tmp*128.0);
            if (n_tmp > 255) n_tmp = 255;
            hist[n_tmp] += 1;
          }
        n_total += (float) (n_cols);

        /* skip 9 lines */
      }
    if (!quietflag) {
      printf("   Using average value of %e over %f pixels\n",avg,n_total);
      printf("   Ratio of pixels to average - min: %f  max: %f\n",min,max);
      printf("   Histogram Values:\n  ");

      for (i = 0; i < 256; i++)
      {
        printf(" %6i ",hist[i]);
        if ((i+1) % 8 == 0 && i != 0) printf("\n  ");
      }
      printf("\n\n");
    }

    /* find the upper range under which 95 percent of data is covered */
    accum = 0.;
    up_cut_off = 0.05 * n_total;
    for (i = 255; i >= 0; i--)
      {
       accum += (float) hist[i];
       if (accum > up_cut_off)
         { ratio = (float) ((i+1)/256.0); mean = avg * ratio; break; }
      }

    if (!quietflag) {
      fprintf (stderr, "   Ratio of mean to avg = %e\n", ratio);
      fprintf (stderr, "   DN compression ratio = %e\n", mean);
    }
    free(ibuff);
    FSEEK64(fp,0,0);
    return(mean);
}


