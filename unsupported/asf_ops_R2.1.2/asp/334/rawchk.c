/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  rawchk_(freq,nlevel,ntotal,mean,std,hilev,lowlev,normal,xfit) -----

 ************** rawchk.c ***********************************
 *
 *  Raw Data Performance Check
 *
 *  date : 27 jan 1989
 * FORTRAN call:
 * rawchk(freq,nlevel,ntotal,mean,std,hilev,lowlev,normal,xfit)
 * C program call:
 * rawchk_(freq[0],&nlevel,&ntotal,&mean,&std,&hilev,&lowlev,
 *             &normal,xfit[0]);
 *
 * This subroutine will calculate the mean, the standard deviations,
 * the highest saturation level, the lowest saturation level, the
 * normality factor and the channels with high occupancy and
 * the associated occupancy levels.
 *
 * All the input parameters are int*4 and all the output parameters are
 * float*4.
 *
 * INPUT     freq (integer vector): integer array vector containing 
 *                  the histogram of raw data.
 *           nlevel (integer pointer): number bins in the histogram 
 *                        (can be up to 256).
 *           ntotal (integer pointer): number of sample in the 
 *                        histogram of raw data.
 *
 * OUTPUT    mean (float pointer): the average of raw data.
 *           std (float pointer):  standard deviation of raw data.
 *           hilev (float pointer): defined as the percentage of samples 
 *                  occupying the highest quantization level.
 *           lowlev (float pointer): defined as the percentage of 
 *                  samples occupying the lowest quantization level.
 *           normal (float pointer): defined as the channel normality 
 *                   factor which is
 *                   the ratio between the chi-square statistic 
 *                   obtained for a fit to the samples of a Gaussian
 *                   distribution and the chi-square value corresponding
 *                   to the fit being accepted at the five percent 
 *                   level of significance.
 *                   It is expected that normal should be less than or
 *                   equal to 1. This will indicate that the hypothesis
 *                   of normality has been accepted at the five percent
 *                   level of significance.
 *           xfit (float vecto): vector contains nlevel elements 
 *                 representing the
 *                 percentage fitting statistic. 
 *                 If the percentage fitting statistic is greater than
 *                 20%, the occupancy of that particular level is
 *                 significantly too high.
 *
 * Note that all the parameter are passed as the pointers ( to
 * be compatible with FORTRAN call).
 **************************************************************
 */

#include <stdio.h>
#include <math.h>

rawchk_(freq, nlevel, ntotal, mean, std, hilev, lowlev, normal,
        xfit) 
int  freq[1];
int  *nlevel, *ntotal;
float *mean, *std, *hilev, *lowlev, *normal;
float xfit[1];
{
   float xpredict[256];
   float xtemp1,xtemp2,arg1,arg2,normdis();
   float sum, sum2, xtemp, x005, pi2_sqrt, arg;
   float val[256];
   int  i;
   double pi2;

   pi2 = 4.0*atan(1.0); /* pi value */
   pi2_sqrt = sqrt(2.0*pi2); /* square root of 2.0*pi */

   x005 = 42.6 ; /* from Schaum's outline series
                    Probability and Statistics page 347 
                    assume 5 bit (32 bin)
                    degree of freedom is 29 (=32-3) 
                    using chi-value of 0.05(17.7)
                    42.6 for 0.95  */
         /* 7.82 for n=3 from Statistical methods p.134 */

   /* construct the vector val[0-255] */
   for(i=0; i < *nlevel; i++)
      val[i] = (float)i;
   /* calculate the mean value from the histogram freq */
   for( sum=0.0, sum2=0.0, i=0; i < *nlevel ; i++)
      { 
          xtemp = freq[i]*val[i];
          sum += xtemp;
          sum2 += xtemp*val[i]; 
      }
   *mean = sum/((float)*ntotal);
   *std  = sqrt( sum2/((float)*ntotal) - (*mean)*(*mean));
  
   /* calculate the highest saturation level and lowest saturation level*/
   *hilev = ((float)freq[*nlevel-1]) * 100.0 / ((float)*ntotal);
   *lowlev = ((float)freq[0]) * 100.0 / ((float)*ntotal);

   /* calculate predict frequency by the fitted Gaussian */
   if( (*std) != 0.0){
      for( i=0; i< *nlevel; i++){
      /* One technique to estimate the fitting curve
         xtemp = ( (float)i  - *mean)/ *std;
         arg = -0.5 * xtemp * xtemp ;
         xtemp = ((float)*ntotal)/( (*std)*pi2_sqrt) * exp(arg);
         xpredict[i] = (int) (xtemp+0.5);
       */
         arg2 = ( ((float)i) + 0.5 - *mean)/ *std;
         xtemp2 = normdis(arg2) * ((float)*ntotal) ;
         arg1 = ( ((float)i) - 0.5 - *mean)/ *std;
         xtemp1 = normdis(arg1) * ((float)*ntotal);
         xpredict[i] = (int)(xtemp2 - xtemp1 + 0.5);
      }
   }else{
      for(i=0; i< *nlevel; i++) xpredict[i] = 0.0;
   }       
   
   for( sum2=0.0, i=0; i < *nlevel; i++)
      {
          if( freq[i] == 0.0) {
	    /*
             printf(" **** WARNING **** : for bin=%d has ZERO elements\n",
                    i);
	    */
             xtemp = 0.0;
             xfit[i] = 0.0;
             }
          else {
             xtemp  = ((float)freq[i] - xpredict[i]);
             xfit[i] = 100.0* xtemp /((float)freq[i]);
             if( (i != 0) && (i != (*nlevel-1))) {
                sum2 += (xtemp*xtemp)/((float)freq[i]);
                }
             }
	/*
          if( xfit[i] > 20.0)
            { printf(" level=%f has the percentage fitting statistic=%f\n",
                     val[i],xfit[i]);
            }
	*/
      }
   *normal = sum2/x005;

} 


/*  float erfcc(x)   ----------------------

*/

float erfcc(x)
float x;
{
	float t,z,ans;

	z=fabs(x);
	t=1.0/(1.0+0.5*z);
	ans=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
		t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
		t*(-0.82215223+t*0.17087277)))))))));
	return  x >= 0.0 ? ans : 2.0-ans;
}

/*  float erfx(x)   --------------------------------

*/

float erfx(x)
float x;
{  float erfcc();
   return( 1.0 - erfcc(x));
}
 
/* float normdis(x)  -------------------------------

*/

float normdis(x)
float x;
{
   float erfx();
   return(0.5*(1.0 + erfx(x/sqrt(2.0))));
}
