/****************************************************************************
NAME:				grey_corr

PURPOSE:  Performs grey (cross) correlation for a tie point

VERSION		DATE	AUTHOR
-------		----	------	
  1.0	        8/92	D. Steinwand
  2.0	        5/99	O. Lawlor-- Yanked out lots of ugly & bad code.

*****************************************************************************/
#include "correlate.h"
#include "complex.h"
#include "asf_reporting.h"

/****************************************************************************
NAME:				GCORR

PURPOSE:  Correlate a reference subimage with a search subimage and evaluate 
	  the results

VERSION	 DATE	AUTHOR	  
-------	 ----	------	   
  1.0	 8/92	D. Steinwand 

REFERENCES:

1.  LAS 4.0 GREYCORR by R. White 6/83
*****************************************************************************/
void gcorr(
	float *seaFloat,		/* Search subimage 				     */
	float *refFloat,		/* Reference subimage 				     */
	int *seaSize,		/* Actual size of search subimage:  samps,lines      */
	int *refSize,		/* Actual size of reference subimage: samps,lines    */
	int mfit,		/* Surface Fit Method:  1 - Elliptical paraboloid 
					        2 - Elliptical Gaussian 
					        3 - Reciprocal Paraboloid 
					        4 - Round to nearest int     */
	float *nomoff,		/* Nominal horiz & vert offsets of UL corner of 
				   reference subimage relative to search subimage    */
	float *streng,		/* Strength of correlation 			     */
	float *bfoffs,		/* Best-fit line & sample offsets of correlation peak */
	float *tlerrs,		/* Est vert error, horiz error, and h-v cross term in 
					best-fit offsets 			     */
	int verbose /*Be verbose?*/
	)
{
int nrow,ncol;/*Number of rows and columns in correlation array*/
float corr[16641];	/* Unnormalized correlation sum for each alignment */
int peakLoc[2];/*Location of correlation peak*/
float cpval[25];	/* 5 by 5 array of xcorr values, in std dev 	     */
float pkoffs[2]={0,0};	/* Fractional horiz & vert offsets of best-fit peak  */

/* Compute raw cross-product sums
  ------------------------------*/
cross_prod_sums(seaFloat, refFloat, seaSize, refSize, corr,&nrow,&ncol);

/* Evaluate strength of correlation peak
  -------------------------------------*/
eval(ncol,nrow,corr,peakLoc,streng,cpval,verbose);

/* Determine offsets of peak relative to nominal location
  ------------------------------------------------------*/
fitreg(cpval, mfit, pkoffs, tlerrs);
if (verbose)
{
	asfPrintStatus("Peak offset=%.2f,%.2f\n",pkoffs[1],pkoffs[0]);
}
bfoffs[0] = peakLoc[0] - nomoff[0] + pkoffs[0];
bfoffs[1] = peakLoc[1] - nomoff[1] + pkoffs[1];
}

/****************************************************************************
NAME: CROSS_PROD_SUMS

PURPOSE:  Compute the unnormalized (raw) sum of pixel-by-pixel cross products 
	  between the reference and search seaFloat for every combination of 
          horizontal and vertical offsets of the reference relative to the 
          search image

VERSION	 DATE	AUTHOR	  
-------	 ----	------	   
  1.0	 8/92	D. Steinwand 

REFERENCES:

1.  LAS 4.0 GREYCORR by R. White & G. Neal 8/83
*****************************************************************************/
void cross_prod_sums(
	float *seaFloat,   /* Search subimage                         */
	float *refFloat,   /* Reference subimage                      */
	int *seaSize,      /* Size of search subimage: samps/lines    */
	int *refSize,      /* Size of reference subimage: samps/lines */
	float *corr,       /* Array of correlation values             */
	int *nrow,         /* Number of rows in correlation values    */
	int *ncol          /* Number of columns in correlation values */
                  )
{
    int ndxout;       /* Pointer into array for correlation output        */
    int memdim[2];    /* Power-of-2 dimensions for AP arrays(line/sample) */
    int m;            /* Power of 2 to use for FFT size                   */
    int i,x,y;        /* Loop counters                                    */
    COMPLEX *seaImg;  /* Complex search subimage (size memdim)            */
    COMPLEX *refImg;  /* Complex reference subimage (size memdim)         */
    float corrScale;  /* Scaling coefficient for correlations             */
    double searchSum; /* Sum of many pixels in search image; for computing average*/
    COMPLEX cBackground={128.0,0.0};/* Complex form of Background pixel value     */
    COMPLEX cZero={0.0,0.0};        /* Complex form of Zero                       */

    #if 0
    /*Write debugging images*/
    FILE *f;/*Debugging file*/
    #define DEBUG_IMG(name,array) \
	    f=fopen(name,"wb"); \
	    fwrite(array,sizeof(COMPLEX),memdim[0]*memdim[1],f);\
	    fclose(f);
    #else
    /*Don't write debugging images*/
    #define DEBUG_IMG(name,array) /*Empty define*/
    #endif


    /* Find next higher power of 2 above image dimentions
      --------------------------------------------------*/
    for(i=0;i<2;i++)
    {
	    memdim[i]=16;
	    m=4;
       while (memdim[i]<seaSize[i])
       {
	  memdim[i] *= 2;
	  m++;
       }
    }
    fft2dInit(m,m);

    seaImg=(COMPLEX *)MALLOC(sizeof(COMPLEX)*memdim[0]*memdim[1]);
    refImg=(COMPLEX *)MALLOC(sizeof(COMPLEX)*memdim[0]*memdim[1]);


    /*Find average value of search image, to set the background color*/
    searchSum=0;
    for (y=0;y<seaSize[0];y++)
	    for (x=0;x<seaSize[1];x++)
		    searchSum+=seaFloat[y*seaSize[1]+x];
    cBackground.re=searchSum/(seaSize[0]*seaSize[1]);

    /* Zero-extend reference image
      -----------------------------------------------*/
    for (y=0;y<refSize[0];y++)
    {
	    for (x=0;x<refSize[1];x++)
	    {
		    refImg[y*memdim[1]+x].re=refFloat[y*refSize[1]+x];
		    refImg[y*memdim[1]+x].im=0.0;
	    }
	    for (;x<memdim[1];x++)
		    refImg[y*memdim[1]+x]=cBackground;
    }
    for (;y<memdim[0];y++)
	    for (x=0;x<memdim[1];x++)
		    refImg[y*memdim[1]+x]=cBackground;

    /* Zero-extend search image
      -----------------------------------------------*/
    for (y=0;y<seaSize[0];y++)
    {
	    for (x=0;x<seaSize[1];x++)
	    {
		    seaImg[y*memdim[1]+x].re=seaFloat[y*seaSize[1]+x];
		    seaImg[y*memdim[1]+x].im=0.0;
	    }
	    for (;x<memdim[1];x++)
		    seaImg[y*memdim[1]+x]=cBackground;
    }
    for (;y<memdim[0];y++)
	    for (x=0;x<memdim[1];x++)
		    seaImg[y*memdim[1]+x]=cBackground;

    DEBUG_IMG("sea.img",seaImg);
    DEBUG_IMG("ref.img",refImg);


    /* Take fft of search and refrence data
      ------------------------------------*/
    fft2d((float *)seaImg,m,m);
    fft2d((float *)refImg,m,m);

    /* Multiply search fft by conjugate of reference fft
      --------------------*/
    for(i=0; i<memdim[0]*memdim[1]; i++)
       {
       float tempim = refImg[i].re*seaImg[i].im-refImg[i].im*seaImg[i].re;
       seaImg[i].re = refImg[i].re*seaImg[i].re+refImg[i].im*seaImg[i].im;
       seaImg[i].im = tempim;
       }

    #if 1
    /*Zero out low-freqency components of product.  This is critical
      to getting good correlation images.*/
    #define lowComponents (m-2) /*Spatial freqencies below this will be eliminated*/
    for (y=0;y<lowComponents;y++)
	    for (x=0;x<lowComponents;x++)
	    {
		    int cx=memdim[0]-1-x;
		    int cy=memdim[1]-1-y;
		    seaImg[y*memdim[1]+x]=cZero;
		    seaImg[y*memdim[1]+cx]=cZero;
		    seaImg[cy*memdim[1]+x]=cZero;
		    seaImg[cy*memdim[1]+cx]=cZero;
	    }
    #endif


    DEBUG_IMG("prefft.img",seaImg);

    /* Take inverse fft of product
      ------------------------*/
    ifft2d((float *)seaImg,m,m);

    DEBUG_IMG("postfft.img",seaImg);


    /* Extract part of correlation array which is valid
      ------------------------------------------------*/
    *ncol = (int)(seaSize[0] - refSize[0]);
    *nrow = (int)(seaSize[1] - refSize[1]);
    corrScale=1.0/(memdim[1]*memdim[0]);
    ndxout = 0;
    for(y=0;y<*nrow;y++)
       for(x=0;x<*ncol;x++)
       {
   	    int dex=y*memdim[1]+x;
   	    corr[ndxout++] =seaImg[dex].re*corrScale;
   	    /* sqrt(seaImg[dex].re*seaImg[dex].re+seaImg[dex].im*seaImg[dex].im)*corrScale;*/
       }

    FREE(seaImg);
    FREE(refImg);

}
