/****************************************************************
FUNCTION NAME: image_resample - resample (downsize) an image 

SYNTAX:
image_resample(unsigned char *inbuf, int nl, int np, float factor,
               unsigned char *outbuf, int *return_nl, int *return_np)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    inbuf       unsigned char * Input image
    nl		int		input number of lines
    np		int		input number of samples
    factor	float		downsizing factor (ie. 3 means 1/3 size)
    outbuf      unsigned char * Output image (already allocated!!!)
    return_nl	int *		return number of lines
    return_np   int *		return number of pixels

DESCRIPTION:

    Resamples the input buffer by the given factor to create the output
    buffer.  Uses kernel averaging on a group of input pixels to get a
    single output pixel.

RETURN VALUE: 

    outbuf is filled with the resampled image
    return_nl is the number of lines in resampled image
    return_np is the number of samples in the resampled image

SPECIAL CONSIDERATIONS:

    outbuf storage needs to be allocated by the calling program!!!	

PROGRAM HISTORY:  Subroutine version of resample program (T. Logan 1/99)

****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

unsigned char filter(    /****************************************/
    unsigned char *inbuf, /* input image buffer                   */
    int    nl,            /* number of lines for inbuf            */
    int    ns,            /* number of samples per line for inbuf */
    int    x,             /* sample in desired line               */
    int    nsk);

image_resample(unsigned char *inbuf, int nl, int np, float factor,
               unsigned char *outbuf, int *return_nl, int *return_np)
{
    int      onp, onl,
	     nsk,                   /* kernel size in samples         */
             half,                  /* half of the kernel size        */
             n_lines,               /* number of lines in this kernel */
             s_line,                /* start line for input file      */
             xi = 0,                /* inbuf int x sample #           */
             yi = 0,                /* inbuf int y line #             */
             i,j;                   /* loop counters                  */
    float    scalfact,              /* scale factor                   */
             base,                  /* base sample/line               */
             rate;                  /* # input pixels/output pixel    */

    nsk = (int) factor;
    if (nsk%2 == 0) nsk++;              /* Must be odd sized kernel */
    scalfact = 1.0/factor;

    onp = np * scalfact;
    onl = nl * scalfact;
    base = 1.0 / (2.0 * scalfact) - 0.5;
    rate = 1.0 / scalfact;
    half = (nsk-1)/2;
    n_lines = nsk;

   /*--------  Process inbuf to give outbuf ------------------------*/
    for (i = 0; i < onl; i++)
      {
       /*--------- Determine next set of lines for kernel ---------------*/
       yi = i * rate + base + 0.5;
       s_line = yi-half;
       if (s_line < 0) s_line = 0;
       if (nl < nsk+s_line) n_lines = nl-s_line;

       /*--------- Produce the output line and write to disk -------*/
       for (j = 0; j < onp; j++)
         {
          xi = j * rate + base + 0.5;
          outbuf[i*onp+j] = filter(&inbuf[s_line*np],n_lines,np,xi,nsk);
         }
       /* if (i%40==0) printf(" Processing Output Line %i\n",i); */
      }

    *return_nl = onl;
    *return_np = onp;

    return(0);
}

/*******************************************************************
FUNCTION NAME:   filter - performs an equal weight filtering
 
DESCRIPTION:     Places filter of size nsk*nsk over point
                 x,y in inbuf.  Sums valid values (>0), and
                 divides by the number of valid samples found.
 
RETURN VALUE:    char value = average kernel value 
 
SPECIAL CONSIDERATIONS:
                 Assumption is made that the kernel is square
                 Does not perform mirroring at edges
 
PROGRAM HISTORY: Written 3/17/94 by Tom Logan (ASF)
		 Ported to Suns and modified for small buffers 4/95
*******************************************************************/
unsigned char filter(
                          /****************************************/
    unsigned char *inbuf, /* input image buffer                   */
    int    nl,            /* number of lines for inbuf            */
    int    ns,            /* number of samples per line for inbuf */
    int    x,             /* sample in desired line               */
    int    nsk)           /* number of samples in kernel          */
{                         /****************************************/
    float  kersum =0.0;                  /* sum of kernel       */
    int    half   =(nsk-1)/2,            /* half size kernel    */
           base   =(x-half),             /* index into inbuf    */
           total  =0,                    /* valid kernel values */ 
           i, j;                         /* loop counters       */
                                         /***********************/
    for (i = 0; i < nl; i++)
      {
       for (j = x-half; j <= x+half; j++)
         {
          if (inbuf[base] != 0 && j < ns)
           {
             kersum += (float) inbuf[base];
             total  += 1;
           }
          base++;
         }
       base += ns;
       base -= nsk;
      }
    if (total != 0) kersum /= (float) total;
    return ((unsigned char) (kersum + .5));
}
