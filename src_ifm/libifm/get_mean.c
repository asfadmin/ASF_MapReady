/****************************************************************
FUNCTION NAME:  get_mean()

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
	Return the avg value of ns*SCANLINES pixels taken near the center of
	the image.

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"


#include "ifm.h"

float get_mean(FILE *famp, int ns, int nl, int doAvg)
{
   float avg = AVG;
   float total = 0;
   float *ampIn;
   int i;
   long long offset;
   int pixelsRead;

   if (doAvg) {
     i = nl / 2;  /* find middle row of image */
     offset = i * ns * sizeof(float);
     pixelsRead = ns * SCANLINES;
     ampIn = (float *)MALLOC(sizeof(float)*pixelsRead);

     /* Read input data for this line */
     FSEEK64(famp,offset,0);
     fread(ampIn,sizeof(float),pixelsRead,famp);

     /* Establish pointers into input data arrays */
     for (i = 0; i < pixelsRead; i++)
     { 
        total += ampIn[i];
     }
     avg = total / (float)pixelsRead;
/*     printf(" Calculated values:\n total = %f\tavg = %f\n",total,avg);*/

     /* free up memory & reset files */
     free(ampIn);
     FSEEK64(famp,0,0);
   }
   return(avg);
}

