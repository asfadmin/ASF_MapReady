/****************************************************************
FUNCTION NAME:  get_meand()

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

double get_meand(FILE *famp, int ns, int nl, int doAvg)
{
   double avg = AVG;
   double total = 0;
   double *ampIn;
   int i;
   long long offset;
   int pixelsRead;

   if (doAvg) {
     i = nl / 2;  /* find middle row of image */
     offset = i * ns * sizeof(double);
     pixelsRead = ns * SCANLINES;
     ampIn = (double *)MALLOC(sizeof(double)*pixelsRead);

     /* Read input data for this line */
     FSEEK64(famp,offset,0);
     fread(ampIn,sizeof(double),pixelsRead,famp);

     /* Establish pointers into input data arrays */
     for (i = 0; i < pixelsRead; i++)
     { 
        total += ampIn[i];
     }
     avg = total / (double)pixelsRead;
     printf(" Calculated values:\n total = %f\tavg = %f\n",total,avg);

     /* free up memory & reset files */
     free(ampIn);
     FSEEK64(famp,0,0);
   }
   return(avg);
}

