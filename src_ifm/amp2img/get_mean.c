/****************************************************************
FUNCTION NAME:  get_mean()

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
	Return the avg value of several well-distributed scan lines of
	the image.

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
	11/97 O. Lawlor-- checked for 0.00000, for IFM corrected
		amplitude images.

****************************************************************/
#include "asf.h"
#include "ifm.h"

float get_mean(FILE *famp, int ns, int nl, int doAvg)
{
   double avg = AVG;
   double total = 0;
   int npix=0;
   float *ampIn;
   int y;
   long long offset;
   int pixelsRead;

   if (doAvg) {
     int yCount,x;
#define yCountMax 20
     pixelsRead = ns;
     ampIn = (float *)MALLOC(sizeof(float)*pixelsRead);

     for (yCount=0;yCount<yCountMax;yCount++)
     {
     	y = 1+yCount*(nl-2)/(yCountMax-1);  /* find row in image */
     	offset = y * ns * sizeof(float);

     	/* Read input data for this line */
     	FSEEK64(famp,offset,0);
     	fread(ampIn,sizeof(float),pixelsRead,famp);

    	 /* Establish pointers into input data arrays */
     	for (x = 0; x < pixelsRead; x++)
           if (ampIn[x]!=0.0000)
     	   {
		total += ampIn[x];
		npix++;
     	   }
     }
     avg = total / ((float)npix);
     if (!quietflag) printf("   Calculated values:\n   Total = %e\tAvg = %e\n",total,avg);

     /* free up memory & reset files */
     free(ampIn);
     FSEEK64(famp,0,0);
   }
   return(avg);
}

