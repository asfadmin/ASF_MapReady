/****************************************************************************
NAME:				EVAL

PURPOSE:  Evaluate various measures of correlation validity and extract a 
	  subarea of the cross correlation array centered on the peak

VERSION	 DATE	AUTHOR	  
-------	 ----	------	   
  1.0	 8/92	D. Steinwand 
  2.0    5/99   O. Lawlor-- Yanked out piles of dead & bad code.

REFERENCES:

1.0  LAS 4.0 GREYCORR and EDGECORR by R. White 6/83
*****************************************************************************/
#include "correlate.h"

void eval(int ncol, int nrow, /*Number of rows and columns in correlation array below*/
	float *corr,/*Image-Image correlation array*/
	int *peakLoc,/*Location (line, sample) of peak*/
	float *strength,/*Output: Strength of correlation*/
	float *hood,/*Output: 5x5 neighborhood of correlation peak*/
	int verbose)
{
	int x,y;
	int dx,dy;
	double corrSum=0,corrAve;
	int peakX=0,peakY=0;/*Best peak's location in corr array*/
	double peakMax=-200000000000000.0;/*Best peak's correlation strength*/
	
/*Run through correlation values, finding the average correlation value
as well as the best one so far*/
	for (y=0;y<nrow;y++)
		for (x=0;x<ncol;x++)
		{
			float corrVal=corr[y*ncol+x];
			corrSum+=corrVal;
			if (peakMax<corrVal)
			{
				peakMax=corrVal;
				peakX=x;
				peakY=y;
			}
		}

/*Find the average correlation value*/
	corrAve=corrSum/(nrow*ncol);
	
/*Find the strength of the best correlation by comparing it to the average*/
	*strength=peakMax/(corrAve+2.0);
	
	if (verbose)
	{/*Print out the peak*/
		printf("Peak at %d,%d in correlation image\n",peakX,peakY);
		printf("Correlation Average=%.3f, peak=%.3f\n",corrAve,peakMax);
		printf("Strength=%.3f\n",*strength);
		for (y=0;y<nrow;y++)
		{
			for (x=0;x<ncol;x++)
			{
				char *table=" .:;!v@$";
				int dex=(int)(7.5*(corr[y*ncol+x]-corrAve)/(peakMax-corrAve));
				if (dex<0) dex=0;
				printf("%c",table[dex]);
			}
			printf("\n");
		}
	}
	

/*Return the peak location*/
	peakLoc[1]=peakX;
	peakLoc[0]=peakY;
	
/*Extract the 5x5 'hood of the peak*/
	for (dy=0;dy<5;dy++)
		for (dx=0;dx<5;dx++)
		{
			y=(dy-2)+peakY;
			x=(dx-2)+peakX;
			if ((y<0)||(y>=nrow)||(x<0)||(x>=ncol))
				hood[dy*5+dx]=corrAve;/*out-of-bounds-- fill with average background*/
			else
				hood[dy*5+dx]=10.0*(corr[y*ncol+x]-corrAve)/(peakMax-corrAve);
		}

}
