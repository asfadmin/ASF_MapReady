/*******************************************************************************
NAME:				DSTBND

PURPOSE:      Display the contents of the band record of the DDR file to 
	      the specified device.

PROGRAM HISTORY:
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Feb. 1988		original development
B. Ailts	      Jul. 1988         LAS 5.0 conversion
M. Shindle            June 1995         Remove TAE dependencies

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:	LAS

ALGORITHM DESCRIPTION:
Format the output strings for each of the BDDR members
If term
   write out each of the output strings to the terminal
If file or lp
   write out each of the output strings to the prtfile

ALGORITHM REFERENCES:  The format of the screens used in DSPDDR
		       are nearly identical to those used in EDITDDR.
*******************************************************************************/
#include "asf.h"
#include "asf.h"
#include "las.h"

#define TERM 0
#define PFILE 1

void mmval(int,char *);
void dstbnd(FILE*,int*,char*,int*,struct BDDR*);

void dstbnd(
            FILE *fp,           /* file pointer to print file     */
            int *print,         /* output destination             */
            char *name,         /* name of input image            */
            int *band,          /* pointer to current band number */
            struct BDDR  *bddr  /* source of data                 */
           )
{
	char blank[81];   /* blank line  */
	char line[8][81]; /* buffer line */
	char vald[23];

	/* set up screen for band information
	-------------------------------------*/
	sprintf(line[0],"\n******** image: %s  band: %-3d ********",name,*band);
	mmval(bddr->valid,vald);
	sprintf(line[1],
	    "        MINIMUM:%-2.14E                       %-15s",
	      bddr->minval,vald);
	sprintf(line[2],
	    "        MAXIMUM:%-2.14E                       %-15s",
	      bddr->maxval,vald);
	sprintf(line[3],"    DATA SOURCE:%-31s",bddr->source);
	sprintf(line[4],"    SENSOR TYPE:%-31s",bddr->instrument);
	sprintf(line[5],"CAPT. DIRECTION:%-63s",bddr->direction);
	sprintf(line[6],"           DATE:%-9s",bddr->date);
	sprintf(line[7],"           TIME:%-7s",bddr->time);

	/* set up for a blank line
	--------------------------*/
	strcpy(blank," ");
	
	/* output to terminal
	---------------------*/
	if (print[TERM]) {
	   printf("%s\n",line[0]);
	   printf("%s\n",blank);
	   printf("%s\n",line[1]);
	   printf("%s\n",line[2]);
	   printf("%s\n",blank);
	   printf("%s\n",line[3]);
	   printf("%s\n",line[4]);
	   printf("%s\n",line[5]);
	   printf("%s\n",line[6]);
	   printf("%s\n",line[7]);
	}

	/* output to print file
	-----------------------*/
	if ( print[PFILE] ) {
	   fprintf(fp,"%s\n",line[0]);
	   fprintf(fp,"%s\n",blank);
	   fprintf(fp,"%s\n",line[1]);
	   fprintf(fp,"%s\n",line[2]);
	   fprintf(fp,"%s\n",blank);
	   fprintf(fp,"%s\n",line[3]);
	   fprintf(fp,"%s\n",line[4]);
	   fprintf(fp,"%s\n",line[5]);
	   fprintf(fp,"%s\n",line[6]);
	   fprintf(fp,"%s\n",line[7]);
	}
	
	return;
}
	
/*******************************************************************************
NAME:				MMVAL

PURPOSE:      To create the correct string according to the value of 
	      the minmax valid flag

ALGORITHM DESCRIPTION:
Based on the value of input
   return the correct string

*******************************************************************************/
void mmval(
           int input, /* input validity flag numeric value */
           char str[] /* validity flag's character value   */
          )
{
	switch (input)
	   {
	   case INVAL: sprintf (str,"Valid:INVALID");
		    break;
	   case VALID: sprintf (str,"Valid:VALID");
		    break;
	   case BOUND: sprintf (str,"Valid:BOUNDED");
		    break;
	   default: sprintf (str,"Valid:UNACCEPTABLE");
	   } 
	return;
}
