/*******************************************************************************
NAME:				DSTIMG

PURPOSE:      Display the contents of the general DDR fields to the 
	      specified device.

PROGRAM HISTORY:
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Feb. 1988		original development
B. Ailts	      Jul. 1988         LAS 5.0 conversion
B. Ailts              Oct. 1989         Changed last accessed to last modified
M. Shindle	      June 1995         Removed TAE dependencies

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:	LAS

ALGORITHM DESCRIPTION:
The output strings are formated for each of the fields of the DDR
If terminal
   Print the strings to the screen
If lp or file
   Print the strings to the prtfile

ALGORITHM REFERENCES:  The format of the screens used in DSPDDR
		       are nearly identical to those used in EDITDDR.
*******************************************************************************/
#include "asf.h"

#include "las.h"
#include "cproj.h"

#define TERM 0
#define PFILE 1

/* Prototypes */
void pntval(int,char*);
void dstimg(FILE*,int*,char*,struct DDR*);


void dstimg(
		FILE *fp,       /* file pointer to print file          */
		int *print,     /* output destination                  */
		char *name,     /* name of input image                 */
		struct DDR *ddr /* pointer to integer data in record 1 */
           )
{
	int i;			
	char line[21][81];  /* buffer for line 1                     */
	char proj_str[45];  /* buffer for the projection string      */
	char vald[23];     /* buffer storage for validity flag value */
	char *endOfLine1;

	/* format lines for first screen
	--------------------------------*/
	sprintf(line[0],"  IMAGE NAME:%-60s",name);
	sprintf(line[1],
		"          NL:%-5d         NS:%-5d          NB:%-3d        DTYPE:",
		ddr->nl,ddr->ns,ddr->nbands);
	endOfLine1=&line[1][strlen(line[1])];
	switch (ddr->dtype)
	    {
	   case EBYTE: sprintf(endOfLine1,"BYTE");break;
	   case EWORD: sprintf(endOfLine1,"INTEGER*2");break;
	   case ELONG: sprintf(endOfLine1,"INTEGER*4");break;
	   case EREAL: sprintf(endOfLine1,"REAL*4");break;
	   case EDOUBLE: sprintf(endOfLine1,"REAL*8");break;
	   default: sprintf(endOfLine1,"%-d",ddr->dtype);break;
	   }
	sprintf(line[2],"  LAST MODIFIED:         DATE:%-11s  TIME:%-10sSYSTEM:%-11s",
		 ddr->last_used_date, ddr->last_used_time, ddr->system);
	pntval(ddr->valid[DDPCV],vald);
	c_prostr(&(ddr->proj_code),proj_str);
	sprintf(line[3],
	        "  %-9s%-*s%-*s%-15s","PROJ. CODE:",
	        strlen(proj_str),proj_str,(46 - strlen(proj_str))," ",vald);
	pntval(ddr->valid[DDZCV],vald);
	sprintf(line[4],
	    "   ZONE CODE:%-4d                                          %-15s",
	    ddr->zone_code,vald);
	pntval(ddr->valid[DDDCV],vald);
	sprintf(line[5],
	    "  DATUM CODE:%-2d                                            %-15s",
	    ddr->datum_code,vald);
	pntval(ddr->valid[DDPPV],vald);
	sprintf(line[6],
	    "  PROJ. PARM:                                              %-15s",vald);
	sprintf(line[7],
	    "  A:  % 1.14E   % 1.14E   % 1.14E",
	   ddr->proj_coef[0],ddr->proj_coef[1],ddr->proj_coef[2]);
	sprintf(line[8],
	    "  B:  % 1.14E   % 1.14E   % 1.14E",
	      ddr->proj_coef[3],ddr->proj_coef[4],ddr->proj_coef[5]);
	sprintf(line[9],
	    "  C:  % 1.14E   % 1.14E   % 1.14E",
	      ddr->proj_coef[6],ddr->proj_coef[7],ddr->proj_coef[8]);
	sprintf(line[10],
	    "  D:  % 1.14E   % 1.14E   % 1.14E",
	      ddr->proj_coef[9],ddr->proj_coef[10],ddr->proj_coef[11]);
	sprintf(line[11],
	    "  E:  % 1.14E   % 1.14E   % 1.14E",
	      ddr->proj_coef[12],ddr->proj_coef[13],ddr->proj_coef[14]);
	pntval(ddr->valid[DDCCV],vald);
	sprintf(line[12],
	    " CORNER COOR:                                              %-15s",vald);
	sprintf(line[13],"    ULcorner:% -1.14E   % -1.14E",
		ddr->upleft[0],ddr->upleft[1]);
	sprintf(line[14],"    URcorner:% -1.14E   % -1.14E",
		ddr->upright[0],ddr->upright[1]);
	sprintf(line[15],"    LLcorner:% -1.14E   % -1.14E",
		ddr->loleft[0],ddr->loleft[1]);
	sprintf(line[16],"    LRcorner:% -1.14E   % -1.14E",
		ddr->loright[0],ddr->loright[1]);
	pntval(ddr->valid[DDPDV],vald);
	sprintf(line[17],
	    "  PROJ. DIST:% -1.14E   % 1.14E   %-15s",
	      ddr->pdist_y,ddr->pdist_x,vald);
	pntval(ddr->valid[DDPUV],vald);
	sprintf(line[18],
	    " PROJ. UNITS: %-10s                                     %-15s",
	      ddr->proj_units,vald);
	pntval(ddr->valid[DDINCV],vald);
	sprintf(line[19],
	    "   INCREMENT:% -1.14E   % 1.14E   %-15s",
	      ddr->line_inc,ddr->sample_inc,vald);
	sprintf(line[20],
	    " MASTER COOR:%-5d   %-5d",
	      ddr->master_line,ddr->master_sample);

	/* output to terminal
	---------------------*/
	if (print[TERM]) {
	   for (i=0; i< 21; i++)
	     printf("%s\n",line[i]);
	}
   
	/* output to print file
	-----------------------*/
	if ( print[PFILE] ) {
	   for (i=0; i<21; i++)
	     fprintf(fp,"%s\n",line[i]);
	}

	return;
}

void pntval(
		int input,  /* input validity flag numeric value  */
		char str[]  /* validity flag's character value    */
           )
{
	switch (input)
	   {
	   case INVAL: sprintf (str,"Valid:INVALID");
		    break;
	   case VALID: sprintf (str,"Valid:VALID");
		    break;
	   case UNKNOW: sprintf (str,"Valid:UNKNOWN");
		    break;
	   default: sprintf (str,"Valid:UNACCEPTABLE");
	   } 
	return;
}
