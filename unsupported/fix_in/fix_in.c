/******************************************************************************
NAME:  fix_in

SYNOPSIS:  fix_in <in par> <in/out .in file>

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0 
    1.1	    6/00    D.Koster    Modified to handle files > 2GB

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   fix_in								    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/


#include "asf.h"
#include "aisp_params.h"

#define VERSION 1.1

main(int argc,char *argv[])
{
   char parFile[255], inFile[255];
   char line[255];
   char param[255];
   FILE *fp;
   double ref=0.0;  /* Reference slant range */
   double prf=0.0;  /* Pulse Repetitiion Freq. */
   double a[4]={0.0,0.0,0.0,0.0};
   double sr=0.0;
   double rtmp, sum;
   int    j;
   struct AISP_PARAMS aisp;

   if (argc != 3)
	{
	  printf("\nUSAGE:  %s <in par> <in/out .in file>\n",argv[0]);
	  printf("Version %.2f, ASF SAR TOOLS\n\n", VERSION);
	  exit(1);
   	}

   strcpy(parFile, argv[1]);
   strcpy(inFile, argv[2]);

   fp = FOPEN(parFile,"r");

   while (ref == 0.0 && NULL!=fgets(line,255,fp))
     {
	sscanf(line,"%s",param);
	if (strncmp(param,"reference_first_dimension",25)==0)
	    sscanf(line,"%s %lf",param,&ref); 
     }
   printf("Reference slant range is %lf\n",ref);

   FSEEK64(fp,0,SEEK_SET);
   while (prf == 0.0 && NULL!=fgets(line,255,fp))
     {
	sscanf(line,"%s",param);
	if (strncmp(param,"pulse_repetition_frequency",26)==0)
	    sscanf(line,"%s %lf",param,&prf); 
     }
   printf("PRF is %lf\n",prf);

   FSEEK64(fp,0,SEEK_SET);
   while (a[0] == 0.0 && NULL!=fgets(line,255,fp))
     {
	sscanf(line,"%s",param);
	if (strncmp(param,"doppler_polynomial",18)==0)
	    sscanf(line,"%s %lf %lf %lf %lf",param,&a[0],&a[1],&a[2],&a[3]); 
     }
   printf("Doppler polynomial is %lf %lf %lf %lf\n",a[0],a[1],a[2],a[3]);

   FSEEK64(fp,0,SEEK_SET);
   while (sr == 0.0 && NULL!=fgets(line,255,fp))
     {
	sscanf(line,"%s",param);
	if (strncmp(param,"center_range_raw",16)==0)
	    sscanf(line,"%s %lf",param,&sr); 
     }
   printf("Center slant range is %lf\n",sr);

   rtmp = sr - ref;
   sum = 0.0;
   for (j=0; j<4; j++) sum += a[j] * pow(rtmp,(double)j);
   
   printf("Calculated doppler centroid is %lf\n",sum);
   
   /* Now put into .in file */ 
   read_params(inFile,&aisp);

   aisp.fd = sum / prf;
   aisp.fdd = 0.0;
   aisp.fddd = 0.0;

   print_params(inFile,&aisp,"fix_in");

}
