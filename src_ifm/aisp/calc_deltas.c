/******************************************************************************
NAME: calc_deltas

SYNOPSIS: 

   calc_deltas [-log <file>] <linePatch1> <linePatchL> <numLines> <output_deltas>

DESCRIPTION:
	Calc_deltas uses the two input lines, generally created
by fit_line(1) on the output of fico(1), to generate a deltas
file for use with aisp(1).  To see this program in action, check
out register_ccsd(1).

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    3/97   T. Logan     For use with aisp(1) interferometry.
    1.01    7/01   R. Gens	Added logfile switch
    1.2     6/03   P. Denny     Update command line parsing

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   calc_deltas -- Uses two input lines to generate a deltas file for use   *
*		   with aisp(1).					    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
#include "asf.h"

#define VERSION 1.2

int main(int argc,char *argv[])
{
   char inName1[256], inName2[256], outName[256];
   FILE *fp;
   float nLines;
   float mx1, bx1, my1, by1;
   float mx2, bx2, my2, by2;
   float delt_m_x, delt_b_x, delt_m_y, delt_b_y;

   /* parse command line */
   logflag=0;
   while (currArg < (argc-4)) {
	char *key = argv[currArg++];
	if (strmatch(key,"-log")) {
		CHECK_ARG(1)
		strcpy(logFile,GET_ARG(1));
		fLog = FOPEN(logFile, "a");
		logflag=1;
	}
	else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
   }
   if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}

   strcpy(inName1,argv[currArg]);
   strcpy(inName2,argv[currArg+1]);
   if (1!=sscanf(argv[currArg+2],"%f",&nLines)) {
     sprintf(errbuf,"   Error: '%s' is not a number of lines.\n",
             argv[currArg+2]);
     printErr(errbuf);
   }
   strcpy(outName,argv[currArg+3]);

   fp = FOPEN(inName1,"r"); 
   fscanf(fp,"%f %f\n",&mx1,&bx1);
   fscanf(fp,"%f %f\n",&my1,&by1);
   FCLOSE(fp);

   fp = FOPEN(inName2,"r"); 
   fscanf(fp,"%f %f\n",&mx2,&bx2);
   fscanf(fp,"%f %f\n",&my2,&by2);
   FCLOSE(fp);

   delt_m_x = (mx2 - mx1) / nLines;
   delt_b_x = (bx2 - bx1) / nLines;
   delt_m_y = (my2 - my1) / nLines;
   delt_b_y = (by2 - by1) / nLines;

   fp = FOPEN(outName,"w");
   fprintf(fp,"%f %f %f %f\n",mx1,bx1,my1,by1);
   fprintf(fp,"%g %g %g %g\n",delt_m_x, delt_b_x, delt_m_y, delt_b_y);
   FCLOSE(fp);

   system("date");
   printf("Program: calc_deltas\n\n");
   printf("   u1 = %f x + %f\n",mx1,bx1);
   printf("   v1 = %f y + %f\n",my1,by1);
   printf("   u8 = %f x + %f\n",mx2,bx2);
   printf("   v8 = %f y + %f\n\n",my2,by2);
   printf("   delu = %f x + %f\n",delt_m_x,delt_b_x);
   printf("   delv = %f y + %f\n\n",delt_m_y,delt_b_y);
   if (logflag) {
     StartWatchLog(fLog);
     printLog("Program: calc_deltas\n\n");
     sprintf(logbuf,"   u1 = %f x + %f\n",mx1,bx1);
     printLog(logbuf);
     sprintf(logbuf,"   v1 = %f y + %f\n",my1,by1);
     printLog(logbuf);
     sprintf(logbuf,"   u8 = %f x + %f\n",mx2,bx2);
     printLog(logbuf);
     sprintf(logbuf,"   v8 = %f y + %f\n\n",my2,by2);
     printLog(logbuf);
     sprintf(logbuf,"   delu = %f x + %f\n",delt_m_x,delt_b_x);
     printLog(logbuf);
     sprintf(logbuf,"   delv = %f y + %f\n\n",delt_m_y,delt_b_y);
     printLog(logbuf);
   }

   return 0;
}


void usage (char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>]\n"
	"               <linePatch1> <linePatchL> <numLines> <output_deltas>\n",
	name);
 printf("\n"
 	"REQUIRED ARGUMENTS:\n"
	"   linePatch1     Input - regressions for 1st patch\n"
	"   linePatchL     Input - regressions for last patch\n"
	"   numLines       Input - number of lines between first and last patch\n"
	"   output_deltas  Output file - linear regression\n");
 printf("\n"
	"OPTIONAL ARGUMENT:\n"
	"   -log <file>    Option to have output written to log file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Uses two input lines to generate a deltas file for use with aisp(1).\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
