/***************************************************************************
NAME:           projprm  -- creates a projection parameter table file

SYNOPSIS:       projprm projection prjkey outfile [options]

DESCRIPTION:    Creates a PROJ parameter labeled table file.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-----------------------------------------------------------------------
 NEWLAS		9/87	B. Ailts	   CSB	    original development
  5.0		1/89	D. Steinwand	   CSB      retrocode to 5.0
  5.1           8/89    D. Steinwand       CSB      Problem with Obl Mer B
  5.2		9/92	S. Nelson	   CSB	    Added map projections
						     Hammer and Robinson.
						     Changed parameters from
						     OPTION to PATHOPT and
						     PTHT to PNTHT.
  5.3	       10/92	D. Etrheim	   CSB	    If latitude is negitive,
						     make the UTM zone negative.
  7.0		5/95	T. Logan	   ASF	    Removed TAE dependencies
  7.1           6/95    M. Shindle         ASF      Added support for
						     plstereo, utm
  7.2           3/96    M. Shindle         ASF      Fixed bug in asf_tm.a
						     library.
  7.3		11/98   M. Ayers	   ASF      Added support for lamazeqa
						     Lambert Azimuthal Projection
  7.31		11/98	M. Ayers	   ASF      Added support for albers
						     Albers Equal Area Projection
  7.32		3/99	M. Ayers	   ASF	    Modified projections to trap
						     for inappropriate units
  7.4		6/01	P. Denny	   ASF	    Restricted units to meters
						     and seconds
  						    Added some error checking
  7.42		6/01	P. Denny	   ASF	    Automated selection of
						     projection units
						     More error checking
  7.5		7/01	R. Gens		   ASF	    Added log switch
  7.51		8/01    S. Watts	   ASF      Added -d option for UTM and
						     ALBERS projection.  Fixed
						     Albers to print smaxis to
						     file correctly.
  7.7		3/02	P. Denny	   ASF      Update commanline parsing
						     and usage()
   -            1/04    P. Denny        Removed GEOGRAPH, LAMAZEQA, and ALBERS
                                         projections because the current
                                         version of the .meta file without a
                                         ddr cannot handle them. Also note that
                                         our tools are made to work with LAMCC
                                         and **NOT** LAMAZEQA

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Parameters and subcommands are retrieved
	Parameters common to all subcommands are read
	Parameter values measured in angles are converted to degrees, minutes, 
	 seconds (DMS) as required by projection transformation package
	Parameters for the chosen subcommand are retrieved by calling the
	 appropriate routine
	Form a record of projection transformation package parameters
	Initialize the output labeled table file and write projection 
	 parameters to the file--creating a new file if needed, or writing 
	 to the end of an existing file.

ALGORITHM REFERENCES		
	Software Documentation for GCTP General Cartographic Transformation
	 Package, U.S. Geological Survey, National Mapping Division, May 1982.

	Software Documentation for GCTP Supplement 1, U.S. Geological Survey,
	 National Mapping Division, May 1982.

	Conversations with John P. Snyder, U.S. Geological Survey,
	 September, 1987.

	Snyder, J.P., Map Projections used by the U.S. Geological Survey:
	 U.S. Geological Survey Bulletin 1532, 1982.

	Snyder, J.P., Map Projections--A Working Manual:  U.S. Geological
	 Survey Professional Paper 1395, 1987.

BUGS:

*****************************************************************************/
/****************************************************************************
*								            *
*   projprm  -- creates a projection parameter table file		    *
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

#include <stdio.h>
#include <string.h>
#include "las.h"
#include "projprm.h"

float VERSION = 7.7;

int main(int argc,char **argv)
{ 
 double prjparms[15];		/* projection parameter array */
 int i;				/* string length */
 int prjsph;			/* projection sphere */
 int prjtype;			/* projection type */	
 int prjunits;			/* projection unit of measure */
 int prjzone;			/* projection zone */
 char hosout[CMLEN];		/* buffer for host output file name */
 char subcmd[20];		/* subcommand */
 char prjkey[9];		/* projection key */


 if (argc < 4)
	{printf("Insufficient arguments.\n"); usage(argv[0]);}

 system("date");
 printf("Program: projprm\n\n");
 logflag=0;
 i=1;
 currArg=1;	/* from cla.h which is in asf.h */

 /* Parse command line args */
 while ('-' == argv[currArg][0])
 {
	char *key=argv[currArg++];
	if (strmatch(key,"-log")) {
		CHECK_ARG(1); /*one string argument: log file */
		strcpy(logFile,GET_ARG(1));
		fLog = FOPEN(logFile,"a");
		logflag=1;
		StartWatchLog(fLog);
		printLog("Program: projprm\n\n");
	}
	else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
 }
 if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}

 strcpy(subcmd, argv[currArg++]);
 strcpy(prjkey, argv[currArg++]);
 strcpy(hosout, argv[currArg]);

 i = 20; c_low2up(subcmd,&i);
 /*****i = 9;  c_low2up(prjkey,&i);*****Case is same as on command line***/

 /*  Initialize the 15 projection parameters to zero.
 ----------------------------------------------------*/
 for (i=0; i < 15; prjparms[i++] = 0.0);

 /*  Call the correct subroutine based upon the subcommand that was called.
 --------------------------------------------------------------------------*/
 /*
 if (strcmp(subcmd,"GEOGRAPH") == 0){
     prjunits = 3; ** seconds **
     geograph(argc-currArg,&argv[currArg],&prjzone,&prjtype,&prjsph,prjparms);
 }
 else 
 */
 if (strcmp(subcmd,"PLSTEREO") == 0){
     prjunits = 2; /* meters */
     plstereo(argc-currArg,&argv[currArg],&prjzone,&prjtype,&prjsph,prjparms);
 } 
 else if (strcmp(subcmd,"UTM") == 0){
     prjunits = 2; /* meters */
     utm(argc-currArg,&argv[currArg],&prjzone,&prjtype,&prjsph,prjparms);
 }
 /*
 else if (strcmp(subcmd,"LAMAZEQA") == 0){
     prjunits = 2; ** meters **
     lamazeqa(argc-currArg,&argv[currArg],&prjzone,&prjtype,&prjsph,prjparms);
 }
 else if (strcmp(subcmd,"ALBERS") == 0){
     prjunits = 2; ** meters **
     albers(argc-currArg,&argv[currArg],&prjzone,&prjtype,&prjsph,prjparms);
 }
**
else if (strcmp(subcmd,"STATEPLN") == 0)
   statepln(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"ALBERS") == 0)
   albers(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"LAMCC") == 0)
   lamcc(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"MERCATOR") == 0)
   mercator(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"POLYCON") == 0)
   polycon(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"EDISCONA") == 0)
   ediscona(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"EDISCONB") == 0)
   edisconb(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"TM") == 0)
   tm(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"STEREO") == 0)
   stereo(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"LAMAZEQA") == 0)
   lamazeqa(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"AZIMEQD") == 0)
   azimeqd(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"GNOMONIC") == 0)
   gnomonic(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"ORTHO") == 0)
   ortho(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"GVNSP") == 0)
   gvnsp(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"SNSOIDAL") == 0)
   snsoidal(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"EQUIRECT") == 0)
   equirect(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"MILLCYL") == 0)
   millcyl(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"GRINTEN") == 0)
   grinten(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"OBLMERCA") == 0)
   oblmerca(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"OBLMERCB") == 0)
   oblmercb(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"SOMA") == 0)
   som(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"SOMB") == 0)
   somb(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"HAMMER") == 0)
   hammer(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"ROBINSON") == 0)
   robinson(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"MOLLWDE") == 0)
   mollwde(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"GOODE") == 0)
   goode(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"ALASKA") == 0)
   alaska(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"IMOLLWDE") == 0)
   imolwde(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"WAGIV") == 0)
   wagiv(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"WAGVII") == 0)
   wagvii(&parb,&prjzone,&prjtype,&prjsph,prjparms);
else if (strcmp(subcmd,"OBLEQ") == 0)
   obleq(&parb,&prjzone,&prjtype,&prjsph,prjparms);
*/

 else
 { 
   c_errmsg("   ERROR: This projection is not recognized","projprm-proj",NON_FATAL);
   c_errmsg("   ERROR: Fatal error encountered","projprm-fatal",LAS_FATAL);
 }

 /*  Write projection parameters to the output labeled table
 -----------------------------------------------------------*/
 wrlabtab(hosout,prjkey,&prjunits,&prjzone,&prjtype,&prjsph,prjparms);

 printf("   Projection=%s\n   Projection Key=%s\n\n",subcmd,prjkey);
 if (logflag) {
   sprintf(logbuf, "   Projection=%s\n   Projection Key=%s\n\n",subcmd,prjkey);
   printLog(logbuf);
   FCLOSE(fLog);
 }

 /*printf("\n Projection Parameter file %s completed.\n\n",hosout);*/

 exit(0);
} 


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <projection> <prjkey> <outfile> <parameters...>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
/*	"   projection   Designate a map style.  Can be: GEOGRAPH,\n"
	"                  PLSTEREO, UTM, LAMAZEQA, or ALBERS\n"
*/	"   projection   Designate a map style.  Can be: PLSTEREO or UTM\n"
	"   prjkey       Key sting to identify this projection in your\n"
	"                  projection file.\n"
	"   outfile      Output file name.\n"
	"   parameters   Projection specific parameters, see man page\n"
	"                  for details.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log <file>  Allows the output to be written to a log file\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Creates a projection parameter table file.\n");
 printf("\n"
	"Version %.2f, ASF SAR TOOLS\n"
	"\n",VERSION);
 exit(1);
}
