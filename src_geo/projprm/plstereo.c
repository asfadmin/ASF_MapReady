/****************************************************************************
NAME:				 PLSTEREO 

PURPOSE:  retrieves parameters for the POLAR STEREOGRAPHICL projection

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
 NEWLAS		9/87	B. Ailts	   CSB	    original development
  5.0		1/89	D. Steinwand	   CSB      retrocode to 5.0
  6.0           6/95    M. Shindle         ASF      Removed TAE dependencies

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Set the zone and projection type
	Retreive the needed parameters
	Convert the parameters from strings to double precision values.  This
	   done because TAE does not allow the input of double precision 		   parameters
	Convert the values to be placed in PRJPARMS to DMS format and check
	   to be sure they are valid. 
	Return

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
*****************************************************************************/
#include "asf.h"


#include "las.h"
#include "projprm.h"

void plstereo(nargs,args,prjzone,prjtype,prjsph,prjparms)
int nargs;                      /* # of remaining arguments */
char **args;	        	/* remaining arguments */
double *prjparms;		/* projection parameters */
int *prjzone;			/* projection zone */
int *prjtype;			/* projection type */
int *prjsph;			/* projection sphere */
{
double temsparl;		/* standard parallel */
double temlon;
int status;			/* status of called subroutines */
char punits[4];			/* buffer for projection units */
char smaxis[37];		/* semi-major axis parameter */
char ecsqval[37];		/* eccentricity squared */
char faleast[37];		/* false east parameter */
char falnorth[37];		/* false north parameter */
int c;
extern char *optarg;
int lflag = 0;
int pflag = 0;

/* set default values for certain parameters, 
   including zone & projection type. */
*prjzone = 62;
*prjtype = 6;
*prjsph = 0;
strcpy(punits,"DEG");
strcpy(smaxis, "0");
strcpy(faleast, "0");
strcpy(falnorth, "0");
ecsqval[0] = 0;

/* check remaining args */
while ((c=getopt(nargs,args,"g:d:s:q:e:n:l:p:")) != -1)
   switch (c) {
     case 'g':
       strcpy(punits,optarg);
       break;
     case 'd':
       *prjsph = atol(optarg);
       break;
     case 's':
       strcpy(smaxis,optarg);
       sscanf(smaxis,"%lf",prjparms);
       break;
     case 'q':
       strcpy(ecsqval,optarg);
       break;
     case 'e':
       strcpy(faleast,optarg);
       break;
     case 'n':
       strcpy(falnorth,optarg);
       break;
     case 'l':
       temlon = atof(optarg); 
       lflag++;
       break;
     case 'p':
       temsparl = atof(optarg);
       pflag++;
       break;
     default:
       print_plstereo_usg();
       break;
   }
if (!pflag || !lflag) {
   c_errmsg("STANDARD PARALLEL and LONGITUDE have to be specified",
	    "projprm-specify", NON_FATAL);
   print_plstereo_usg();
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);
} 

/*  Convert the parameters from strings to double precision values.  This
    done because TAE does not allow the input of double precision parameters.
-----------------------------------------------------------------------------*/
/*sscanf(lon,"%lf",&temlon);
sscanf(stdparl,"%lf",&temsparl);*/
sscanf(ecsqval,"%lf",(prjparms + 1));
sscanf(faleast,"%lf",(prjparms + 6));
sscanf(falnorth,"%lf",(prjparms + 7));

/*  Convert the values to be placed in PRJPARMS to DMS format and check
    to be sure they are valid. 
-----------------------------------------------------------------------*/
status = c_degdms(&temsparl,(prjparms + 5),punits,"LAT");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);

status = c_degdms(&temlon,(prjparms + 4),punits,"LON");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);
return;
}

void print_plstereo_usg() {
 fprintf(stderr,"Usage for polar stereographic projection:\n\n");
 fprintf(stderr,"projprm <plstereo> <prjkey> <outfile> [-g geounits]\n");
 fprintf(stderr,"  [-d datum] [-s smajoraxis] [-q ecsqval] [-e faleast]\n");
 fprintf(stderr,"  [-n falnorth] -l longitude -p stdparl\n");
 fprintf(stderr,"\nVersion %.2f, ASF SAR TOOLS.\n",VERSION);
 exit(1);
}
