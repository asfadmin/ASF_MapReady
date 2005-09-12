/****************************************************************************
NAME:				 LAMAZEQA 

PURPOSE:  Retrieves parameters for the LAMBER AZIMUTHAL EQUAL AREA projection

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
 NEWLAS		9/87	B. Ailts	   CSB	    original development
  5.0		1/89	D. Steinwand	   CSB      retrocode to 5.0
  6.0		11/98	M. Ayers	   ASF	    removed TAE dependencies
  6.2		 6/01	P. Denny	   ASF	    Error checking

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   PROJPRM must be run under TAE

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


#ifdef PROTO
void print_lamazeqa_usg(void);
#else
void print_lamazeqa_usg();
#endif

void lamazeqa(nargs,args,prjzone,prjtype,prjsph,prjparms)

int nargs;			/* number of remaining arguments*/
char **args;			/* remaining arguments*/
double *prjparms;		/* projection parameters */
int *prjzone;			/* projection zone */
int *prjtype;			/* projection type */
int *prjsph;			/* projection sphere */

{
double temcen[2];		/* center coordinates */
int status;			/* status of called subroutines */
char punits[4];			/* buffer for projection units */
char centmer[2][37];		/* center coordinates parameter */
char faleast[37];		/* false east parameter */
char falnorth[37];		/* false north parameter */
double radius;			/* radius of sphere */
int c;
int xflag=0, yflag=0, rflag=0;
extern char *optarg;


/*  Set the default values for certain parameters, 
including zone and projection type.
---------------------------------------------------*/

*prjzone = 62;
*prjtype = 11;
*prjsph = 0;
strcpy(punits, "DEG");


/* Check the remaining parameters 
--------------------------------------*/

while ((c=getopt(nargs,args,"g:x:y:r:e:n:")) != -1)
	switch (c) {
		case 'g':
			strcpy(punits,optarg);
			break;
		case 'x':
			strcpy(centmer[0], optarg);
			xflag = 1;
			break;
		case 'y':
			strcpy(centmer[1], optarg);
			yflag = 1;
			break;
		case 'r':
			radius = atof(optarg);
			rflag = 1;
			break;
		case 'e':
			strcpy(faleast, optarg);
			break;
		case 'n':
			strcpy(falnorth, optarg);
			break;
		default:
			print_lamazeqa_usg();
			
		}

if (!(xflag && yflag)) {
   c_errmsg("CENTER LATITUDE and CENTER LONGITUDE have to be specified",
	    "projprm-specify", NON_FATAL);
   print_lamazeqa_usg();
}

/*  Convert the parameters from strings to double precision values.  This
    done because TAE does not allow the input of double precision parameters.
-----------------------------------------------------------------------------*/

sscanf(centmer[0],"%lf",&temcen[0]);
sscanf(centmer[1],"%lf",&temcen[1]);
sscanf(faleast,"%lf",(prjparms + 6));
sscanf(falnorth,"%lf",(prjparms + 7));
*prjparms = rflag ? radius : 6370997;

/*  Convert the values to be placed in PRJPARMS to DMS format and check
    to be sure they are valid. 
-----------------------------------------------------------------------*/
status = c_degdms(&temcen[0],(prjparms + 5),punits,"LAT");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);

status = c_degdms(&temcen[1],(prjparms + 4),punits,"LON");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);
return;
}


void print_lamazeqa_usg() {
	fprintf(stderr, "Usage for Lambert Azimuthal Projection:\n\n");
	fprintf(stderr, "projprm <lamazeqa> <prjkey> <outfile> [-g geounits]\n");
	fprintf(stderr, "  [-x center coordinate (latitude)]	[-y center coordinate (longitude)]\n");
	fprintf(stderr, "  [-r radius]	[-e faleast]	[-n falnorth]\n");
	fprintf(stderr, "\nVersion %.2f, ASF STEP tools.\n", VERSION);
exit(1);

}
