/*****************************************************************************
NAME:			ALBERS

PURPOSE:  Retrieves parameters for the ALBERS CONICAL EQUAL AREA projection.

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
 NEWLAS		9/87	B. Ailts	   CSB	    original development
  5.0		1/89	D. Steinwand	   CSB      retrocode to 5.0
  6.0		11/98	M. Ayers	   ASF	    removed TAE dependencies
  6.2		 6/01	P. Denny	   ASF	    Error checking
  6.21           8/01   S. Watts	   ASF      -d option w/ error checking.  						    Fixed so semi major axis 
						    printed correctly to file.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

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

void albers(nargs,args,prjzone,prjtype,prjsph,prjparms)

int nargs;			/* number of remaining arguements*/
char **args;			/* remaining arguments */
double *prjparms;		/* projection parameters */
int *prjzone;			/* projection zone */
int *prjtype;			/* projection type */
int *prjsph;			/* projection sphere */
{
double temspar1;		/* first standard parallel */
double temspar2;		/* second standard parrallel */
double temcen;			/* center longitude */
double temorg;			/* longitude of origin */
int status;			/* status of called subroutines */
char punits[5];			/* buffer for projection units */
char stdpar1[37];		/* first standard parallel parameter */
char stdpar2[37];		/* second standard parallel parameter */
char centmer[37];		/* center longitude parameter */
char origin[37];		/* latitude of origin */
char smaxis[37];		/* semi-major axis parameter */
char ecsqval[37];		/* eccentricity squared */
char faleast[37];		/* false east parameter */
char falnorth[37];		/* false north parameter */
int c;
int aflag=0, bflag=0, cflag=0, sflag=0, dflag=0, eflag=0;
extern char *optarg;

/*  Set the zone and projection type.
-------------------------------------*/
*prjzone = 62;
*prjtype = 3;
*prjsph = 0;
strcpy(punits, "DEG");
strcpy(faleast, "0");
strcpy(falnorth, "0");
strcpy(smaxis, "0");
strcpy(ecsqval, "0");
strcpy(origin, "0");


/* Check the remaining parameters */

while ((c=getopt (nargs,args,"g:p:a:b:c:o:s:e:d:")) != -1)
	switch(c) {
		case 'g':
			strcpy(punits, optarg);
			break;
		case 'a':
			strcpy(stdpar1, optarg);
			aflag++;
			break;
		case 'b':
			strcpy(stdpar2, optarg);
			bflag++;
			break;
		case 'c':
			strcpy(centmer, optarg);
			cflag++;
			break;
		case 'o':
			strcpy(origin, optarg);
			break;
		case 's':
			strcpy(smaxis, optarg);
			sflag++;
			break;
		case 'e':
			strcpy(ecsqval, optarg);
			eflag++;
			break;
		case 'd':
      			*prjsph = atol(optarg);
      			dflag++;
			break;
    		default:
			print_albers_usg();
		}

/***************ERROR CHECKING FOR MUTUALLY EXCLUSIVE OPTIONS****************/
if (dflag && sflag)
  {
  printf("Mutually exclusive options:  (-s and -e) && (-d)."
     	 "\nMust use either -s and -e OR -d.  Cannot use both.\n"
  	 "\nExiting...\n\n");
  exit(0);
  }
		
if (!aflag || !bflag || !cflag) {
   c_errmsg("1ST STD PARALLEL, 2ND STD PARALLEL, and CENTERAL MERIDIAN have to be specified",
	    "projprm-specify", NON_FATAL);
   print_albers_usg();
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);
}

/*  Convert the parameters from strings to double precision values.  This
    done because TAE does not allow the input of double precision parameters.
-----------------------------------------------------------------------------*/
sscanf(stdpar1,"%lf",&temspar1);
sscanf(stdpar2,"%lf",&temspar2);
sscanf(centmer,"%lf",&temcen);
sscanf(origin,"%lf",&temorg);
sscanf(smaxis, "%lf",(prjparms + 0));
sscanf(ecsqval,"%lf",(prjparms + 1));
sscanf(faleast,"%lf",(prjparms + 6));
sscanf(falnorth,"%lf",(prjparms + 7));

/*  Convert the values to be placed in PRJPARMS to DMS format and check
    to be sure they are valid. 
-----------------------------------------------------------------------*/
status = c_degdms(&temspar1,(prjparms + 2),punits,"LAT");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);
   
status = c_degdms(&temspar2,(prjparms + 3),punits,"LAT");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);
   
status = c_degdms(&temorg,(prjparms + 5),punits,"LAT");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);

status = c_degdms(&temcen,(prjparms + 4),punits,"LON");
if (status == E_FAIL)
   c_errmsg("Fatal error encountered","projprm-fatal",LAS_FATAL);
   
return;
} 


void print_albers_usg()	{
	fprintf(stderr, "Usage for Ablers Equal Area Projection:\n\n");
	fprintf(stderr, "projprm <albers> <prjkey> <outfile> [-g geounits]\n");
	fprintf(stderr, "  [-a 1st standard parallel]	[-b 2nd standard parallel]\n");
	fprintf(stderr, "  [-c center longitude]	[-o latitude of origin]\n");
	fprintf(stderr, "  [-s semi-major axis]	[-e eccentricity squared]\n");
	fprintf(stderr, "  [-d datum number]\n");
	fprintf(stderr, "\nVersion %.2f, ASF SAR tools.\n", VERSION);
exit(1);
}
