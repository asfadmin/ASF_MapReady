/****************************************************************************
NAME:					 UTM 

PURPOSE:  Retrieves parameters for the UNIVERSAL TRANSVERSE MERCATOR projection

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
 NEWLAS		9/87	B. Ailts	   CSB	    original development
  5.0		1/89	D. Steinwand	   CSB      retrocode to 5.0
  5.01		4/89	D. Steinwand	   CSB      Calc zone # from longitude
  5.1	       10/92    D. Etrheim         CSB      If latitude is negitive,
						    make the zone negative.
  6.0           6/95    M. Shindle         ASF      Remove TAE dependencies.
  6.01          2/98    O. Lawlor	   ASF      Removed asf_tm library,
       						    added math.h include.
  6.02          8/01    S. Watts           ASF      Added -d option.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Set the zone and projection type
	Retreive the needed parameters
	Convert the parameters from strings to double precision values.  This
	   done because TAE does not allow the input of double precision 		   parameters
	Convert the longitude value to degrees & calculate zone number
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

void print_utm_usg(void);

void utm(
         int nargs,
         char **args,
         int *prjzone,       /* projection zone */
         int *prjtype,       /* projection type */
         int *prjsph,        /* projection sphere */
         double *prjparms    /* projection parameters */
        )
{
 double tlat;                   /* latitude parameter */
 double tlon;                   /* longitude parameter */
 int status;                    /* status of called subroutines */
 char punits[4];                /* buffer for projection units */
 int c;
 extern char *optarg;
 int zflag=0, lflag=0, aflag=0;
 int i = 3;

 /* set default values */
 strcpy(punits,"DEG");
 *prjtype = 1;
 *prjsph = 0; 

 /*  Retrieve the parameters needed.
 -----------------------------------*/
 while ((c=getopt(nargs,args,"a:g:l:z:d:")) != -1) {
   switch (c) {
    case 'g':
      strcpy(punits,optarg);
      c_low2up(punits,&i);
      break;
    case 'a':
      tlat = atof(optarg);
      aflag = 1;
      break;
    case 'l':
      tlon = atof(optarg);
      lflag = 1;
      break;
    case 'z':
      *prjzone = atol(optarg);
      zflag = 1;
      break;
    case 'd':
      *prjsph = atol(optarg);
      break;
    default:
      print_utm_usg();
      break;
   }
 }
 if (!zflag && !(aflag && lflag)) {
   c_errmsg("ZONENUM or LATITUDE and LONGITUDE have to be specified",
	    "projprm-specify", NON_FATAL);
   print_utm_usg();
 }
 if (zflag && (aflag || lflag)) {
   c_errmsg("Zone number vs. Latitude & Longitude are mutually exclusive",
            "projprm-specify", NON_FATAL);
   print_utm_usg();
 }
   
 /*  If LAT and LONG are specified, convert them to DMS.
 ------------------------------------------------------*/
 if ( lflag && aflag ) {
   status = c_decdeg(&tlon,punits,"LON");
   if (status == E_FAIL)
      c_errmsg("Error converting longitude to DMS","projprm-fatal",LAS_FATAL);
   status = c_decdeg(&tlat,punits,"LAT");
   if (status == E_FAIL)
      c_errmsg("Error converting latitude to DMS","projprm-fatal",LAS_FATAL);
   *prjzone = (int)(fabs((-180.0 - tlon) / 6.0)) + 1;
   if (tlat < 0.0)
      *prjzone = -(*prjzone);

   /* *prjzone = (int)((tlon + 180) / 6) + 1;
   if (*prjzone > 60) *prjzone -= 60;*/
 } 
 return;
}

void print_utm_usg(void) {
 printf("\n"
	"USAGE FOR UTM PROJECTION:\n"
	"   projprm <utm> <prjkey> <outfile> [-g geounits] [-d datum number]\n"
	"           <-z zonenum> | (<-a latitude> <-l longitude>)\n"
	"\n");
 printf("Version %.2f, ASF SAR TOOLS.\n"
	"\n",VERSION);
 exit(2);
}

