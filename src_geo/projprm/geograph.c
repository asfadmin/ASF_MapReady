/****************************************************************************
NAME:				 GEOGRAPH 

PURPOSE:  Retrieves parameters for the GEOGRAPHIC projection

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
 NEWLAS		9/87	B. Ailts	   CSB	    original development
  5.0		1/89	D. Steinwand	   CSB      retrocode to 5.0

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   PROJPRM must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Set the zone and projection type
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


void geograph(nargs,args,prjzone,prjtype,prjsph,prjparms)

int nargs;			/* number of remaining arguements*/
char **args;			/* remaining arguments */
double *prjparms;		/* projection parameters */
int *prjzone;			/* projection zone */
int *prjtype;			/* projection type */
int *prjsph;			/* projection sphere */
{
/*  Set the zone, projection type, and speriod.
-----------------------------------------------*/
int c;

*prjzone = 62;
*prjtype = 0;  


while ((c=getopt (nargs,args,"d:")) != -1)
	switch(c) {
		
		case 'd':
      			*prjsph = atol(optarg);
      			break;
    		default:
			printf("Usage for Geograph Projection:\n"
			       "projprm <geograph> <prjkey> <outfile>"
				" [-d datum_number]\n"
				"Version %.2f, ASF SAR Tool. \n\n", VERSION); 
			exit(0);
		   }


return;

}  /* geograph */
