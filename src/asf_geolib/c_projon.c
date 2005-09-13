/*******************************************************************************
NAME			      PROJON

PURPOSE	     Reports user entered projection parameters to a user specified
	     device.  This routine also does minor parameter validation,
	     checks compatablity of datum codes, and supplies default datum
	     codes if requested.

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand		Aug, 1988	Original Development
D. Steinwand		Feb, 1989	Misc updates
D. Steinwand		Aug, 1989	Fixed UTM zone code change
D. Etrheim		Jul, 1990	Standardized error message handling
T. Mittan		Jun, 1993	Adapted for the C version of GCTP.
S. Nelson		Jul, 1993	Set the datum for State Plane in
					the projection array to be transferred
					in PROJ.

PROJECT:	Sarreg 

ALGORITHM

	Check validity of parameters
	Pack spheroid codes
	Compare input and output datums--are they compatable?
	Report user entered parameters to user selected device
	Return

ALGORITHM REFERENCES

1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Profesional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987. 

2.  "Software Documentation for GCTP General Cartographic Transformation
    Package", U.S. Geological Survey National Mapping Division, May 1982.

3.  Clarie, Charles N, "State Plane Coordinates by Automatic Data Processing",
    U.S. Department of Commerce, Environmental Science Services Admin.,
    Coast and Geodetic Survey, Publication 62-4, 1973.
*******************************************************************************/
#include "worgen.h"
#include "cproj.h"
#include "proj.h"

int c_projon(inproj, inunit, inzone, indatum, inparm, outproj, 
                  outunit, outzone, outdatum, outparm, prtprm, fname)

int *inproj;		/* Input projection number */
int *inunit;		/* Input projection unit code */
int *inzone;		/* Input projection zone number */
int *indatum;		/* Input projection datum code */
double inparm[15];	/* 15 input projection parameters */
int *outproj;		/* Output projection number */
int *outunit;		/* Output projection unit code */
int *outzone;		/* Output projection zone number */
int *outdatum;		/* Output projection datum code */
double outparm[15];	/* 15 output projection parameters */
int prtprm[2];		/* Report destination.  A TRUE/FALSE toggle is used:
			   prtprm[GEO_TERM], prtprm[GEO_FILE] */
char *fname;		/* File name if printing to file, NULL if not */

{

/* Check for same projection as input and output & adjust zone numbers,
   if not UTM or State Plane
  -------------------------*/
if ((*inproj != GEO) && (*inproj != SPCS) && (*inproj != UTM) &&
    (*outproj == *inproj) && (*outzone == *inzone)) (*outzone)++;

/* Do preliminary error checking on parameters where possible
 -----------------------------------------------------------*/
if((*inproj < 0) || (*inproj > MAXPROJ)) return(proj_err(1001));
if((*outproj < 0) || (*outproj > MAXPROJ)) return(proj_err(1002));
if((*inunit < 0) || (*inunit > MAXUNIT)) return(proj_err(1003));
if((*outunit < 0) || (*outunit > MAXUNIT)) return(proj_err(1004));

if((*inproj != GEO) && (*inunit != 2) && (*inunit != 1)) return(proj_err(1005));
if((*outproj != GEO)&&(*outunit != 2) && (*outunit != 1))return(proj_err(1006));

/* Check for valid DMS angles 
 ---------------------------*/
if (*inproj != SOM)
   if (check_dms(inparm[3]) != E_SUCC) return(proj_err(1116));
if (check_dms(inparm[4]) != E_SUCC) return(proj_err(1116));
if (check_dms(inparm[5]) != E_SUCC) return(proj_err(1116));
if (*outproj != SOM)
   if (check_dms(outparm[3]) != E_SUCC) return(proj_err(1116));
if (check_dms(outparm[4]) != E_SUCC) return(proj_err(1116));
if (check_dms(outparm[5]) != E_SUCC) return(proj_err(1116));
if ((*inproj == ALBERS) || (*inproj == LAMCC) || (*inproj == EQUIDC))
   if (check_dms(inparm[2]) != E_SUCC) return(proj_err(1116));
if ((*outproj == ALBERS) || (*outproj == LAMCC) || (*outproj == EQUIDC))
   if (check_dms(outparm[2]) != E_SUCC) return(proj_err(1116));

if (*inproj == SPCS)
   if ((*indatum == 0) || (*indatum == 8))
      inparm[0] = *indatum;
if (*outproj == SPCS)
   if ((*outdatum == 0) || (*outdatum == 8))
      outparm[0] = *outdatum;

/* Pack spheroid codes into the input and output parameter arrays
 ---------------------------------------------------------------*/
if (spheroid(*inproj, *indatum, inparm) != E_SUCC) 
   {
   c_errmsg("Error returned from spheroid","projon-call",NON_FATAL);
   return(E_FAIL);
   }
if (spheroid(*outproj, *outdatum, outparm) != E_SUCC)
   {
   c_errmsg("Error returned from spheroid","projon-call",NON_FATAL);
   return(E_FAIL);
   }

/* Write projection parameters to output device
 ---------------------------------------------*/
if (prtprm[GEO_TERM] == GEO_TRUE)
  {
   proj_report(*inproj, *inzone, inparm, "Input Projection:");
   proj_report(*outproj, *outzone, outparm, "Output Projection:");
  }

return(E_SUCC);
}
