/*******************************************************************************
NAME			      PROJ

PURPOSE	    Converts coordinates from one map projection to another 

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand		Aug, 1988	Original Development
D. Steinwand		Apr, 1989	Augmented to handle packed DMS units
					& swapped X,Y coordinates for SOM
D. Etrheim		Jul, 1990	Standardized error message handling
T. Mittan		Jun, 1993	Adapted for the C version of GCTP.
S. Nelson		Jul, 1993	Set the datum for State Plane from
					the projection array.
J. Fenno		Oct, 1993	Correct call for getenv and correct 
					the file name for VMS systems

PROJECT     LAS

ALGORITHM 

	Combine X and Y input values into an array
        Call proj_err if an error occured; return E_FAIL or IN_BREAK
   	Unpack array of X and Y output coordinates into separate X & Y
	Return E_SUCC

ALGORITHM REFERENCES

1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987. 

2.  "Software Documentation for GCTP General Cartographic Transformation
    Package", U.S. Geological Survey National Mapping Division, May 1982.

3.  Clarie, Charles N, "State Plane Coordinates by Automatic Data Processing",
    U.S. Department of Commerce, Environmental Science Services Admin.,
    Coast and Geodetic Survey, Publication 62-4, 1973.
*******************************************************************************/
#include "asf.h"
#include "worgen.h"
#include "cproj.h"
#include "proj.h"


int c_proj(inproj, inunit, inzone, inparm, outproj, outunit, outzone, 
                outparm, inx, iny, outx, outy)

int *inproj;		/* Input projection code */
int *inunit;		/* Input projection units code */
int *inzone;		/* Input projection zone code */
double *inparm;		/* Array of 15 projection parameters--input */
int *outproj;		/* Output projection code */
int *outunit;		/* Output projection units code */
int *outzone;		/* Output projection zone code */
double *outparm;	/* Array of 15 projection parameters--output */
double *inx;		/* Input X projection coordinate */
double *iny;		/* Input Y projection coordinate */
double *outx;		/* Output X projection coordinate */
double *outy;		/* Output Y projection coordinate */
{
int off = 3;		/* Off flag for GCTP error and print messages */
int dummy_datum = -1;	/* Dummy datum code--all datums are packed in projon */
int status;		/* Status code from call to GCTP */
int out_dms, in_dms;	/* Boolean flags for DMS unit check */
char efile[CMLEN];
char pfile[CMLEN];
char fn27[CMLEN];
char fn83[CMLEN];

double incoor[2];	/* Input coordinates */
double outcoor[2];	/* Output coordinates */

/* If State Plane, set datum equal to 0
   -----------------------------------*/
if (*outproj == SPCS)
   dummy_datum = outparm[0];
if (*inproj == SPCS)
   dummy_datum = inparm[0];

/* Pack input coordinates into a GCTP compatable array
  ---------------------------------------------------*/
incoor[0] = *inx;
incoor[1] = *iny;

/* Swap X & Y if the input projection is SOM
  -----------------------------------------*/
if (*inproj == SOM)
   {
   incoor[0] = -(*iny);
   incoor[1] = *inx;
   }

/* Since the GCTP doesn't work with DMS input or output units, convert to
   degrees if DMS units are being used
  -----------------------------------*/
out_dms = FALSE;
in_dms = FALSE;
if (*inunit == 5)
   {
   if (c_decdeg(&(incoor[0]), "DMS", "LON") != E_SUCC) 
      {
      c_errmsg("Error returned from decdeg","proj-call",NON_FATAL); 
      return(E_FAIL);
      }
   if (c_decdeg(&(incoor[1]), "DMS", "LON") != E_SUCC)
      {
      c_errmsg("Error returned from decdeg","proj-call",NON_FATAL); 
      return(E_FAIL);
      }
   in_dms = TRUE;
   *inunit = 4;
   }
if (*outunit == 5)
   {
   out_dms = TRUE;
   *outunit = 4;
   }

/* Place State Plane directory in fn27, fn83
------------------------------------------*/
/*
if (strlen(worconfig) == 0)
    {
    ptr = (char *)getenv("WORCONFIG");
    if (ptr == NULL)
	{
	c_errmsg("WORCONFIG not defined","proj-getenv",NON_FATAL); 
	return(E_FAIL);
	}
    strcpy(worconfig, ptr);
    }
*/
sprintf(fn27, "%s/nad27sp", "../../include");
sprintf(fn83, "%s/nad83sp", "../../include");


/* Call the GCTP  entry point
  -------------------------*/
  gctp(incoor,inproj,inzone,inparm,inunit,&dummy_datum,&off,efile,&off,pfile,
       outcoor, outproj,outzone,outparm,outunit,fn27,fn83,&status);

/* Unpack transformed coordinates
  ------------------------------*/
*outx = outcoor[0];
*outy = outcoor[1];

/* IF either input or output units were originally DMS, adjust values
  ------------------------------------------------------------------*/
if (in_dms) *inunit = 5;
if (out_dms)
   {
   *outunit = 5;
   if (c_degdms(&(outcoor[0]), outx, "DEG", "LON") != E_SUCC) 
      {
      c_errmsg("Error return fromed degdms","proj-call",NON_FATAL); 
      return(E_FAIL);
      }
   if (c_degdms(&(outcoor[1]), outy, "DEG", "LON") != E_SUCC)
      {
      c_errmsg("Error returned from degdms","proj-call",NON_FATAL); 
      return(E_FAIL);
      }
   }

/* If the output projection is SOM, swap the X & Y coordinates
  -----------------------------------------------------------*/
if (*outproj == SOM)
   {
   *outx = outcoor[1];
   *outy = -(outcoor[0]);
   }

/* Check the return status and return to calling function
  ------------------------------------------------------*/
return(proj_err(status));
}
