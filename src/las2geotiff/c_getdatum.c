/****************************************************************
FUNCTION NAME: c_getdatum

SYNTAX: c_getdatum(dtm_info)  

PARAMETERS:
    NAME:	TYPE:		   PURPOSE:
    --------------------------------------------------------
    dtm_info  struct DATUMDEF*     Datum code

DESCRIPTION:
    Retrieve datum information from datum and spheroid tables and fill
    the DATUMDEF variable

RETURN VALUE: Integer (1 or o for sucess of failure)

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.2    02/98   J. Willems   Initial development for datum conversi
    0.4    05/99   D. Lloyd     Free malloc'd memory
    0.6    10/99   T. Ratliff	Redesigned the routine to fix an
				error and be more efficient
    0.8	   12/99   D. Hames	Previous redesign discontinued the file
				check to spheroid.txt after a datum code
				was assigned. Copy and modify c_getdatum
				version 09/01/99. Add additional checks
				to while statements to fix infinite 
				looping and fatal error if the datum is
				not found. SRF#3127.1.0     8/01   S.
    1.0     8/01    S. Watts    Removed TAE dependencies.

*******************************************************************************/

#include <stdlib.h>
#include <math.h>

#include "asf.h"
#include "datum.h"
#include "datum_table.h"
#include "spheroid_table.h"

#define E_SUCC 0
#define E_FAIL -1

int c_getdatum
(
    struct DATUMDEF *dtm_info		/* Datum code */
)
{
    long datum_num = -1;            /* Datum code */
    long sphere_num = -1;           /* Spheroid code */
    long dtmflag = FALSE;           /* Flag set if datum code represents datum */
    double flat = 0.0;              /* Flattening of the spheroid */

    datum_num = dtm_info->datumnum;

    /* Check for a valid datum code
      ----------------------------*/
    if(datum_num < 0) {
       printf("\nDatum value below valid range\n");
       return(E_FAIL);
    }
    else if(datum_num > MAX_DTM) {
       printf("\nDatum value above valid range\n");
       return(E_FAIL);
    }
    else if(datum_num > MIN_DTM)
       dtmflag = TRUE;

    /* If datum is present, retrieve information from datum table
      ----------------------------------------------------------*/
    if (dtmflag && ((datum_num-100)>=0)) {
        strcpy(dtm_info->datumname, datum_table[datum_num-100].datumname);
        strcpy(dtm_info->area,      datum_table[datum_num-100].area);
        strcpy(dtm_info->category,  datum_table[datum_num-100].category);
        dtm_info->spherenum = datum_table[datum_num-100].spherenum;
        dtm_info->xshift    = datum_table[datum_num-100].xshift;
        dtm_info->yshift    = datum_table[datum_num-100].yshift;
        dtm_info->zshift    = datum_table[datum_num-100].zshift;
        dtm_info->xdelta    = datum_table[datum_num-100].xdelta;
        dtm_info->ydelta    = datum_table[datum_num-100].ydelta;
        dtm_info->zdelta    = datum_table[datum_num-100].zdelta;
    }
    /* Datum not in table, assign spherenum and set everything else to zero
      --------------------------------------------------------------------*/
    else {
       dtm_info->spherenum = datum_num;
       dtm_info->datumname[0] = '\0';
       dtm_info->area[0] = '\0';
       dtm_info->category[0] = '\0';
       dtm_info->xshift = 0;
       dtm_info->yshift = 0;
       dtm_info->zshift = 0;
       dtm_info->xdelta = 0;
       dtm_info->ydelta = 0;
       dtm_info->zdelta = 0;
    }


    sphere_num = dtm_info->spherenum;

    /* Check for a valid spheroid code
      -------------------------------*/
    if(sphere_num < MIN_SPHEROID) {
       printf("\nSpheroid value below valid range\n");
       return(E_FAIL);
    }
    else if(sphere_num > MAX_SPHEROID) {
       printf("\nSpheroid value above valid range\n");
       return(E_FAIL);
    }

    /*Retrieve necessary spheroid data
      -------------------------------*/
    strcpy(dtm_info->spherename, spheroid_table[sphere_num].spherename);
    dtm_info->recip_flat = spheroid_table[sphere_num].recip_flat;
    dtm_info->smajor     = spheroid_table[sphere_num].smajor;

    /* Compute sminor if recip_flat is not zero, otherwise we have a sphere
     ---------------------------------------------------------------------*/
    if (dtm_info->recip_flat != 0) {
       flat = (1.0 / dtm_info->recip_flat);
       dtm_info->sminor = (dtm_info->smajor * (1 - flat));
    }
    else
       dtm_info->sminor = spheroid_table[sphere_num].smajor;


    return(E_SUCC);
}
