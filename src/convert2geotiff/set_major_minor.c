/*****************************************************************************
NAME: SET_MAJOR_MINOR

PURPOSE:  Set the proper information in the geoTIFF based on the values
          stored in the semi-major and semi-minor parameters in the
          LAS DDR.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include <math.h>

#include "ddr.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "fhdr.h"
#include "datum.h"
#include "protos.h"

/*PROTOTYPE*/ int c_getdatum(struct DATUMDEF *dtm_info);

SETSPHEREDEF (spheredefines);     /* Spheroid keywords, names, and axes */

void set_major_minor 
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
)

{
  struct DATUMDEF datuminfo; /* datum information                 */

  /* Need to check if the coefficients are valid
     ------------------------------------------- */
  if (ddr.valid[3])
  {
    if (ddr.proj_coef[0] != 0)
    {
      /* The semi-major axis is set.
         --------------------------- */
      GTIFKeySet(gtif,GeogSemiMajorAxisGeoKey,TYPE_DOUBLE,1,ddr.proj_coef[0]);
      if (ddr.proj_coef[1] == 0)
      {
        /* The semi-minor axis is set to zero in the ddr, so
           set the semi-minor to the same value as the semi-major.
           ------------------------------------------------------- */
        GTIFKeySet(gtif,GeogSemiMinorAxisGeoKey,TYPE_DOUBLE,1,
                        ddr.proj_coef[0]);
      }
      else if (ddr.proj_coef[1] < 1)
      {
        /* Semi-minor axis is less than one, so it is the eccentricity squared.
           GeoTIFF has a parameter for inverse flattening.
           Relationship between eccentricity and flattening is
           (according to MAP PROJECTIONS--A WORKING MANUAL, U.S.
           Geological Survey Professional Paper 1395, page 13)
           f = 1 - (1 - e^2)^1/2
           -------------------------------------------------------------- */
        GTIFKeySet(gtif,GeogInvFlatteningGeoKey,TYPE_DOUBLE,1,
                    1/(1-sqrt(1-ddr.proj_coef[1])));
      }
      else
      {
        /* Semi-minor axis is greater than one, so it actually 
           is the semi-minor axis
           ---------------------------------------------------- */
        GTIFKeySet(gtif,GeogSemiMinorAxisGeoKey,TYPE_DOUBLE,1,
                        ddr.proj_coef[1]);
      }
    }
    else
    {
      /* The major axis is not set, so we look at the datum code
         to determine the major and minor axes.
         ------------------------------------------------------- */
      if (ddr.valid[2])
      {
        datuminfo.datumnum = ddr.datum_code;
        c_getdatum (&datuminfo);
        ddr.proj_coef[0] = datuminfo.smajor;
        ddr.proj_coef[1] = datuminfo.sminor;
      }

      GTIFKeySet(gtif,GeogSemiMajorAxisGeoKey,TYPE_DOUBLE,1,
                 spheredefines[ddr.datum_code].smjaxis);
      GTIFKeySet(gtif,GeogSemiMinorAxisGeoKey,TYPE_DOUBLE,1,
                 spheredefines[ddr.datum_code].smnaxis);
     
    }
  }
  else
  {
    /* The projection coefficients were not valid, so there
       in nothing to set.
       ---------------------------------------------------- */
   printf("\nLAS image does not contain valid projection coefficients\n");
    GTIFKeySet(gtif,GeographicTypeGeoKey,TYPE_SHORT,1,get_geocode(ddr));
  }
  /* The linear units need to be set no matter what. 
     ------------------------------------------------ */ 
  GTIFKeySet(gtif,GeogLinearUnitsGeoKey,TYPE_SHORT,1,
                  get_linear_units(ddr.proj_units));
}
