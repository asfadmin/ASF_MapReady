/****************************************************************
FUNCTION NAME: geokeyset

SYNTAX:  geokeyset (ddr, gtif); 

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    ddr      struct DDR         I: LAS image file descriptor record    
    gtif       GTIF *           I/O: GeoKey-level descriptor           

DESCRIPTION:
    Set the geoTIFF keys for Geographic projection.

RETURN VALUE: None

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0		    tae		Must run under TAE.  PROJECT: LAS
    1.0     8/01   S. Watts     Removed TAE dependencies.

****************************************************************/
#include "ddr.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"


#define FALSE 0


void geokeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  unsigned short units;    /* geoTIFF unit of measure code        */

  if (ddr.valid[2])
  {
    /* the ellipsoid code is valid so try to get the geoTIFF 
       geocode.  Returns zero if it can't find one to match
       ----------------------------------------------------- */
    units = get_geocode(ddr);
    if (units)
      GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, units+3000);
  }
  else
  {
    printf("\nLAS image does not contain valid ellipsoid code\n");
    GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, 32767);
  }
  if (ddr.valid[4])
  {
    /* the ground units code is valid so try to get the geoTIFF
       equivalent unit code, otherwise returns zero.
       -------------------------------------------------------- */
    units = get_linear_units(ddr.proj_units);
    if (units != 0)
      /* it is a linear unit (ie meter, feet, etc)
         ----------------------------------------- */
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, units);
    else
    {
      units = get_angular_units(ddr.proj_units);
      if (units != 0)
        /* it is an angular unit (ie dms, degrees, seconds, etc)
           ----------------------------------------------------- */ 
        GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, units);
    }
  }
  GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII, 0,
             "Geographic converted from LAS");
}
