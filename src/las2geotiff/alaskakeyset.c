/****************************************************************
FUNCTION NAME: alaskakeyset

SYNTAX:  alaskakeyset (ddr, gtif); 

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    ddr      struct DDR         I: LAS image file descriptor record   
    gtif       GTIF *           I/O: GeoKey-level descriptor          

DESCRIPTION:
    Set the geoTIFF keys for Alaska Conformal projection.

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

void alaskakeyset 
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                   CT_TransvMercator_Modified_Alaska);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "Transverse Mercator Modified Alaska converted from LAS");

  /* Set the major and minor axis values
     ----------------------------------- */
  set_major_minor(ddr,gtif);

  if (ddr.valid[3])
  {
    /* Set the false easting and false northing
       ---------------------------------------- */
    GTIFKeySet(gtif,ProjFalseEastingGeoKey,TYPE_DOUBLE,1,ddr.proj_coef[6]);
    GTIFKeySet(gtif,ProjFalseNorthingGeoKey,TYPE_DOUBLE,1,ddr.proj_coef[7]);
  }
}
