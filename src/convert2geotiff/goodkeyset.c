/*****************************************************************************
NAME: GOODKEYSET

PURPOSE:  Set the geoTIFF keys for Interrupted Goode Homolosine projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "ddr.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void goodkeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  double sphere = 0.0;     /* radius of the reference sphere      */

  /* Goode Homolosine is not a known projection in the current
     version of geoTIFF.  The sphere should be all that is
     required for this projection.
     --------------------------------------------------------- */
  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "Goode Homolosine--Incomplete implementation in geoTIFF");

  /* Try to set as much as possible
     All that can be set is the radius of the reference sphere
     --------------------------------------------------------- */
  get_sphere(ddr,&sphere);
  GTIFKeySet(gtif,GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, sphere);
  GTIFKeySet(gtif,GeogSemiMinorAxisGeoKey, TYPE_DOUBLE, 1, sphere);
}
