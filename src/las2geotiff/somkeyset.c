/*****************************************************************************
NAME: SOMKEYSET

PURPOSE:  Set the geoTIFF keys for Space Oblique Mercator projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "las.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void somkeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  /* Space Oblique Mercator is not a known projection in the current
     version of geoTIFF.  The Inclination of orbit at ascending node,
     counter-clockwise from equator; Longitude of ascending orbit at
     equator; Period of satellite revolution; Landsat ratio; End of path
     flag; Landsat satellite number; and Landsat satellite number are not
     carried over from LAS.  Plans to add SOMInclinAngleGeoKey,
     SOMAscendLongGeoKey, SOMRevPeriodGeoKey, SOMEndOfPathGeoKey,
     SOMRatioGeoKey, SOMPathNumGeoKey, and SOMSatelliteNumGeoKey to
     Revision 2.0 of geoTIFF are under consideration.
     -------------------------------------------------------------------- */
  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
      "Space Oblique Mercator--incomplete implementation in geoTIFF");

  /* Set the major and minor axis values
     ----------------------------------- */
  set_major_minor(ddr,gtif);

}
