/*****************************************************************************
NAME: WAGVIIKEYSET

PURPOSE:  Set the geoTIFF keys for Wagner VII projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "las.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void wagviikeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  double lon = 0;          /* Longitude                           */
  double sphere = 0.0;     /* radius of the reference sphere      */

  /* Wagner VII is not a known projection in the current version of geoTIFF.
     ----------------------------------------------------------------------- */
  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
      "Wagner VII--incomplete implementation in geoTIFF");
  if (ddr.valid[3])
  {
    /* Try to set as much as possible
       Set the radius of the reference sphere
       ----------------------------------------- */
    get_sphere(ddr,&sphere);
    GTIFKeySet(gtif,GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, sphere);
    GTIFKeySet(gtif,GeogSemiMinorAxisGeoKey, TYPE_DOUBLE, 1, sphere);

    /* Set the longitude of the central meridian
       ----------------------------------------- */
    lon = ddr.proj_coef[4];
    c_decdeg (&lon, "DMS", "LON");
    GTIFKeySet(gtif, GeogPrimeMeridianGeoKey, TYPE_DOUBLE, 1, lon);

    /* Set the false easting and false northing
       ---------------------------------------- */
    GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[6]);
    GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[7]);
  }
}
