/*****************************************************************************
NAME: SPCSKEYSET

PURPOSE:  Set the geoTIFF keys for State Plane Coordinates projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "las.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void spcskeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                   get_stateplanezone(ddr));
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "State Plane converted from LAS");
  GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
  GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
  if (ddr.valid[4])
    /* units are valid */
    GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1,
                     get_linear_units(ddr.proj_units));
  GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
}
