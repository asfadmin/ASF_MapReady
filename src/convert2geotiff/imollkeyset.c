/*****************************************************************************
NAME: IMOLLKEYSET

PURPOSE:  Set the geoTIFF keys for Interrupted Mollweide projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "las.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void imollkeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  double sphere = 0.0;     /* radius of the reference sphere      */

  /* Interrupted Mollweide is not a known projection in the current version
     of geoTIFF.
     ---------------------------------------------------------------------- */

  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
            "Interrupted Mollweide--incomplete implementation in geoTIFF");

  GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
  GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
  GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, get_geocode(ddr));
  GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, 
                   get_linear_units(ddr.proj_units));
  GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
  GTIFKeySet(gtif, ProjectionGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, 
                   get_linear_units(ddr.proj_units));

  if (ddr.valid[3])
  {
    /* Try to set as much as possible
       Set the radius of the reference sphere
       ----------------------------------------- */
    get_sphere(ddr,&sphere);
    GTIFKeySet(gtif,GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, sphere);
    GTIFKeySet(gtif,GeogSemiMinorAxisGeoKey, TYPE_DOUBLE, 1, sphere);
  }
}
