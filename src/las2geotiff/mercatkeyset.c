/*****************************************************************************
NAME: MERCATKEYSET

PURPOSE:  Set the geoTIFF keys for Mercator projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "las.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void mercatkeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  double lat = 0;          /* Latitude                            */
  double lon = 0;          /* Longitude                           */

  GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
  GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);

  /* Set the major and minor axis values
     ----------------------------------- */
  set_major_minor(ddr, gtif);

  GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, ProjectionGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, 
                   get_linear_units(ddr.proj_units));
  GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_Mercator);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "Mercator converted from LAS");
  if (ddr.valid[3])
  {
    /* Set the longitude of the reference
       ---------------------------------- */
    lon = ddr.proj_coef[4];
    c_decdeg (&lon, "DMS", "LON");
    GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, lon);

    /* Set the latitude of the reference
       --------------------------------- */
    lat = ddr.proj_coef[5];
    c_decdeg (&lat, "DMS", "LAT");
    GTIFKeySet(gtif, ProjOriginLatGeoKey, TYPE_DOUBLE, 1, lat);

    /* Set the false easting and false northing
       ---------------------------------------- */
    GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[6]);
    GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[7]);
  }
}
