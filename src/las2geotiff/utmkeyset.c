/*****************************************************************************
NAME: UTMKEYSET

PURPOSE:  Set the geoTIFF keys for Universal Transverse Mercator projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "ddr.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void utmkeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  unsigned short ellipse=0;  /* geoTIFF code for ellipse            */

  if ((ddr.datum_code == 5) || (ddr.datum_code == 12) || 
       ((ddr.datum_code >= 217) && (ddr.datum_code <= 222)) ||
       ((ddr.datum_code >= 223) && (ddr.datum_code <= 242)) ||
       (ddr.datum_code == 316) || (ddr.datum_code == 317))
  {
    GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
    GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
    GTIFKeySet(gtif, ProjectedCSTypeGeoKey,TYPE_SHORT, 1,
                     get_utmzone(ddr));
  }
  else
  {
    GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
    GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
    GTIFKeySet(gtif, ProjectedCSTypeGeoKey,TYPE_SHORT, 1, 32767);
    GTIFKeySet(gtif, ProjectionGeoKey,TYPE_SHORT, 1, get_utmzonegen(ddr));
    ellipse = get_geocode(ddr);
    if (ellipse != 0)
    {
      GTIFKeySet(gtif, GeographicTypeGeoKey,TYPE_SHORT, 1, ellipse);
    }
    else
    {
      printf("\nLAS image does not contain valid ellipsoid code\n");

    }
  }
  if (ddr.valid[4])
    GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1,
                     get_linear_units(ddr.proj_units));
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "UTM converted from LAS");
}
