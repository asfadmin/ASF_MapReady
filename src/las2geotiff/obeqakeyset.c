/*****************************************************************************
NAME: OBEQAKEYSET

PURPOSE:  Set the geoTIFF keys for Oblated Equal Area projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "las.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void obeqakeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  double lat = 0;          /* Latitude                            */
  double lon = 0;          /* Longitude                           */
  double sphere = 0.0;     /* radius of the reference sphere      */

  /* Oblated Equal Area is not a known projection in the current version
     of geoTIFF.  The shape parameters (m & n) and the rotation angle
     are not carried over from LAS.  Plans to add OEAShapeMGeoKey, 
     OEAShapeNGeoKey, and OEARotationAngleGeoKey for Revision 2.0 of
     geoTIFF are under consideration.
     ------------------------------------------------------------------- */

  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
         "Oblated Equal Area--incomplete implementation in geoTIFF");
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

    /* Set the longitude of the center of projection
       --------------------------------------------- */
    lon = ddr.proj_coef[4];
    c_decdeg (&lon, "DMS", "LON");
    GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, lon);

    /* Set the latitude of the center of projection
       --------------------------------------------- */
    lat = ddr.proj_coef[5];
    c_decdeg (&lat, "DMS", "LAT");
    GTIFKeySet(gtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1, lat);

    /* Set the false easting and false northing
       ---------------------------------------- */
    GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[6]);
    GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[7]);
  }
}
