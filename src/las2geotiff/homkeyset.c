/*****************************************************************************
NAME: HOMKEYSET

PURPOSE:  Set the geoTIFF keys for Hotine Oblique Mercator projection.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "ddr.h"
#include "tiffio.h"
#include "xtiffio.h"
#include "geotiffio.h"
#include "protos.h"

void homkeyset
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
  GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                   CT_ObliqueMercator_Hotine);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "Oblique Mercator Hotine converted from LAS");
  if (ddr.valid[3])
  {
    GTIFKeySet(gtif, ProjScaleAtOriginGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[2]);
    if (ddr.proj_coef[12] == 1)
    {
      /* 'B' format image
         -------------- */

      /* Azimuth angle east of north of center line
         ------------------------------------------ */ 
      lon = ddr.proj_coef[3];
      c_decdeg (&lon, "DMS", "LON");
      GTIFKeySet(gtif, ProjAzimuthAngleGeoKey, TYPE_DOUBLE, 1, lon);

      /* Longitude of point on central meridian where azimuth occurs
         ----------------------------------------------------------- */
      lon = ddr.proj_coef[4];
      c_decdeg (&lon, "DMS", "LON");
      GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, lon);
    }
    else
    {
      /* 'A' format image
         I am not sure what these will do as the names do not match up
         very well...they are support to be the lat and long for two
         points on the center line.
         ------------------------------------------------------------- */

      /* Longitude of first point on center line
         --------------------------------------- */
      lon = ddr.proj_coef[8];
      c_decdeg (&lon, "DMS", "LON");
      GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, lon);

      /* Latitude of first point on center line
         --------------------------------------- */
      lat = ddr.proj_coef[9];
      c_decdeg (&lat, "DMS", "LAT");
      GTIFKeySet(gtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1, lat);

      /* Longitude of first point on center line
         --------------------------------------- */
      lon = ddr.proj_coef[10];
      c_decdeg (&lon, "DMS", "LON");
      GTIFKeySet(gtif, ProjFalseOriginLongGeoKey, TYPE_DOUBLE, 1, lon);

      /* Latitude of first point on center line
         --------------------------------------- */
      lat = ddr.proj_coef[11];
      c_decdeg (&lat, "DMS", "LAT");
      GTIFKeySet(gtif, ProjFalseOriginLatGeoKey, TYPE_DOUBLE, 1, lat);
    }
    /* Set the latitude of the projection origin
       ----------------------------------------- */
    lat = ddr.proj_coef[5];
    c_decdeg (&lat, "DMS", "LAT");
    GTIFKeySet(gtif, ProjOriginLatGeoKey, TYPE_DOUBLE, 1, lat);

    /* Set the false easting and false northing
       ---------------------------------------- */
    GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[6]);
    GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[7]);
  }
}
