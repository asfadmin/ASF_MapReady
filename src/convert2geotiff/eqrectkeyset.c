/****************************************************************
FUNCTION NAME: eqrectkeyset

SYNTAX:  eqrectkeyset (ddr, gtif); 

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    ddr      struct DDR         I: LAS image file descriptor record   
    gtif       GTIF *           I/O: GeoKey-level descriptor         

DESCRIPTION:
    Set the geoTIFF keys for Equirectangular projection.

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

void eqrectkeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  double lat = 0;          /* Latitude                            */
  double lon = 0;          /* Longitude                           */
  double sphere = 0.0;     /* radius of the reference sphere      */

  GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_Equirectangular);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "Equirectangular converted from LAS");

  GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
  GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
  GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, 
                   get_linear_units(ddr.proj_units));
  GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
  GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, get_geocode(ddr));

  /* Set the projection as user defined
     ---------------------------------- */
  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, ProjectionGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, 
                   get_linear_units(ddr.proj_units));
  if (ddr.valid[3])
  {
    /* Set the radius of the sphere
       ---------------------------- */
    get_sphere(ddr,&sphere);
    GTIFKeySet(gtif,GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, sphere);
    GTIFKeySet(gtif,GeogSemiMinorAxisGeoKey, TYPE_DOUBLE, 1, sphere);

    /* Set the longitude of the reference
       ---------------------------------- */
    lon = ddr.proj_coef[4];
    c_decdeg (&lon, "DMS", "LON");
    GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, lon);

    /* Set the latitude of the reference 
       --------------------------------- */
    lat = ddr.proj_coef[5];
    c_decdeg (&lat, "DMS", "LAT");
    GTIFKeySet(gtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1, lat);

    /* Set the false easting and false northing
       ---------------------------------------- */
    GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[6]);
    GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, ddr.proj_coef[7]);
  }
}
