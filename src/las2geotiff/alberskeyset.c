/****************************************************************
FUNCTION NAME: alberskeyset

SYNTAX:  alberskeyset (ddr, gtif); 

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    ddr      struct DDR         I: LAS image file descriptor record  
    gtif       GTIF *           I/O: GeoKey-level descriptor          

DESCRIPTION:
    Set the geoTIFF keys for Albers Equal Area projection.

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

void alberskeyset
(
struct DDR ddr,            /* I: LAS image file descriptor record    */
GTIF *gtif                 /* I/O: GeoKey-level descriptor           */
) 

{
  double lat = 0;          /* Latitude                            */
  double lon = 0;          /* Longitude                           */

  GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
  GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
  GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);

  /* Set the major and minor axis values
     ----------------------------------- */
  set_major_minor(ddr,gtif);

  /* Set the projection as user defined
     ---------------------------------- */
  GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767);
  GTIFKeySet(gtif, ProjectionGeoKey, TYPE_SHORT, 1, 32767);
  if (ddr.valid[4])
    GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, 
                   get_linear_units(ddr.proj_units));
  GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_AlbersEqualArea);
  GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
                   "Albers Equal Area converted from LAS");
  if (ddr.valid[3])
  {
    /* Set the standard parallels
       -------------------------- */
    lat = ddr.proj_coef[2];
    c_decdeg (&lat, "DMS", "LAT");
    GTIFKeySet(gtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1, lat);
    lat = ddr.proj_coef[3];
    c_decdeg (&lat, "DMS", "LAT");
    GTIFKeySet(gtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1, lat);

    /* Set the longitude of the central meridian
       ----------------------------------------- */
    lon = ddr.proj_coef[4];
    c_decdeg (&lon, "DMS", "LON");
    GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, lon);

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
