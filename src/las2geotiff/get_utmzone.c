/*****************************************************************************
NAME: GET_UTMZONE

PURPOSE:  Returns the geoTIFF code for the LAS UTM zone.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/

#include "ddr.h"
#include "protos.h"

unsigned short get_utmzone
(
    struct DDR ddr            /* I: LAS image file descriptor record    */
)

{
  long las_zone;                  /* LAS UTM zone code            */ 

  unsigned short utmzone = 0;     /* geoTIFF UTM code             */

  if (ddr.zone_code != 0)
  {
    /* The UTM zone is set, so we will use it
       -------------------------------------- */
    las_zone = ddr.zone_code;
  }
  else
  {
    /* The UTM zone is not set, so we need to calculate it.
       ---------------------------------------------------- */
    las_zone = ((long)(ddr.proj_coef[1]/1000000) + 180) / 6 + 1;
    if (ddr.proj_coef[0] < 0)
    {
      /* The UTM zone is southern hemisphere so needs to be negative
         ----------------------------------------------------------- */ 
      las_zone *= -1;
    }
  }
  if ((ddr.datum_code == 5) || (ddr.datum_code == 316))
  {
    /* Base number for geoTIFF WGS72 UTM zone
       -------------------------------------- */
    utmzone = 32200;
  }
  else if ((ddr.datum_code == 12) || (ddr.datum_code == 317))
  {
    /* Base number for geoTIFF WGS84 UTM zone
       -------------------------------------- */
    utmzone = 32600;
  }
  else if ((ddr.datum_code >= 217 && ddr.datum_code <=  222))
  {
    /* Base number for geoTIFF NAD83 UTM zone
       -------------------------------------- */
    utmzone = 26900;
  }
  else if ((ddr.datum_code >= 223) && (ddr.datum_code <=242))
  {
    /* Base number for geoTIFF NAD27 UTM zone
       -------------------------------------- */
    utmzone = 26700;
  }
  else
  {
    /* Don't know how to handle other ellipsoids for UTM
       ------------------------------------------------- */
    return (0);
  }
  if (las_zone < 0)
  {
    /* this is a southern hemisphere zone so it's geoTIFF code
       is 100 higher than the northern hemisphere zones
       (subtracting the las_zone because it is negative)
       ------------------------------------------------------- */
    utmzone += 100 - las_zone;
  }
  else
  {
    utmzone += las_zone;
  }
  return (utmzone);
} 
