/*****************************************************************************
NAME: GET_LINEAR_UNITS

PURPOSE:  Returns the geoTIFF code for the LAS projection units.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/


#include "ddr.h"
#include "protos.h"

unsigned short get_linear_units
(
    char *unitstr            /* I: Units used (in English)             */
)

{
  unsigned short unit = 0; /* geoTIFF unit code number            */
 
  upcase(unitstr); 
  if (strcmp(unitstr,"METERS") == 0)
    unit = 9001;
  else if (strcmp(unitstr,"FEET") == 0)
    unit = 9003;
  return (unit); 
}
