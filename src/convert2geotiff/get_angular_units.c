/*****************************************************************************
NAME: GET_ANGULAR_UNITS

PURPOSE: Returns the geoTIFF unit code for the angular unit in the ddr. 

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "ddr.h"
#include "protos.h"

/*PROTOTYPE*/ void upcase(char *buf);

unsigned short get_angular_units
(
    char *unitstr               /* I: Units used (in English)              */
)
{
  unsigned short unit = 0;      /* geoTIFF unit code                       */
 
  upcase(unitstr); 
  if (strcmp(unitstr,"RADIANS") == 0)
    unit = 9101;
  else if (strcmp(unitstr,"DEGREES") == 0)
    unit = 9102;
  else if (strcmp(unitstr,"SECONDS") == 0)
    unit = 9104;
  else if (strcmp(unitstr,"DMS") == 0)
    unit = 9107;
  return (unit); 
}
