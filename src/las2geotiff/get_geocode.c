/*****************************************************************************
NAME: GET_GEOCODE

PURPOSE:  Returns the geoTIFF code for the LAS ellipoid value passed.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "ddr.h"
#include "protos.h"

unsigned short get_geocode
(
    struct DDR ddr            /* I: LAS image file descriptor record    */
)

{
  unsigned short ellipsoid_value = 0;     /* geoTIFF code for ellipsoid    */

  switch (ddr.datum_code)
  {
    case 0: ellipsoid_value = 4008;break;  /* Clarke 1866                  */
    case 1: ellipsoid_value = 4012;break;  /* Clarke 1880                  */
    case 2: ellipsoid_value = 4004;break;  /* Bessel 1841                  */
    case 3: break; /* International 1967 has different semi-major axis     */
                   /* LAS: 6378157.5  GEO: 6378160                         */ 
    case 4: ellipsoid_value = 4022;break;  /* International 1924           */
    case 5: ellipsoid_value = 4026;break;  /* WGS 72 (NWL10D)              */
    case 6: ellipsoid_value = 4015;break;  /* Everest 1930 1937 Adjustment */
    case 7: ellipsoid_value = 4025;break;  /* WGS 66 (NWL9D)               */
    case 8: ellipsoid_value = 4019;break;  /* GRS 1980                     */
    case 9: ellipsoid_value = 4001;break;  /* Airy 1930                    */
    case 10: ellipsoid_value = 4018;break; /* Everest 1930 Modified        */
    case 11: ellipsoid_value = 4002;break; /* Airy Modified 1849           */
    case 12: ellipsoid_value = 4030;break; /* WGS 84                       */
    case 13: break; /* Translation for Southeast Asia not found            */
    case 14: ellipsoid_value = 4003;break; /* Australian National Spheroid */
    case 15: ellipsoid_value = 4024;break; /* Krassovsky 1940              */
    case 16: break; /* Translation for Hough not found                     */
    case 17: break; /* Translation for Mercury 1960 not found              */
    case 18: break; /* Translation for Modified Mercury 1960 not found     */
    case 19: break; /* Sphere has different radii (LAS:6370997  GEO:6371000*/
    case 20: ellipsoid_value = 4006;break; /* Bessel 1841 (Namibia)        */
    case 21: ellipsoid_value = 4016;break; /* Everest 1830 1967 Definition */
                                           /* (Sabah & Sarawak)            */
    case 22: ellipsoid_value = 4017;break; /* Everest 1830 1975 Definition */
                                           /* (India 1956)                 */
    case 23: break; /* Translation for Everest (Malaysia 1969) not found   */
    case 24: ellipsoid_value = 4018;break; /* Everest 1830 Modified        */
                                           /* (Malay. & Singapore 1948)    */
    case 25: break; /* Translation for Everest (Pakistan) not found        */
    case 26: ellipsoid_value = 4022;break; /* International 1924 OR Hayford*/
    case 27: ellipsoid_value = 4020;break; /* Helmert 1906                 */
    case 28: ellipsoid_value = 4021;break; /* Indonesian National Spheroid */
    case 29: ellipsoid_value = 4023;break; /* International 1967 OR        */
                                           /* South American 1969          */
    case 30: break; /* Translation for WGS60 not found                     */
  } 
  return (ellipsoid_value);
}
