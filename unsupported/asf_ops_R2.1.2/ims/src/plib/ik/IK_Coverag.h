/*
 * Name: IK_Coverag.h
 *
 * Description: This file defines data stuctures and manifest
 * constants for IK_Coverage data types.
 *
 * Notes:
 *
 *-------------------------------------------------------------------------
 *
 * RCS information:
 *
 * $RCSfile$
 * $Id$
 *
 * $Log$
 * Revision 1.1  2004/02/03 03:32:54  pdenny
 * Initial revision
 *
 * Revision 5.0  1995/11/06  13:03:29  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:41:53  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.1  1995/03/21  19:08:17  jong
 * Added IK_NONE and IK_G_RANGE into IK_Loc structure.
 *
 * Revision 4.4  1995/02/13  22:19:45  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.1  1994/08/26  11:40:41  ims
 * COPIED FROM REVISION 4.0.1.1.
 *
 * Revision 4.0.1.1  1994/06/08  16:53:21  ims
 * COPIED FROM REVISION 3.1.1.1.
 *
 * Revision 3.1.1.1  1994/04/06  20:12:01  winter
 * Cleaned up in preparation for debugging. Removed rcsid, added #include
 * of IK_DataDic.h, to ensure order-independence of headers.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.2  1992/09/18  18:18:41  honce
 * Changed floats to char's for lat's and long's.  Makes structure usable
 * by more of the system.
 *
 * Revision 1.1  1992/09/10  15:24:34  honce
 * Initial revision
 *
*/

/*****************************************************************************/

#ifndef __IK_COVERAG_H__
#define __IK_COVERAG_H__

/*****************************************************************************/

/* #include directives */

/* IMS headers */
#include "IK_DataDic.h"

/*****************************************************************************/

/* Data type definitions */

/* This enumeration defines constants which represent different types
   of granule geographical locations. */

typedef enum IK_Loc {

  /* Uninitialized IK_Coverage */
  IK_EMPTY,

  /* Single latitude, longitude pair */
  IK_POINT,

  /* Bounding box */
  IK_RANGE,

  /* n-sided box */
  IK_POLYGON,

  /* Global coverage, all other fields in IK_Coverage undefined */
  IK_GLOBAL,

  /* Only used for input */
  IK_XHAIRS,

  /* Default none value */
  IK_NONE,

  /* Global Search */
  IK_G_RANGE
} IK_Loc;

/*-------------------------------------------------------------------------*/

typedef struct IK_Coverage {
  IK_Loc type;
  union IK_Area {
    struct IK_Point {
      char longitude[IK_LONGITUDE_SZ];
      char latitude[IK_LATITUDE_SZ];
    } point;
    struct IK_Range {
      char north_lat[IK_LATITUDE_SZ];
      char south_lat[IK_LATITUDE_SZ];
      char east_long[IK_LONGITUDE_SZ];
      char west_long[IK_LONGITUDE_SZ];
    } range;
    struct IK_Xhairs {
      char longitude[IK_LONGITUDE_SZ];
      char latitude[IK_LATITUDE_SZ];
      char long_range[IK_LONGITUDE_SZ];
      char lat_range[IK_LATITUDE_SZ];
    } xhairs;

    /* In this structure, latitude[0] and longitude[0] define a
       coordinate pair. */
    struct IK_Polygon {
      char latitude[IK_NPOLYSIDES][IK_LATITUDE_SZ];
      char longitude[IK_NPOLYSIDES][IK_LONGITUDE_SZ];
      char centroid_lat[IK_LATITUDE_SZ];
      char centroid_long[IK_LONGITUDE_SZ];
      char pole_inside;
    } polygon;
  } area;
} IK_Coverage;

typedef union IK_Area IK_Area;
typedef struct IK_Point IK_Point;
typedef struct IK_Range IK_Range;
typedef struct IK_Xhairs IK_Xhairs;
typedef struct IK_Polygon IK_Polygon;

/*****************************************************************************/

#endif   /* ! __IK_COVERAG_H__ */
