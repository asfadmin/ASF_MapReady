/************************************************************************
*
*     **************************
*     *                        *
*     * SPATIAL_QUERY_ROUTINES *    class spec
*     *                        *
*     **************************
*
* Source Code Version: "@(#)spatial_query_routines.h	1.1  11/22/93"
*
* Purpose: This package contains routines to determe if two regions on the
*          Earth's surface overlap.  Two regions are said to overlap if any
*          point on the boundary of either region, or in the interior of either
*          region is common to both regions.  The spatial query routines can
*          currently handle three types of regions: single points, max/min
*          latitudes and longitudes, and regions defined by connecting straight
*          lines between PT_MAX (only tested with 4) latitude/longitude pairs.
*          The two regions are usually referred to as the user's region and the
*          dataset region.
*
* Modifications:
* G. Tate  19-April-1993 (initial version)
* K. Stephens May-June-1993 (major modifications)
* G. Tate, 22-Nov-1993 ( put under configuration management)
******************************************************************************/
#ifndef SPATIAL_QUERY_DEFINED
#define SPATIAL_QUERY_DEFINED

#include <math.h>

enum Geographical_Groups {POINT,GLOBAL,WORLD,POLYGON,NULL_GEO_TYPE};
enum Map_Projections {PLATE_CARREE,MILLER,MERCATOR,POLAR_STEREOGRAPHIC,POLAR_ORTHOGRAPHIC,EQUATORIAL_ORTHOGRAPHIC,OBLIQUE_ORTHOGRAPHIC,NO_PROJECTION};

/* 
** PT_MAX is the number of points used to define a region 
*/
#define PT_MAX 4

typedef struct Region
{
  enum Geographical_Groups  geographic_type;
  float                     latitude[PT_MAX];
  float                     longitude[PT_MAX];
  float			    tangent_latitude; /* used with oblique projection*/
  float			    tangent_longitude;/* used with mercator projection*/
  char                      pole_included;    /* 'N', 'S', or ' ' */
  enum Map_Projections      projection;
} REGION;

#ifndef INFINITY
#define INFINITY 100000
#endif

/* 
** DEFAULT_PROJECTION is very dependent on the dataset region.  This code
** was developed using POLAR_STEREOGRAPHIC for the default projection.   
*/
#define DEFAULT_PROJECTION PLATE_CARREE
#define DEFAULT_DATASET_PROJECTION POLAR_STEREOGRAPHIC

#define e  2.718281828459
#define pi 3.145926535898

static double deg2rad = (2.0 * pi) / 360.0; 

/*
** MACROS
*/
#define sind(x) sin((x) * deg2rad)
#define cosd(x) cos((x) * deg2rad)
#define atan2d(y,x) (atan2((y), (x)) / deg2rad)


/*
** Macro used to initialize region data structure
*/ 
#define DEFAULT_REGION(region) do {                       \
    region.geographic_type = NULL_GEO_TYPE;               \
    region.projection = NO_PROJECTION;                    \
    region.latitude[0] = region.latitude[1] = INFINITY;   \
    region.latitude[2] = region.latitude[3] = INFINITY;   \
    region.longitude[0] = region.longitude[1] = INFINITY; \
    region.longitude[2] = region.longitude[3] = INFINITY; \
    region.tangent_latitude = 0.0;                        \
    region.tangent_longitude = 0.0;                       \
    region.pole_included = ' ';                           \
} while(0)

/*
** The following are used for logging/printing errors
*/
#define SIGN -            /* This may need to be changed in some applications */
#define MAX_ERROR 16      /* This needs to be changed if new errors are added */
#define MAX_ERR_STR_LEN 80/* Max length of error string (used with status log)*/

#define E_MAX -1
#define E_PT_IN_REG -2
#define E_ARCTANH -3
#define E_EQU_ORTH_TO_LAT_LON -4
#define E_POLAR_ORTH_TO_LAT_LON -5
#define E_CHECK_MDPT -6
#define E_REG_IN_REG -7
#define E_NULL_DATASET-8
#define E_BAD_REGION_TYPE -9
#define E_BAD_MERC_LAT -10
#define E_BAD_POLAR_STEREO_LAT -11
#define E_BAD_PROJ_FOR_GIVEN_REG -12
#define E_MIN -13
#define E_DIFF_HEMISPHERES -14
#define E_USERS_REGION -15
#define E_USER_REG_OVER_EQ -16

/*
** The error array holds all of the error messages available.  The first
** message in this array is invalid, errors must start at SIGN 1.
*/
static char *error_array[] = {
  "invalid error number",          /* the errors are indexed starting at 1 */
  "in Max(), returns negative infinity",
  "in pt_in_region(), unknown projection type",
  "in arctanh(), value of x is greater then 1.0",
  "in equatorial_orthographic_to_lat_lon(), divide by zero",
  "in polar_orthographic_to_lat_lon(), divide by zero",
  "in check_pts_along_lat(), divide by zero",
  "in region_in_region(), invalid projection type found",
  "in do_regions_overlap(), null dataset region detected",
  "invalid region type found.",
  "in mercator_transform(), invalid latitude detected",
  "in convert_pts(), invalid latitude detected",
  "invalid projection type found",
  "in Min(), returns positive infinity",
  "regions are located in different hemispheres",
  "nsidc_initial_checks() detected error in the user's region",
  "nsidc_initial_checks() user's region crosses equator"
};

/******************************************************************************
*
*     ************************
*     *                      *
*     *  SPLIT_WORLD_REGION  *  method spec 
*     *                      *
*     ************************
*
* Purpose:  This function is used to split world regions that overlap the
*           equator into two regions.   The region above the equator is
*           returned in the "above_eq_region" structure, while the region
*           below the equator is returned in the "below_eq_region" structure.
*           If the original region is completely defined above or below the
*           equator, the "below_eq_region" will be a null_region.
*
*           NOTE: The "above_eq_region" might or might not be modified.
*                 However, the "below_eq_region" is ALWAYS modified.
*
* Called by: inventory_definition__get_analysis_result,
*            granule__verify_position_match
******************************************************************************/
void spatial_query_routines__split_world_region
   ( REGION
        *above_eq_region,
     REGION
        *below_eq_region );

/*****************************************************************************
*
*     ************************
*     *                      *
*     * NSIDC_INITIAL_CHECKS *  method spec
*     *                      *
*     ************************
*
* Purpose:  This procedure checks to see if both regions are completely 
*           defined in the give projection.  This procedure is non-generic
*           and is used with POLAR_STEREOGRAPHIC and MERCATOR projections.
*
* Called by: granule__verify_position_match,
*            spatial_query_routines__do_regions_overlap
*            inventory_definitions__get_analysis_results
******************************************************************************/
int spatial_query_routines__nsidc_initial_checks
  (REGION 
     user_region, 
   REGION 
     dataset_region); 

/*****************************************************************************
*
*     ************************
*     *                      *
*     *   IS_REGION_NULL     *  method spec
*     *                      *
*     ************************
*
* Purpose:  This procedure checks to see if a region is a "NULL" region.
*           A NULL region is a region that has not been defined (i.e. the
*           default).  One is returned if region is NULL, otherwise zero is
*           returned.
*
* Called by: granule__verify_position_match,
*            spatial_query_routines__do_regions_overlap
*            inventory_definition__get_analysis_result
******************************************************************************/
int spatial_query_routines__is_region_null (REGION region);

/*****************************************************************************
*
*     ************************
*     *                      *
*     *  DO_REGIONS_OVERLAP  *  method spec
*     *                      * 
*     ************************
*
* Purpose:  Determines whether two regions overlap. One is returned if
*           regions overlap, otherwise zero is returned.
*
* Called by: granule__verify_position_match,
*            inventory_definitions__get_analysis_results
******************************************************************************/
int spatial_query_routines__do_regions_overlap
  (REGION 
     user_region, 
   REGION 
     dataset_region); 

/*****************************************************************************
*
*     *********************
*     *                   *
*     *  POINT_IN_POINT   *  method spec 
*     *                   *
*     *********************
*
* Purpose: Determines whether two latitude/longitude points are equal to
*          each other.  One is returned if regions overlap, otherwise zero (or
*          a negative error number) is returned.
*
* Called by: spatial_query_routines__do_regions_overlap
******************************************************************************/
int spatial_query_routines__point_in_point
  (REGION          
     region1, 
   REGION
     region2); 

/******************************************************************************
*
*     *********************
*     *                   *
*     *  POINT_IN_WORLD   *  method spec 
*     *                   *
*     *********************
*
* Purpose: Determines whether a latitude/longitude point is inside (or
*          on the boundary of) a region defined by maximum/minimum latitudes
*          and maximum/minimum longitudes.  One is returned if the regions
*          overlap, otherwise zero (or a negative error number) is returned.
*
* Called by: spatial_query_routines__do_regions_overlap
******************************************************************************/
int spatial_query_routines__point_in_world
  (REGION          
     point_region, 
   REGION
     world_region);

/******************************************************************************
*
*     ************************
*     *                      *
*     *   POINT_IN_POLYGON   *  method spec 
*     *                      *
*     ************************
*
* Purpose: Determines whether a latitude/longitude point is inside (or on the
*          boundary of) a region defined by PT_MAX (probably 4) lat/lon pairs.
*          One is returned if the regions overlap, otherwise zero (or a
*          negative error number) is returned.
*
* Called by: spatial_query_routines__do_regions_overlap
******************************************************************************/
int spatial_query_routines__point_in_polygon
  (REGION          
     point_region, 
   REGION
     polygon_region);

/******************************************************************************
*
*     **********************
*     *                    *
*     *   WORLD_IN_WORLD   *  method spec 
*     *                    *
*     **********************
*
* Purpose: Determines whether a user's geographical area, maximum/minimum
*          latitude/longitude points, overlaps a dataset whose geographical
*          description is maximum/minimum latitude/longitude points.  One is
*          returned if regions overlap, otherwise zero (or a negative error
*          number) is returned.
*
* Called by: spatial_query_routines__do_regions_overlap
******************************************************************************/
int spatial_query_routines__world_in_world
  (REGION          
     region1, 
   REGION
     region2);

/******************************************************************************
*
*     **********************
*     *                    *
*     *  WORLD_IN_POLYGON  *  method spec 
*     *                    *
*     **********************
*
* Purpose: Determines whether a region defined by maximum/minimum lat/lon
*          points overlaps a region defined by PT_MAX (probably 4) lat/lon
*          points.  One is returned if the regions overlap, otherwise zero
*          (or a negative error number) is returned.
*
* Called by: spatial_query_routines__do_regions_overlap
******************************************************************************/
int spatial_query_routines__world_in_polygon
  (REGION          
     world_region, 
   REGION
     polygon_region);

/******************************************************************************
*
*     ************************
*     *                      *
*     *  POLYGON_IN_POLYGON  *  method spec 
*     *                      *
*     ************************
*
* Purpose: Determines whether a user's geographical area, either 4 corner
*          points specified on a defined map projection, overlaps a dataset
*          whose geographical description is 4 corner points specified on a
*          defined map projection.  One is returned if regions overlap,
*          otherwise zero, (or a negative error number) is returned.
*
* Called by: spatial_query_routines__do_regions_overlap
******************************************************************************/
int spatial_query_routines__polygon_in_polygon
  (REGION          
     region1, 
   REGION
     region2);

/*************************************************************************
*
*       ********************
*       *                  *
*       *  generate_error  *  method spec 
*       *                  *
*       ********************
*
* Purpose:  This function is used to print errors to standard error
*           or to status_log.
*
* Called by: granule__verify_position_match,
*            inventory_definitions__get_analysis_results
**************************************************************************/
void spatial_query_routines__generate_error( int err_num );

#endif
