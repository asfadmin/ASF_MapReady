/*****************************************************************************
*
*     **************************
*     *                        *
*     * SPATIAL_QUERY_ROUTINES *    object body
*     *                        *
*     **************************
*
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
******************************************************************************/
static char Spatial_Query_ID[] = "@(#)spatial_query_routines.c	1.2  7/9/93";

#include <stdio.h>
#include <math.h>
#include "spatial_query_routines.h"
#include "sqr_function_prototypes.h"

/*
** RESOLUTION determines how close two points need to be to be considered
** the same point.  (approx resolution in minutes +- 7.0 minutes)
*/
#define RESOLUTION 15.0

/*
** EQUAL_PERCISION is used in the eq() function.  If this is defined,
** then if two floats are less than PERCISION away from each other, they
** are considered equal.  
#define EQUAL_PERCISION
#define PERCISION 0.001
*/

/* 
** If status_log is begin used, STATUS_LOG must be defined 
*/
/*
#define STATUS_LOG  
*/


/*
** Global variables
*/
static int not_visible  = -32767;  /* - (2^16) */
/* static double deg2rad   = 2.0 * 3.1415926535898 / 360.0;  -- in a .h file?*/
static float scale      = 6378.145;
static int error_code = 0;  /* keeps track of errors generated at low levels */
static float MERC_MIN_LON = 0.0; /* used in Mercator calculations */

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
*           If the regions are okay to process, one is returned.  Otherwise
*           an error number is returned.
*
* Called by: inventory_defintion__get_analysis_result
*	     granule__verify_position_match
******************************************************************************/
int spatial_query_routines__nsidc_initial_checks
  (REGION
     user_region,
   REGION
     dataset_region)

{ /* NSIDC_INITIAL_CHECKS */

  int pos_user_lat = 0;
  int pos_dataset_lat = 0;
  int neg_user_lat = 0;
  int neg_dataset_lat = 0;
  int user_region_crosses_eq = 0;
  int dataset_region_crosses_eq = 0;
  int i;

  /*
  ** The first thing we need to check is if the dataset_region is null.  This
  ** occurs when we are checking to see if the user's region is okay and the
  ** dataset is ssmi_orbital_temps (i.e. entire world)
  */
  if ( spatial_query_routines__is_region_null(dataset_region) ) {

     /*
     ** If the user's region is a point, we don't need to check anything else.
     */
     if ( user_region.geographic_type == POINT ) return 1;

     /*
     ** Make sure that user's region is defined in only one hemisphere
     ** (we assume that the dataset_region is defined in POLAR_STEREOGRPHIC)
     */
     for(i=0; i<PT_MAX ; i++) {

       /*
       ** We need to be careful that we don't confuse INFINITY with
       ** a positive latitude for regions defined by only 2 latitude
       ** points. (i.e. region's of geographic type WORLD have their
       ** 3rd and 4th latitude points defined as INFINITY...or maybe 
       ** not defined at all! )
       */
       if ( !( user_region.geographic_type == WORLD && i > 1 ) ) { 
            if ( user_region.latitude[i] < 0.0 ) neg_user_lat = 1;  
            if ( user_region.latitude[i] > 0.0 ) pos_user_lat = 1;  
       }
     }

     if ( neg_user_lat && pos_user_lat )  return E_USER_REG_OVER_EQ;

     /*
     ** If the user's region is defined in the MERCATOR projection,
     ** then latitude values about 85 degrees and below -85 degrees
     ** make no sense.  So, let's check for them.
     */
     if ( user_region.projection == MERCATOR ) {
              
         if ( Max( PT_MAX, user_region.latitude ) > 85.0 )
                  return E_USERS_REGION;

         if ( Min( PT_MAX, user_region.latitude ) < -85.0 )
                  return E_USERS_REGION;

      }

  /* user's region is okay */
  return 1;
  }

  /* 
  ** If the regions are of type: WORLD/WORLD, POINT/WORLD, WORLD/POINT,
  ** or POINT/POINT we don't worry about any problems due to invalid 
  ** projection types.
  */
  if ( user_region.geographic_type == WORLD &&
       dataset_region.geographic_type == WORLD ) return 1;
  
  if ( user_region.geographic_type == POINT &&
       dataset_region.geographic_type == WORLD ) return 1;
  
  if ( user_region.geographic_type == WORLD &&
       dataset_region.geographic_type == POINT ) return 1;
  
  if ( user_region.geographic_type == POINT &&
       dataset_region.geographic_type == POINT ) return 1;

  /*
  ** At this point, only these possible problem scenarios exist: 
  ** POLYGON/POLYGON, WORLD/POLYGON, POINT/POLYGON, POLYGON/WORLD, or 
  ** POLYGON/POINT.  Any of these cominations could be invalid depending 
  ** on the projection and the location of the points making up the region.  
  */ 

    /*
    ** If either region's projection is POLAR_STEREOGRAPHIC
    ** make sure that both regions are in the same hemisphere
    */
    if ( ( user_region.projection == POLAR_STEREOGRAPHIC ) ||
           ( dataset_region.projection == POLAR_STEREOGRAPHIC ) ) {

          /*
          ** determine if dataset region or user region crosses
          ** equator.
          */
          for(i=0; i<PT_MAX ; i++) {

              /*
              ** We need to be careful that we don't confuse INFINITY with
              ** a positive latitude for regions defined by only 1 or 2 
              ** latitude points. (i.e. region's of geographic type POINT 
              ** have their 2nd, 3rd and 4th latitude points defined as 
              ** INFINITY...or maybe  not defined at all! )
              */
              if ( user_region.geographic_type == POINT ) {

                  if ( user_region.latitude[0] < 0.0 ) neg_user_lat = 1;  
                  if ( user_region.latitude[0] > 0.0 ) pos_user_lat = 1;  
              }
              else if ( !( user_region.geographic_type == WORLD && i>1 ) ) {
 
                  if ( user_region.latitude[i] < 0.0 ) neg_user_lat = 1;  
                  if ( user_region.latitude[i] > 0.0 ) pos_user_lat = 1;  
              }
 
              /*
              ** We need to be careful that we don't confuse INFINITY with
              ** a positive latitude for regions defined by only 1 or 2 
              ** latitude points. (i.e. region's of geographic type POINT 
              ** have their 2nd, 3rd and 4th latitude points defined as 
              ** INFINITY...or maybe  not defined at all! )
              */
              if ( dataset_region.geographic_type == POINT ) {

                  if ( dataset_region.latitude[0] < 0.0 ) neg_dataset_lat = 1;  
                  if ( dataset_region.latitude[0] > 0.0 ) pos_dataset_lat = 1;  
              }
              else if ( !( dataset_region.geographic_type == WORLD && i>1 ) ) { 

                  if ( dataset_region.latitude[i] < 0.0 ) neg_dataset_lat = 1; 
                  if ( dataset_region.latitude[i] > 0.0 ) pos_dataset_lat = 1;
              }

          }  /* end for loop */    

          if ( neg_user_lat && pos_user_lat ) 
                          return E_USER_REG_OVER_EQ;

          if ( neg_dataset_lat && pos_dataset_lat ) 
                          return E_BAD_PROJ_FOR_GIVEN_REG;

          /* 
          ** Check to see if regions are in different hemispheres
          */
          if ( ( neg_user_lat && pos_dataset_lat ) ||
               ( neg_dataset_lat && pos_user_lat ) ) return E_DIFF_HEMISPHERES;

    }  /* end if check for POLAR_STEREO projection */


    /*
    ** If the user's region is defined in the MERCATOR projection,
    ** then latitude values about 85 degrees and below -85 degrees
    ** make no sense.  So, let's check for them.
    */
    if ( user_region.projection == MERCATOR ) {
              
         if ( Max( PT_MAX, user_region.latitude ) > 85.0 )
                  return E_BAD_PROJ_FOR_GIVEN_REG;
              
         if ( Min( PT_MAX, user_region.latitude ) < -85.0 )
                  return E_BAD_PROJ_FOR_GIVEN_REG;

    }

    /*
    ** If the dataset's region is defined in the MERCATOR projection,
    ** then latitude values about 85 degrees and below -85 degrees
    ** make no sense.  So, let's check for them.
    */
    if ( dataset_region.projection == MERCATOR ) {

         if ( Max( PT_MAX, dataset_region.latitude ) > 85.0 )
                  return E_BAD_PROJ_FOR_GIVEN_REG;
              
         if ( Min( PT_MAX, dataset_region.latitude ) < -85.0 )
                  return E_BAD_PROJ_FOR_GIVEN_REG;
     }

/* at this point everything is OK */
return 1;

} /* NSIDC_INITIAL_CHECKS */

/******************************************************************************
*
*     ************************
*     *                      *
*     *  SPLIT_WORLD_REGION  *  method body
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
* Called by: inventory_defintion__get_analysis_result
*	     granule__verify_position_match
******************************************************************************/
void spatial_query_routines__split_world_region
   ( REGION
        *above_eq_region,
     REGION
        *below_eq_region )

{ /* SPLIT_WORLD_REGION */

  int i;

  /* initialize below_eq_region */
  DEFAULT_REGION( (*below_eq_region) );

  if ( above_eq_region->geographic_type == WORLD ) {

    /*
    ** Check to see if the original region crosses the equator
    */
    if ( above_eq_region->latitude[0] < 0.0 &&
         above_eq_region->latitude[1] > 0.0 )  {

       /*
       ** Set the below_eq_region equal to the above_eq_region.  Note, some
       ** of these assignments might not make any sense, (i.e. pole_included)
       ** but we'll do them anyway.
       */
       below_eq_region->geographic_type = above_eq_region->geographic_type;
       below_eq_region->tangent_latitude = above_eq_region->tangent_latitude;
       below_eq_region->pole_included = above_eq_region->pole_included;
       below_eq_region->projection = above_eq_region->projection;

       /* 
       ** Copy over longitudes 
       */ 
       for(i=0; i < PT_MAX ; i++ ) {
          below_eq_region->longitude[i] = above_eq_region->longitude[i];
       }

       /*
       ** Set below_eq_region's minimum latitude equal to above_eq_region's
       ** minimum latitude.
       */
       below_eq_region->latitude[0] = above_eq_region->latitude[0];

       /*
       ** Set below_eq_region's maximum latitude equal to zero (equator)
       */
       below_eq_region->latitude[1] = 0.0;

       /*
       ** Set above_eq_region's minimum latitude equal to zero (equator)
       */
       above_eq_region->latitude[0] = 0.0;

    }  /* if crosses equator */

  }  /* if WORLD geo type */

} /* SPLIT_WORLD_REGION */
 
/*****************************************************************************
*
*     ************************
*     *                      *
*     *  DO_REGIONS_OVERLAP  *  method body 
*     *                      *
*     ************************
*
* Purpose:  Determines whether two regions overlap.  One is returned if
*           regions overlap, otherwise zero (or a negative error number) is 
*           returned.
*
* Called by: granule__verify_position_match,
*            inventory_definitions__get_analysis_results
******************************************************************************/
int spatial_query_routines__do_regions_overlap
  (REGION
     user_region,
   REGION
     dataset_region)

{  /*  DO_REGIONS_OVERLAP */

  enum Geographical_Groups user_type, dataset_type;
  int overlap = 0;
  int status = 1;

/*
** This function is basically just a case statement used to determine 
** which spatial_query_routine to call. (point_in_point, point_in_world,
** point_in_polygon, world_in_world, world_in_polygon, or polygon_in_polygon)
*/

  /*
  ** If user_region is null, then entire world is the user's region
  */
  if ( spatial_query_routines__is_region_null(user_region) ) return 1;

  /*
  ** Make sure the dataset region is not NULL
  */
  if ( spatial_query_routines__is_region_null(dataset_region) ) 
       return E_NULL_DATASET; 

  /*
  ** If no projection was defined for the user's region, we'll use
  ** the default projection.  
  */
  if ( user_region.projection == NO_PROJECTION )
       user_region.projection = DEFAULT_PROJECTION;

  /*
  ** If the user's region is of type GLOBAL, then it automatically overlaps
  */
  if ( user_region.geographic_type == GLOBAL ) return 1;

  /*
  ** Set up MERC_MIN_LON.  It is assumed that the user's region and the
  ** dataset regions have been initialized.  MERC_MIN_LON is only used with 
  ** regions with projection type MERCATOR.  If the two region's projections 
  ** are both MERCATOR, we'll use the default value for MERC_MIN_LON (0.0)
  */
  if ( user_region.projection != dataset_region.projection ) {

      if ( user_region.projection == MERCATOR ) 
                MERC_MIN_LON = user_region.tangent_longitude;

      else if ( dataset_region.projection == MERCATOR )
                MERC_MIN_LON = dataset_region.tangent_longitude;
  
  }
 
  user_type = user_region.geographic_type;
  dataset_type = dataset_region.geographic_type;

  /*
  ** Determine which routine should handle checking the regions, then
  ** pass the regions to this routine
  */
  switch ( user_type ) { 

       case POINT :

            switch ( dataset_type ) {

      	      case POINT : overlap = spatial_query_routines__point_in_point
                                      (user_region,dataset_region);
                           break;

       	      case WORLD : overlap = spatial_query_routines__point_in_world
                                      (user_region,dataset_region);
                           break;

       	      case POLYGON : overlap = spatial_query_routines__point_in_polygon
                                       (user_region,dataset_region);
                             break;

              case NULL_GEO_TYPE : return E_BAD_REGION_TYPE;
              case GLOBAL        : return E_BAD_REGION_TYPE;
              default            : return E_BAD_REGION_TYPE;
            }
            break;

       case WORLD : 

            switch ( dataset_type ) {
	  			     /* notice order of parameters */
              case POINT : overlap = spatial_query_routines__point_in_world
                                      (dataset_region,user_region);
                           break;

       	      case WORLD : overlap = spatial_query_routines__world_in_world 
                                      (user_region,dataset_region);
                           break;

       	      case POLYGON : overlap = spatial_query_routines__world_in_polygon
                                        (user_region,dataset_region);
                             break;

              case NULL_GEO_TYPE : return E_BAD_REGION_TYPE;
              case GLOBAL        : return E_BAD_REGION_TYPE;
              default            : return E_BAD_REGION_TYPE;
            }
            break;

       case POLYGON :

            switch ( dataset_type ) {
	 		             /* notice order of parameters */
      	      case POINT : overlap = spatial_query_routines__point_in_polygon
                                      (dataset_region,user_region);
                           break;

				     /* notice order of parameters */
       	      case WORLD : overlap = spatial_query_routines__world_in_polygon 
                                      (dataset_region,user_region); 
                           break;

	      case POLYGON: overlap = spatial_query_routines__polygon_in_polygon
				       (user_region,dataset_region);
                            break;
 
 	      case NULL_GEO_TYPE : return E_BAD_REGION_TYPE; 
	      case GLOBAL        : return E_BAD_REGION_TYPE; 
	      default            : return E_BAD_REGION_TYPE; 
            } 
            break;

	case NULL_GEO_TYPE : return E_BAD_REGION_TYPE; 
	case GLOBAL        : return E_BAD_REGION_TYPE;
        default            : return E_BAD_REGION_TYPE;

  } /* end switch ( r1_type ) */

  /*
  ** If error_code equals 0, this indicates that no errors were generated
  ** at the lowest level of computation (e.g. in projection transformations).
  ** However, error_code does NOT reflect errors that occur at higher levels.
  ** Errors occuring at higher levels will be returned in overlap.  The
  ** assumption here is that lower level errors have priority over the higher
  ** level errors (they might cause higher level errors to occur)
  */ 
  if ( error_code != 0 ) return error_code;
  else return overlap;

}  /*  DO_REGIONS_OVERLAP */

/*****************************************************************************
*
*     *********************
*     *                   *
*     *  POINT_IN_POINT   *  method body
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
     region2)

{  /*  POINT_IN_POINT  */

  float  lat1_pt,long1_pt;
  float  lat2_pt,long2_pt;
  int    overlap = 0;
  int    i;

   /* 
   ** Verify that regions are of correct type 
   */
   if ( region1.geographic_type != POINT ||
        region2.geographic_type != POINT ) return E_BAD_REGION_TYPE;

   /* 
   ** Get point information from regions 
   */
   lat1_pt = region1.latitude[0];
   long1_pt = region1.longitude[0];

   lat2_pt = region2.latitude[0];
   long2_pt = region2.longitude[0];

   /* 
   ** Compare to determine if Regions overlap 
   */
   overlap = 0;    /* initialize to no overlap */

   if (eq(lat1_pt,lat2_pt) && eq(long1_pt,long2_pt)) overlap = 1;

 return overlap;

} /*  POINT_IN_POINT  */

/******************************************************************************
*
*     *********************
*     *                   *
*     *  POINT_IN_WORLD   *  method body
*     *                   * 
*     *********************                                                 
*
* Purpose: Determines whether a latitude/longitude point is inside (or
*          on the boundary of) a region defined by maximum/minimum latitudes
*          and maximum/minimum longitudes.  One is returned if the regions 
*          overlap, otherwise zero (or a negative error number) is returned. 
*
* Called by: spatial_query_routines__do_regions_overlap 
*            spatial_query_routines__world_in_polygon
******************************************************************************/
int spatial_query_routines__point_in_world
  (REGION          
     point_region,
   REGION
     world_region)

{  /*  POINT_IN_WORLD  */

  float  max_lat1, min_lat1, max_long1, min_long1;
  float  lat_pt,long_pt;
  int    overlap = 0;
  int    i;
   
  /* 
  ** Verify that regions are of correct type 
  */
  if ( point_region.geographic_type != POINT ||
       world_region.geographic_type != WORLD ) return E_BAD_REGION_TYPE;

  /* 
  ** Get point information from point region 
  */
  lat_pt = point_region.latitude[0];
  long_pt = point_region.longitude[0];

  /* 
  ** Get max and min latitude and longitude from world region 
  */
  min_lat1 = world_region.latitude[0];
  max_lat1 = world_region.latitude[1];
  min_long1 = world_region.longitude[0];
  max_long1 = world_region.longitude[1];

  /* 
  ** Compare to determine if Regions overlap 
  */
  overlap = 1;     /* assume regions overlap */

  if (max_lat1 < lat_pt) overlap = 0;
  if (min_lat1 > lat_pt) overlap = 0;
  if (max_long1 < long_pt) overlap = 0;
  if (min_long1 > long_pt) overlap = 0;

 return overlap;

} /*  POINT_IN_WORLD  */

/******************************************************************************
*
*     ************************
*     *                      *
*     *   POINT_IN_POLYGON   *  method body 
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
    polygon_region)

{  /* POINT_IN_POLYGON */

  float polygon_region_x[PT_MAX];
  float polygon_region_y[PT_MAX];
  int reg_def_by_pts = 0;
 
 /* 
 ** Verify that regions are of correct type 
 */
 if ( point_region.geographic_type != POINT ||
      polygon_region.geographic_type != POLYGON ) return E_BAD_REGION_TYPE;

 /*
 ** convert polygon_region latitudes and longitudes into x/y points. 
 ** (this is needed for the call to check_points)
 */
 convert_pts(polygon_region.latitude, polygon_region.longitude,
             polygon_region_x, polygon_region_y, 
             polygon_region.projection, PT_MAX);

 if ( error_code != 0 ) return error_code;

 /*
 ** determine the region defined by the polygon_pts
 */
 reg_def_by_pts = determine_region(polygon_region);
  
 /*
 ** check_point() handles the actual calculation.  
 */
 return ( check_point(point_region.latitude[0], point_region.longitude[0], 
                      polygon_region_x, polygon_region_y, 
                      polygon_region.projection, point_region.projection,
                      reg_def_by_pts) );

}  /* POINT_IN_POLYGON */

/******************************************************************************
*
*     **********************
*     *                    *
*     *   WORLD_IN_WORLD   *  method body
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
     region1,  /* latitude and longitude values supplied by user  */    
   REGION
     region2)  /* latitude and longitude values associated with dataset */


{  /*  WORLD_IN_WORLD  */

  float  max_lat1, min_lat1, max_long1, min_long1;
  float  max_lat2, min_lat2, max_long2, min_long2;
  int    overlap, i;

 /* 
 ** Verify that regions are of correct type 
 */
 if ( region1.geographic_type != WORLD ||
      region2.geographic_type != WORLD ) return E_BAD_REGION_TYPE;

 /* 
 ** Get max and min latitude and longitude for region 1
 */
 min_lat1 = region1.latitude[0];
 max_lat1 = region1.latitude[1];
 min_long1 = region1.longitude[0];
 max_long1 = region1.longitude[1];

 /* 
 ** Get max and min latitude and longitude for region 2 
 */ 
 min_lat2 = region2.latitude[0];
 max_lat2 = region2.latitude[1];
 min_long2 = region2.longitude[0];
 max_long2 = region2.longitude[1];

 /* 
 ** Compare to determine if Regions overlap 
 */
 overlap = 1;    /* assume regions overlap */

 if (max_lat2 < min_lat1) overlap = 0;
 if (min_lat2 > max_lat1) overlap = 0;
 if (max_long2 < min_long1) overlap = 0;
 if (min_long2 > max_long1) overlap = 0;

 return overlap;

} /*  WORLD_IN_WORLD  */

/******************************************************************************
*
*     **********************
*     *                    *
*     *  WORLD_IN_POLYGON  *  method body 
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
     polygon_region)

{  /* WORLD_IN_POLYGON */
  
  REGION pt_region;             /* used to check polygon corner points */ 
  int reg_def_by_world_pts = 1; /* default to inside */ 
  int reg_def_by_poly_pts = 1;  /* default to inside */
  int i;
  int overlap = 0;

  /* initialize pt_region */
  DEFAULT_REGION(pt_region);
  pt_region.geographic_type = POINT;
 
  /* 
  ** Verify that regions are of correct type 
  */
  if ( world_region.geographic_type != WORLD ||
       polygon_region.geographic_type != POLYGON ) return E_BAD_REGION_TYPE;

   /*
   ** The first check we'll do is to see if any of the polygon's corner
   ** points are inside the world_region.  We do this check here because
   ** we can call the point_in_world routine and save duplicating code.
   */
   for(i=0; i < PT_MAX ; i++) {

      pt_region.latitude[0] = polygon_region.latitude[i];
      pt_region.longitude[0] = polygon_region.longitude[i];
       
      overlap = spatial_query_routines__point_in_world
                        (pt_region,world_region);

      if ( overlap != 0 ) return (overlap);

   } 
        
   /*
   ** set up world_region.latitude array so that it contains: min_lat, max_lat,
   ** max_lat, min_lat.  This is necessary for the call to check_world_region.
   */
   world_region.latitude[2] = world_region.latitude[1];
   world_region.latitude[3] = world_region.latitude[0];
   
   /*
   ** set up world_region.longitude array so that it contains: min_lon, min_lon,
   ** max_lon, max_lon.  This is necessary for the call to check_world_region.
   */
   world_region.longitude[2] = world_region.longitude[1];
   world_region.longitude[3] = world_region.longitude[1];
   world_region.longitude[1] = world_region.longitude[0];

   /*
   ** determine how regions are defined.
   */
   reg_def_by_world_pts = determine_region(world_region);
   reg_def_by_poly_pts = determine_region(polygon_region);

   /*
   ** world_in_region() handles actual calculation
   */
   return( world_in_region(world_region.latitude, world_region.longitude,
	  		   polygon_region.latitude, polygon_region.longitude,
			   polygon_region.projection, world_region.projection,
                           reg_def_by_world_pts, reg_def_by_poly_pts) );

}  /* WORLD_IN_POLYGON */

/******************************************************************************
*
*     ************************
*     *                      *
*     *  POLYGON_IN_POLYGON  *  method body 
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
     region2)

{  /* POLYGON_IN_POLYGON */

   int reg_def_by_r1_pts = 1;  /* default to inside */
   int reg_def_by_r2_pts = 1;  /* default to inside */

   /* 
   ** Verify that regions are of correct type 
   */
   if ( region1.geographic_type != POLYGON ||
        region2.geographic_type != POLYGON ) return E_BAD_REGION_TYPE;

   /*
   ** determine how the regions are defined.
   */
   reg_def_by_r1_pts = determine_region(region1);
   reg_def_by_r2_pts = determine_region(region2);

   /*
   ** region_in_region() handles actual calculation
   */
   return( region_in_region(region1.latitude, region1.longitude,
			    region2.latitude, region2.longitude,
			    region1.projection, region2.projection, 
                            reg_def_by_r1_pts, reg_def_by_r2_pts) );

}  /* POLYGON_IN_POLYGON */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *          ABSF             *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function determines the absolute value of a float. 
*
* Called by: check_pts_along_lat 
******************************************************************************/
float absf (float  x) 

{  /* ABSF */
 
  if ( x < 0.0 ) return ( -1.0 * x );

return x;
}

/******************************************************************************
*
*        *****************************
*        *                           *
*        *        ARCTANH            *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function calculates the hyperbolic arctangent of x. 
*
* Called by: mercator_transform
******************************************************************************/
float arctanh (float  x)                 /* x is an angle in radians */
{
  float a;

  if ( absf( x ) < 1.0 )
     {
     a = 0.5 * log((1.0 + absf(x)) / (1.0 - absf(x)));

     if (x < 0.0) return ( -1.0 * a );
     else return a;

     }

/* this should never be reached */    
return((float)E_ARCTANH);

}  /* end ARCTANH */

/****************************************************************************
*
*        *****************************
*        *                           *
*        *       CHECK_BOUNDS        *    method body
*        *                           *
*        *****************************
*
*
* Purpose: This function checks to see if the region defined by x1_pts and 
*          y1_pts overlaps the region defined by x2_pts and y2_pts.  The points 
*          are assumed in the same frame of reference.  Start1/stop1 and 
*          start2/stop2 are used as indexes into the corresponding arrays of 
*          pts, which allows for the use of complex regions.  Reg_def_by_pts 
*          refers to whether the region of interest, associated with x1_pts and
*          y1_pts, is defined as the region inside these points (1) or outside 
*          these points (0).
*
* Returns: one if regions overlap otherwise zero
* Called by: region_in_region and check_world_region
*
* Written by K. Stephens May 1993
****************************************************************************/
int check_bounds(start1, stop1, start2, stop2, x1_pts, y1_pts,
                 x2_pts, y2_pts, reg_def_by_r1_pts, reg_def_by_r2_pts,
                 r1_projection, r2_projection, check_line_seg_only)

int start1,stop1,start2,stop2;
float *x1_pts, *y1_pts, *x2_pts, *y2_pts;
int reg_def_by_r1_pts, reg_def_by_r2_pts; 
int r1_projection, r2_projection, check_line_seg_only;
{
 int i,j,at_vertex;
 int location = 0;
 float pt_x, pt_y;
 float p1[2],p2[2],w1[2],w2[2]; /* does the line between p1 and p2 intersect
                                ** the line between w1 and w2
                                */

 /*
 ** check intersections over all x2/y2 lines
 */
 for(i = start2; i <= stop2; i++) {

    w1[0] = x2_pts[i];
    w1[1] = y2_pts[i];

    if ( i == stop2 ) {
        w2[0] = x2_pts[start2];
        w2[1] = y2_pts[start2];
    }
    else {
        w2[0] = x2_pts[i+1];
        w2[1] = y2_pts[i+1];
    }

    /*
    ** check intersections over all x1/y1 lines
    */
    for(j = start1; j <= stop1; j++) {

        p1[0] = x1_pts[j];
        p1[1] = y1_pts[j];

        if ( j == stop1 ) {
            p2[0] = x1_pts[start1];
            p2[1] = y1_pts[start1];
        }
        else {
            p2[0] = x1_pts[j+1];
            p2[1] = y1_pts[j+1];
        }

        /*
        ** if there are any intersections, the regions overlap
        */
        if ( intersect(p1,p2,w1,w2,&at_vertex) ) return(1);

    }
 }

 /****************************************************************************
 ** At this point there were no intersections. However, there are three ways 
 ** that the region defined by x1_pts and y1_pts (R1) overlaps the region 
 ** defined by x2_pts and y2_pts (R2).
 **
 ** 1) If R2 is completely enclosed by R1, the only way that the regions CAN- 
 **    NOT overlap is if reg_def_by_r1_pts == 0 AND reg_def_by_r2_pts == 1.
 **    Otherwise the regions overlap.
 **
 ** 2) If R1 is completely enclosed by R2, the only way that the regions CAN- 
 **    NOT overlap is if reg_def_by_r1_pts == 1 AND reg_def_by_r2_pts == 0.
 **    Otherwise the regions overlap.
 **
 ** 3) If neither region is enclosed by the other region, the only way that
 **    the regions CANNOT overlap is if reg_def_by_r1_pts == 0 AND 
 **    reg_def_by_r2_pts == 0.  Otherwise the regions overlap.
 *****************************************************************************/

 /* 
 ** There is the posibility that we are checking individual line segments,
 ** and not entire regions.  If this is the case, the parameter: 
 ** check_line_seg_only will be set to one.  So let's check for it.
 **
 ** (This is the case for the call to check_bounds from check_world_region)
 */
 if ( check_line_seg_only ) return (0);

 /*
 ** First we check to see if R2 is completely enclosed by R1.  We only need
 ** to check if one of R2 points is inside R1 because we know there are no
 ** intersections between the two regions. 
 */
 pt_x = x2_pts[start2];
 pt_y = y2_pts[start2];

 pt_in_region(x1_pts,y1_pts,pt_x,pt_y,r1_projection,r2_projection,&location,1);

 if ( error_code != 0 ) return error_code;

 if ( location ) {  /* region2 is enclosed by region1 */

    if ( ( reg_def_by_r1_pts == 0 ) && ( reg_def_by_r2_pts == 1 ) ) return(0);
    else return(1);
 }

 /*
 ** Next we check to see if R1 is completely enclosed by R2.  We only need
 ** to check if one of R1 points is inside R2 because we know there are no
 ** intersections between the two regions. 
 */
 pt_x = x1_pts[start1];
 pt_y = y1_pts[start1];

 pt_in_region(x2_pts,y2_pts,pt_x,pt_y,r2_projection,r1_projection,&location,1);
 
 if ( error_code != 0 ) return error_code;

 if ( location ) {  /* region1 is enclosed by region2 */
 
    if ( ( reg_def_by_r1_pts == 1) && ( reg_def_by_r2_pts == 0 ) ) return(0);
    else return(1);
 }

 /*
 ** At this point, the only way that the regions CANNOT be overlapping is if
 ** both regions are defined inside (1) their respective points.
 */
 if ( reg_def_by_r1_pts && reg_def_by_r2_pts ) return(0);

return(1);

} /* END CHECK_BOUNDS */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *       CHECK_POINT         *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function determines if the point described by lat_pts[0] and
*          lon_pts[0] is in the region defined by region_x_pts and region_y_pts.
*          One is returned if the point is in the region, otherwise zero. 
*
* Called by: spatial_query_routines__point_in_polygon, world_in_region, and 
*            region_in_region 
******************************************************************************/
int check_point (float lat_pt,
                 float lon_pt,
   		 float region_x_pts[PT_MAX],
   		 float region_y_pts[PT_MAX],
   		 enum Map_Projections region_projection,
   		 enum Map_Projections point_projection,
                 int reg_def_by_pts)

{  /* CHECK_POINT */ 

  int   location = 0;
  int   i;
  float region_pts_x[PT_MAX];
  float region_pts_y[PT_MAX];
  float lat_pts[PT_MAX];
  float lon_pts[PT_MAX];
  float pt_x, pt_y;

  /*
  ** initialize lat/lon pts
  */
  lat_pts[0] = lat_pt;
  lon_pts[0] = lon_pt;

  lat_pts[1] = lat_pts[2] = lat_pts[3] = 0.0;
  lon_pts[1] = lon_pts[2] = lon_pts[3] = 0.0;

  /* 
  ** convert the lats and longs into x, y coordinates in the mapping 
  ** associated with the projection 
  */
  convert_pts(lat_pts, lon_pts, region_pts_x, region_pts_y, 
              point_projection, 1);
  
  if ( error_code != 0 ) return error_code;

  pt_x = region_pts_x[0];
  pt_y = region_pts_y[0];

  /*
  ** determine if the point is in the region defined by region_y_pts and
  ** region_x_pts
  */
  pt_in_region(region_x_pts, region_y_pts, pt_x, pt_y,region_projection, 
               point_projection, &location, reg_def_by_pts);
 
  if ( error_code != 0 ) return error_code;
    
  return ( location );

} /* CHECK_POINT */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK POLAR STEREO     *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function first determines if their_region crosses the equator
*          If so, it then determines if the line defined by x1,y1 and x2,y2
*          crosses the equator.  If this line crosses the equator, the 
*          x value is calculated and new x and y values are determined for
*          either x1,y1 or x2,y2 depending on which hemisphere we are in.
*
* Called by: check_poly_vs_poly
*
******************************************************************************/
int CheckPolarStereo(int their_projection,
                     float our_region_lat[PT_MAX],
                     float their_region_lat[PT_MAX],
                     float *x1, float *y1, 
                     float *x2, float *y2,
                     float slope, 
                     int index1, int index2) 

{ /* CHECK POLAR STEREO */

  int pt1_above = 0;
  int pt2_above = 0;
  char our_region_hem = ' ';
  int their_region_crosses_eq = 0;
  float their_max_lat, their_min_lat;
  float our_max_lat;
  float equator_lat = 0.0;
  float bogus_lon = 0.0;
  float new_x, new_y;

    /*
    ** Check to see if their_region crosses the equator
    */
    their_max_lat = Max(PT_MAX,their_region_lat);
    their_min_lat = Min(PT_MAX,their_region_lat);

    if ( (their_max_lat > 0.0) && (their_min_lat < 0.0) )
              their_region_crosses_eq = 1; 

    /*
    ** Determine which hemisphere the dataset is defined in.
    ** We assume that the dataset is correclty defined (i.e. a dataset 
    ** correctly defined in a polar stereographic projection cannot
    ** cross the equator)
    */ 
    our_max_lat = Max(PT_MAX,our_region_lat);
 
    if ( our_max_lat > 0.0 ) our_region_hem = 'N';
    else our_region_hem = 'S'; 

    /*
    ** If their_region crosses equator (and our projection is polar stereo)
    ** then we must make sure that we stop stepping once we get to the
    ** equator.  Also we must make sure we discard any part of their_region 
    ** that is not in the same hemisphere as our_region.
    */
    if ( their_region_crosses_eq ) {

          /* 
          ** Determine if pt1 (x1,y1) and pt2 (x2,y2) are above or below
          ** equator.  There are four possible scenarios: both above (or on),
          ** both below, pt1 above and pt2 below, pt1 below and pt2 above.
          */
          if ( their_region_lat[index1] >= 0.0 ) pt1_above = 1;
          if ( their_region_lat[index2] >= 0.0 ) pt2_above = 1;

          /*
          ** If pt1 and pt2 are not in the same hemisphere, calculate
          ** new_x and new_y value.  We know that we want y to be equal
          ** to the equator so we'll compute the value for y at lat = 0.0.
          ** (PROJECTION_NOTE:  This assumes that the transformation equations 
          ** for the projection that their_region is defined in compute x  
          ** with respect to longitude and y with respect to latitude
          ** independent of each other...(i.e. MERCATOR transformations) )
          */
          if ( pt1_above != pt2_above ) {
      
             convert_pts(&equator_lat, &bogus_lon, &new_x, &new_y, 
                         their_projection, 1);

             /* Calculate new_x */
             if ( slope == 0.0 ) new_x = *x1;
             else if ( new_y == *y2 ) new_x = *x2;
             else new_x = ( (new_y - *y2) / slope ) + *x2;
              
          }

          switch ( our_region_hem ) {

            /* our region defined in northern hemisphere */
            case 'N' : 
       
               /* both above */      
               if ( pt1_above && pt2_above ) return 1;

               /* both below */
               else if ( !pt1_above && !pt2_above ) return 0;

               /* pt1 above and pt2 below...set up new x2,y2 */
               else if ( pt1_above && !pt2_above ) {

                   *x2 = new_x; 
                   *y2 = new_y;
                   return 1;
               }

               /* pt1 below and pt2 above...set up new x1,y1 */
               else if ( !pt1_above && pt2_above ) {

                   *x1 = new_x;
                   *y1 = new_y;
                   return 1;
               }         
               break;

            /* our region defined in southern hemisphere */
            case 'S' : 

               /* both above equator */      
               if ( pt1_above && pt2_above ) return 0;

               /* both below equator */
               else if ( !pt1_above && !pt2_above ) return 1;

               /* pt1 above and pt2 below...set up new x1,y1 */
               else if ( pt1_above && !pt2_above ) {

                   *x1 = new_x;
                   *y1 = new_y;
                   return 1;
               }

               /* pt1 below and pt2 above...set up new x2,y2*/
               else if ( !pt1_above && pt2_above ) {

                   *x2 = new_x;
                   *y2 = new_y;
                   return 1;
               }        
               break;

            default  : return 1; break;

         }  /* end switch */

    } /* end if their_region_crosses_eq */

/* default to okay to process */
return 1;

} /* CHECK POLAR STEREO */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK_POLY_VS_POLY     *    method body 
*        *                           *
*        *****************************
*
* Purpose:  This function is used to determine if two polygons defined in
*           two different projections overlap.  Also, if either of the two
*           projections have special limitations, this procedure takes them
*           into account.  If the two polygons overlap one is returned, 
*           otherwise zero is returned.
*
* Called by: region_in_region 
******************************************************************************/
int check_poly_vs_poly(float our_region_lat[PT_MAX],
                       float our_region_lon[PT_MAX],
                       float their_region_lat[PT_MAX],
                       float their_region_lon[PT_MAX],
                       float our_region_x[PT_MAX], 
                       float our_region_y[PT_MAX], 
		       int our_projection,
                       float their_region_x[PT_MAX], 
                       float their_region_y[PT_MAX], 
                       int their_projection)

{ /* CHECK_POLY_VS_POLY */

  int i, k;
  int overlap = 0;
  int decrement_y = 0;
  int decrement_x = 0;
  int still_checking = 1;
  int okay_to_process = 1;
  float m = 0.0;  /* slope */
  float x, y, x1, y1, x2, y2;
  float step_x, step_y, new_y, new_x;

   /*
   ** Now, loop over all of their region's edges, checking them against
   ** our region.  
   */
   for (i = 0; i < PT_MAX; i++) {

       /*
       ** Initialize variables used everytime through for loop
       */ 
       decrement_x = decrement_y = 0;
       still_checking = 1;
       okay_to_process = 1;

       k = i + 1;
       if ( k == PT_MAX ) k = 0;

       /*
       ** We have to stick to the convention of stepping from the NW point to 
       ** the NE point (i=0,k=1); from the NE point to the SE point (i=1,k=2);
       ** from the SE point to the SW point (i=2,k=3); then from the SW point 
       ** back to the NW point (i=3,k=0).  This convention is necessary to 
       ** ensure that we will always step along the lines that correclty 
       ** define the region.
       */
       x1 = their_region_x[i];
       y1 = their_region_y[i];
       x2 = their_region_x[k];
       y2 = their_region_y[k];

       /*
       ** If y1 is larger than y2, we need to decrement y's value when
       ** stepping
       */ 
       if ( y1 > y2 ) decrement_y = 1;

       /*
       ** If x1 is larger than x2, we need to decrement x's value when
       ** stepping.
       */ 
       if ( x1 > x2 ) decrement_x = 1;

       /*
       ** Calculate step_x and step_y.
       */ 
       if ( y1 == y2 ) { /* Horizontal line */

             step_x = RESOLUTION; 
             step_y = 0.0; 

       }
       else if ( x1 == x2 ) { /* Vertical line */

             step_x = 0.0;
             step_y = RESOLUTION; 

       }
       else { /* Normal calculation */

             step_x = RESOLUTION;

             /* Calculate slope of line */
             m = ( y1 - y2 ) / ( x1 - x2);

             /* Calculate x value used to find step_y */
             if ( decrement_x ) 
                 new_x = x1 - step_x;
             else 
                 new_x = x1 + step_x;

             /* Calculate y value used to find step_y */
             new_y  = m * ( new_x - x2 ) + y2;

             if ( new_y > y1 ) step_y = new_y - y1;
             else step_y = y1 - new_y;

       }  /* end normal calculation */

       /*
       ** Since we are converting their region's x and y values into
       ** latitudes and longitudes (pt_in_region does this), we need
       ** to make sure that all latitudes and longitudes that we encounter
       ** are valid in our projection. (This is why this function has
       ** their_region lat/longs and our_region lat/longs as parameters)
       **
       ** Currently we are only checking for problems associated with
       ** the POLAR STEREOGRAPHIC projection.  (i.e. does their_region cross
       ** the equator)
       */
       if ( our_projection == POLAR_STEREOGRAPHIC ) { 

          okay_to_process = CheckPolarStereo(their_projection, our_region_lat,
                                             their_region_lat, &x1, &y1, &x2, 
                                             &y2, m, i, k); 

       }

       /*
       ** If the current line (x1,y1) to (x2,y2) is okay to process, go
       ** ahead.  
       */
       if ( okay_to_process ) {

          x = x1;
          y = y1;

          /*
          ** Step along the line segment from x1,y1 to x2,y1 checking each
          ** point to see if their region overlaps our region.
          */
          while ( still_checking ) {

              /*
              ** See if the current x,y point is in our region
              */
              pt_in_region(our_region_x, our_region_y, x, y, our_projection, 
                           their_projection, &overlap, 1);

              if ( error_code != 0 ) return error_code;
              if ( overlap != 0 ) return overlap;

              /* Calculate new x */
              if ( decrement_x ) x -= step_x;
              else x += step_x;

              /* Calculate new y */
              if ( decrement_y ) y -= step_y;
              else y += step_y;

              if ( step_x != 0.0 ) {
                 if ( absf( x - x2 ) <= RESOLUTION ) still_checking = 0;
              }
              else if ( step_y != 0.0 ) {
                 if ( absf( y - y2) <= RESOLUTION ) still_checking = 0;
              }
              else still_checking = 0;  /* step_x == step_y == 0.0 */

          }  /* end while loop */

       }  /* end okay_to_process */

   }  /* end for loop */

/* the regions didn't overlap */    
return 0;

}  /* END CHECK_POLY_VS_POLY */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK_PTS_ALONG_LAT    *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function is used to step along longitudinal lines to determine
*          if a point is in a world region (i.e. a region defined by maximum
*          and minimum latitudes and longitudes).
*
* Called by: check_world_region
******************************************************************************/
int check_pts_along_lat (float west_long,
   		         float east_long,   
                         float lat1,        /* current latitude line  */
   		         float our_region_x[PT_MAX],
   		         float our_region_y[PT_MAX],
   		         int our_projection,
   		         int their_projection,
   		         int reg_def_by_pts)

{ /* CHECK_PTS_ALONG_LAT */

  int i;
  int location;
  float dlta1 = 6378.145/90.0 * 0.25;
  float diff, step;
  int num_steps;
  float lat_pts[PT_MAX], lon_pts[PT_MAX];
  float x_pts[PT_MAX], y_pts[PT_MAX];

  /* 
  ** set up lat_pts 
  */
  for(i=1; i< PT_MAX; i++) lat_pts[i] = 0.0;
  lat_pts[0] = lat1;
 
  /* 
  ** set up lon_pts 
  */
  for(i=1; i< PT_MAX; i++) lon_pts[i] = 0.0;
  lon_pts[0] = west_long;

  /*
  ** If west_long is equal to east_long, then we are really dealing with
  ** one point on the line.  NOTE:  if east_long equals 0.0 and west_long
  ** equals 360.0 or west_long equals 0.0 and east_long equals 360.0 this
  ** is NOT the same as the two longitudes being equal to each other. (at
  ** least I assumed that the user wanted the two to be 360.0 apart, not
  ** equal to each other)  
  */
  if ( west_long == east_long )  {
  
      convert_pts(lat_pts, lon_pts, x_pts, y_pts, their_projection, 1); 

      /*
      ** Determine if the current point is in our_region or not
      */
      pt_in_region(our_region_x, our_region_y, x_pts[0], y_pts[0], 
                   our_projection, their_projection, &location, reg_def_by_pts);
 
      if ( error_code != 0 ) return error_code;
    
      return ( location );
  }

  /*
  ** Determine distance between west_long and east_long (from west to east)
  */
  if ( west_long > east_long ) 
          diff = 360.0 - west_long + east_long;
  else 
          diff = east_long - west_long;

  /*
  ** Make sure that diff makes sense. If it is supposed to be zero (i.e. the
  ** two longitudes are equal to each other) then we would have caught this
  ** earlier.  If diff is zero at this point, it is really 360.0. 
  */
  if ( diff == 0.0 ) diff = 360.0;

  /*
  ** Calculate step to use to step along the current latitude line from
  ** the west_long (min_lon) to the east_long (max_lon)
  */
  step = ( diff ) / (scale / dlta1);

  /*
  **  determine number of times to step
  */
  num_steps = (int)  diff / step;

  for (i = 0; i <= num_steps ; i++) {

      convert_pts(lat_pts, lon_pts, x_pts, y_pts, their_projection, 1); 

      /*
      ** Determine if the current point is in our_region or not
      */
      pt_in_region(our_region_x, our_region_y, x_pts[0], y_pts[0], 
                   our_projection,their_projection, &location, reg_def_by_pts);

      if ( error_code != 0 ) return error_code;
    
      if (location == 1) return 1;
      
      lon_pts[0] = lon_pts[0] + step;
      
      /*
      ** since we go from west to east, we might cross the prime meridian,
      ** so we need to handle that condition
      */ 
      if ( lon_pts[0] > 360.0 ) lon_pts[0] = lon_pts[0] - 360.0;
      
  }

  return location;

} /* END CHECK_PTS_ALONG_LAT */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK_WORLD_REGION     *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function is used to determine if a region defined by maximum
*          and minimum latitudes and longitudes overlaps a region defined by
*          x and y points.  If the two regions overlap one is returned,
*          otherwise zero is returned.
*
* Called by: world_in_region
******************************************************************************/
int check_world_region (float lat1[PT_MAX], 
   			float lon1[PT_MAX],
   			float our_region_x[PT_MAX],
   			float our_region_y[PT_MAX],
                        int our_projection,
                        int their_projection,
   			int reg_def_by_their_pts,
   			int reg_def_by_our_pts)

{ /* CHECK_WORLD_REGION */
  
  int i,k;
  int location;
  float their_region_x[PT_MAX];
  float their_region_y[PT_MAX];
  float start, stop;
  int check_line_seg_only = 1; /* we only want to check individual line 
                                  segments */

  /* 
  ** compute the x, y coordinates of their region in their projection 
  */
  convert_pts(lat1, lon1, their_region_x, their_region_y, 
              their_projection, PT_MAX);
  
  if ( error_code != 0 ) return error_code;
  
  /*
  ** The way we check the lat1/lon1 regions is by starting with the
  ** min_lat/min_lon pt and following the longitude line to the max_lat/min_lon
  ** pt, then following the latitude line to the max_lat/max_lon pt, then
  ** following the longitude line to the min_lat/max_lon pt, then following
  ** the latitude line back to the starting min_lat/min_lon pt.  Along the 
  ** way we check intersections for straight longitude lines, and individual
  ** points for curved latitude lines.
  */
  for (i = 0; i < PT_MAX; i++) {

    k = i + 1;
    if (i == PT_MAX - 1) k = 0;

    /* Check whether we are on a (straight) longitude line.
    **
    ** PROJECTION_NOTE: This is highly dependent on the projections used.  
    ** Since this code was developed using POLAR_STEREOGRAPHIC and MERCATOR 
    ** as the user and dataset projections; we used the fact that longitude 
    ** lines are straight in these projections.  However, if different 
    ** projections are used, this code will need to be modified if the new 
    ** projections don't have the property of straight longitudinal lines.
    */
    if ( lon1[i] == lon1[k] ) {

      location = check_bounds(i, k, 0, PT_MAX -1, their_region_x, 
                              their_region_y, our_region_x, our_region_y,
                              reg_def_by_their_pts, reg_def_by_our_pts, 
                              their_projection, our_projection,
                              check_line_seg_only);

    }
    else {   /* else we are on a curved (latitude line) */

      start = lon1[0];  /* min longitude (i.e. west) */
      stop  = lon1[2];  /* max longitude (i.e. east) */ 
      
      location = check_pts_along_lat(start,stop,lat1[i], our_region_x, 
                                     our_region_y, our_projection, 
                                     their_projection,reg_def_by_our_pts);
    }
    if (location != 0) return location;

  } /* end for loop */

  return location;

} /* CHECK_WORLD_REGION; */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CONVERT_CORNER_PTS     *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function is used to convert the corner points of a region 
*          defined by latitudes and longitudes into points defined in x and y
*
* Called by: world_in_region, check_point, check_world_region,
             determin_region, region_in_region, and
*            spatial_query_routines__point_in_polygon.
******************************************************************************/
int convert_pts (float *lat,
     	         float *lon,
     		 float *x,
     		 float *y,
     		 int mapping_type,
                 int num_pts)

{ /* CONVERT_CORNER_PTS */

  int i;
  float pt_x, pt_y;
  float max_lat = -INFINITY;
  float min_lat = INFINITY;

  if (mapping_type == POLAR_STEREOGRAPHIC) {
    for (i = 0; i < num_pts; i++) {

      polar_stereo_transform(lat[i], lon[i], &pt_x, &pt_y);
      x[i] = pt_x;
      y[i] = pt_y;

      /* keep track of max/min lat */
      if ( lat[i] > max_lat ) max_lat = lat[i];
      if ( lat[i] < min_lat ) min_lat = lat[i];
    }
    if ( max_lat > 0.0 && min_lat < 0.0 ) error_code = E_BAD_POLAR_STEREO_LAT;

  }
  else if (mapping_type == POLAR_ORTHOGRAPHIC)
    for (i = 0; i < num_pts; i++) {

      polar_orthographic_transform((double)lat[i], (double)lon[i],&pt_x,&pt_y);
      x[i] = pt_x;
      y[i] = pt_y;

    }
  else if (mapping_type == EQUATORIAL_ORTHOGRAPHIC)
    for (i = 0; i < num_pts; i++) {

      equatorial_ortho_transform(lat[i], lon[i], &pt_x, &pt_y);
      x[i] = pt_x;
      y[i] = pt_y;

    }
  else if (mapping_type == MERCATOR) 
    for (i = 0; i < num_pts; i++) {

      mercator_transform(lat[i], lon[i], &pt_x, &pt_y);
      x[i] = pt_x;
      y[i] = pt_y;

    }
  else error_code = E_BAD_PROJ_FOR_GIVEN_REG;

}  /* end CONVERT_CORNER_PTS */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *     DETERMINE_REGION      *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function returns a 1 if the area defined by 'region' is
*          inside the given region.  Else 0 is returned (area defined by
*	   region is outside the given region)
*
* Called by: spatial_query_routines__world_in_polygon and
*            spatial_query_routines__polygon_in_polygon
***************************************************************************/
int determine_region(REGION region)

{ /* DETERMINE_REGION */

  float region_x_pts[PT_MAX];   /* converted lat/lon to x/y x pts      */ 
  float region_y_pts[PT_MAX];   /* converted lat/lon to x/y y pts      */
  float poles_x_pts[PT_MAX];    /* north and south poles x pts         */
  float poles_y_pts[PT_MAX];    /* north and south poles y pts         */
  float poles_lat[PT_MAX];      /* north and south poles latitude pts  */
  float poles_lon[PT_MAX];      /* north and south poles longitude pts */
  float distance = 0.0;         /* distance between max/min longitudes */
  int location = 1;             /* inside/outside the region           */ 


  return 1; /* quick fix (easier than removing all calls to this function) */

#ifdef DETERMINE_REGION  /* THIS CODE IS NOT CURRENTLY BEING USED */
 
  /*
  ** If the region is a point, for the point in region algorithm to work 
  ** correctly the region defined by the point must be declared as inside
  */
  if( region.geographic_type == POINT ) return(1);

  /*
  ** If the region is global, the region defined is automatically inside 
  */
  if( region.geographic_type == GLOBAL ) return(1);

  /*
  ** If the region is of type WORLD, the region defined by the points is 
  ** automatically inside. 
  */
  if( region.geographic_type == WORLD ) return (1);

  /*
  ** If the region is of type POLYGON, determine if the region is inside or
  ** outside the given points by looking at the pole included.
  */
  if( region.geographic_type == POLYGON ) { 

        /*
        ** calculate the x/y corner pts of the region
        */
	convert_pts(region.latitude, region.longitude, region_x_pts, 
                    region_y_pts, region.projection, PT_MAX);
	
        /*
        ** calculate the x/y north and south pole pts 
        */
        poles_lat[0] = 90.0;  poles_lon[0] = 0.0;  /* north pole */
        poles_lat[1] = -90.0; poles_lon[1] = 0.0;  /* south pole */
        poles_lat[2] = poles_lat[3] = 0.0; /* not used */
        poles_lon[2] = poles_lon[3] = 0.0; /* not used */
 
	convert_pts(poles_lat, poles_lon, poles_x_pts, poles_y_pts, 
                    region.projection, 2); 

        /*
        ** If region.projection is equal to POLAR_STEREOGRAPHIC, then the
        ** call to convert_pts will set the error_code to E_BAD_POL_STEREO_LAT 
        ** because we are converting both the north and south pole at the same 
        ** time.  This error should be ignored.
        */
        if ( ( region.projection == POLAR_STEREOGRAPHIC ) && 
             ( error_code == E_BAD_POLAR_STEREO_LAT ) ) error_code = 0;
 
        /*
        ** Case of N, S, or ' '.  If N check if north pole (poles_x/y_pts[0])
        ** is in region.  If S check if south pole (poles_x/y_pts[1]) is in
        ** region.  
	*/
        switch( region.pole_included ) {

	   case 'N': case 'n':
                     if ( region.projection == MERCATOR ) return (0);

                     pt_in_region(region_x_pts, region_y_pts, poles_x_pts[0], 
                                  poles_y_pts[0], region.projection,
                                  region.projection, &location,1);

                     if ( error_code != 0 ) return error_code;

                     break;

           case 'S': case 's': 
                     if ( region.projection == MERCATOR ) return (0);

         	     pt_in_region(region_x_pts, region_y_pts, poles_x_pts[1], 
                                  poles_y_pts[1],  region.projection, 
                                  region.projection, &location,1);

                     if ( error_code != 0 ) return error_code;

                     break;

	   case ' ' : /* 
                      ** Since neither the north pole nor the south pole were 
                      ** included in the region, the region must be defined 
                      ** inside the points. 
                      */ 
                      location = 1; 
                      break;

           default  : location = 1;
		      break;       
         }

         /*
         ** 'location' contains the location of the point in the specified
         ** region.  If location is INSIDE (1), this means that the pole was
         ** inside the specified region, thus the region is defined inside
         ** the points.  If location is OUTSIDE (0) the region is defined 
         ** outside the given points.  So we can just return the value of
         ** location.
         */
         return(location);

  }

/* default to INSIDE */
return(1);

#endif  /* THIS CODE IS NOT CURRENTLY BEING USED */

} /* DETERMINE_REGION */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *            EQ             *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function is used to determine if two numbers are equal; since
*          rounding off errors occur in most floating point operations. If 
*          two numbers are within one thousandth of each other, they are 
*          considered equal.
*
* Called by: spatial_query_routines__point_in_point
******************************************************************************/
int eq (float x,  float y)
{
  float diff = x - y;

  if ( absf(diff) == 360.0 ) return 1;

#ifdef EQUAL_PERCISION

  if ( absf(diff) > PERCISION ) return 0;

#else

  if ( x != y ) return 0;

#endif

return 1;

} /* END EQ */

/******************************************************************************
*
*        *********************************
*        *                               *
*        *  EQUATORIAL_ORTHO_TO_LAT_LON  *    method body 
*        *                               *
*        *********************************
*
* Purpose: This function transforms x and y equatorial orthographic coordinates
*          into latitude and longitude coordinates (world coordinates). 
*
* Called by: pt_in_region
******************************************************************************/
int equatorial_ortho_to_lat_lon (float x,
   				 float y,
   				 float *lat,
   				 float *lon)

{ /* EQUATORIAL_ORTHO_TO_LAT_LON */

  if ( x == 0.0 ) return E_EQU_ORTH_TO_LAT_LON;

  *lat = asin(y / (2.0 * scale) ) / deg2rad;
  *lon = asin(x / 2.0 * scale * sqrt(1.0 - pow( (y/x), 2))) / deg2rad;

} /* END EQUATORIAL_ORTHO_TO_LAT_LON */

/******************************************************************************
*
*        ********************************
*        *                              *
*        *  EQUATORIAL_ORTHO_TRANSFORM  *    method body 
*        *                              *
*        ********************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in an equatorial orthographic
*          projection.
*
* Called by: convert_pts
******************************************************************************/
int equatorial_ortho_transform (float lat,
   				float lon,
   				float *x,
   				float *y)

{ /* EQUATORIAL_ORTHO_TRANSFORM */

  *x = 2.0 * scale * cos(lat*deg2rad) * sin(lon*deg2rad);
  *y = 2.0 * scale * sin(lat*deg2rad);

  /*
  ** (double)0 is an invalid float 
  */ 
  if ( *y == (double)0 ) *y = 0.0;
  if ( *x == (double)0 ) *x = 0.0;
 
} /* EQUATORIAL_ORTHO_TRANSFORM */

/*************************************************************************
*
*       ********************
*       *                  *
*       *  generate_error  *  function body
*       *                  *
*       ********************
*
*       Purpose:  This function is used to print errors to standard error
*                 or to status_log.
*
*Written by K. Stephens May 1993
**************************************************************************/
void spatial_query_routines__generate_error( int err_num )

{ /* GENERATE_ERROR */

  int error_number = 0;            /* this is the index into the error_array */
  char error_str[MAX_ERR_STR_LEN]; /* this is used with the status log */
  int i;

  /* clear out error_str */
  for ( i=0; i < MAX_ERR_STR_LEN; i++ )  error_str[i] = '\0';

  /* set up error_str */
  error_str[0] = 'S';
  error_str[1] = 'Q';
  error_str[2] = 'R';
  error_str[3] = ':';
  error_str[4] = ' ';
  
  /* clear error_code (it's static) */
  error_code = 0;
 
  /*
  ** Determine the index into the error_array.  We multiply the input
  ** error number by SIGN incase negative error numbers are being used.
  ** SIGN must be defined. 
  */
  error_number = SIGN 1 * err_num;

  /*
  ** Make sure that the error number is valid
  */
  if ( error_number > MAX_ERROR || error_number < 1 ) {

#ifdef STATUS_LOG 
       status_log__log_message
            ("Invalid err_num received in SQR__generate_error.",error_number);

#else /* print error to stderr */

      fprintf(stderr,"Invalid err_num received in SQR__generate_error.\n");

#endif
  }
  else { 

#ifdef STATUS_LOG 

       strcat(error_str, error_array[error_number]);
       status_log__log_message(error_str,0);

#else /* print error to stderr */

       fprintf(stderr,"Error SQR: %s\n",error_array[error_number]);

#endif
  } 

} /* END GENERATE_ERROR */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *        INTERSECT          *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function calculates the intersection of the lines
*          defined by pt and max_xy, and xy1 and xy2.
*
* Returns: one if intersection is real otherwise zero 
* Called by: check_bounds, and pt_in_poly
*
* Written by K. Stephens May 1993
*******************************************************************************/
int intersect(pt,max_xy,xy1,xy2,at_vertex)
float pt[2],max_xy[2],xy1[2],xy2[2];
int *at_vertex;
{
 float ly1,ly2,lx1,lx2,my1,my2,mx1,mx2,s,t;
 float tnum, tden;

  lx1 = pt[0];
  ly1 = pt[1];
  lx2 = max_xy[0];
  ly2 = max_xy[1];

  mx1 = xy1[0];
  my1 = xy1[1];
  mx2 = xy2[0];
  my2 = xy2[1];
 
  *at_vertex = 0;

  /* 
  ** check that the vertices don't intersect 
  */
  if( (lx1 == mx1 && ly1 == my1) || (lx2 == mx1 && ly2 == my1) ||
      (lx1 == mx2 && ly1 == my2) || (lx2 == mx2 && ly2 == my2) ) {
                *at_vertex = 1;
                 return(1);
  }

  /*
  ** If the first line's two vertices are equal or the second lines two
  ** vertices are equal and the last test didn't fail, there is no way the
  ** lines can intersect.
  */
  if( (lx2 == lx1 && ly2 == ly1) || (mx2 == mx1 && my2 == my1) )return(0);

  /* 
  ** Make sure we will don't divide by zero 
  */
  if( lx2 - lx1 == 0.0 ) {
     if( mx2 - mx1 == 0.0) return(0);
     if( ly2 - ly1 == 0.0) return(0);
     t = (lx1 - mx1)/(mx2 - mx1);
     s = ((my1 - ly1) + t*(my2 - my1)) / (ly2 - ly1);
  }
  else { /* normal calculation */
     tnum = (ly1-my1) + (mx1*(ly2-ly1)/(lx2-lx1)) - (lx1*(ly2-ly1)/(lx2-lx1));
     tden = (my2-my1) - ((mx2-mx1)*(ly2-ly1))/(lx2-lx1);

     if( tden == 0.0 ) return(0); /* lines never intersect */
     t = tnum / tden;
     s = ((mx1 - lx1) + t*(mx2 - mx1)) / (lx2 - lx1);
  }

  /* 
  ** Check s and t values to see if the lines intersect 
  */
  if ( (s >= 0.0 && s <= 1.0) && (t >= 0.0 && t <= 1.0) )  return(1);

return(0);

} /* END INTERSECT */

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
* Called by: spatial_query_routines__do_regions_overlap 
******************************************************************************/
int spatial_query_routines__is_region_null (REGION region)

{  /* IS_REGION_NULL */

 REGION NULL_REG;

 /* setup NULL_REG */
 DEFAULT_REGION(NULL_REG);

 if( ( region.geographic_type == NULL_REG.geographic_type ) &&
     ( region.projection == NULL_REG.projection ) &&
     ( region.latitude[0] == NULL_REG.latitude[0] ) &&
     ( region.latitude[1] == NULL_REG.latitude[1] ) &&
     ( region.latitude[2] == NULL_REG.latitude[2] ) &&
     ( region.latitude[3] == NULL_REG.latitude[3] ) &&
     ( region.longitude[0] == NULL_REG.longitude[0] ) &&
     ( region.longitude[1] == NULL_REG.longitude[1] ) &&
     ( region.longitude[2] == NULL_REG.longitude[2] ) &&
     ( region.longitude[3] == NULL_REG.longitude[3] ) &&
     ( region.tangent_latitude == NULL_REG.tangent_latitude ) &&
     ( region.tangent_longitude == NULL_REG.tangent_longitude ) &&
     ( region.pole_included == NULL_REG.pole_included ) ) return 1;

  else return 0;

}  /* END IS_REGION_NULL */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *      LONG_DISTANCE        *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function returns the distance between two longitudes.  
*
* Called by: determine_region 
******************************************************************************/
float long_distance  (float west, float east)  /* these are longitudes */
{

  if (east < west) return (east + 360.0) - west;
  else return east - west;

} /* END LONG_DISTANCE */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *           MAX             *    method body 
*        *                           *
*        *****************************
*
*
* Purpose: This function calculates the maximum (positive) value in an array
*          of floats and returns that value.
*
* called by: world_in_region, check_bounds, check_point, determine_region, 
*            pt_in_poly and region_in_region.
*******************************************************************************/
float Max(nverts,verts)
int nverts;
float *verts;
{
 int i;
 float max_value = -INFINITY;

 for(i=0; i < nverts; i++ )
    if ( max_value < verts[i] ) max_value = verts[i];

 if ( max_value == -INFINITY ) return (E_MAX);

return(max_value);

} /* END MAX */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *  MERCATOR_TO_LAT_LON      *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function transforms x and y mercator coordinates into latitude
*          and longitude coordinates (world coordinates). 
*
* Called by: pt_in_region
******************************************************************************/
int mercator_to_lat_lon (float x, 
                         float y, 
			 float *lat, 
			 float *lon)

{ /* MERCATOR_TO_LAT_LON */

    if (y > not_visible)
      *lat = ( atan(sinh( y/scale )) / deg2rad );
    else {
      *lat = 85.0;
      error_code = E_BAD_MERC_LAT;
    }

    *lon = x / scale / deg2rad + MERC_MIN_LON;

} /* end MERCATOR_TO_LAT_LON */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    MERCATOR_TRANSFORM     *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in a mercator projection.
*           
* Called by: convert_pts
******************************************************************************/
float mercator_transform (float lat,
                          float lon,
   			  float *x,
   		          float *y)

{ /* MERCATOR_TRANSFORM */

  float max_latitude = 1.484; /* approx. 85 degrees */
  float lat_rad, lon_rad;
  float new_lon = lon - MERC_MIN_LON;

    if ( new_lon < 0.0 ) new_lon += 360.0;

    lat_rad = lat * deg2rad;
    lon_rad = new_lon * deg2rad;

    *x = scale * lon_rad;
    if ( absf( lat_rad ) < max_latitude )
      *y = scale * arctanh((float)sin(lat_rad));
    else {
      *y = (float) not_visible;
      error_code = E_BAD_MERC_LAT;
    }
  
  /*
  ** (double)0 is an invalid float 
  */ 
  if ( *y == (double)0 ) *y = 0.0;
  if ( *x == (double)0 ) *x = 0.0;

} /* end MERCATOR TRANSFORM */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *           MIN             *    method body 
*        *                           *
*        *****************************
*
*
* Purpose: This function calculates the minimum value in an array of floats 
*          and returns that value.
*
* called by: spatial_query_routines__nsidc_initial_check 
*******************************************************************************/
float Min(nverts,verts)
int nverts;
float *verts;
{
 int i;
 float min_value = INFINITY;

 for(i=0; i < nverts; i++ )
    if ( min_value > verts[i] ) min_value = verts[i];

 if ( min_value == INFINITY ) return (E_MIN);

return(min_value);

} /* END MIN */

/******************************************************************************
*
*        *************************************
*        *                                   *
*        *  OBLIQUE_ORTHOGRAPHIC_TRANSFORM   *    method body 
*        *                                   *
*        *************************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in a oblique orthographic
*          projection.
*
* Called by: not currently being used.
*
* NOTE: THIS PROCEDURE HAS NOT BEEN TESTED
******************************************************************************/
int oblique_orthographic_transform(float tangent_lat, /*latitude of tangency*/
   				   float lat,
   				   float lon,
   				   float *x,
   				   float *y)

{ /* OBLIQUE_ORTHOGRAPHIC_TRANSFORM */
		
  lat = lat * deg2rad;
  lon = lon * deg2rad;

  *x = 2.0 * scale * cos(lat) * sin(lon);
  *y = 2.0 * scale * (cos(tangent_lat) * sin(lat)
                    - sin(tangent_lat) * cos(lat) * cos(lon));
  
  /*
  ** (double)0 is an invalid float 
  */ 
  if ( *y == (double)0 ) *y = 0.0;
  if ( *x == (double)0 ) *x = 0.0;

} /* END OBLIQUE_ORTHOGRAPHIC_TRANSFORM */

/******************************************************************************
*
*        *********************************
*        *                               *
*        * POLAR_ORTHOGRAPHIC_TO_LAT_LON *    method body 
*        *                               *
*        *********************************
*
* Purpose: This function transforms x and y polar orthographic coordinates
*          into latitude and longitude coordinates (world coordinates). 
*
* Called by:  pt_in_region
******************************************************************************/
int polar_orthographic_to_lat_lon (double x,
   				   double y,
   				   float *lat,
   				   float *lon)

{ /* POLAR_ORTHOGRAPHIC_TO_LAT_LON */

   double scale1 = (double)scale;

  if ( x == 0.0 && y == 0.0 ) return E_POLAR_ORTH_TO_LAT_LON;

  *lat = (float)(acos (sqrt (pow(x,2) + pow(y,2))/scale1 ) /deg2rad);
  *lon = (float)(asin (x / sqrt(pow(x,2) + pow(y,2))) /deg2rad);

  if ((x > -0.0001) && (y < 0.0001))
    *lon = (float)(180.0 - *lon);

  else if ((x < 0.0001) && (y < 0.0001))
    *lon = (float)(180.0 + (-1.0) * *lon);

  else if ((x < 0.0001) && (y > -0.0001))
    *lon = (float)(360.0 +  *lon);

  else
    *lon = (float) *lon;

}  /* END POLAR_ORTHOGRAPHIC_TO_LAT_LON */

/******************************************************************************
*
*        *********************************
*        *                               *
*        *  POLAR_ORTHOGRAPHIC_TRANSFORM *    method body 
*        *                               *
*        *********************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in a polar orthographic 
*          projection.
*
* Called by: convert_pts
******************************************************************************/
int polar_orthographic_transform (double lat,
   				  double lon,
   				  float *x,
   				  float *y)

{ /* POLAR_ORTHOGRAPHIC_TRANSFORM */
	
  double scale1  = (double)scale;

  *x = (float) (scale1 * cos(lat*deg2rad)* sin(lon*deg2rad));
  *y = (float) (scale1 * cos(lat*deg2rad)* cos(lon*deg2rad));

  /*
  ** (double)0 is an invalid float 
  */ 
  if ( *y == (double)0 ) *y = 0.0;
  if ( *x == (double)0 ) *x = 0.0;

}  /* END POLAR_ORTHOGRAPHIC_TRANSFORM */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *  POLAR_STEREO_TO_LAT_LON  *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function transforms x and y polar stereographic coordinates
*          into latitude and longitude coordinates (world coordinates). 
*
* Called by: not currently being used. 
******************************************************************************/
int polar_stereo_to_lat_lon (float x,
   			     float y,    
   			     float *lat,
   			     float *lon)        

{ /* POLAR_STEREO_TO_LAT_LON */

   if ((y < 0.0001) && (y != 0.0))
    {
     if (x > -0.0001)
       *lon = (float)atan(x/(-1.0 * y)) / deg2rad;
     else
       *lon = (float)atan(x/(-1.0 * y)) / deg2rad + 360.0;
    }
   else if ((y > -0.0001) && (y != 0.0))
     *lon = (float)atan(x/(-1.0 * y)) / deg2rad + 180.0;
   else
    {
     if (x >= 0.0)
       *lon = 90.0;
     else
       *lon = 270.0;
    }

  if ( *lon < 0.0 ) *lon = absf(*lon);

  /*  Old derivation 
  *lat=90.0-(2.0* (float)atan (sqrt( pow(x,2) + pow(y,2)) / scale) / deg2rad);
  */

  /* Added 6-28-93 (k.s.) */
  if ( *lon != 0.0 ) {
   *lat = 90.0 - (2.0 * (atan( x /( sin(*lon * deg2rad) * scale)) / deg2rad));
  }
  else {
   *lat = 90.0 + (2.0 * (atan( y /( cos(*lon * deg2rad) * scale)) / deg2rad));
  }

} /* END POLAR_STEREO_TO_LAT_LON */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *  POLAR_STEREO_TRANSFORM   *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in a polar stereographic 
*          projection.
*
* Called by: check_pts_along_lat, convert_pts and pt_in_region
******************************************************************************/
int polar_stereo_transform (float lat,
   			    float lon,
   			    float *x,
   			    float *y)

{ /* POLAR_STEREO_TRANSFORM */

  float a,b;

  /*
  ** make sure that longitudes of 0.0 and 360.0 are treated the same 
  */
  if ( lon == 0.0 ) lon = 360.0;

  /*
  ** Since the calculation of x depends on the Tangent function, -90.0 will
  ** produce: tan(90.0) which is undefined (infinity).  So, if -90.0 is 
  ** encountered we'll use -89.999 instead.
  if ( lat == -90.0 ) lat = -89.999;
  */

  /*
  ** A bug seems to be associated with negative latitudes, so since we can
  ** only use polar stereo in one hemisphere anyway, we'll convert all negative
  ** latitudes into positive latitudes (determine region needs to be modified
  ** if used)
  */
  if ( lat < 0.0 ) lat = -1.0 * lat;

  a = scale * (float)sin(lon * deg2rad);
  b = (float)tan((45.0 - lat/2.0) * deg2rad);
 
  *x = b * a;

  *y = -1.0 * scale * b * (float)cos(lon * deg2rad) ;

  /*
  ** (double)0 is an invalid float 
  */ 
  if ( *y == (double)0 ) *y = 0.0;
  if ( *x == (double)0 ) *x = 0.0;

} /* END POLAR_STEREO_TRANSFORM */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *        PT_IN_POLY         *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function is used to determine if the point defined by
*          the array pt is in the region defined by the arrays: verts1
*          and verts2.
*
* Returns: one if pt is in region otherwise zero 
* Called by: pt_in_region
*
* Written by K. Stephens May 1993
*******************************************************************************/
int pt_in_poly(pt,nverts,verts1,verts2)
float pt[2];
int nverts;
float *verts1, *verts2;
{

 /* 
 ** NOTE: treat verts1 like x and verts2 like y, it really doesn't matter
 ** so to keep things simple this is the convention I'll use
 */

 float max_xy[2];
 float xy1[2];
 float xy2[2];
 int i;
 int at_vertex = 0;
 int num_inter = 0;

 max_xy[0] = Max(nverts,verts1) + INFINITY;
 max_xy[1] = Max(nverts,verts2) + INFINITY;

 for( i = 0; i < nverts ; i++ ) {

    xy1[0] = verts1[i];
    xy1[1] = verts2[i];

    if ( i != nverts -1 ) {
       xy2[0] = verts1[i + 1];
       xy2[1] = verts2[i + 1];
    }
    else {
       xy2[0] = verts1[0];
       xy2[1] = verts2[0];
    }

    num_inter = num_inter + intersect(pt,max_xy,xy1,xy2,&at_vertex);

    /* 
    ** If the point is at the vertex, the point is in the polygon 
    */
    if ( at_vertex ) return 1;

    if ( num_inter == 2 ) num_inter = 0;

 }

 if ( num_inter == 1 ) return(1);

return(0);

} /* END PT_IN_POLY */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *       PT_IN_REGION        *    method body
*        *                           *
*        *****************************
*
* Purpose: This function determines if the point described by pt_x and pt_y
*          is in the region defined by region_x_pts and region_y_pts.  If this
*          point is in the region one is returned otherwise zero is returned.
*
* Called by: check_bounds, check_pts_along_lat, check_point and 
*            determine_region
******************************************************************************/
int pt_in_region (float region_x_pts[PT_MAX],  
   		  float region_y_pts[PT_MAX], 
   		  float pt_x,
                  float pt_y,
                  int region_projection,
                  int point_projection,
                  int *location,
                  int region_defined_by_pts)

{ /* PT_IN_REGION */	

  int   i, inside;
  float lat = 0.0;
  float lon = 0.0;
  float lat_pts[PT_MAX],lon_pts[PT_MAX];
  float x_pts[PT_MAX], y_pts[PT_MAX];
  float pt[2];

  /*
  ** If projections are different, convert point's projection to 
  ** region's projection
  */
  if ( region_projection != point_projection ) {

    if (point_projection == POLAR_ORTHOGRAPHIC)
       polar_orthographic_to_lat_lon((double)pt_x, (double)pt_y, &lat, &lon);

    else if (point_projection == POLAR_STEREOGRAPHIC) 
       polar_stereo_to_lat_lon(pt_x, pt_y, &lat, &lon);

    else if (point_projection == EQUATORIAL_ORTHOGRAPHIC)
       equatorial_ortho_to_lat_lon(pt_x, pt_y, &lat, &lon);
  
    else if (point_projection == MERCATOR) 
       mercator_to_lat_lon(pt_x, pt_y, &lat, &lon);

    else error_code = E_PT_IN_REG;
   
    /*
    ** Make sure that we keep longitudes between 0.0 and 360.0 and
    ** latitudes between -90.0 and 90.0. (This code might not be
    ** necessary, but should remain until transformation routines
    ** are thouroughly tested)
    */  
    if ( lon > 360.0 ) lon -= 360.0;
    if ( lat > 90.0 ) lat = 180.0 - lat;
    if ( lat < -90.0 ) lat = -1.0 * (180.0 + lat);

    lat_pts[0] = lat;
    lon_pts[0] = lon;

    lat_pts[1] = lat_pts[2] = lat_pts[3] = 0.0;
    lon_pts[1] = lon_pts[2] = lon_pts[3] = 0.0;

    convert_pts(lat_pts,lon_pts,x_pts,y_pts,region_projection, 1);
   
    pt[0] = x_pts[0];
    pt[1] = y_pts[0];
 
  }  /* if region_proj != point_proj */ 
  else {

    pt[0] = pt_x;
    pt[1] = pt_y;

  }

  inside = pt_in_poly(pt,PT_MAX,region_x_pts,region_y_pts); 
  
  if ( inside ) {
     if ( region_defined_by_pts == 1) *location = 1;
     else *location = 0; 
  }
  else { /* outside */
     if ( region_defined_by_pts == 1) *location = 0;
     else *location = 1; 
  } 

} /* PT_IN_REGION */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *     REGION_IN_REGION      *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function is used to determine if two regions, both defined
*          by latitude and longitude points, overlap.  If the regions overlap
*          one is returned, otherwise zero is returned
*
* Called by: spatial_query_routines__polygon_in_polygon
******************************************************************************/
int region_in_region (float their_region_lat[PT_MAX],
                      float their_region_lon[PT_MAX],
   		      float our_region_lat[PT_MAX],
   		      float our_region_lon[PT_MAX],
   		      enum Map_Projections their_projection,
                      enum Map_Projections our_projection,
   		      int reg_def_by_their_pts,
   		      int reg_def_by_our_pts)

{ /* REGION_IN_REGION */

  float our_region_x[PT_MAX]; 
  float our_region_y[PT_MAX];
  float their_region_x[PT_MAX]; 
  float their_region_y[PT_MAX];
  float lat_pt, lon_pt;
  int check_line_seg_only = 0;  /* we want to check the entire region */
  int i, k;
  int overlap = 0;

  /*
  ** Conver latitude/longitude points to x/y via projection transformations.
  */
  convert_pts(our_region_lat, our_region_lon, our_region_x, 
              our_region_y, our_projection, PT_MAX);

  if ( error_code != 0 ) return error_code;

  convert_pts(their_region_lat, their_region_lon, their_region_x, 
              their_region_y, their_projection, PT_MAX);

  if ( error_code != 0 ) return error_code;
 
  /*
  ** If the two regions are defined in the same projection, then we can
  ** compute intersections directly.
  */
  if ( their_projection == our_projection ) {

    return check_bounds(0,PT_MAX-1,0,PT_MAX-1, their_region_x, their_region_y,
                         our_region_x, our_region_y, reg_def_by_their_pts,
                         reg_def_by_our_pts, their_projection,
                         our_projection, check_line_seg_only );
  }

  /* 
  **  Now we check to see whether any of their corner points are in our 
  **  region.  PROJECTION_NOTE: we can do this only because we assume that the 
  **  edges of our region are straight lines in our reference projection. 
  */
  
  /************************************************************************ 
   * 
   *         ******************************
   *         *           ours             *                Since their region 
   *         *                            *  ^ ^ ^ ^       is defined in a 
   *         *                           ^ ^         ^ ^   different projection
   *         *                         ^  *              ^   it might appear
   *         *                       ^    *  theirs        ^ to have curved 
   *         *                         ^  *              ^   edges in our 
   *         *                           ^ ^         ^ ^     projection.
   *         *                            *  ^ ^ ^ ^ 
   *         *                            *
   *         ******************************
   * 
   ************************************************************************/

    /* 
    ** check each of their corner points in our region. 
    */
    for (k = 0; k < PT_MAX; k++) {

      pt_in_region(our_region_x, our_region_y, their_region_x[k], 
                   their_region_y[k], our_projection, their_projection, 
                   &overlap, 1);
 
      if ( error_code != 0 ) return error_code;
      if ( overlap != 0 ) return overlap;

     }

  /*
  **  We then check to see whether any of our corner points are in their 
  **  region.  PROJECTION_NOTE: we can do this only because we assume that the 
  **  edges of their region are straight lines in their reference projection. 
  */

   /************************************************************************
   *
   *               ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ 
   *                 ^                             ^ 
   *                   ^                             ^ 
   *          ***********^******************           ^ 
   *           *           ^              *              ^ 
   *            *            ^           *                 ^ 
   *             *             ^        *                    ^ 
   *             *               ^      *  theirs           ^ 
   *             *      ours       ^    *                  ^ 
   *             *                   ^  *                 ^ 
   *            *                      ^ *               ^ 
   *           *                          *             ^ 
   *          *                            ^           ^ 
   *          ****************************** ^        ^ 
   *                                           ^     ^ 
   *  Since our region is defined in a           ^  ^ 
   *  different projection, the edges might        ^ 
   *  appear curved in their projection.
   *************************************************************************/

    /* 
    ** check each of our corner points in their region.
    */
    for (k = 0; k < PT_MAX; k++) {

      pt_in_region(their_region_x, their_region_y, our_region_x[k], 
                   our_region_y[k], their_projection, our_projection, 
                   &overlap, 1);
 
      if ( error_code != 0 ) return error_code;
      if ( overlap != 0 ) return overlap;

     }

  /*************************************************************************
   * 
   *  Messy case, none of their corner points are in our region and none
   *  of our corner points are in their region.
   *
   * 
   *         ******************************
   *         *           ours             *
   *         *                            *
   *         *                            *
   *    ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ 
   *    ^    *                            *    theirs     ^ 
   *    ^    *                            *               ^ 
   *    ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ 
   *         *                            *
   *         *                            *
   *         ******************************
   * 
   ************************************************************************/

   /*
   ** At this point we have no choice but to check each of the line segments
   ** making up their region against our_region.
   */
   return check_poly_vs_poly(our_region_lat, our_region_lon, their_region_lat, 
                             their_region_lon, our_region_x, our_region_y, 
                             our_projection, their_region_x, their_region_y, 
                             their_projection);

}  /* REGION_IN_REGION */

/******************************************************************************
*
*        *****************************
*        *                           *
*        *     WORLD_IN_REGION       *    method body 
*        *                           *
*        *****************************
*
* Purpose: This function is used to determine if a region defined by maximum
*          and minimum latitude and longitude points overlaps a region defined 
*          by 4 latitude/longitude pairs.  If the two regions overlap one is 
*          returned, otherwise zero is returned.
*
* Called by: spatial_query_routines__world_in_polygon
******************************************************************************/
int world_in_region (float lat1[PT_MAX],
                     float lon1[PT_MAX],
   		     float our_region_lat[PT_MAX],
   		     float our_region_lon[PT_MAX],
   		     int our_projection,
   		     int their_projection,
                     int reg_def_by_their_pts,
                     int reg_def_by_our_pts)

{ /* WORLD_IN_REGION */
  
  float our_region_x[PT_MAX];
  float our_region_y[PT_MAX];
  float lat_pt, lon_pt;
  int k;

  convert_pts(our_region_lat, our_region_lon, our_region_x, 
              our_region_y, our_projection, PT_MAX);
  
  if ( error_code != 0 ) return error_code;
 
  /************************************************************************ 
   *
   *  We first check to see whether any of their corner points are in our 
   *  region.  PROJECTION_NOTE: we can do this only because we assume that the 
   *  edges of our region are straight lines in our reference projection 
   *  (polar stereo).  Since we can't make that assumption for their region 
   *  we don't check whether our corner points are in their region in this 
   *  manner.
   * 
   *         ******************************
   *         *           ours             *
   *         *                            *
   *         *                            *
   *         *                       ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ 
   *         *                       ^    *  theirs        ^ 
   *         *                       ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ 
   *         *                            *
   *         *                            *
   *         *                            *
   *         ******************************
   * 
   ************************************************************************/

    /* 
    ** check each of their corner points 
    */
    for (k = 0; k < PT_MAX; k++) {

         lat_pt = lat1[k];
         lon_pt = lon1[k];

         if (check_point(lat_pt, lon_pt, our_region_x, our_region_y, 
                         our_projection, their_projection, 
                         reg_def_by_our_pts) == 1 ) return 1;
    }

  /*************************************************************************
   * 
   *  Messy case, none of their corner points are in our region
   *
   *               ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ 
   *                 ^                             ^ 
   *                   ^                             ^ 
   *          ***********^******************           ^ 
   *          *            ^               *             ^ 
   *          *              ^             *               ^ 
   *          *                ^           *                 ^ 
   *          *                  ^         *  theirs        ^ 
   *          *         ours       ^       *               ^ 
   *          *                      ^     *              ^ 
   *          *                        ^   *             ^ 
   *          *                          ^ *            ^ 
   *          *                            ^           ^ 
   *          ****************************** ^        ^ 
   *                                           ^     ^ 
   *                                             ^  ^ 
   *                                               ^ 
   *
   *************************************************************************/

   return ( check_world_region(lat1, lon1, our_region_x, our_region_y, 
                               our_projection, their_projection,  
                               reg_def_by_their_pts, reg_def_by_our_pts) );

} /* END WORLD_IN_REGION */

