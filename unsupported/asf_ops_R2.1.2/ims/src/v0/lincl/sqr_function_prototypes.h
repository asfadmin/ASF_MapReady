/**************************************************************************
*
*	*************************************
*	*	  	      	            *	
*	*      SQR_FUNCTION_PROTOTYPES      *
*	*		      	            *
*  	*************************************	
*
* Purpose: This file contains the prototypes for all of the routines used in 
*          spatial_query_routines.c EXCEPT for the routines that are declared 
*          in spatial_query_routines.h  (those routines conform to the naming 
*          convention: spatial_query_routines__function_name )
*
* Modifications:
* K. Stephens April 1993 (initial version)
*  3/23/94 njc - Removed directory path from includes.
**************************************************************************/
#include "spatial_query_routines.h"


/******************************************************************************
*
*        *****************************
*        *                           *
*        *          ABSF             *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function determines the absolute value of a float.
*
* Called by: check_pts_along_lon
******************************************************************************/
float absf (float x);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *        ARCTANH            *    method spec  
*        *                           *
*        *****************************
*
* Purpose: This function calculates the hyperbolic arctangent of x.
*
* Called by: mercator_transform
******************************************************************************/
float arctanh (float x); 

/****************************************************************************
*
*        *****************************
*        *                           *
*        *       CHECK_BOUNDS        *    method spec 
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
int check_bounds (int start1,
                  int stop1,
                  int start2,
                  int stop2,
                  float *x1_pts,
                  float *y1_pts,
                  float *x2_pts,
                  float *y2_pts,
                  int reg_def_by_r1_pts,
                  int reg_def_by_r2_pts,
                  int r1_projection,
                  int r2_projection,
                  int check_line_seg_only);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *       CHECK_POINT         *    method spec 
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
                 int reg_def_by_pts);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK POLAR STEREO     *    method spec 
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
                     int index1, int index2);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK_POLY_VS_POLY     *    method body
*        *                           *
*        *****************************
*
* Purpose:  This function is used to determine if two polygons defined in
*           two different projections overlap.  This function sets up the
*           global CMXY variables and then calls check_mdpt_of_xy on each
*           line segement making up the region.  Also, if either of two
*           projections have special limitations, this procedure takes them
*           into account when setting up CMXY.  If the two polygons overlap
*           one is returned, otherwise zero is returned.
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
                       int their_projection);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK_PTS_ALONG_LAT    *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function is used to step along latitudinal lines to determine
*          if a point is in a world region (i.e. a region defined by maximum
*          and minimum latitudes and longitudes).
*
* Called by: check_world_region
******************************************************************************/
int check_pts_along_lat (float west_long, 
                         float east_long,
   		         float lat1,  
                         float our_region_x[PT_MAX],
   		         float our_region_y[PT_MAX], 
                         int   our_projection,
                         int   their_projection,
   	                 int   reg_def_by_pts);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    CHECK_WORLD_REGION     *    method spec 
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
   			int reg_def_by_our_pts);
  
/******************************************************************************
*
*        *****************************
*        *                           *
*        *       CONVERT_PTS         *    method spec 
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
                 int num_pts);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *     DETERMINE_REGION      *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function returns a 1 if the area defined by 'region' is
*          inside the given region.  Else 0 is returned (area defined by
*          region is outside the given region)
*
* Called by: spatial_query_routines__world_in_polygon and
*            spatial_query_routines__polygon_in_polygon
***************************************************************************/
int determine_region (REGION region);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *            EQ             *    method spec 
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
int eq (float x, float y);

/******************************************************************************
*
*        *********************************
*        *                               *
*        *  EQUATORIAL_ORTHO_TO_LAT_LON  *    method spec 
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
   				 float *lon);

/******************************************************************************
*
*        ********************************
*        *                              *
*        *  EQUATORIAL_ORTHO_TRANSFORM  *    method spec 
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
   				float *y);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *        INTERSECT          *    method spec 
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
int intersect (float pt[2],
               float max_xy[2],
               float xy1[2],
               float xy2[2],
               int *at_vertex);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *      LONG_DISTANCE        *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function returns the distance between two longitudes.
*
* Called by: determine_region
******************************************************************************/
float long_distance(float west, 
                    float east);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *           MAX             *    method spec 
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
float Max(int nverts, float *verts);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    MILLER_TO_LAT_LON      *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function transforms x and y miller cylindrical coordinates 
*          into latitude and longitude coordinates (world coordinates).
*
* Called by: pt_in_region
******************************************************************************/
int miller_to_lat_lon (float x,
   	 	       float y,
   		       float *lat,
   		       float *lon);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *     MILLER_TRANSFORM      *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in a miller cylindrical 
*          projection.
*
* Called by: convert_pts
******************************************************************************/
float miller_transform (float lat,
   			float lon,
   			float  *x,
   			float  *y);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *   MERCATOR_TO_LAT_LON     *    method spec 
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
   			 float *lon);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *    MERCATOR_TRANSFORM     *    method spec 
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
   			  float  *x,
   			  float  *y);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *           MIN             *    method spec
*        *                           *
*        *****************************
*
*
* Purpose: This function calculates the minimum value in an array of floats
*          and returns that value.
*
* called by: spatial_query_routines__nsidc_initial_check
*******************************************************************************/
float Min(int nverts, float *verts);

/******************************************************************************
*
*        *************************************
*        *                                   *
*        *  OBLIQUE_ORTHOGRAPHIC_TRANSFORM   *    method spec 
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
int oblique_orthographic_transform (float tangent_lat,
   				    float lat,
   				    float lon,
   				    float *x,
   				    float *y);

/******************************************************************************
*
*        *********************************
*        *                               *
*        *    PLATE_CAREE_TO_LAT_LON     *    method spec 
*        *                               *
*        *********************************
*
* Purpose: This function transforms x and y plate carree coordinates
*          into latitude and longitude coordinates (world coordinates).
*
* Called by:  pt_in_region
******************************************************************************/
int plate_carree_to_lat_lon (float x,
   			     float y,
   			     float *lat,
   			     float *lon);

/******************************************************************************
*
*        *********************************
*        *                               *
*        *     PLATE_CARREE_TRANSFORM    *    method spec 
*        *                               *
*        *********************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in a plate carree projection.
*
* Called by: convert_pts
******************************************************************************/
int plate_carree_transform (float lat,
   			    float lon,
   			    float *x,
   			    float *y);

/******************************************************************************
*
*        *********************************
*        *                               *
*        * POLAR_ORTHOGRAPHIC_TO_LAT_LON *    method spec 
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
   				   float *lon);

/******************************************************************************
*
*        *********************************
*        *                               *
*        *  POLAR_ORTHOGRAPHIC_TRANSFORM *    method spec 
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
   				  float *y);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *  POLAR_STEREO_TO_LAT_LON  *    method spec 
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
   			     float *lon);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *  POLAR_STEREO_TRANSFORM   *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function transforms latitude and longitude coordinates (world
*          coordinates) into x and y coordinates in a polar stereographic
*          projection.
*
* Called by: check_pts_along_lon, convert_pts and pt_in_region
******************************************************************************/
int polar_stereo_transform (float lat,
   			    float lon,
    			    float *x,
   			    float *y);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *        PT_IN_POLY         *    method spec 
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
int pt_in_poly(float pt[2],
               int nverts,
               float *verts1,
               float *verts2);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *       PT_IN_REGION        *    method spec 
*        *                           *
*        *****************************
*
* Purpose: This function determines if the point described by pt_x and pt_y
*          is in the region defined by region_x_pts and region_y_pts.  If this
*          point is in the region one is returned otherwise zero is returned.
*
* Called by: check_bounds, check_pts_along_lon, check_point and
*            determine_region
******************************************************************************/
int pt_in_region (float region_x_pts[PT_MAX],  
   		  float region_y_pts[PT_MAX], 
   	          float pt_x,
   		  float pt_y,
                  int region_projection,
                  int point_projection,
   		  int *location,
   	          int region_defined_by_pts);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *     REGION_IN_REGION      *    method spec 
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
		      int reg_def_by_our_pts);

/******************************************************************************
*
*        *****************************
*        *                           *
*        *     WORLD_IN_REGION       *    method spec 
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
                     int reg_def_by_our_pts);

