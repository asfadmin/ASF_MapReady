#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       create_grs_list.c

Description:    takes a darid and, if it exists,  creates the linked list
                  structure of GRS coordinate nodes

External Functions Defined:
create_grs_list()

Notes:
        This file written with a 4-character tab setting. 

==============================================================================*/
#pragma ident	"@(#)create_grs_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.create_grs_list.c"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "list_handling.h"
#include "crt_grs_reqq.h"


/*==============================================================================
Function:      create_grs_list()

Description:   Given a darid and the pointer to the head of the list structure,
                 this calls load_dar() to retrieve all the relevant geometry
                 from the DAR, as well as the observation period.  It then 
                 calculates an appropriate bounding box for a search to find
                 all the GRS points inside the region.  It takes these points
                 and adds them to the list, which happens to be sorted, and
                 returns the list head pointer and the observation period
                 string needed for the REQQ records.
               If no problems were encountered with the load or creation, then
                 it returns 1, otherwise i returns the load error code, or
                 a 0 if there was some problem in the routine, like a bad
                 shape definition.

Parameters:
    Input Parameters:
    darid        long                  the darid we are currently processing

    grslist      * struct pathnode     pointer to the head of the list structure

    Output Parameters:
    observe      * char[]              double pointer to string representing
                                         the observation period specified in
                                         the dar.  is double pointer because
                                         it was assigned in load_dar() and then
                                         passed through here back to main,
                                         then into REQQ_append()

Returns:       int

Creator:       Brian J Griglak

Creation Date: Thu Jul 24 14:30:20 PDT 1997
==============================================================================*/

int create_grs_list(
    long darid,
    PATH *grslist,
    FILE *logfp  )

{

    double lat[2], lon[2], testpt[2], center[2];
    double nwxyz[3], nexyz[3], sexyz[3], swxyz[3];
    double centerxyz[3], testxyz[3];
    double radius, nw[2], ne[2], se[2], sw[2];
    double dlat, dlon, Pi, dist, lonoff ;
    int path, row, inside, dar_status;
    int mask_flag;
    char shape;
    double station_xyz[3], station_radius;

    double side_distance ;
    double min_side_distance ;
    /* 
    -- the following variables are used to interface with 
    -- mps_cgen_() and mpsintrp(), a routines 
    -- that uses real*4   The "_4" appended to each name 
    -- is a reminder that they are just 4-byte reals.  
    */
    #define CGEN_NUMBER_OF_POINTS  20
    int    j, npoints ;
    float  p1_lat_4 ;
    float  p1_lon_4 ;
    float  p2_lat_4 ;
    float  p2_lon_4 ;
    float  center_lat_4 ;
    float  center_lon_4 ;
    float  clats_4[CGEN_NUMBER_OF_POINTS] ;
    float  clons_4[CGEN_NUMBER_OF_POINTS] ;
    float  radius_km_4 ;



    Pi = 4.0 * atan(1.0);

    /*
    --- Load the dar and see if any errors occured with the load.
    --- If so, quit before problems happen and program crashes.
    */

    dar_status = load_dar(darid, &shape, nw, ne, se, sw, logfp);

    if (dar_status != 1)
        return dar_status;

    if (shape == 'P')
        radius = ne[0] ;
    else
        radius = 0.0;

    /*
    --- Conversion from float to double, since Fortran routines all use
    --- doubles.  Note that I had to use pointer to array of floats so
    --- that values would be returned.  May have been easier to have had
    --- 10 parameters instead of 6, but I didn't go that way.
    */


    /* 
    -- the station_radius, returned here, is the station 
    -- mask radius divided by the earth radius and is 
    -- therefore in radians, so that the routine gcdist_() can be 
    -- used as a distance test on the unit sphere.  
    */
    mask_flag = station_mask_flag(shape, radius, nw, ne, se, sw, station_xyz,
        &station_radius, logfp);

    if ((mask_flag != 1) && (mask_flag != STATION_MASK_FLAG_QUAD_OVERLAPS_MASK)
    && (mask_flag != STATION_MASK_FLAG_CIRCLE_OVERLAPS_MASK))
        return mask_flag;

    if ((mask_flag == STATION_MASK_FLAG_QUAD_OVERLAPS_MASK) 
    ||  (mask_flag == STATION_MASK_FLAG_CIRCLE_OVERLAPS_MASK))
        (void) fprintf(logfp,
 "DAR overlaps station mask, make sure it has been processed as data-takes.\n");

    if (shape == 'Q')
    {
        /*
        --- First case, a quadrilateral
        */

        /*
        --- First convert the lat/lon to Cartesian coords here, instead of each
        --- time through the loop.  Speeds things up a little
        */
        ll2xyz_(nw, nwxyz);
        ll2xyz_(ne, nexyz);
        ll2xyz_(se, sexyz);
        ll2xyz_(sw, swxyz);

        /* determine the min side.    */

        (void) gcdist_( &min_side_distance, nwxyz, nexyz ) ; 
        (void) gcdist_( &side_distance, nexyz, sexyz ) ; 
        if( min_side_distance > side_distance )
            min_side_distance = side_distance ;
        (void) gcdist_( &side_distance, sexyz, swxyz ) ; 
        if( min_side_distance > side_distance )
            min_side_distance = side_distance ;
        (void) gcdist_( &side_distance, swxyz, nwxyz ) ; 
        if( min_side_distance > side_distance )
            min_side_distance = side_distance ;

        /* 
        -- gcdist_() distances are on the unit 
        -- sphereKM.  convert to kilometers.  
        -- (perfect accuracy not important here)
        */
        min_side_distance *= 6378.0 ;
        if( min_side_distance < 100.0 )
        {
            /* this is the SMALL QUADRILATERAL case.  */

            /*
            -- for small quadrilaterals.  In the case of small quadrilaterals, 
            -- (for example, a square 10 km on a side) this could easily 
            -- result in none of the .25-degree spaced points being inside 
            -- the box, and therefore result in no GRS request.  
            -- large quadrilaterals would be OK.  
            -- For small quadrilaterals, we will force a node insertion 
            -- at each of the corners, plus 2 extra, evenly-spaced points
            -- along each of the 4 sides, like this:  
            --                                           
            --       +---+---+---+
            --       |           |                       
            --       +           +                       
            --       |           |                       
            --       +           +                       
            --       |           |                       
            --       +---+---+---+
            --                                           
            --  Each of the points marked '+' will be converted to a GRS
            --  and inserted, a total of 12 points.  
            --
            --  A "small" quadrilateral is where the shortest side is 
            --  less than 100 km.  
            --
            --  FUTURE MODIFICATION RECOMMENDATION:  
            --  A better method for ALL quadrilaterals would be to derive 
            --  points from the edges and create .25-degree-spaced points 
            --  in the interior of the quadrilateral, and then insert them 
            --  all, instead of the current method.  
            --
            --  The current method is to expand the box and create test 
            --  points that are spaced by about .25 degrees, then to test 
            --  the points to see if they are inside the quadrilateral.  
            --  For small quadrilaterals, it is possible that none of 
            --  the .25 degree points are inside, and thus provide no 
            --  GRS requests at all.  
            */
            /* small quadrilateral case, continued.  */

            npoints = 4 ;

            /* points for SIDE 1:  nw-ne    */
            /* interface with mpsintrp_()   */
            p1_lat_4 = nw[0] ;
            p1_lon_4 = nw[1] ;
            p2_lat_4 = ne[0] ;
            p2_lon_4 = ne[1] ;
            (void) mpsintrp_( 
                &p1_lat_4, &p1_lon_4, 
                &p2_lat_4, &p2_lon_4, 
                &npoints, 
                clats_4, clons_4 ) ;
            /* 
            -- the points were returned as 
            -- lats (in the clats_4[] and clons_4[] arrays)
            -- now insert the GRS for each of these points 
            -- except the last point.  
            -- when we request 4 points, this includes the 
            -- start and end points, and the 2 intermediate points 
            -- are computed.  We wish to write the first 3 of the 
            -- 4 points, so as not to insert each corner point twice.
            -- 
            -- this is why "j < (npoints - 1)" is used as the 
            -- loop limit.  
            */
            for( j = 0 ; j < (npoints - 1)  ; j++ )
            {
                latlon2grs((double)clats_4[j], (double)clons_4[j], &path, &row);
                if ((row <= 439) && (row >= 142))
                    insert_node(grslist, NULL, path, row);
            }

            /* small quadrilateral case, continued.  */
            /* points for SIDE 2:  ne-se    */
            /* interface with mpsintrp_()   */
            p1_lat_4 = ne[0] ;
            p1_lon_4 = ne[1] ;
            p2_lat_4 = se[0] ;
            p2_lon_4 = se[1] ;
            (void) mpsintrp_( &p1_lat_4, &p1_lon_4, &p2_lat_4, &p2_lon_4, 
                &npoints, clats_4, clons_4 ) ;
            for( j = 0 ; j < (npoints - 1)  ; j++ )
            {
                latlon2grs((double)clats_4[j], (double)clons_4[j], &path, &row);
                if ((row <= 439) && (row >= 142))
                    insert_node(grslist, NULL, path, row);
            }

            /* points for SIDE 3:  se-sw    */
            /* interface with mpsintrp_()   */
            p1_lat_4 = se[0] ;
            p1_lon_4 = se[1] ;
            p2_lat_4 = sw[0] ;
            p2_lon_4 = sw[1] ;
            (void) mpsintrp_( &p1_lat_4, &p1_lon_4, &p2_lat_4, &p2_lon_4, 
                &npoints, clats_4, clons_4 ) ;
            for( j = 0 ; j < (npoints - 1)  ; j++ )
            {
                latlon2grs((double)clats_4[j], (double)clons_4[j], &path, &row);
                if ((row <= 439) && (row >= 142))
                    insert_node(grslist, NULL, path, row);
            }

            /* small quadrilateral case, continued.  */
            /* points for SIDE 4:  sw-nw    */
            /* interface with mpsintrp_()   */
            p1_lat_4 = sw[0] ;
            p1_lon_4 = sw[1] ;
            p2_lat_4 = nw[0] ;
            p2_lon_4 = nw[1] ;
            (void) mpsintrp_( &p1_lat_4, &p1_lon_4, &p2_lat_4, &p2_lon_4, 
                &npoints, clats_4, clons_4 ) ;
            for( j = 0 ; j < (npoints - 1)  ; j++ )
            {
                latlon2grs((double)clats_4[j], (double)clons_4[j], &path, &row);
                if ((row <= 439) && (row >= 142))
                    insert_node(grslist, NULL, path, row);
            }
            /* small quadrilateral case, end.  */

        }  /* end if( min_side_distance < 100.0 )    */
        /*
        --- Test for north or south pole being inside the region, because that
        --- would make the boudingbox routine have problems.  If a pole is
        --- encountered, then we'll do a search over all longitudes in the
        --- latitude strip of the region.  If not, call the boundingbox()
        --- routine to refine the search parameters
        */

        testxyz[0] = 0.0;
        testxyz[1] = 0.0;
        testxyz[2] = 1.0;
        gcpinq_(&inside, nwxyz, nexyz, sexyz, swxyz, testxyz);

        if (inside == 1)
        {
            lat[0] = 86.0;
            lon[0] = -179.0;
            lon[1] = 179.0;
            lat[1] = nw[0];
            if (ne[0] < lat[1]) lat[1] = ne[0];
            if (se[0] < lat[1]) lat[1] = se[0];
            if (sw[0] < lat[1]) lat[1] = sw[0];
        }
        else
        {
            testxyz[2] = -1.0;
            gcpinq_(&inside, nwxyz, nexyz, sexyz, swxyz, testxyz);
            if (inside == 1)
            {
                lat[1] = -86.0;
                lon[0] = -179.0;
                lon[1] = 179.0;
                lat[0] = nw[0];
                if (ne[0] > lat[0]) lat[0] = ne[0];
                if (se[0] > lat[0]) lat[0] = se[0];
                if (sw[0] > lat[0]) lat[0] = sw[0];
            }
            else
                boundingbox(nw, ne, se, sw, lat, lon);
        }
        /* quadrilateral case, continued  */

        /*
        --- Error check, may not be necessary, but doesn't hurt.
        */

        if (lat[1] > lat[0])
        {
            (void) printf("lat[0]: %f, lat[1]: %f\n", lat[0], lat[1]);
            (void) printf("No GRS coords!\n");
            return CALC_GRS_ERROR_NO_GRS_COORDS;
        }

        /* quadrilateral case, continued  */
        /*
        --- Do sequential search over longitude the 
        --- latitude.  Using a 0.25 degree
        --- increment, which is finer than the GRS 
        --- mesh, so as to achieve good
        --- edge definition for circles or 
        --- steep sides of quadrilateral.  Test
        --- each point to see if it is inside 
        --- the region.  If it is, calculate
        --- the GRS coords and add the point to the list structure.
        */
        /* quadrilateral case, continued  */

        testpt[0] = lat[0] + 1.0;
        while (testpt[0] > lat[1] - 1.0)
        {
            testpt[1] = lon[0] - 1.0;
            while (testpt[1] <= lon[1] + 1.0)
            {
                ll2xyz_(testpt, testxyz);
                gcpinq_(&inside, nwxyz, nexyz, sexyz, swxyz, testxyz);
                if (inside != -1)
                {
                    if (mask_flag == STATION_MASK_FLAG_QUAD_OVERLAPS_MASK)
                    {
                        (void) gcdist_(&dist, testxyz, station_xyz);
                        if (dist > station_radius)
                        {
                            latlon2grs(testpt[0], testpt[1], &path, &row);
                            if ((row <= 439) && (row >= 142))
                                insert_node(grslist, NULL, path, row);
                        }
                    }
                    else
                    {
                        latlon2grs(testpt[0], testpt[1], &path, &row);
                        if ((row <= 439) && (row >= 142))
                                insert_node(grslist, NULL, path, row);
                    }       
                }

                /* quadrilateral case, continued  */

                testpt[1] += 0.25;
                if ((testpt[1] < 0.0) && (lon[1] > 180.0)) 
                    testpt[1] += 360.0;

            }   /*  while (testpt[1] <= lon[1] + 1.0)   */

            testpt[0] -= 0.25;

        } /* end while (testpt[0] > lat[1] - 1.0)    */

        return dar_status;

    }  /* if (shape == 'Q') (quadrilateral case)  */
    else if (shape == 'P')
    {
        /*
        --- Second case, circular region (Point and radius)
        */

        /*
        --- Calculate rough bounds for a boundingbox, 
        --- based on geometry of sphere.
        --- Note later that we will need to 
        --- possibly expand the longitude region
        --- due to fact that this calculates 
        --- distances on circle of constant
        --- latitude, and not along geodesics, 
        --- which may span greater longitudinal
        --- range.  Then I calculate the corners 
        --- of a diamond that the circle
        --- circumscribes, and do a boundingbox calculation on the diamond.
        */

        center[0] = nw[0];
        center[1] = nw[1];


        if( radius < 200.0 )
        {
            /* 
            -- special processing for small circles, to force 
            -- the inclusion of GRS points.  
            --
            -- GRS points are about 61 km from each other 
            -- at the equator.  
            -- the usual method is to include all GRS points 
            -- which lie within the circle.  
            -- But for small circles ( 10 km radius, for example, )
            -- There can easily be no GRS points which are inside 
            -- the circle.  
            -- Therefore, for "small" circles, we force the inclusion 
            -- of the GRS point corresponding to the center of the 
            -- circle.  We do the same for 20 evenly-spaced points 
            -- on the edge of the circle.  
            -- For "large" circles, it is OK to include only GRS points 
            -- within the circle.  
            */

            /* 
            -- insert the GRS corresponding to the 
            -- center point:  
            */
            latlon2grs(center[0], center[1], &path, &row);
            if ((row <= 439) && (row >= 142))
                insert_node(grslist, NULL, path, row);

            /* 
            -- create npoints equally-spaced points on the circumference of 
            -- the circle, and then check out each of these, putting 
            -- in the GRS for each.  
            */
            npoints = CGEN_NUMBER_OF_POINTS ;
            center_lat_4 = center[0] ;
            center_lon_4 = center[1] ;
            radius_km_4 = radius ;
            (void) mps_cgen_( 
                &center_lat_4, &center_lon_4, &radius_km_4, &npoints, 
                clats_4, clons_4 ) ;
            /* 
            -- the points were returned as 
            -- lats (in the clats_4[] and clons_4[] arrays)
            -- now insert the GRS for each of these points:  
            */
            for( j = 0 ; j < npoints ; j++ )
            {
                latlon2grs((double)clats_4[j], (double)clons_4[j], &path, &row);
                if ((row <= 439) && (row >= 142))
                    insert_node(grslist, NULL, path, row);
            }

        } /* end if radius < 500.0    */

        /* 
        -- compute dlat and dlon, the delta latitude and 
        -- longitude, for the circle.  
        */
        dlat = radius / 6356.757;
        dlon = radius / (6356.757 * cos(center[0] * Pi / 180.0));
        dlat *= 180.0 / Pi;
        dlon *= 180.0 / Pi;
        nw[0] = center[0] + dlat;
        nw[1] = center[1];
        ne[0] = center[0];
        ne[1] = center[1] + dlon;
        se[0] = center[0] - dlat;
        se[1] = center[1];
        sw[0] = center[0];
        sw[1] = center[1] - dlon;
        if (ne[1] > 180.0) ne[1] -= 360.0;
        if (sw[1] < -180.0) sw[1] += 360.0;

        /* circle case, continued  */
        if ((nw[0] > 90.0) || (se[0] < -90.0))
        {
            /*
            --- Like the quads, if the circular region 
            --- contains a pole, then just
            --- set the search parameters to 
            --- the entire longitude range in the
            --- latitudinal strip the circle overlaps.
            */

            if (nw[0] > 90.0)
            {
                lat[0] = 87.0;
                lat[1] = se[0];
            }
            else
            {
                lat[0] = nw[0];
                lat[1] = -83.0;
            }
            lon[0] = -180.0;
            lon[1] = 180.0;

            ll2xyz_(center, centerxyz);

            /*
            --- Now the sequential search for points that 
            --- are less than the radius
            --- distance away along a geodesic.  Note 
            --- the large redundancy in the
            --- code here is due to the fact that 
            --- here I am searching the full
            --- -180 to 180 longitude range, 
            --- which is complete, whereas below I
            --- start with the estimated range, 
            --- and then expand it where I need
            --- to.  If I did that here, and the 
            --- circle overlapped the seem at 180,
            --- then I would be making the longiutde 
            --- range more than 360 degrees.
            */
            /* circle case, continued  */
            testpt[0] = lat[0];
            while (testpt[0] > lat[1] - 1.0)
            {
                testpt[1] = lon[0];
                while (testpt[1] <= lon[1])
                {
                    ll2xyz_(testpt, testxyz);
                    gcdist_(&dist, testxyz, centerxyz);
                    dist *= 6356.757;
                    if (dist < radius)
                    {
                        if (mask_flag == STATION_MASK_FLAG_CIRCLE_OVERLAPS_MASK)
                        {
                            gcdist_(&dist, testxyz, station_xyz);
                            if (dist > station_radius)
                            {
                                latlon2grs(testpt[0], testpt[1], &path, &row);
                                if ((row <= 439) && (row >= 142))
                                    insert_node(grslist, NULL, path, row);
                            }
                        }
                        else
                        {
                            latlon2grs(testpt[0], testpt[1], &path, &row);
                            if ((row <= 439) && (row >= 142))
                                insert_node(grslist, NULL, path, row);
                        }
                    }
                    testpt[1] += 0.25;
                }
                testpt[0] -= 0.25;
            }   /* end while (testpt[1] <= lon[1])  */
        }  /* if ((nw[0] > 90.0) || (se[0] < -90.0))    (includes pole)  */
        else
        {
            /* circle case, continued  */
            boundingbox(nw, ne, se, sw, lat, lon);
            if (lat[0] > 86.0) lat[0] = 86.0;
            if (lat[1] < -82.0) lat[1] = -82.0;

            ll2xyz_(center, centerxyz);
            testpt[0] = lat[0] + 1.0;
            while (testpt[0] > lat[1] - 1.0)
            {
                /*
                --- Here's where the search range gets 
                --- expanded if it needs to be.  Due
                --- to the fact that I did not travel 
                --- on geodesics, the longitude range
                --- may be a little small, so I calculate 
                --- the distance from the left
                --- edge of each search row and if it 
                --- is less than the radius, back
                --- up a degree at a time until I 
                --- am out of the circle.  To be on the
                --- safe side, I go back yet another 
                --- degree, and then increase the end
                --- longitude by the same amount, which 
                --- due to the symmetry of the
                --- situation is good enough.
                */

                testpt[1] = lon[0];
                ll2xyz_(testpt, testxyz);
                gcdist_(&dist, testxyz, centerxyz);
                dist *= 6356.757;
                while (dist < radius)
                {
                    testpt[1] -= 1.0;
                    ll2xyz_(testpt, testxyz);
                    gcdist_(&dist, testxyz, centerxyz);
                    dist *= 6356.757;
                }
                testpt[1] -= 1.0;
                lonoff = lon[0] - testpt[1];

                /*
                --- Then the sequential search again, 
                --- where points that are in the
                --- circle are converted to GRS and 
                --- added to the list structure.
                */
                /* circle case, continued  */

                while (testpt[1] < lon[1] + lonoff)
                {
                    ll2xyz_(testpt, testxyz);
                    gcdist_(&dist, testxyz, centerxyz);
                    dist *= 6356.757;
                    if (dist < radius)
                    {
                        /* test point is inside circle; it is good.  */
                        if (mask_flag == STATION_MASK_FLAG_CIRCLE_OVERLAPS_MASK)
                        {
                            gcdist_(&dist, testxyz, station_xyz);
                            if (dist > station_radius)
                            {
                                latlon2grs(testpt[0], testpt[1], &path, &row);
                                if ((row <= 439) && (row >= 142))
                                    insert_node(grslist, NULL, path, row);
                            }
                        }
                        else
                        {
                            latlon2grs(testpt[0], testpt[1], &path, &row);
                            if ((row <= 439) && (row >= 142))
                                insert_node(grslist, NULL, path, row);
                        }
                    }
                    testpt[1] += 0.25;
                    if ((testpt[1] < 0.0) && (lon[1] + lonoff > 180.0)) 
                        testpt[1] += 360.0;
                }
                testpt[0] -= 0.25;
            }
        }

        /* end of circle case */
        return dar_status;

    }  /* else if (shape == 'P')   (circle case )  */
    else 
        return LOAD_DAR_ERROR_BAD_SHAPE;

}

