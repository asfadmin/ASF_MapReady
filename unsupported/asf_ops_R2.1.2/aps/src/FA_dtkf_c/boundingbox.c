#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       boundingbox.c
 
Description:    contains boundingbox()
 
External Functions Defined:
boundingbox()
 
Notes:
        This file written with a 4-character tab setting. 
 
==============================================================================*/
#pragma ident	"@(#)boundingbox.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.boundingbox.c"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "crt_grs_reqq.h"   /* for ll2xyz_(), sangld_(), gcpinq_()  */


/*==============================================================================
Function:      boundingbox()

Description:   Given the lat/lon coords for the four corners of a quadrilateral
                 defined in a DAR, returns the lat/lon coords of the smallest
                 rectangle with edges parallel to meridians and lines of
                 latitude that contains the DAR quad.  Also returns error
                 messages if the quad isn't defined properly.

Parameters:
    Input Parameters:
    nw, ne, se, sw      double[2]      the lat/lon coords of four corners

    Output Parameters:
    lat, lon            double[2]      the lat/lon of bounding box limits

Returns:       void

Creator:       Brian J Griglak

Creation Date: Tue Jul  8 15:37:37 PDT 1997
==============================================================================*/

void boundingbox(double nw[2], double ne[2], double se[2], double sw[2],
    double lat[2], double lon[2])
{
    double pt[2], angle[4];
    double nwxyz[3], nexyz[3], sexyz[3], swxyz[3], ptxyz[3];
    int inside, signflag, i, yes;

    /*
    --- First convert lat/lon coords to Cartesian on unit sphere, which
    --- other FORTRAN routines need for their calculations.
    */

    ll2xyz_(nw, nwxyz);
    ll2xyz_(ne, nexyz); 
    ll2xyz_(se, sexyz);
    ll2xyz_(sw, swxyz);

    /*
    --- Test the internal angle at each vertex to make sure region is convex
    --- and clockwise.  The point in a box routine gcpinq_ needs this to work.
    --- This routine uses 3 points on a sphere, using the shorter part of the
    --- geodesic containing any two points, so it will flag an error if you
    --- try to look at a region that does no fit onto one hemisphere.
    */

    sangld_(&angle[0], nwxyz, nexyz, sexyz);
    sangld_(&angle[1], nexyz, sexyz, swxyz);
    sangld_(&angle[2], sexyz, swxyz, nwxyz);
    sangld_(&angle[3], swxyz, nwxyz, nexyz);

    for(i=0;i<4;i++)
    if (angle[i] < 0.0)
    {
        (void) printf("Please enter the corners in a clockwise order, and make sure the region is convex\n");
        (void) printf("You made an error at the %dth vertex you entered.\n",i+1);
        exit(1);
    }

    /*
    --- Finding the bounds on latitude is an easy min/max problem, since
    --- there is no discontinuity in value like with longitude.
    */

    lat[1] = nw[0];
    if (ne[0] < lat[1]) lat[1] = ne[0];
    if (se[0] < lat[1]) lat[1] = se[0];
    if (sw[0] < lat[1]) lat[1] = sw[0];

    lat[0] = nw[0];
    if (ne[0] > lat[0]) lat[0] = ne[0];
    if (se[0] > lat[0]) lat[0] = se[0];
    if (sw[0] > lat[0]) lat[0] = sw[0];

    /*
    --- Now for longitude, I broke it up into 3 cases:
    ---     case 1: all four longitude coords have same sign
    ---     case 2: different signs, containing points on lon=180
    ---     case 3: different signs, containing points on lon=0
    --- this is discriminated first by signflag, which keeps track of
    --- how many lon coords have positive sign.
    */

    signflag = 0;
    if(nw[1] > 0) signflag++;
    if(ne[1] > 0) signflag++;
    if(se[1] > 0) signflag++;
    if(sw[1] > 0) signflag++;

    /*
    --- For case 1, all same sign, it is again an easy min/max problem
    */

    if ((signflag == 0) || (signflag == 4))
    {
    lon[1] = nw[1]; 
    if (ne[1] > lon[1]) lon[1] = ne[1];
    if (se[1] > lon[1]) lon[1] = se[1];
    if (sw[1] > lon[1]) lon[1] = sw[1];

    lon[0] = nw[1]; 
    if (ne[1] < lon[0]) lon[0] = ne[1];
    if (se[1] < lon[0]) lon[0] = se[1];
    if (sw[1] < lon[0]) lon[0] = sw[1];
    }
    else

    /*
    --- Now we have lon coords of different sign.  Add a full circle to the
    --- points with negative values and calculate center of quad, this moves
    --- the discontinuity from 180 to 360.  Therefore when we take the
    --- average of the values to get the center, we will be accurate if
    --- we are around the 180 meridian, since there is no longer the
    --- discontinuity there.
    */

    {
    if (nw[1] < 0) nw[1] += 360.0;
    if (ne[1] < 0) ne[1] += 360.0;
    if (se[1] < 0) se[1] += 360.0;
    if (sw[1] < 0) sw[1] += 360.0;

        yes = 0;
        pt[1] = 180.0;
        for(pt[0] = 90; pt[0] > -90; pt[0] --)
    {
            ll2xyz_(pt, ptxyz);
            gcpinq_(&inside, nwxyz, nexyz, sexyz, swxyz, ptxyz);
            if (inside != -1) yes = 1;
    }

    /*
    --- Center point inside the quad indicates we are in case 2.
    --- Now we can do a min/max on the points in the (0,360) range.
    */

    if (yes == 1)
    {
        lon[1] = nw[1]; 
        if (ne[1] > lon[1]) lon[1] = ne[1];
        if (se[1] > lon[1]) lon[1] = se[1];
        if (sw[1] > lon[1]) lon[1] = sw[1];

        lon[0] = nw[1]; 
        if (ne[1] < lon[0]) lon[0] = ne[1];
        if (se[1] < lon[0]) lon[0] = se[1];
        if (sw[1] < lon[0]) lon[0] = sw[1];
    }
    else

     /*
     --- The "center" point is not inside the quad, indicates that
     --- we are bridging the discontinuity at 360/0.  So just shift
     --- the corner points back, and reaverage the center point.
     --- Now you shouldn't ever get the error here, since the only
     --- way to trip on error by now would have been to have defined
     --- a region that does not fit in a hemisphere, but that has
     --- already been removed early on by sangld_.
     */

    {
            if (nw[1] > 180.0) nw[1] -= 360.0;
        if (ne[1] > 180.0) ne[1] -= 360.0;
        if (se[1] > 180.0) se[1] -= 360.0;
        if (sw[1] > 180.0) sw[1] -= 360.0;

            yes = 0;
        pt[1] = 0.0;
        for(pt[0] = 90; pt[0] > -90; pt[0] --)
        {
        ll2xyz_(pt, ptxyz);
        gcpinq_(&inside, nwxyz, nexyz, sexyz, swxyz, ptxyz);
        if (inside != -1)
                    yes = 1;
        }

        if (yes == 0) (void) printf("something really screwy here!\n");

        else
        {
        lon[1] = nw[1]; 
        if (ne[1] > lon[1]) lon[1] = ne[1];
        if (se[1] > lon[1]) lon[1] = se[1];
        if (sw[1] > lon[1]) lon[1] = sw[1];

        lon[0] = nw[1]; 
        if (ne[1] < lon[0]) lon[0] = ne[1];
        if (se[1] < lon[0]) lon[0] = se[1];
        if (sw[1] < lon[0]) lon[0] = sw[1];
        }
    }
    }
                
    /*
    --- Need to shift coords back to proper region.  This should only
    --- pertain to points from case 2, but just to be sure nothing
    --- has fallen through the cracks.
    */
/*
    if(lon[0] > 180.0) lon[0] -= 360.0;
    if(lon[1] > 180.0) lon[1] -= 360.0;
*/
    /*
    --- Need to round off latitude to nearest .25, because you can run into
    --- problems at the seam between the polar region and upper latitude
    --- region.  Might as well round off longitude while we're at it.
    */

    lat[0] = floor(4.0 * lat[0] + 0.5) / 4.0;
    lat[1] = floor(4.0 * lat[1] + 0.5) / 4.0;
    lon[0] = floor(4.0 * lon[0] + 0.5) / 4.0;
    lon[1] = floor(4.0 * lon[1] + 0.5) / 4.0;




    return;
}
