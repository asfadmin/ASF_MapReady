#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       latlon2grs.c
 
Description:    contains thin() and boundingbox()
 
External Functions Defined:
boundingbox()
 
Notes:
        This file written with a 4-character tab setting. 
==============================================================================*/
#pragma ident	"@(#)latlon2grs.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.latlon2grs.c"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/*==============================================================================
Function:      thin()

Description:   Given the latitude in the GRS polar regions, returns the
                 thinning parameter - the number of path numbers to skip
                 due to the proximity of meridians.  It tries to maintain
                 a spacing of about 40 km between the paths, which are now
                 running parallel to meridians.
               This should work in all cases, but it does not.  When at
                 very extreme latitudes, the thinning number doesn't change
                 at the exact same latitudes that the row number does, so I
                 am implementing a case basis to insure only proper GRS
                 coordinate pairs are generated.

Parameters:
    Input Parameters:
    row                 int            row number for point (new)

    Output Parameters:
    thin                int            the thinning parameter

Returns:       int

Creator:       Brian J Griglak

Creation Date: Tue Jul  8 19:08:30 PDT 1997
==============================================================================*/

static int thin(int row)
{
    switch (row)
    {
     case 142 :                                  return 12; break;
     case 143 :                                  return 11; break;
     case 144 :                                  return 10; break;
     case 145 :                                  return 9;  break;
     case 146 : case 147 :                       return 8;  break;
     case 148 : case 149 :                       return 7;  break;
     case 150 : case 151 : case 152 : case 153 : return 6;  break;
     case 154 : case 155 : case 156 : case 157 : return 5;  break;
     case 158 : case 159 : case 160 : case 161 :
            case 162 : case 163 : case 164 : return 4;  break;
     case 437 : case 438 : case 439 : case 440 :
            case 441 : case 442 : case 443 : return 4;  break;
     case 444 : case 445 : case 446 : case 447 : return 5;  break;
     case 448 : case 449 :                       return 6;  break;
     case 171 : case 430 :                       return 2;  break;
         default :                                   return 3;  break;
    }
}


/*==============================================================================
Function:      latlon2grs()

Description:   Give the lat/lon of a point, returns the GRS path and row values
                 as defined in the J-ERS-1 Operation Interface Specifications

Parameters:
    Input Parameters:
    lat, lon            double         the lat/lon coords of the point

    Output Parameters:
    path, row           int            the path and row values

Returns:       void

Creator:       Brian J Griglak

Creation Date: Tue Jul  8 18:20:06 PDT 1997
==============================================================================*/

void latlon2grs(double lat_D, double lon_0, int *path, int *row)
{
    double lat_C,    /* latitude in geocentric coords */
       ecc,      /* eccentricity of earth from being sphere */
       I,        /* inclination of orbit = 82.338 deg */
       gamma,    /* angle in orbit from ascending node */
       theta_0,  /* lon difference from ascending node to point */
       lon_A,    /* lon of ascending node for this path */
       lon_A0,   /* lon of ascending node of path 659 = -1.153 deg */
       W,        /* angular velocity of satellite = 3.744318 deg/min */
       We,       /* angular velocity of earth = 0.25 deg/min */
       pathreal, /* real number from path calculations */
       lon_0n,   /* longitude of path 659, row 170 */
       lon_0s,   /* longitude of path 659, row 431 */ 
       lat_n,    /* latitude of row 170 */
       lat_s,    /* latitude of row 431 */
       Pi;

    if ((lat_D > 86.094) || (lat_D < -82.538))
    {
        *path = 0;
        *row = 0;
        return;
    }

    Pi = 4.0 * atan(1.0);
    ecc = 6356.757 / 6378.142;  /* polar radius over equatorial */
    I = 82.338;
    lon_A0 = -1.153;
    W = 3.744318;
    We = 0.25;
    lon_0n = -155.169;
    lon_0s = 128.827;
    lat_n = 76.044;
    lat_s = -76.044;

    I *= Pi / 180.0;

    lat_C  = atan( tan(lat_D * Pi / 180.0) * ecc * ecc);

    if ((lat_D < 75.633) && (lat_D > -75.633))
    {
    gamma    = Pi - asin( sin(lat_C) / sin(I));
    theta_0  = atan( tan(gamma) * cos(I)) + Pi;

    theta_0 *= 180.0 / Pi;
    gamma   *= 180.0 / Pi;

    lon_A    = lon_0 + theta_0 + gamma * We / W;
    pathreal = (lon_A0 - lon_A) / 360.0 * 659.0;

        while (pathreal < 0.0)
            pathreal += 659.0;

    *row = (int) floor(gamma / 360.0 * 600.0 + 1.0);

        if ((*row < 401) && (*row > 200))
        *path = (int) floor(pathreal + 0.5);
    else
        *path = (int) (floor((pathreal - 1) / 2.0 + 0.5) * 2 + 1);

        if (*path == 0) *path = 659;
    }
    else
    {
    lat_C *= 180.0 / Pi;
    if (lat_D > 0.0)
    {
        pathreal = (lon_0n - lon_0) / 360.0 * 659.0;
        *row = (int) floor((lat_n - lat_C) / 0.358 + 170.5);
        }
    else
    {
        pathreal = (lon_0s - lon_0) / 360.0 * 659.0;
        *row = (int) floor((lat_s - lat_C) / 0.358 + 431.5);
    }

    if (pathreal < 0.0) pathreal += 659.0;

    *path = (int) (floor((pathreal - 1.0) / thin(*row) + 0.5) *
        thin(*row) + 1);

    /*
    ---  Need to add a little fix here to catch points right on the 
    ---    edge of the discontinuity.
    */ 
        if (*path == 661) *path = 1;
    }

    return;
}
