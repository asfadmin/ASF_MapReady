/******************************************************************************
NAME:	psconv - Lon, Lat to Polar Stereographic Coordinate X-forms

SYNOPSIS:	psconv  (interactive program)

DESCRIPTION:    Interactively reads lon, lat coordinates and displays
		the equivalent X,Y polar stereographic coordinates.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    psforint(...)	 Initializes the Forward Polar Stereographic Xform
    psfor(lon,lat,&x,&y) Given lon,lat calculates PS x,y coordinates

FILE REFERENCES:	NONE

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    1/97   T. Logan	Respond to user request

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:	Uses GCTP package

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   psconv  - Interactive geographic to polar stereographic convertor	    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include "asf.h"
#include "cproj.h"

#define VERSION 1.0

int main(int argc,char *argv[])
 {
   double r_maj, r_min;
   double c_lon, c_lat;
   double false_east, false_north;
   double lat, lon, x, y;

   if (argc>1) {
	printf("psconv, an interactive polar-stereographic\n"
              "map projection conversion program.\n"
              "Version %.2f, ASF SAR TOOLS\n\n", VERSION); exit(1);}

   r_maj = 6.37814404299999960000E+06;
   r_min = 6.35675488300000038000E+06;
   c_lon = -45.0 * D2R;
   c_lat = 70.0  * D2R;
   false_east = 0.0;
   false_north = 0.0;
   psforint(r_maj, r_min, c_lon, c_lat, false_east, false_north);

   printf("\nPSCONV: Converts longitude, latitude coordinates into Polar ");
   printf("Stereographic X, Y\n\n");

   lat = lon = 0.0;

   while (lon != -99.0)
     {
      char buf[200];
      printf("\n Enter a longitude, latitude pair (degrees) (-99 to exit): ");
      fgets(buf,200,stdin);
      if (buf[0]=='q'||buf[0]=='e') 
         break;
      sscanf(buf,"%lf %lf",&lon,&lat);
      if (lon==-99.0) break;
      
      lon *= D2R; lat *= D2R;
      psfor(lon,lat,&x,&y);
      printf("\n   GEO Coords: %f %f (radians)\n",lon,lat);
      printf("   PS  Coords: %f %f (meters)\n",x,y);
     }
   
   printf("\n psconv exited\n\n");
   return(0);
 }
