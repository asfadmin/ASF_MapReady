/******************************************************************************
NAME: refine_offset

SYNOPSIS:  refine_offset metadata ddr_file [ DJ# | locSpec ] y x
                                [ [ DJ# | locSpec ] y x ] [...]
DESCRIPTION:
    Refine_offset takes as input a set of points in a SAR image
    whose latitude and longitude are known.  Using this, the
    program refines the slant range and timing offset in
    the .meta file, which improves the geolocation of the image.
    (for example, in demIFM(1) or geocode(1))

          We have to do this because the CEOS timing information is
    inaccurate, often by a large fraction of a second, in the
    along-track (azimuth) direction.

        The timing and slant range compensations are written to
    the the given .meta file as "timeShift" and "slantShift",
    in seconds and meters.  Typically, the slant range values
    are very good, and the shift is only a few dozen meters.
    The timing offset is less predictable-- this value is
    nominally about 0.25 seconds (half the synthetic aperture
    time), but can vary by as much as a half second
    (corresponding to 3 kilometers of geolocation error).

        When the image point and latitude and longitude are
    accurate, you can expect sub-pixel geolocation accuracy
    (<10m).  If you pick the point incorrectly, or mis-type
    the latitude or longitude, the geolocations of the image
    will be correspondingly shifted.

        It is not necessary, but may help to enter multiple
    points.  Using multiple points helps cancel out any
    random error that may be present in the latitudes and
    longitudes, or uncertainty in location.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	   6/98   O. Lawlor 	For Interferometric DEM generation
    1.1	   6/98   O. Lawlor	Corrected NAN bug.
    1.2	   6/98   O. Lawlor	Forgot to copy over elevation!
    1.3	   6/98   O. Lawlor	Corrected typo in list of DJs.
    2.0	   6/98   O. Lawlor	Added non-changing version, check_offset
    2.1	   7/98   O. Lawlor	Added multiple points, across-track offset.
    2.2	   6/99   O. Lawlor	Added the DJR array locations.	

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   refine_offset -- refines the timing offset estimate in the .in file,    *
*                    as well as an across-track offset estimate,	    *
*		     which improves the geolocation of the image.           *
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
#include <unistd.h>
#include "ddr.h"
#include "geolocate.h"
#include "asf_meta.h"
#include "offset.h"

#define VERSION 2.2

void usage(char *name);

#include "corner_reflectors.list"

/*
	Insert_time_offset searches the given AISP input
file for the state vector timing offset, then writes the
given timing and across-track offset to it.
*/

void insert_time_offset(double t_offset,double x_offset,char *ceos)
{
	meta_parameters *meta;
	meta=meta_init(ceos);
	meta->geo->timeShift=t_offset;
	meta->geo->slantShift=x_offset;
	meta_write(meta,ceos);
}



int main(int argc,char *argv[])
{
	int argStart;
	meta_parameters *meta;
	double t_offset,x_offset;
	char *ceos,*ddrFile;
	struct DDR ddr;
	void *offset;
/*Parse initial Command Line Arguments, add points to */
	if (argc<6) usage(argv[0]);
	ceos=argv[1];
	ddrFile=argv[2];
	
/*Extract parameter structures from file.*/
	c_getddr(ddrFile,&ddr);
	meta=meta_init(ceos);
	offset=init_offset(ceos,meta,&ddr);

/*Add each point to offset structure.*/
	argStart=3;
	while (argStart+3<=argc)
	{
		double x,y,lat,lon,elev;
		if (3!=sscanf(argv[argStart],"%lf/%lf/%lf",
				&lat,&lon,&elev))
		{/*Must be a corner reflector: search djArr for it.*/
			int djNum;
			char *inDj=argv[argStart];
			for (djNum=0;djArr[djNum].name!=NULL;djNum++)
				if (0==strcasecmp(inDj,djArr[djNum].name))
				{
					lat=djArr[djNum].lat;
					lon=djArr[djNum].lon;
					elev=djArr[djNum].elev;
					goto foundDj;
				}
			printf("Error! String '%s' is neither a \n"
				"location specification nor a dj corner reflector!\n",
				inDj);
			exit(1);
		foundDj:
			printf("Matched corner reflector '%s'\n",inDj);
		}
		printf("\tFinding lat=%f, lon=%f, elev=%f\n",lat,lon,elev);
		y=atof(argv[argStart+1]);
		x=atof(argv[argStart+2]);
		add_offset(offset,x,y,lat,lon,elev);
		argStart+=3;
	}
	if (argStart!=argc) usage(argv[0]);
	
/*Refine timing offset*/
	refine_offset(offset,&t_offset,&x_offset);
	
/*Dump timing offset to file.*/
#ifndef CHECK_OFFSET
	if (t_offset==t_offset) /*"Self ==" actually fails for NAN's.*/
		insert_time_offset(t_offset,x_offset,ceos);
	else
		printf("Error: Timing offset is not a number!!\n");
#endif

	return (0);
}

void usage(char *name)
{
 printf("\n"
	"Usage: %s metadata ddr_file  [ DJ# | locSpec ] y x \\\n"
	"                       [ [ DJ# | locSpec ] y x ] \\\n"
	"                       [ ... ]\n"
	"\n"
	"     metdata: image metadata, either .meta or .L\n"
	"     ddr_file: Image from which the coordinates were taken\n"
	"     DJ#: the name of a DJ-series corner reflector (e.g. 'DJ3')\n"
	"     locSpec: a latitude, longitude, and elevation (meters),\n"
	"             as lat/lon/elev (e.g. '65.23/-145.72/0.0')\n"
	"     y: line position of point (pixels)\n"
	"     x: sample position of point (pixels)\n"
	"\n"
	"Refine_offset takes as input a set of points in a SAR image \n"
	"whose latitudes and longitudes are known.  Using this, the \n"
	"program refines the timing offset estimate in the .in file,\n"
	"as well as an across-track offset estimate,\n"
	"which improves the geolocation of the image.\n",
	name);
 printf("\nVersion %.2f, ASF SAR Tools\n\n", VERSION);
 exit(1);
}
