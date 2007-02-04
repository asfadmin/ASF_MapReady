/******************************************************************************
NAME: refine_offset

SYNOPSIS:  refine_offset <metadata> <DJ# | locSpec> <y> <x>
                                [ [ DJ# | locSpec ] y x ] [...]
DESCRIPTION:
    Refine_offset takes as input a set of points in a SAR image whose latitude
    and longitude are known.  Using this, the program refines the slant range
    and timing offset in the .meta file, which improves the geolocation of the
    image. (for example, in demIFM(1) or geocode(1))

    We have to do this because the CEOS timing information is inaccurate, often
    by a large fraction of a second, in the along-track (azimuth) direction.

    The timing and slant range compensations are written to the the given .meta
    file as "timeShift" and "slantShift", in seconds and meters. Typically,
    the slant range values are very good, and the shift is only a few dozen
    meters. The timing offset is less predictable-- this value is nominally
    about 0.25 seconds (half the synthetic aperture time), but can vary by as
    much as a half second (corresponding to 3 kilometers of geolocation error).

    When the image point and latitude and longitude are accurate, you can
    expect sub-pixel geolocation accuracy (<10m).  If you pick the point
    incorrectly, or mis-type the latitude or longitude, the geolocations of the
    image will be correspondingly shifted.

    It is not necessary, but may help to enter multiple points. Using multiple
    points helps cancel out any random error that may be present in the
    latitudes and longitudes, or uncertainty in location.

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
    2.2	   6/99   O. Lawlor	Added the DJR array locations
    2.5   12/03   P. Denny      Standardize command line parsing & usage
                                  Use meta 1.1+ instead of meta/ddr

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
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/

#include "asf.h"
#include <unistd.h>
#include "asf_meta.h"
#include "offset.h"

#define VERSION 2.5

void usage(char *name);

#include "corner_reflectors.list"


/* insert_time_offset:
 * searches the given AISP input file for the state vector timing offset, then
 * writes the given timing and across-track offset to it. */
void insert_time_offset(double t_offset,double x_offset,char *metaName)
{
	meta_parameters *meta;
	meta = meta_read(metaName);
	meta->sar->time_shift=t_offset;
	meta->sar->slant_shift=x_offset;
	meta_write(meta,metaName);
	meta_free(meta);
}



int main(int argc,char *argv[])
{
	int argStart;
	meta_parameters *meta;
	double t_offset,x_offset;
	char *metaName;
	void *offset;

/*Parse initial Command Line Argument, add points to */
	if (argc<5) usage(argv[0]);
	metaName=argv[1];
	
/*Extract parameter structures from file.*/
	meta=meta_read(metaName);
	offset=init_offset(metaName,meta);

/*Add each point to offset structure.*/
	argStart=2;
	while (argStart+3<=argc)
	{
		double x,y,lat,lon,elev;
		if (3!=sscanf(argv[argStart],"%lf/%lf/%lf",&lat,&lon,&elev))
		{/*Must be a corner reflector: search djArr for it.*/
			int djNum;
			char *inDj=argv[argStart];
			for (djNum=0;djArr[djNum].name!=NULL;djNum++)
				if (0==strcmp_case(inDj,djArr[djNum].name))
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
		insert_time_offset(t_offset,x_offset,metaName);
	else
		printf("Error: Timing offset is not a number!!\n");
#endif

	meta_free(meta);

	return (0);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <metadata> <DJ#|locSpec> <y> <x> [<DJ#|locSpec> <y> <x>] [...]\n",
	name);
 printf("\n"
	"ARGUMENTS:\n"
	"   metdata   Image metadata\n"
	"   DJ#       The name of a DJ-series corner reflector (e.g. 'DJ3')\n"
	"   locSpec   A latitude, longitude, and elevation (meters),\n"
	"               as lat/lon/elev (e.g. '65.23/-145.72/0.0')\n"
	"   y         Line position of point (pixels)\n"
	"   x         Sample position of point (pixels)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Takes as input a set of points in a SAR image whose latitudes and longitudes\n"
	"   are known.  Using this, the program refines the timing offset estimate in\n"
	"   the .in file, as well as an across-track offset estimate, which improves the\n"
	"   geolocation of the image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}
