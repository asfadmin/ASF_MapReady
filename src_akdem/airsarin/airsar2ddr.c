/******************************************************************************
NAME:  airsar2ddr.c

SYNOPSIS:

DESCRIPTION:  Extracts a data record from a AirSAR encoded file.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    2/01   J. Badgley   Extract ddr data for a AirSAR image.
    1.1     6/01   T. Logan     Fixed calls that reference DEM record.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   <one line to give the program's name and a brief idea of what it does.> *
*   Copyright (C) 2001  ASF STEP LAB                                        *
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
*   ASF STEP LAB Contacts:						    *
*	Lab Coordinator   - Rick Guritz		rguritz@images.alaska.edu   *
*	Software Engineer - Tom Logan		tlogan@images.alaska.edu    *
* 									    *
*	Alaska SAR Facility			STEP Lab Web Site:	    *	
*	Geophysical Institute			www.images.alaska.edu	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include "asf.h"
#include "airsar_io.h"

void airsar2ddr(char* airsarname, struct DDR *ddrOut)
{
    char dtype[50];
    int dt, i;

printf("Starting AirSAR Header -> DDR conversion!\n");
/* The necessary parts of a DDR file: setting up. *
 *------------------------------------------------*
 **************************************************/
    c_intddr(ddrOut);

    /* number of lines in image	*/
    ddrOut->nl = atoi(get_airsar(airsarname, "FIRST", "NUMBER OF LINES IN IMAGE"));
    printf(".");

    /* number of samples in image */
    ddrOut->ns = atoi(get_airsar(airsarname, "FIRST", "NUMBER OF SAMPLES PER RECORD"));
    printf(".");

    /* number of bands in image	*/
    ddrOut->nbands = 1; 
    printf(".");

    ddrOut->pdist_y = atof(get_airsar(airsarname, "FIRST", "AZIMUTH PIXEL SPACING (METERS)"));
    printf(".");
    ddrOut->pdist_x = atof(get_airsar(airsarname, "FIRST", "RANGE PIXEL SPACING (METERS)"));
    printf(".");

    strcpy(dtype, get_airsar(airsarname, "FIRST", "DATA TYPE"));

    if(!strcmp(dtype, "BYTE")) dt=EBYTE;
    else if(!strcmp(dtype, "INTEGER*2")) dt=EWORD;
    else if(!strcmp(dtype, "INTEGER*4")) dt=ELONG;
    else if(!strcmp(dtype, "REAL*4")) dt=EREAL;
    else if(!strcmp(dtype, "REAL*8")) dt=EDOUBLE;
    else dt=NULL;

    /* data type of pixels (unsigned char, short int, float)*/
    ddrOut->dtype = dt;
    printf(".");

    /* computer system data is on */
    strcpy(ddrOut->system, "ieee-std"); 
    printf(".");

    /* line relative to master image. */
    ddrOut->master_line = 1; 
    printf(".");

    /* sample relative to master image */
    ddrOut->master_sample = 1; 
    printf(".");

    for(i=0; i<8; i++) {
      ddrOut->valid[i] = INVAL; /* valid flags: 0, 1, 2		*/
    printf(".");
    }

    ddrOut->valid[4] = VALID;           /* Projection Units    */
    ddrOut->valid[5] = VALID;           /* Projection Distance */
    ddrOut->valid[7] = VALID;           /* Increment           */

    /* Projection units (GCTP units+other) */
    strcpy(ddrOut->proj_units, "METERS"); 
    printf(".");

    /*  Not currently implemented
    ddrOut->upleft[0] = atof(get_airsar(airsarname, "DEM", "LATITUDE OF CORNER 2")); 
    ddrOut->upleft[1] = atof(get_airsar(airsarname, "DEM", "LONGITUDE OF CORNER 2")); 
    ddrOut->loleft[0] = atof(get_airsar(airsarname, "DEM", "LATITUDE OF CORNER 4")); 
    ddrOut->loleft[1] = atof(get_airsar(airsarname, "DEM", "LONGITUDE OF CORNER 4")); 
    ddrOut->upright[0] = atof(get_airsar(airsarname, "DEM", "LATITUDE OF CORNER 1"));
    ddrOut->upright[1] = atof(get_airsar(airsarname, "DEM", "LONGITUDE OF CORNER 1")); 
    ddrOut->loright[0] = atof(get_airsar(airsarname, "DEM", "LATITUDE OF CORNER 3")); 
    ddrOut->loright[1] = atof(get_airsar(airsarname, "DEM", "LONGITUDE OF CORNER 3")); 

    ddrOut->pdist_y = atof(get_airsar(airsarname, "DEM", "Y-DIRECTION POST SPACING"));
    ddrOut->pdist_x = atof(get_airsar(airsarname,"DEM", "X-DIRECTION POST SPACING")); 
    */

    /* line increment for sampling */
    ddrOut->line_inc = 1.0; 
    printf(".");

    /* sample increment for sampling */
    ddrOut->sample_inc = 1.0; 
    printf(".\n");

    printf("Finished DDR conversion!\n");

    return;
}
