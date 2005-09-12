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
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "airsar_io.h"

void airsar2ddr(char* airsarname, struct DDR *ddrOut)
{
    char dtype[50];
    int dt, i;

printf("Starting AirSAR Header -> DDR conversion!\n");
/* The necessary parts of a DDR file: setting up. *
 *------------------------------------------------*
 ********************void airsar2ddr(char* airsarname, struct DDR *ddrOut)
******************************/
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

void airsar2meta(char* airsarname, meta_parameters *meta)
{
    char dtype[50];
    int dt, i;

    strcpy(meta->general->sensor, "AirSAR");

    /* number of lines in image	*/
    meta->general->line_count = 
      atoi(get_airsar(airsarname, "FIRST", "NUMBER OF LINES IN IMAGE"));
    printf(".");

    /* number of samples in image */
    meta->general->sample_count = 
      atoi(get_airsar(airsarname, "FIRST", "NUMBER OF SAMPLES PER RECORD"));
    printf(".");

    /* number of bands in image	*/
    meta->general->band_number = 0; 
    printf(".");

    meta->general->y_pixel_size = 
      atof(get_airsar(airsarname, "FIRST", "AZIMUTH PIXEL SPACING (METERS)"));
    printf(".");
    meta->general->x_pixel_size = 
      atof(get_airsar(airsarname, "FIRST", "RANGE PIXEL SPACING (METERS)"));
    printf(".");

    /* data type of pixels (unsigned char, short int, float)*/
    strcpy(dtype, get_airsar(airsarname, "FIRST", "DATA TYPE"));
    if(!strcmp(dtype, "BYTE")) meta->general->data_type = BYTE;
    else if(!strcmp(dtype, "INTEGER*2")) meta->general->data_type = INTEGER16;
    else if(!strcmp(dtype, "INTEGER*4")) meta->general->data_type = INTEGER32;
    else if(!strcmp(dtype, "REAL*4")) meta->general->data_type = REAL32;
    else if(!strcmp(dtype, "REAL*8")) meta->general->data_type = REAL64;
    printf(".");

    /* computer system data is on */
    strcpy(meta->general->system, "ieee_std");
    printf(".");

    /* line relative to master image. */
    meta->general->start_line = 0; 
    printf(".");

    /* sample relative to master image */
    meta->general->start_sample = 0; 
    printf(".");

    /* line increment for sampling */
    meta->sar->line_increment = 1; 
    printf(".");

    /* sample increment for sampling */
    meta->sar->sample_increment = 1; 
    printf(".\n");

    return;
}


