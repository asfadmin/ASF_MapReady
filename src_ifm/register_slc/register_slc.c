/******************************************************************************
NAME: register_slc 


SYNOPSIS: register_slc <file 1> <file 2> <igram>
			[-g grid_resolution] [-s [-w]] [-a]
			[-W start_line start_sample height width]

	<file 1> a Single-Look-Complex file (.D and .L or other metadata)
	<file 2> a Single-Look-Complex file (.D and .L or other metadata)
	<igram> the output interferogram (.amp, .phase, .ddr)
	[ -g grid_resolution ] Grid resolution for fico(1).
			(the default is 20, for a 20x20 grid)
	[ -s ] Use Sinc kernel to do the remapping-- this is
			slower, but may result in higher quality.
			The default is bilinear interpolation.
	[ -w ] Create and use a non-linear warping image instead
			of a linear planar map.  See fit_warp(1).
			Can only be used with sinc.
	[ -a ] Use cpx_autofilter to attempt to improve interferogram
			coherence (takes extra time)
	[ -W  <start_line> <start_sample> <height> <width>]
			Specify the window starting place and size.
        <start line>,<start sample> Define the vertical and horizontal
        	coordinates of the top left corner of the desired section.
        <heigth>,<width> Define the height and width of the output images.


DESCRIPTION: does a registration run on two single-integer-complex images,
		by converting them to complex and calling register_cpx.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.00	   O. Lawlor
    1.00   07/97   D.Corbett	 Repaired broken usage
    1.20   04/01   P.Denny	 Converted from script to C
				 enabled variable file lengths
				 less restrictive command line
				 allow autofilter option
				 allow user defined window

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*                                                                           *
*   Register_slc will convert the two images to float complex format, and   *
*		then run register_cpx to coregister the two images.	    *
*   Copyright (C) 2001  Alaska SAR Facility                                 *
*                                                                           *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.                                     *
*                                                                           *
*   This program is distributed in the hope that it will be useful,         *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).                                  *
*                                                                           *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software             *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*                                                                           *
*   ASF APD Contacts:                                                       *
*       Lead Software Engineer - Tom Logan      tlogan@asf.alaska.edu       *
*                                                                           *
*       Alaska SAR Facility                     APD Lab Web Site:           *
*       Geophysical Institute                   www.asf.alaska.edu          *
*       University of Alaska Fairbanks                                      *
*       P.O. Box 757320                                                     *
*       Fairbanks, AK 99775-7320                                            *
*                                                                           *
****************************************************************************/

#include "ceos.h"

#define VERSION 1.20

void usage();

int main(int argc, char **argv)
{
        int 	argNo=4,				/* Current command-line argument number */
		length, width,				/* Image lines & image samples */
		sl,ss,					/* user defined starting point */
		grid_res=20,				/* default grid resolution */
		filterFlag=0,				/* flag for running cpx_autofilter */
		windowFlag=0;				/* flag to allow user to specify windowing */
	char	file1[256], file2[256], igram[256],	/* Command line args */
		sinc[6] = "", warp[6] = "",		/* Command line options */
		command[256];				/* System commands */
	struct IOF_VFDR vfdr1, vfdr2;			/* Meta structures */
	
	for ( ; argNo<argc; argNo++)
	{
		if (argv[argNo][0] != '-')
			usage();
		switch (argv[argNo][1])
		{
			case 'a': /*use cpx_autofilter*/
				filterFlag=1;
				break;
			case 'g': /*get grid resolution*/
				grid_res = atoi(argv[++argNo]);
				break;
			case 's': /*get sinc*/
				strcpy(sinc,"-sinc");
				break;
			case 'w': /*get warp*/
				strcpy(warp,"-warp");
				break;
			case 'W': /*let user set window parameters*/
				windowFlag=1;
				sl = atoi(argv[++argNo]);
				ss = atoi(argv[++argNo]);
				length = atoi(argv[++argNo]);
				width = atoi(argv[++argNo]);
				break;
			default: /*invalid arg*/
				usage();
				break;
		}
	}
	if (argNo == argc)
	{
		strcpy(file1,argv[1]);
		strcpy(file2,argv[2]);
		strcpy(igram,argv[3]);
	}
	else	usage();

	if (warp[0] == '-')
	{
		if (sinc[0] != '-')
		{
			printf("\nCannot use warp if sinc is not used!!\n\n");
			usage();
		}			
	}
		
	/* If user has not specified windowing values get them automatically
	   ----------------------------------------------------------------*/
	if (!windowFlag)
	{
		/* Get meta data for the two images
		   -------------------------------*/
		get_ifiledr(file1,&vfdr1);
		get_ifiledr(file2,&vfdr2);

		/* Get the lesser image length & width for both files
		   -------------------------------------------------*/
		length = (vfdr1.linedata<vfdr2.linedata) ? vfdr1.linedata : vfdr2.linedata;
		width = (vfdr1.datgroup<vfdr2.datgroup) ? vfdr1.datgroup : vfdr2.datgroup;
	}
	
	/* Trim file1 to length & width 
	   and remove top boarder lines and left boarder pixels
	   ---------------------------------------------------*/
	if (windowFlag)	sprintf(command,"trim_slc %s %s %d %d %d %d",file1,file1,sl,ss,length,width);
	else sprintf(command,"trim_slc %s %s %d %d %d %d",
			file1,file1,vfdr1.topbrdr,vfdr1.lbrdrpxl,length,width);
	system(command);

	/* Trim down file2 the same way
	   ---------------------------*/
	if (windowFlag) sprintf(command,"trim_slc %s %s %d %d %d %d",file2,file2,sl,ss,length,width);
	else sprintf(command,"trim_slc %s %s %d %d %d %d",
			file2,file2,vfdr2.topbrdr,vfdr2.lbrdrpxl,length,width);
	system(command);
	
	/* Attempt to improve interferogram coherence if so desired
	   -------------------------------------------------------*/
	if (filterFlag)
	{
		sprintf(command,"cpx_autofilter %s %s %s_fil %s_fil",file1,file2,file1,file2);
		system(command);
		sprintf(command,"cp %s.meta %s_fil.meta",file1,file1);
		system(command);
		sprintf(command,"cp %s.meta %s_fil.meta",file2,file2);
		system(command);
	}

	/* Coregister the two images with any flags specified
	   -------------------------------------------------*/
	if (filterFlag)	sprintf(command,"register_cpx %s_fil %s_fil %s %d %s %s",
					file1,file2,igram,grid_res,sinc,warp);
	else sprintf(command,"register_cpx %s %s %s %d %s %s",file1,file2,igram,grid_res,sinc,warp);
	system(command);

	printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");
	printf("register_slc complete...\n");
}

void usage()
{
	printf("Register_slc:\n");
	printf("Usage: register_slc <file 1> <file 2> <igram>\n");
	printf("          [-g <grid_resolution>] [-s [-w]] [-a]\n");
	printf("	  [-W <start_line> <start_sample> <height> <width>]\n\n");
	printf("  <file 1> a Single-Look-Complex file (.D and .L or other metadata)\n");
	printf("  <file 2> a Single-Look-Complex file (.D and .L or other metadata)\n");
	printf("  <igram> the output interferogram (.amp, .phase, .ddr)\n");
	printf("  [ -g grid_resolution ] Grid resolution for fico(1).\n");
	printf("		(the default is 20, for a 20x20 grid)\n");
	printf("  [ -s ] Use Sinc kernel to do the remapping-- this is\n");
	printf("		slower, but may result in higher quality.\n");
	printf("		The default is bilinear interpolation.\n");
	printf("  [ -w ] Create and use a non-linear warping image instead\n");
	printf("		of a linear planar map.  See fit_warp(1).\n");
	printf("		Can only be used with sinc.\n");
	printf("  [ -a ] Use cpx_autofilter to attempt to improve interferogram\n");
	printf("		coherence (takes extra time)\n");
	printf("  [ -W  <start_line> <start_sample> <height> <width>]\n");
	printf("		Specify the window starting place and size.\n");
        printf("	<start line>,<start sample> Define the vertical and horizontal\n");
        printf("		coordinates of the top left corner of the desired section.\n");
        printf("	<heigth>,<width> Define the height and width of the output images.\n\n");
 	printf("   Register_slc will convert the two images to float complex\n");
	printf("format, and then run register_cpx to coregister the two images,\n");
	printf("using resolve, fico, fit_plane, and remap.\n");
	printf("    After this, interferometry can be done using tandem_ifm.\n");
	printf("Version %.2f, ASF SAR TOOLS\n\n", VERSION);
	exit(1);
}    
