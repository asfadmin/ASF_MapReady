/*************************************************************************
 NAME:	geocode
 
 SYNOPSIS:  geocode [-h ht] [-p pix] [ [-o|-l] N W S E ]  [-i lat lon nl ns ]
                    [-r [-nearest | -bilinear |-sinc | '-kernel x y' ] ]
             	    [-background <fill> ] [-x <file>]
                    <in_img> <projfile> <projkey> <outfile>

 DESCRIPTION:
	Performs automatic geocoding of SAR files.
       Geocode works on slant range, ground range, or
       even already-geocoded image files.
 
 EXTERNAL ASSOCIATES:
     NAME:                USAGE:
     ---------------------------------------------------------------
     geoLatLon            Maps input image points to lat, lon
     projectGeo           Map-projects lat,lon to output image points
     fit_quadratic        Fits a quadratic function to map input to output
     remap                Remaps input image to output projection

 FILE REFERENCES:
     NAME:                USAGE:			         CREATED BY:
     -----------------------------------------------------------------------
     geo_1_$$.tie         ASCII file of (lat,lon,inX,inY)       geoLatLon
     geo_2_$$.tie         ASCII file of (outX,outY,inX,inY)     projectGeo
     geo_3_$$.meta        Output meta with projection info.     projectGeo
     geo_4_$$.map         Mapping function for remap.           fit_quadratic
 
 PROGRAM HISTORY:
     VERS:   DATE:  AUTHOR:	PURPOSE:
     ---------------------------------------------------------------
     1.0     3/99   O. Lawlor	Total Re-Write of geocoding software
     1.1     7/01   R. Gens	Converted into program, added log file switch
     1.25    3/02   P. Denny    Update commandline parsing & usage()
     1.5     3/03   P. Denny    Remove DDR dependency... use only new meta

 HARDWARE/SOFTWARE LIMITATIONS:
 	A projection file must be created before running this program. Use the
      	projprm program to create such a file. See projprm for more details.

 ALGORITHM DESCRIPTION:
 
 ALGORITHM REFERENCES:
 
 BUGS:

*******************************************************************************
*                                                                             *
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

#define VERSION 1.25

int projectGeo(double pixSize, char *metaName, char *in_proj, char *in_key,
               char *in_tie, char *out_tie, char *out_meta, char win_type,
	       char *win);
void geoLatLon(double eleva, char *metaName, char *out_tps);

int main(int argc, char *argv[])
{
	char in_img[255], projfile[255], projkey[255], outfile[255], outWindow[255]="", cmd[255];
	char resample[15]="-nearest", background[20]="";
	float height=0.0, pixel_size=0.0;
	char win_type;
	extern int currArg; /* from cla.h which is in asf.h */

	logflag=0;

    /* Parse command line args */
	while (currArg < (argc-4))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1); /*one string argument: log file */
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile,"a");
			logflag=1;
		}
		else if (strmatch(key,"-background")) {
			CHECK_ARG(1);
			sprintf(background,"-background %s",GET_ARG(1));
		}
		else if (strmatch(key,"-h")) {
			CHECK_ARG(1);  /*float: average input elevation in meters */
			height = atof(GET_ARG(1));
		}
		else if (strmatch(key,"-p")) {
			CHECK_ARG(1);  /*float: output pixel size to pix meters */
			pixel_size=atof(GET_ARG(1));
		}
		else if (strmatch(key,"-r")) {
			CHECK_ARG(1);  /* string: remapping scheme */
			strcpy(resample,GET_ARG(1));
		}
		else if (strmatch(key,"-o")) {
			CHECK_ARG(5);  /* N W S E projection coordinates */
			win_type='o';
			sprintf(outWindow, "%s %s %s %s %s", GET_ARG(5), GET_ARG(4), GET_ARG(3),
								GET_ARG(2), GET_ARG(1));
		}
		else if (strmatch(key,"-l")) {
			CHECK_ARG(5);  /* N S latitude, and E W longitude */
			win_type='l';
			sprintf(outWindow, "%s %s %s %s %s", GET_ARG(5), GET_ARG(4), GET_ARG(3),
								GET_ARG(2), GET_ARG(1));
		}
		else if (strmatch(key,"-i")) {
			CHECK_ARG(5); /* Center lat lon and nl x ns output */
			win_type='i';
			sprintf(outWindow, "%s %s %s %s %s", GET_ARG(5), GET_ARG(4), GET_ARG(3),
								GET_ARG(2), GET_ARG(1));
		}
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}

    /* Nab required arguments */
	strcpy(in_img,  argv[currArg]);
	strcpy(projfile,argv[currArg+1]);
	strcpy(projkey, argv[currArg+2]);
	strcpy(outfile, argv[currArg+3]);

	StartWatch();
	system("date");
	printf("Program: geocode\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: geocode\n\n");
	}

    /* Creating tiepoint file from image metadata */
	geoLatLon(height, in_img, "tmp.geo");

    /* Projecting tie points */
	projectGeo(pixel_size, in_img, projfile, projkey, "tmp.geo", "tmp.tie",
	           outfile, win_type, outWindow);

	if (logflag) FCLOSE(fLog);

    /* Finding least-squares polynomial fit of tie points */
	sprintf(cmd, "fit_quadratic tmp.tie tmp.map"); 
        if (logflag) {
          fLog = FOPEN(logFile, "a");
	  sprintf(cmd, "fit_quadratic -log %s tmp.tie tmp.map", logFile); 
          sprintf(logbuf,"\nCommand line: %s\n", cmd);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        printf("\nCommand line: %s\nDate: ", cmd);
        system(cmd);

    /* Remapping image */
        sprintf(cmd, "remap %s %s -quadratic tmp.map -asDDR tmp %s %s", resample,
	        background, in_img, outfile);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(cmd, "remap %s %s -quadratic tmp.map -asDDR tmp -log %s %s %s",
	          resample, background, logFile, in_img, outfile);
          sprintf(logbuf,"\nCommand line: %s\n", cmd);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        printf("\nCommand line: %s\nDate: ", cmd);
        system(cmd);

	/* clean up and exit gracefully */
 	system("rm tmp.geo tmp.tie tmp.meta tmp.map");

	StopWatch();
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
	  StopWatchLog(fLog);
	  FCLOSE(fLog);
	}

	exit(EXIT_SUCCESS);
}


/* usage - enter here on command-line usage error*/
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-h ht] [-p pix] [ [-o|-l] N W S E ][-i lat lon nl ns ]\n"
	"           [-r <nearest | bilinear | sinc | 'kernel x y'> ]\n"
	"           [-background <fill> ] [-log <file>]\n"
	"           <in_img> <projfile> <projkey> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   in_img    input image (.meta and .img) [from sarin]\n"
	"   projfile  projection definition file name [from projprm]\n"
	"   projkey   projection key name [as passed to projprm]\n"
	"   outfile   output image (.meta and .img)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -h <ht>      Set average input elevation to ht meters.\n" 
	"   -p <pix>     Set output pixel size to pix meters.\n"
	"   -r <...>     Set the output resampling scheme used. (See remap(1)).\n"
	"                 Default is nearest, for nearest-neighbor interpolation.\n"
	"   -background <fill>  Set the background (non-image) parts of the output\n"
	"                        image to the value <fill> (default is zero-- black).\n"
	"   -log <file>  Allows the output to be written to a log file.\n"
	"\n"
	"    Windowing Modes: -o, -l, or -i set the size of the output image.\n" 
	"    The default is just big enough to contain the input image.\n"
	"\n"
	"   -o  Means N W S E are projection coordinates:\n"
	"       N  Y projection coordinate of upper edge\n"
	"       W  X projection coordinate of left edge\n"
	"       S  Y projection coordinate of lower edge\n"
	"       E  X projection coordinate of right edge\n"
	"\n"
	"   -l  Means N S are latitude, and E W are longitude:\n"
	"       N  Latitude of north edge of output\n"
	"       W  Longitude of west edge of output\n"
	"       S  Latitude of south edge of output\n"
	"       E  Longitude of east edge of output\n"
	"\n"
	"   -i  Means write a nl x ns output image with:\n" 
	"       lat  Latitude of center of output\n"
	"       lon  Longitude of center of output\n"
	"       nl   Number of lines in output image\n"
	"       ns   Number of samples in output image\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   %s projects the given SAR image into the given map projection\n"
	"   at the given pixel size. This process is called geocoding.\n",name);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(1);
}

