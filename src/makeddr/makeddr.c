/******************************************************************************
NAME:  makeddr -- creates a ddr file from the command line.

SYNOPSIS: 
   
   makeddr [-p <projection> <parameters> <ULn> <ULe> <LRn> <LRe> <pdist>]
   		[-d <datum code>] [-log <file>] <filename> <nl> <ns> <type> 

DESCRIPTION: 
	
 	Create a LAS 6.0 DDR (Data Descriptor Record) from the command line.
This is only useful when you don't have a DDR already (which is actually pretty
rare, but when you need one it's tough to get along without it).  This
might happen if you're importing data from an source that doesn't use
LAS (the Land Analysis System).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/97   O. Lawlor    Needed DDR's for new data, to test
    				 processing programs before ingestion
    				 programs were finalized.
    2.0	    4/98   T. Logan     Need ability to create valid LAS ddr files
				 without using LAS. (*including geolocations*)
    3.0	    6/00   P. Denny	Added projections GEOGRAPH, PLSTEREO, 
				 LAMAZEQA, and ALBERS to the existing UTM
    3.1	    7/00   T. Logan     Modified Albers to store proj.parms in DMS
    3.11    7/01   R. Gens 	Added logfile switch
    3.5	    9/01   P. Denny	Fixed command line parsing
    				 added datum code option
    				 cleaned up the code
				 some error checking

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:    

*****************************************************************************
*								            *
*   makeddr -- creates a ddr file from the command line.		    *
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
#include "ddr.h"
#include "las.h"
#include "locinc.h"
#include "cproj.h"

#define VERSION 3.5

enum projections {GEOGRAPH=1,PLSTEREO,UTM,LAMAZEQA,ALBERS};

void geograph(struct DDR*);
void plstereo(struct DDR*, char**);
void utm(struct DDR*, char**);
void lamazeqa(struct DDR*, char**);
void albers(struct DDR*, char**);
void corner_pdist(struct DDR*, char**);
void usage(char*);


int main(int argc, char **argv)
{
    struct DDR ddr;		/* DDR structure */
    char *key,datatype[7],proj[9];
    char fname[256];		/* DDR File name */
    char *projParam[4], *corners[5];/* pointers to projection information */
    int ii;			/* string size for projection */
    int nl,ns;			/* Number of lines, number of samples */
    int type=0;			/* data type value for DDR */
    int argNo=1;		/* command line argument counter*/
    int projection=0;		/* One of five projections, initialized as false */
    int datumFlag=0;		/* Did user specify datum code */

    logflag=0;			/* initialize flag for no logging */

/* allocate memory for pointers */
    for (ii=0; ii<4; ii++)
	projParam[ii] = (char*)MALLOC(sizeof(char)*128);
    for (ii=0; ii<5; ii++)
	corners[ii] = (char*)MALLOC(sizeof(char)*128);

/* initialize ddr structure */
    c_intddr(&ddr);

/* Parse command line arguments
------------------------------*/
/* Optional cla's */
    for (argNo=1; argNo<argc; ++argNo)
    {
	if (argc == argNo+4) break;
	if (argv[argNo][0] != '-') usage(argv[0]);
	switch (argv[argNo][1])
	{
	    case 'p': /*Get projection parameters & corners and meters per pixel (projParam & corners)*/
		strcpy(proj,argv[++argNo]);
		ii = strlen(proj);
		c_low2up(proj,&ii);
		if (0==strcmp(proj,"GEOGRAPH"))
		{
		    projection = GEOGRAPH;
		}
		else if (0==strcmp(proj,"PLSTEREO"))
		{
		    projection = PLSTEREO;
		    projParam[0] = argv[++argNo];
		    projParam[1] = argv[++argNo];
		}		    
		else if (0==strcmp(proj,"UTM"))
		{
		    projection = UTM;
		    projParam[0] = argv[++argNo];
		}
		else if (0==strcmp(proj,"LAMAZEQA"))
		{
		    projection = LAMAZEQA;
		    projParam[0] = argv[++argNo];
		    projParam[1] = argv[++argNo];
		    projParam[2] = argv[++argNo];
		}
		else if (0==strcmp(proj,"ALBERS"))
		{
		    projection = ALBERS;
		    projParam[0] = argv[++argNo];
		    projParam[1] = argv[++argNo];
		    projParam[2] = argv[++argNo];
		    projParam[3] = argv[++argNo];
		}
		else 
		{
		    sprintf(errbuf, "ERROR: %s does not support the %s projection\n\n",argv[0],proj);
		    printErr(errbuf);
		}
		corners[0] = argv[++argNo];/*uln*/
		corners[1] = argv[++argNo];/*ule*/
		corners[2] = argv[++argNo];/*lrn*/
		corners[3] = argv[++argNo];/*lre*/
		corners[4] = argv[++argNo];/*meters per pixel*/
		break;
	    case 'd': /*datum code*/
		ddr.datum_code = atoi(argv[++argNo]);
		ddr.valid[2] = VALID;
		datumFlag=1;
		break;
	    case 'l': /*use a logfile*/
		sprintf(logFile,"%s",argv[++argNo]);
		fLog = FOPEN(logFile, "a");
		logflag=1;
		break;
	    default: /*invalid arg*/
		usage(argv[0]);
		break;
	}
    }
/* Required cla's */
    if (argc == argNo+4)
    {
	strcpy(fname,argv[argNo++]);
	nl=atol(argv[argNo++]);
	ns=atol(argv[argNo++]);
	key=argv[argNo++];
	if ((0==strcmp(key,"byte")) || (0==strcmp(key,"char"))) {type=1; strcpy(datatype,"byte");}
	else if (0==strcmp(key,"short")) {type=2; strcpy(datatype,"short");}
	else if (0==strcmp(key,"long")) {type=3; strcpy(datatype,"long");}
	else if (0==strcmp(key,"float")) {type=4; strcpy(datatype,"float");}
	else if (0==strcmp(key,"double")) {type=5; strcpy(datatype,"double");}
	else {sprintf(errbuf, "ERROR: Unrecognized data type '%s'.\n\n",key); printErr(errbuf);}
    }
    else usage(argv[0]);
    
/* Finally, we can begin DDR creation 
-----------------------------------*/
    printf("\nCreating ddr...\n");
    printf("    Lines:     %i\n", nl);
    printf("    Samples:   %i\n", ns);
    printf("    Data type: %s\n", datatype);
 
/* Enter default info into DDR structure
----------------------------------------*/
    ddr.nl = nl;
    ddr.ns = ns;
    ddr.nbands = (int)1;
    ddr.dtype = (int)type;

    ddr.pdist_x = 12.5;			/* dummy values */
    ddr.pdist_y = 12.5;

    if (!datumFlag)
    {
	ddr.datum_code = (int)0;
	ddr.valid[2] = INVAL;		/* Datum Code */
    }

    strcpy(ddr.proj_units,"METERS");	/* Changed to degrees if GEOGRAPH projection */
    ddr.valid[4] = INVAL;		/* Projection Units (can be modified by projection fuctions */

    ddr.valid[5] = INVAL;		/* Projection Distance (can be modified by *corner_pdist()) */

    ddr.line_inc = 1.0;
    ddr.sample_inc = 1.0;
    ddr.valid[7] = VALID;		/* Increment */

    ddr.master_line = (int)1;
    ddr.master_sample = (int)1;

/* Enter projection specific info into DDR structure
---------------------------------------------------*/
    if (projection)
    { 
	if (projection == GEOGRAPH)	 geograph(&ddr);
	else if (projection == PLSTEREO) plstereo(&ddr, projParam);
	else if (projection == UTM) 	 utm(&ddr, projParam);
	else if (projection == LAMAZEQA) lamazeqa(&ddr, projParam);
	else if (projection == ALBERS)	 albers(&ddr, projParam);
	else {sprintf(errbuf,"\tERROR: %s does not support the %s projection\n\n",argv[0],proj); printErr(errbuf);}
	corner_pdist(&ddr, corners);
    }

/* Write to error log if specified
---------------------------------*/
    if (logflag)
    {
	StartWatchLog(fLog);
	printLog("Creating ddr...\n");
    	sprintf(logbuf,"    Lines:     %i\n", nl);	   printLog(logbuf);
    	sprintf(logbuf,"    Samples:   %i\n", ns);	   printLog(logbuf);
	sprintf(logbuf,"    Data type: %s\n\n", datatype); printLog(logbuf);
    }

/* Put DDR structure info into specified file
---------------------------------------------*/
    if (c_putddr(fname, &ddr) != E_SUCC)
       {sprintf(errbuf,"\tERROR: Error returned from putddr\n\n"); printErr(errbuf);}
    printf("\nSuccessfully created %s DDR file\n\n",fname);
    return(0);

} /******** END PROGRAM ********/




/* Enter DDR info for geographic
-------------------------------*/
void geograph(struct DDR *ddr)
{
	printf("\n    Adding GEOGRAPH projection information...\n");

	ddr->proj_code = (int)0;
	ddr->valid[0] = VALID;		/* Projection Code	*/
	ddr->zone_code = (int)62;
	ddr->valid[1] = VALID;		/* Zone Code N/A	*/
	ddr->valid[3] = VALID;	        /* Projection Parameters*/
	strcpy(ddr->proj_units,"DEGREES");
	ddr->valid[4] = VALID;		/*  Projection Units	*/
}

/* Enter DDR info for Polar Stereographic
----------------------------------------*/
void plstereo(struct DDR *ddr, char **projParam)
{
    	double par_deg, lon_deg, par_dms, lon_dms;
	
    	par_deg = atof(projParam[0]);
    	lon_deg = atof(projParam[1]);

	printf("\n    Adding PLSTEREO projection information...\n");

	printf("\tStandard Parallel: %f degrees\n",par_deg);
    	if (E_SUCC != c_degdms(&par_deg,&par_dms,"DEG","LAT"))
	  {sprintf(errbuf,"ERROR: %f is not a valid latitude\n\n",par_deg); printErr(errbuf);}
	printf("\tCentral Longitude: %f degrees\n",lon_deg);
	if (E_SUCC != c_degdms(&lon_deg,&lon_dms,"DEG","LON"))
	  {sprintf(errbuf,"ERROR: %f is not a valid longitude\n\n",lon_deg); printErr(errbuf);}

	ddr->proj_code = (int)6;
	ddr->valid[0] = VALID;		/* Projection Code	*/
	ddr->zone_code = (int)62;
	ddr->valid[1] = VALID;		/* Zone Code N/A	*/
	ddr->valid[3] = VALID;	        /* Projection Parameters*/
	ddr->valid[4] = VALID;		/* Projection Units	*/

	ddr->proj_coef[0] = (double)6378273;
	ddr->proj_coef[1] = (double)0.006693883;
	ddr->proj_coef[5] = par_dms;
	ddr->proj_coef[4] = lon_dms;
}

/* Enter DDR info for Universal Transverse Mercator
--------------------------------------------------*/
void utm(struct DDR *ddr, char **projParam)
{
	int zone = atoi(projParam[0]);

	printf("\n    Adding UTM projection information...\n");

    	printf("\tZone: %i\n",zone);

	ddr->proj_code = (int)1;
        ddr->valid[0] = VALID;		/* Projection Code       */
	ddr->zone_code = (int)zone;
        ddr->valid[1] = VALID;		/* Zone Code 	         */
        ddr->valid[3] = VALID;	        /* Projection Parameters */
	ddr->valid[4] = VALID;		/* Projection Units	*/
}

/* Enter DDR info for Lambert Azimuthal Equal Area
-------------------------------------------------*/
void lamazeqa(struct DDR *ddr, char **projParam)
{
    	double lat_deg, lon_deg, radius, lat_dms, lon_dms;

    	lat_deg = atof(projParam[0]);
    	lon_deg = atof(projParam[1]);
    	radius  = atof(projParam[2]);

	printf("\n    Adding LAMAZEQA projection information...\n");

	printf("\tCentral Latitude: %f degrees\n",lat_deg);
	if (E_SUCC != c_degdms(&lat_deg,&lat_dms,"DEG","LAT"))
	  {sprintf(errbuf,"ERROR: %f is not a valid latitude\n\n",lat_deg); printErr(errbuf);}
	printf("\tCentral Longitude: %f degrees\n",lon_deg);
	if (E_SUCC != c_degdms(&lon_deg,&lon_dms,"DEG","LON"))
	  {sprintf(errbuf,"ERROR: %f is not a valid longitude\n\n",lon_deg); printErr(errbuf);}

	printf("\tRadius of reference sphere: %f meters\n", radius);

	ddr->proj_code = (int)11;
        ddr->valid[0] = VALID;		/* Projection Code       */
	ddr->zone_code = (int)62;
        ddr->valid[1] = VALID;		/* Zone Code N/A    	 */
        ddr->valid[3] = VALID;	        /* Projection Parameters */
	ddr->valid[4] = VALID;		/* Projection Units	*/
	
    	ddr->proj_coef[0] = radius;
    	ddr->proj_coef[4] = lat_dms;
    	ddr->proj_coef[5] = lon_dms;
}

/* Enter DDR info for Albers Conical Equal Area
----------------------------------------------*/
void albers(struct DDR *ddr, char **projParam)
{
    	double lat1, lat2, cenMer, latorigin;

    	lat1 = atof(projParam[0]);
    	lat2 = atof(projParam[1]);
    	cenMer = atof(projParam[2]);
	latorigin = atof(projParam[3]);

	/* Report parameters and put in .ddr in dms form */
	printf("\n    Adding ALBERS projection information...\n");

    	printf("\tFirst Standard Parallel Parameter: %f degrees\n", lat1);
	if (E_SUCC != c_degdms(&lat1,&(ddr->proj_coef[2]),"DEG","LAT"))
	  {sprintf(errbuf,"ERROR: %f is not a valid latitude\n\n",lat1); printErr(errbuf);}
    	printf("\tSecond Standard Parallel Parameter: %f degrees\n", lat2);
	if (E_SUCC != c_degdms(&lat2,&(ddr->proj_coef[3]),"DEG","LAT"))
	  {sprintf(errbuf,"ERROR: %f is not a valid latitude\n\n",lat2); printErr(errbuf);}
    	printf("\tCentral Meridian: %f degrees\n", cenMer);
	if (E_SUCC != c_degdms(&cenMer,&(ddr->proj_coef[4]),"DEG","LON"))
	  {sprintf(errbuf,"ERROR: %f is not a valid longitude\n\n",cenMer); printErr(errbuf);}
	printf("\tLatitude of Origin: %f degrees\n", latorigin);
        if (E_SUCC != c_degdms(&latorigin,&(ddr->proj_coef[5]),"DEG","LAT"))
	  {sprintf(errbuf,"ERROR: %f is not a valid latitude\n\n",latorigin); printErr(errbuf);}

	ddr->proj_code = (int)3;
	ddr->valid[0] = VALID;		/* Projection Code       */
	ddr->zone_code = (int)62;
	ddr->valid[1] = VALID;		/* Zone Code N/A    	 */
	ddr->valid[3] = VALID;	        /* Projection Parameters */
	ddr->valid[4] = VALID;		/* Projection Units	*/
}

/* Enter corner coordinates and pixel size to DDR struct
   necessary for all projections
--------------------------------------------------------*/
void corner_pdist(struct DDR *ddr, char **corners)
{
	double uln,ule,lrn,lre;
	double pdist;

	uln = atof(corners[0]);
	ule = atof(corners[1]);
        lrn = atof(corners[2]);
	lre = atof(corners[3]);
	pdist = atof(corners[4]);

    	printf("\n    Pixel Size: %f meters\n", pdist);

	ddr->upleft[0]  = uln; ddr->upleft[1] = ule;
	ddr->loleft[0]  = lrn; ddr->loleft[1] = ule;
	ddr->upright[0] = uln; ddr->upright[1] = lre;
	ddr->loright[0] = lrn; ddr->loright[1] = lre;
        ddr->valid[6] = VALID;		/* Corner Coordinates    */

	ddr->pdist_x = ddr->pdist_y = pdist;
        ddr->valid[5] = VALID;		/* Projection Distance   */
}

void usage(char *name)
{
    printf("\nUSAGE:\n");
    printf("  %s [-p <projection> <projection parameters> <uln> <ule> <lrn> <lre> <pdist>]\n",name);
    printf("\t [-d <datum code>] [-log <file>] <filename> <nl> <ns> <type>\n\n");
    printf("  Options:\n");
    printf("\t-l   - Option to have output written to a log <file>\n");
    printf("\t-d   - Option to enter the <datum code>\n");
    printf("\t	 	(see man page for more information)\n");
    printf("\t-p   - Specific projection type & parameters\n");
    printf("\t		(see man page for more information)\n");
    printf("\t\t   Projections	| Parameters\n");
    printf("\t\t   -------------+-----------------------------------------\n");
    printf("\t\t   GEOGRAPH	| N/A\n");
    printf("\t\t   PLSTEREO	| Standard Parallel, Central Meridian\n");
    printf("\t\t   UTM		| Zone\n");
    printf("\t\t   LAMAZEQA	| Central Latitude, Central Longitude,\n");
    printf("\t\t		|	Radius of reference sphere\n");
    printf("\t\t   ALBERS	| 1st Standard Parallel, 2nd Std Parallel,\n");
    printf("\t\t		|	Central Meridian\n");
    printf("  Required inputs:\n");
    printf("\tfilename - Base name for .ddr (use same name as the base image name)\n");
    printf("\tnl\t - Number of lines in image\n");
    printf("\tns\t - Number of samples in image\n");
    printf("\ttype\t - Data type used for image.  Types are:\n");
    printf("\t\t\tbyte, short, long, float, double\n\n");
    printf("  Description:\n");
    printf("\tThe makeddr program will construct a 1-band ddr\n");
    printf("\tfile with nl lines, ns samples, and data type.\n\n");
    printf("  Version %.2f, ASF SAR Tools\n",VERSION);
    exit(1);
}

