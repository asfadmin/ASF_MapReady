/*************************************************************************
 NAME:	image_layers
 
 SYNOPSIS:  image_layers [-look] [-incidence] [-latitude] [-longitude] 
			 [-range] infile

 DESCRIPTION:
		Image_layers creates image layers for data attributes
 
 EXTERNAL ASSOCIATES:

 FILE REFERENCES:
 
 PROGRAM HISTORY:
     VERS:   DATE:  AUTHOR:	PURPOSE:
     ---------------------------------------------------------------
     1.0    11/03   R. Gens	Original development

 HARDWARE/SOFTWARE LIMITATIONS:

 ALGORITHM DESCRIPTION:
 
 ALGORITHM REFERENCES:
 
 BUGS:
 
****************************************************************************/

/******************************************************************************
*                                                                             *
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
#include "asf_meta.h"
#include "lzFetch.h"
#include "jpl_proj.h"

#define VERSION 1.0
#define BUFSIZE 1024
#ifndef PI
# define PI 3.14159265358979323846
#endif

double get_satellite_height(double time, stateVector stVec)
{
        return sqrt(stVec.pos.x*stVec.pos.x+stVec.pos.y*stVec.pos.y+stVec.pos.z*stVec.pos.z);
}

double get_earth_radius(double time, stateVector stVec, double re, double rp)
{
        double er = sqrt(stVec.pos.x*stVec.pos.x+stVec.pos.y*stVec.pos.y+stVec.pos.z*stVec.pos.z);
        double lat = asin(stVec.pos.z/er);
        return (re*rp)/sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
}

double get_slant_range(meta_parameters *meta, double er, double ht, int sample)
{
        double minPhi=acos((ht*ht+er*er-meta->geo->slantFirst*meta->geo->slantFirst)/(2.0*ht*er));
        double phi=minPhi+sample*(meta->geo->xPix/er);
        double slantRng=sqrt(ht*ht+er*er-2.0*ht*er*cos(phi));
        return slantRng+meta->geo->slantShift;
}

double get_look_angle(double er, double ht, double sr)
{
	return acos((sr*sr+ht*ht-er*er)/(2.0*sr*ht));
}

double get_incidence_angle(double er, double ht, double sr)
{
	return PI-acos((sr*sr+er*er-ht*ht)/(2.0*sr*er));
}

void usage(char *name);

int main(int argc, char *argv[])
{
        FILE *fpIn=NULL, *fpLook=NULL, *fpIncid=NULL, *fpLat=NULL, *fpLon=NULL, *fpRange=NULL;
        meta_parameters *meta;
	struct DDR outddr;
	stateVector stVec;
	int lookFlag=0, incidFlag=0, latFlag=0, lonFlag=0, rangeFlag=0, nl, ns, ii, kk, ll, size, deskewed;
        char *inFile=NULL, metaFile[255], outLook[255], outIncid[255], outLat[255], outLon[255], outRange[255];
	char *inBuf=NULL, *projected; 
	float *bufLook=NULL, *bufIncid=NULL, *bufLat=NULL, *bufLon=NULL, *bufRange=NULL;
	double latitude, longitude, time, doppler, earth_radius, satellite_height, range, look_angle, incidence_angle;
	double ignored, re=6378144.0, rp=6356754.9, px, py, startX, startY, perX, perY;

	logflag=0;
	currArg=1;	/* from cla.h which is in asf.h */

	/* Parse command line args */
	while (currArg < (argc-1))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-look")) 
			lookFlag=1; /* look angle image */
		else if (strmatch(key,"-incidence")) 
			incidFlag=1;  /* incidence angle image */
		else if (strmatch(key,"-latitude")) 
			latFlag=1;  /* latitude image */
		else if (strmatch(key,"-longitude")) 
			lonFlag=1;  /* longitude image */
		else if (strmatch(key,"-range")) 
			rangeFlag=1;  /* slant range image */
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 1) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	/* Get required arguments */
	inFile = argv[currArg];
	sprintf(metaFile, "%s.meta", inFile); 
	sprintf(outLook, "%s_look", inFile);
	sprintf(outIncid, "%s_incid", inFile);
	sprintf(outLat, "%s_lat", inFile);
	sprintf(outLon, "%s_lon", inFile);
	sprintf(outRange, "%s_range", inFile);

	StartWatch();
	system("date");
	printf("Program: image_layers\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: image_layers\n\n");
	}

	/* Prepare for reading/writing */
	nl = lzInt(metaFile, "ifm.orig_lines:", NULL);        
	ns = lzInt(metaFile, "ifm.orig_samples:", NULL);
        fpIn = fopenImage(inFile, "rb");
        inBuf = (char *) MALLOC(ns * sizeof(char) * BUFSIZE);

        if (lookFlag) {
	  fpLook = fopenImage(outLook,"wb");
          bufLook = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);        
	}
        if (incidFlag) {
	  fpIncid = fopenImage(outIncid,"wb");
          bufIncid = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
	}        
        if (latFlag) {
	  fpLat = fopenImage(outLat,"wb");
          bufLat = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
	}        
        if (lonFlag) {
	  fpLon = fopenImage(outLon,"wb");
          bufLon = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
	}        
        if (rangeFlag) {
	  fpRange = fopenImage(outRange,"wb");
          bufRange = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);        
	}

	/* Create metadata */
        meta = meta_init(inFile);
	c_intddr(&outddr);
	outddr.nl = nl;
	outddr.ns = ns;
	outddr.nbands = 1;
	outddr.dtype = 4;

        if (lookFlag) {
	  meta_write(meta, outLook);
          c_putddr(outLook,&outddr);
	}
        if (incidFlag) {
	  meta_write(meta, outIncid);
          c_putddr(outIncid,&outddr);
	}
        if (latFlag) {
	  meta_write(meta, outLat);
          c_putddr(outLat,&outddr);
	}
        if (lonFlag) {
	  meta_write(meta, outLon);
          c_putddr(outLon,&outddr);
	}
        if (rangeFlag) {
	  meta_write(meta, outRange);
          c_putddr(outRange,&outddr);
	}

	/* Read, convert and write data */
	size = BUFSIZE;
	doppler = 0.0;
	deskewed = lzInt(metaFile, "geo.deskew:", NULL);
	projected = lzStr(metaFile, "geo.type:", NULL);        

 	for (ii=0; ii<nl; ii+=size) {
          if ((nl-ii)<BUFSIZE) size = nl-ii;
/*	  FREAD(inBuf, sizeof(char), ns*size, fpIn);    for layers that require data values */

	  if (strncmp(projected, "P", 1)==0) {
	    /* calculation in case the imagery is map projected */
	    re = lzDouble(metaFile, "geo.proj.re_major:", NULL);
	    rp = lzDouble(metaFile, "geo.proj.re_minor:", NULL);
	    startX = lzDouble(metaFile, "geo.proj.startX:", NULL);
	    startY = lzDouble(metaFile, "geo.proj.startY:", NULL);
	    perX = lzDouble(metaFile, "geo.proj.perX:", NULL);
	    perY = lzDouble(metaFile, "geo.proj.perY:", NULL);
	    for (ll=0; ll<size; ll++) 
	      for (kk=0; kk<ns; kk++) {
		px= startX + perX * kk;
                py= startY + perY * (ii+ll);
                proj_to_ll(meta->geo, px, py, &latitude, &longitude);
                latLon2timeSlant(meta, latitude, longitude, &time, &range, &doppler);
		stVec = meta_get_stVec(meta, time);
		earth_radius = get_earth_radius(time, stVec, re, rp);
		satellite_height = get_satellite_height(time, stVec);
	        look_angle = get_look_angle(earth_radius, satellite_height, range);
		incidence_angle = get_incidence_angle(earth_radius, satellite_height, range);
		incidence_angle = meta_incid(meta, ii+ll, kk);

	        if (lookFlag) bufLook[kk+ll*ns] = (float) look_angle*R2D;
		if (incidFlag) bufIncid[kk+ll*ns] = (float) incidence_angle*R2D;
	        if (latFlag) bufLat[kk+ll*ns] = (float) latitude;
	        if (lonFlag) bufLon[kk+ll*ns] = (float) longitude;
	        if (rangeFlag) bufRange[kk+ll*ns] = (float) range;
	      }
	  }
	  else
	    /* calculation in case the imagery is in slant range or ground range */
	    for (ll=0; ll<size; ll++) 
	      for (kk=0; kk<ns; kk++) {
		time = meta_get_time(meta, ii+ll, kk);
		stVec = meta_get_stVec(meta, time);
		earth_radius = get_earth_radius(time, stVec, re, rp);
		satellite_height = get_satellite_height(time, stVec);
		range = get_slant_range(meta, earth_radius, satellite_height, kk);
	        look_angle = get_look_angle(earth_radius, satellite_height, range);
		incidence_angle = get_incidence_angle(earth_radius, satellite_height, range);
		if (latFlag || lonFlag) {
		  if (!deskewed) doppler = meta_get_dop(meta, ii+ll, kk);
		  fixed2gei(&stVec, 0.0); /* subtracting the Earth's spin */
		  getLatLongMeta(stVec, meta, range, doppler, 0, &latitude, &longitude, &ignored);
		}
	        if (lookFlag) bufLook[kk+ll*ns] = (float) look_angle*R2D;
		if (incidFlag) bufIncid[kk+ll*ns] = (float) incidence_angle*R2D;
	        if (latFlag) bufLat[kk+ll*ns] = (float) latitude;
	        if (lonFlag) bufLon[kk+ll*ns] = (float) longitude;
	        if (rangeFlag) bufRange[kk+ll*ns] = (float) range;
	      }

	    if (lookFlag) FWRITE(bufLook, sizeof(float), ns*size, fpLook);
	    if (incidFlag) FWRITE(bufIncid, sizeof(float), ns*size, fpIncid);
	    if (latFlag) FWRITE(bufLat, sizeof(float), ns*size, fpLat);
	    if (lonFlag) FWRITE(bufLon, sizeof(float), ns*size, fpLon);
	    if (rangeFlag) FWRITE(bufRange, sizeof(float), ns*size, fpRange);

            printf("   Completed %3.0f percent\n", (float)((ii+ll)*100/nl));
	  }

	/* Clear up and exit */
        FCLOSE(fpIn);
	FREE(inBuf);
        if (lookFlag) {
	  FCLOSE(fpLook);
	  FREE(bufLook);
	}
        if (incidFlag) {
	  FCLOSE(fpIncid);
	  FREE(bufIncid);
	}
        if (latFlag) {
	  FCLOSE(fpLat);
	  FREE(bufLat);
	}
        if (lonFlag) {
	  FCLOSE(fpLon);
	  FREE(bufLon);
	}
        if (rangeFlag) {
	  FCLOSE(fpRange);
	  FREE(bufRange);
	}

	StopWatch();
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
	  StopWatchLog(fLog);
	  FCLOSE(fLog);
	}

        system("date");
	exit(0);
}


/* usage - enter here on command-line usage error*/
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [ -look ] [ -incidence ] [ -latitude ] [ -longitude ] [ -range ]\n"
	"             <infile>\n", name);
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -look	Stores look angles for each pixel as image.\n" 
	"   -incidence	Stores incidence angles for each pixel as image.\n"
	"   -latitude	Stores latitude information for each pixel as image.\n"
	"   -longitude	Stores longitude information for each pixel as image.\n"
	"   -range	Stores the slant range for each pixel as image.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   %s creates image layers containing information about data attributes.\n",name);
 printf("\n"
	"Version %.2f, ASF SAR TOOLS\n\n", VERSION);
 exit(1);
}

