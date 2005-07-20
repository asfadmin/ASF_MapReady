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
#include "least_squares.h"
#include "matrix.h"

#define VERSION 1.0
#define BUFSIZE 1024
#ifndef PI
# define PI 3.14159265358979323846
#endif
#define RES_X 16
#define RES_Y 16
#define MAX_PTS 300

#define SQR(X) ((X)*(X))

double get_satellite_height(double time, stateVector stVec)
{
  return sqrt(stVec.pos.x*stVec.pos.x+stVec.pos.y*stVec.pos.y+stVec.pos.z*stVec.pos.z);
}

double get_earth_radius(double time, stateVector stVec, double re, double rp)
{
  double er = 
    sqrt(stVec.pos.x*stVec.pos.x+stVec.pos.y*stVec.pos.y+stVec.pos.z*stVec.pos.z);
  double lat = asin(stVec.pos.z/er);
  return (re*rp)/sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
}

double get_slant_range(meta_parameters *meta, double er, double ht, int sample)
{
  double minPhi=acos((ht*ht + er*er -
		      SQR(meta->sar->slant_range_first_pixel))/(2.0*ht*er));
  double phi=minPhi+sample*(meta->general->x_pixel_size/er);
  double slantRng=sqrt(ht*ht+er*er-2.0*ht*er*cos(phi));
  return slantRng+meta->sar->slant_shift;
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
  FILE *fpIn=NULL, *fpLook=NULL, *fpIncid=NULL, *fpLat=NULL, *fpLon=NULL;
  FILE *fpRange=NULL, *fpOut=NULL;
  meta_parameters *meta, *metaOut;
  stateVector stVec;
  quadratic_2d q;
  int lookFlag=0, incidFlag=0, latFlag=0, lonFlag=0, rangeFlag=0, nl, ns, ii, kk, ll;
  int size, nPoints, x, y;
  char *inFile=NULL, metaFile[255], outLook[255], outIncid[255], outLat[255];
  char outLon[255], outRange[255], inLine[255]; 
  float *outBuf=NULL, *bufLook=NULL, *bufIncid=NULL, *bufLat=NULL, *bufLon=NULL;
  float *bufRange=NULL;
  double latitude, longitude, time, doppler, earth_radius, satellite_height, range;
  double look_angle, incidence_angle;
  double ignored, re=6378144.0, rp=6356754.9, px, py;
  double *l, *s, *value, line, sample, firstLook=0.0, firstIncid=0.0, firstLat=0.0;
  double firstLon=0.0, firstRange=0.0;
  
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
 
  printf("Date: %s\n",date_time_stamp());
  fflush (NULL);
  printf("Program: image_layers\n\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: image_layers\n\n");
  }
  
  /* Create metadata */
  meta = meta_read(inFile);
  nl = meta->general->line_count;
  ns = meta->general->sample_count;
  metaOut = meta_read(inFile);
  metaOut->general->data_type = REAL32;
  
  /* Creating tie point files */
  doppler = 0.0;
  
  if (meta->sar->image_type=='P') {
    /* Calculation in case the imagery is map projected */
    size = BUFSIZE;
    re = meta->projection->re_major;
    rp = meta->projection->re_minor;
    
    if (lookFlag) {
      fpLook = fopenImage(outLook,"wb");
      bufLook = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);        
      meta_write(metaOut, outLook);
    }
    if (incidFlag) {
      fpIncid = fopenImage(outIncid,"wb");
      bufIncid = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      meta_write(metaOut, outIncid);
    }        
    if (latFlag) {
      fpLat = fopenImage(outLat,"wb");
      bufLat = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      meta_write(metaOut, outLat);
    }        
    if (lonFlag) {
      fpLon = fopenImage(outLon,"wb");
      bufLon = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      meta_write(metaOut, outLon);
    }        
    if (rangeFlag) {
      fpRange = fopenImage(outRange,"wb");
      bufRange = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);        
      meta_write(metaOut, outRange);
    }
    
    for (ii=0; ii<nl; ii+=size) {
      if ((nl-ii)<BUFSIZE) size = nl-ii;
      for (ll=0; ll<size; ll++) 
	for (kk=0; kk<ns; kk++) {
	  px = meta->projection->startX + meta->projection->perX * kk;
	  py = meta->projection->startY + meta->projection->perY * (ii+ll);
	  proj_to_ll(meta->projection, meta->sar->look_direction, px, py,
		     &latitude, &longitude);
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
      
      if (lookFlag)
	put_float_lines(fpLook, meta, ii, size, bufLook); 
      if (incidFlag) 
	put_float_lines(fpIncid, meta, ii, size, bufIncid); 
      if (latFlag) 
	put_float_lines(fpLat, meta, ii, size, bufLat); 
      if (lonFlag) 
	put_float_lines(fpLon, meta, ii, size, bufLon); 
      if (rangeFlag) 
	put_float_lines(fpRange, meta, ii, size, bufRange); 
      
      printf("   Completed %3.0f percent\n", (float)((ii+ll)*100/nl));
    }
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
  }
  
  else {
    /* Calculation in case the imagery is in slant range or ground range */
    if (lookFlag) fpLook = FOPEN(outLook, "w");
    if (incidFlag) fpIncid = FOPEN(outIncid, "w");
    if (latFlag) fpLat = FOPEN(outLat, "w");
    if (lonFlag) fpLon = FOPEN(outLon, "w");
    if (rangeFlag) fpRange = FOPEN(outRange, "w");
    
    for (ll=0; ll<=RES_X; ll++)
      for (kk=0; kk<=RES_Y; kk++) {
	line = ll * nl / RES_Y;
	sample = kk * ns / RES_X;
	time = meta_get_time(meta, line, sample);
	stVec = meta_get_stVec(meta, time);
	earth_radius = get_earth_radius(time, stVec, re, rp);
	satellite_height = get_satellite_height(time, stVec);
	range = get_slant_range(meta, earth_radius, satellite_height, sample);
	look_angle = get_look_angle(earth_radius, satellite_height, range);
	incidence_angle = get_incidence_angle(earth_radius, satellite_height, range);
	if (latFlag || lonFlag) {
	  if (!meta->sar->deskewed)
	    doppler = meta_get_dop(meta, line, sample);
	  fixed2gei(&stVec, 0.0); /* subtracting the Earth's spin */
	  getLatLongMeta(stVec, meta, range, doppler, 0, &latitude, &longitude, 
			 &ignored);
	}
	
	if (ll==0 && kk==0) {
	  firstLook = look_angle * R2D;
	  firstIncid = incidence_angle * R2D;
	  firstLat = latitude;
	  firstLon = longitude;
	  firstRange = range;
	}
	
	if (lookFlag) 
	  fprintf(fpLook, "%.18f %.12f %.12f\n", (float)look_angle*R2D, line, sample);
	if (incidFlag) 
	  fprintf(fpIncid, "%.18f %.12f %.12f\n", (float)incidence_angle*R2D, line, 
		  sample);
	if (latFlag) 
	  fprintf(fpLat, "%.18f %.12f %.12f\n", (float)latitude, line, sample);
	if (lonFlag) 
	  fprintf(fpLon, "%.18f %.12f %.12f\n", (float) longitude, line, sample);
	if (rangeFlag) 
	  fprintf(fpRange, "%.18f %.12f %.12f\n", (float)range, line, sample);
      }
    
    /* Close files for now*/
    if (lookFlag) FCLOSE(fpLook);
    if (incidFlag) FCLOSE(fpIncid);
    if (latFlag) FCLOSE(fpLat);
    if (lonFlag) FCLOSE(fpLon);
    if (rangeFlag) FCLOSE(fpRange);
    
    /* set things up for least square calculation */
    value=(double *)MALLOC(sizeof(double)*MAX_PTS);
    l=(double *)MALLOC(sizeof(double)*MAX_PTS);
    s=(double *)MALLOC(sizeof(double)*MAX_PTS);
    
    if (lookFlag) {
      size = BUFSIZE;
      nPoints = 0;
      fpIn = FOPEN(outLook, "r");
      fpOut = fopenImage(outLook, "wb");
      outBuf = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      while (NULL!=(fgets(inLine, 255, fpIn))) {
	sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]); 
	nPoints++;
      }
      q = find_quadratic(value, l, s, nPoints);
      q.A = firstLook;
      for (ii=0; ii<nl; ii+=size) {
	if ((nl-ii)<BUFSIZE) size = nl-ii;
	for (ll=0; ll<size; ll++) 
	  for (kk=0; kk<ns; kk++) {
	    x = ii + ll;
	    y = kk;
	    look_angle = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y + q.G*x*x*y +
	      q.H*x*y*y + q.I*x*x*y*y + q.J*x*x*x + q.K*y*y*y; 
	    outBuf[kk+ll*ns] = (float) look_angle;
	  }
	put_float_lines(fpOut, meta, ii, size, outBuf); 
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(outBuf);
      meta_write(metaOut, outLook);
      printf("   ... created look angle layer\n");
    }
    
    if (incidFlag) {
      size = BUFSIZE;
      nPoints = 0;
      fpIn = FOPEN(outIncid, "r");
      fpOut = fopenImage(outIncid, "wb");
      outBuf = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      while (NULL!=(fgets(inLine, 255, fpIn))) {
	sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]); 
	nPoints++;
      }
      q = find_quadratic(value, l, s, nPoints);
      q.A = firstIncid;
      for (ii=0; ii<nl; ii+=size) {
	if ((nl-ii)<BUFSIZE) size = nl-ii;
	for (ll=0; ll<size; ll++) 
	  for (kk=0; kk<ns; kk++) {
	    x = ii + ll;
	    y = kk;
	    incidence_angle = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y + 
	      q.G*x*x*y + q.H*x*y*y + q.I*x*x*y*y + q.J*x*x*x + q.K*y*y*y; 
	    outBuf[kk+ll*ns] = (float) incidence_angle;
	  }
	put_float_lines(fpOut, meta, ii, size, outBuf); 
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(outBuf);
      meta_write(metaOut, outIncid);
      printf("   ... created incidence angle layer\n");
    }
    
    if (latFlag) {
      size = BUFSIZE;
      nPoints = 0;
      fpIn = FOPEN(outLat, "r");
      fpOut = fopenImage(outLat, "wb");
      outBuf = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      while (NULL!=(fgets(inLine, 255, fpIn))) {
	sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]); 
	nPoints++;
      }
      q = find_quadratic(value, l, s, nPoints);
      q.A = firstLat;
      for (ii=0; ii<nl; ii+=size) {
	if ((nl-ii)<BUFSIZE) size = nl-ii;
	for (ll=0; ll<size; ll++) 
	  for (kk=0; kk<ns; kk++) {
	    x = ii + ll;
	    y = kk;
	    latitude = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y + q.G*x*x*y + 
	      q.H*x*y*y + q.I*x*x*y*y + q.J*x*x*x + q.K*y*y*y; 
	    outBuf[kk+ll*ns] = (float) latitude;
	  }
	put_float_lines(fpOut, meta, ii, size, outBuf); 
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(outBuf);
      meta_write(metaOut, outLat);
      printf("   ... created latitude layer\n");
    }
    
    if (lonFlag) {
      size = BUFSIZE;
      nPoints = 0;
      fpIn = FOPEN(outLon, "r");
      fpOut = fopenImage(outLon, "wb");
      outBuf = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      while (NULL!=(fgets(inLine, 255, fpIn))) {
	sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]); 
	nPoints++;
      }
      q = find_quadratic(value, l, s, nPoints);
      q.A = firstLon;
      for (ii=0; ii<nl; ii+=size) {
	if ((nl-ii)<BUFSIZE) size = nl-ii;
	for (ll=0; ll<size; ll++) 
	  for (kk=0; kk<ns; kk++) {
	    x = ii + ll;
	    y = kk;
	    longitude = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y + q.G*x*x*y + 
	      q.H*x*y*y + q.I*x*x*y*y + q.J*x*x*x + q.K*y*y*y;
	    outBuf[kk+ll*ns] = (float) longitude;
	  }
	put_float_lines(fpOut, meta, ii, size, outBuf); 
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(outBuf);
      meta_write(metaOut, outLon);
      printf("   ... created longitude layer\n");
    }
    
    if (rangeFlag) {
      size = BUFSIZE;
      nPoints = 0;
      fpIn = FOPEN(outRange, "r");
      fpOut = fopenImage(outRange, "wb");
      outBuf = (float *) MALLOC(ns * sizeof(float) * BUFSIZE);
      while (NULL!=(fgets(inLine, 255, fpIn))) {
	sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]); 
	nPoints++;
      }
      q = find_quadratic(value, l, s, nPoints);
      q.A = firstRange;
      for (ii=0; ii<nl; ii+=size) {
	if ((nl-ii)<BUFSIZE) size = nl-ii;
	for (ll=0; ll<size; ll++) 
	  for (kk=0; kk<ns; kk++) {
	    x = ii + ll;
	    y = kk;
	    range = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y + q.G*x*x*y + 
	      q.H*x*y*y + q.I*x*x*y*y + q.J*x*x*x + q.K*y*y*y; 
	    outBuf[kk+ll*ns] = (float) range;
	  }
	put_float_lines(fpOut, meta, ii, size, outBuf); 
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(outBuf);
      meta_write(metaOut, outRange);
      printf("   ... created range layer\n");
    }
  }

  if (logflag) {
    fLog = FOPEN(logFile, "a");
    StopWatchLog(fLog);
    FCLOSE(fLog);
  }
  
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
	 "   %s creates image layers containing information about data attributes.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n", VERSION);
  exit(1);
}

