/**************************************************************************
 
NAME:

SYNOPSIS:

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     9/15     T. Logan     Create geocoding mapping for s1a
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <asf_meta.h>
#include <asf_license.h>


void create_mapping(double xs, double ys, double valstart,  double *xin, double *yin, double *vals, int cnt, double *coefs);


int main(int argc, char *argv[])
{
  FILE *fpin, *fpout;
  char line[256];
  int  i;
  double *lats, *lons;
  double *lines, *samps;
  int cnt;

  if (argc != 2) {
    printf("Usage: %s <inxmlfile>\n",argv[0]);
    exit(1);
  }

  fpin = FOPEN(argv[1],"r");
  if (fpin==NULL) { printf ("ERROR: Unable to open file %s\n",argv[1]); exit(1);}

  while (fgets(line,sizeof(line),fpin)) {
     if (strstr(line,"geolocationGridPointList count")) {
        char *s=strstr(line,"\"");
        if (s==NULL) {printf("Can't find \" in line!!!\n"); exit(1);}
   	s++;
        cnt = atoi(s);
        printf("Found geolocationGridPointList with %i points\n",cnt);

 	lats = (double *) malloc (sizeof(double)*cnt);
 	lons = (double *) malloc (sizeof(double)*cnt);
 	lines= (double *) malloc (sizeof(double)*cnt);
 	samps= (double *) malloc (sizeof(double)*cnt);

        for (i=0; i<cnt; i++) {
 	   fgets(line,sizeof(line),fpin);  /* geolocationGridPoint Start */
	   fgets(line,sizeof(line),fpin);  /* azimuthTime          */
           fgets(line,sizeof(line),fpin);  /* slantRangeTime       */
           fgets(line,sizeof(line),fpin);  /* line                 */
           s = strstr(line,">"); s++; lines[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* pixel                */
           s = strstr(line,">"); s++; samps[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* latitude             */
           s = strstr(line,">"); s++; lats[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* longitude            */
           s = strstr(line,">"); s++; lons[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* height               */
           fgets(line,sizeof(line),fpin);  /* incidenceAngle       */
           fgets(line,sizeof(line),fpin);  /* elevationAngle       */
           fgets(line,sizeof(line),fpin);  /* geolocationGridPoint End */
	   printf("Found (%f,%f) -> (%f,%f)\n",lines[i],samps[i],lats[i],lons[i]);
        }
     }
  }

  double ll2samp[25];
  double ll2line[25];
  double ls2lat[25];
  double ls2lon[25];

  double max_lat = -200;
  double max_lon = -200;
  int max_samp = 0;
  int max_line = 0;
  double min_lat = 200;
  double min_lon = 200;
  int min_samp = 999999;
  int min_line = 999999;
  double mid_lat, mid_lon, mid_samp, mid_line; 

  for (i=0; i<cnt; i++)
   {
     max_lat = (max_lat>lats[i]) ? max_lat : lats[i];
     min_lat = (min_lat<lats[i]) ? min_lat : lats[i];

     max_lon = (max_lon>lons[i]) ? max_lon : lons[i];
     min_lon = (min_lon<lons[i]) ? min_lon : lons[i];

     max_samp = (max_samp>samps[i]) ? max_samp : samps[i];
     min_samp = (min_samp<samps[i]) ? min_samp : samps[i];

     max_line = (max_line>lines[i]) ? max_line : lines[i];
     min_line = (min_line<lines[i]) ? min_line : lines[i];
   }

  printf("Found line min/max %i %i\n",min_line,max_line);
  printf("Found samp min/max %i %i\n",min_samp,max_samp);
  printf("Found lat min/max %f %f\n",min_lat,max_lat);
  printf("Found lon min/max %f %f\n",min_lon,max_lon);

  mid_lat = (min_lat+max_lat)/2;
  mid_lon = (min_lon+max_lon)/2;
  mid_samp = (min_samp+max_samp)/2+1;
  mid_line = (min_line+max_line)/2+1;

  printf("Found middle line %f\n",mid_line);
  printf("Found middle samp %f\n",mid_samp);
  printf("Found middle lat %f\n",mid_lat);
  printf("Found middle lon %f\n",mid_lon);

  create_mapping2(mid_lat,mid_lon,mid_samp,lats,lons,samps,cnt,ll2samp,0);
  create_mapping2(mid_lat,mid_lon,mid_line,lats,lons,lines,cnt,ll2line,0);
  create_mapping2(mid_line,mid_samp,mid_lat,lines,samps,lats,cnt,ls2lat,1);
  create_mapping2(mid_line,mid_samp,mid_lon,lines,samps,lons,cnt,ls2lon,1);
  
  exit(0);
}	

