/******************************************************************************
NAME:	psconv - Lon, Lat to Polar Stereographic Coordinate X-forms

SYNOPSIS:	psconv  (interactive program)

DESCRIPTION:    Interactively reads lon, lat coordinates and displays
		the equivalent X,Y polar stereographic coordinates.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    psforint(...)	 Initializes the Forward Polar Stereographic Xform
    psfor(lon,lat,&x,&y) Given lon,lat calculates PS x,y coordinates

FILE REFERENCES:	NONE

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    1/97   T. Logan	Respond to user request

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:	Uses GCTP package

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright 
laws of the United States. Permission is hereby granted to 
use, reproduce, and prepare derivative works for noncommercial 
purposes at no charge, provided that this original copyright 
notice, and disclaimer are retained and any changes are 
clearly documented. Any entity desiring permission to 
incorporate this software or a work based on the software 
into a product for sale must contact the University of 
Alaska Technology Corporation.


This software was authored by:

RICK GURITZ      rguritz@images.alaska    (907)474-7886
Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu


NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF, 
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR 
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY 
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ceos.h"


#define D2R     1.745329251994328e-2
void projectionInit(void)
{
   double r_maj, r_min;
   double c_lon, c_lat;
   double false_east, false_north;
   r_maj = 6.37814404299999960000E+06;
   r_min = 6.35675488300000038000E+06;
   c_lon = -45.0 * D2R;
   c_lat = 70.0  * D2R;
   false_east = 0.0;
   false_north = 0.0;
   psforint(r_maj, r_min, c_lon, c_lat, false_east, false_north);

}
FILE *psOut;
void postScriptInit(char *outName)
{
	psOut=fopen(outName,"w");
	if (psOut==NULL)
		{printf("Couldn't Open Postscript output file!\n");psOut=stdout;}
	fprintf(psOut,"	%%!PS-Adobe-2.0 EPSF-2.0\n");
	fprintf(psOut,"/Times-Roman findfont 14 scalefont setfont\n");
}
void postScriptDone(void)
{
	fprintf(psOut,"showpage\n");
	fclose(psOut);
}
int drawing;/*==0: enlarging; ==1: actually drawing.*/
typedef struct {
	double x,y;
} point;
point midPoint(point p1,point p2)
{
	point ret;
	ret.x=(p1.x+p2.x)/2;
	ret.y=(p1.y+p2.y)/2;
	return ret;
}
point min={10000000.0,100000000.0},max={-10000000.0,-10000000.0};
void enlargeBounds(point p)
{
	if (min.x>p.x) min.x=p.x;
	if (min.y>p.y) min.y=p.y;
	if (max.x<p.x) max.x=p.x;
	if (max.y<p.y) max.y=p.y;
}
double scale=1.0;
void computeScale(void)
{
	double largerDim;
	if (max.x-min.x>max.y-min.y)
		largerDim=max.x-min.x;
	else
		largerDim=max.y-min.y;
	scale=500.0/largerDim;
}
float toX(point p)
{
	return 72+(p.x-min.x)*scale;
}
float toY(point p)
{
	return 72+(p.y-min.y)*scale;
}
void postScriptComment(char *str1,char *str2)
{
	if (drawing==1)
		fprintf(psOut,"\n%% %s%s\n",str1,str2);
}
void postScriptLine(point p1,point p2)
{
	if (drawing==0)
		{enlargeBounds(p1);enlargeBounds(p2);}
	else
		fprintf(psOut,"newpath %f %f moveto %f %f lineto stroke\n",toX(p1),toY(p1),
			toX(p2),toY(p2));
}
void postScriptString(char *str)
{
	if (drawing==1)
		fprintf(psOut,"newpath 0 0 moveto (%s) show\n",str);
}
void postScriptF(char *fmt,float number)
{
	char numStr[255];
	sprintf(numStr,fmt,number);
	postScriptString(numStr);
}
#define D2R     1.745329251994328e-2
/*Rotate so p1,p2 lie on the origin and positive x-axis, respectively.*/
point orig;
double angle;
void postScriptRotate(point p1,point p2)
{
	if (drawing==1)
	{
		orig=p1;
		angle=atan2(toY(p2)-toY(p1),toX(p2)-toX(p1))/D2R;
		fprintf(psOut,
			"%f %f translate %f rotate\n",
			toX(p1),toY(p1),
			angle);
	}
}
void postScriptRotateEnd(void)
{
	if (drawing==1)
		fprintf(psOut,
			"%f rotate %f %f translate\n",
			-angle,
			-toX(orig),-toY(orig));
}
void postScriptSetGrey(float grey)/*0-- black; 1-- white.*/
{
	if (drawing==1)
		fprintf(psOut,"%f setgray\n",grey);
}
void printImage(char *imgNameIn)
{
	point originPt={0.0,0.0};
	float doppler;
	char dopStr[255],imgName[255],*namePtr;
	point c[4];
	struct VFDRECV facdr;
	sprintf(imgName,"%s",imgNameIn);
	get_facdr(imgName,&facdr);
	psfor(D2R*facdr.nearslon,D2R*facdr.nearslat,&c[0].x,&c[0].y);
	psfor(D2R*facdr.nearelon,D2R*facdr.nearelat,&c[1].x,&c[1].y);
	psfor(D2R*facdr.farslon,D2R*facdr.farslat,&c[3].x,&c[3].y);
	psfor(D2R*facdr.farelon,D2R*facdr.farelat,&c[2].x,&c[2].y);
	postScriptComment("Image ",imgName);
	postScriptLine(c[0],c[1]);
	postScriptLine(c[1],c[2]);
	postScriptLine(c[2],c[3]);
	postScriptLine(c[3],c[0]);
	postScriptLine(c[0],c[1]);
	postScriptLine(c[0],c[2]);
	postScriptLine(c[1],c[3]);
	postScriptRotate(midPoint(c[0],c[1]),midPoint(c[2],c[3]));
	doppler=facdr.dpplrfrq/facdr.prfreq;
	imgName[10]=0;
	namePtr=&imgName[3];
	sprintf(dopStr,"%s/%4.2f",namePtr,doppler);
	postScriptString(dopStr);
	postScriptRotateEnd();
}
void drawLatLon(void)
{
	point originPt={0.0,0.0};
	point p,old_p;
	int lat,lon;
	postScriptSetGrey(0.7);
	for (lat=50;lat<90;lat+=2)
	{
		lon=0;
		psfor(D2R*lon,D2R*lat,&p.x,&p.y);
		psfor(D2R*(lon+1),D2R*lat,&old_p.x,&old_p.y);
		postScriptRotate(p,old_p);
		postScriptF("%.0f lat,0 lon",(float)lat);
		postScriptRotateEnd();
		for (lon=5;lon<=360;lon+=1)
		{
			old_p=p;
			psfor(D2R*lon,D2R*lat,&p.x,&p.y);
			postScriptLine(p,old_p);
			if (lon!=0&&lon!=360&&lon%45==0)
			{
				postScriptRotate(p,old_p);
				postScriptF("%.0f lon",(float)lon);
				postScriptRotateEnd();
			}
		}	
	}
	postScriptSetGrey(0.0);
}

main(int argc,char *argv[])
{
	int imageNo;
	if (argc<2)
		{printf("Usage dopPlot <files...>\n"); exit(1);}
	projectionInit();
	postScriptInit("output.ps");
	drawing=0;
	for (imageNo=1;imageNo<argc;imageNo++)
		printImage(argv[imageNo]);
	drawing=1;
	computeScale();
	drawLatLon();
	for (imageNo=1;imageNo<argc;imageNo++)
		printImage(argv[imageNo]);
 	postScriptDone();
}
