/*cproc_desc.c:
	Implements Tcl/Tk-callable routines in C which deal
with describing images or points (see also image_desc.c).
These are called by the "routing" routines in tkCommand.c.
*/
#include "main.h"
#include "image.h"
#include "proj.h"

/**********************************************
 * cproc_polyinfo:
 * TCL asks C to return a string describing the
 * currently selected region.*/
void cproc_polyinfo(char *dest)
{
	int totalPixels=0,y,i;
	double totalLength;
	polygon *p=newPoly();
	if (p==NULL) /*Bail if there is no region*/
		{sprintf(dest,"No region is currently selected.");return;}

/*Sum up the number of pixels in the region*/
	rasterizePoly(p,image.height);
	for (y=0;y<image.height;y++)
	{
		int scanNo=0;/*The number of this scan chunk*/
		int min,max;
		while (scanPoly(p,y,image.width,scanNo++,&min,&max))
			totalPixels+=max-min;
	}

/*Print the area of the selected region*/
	sprintf(dest,"The currently selected region contains\n"
		"%d pixels",totalPixels);
	if (meta!=NULL)
		sprintf(&dest[strlen(dest)],", which is an area of %.2f square km.\n\n",
			totalPixels*meta->general->x_pixel_size*meta->general->y_pixel_size/1000000.0);
	else
		sprintf(&dest[strlen(dest)],"\n\n");

/*Find the length of the polygon, in pixels and meters*/
	totalLength=0.0;
	for (i=0;i<p->nPts-1;i++)
	{
		int n=(i+1)%p->nPts;/*Index of "next" point in polygon*/
		double dx=p->pts[n].x-p->pts[i].x;
		double dy=p->pts[n].y-p->pts[i].y;
		totalLength+=sqrt(dx*dx+dy*dy);/*Sum length in image (in pixels)*/
	}
	sprintf(&dest[strlen(dest)],"Area's edge (not counting dark green closing line)\n"
		"is %.3f pixels long\n",totalLength);
	
/*Print the points of the polygon*/
	sprintf(&dest[strlen(dest)],"Polygon points: \n     <x>        <y>\n");
	for (i=0;i<p->nPts;i++)
		sprintf(&dest[strlen(dest)],"%14.3f %14.3f\n",p->pts[i].x,p->pts[i].y);

	deletePoly(p);
	
}

/*************************************************************
 * cproc_pointloc:
 * TCL asks C to compute the latitude and longitude of (x,y)*/
int cproc_pointloc(double x,double y, double *lat,double *lon)
{
	if (meta==NULL /*
	    || meta->state_vectors==NULL
	    || meta->state_vectors->vector_count<3
	    || !(meta->sar->image_type=='S' || meta->sar->image_type=='G' || meta->sar->image_type=='P') */ )
		return 0;
	meta_get_latLon(meta,y,x,0.0,lat,lon);
	return 1;
}


/*******************************************************
 * print_dms:
 * Print a textual description fo the current point,
 * in Degrees-Minutes-Seconds units.  Uses PCI's format,
 * as in 157d29'14.17" W */
char *print_dms(int isLat,double val,char *buf)
{
	int deg,min,sec,hsec;/*Degrees, minutes, seconds, and hundredths*/
	char code;/*NSEW code*/
	if (val<0)
	{/*Negative value-- south or west*/
		code=isLat?'S':'W';
		val*=-1;
	} else /*Positive value-- north or east*/
		code=isLat?'N':'E';
	
	deg=(int)floor(val);
	min=(int)floor((val-deg)*60);
	sec=(int)floor(((val-deg)*60-min)*60);
	hsec=(int)floor((((val-deg)*60-min)*60-sec)*100);

	sprintf(buf,"%dd%02d'%02d.%02d\" %c",deg,min,sec,hsec,code);
		
	return buf;
}

/*****************************************************************
 * image_describePixel:
 * Write a description of the current pixel to the given string */
void image_describePixel(char *dest,double x,double y)
{
	int ix=(int)floor(x+0.5),iy=(int)floor(y+0.5);
	if ((ix<0)||(ix>=image.width)||
		(iy<0)||(iy>=image.height))
		sprintf(dest,"Line: %.3f  Sample %.3f is out of the image.\n",y,x);
	else 
	{
		int color;
		double lat,lon;

                
		sprintf(dest,"Line: %.3f   Sample: %.3f \n",y,x);
		/*Print pixel's value (for each band)*/
		for (color=0;color<ddr->nbands;color++)
		{
			float *inBuf=(float *)MALLOC(sizeof(float)*image.width);
			image_readLine(inBuf,iy,color);
			sprintf(&dest[strlen(dest)],
				"Pixel value: %f  Scaled to: %d (band %d)\n",
				inBuf[ix],scale_toByte(inBuf[ix]),color);
			FREE(inBuf);
		}
		/*Print projection information*/
		if (image_projOK())
		{
			double projX,projY,lat,lon;
			int iflg;
			inverse_transform proj2LatLon[100];
			
			inv_init(ddr->proj_code,ddr->zone_code,ddr->proj_coef,ddr->datum_code,NULL,NULL,&iflg,proj2LatLon);
			image_point2proj(x,y,&projX,&projY);
			proj2LatLon[ddr->proj_code](projX,projY,&lon,&lat);			

			sprintf(&dest[strlen(dest)],"ProjX: %.1f m,ProjY: %.1f m\n",projX,projY);
			if(iflg==0 && meta==NULL)
				sprintf(&dest[strlen(dest)],"Latitude: %.3f Longitude: %.3f (decimal)\n",lat*R2D,lon*R2D);
		}
		/*Print pixel location*/
		if (cproc_pointloc(x,y,&lat,&lon))
		{
			char latBuf[100],lonBuf[100];
			sprintf(&dest[strlen(dest)],"Lat: %f  Lon: %f (decimal)\n",lat,lon);
			sprintf(&dest[strlen(dest)],"Lat: %s  Lon: %s (DMS)\n",
				print_dms(1,lat,latBuf),print_dms(0,lon,lonBuf));
		}
                /* Incidence angle */
                if (meta && meta->state_vectors) {
                    sprintf(&dest[strlen(dest)],
                            "Incid: %f (deg), Look: %f (deg)\n",
                            R2D*meta_incid(meta, y, x), 
                            R2D*meta_look(meta, y, x));
                }
	}
}
