/*cproc.c:
	Implements Tcl/Tk-callable routines in C.
These are called by the "routing" routines in tkCommand.c.
*/
#include "main.h"
#include "image.h"

int cproc_loadimage(char * imageName)/*  TCL asks C to load up the given
   image.  Returns 0 on failure; 1 on success, stashing the image
   name and any relevant data in C global storage.
   Sets the link_imagewidth and link_imageheight variables.*/
{
/*Blow away the old image*/
	image_delete();

/*Determine the type of the image, and read it in*/
	if ( extExists(imageName,".ddr") ) {
	  /* Is a LAS image.  */
	  if ( 0 == image_loadLas(imageName) ) return 0;
	} else if ( extExists(imageName, ".meta") 
		    && meta_is_new_style(imageName) ) {
	  if ( 0 == image_loadNewMeta(imageName) ) return 0;
	} else {
	  /* Is a CEOS image.  */
	  if (0==image_loadCeos(imageName)) return 0;
	}

/*Set the zoom factor based on the image and screen sizes--
	don't let either image dimention grow larger than its
	screen dimention (at first).*/
	link_zoom=1.0;
	while ((image.width/link_zoom>0.9*link_screenx)||
		(image.height/link_zoom>0.9*link_screeny))
			link_zoom*=(1/sqrt(0.5));/*Too big-- Zoom out*/

/*Find the image slope and offset*/
	image_slopeOffset();
	
	return 1;
}

/*Draw a single arrow from 0,0 to dirX,dirY; and print "Label"*/
void drawArrow(char *canvas,char *label,double dirX,double dirY)
{
	char buf[1024];
	double mag=sqrt(dirX*dirX+dirY*dirY);
	double arrowMag=30.0,textMag=42.0;
	dirX*=arrowMag/mag;
	dirY*=arrowMag/mag;
	sprintf(buf,"%s create line 0 0 %f %f -tag arrows -arrow last",canvas,dirX,dirY);
	Tcl_GlobalEval(interp,buf);
	dirX*=textMag/arrowMag;
	dirY*=textMag/arrowMag;
	sprintf(buf,"%s create text %f %f -tag arrows -anchor center -text %s",canvas,dirX,dirY,label);
	Tcl_GlobalEval(interp,buf);
}
/* TCL asks C to 
	draw arrows pointing north and east from the image point (x,y) in the image
	into the given canvas.*/
void cproc_ne_arrows(char *canvas,double x,double y)
{
	char buf[1024];
	sprintf(buf,"catch \"%s delete arrows\"",canvas);Tcl_GlobalEval(interp,buf);

	if (meta!=NULL)
	{/*We have metadata for this image-- figure out which way north is.*/
		int ix=(int)x,iy=(int)y;
		double lat,lon,time,slant,doppler;
		double destX,destY;
		meta_get_orig((void *)ddr,iy,ix,&iy,&ix);
		meta_get_latLon(meta,iy,ix,0.0,&lat,&lon);
		meta_get_timeSlantDop(meta,iy,ix,&time,&slant,&doppler);

		meta_get_lineSamp(meta,lat+0.01,lon,0.0,&destY,&destX);
		drawArrow(canvas,"N",destX-ix,destY-iy);

		meta_get_lineSamp(meta,lat,lon+0.01,0.0,&destY,&destX);
		drawArrow(canvas,"E",destX-ix,destY-iy);

		meta_timeSlantDop2latLon(meta,time,slant+5000.0,doppler,0.0,&lat,&lon);
		meta_get_lineSamp(meta,lat,lon,0.0,&destY,&destX);
		drawArrow(canvas,"Illum",destX-ix,destY-iy);

		meta_timeSlantDop2latLon(meta,time+500*meta->geo->azPixTime,slant,doppler,0.0,&lat,&lon);
		meta_get_lineSamp(meta,lat,lon,0.0,&destY,&destX);
		drawArrow(canvas,"Flight",destX-ix,destY-iy);
	}
}

void cproc_log(const char *fName,const char *writeThis)/*Log writeThis to fName*/
{
	FILE *f=fopen(fName,"a");
	fprintf(f,"%s\n",writeThis);
	fclose(f);
}

/*Allocate a private array of "canvasTypes", which
describe the coordinate transformation for a given canvas.*/
typedef struct {
	double startx,starty;/*Starting location of the canvas' image on the original image*/
	double zoom;/*Amount by which the image on this canvas is zoomed out*/
/*image->canvas:(val-start)/zoom*/
/*canvas->image:val*zoom+start*/
} canvasType;

static canvasType *c[100]={NULL,NULL,NULL/*,...*/};


/*Canvas coordinate system routines*/
void cproc_initCanvas(int canvNo,double startx,double starty,double zoom)/*Save given
	coordinate system under canvasNo*/
{
	if (c[canvNo]==NULL)
		c[canvNo]=(canvasType *)malloc(sizeof(canvasType));
	c[canvNo]->startx=startx;
	c[canvNo]->starty=starty;
	c[canvNo]->zoom=zoom;
}

int cproc_fromCanvas(int canvNo,double *x,double *y)/*Map given point from canvas
	coordinates to image coordinates*/
{
	if (c[canvNo]==NULL) return 0;
	*x=(*x)*c[canvNo]->zoom+c[canvNo]->startx+0.00001;
	*y=(*y)*c[canvNo]->zoom+c[canvNo]->starty+0.00001;
	return 1;
}

int cproc_toCanvas(int canvNo,double *x,double *y)/*Map given point from image
	coordinates to canvas coordinates*/
{
	if (c[canvNo]==NULL) return 0;
	*x=((*x)-c[canvNo]->startx)/c[canvNo]->zoom;
	*y=((*y)-c[canvNo]->starty)/c[canvNo]->zoom;
	return 1;
}
