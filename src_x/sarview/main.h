/*SarView main include file*/
#include "tcl.h"
#include "tk.h"
#include "asf.h"
#include "ceos_io.h"
#include "asf_meta.h"
#include "tk_interface.h"

/*Global Tcl Interpreter*/
extern Tcl_Interp *interp;

/*Figure out the directory separator*/
#ifdef WIN32
# define DIRSEP '\\'
#else
# define DIRSEP '/'
#endif


/*********The Histogram interface (histo.c) ****/
typedef struct {
	int nBins;		/*Number of bins in histogram*/
	int *bins;		/*array of histogram bins*/
	double min,max;		/*Approximate minimum & maximum values*/
	double slope,offset;	/*Scaling to map image values to hist. bins*/
	int nPixels;		/*Total number of pixels in all bins*/
} histogram;

#define val2bin(v) (int)(h->slope*(v)+h->offset)
#define bin2val(b) (((b)-h->offset)/h->slope)

/*Create a histogram structure with the given number of bins*/
histogram *createHist(int nBins,double min,double max);

/*Add the given array of pixels to the histogram*/
void addToHist(histogram *h,float *vals,int nVals);

/*Compute the trimmed max & min values.
minDex & maxDex whould be the image minimum and maximum if we removed 
the trimFraction smallest and trimFraction largest pixels.*/
void trimMaxMin(const histogram *h,double trimFraction,double *Tmin,double *Tmax);

/*Copy the source histogram's bins into the dest histogram's*/
void copyHist(histogram *dest,const histogram *source);

/*Render the given histogram into the given (4-byte) pixel buffer.
This routine draws the logarithm of the number of pixels in the
histogram bin, returning the number of pixels in the largest bin.*/
void renderHist(histogram *src,double min,double max,
				int *lowest,int *highest,double *mean,
				int fg,int bg,
				int *dest,int width,int height);

/*Blow away the given histogram record*/
void deleteHist(histogram *h);


/*********** Polygon Routines***************
(poly.c)
*/

typedef struct {
	double x,y;
} point;

typedef struct {
#define max_hits 20
	int hits[max_hits];
	int nHits;
} hitList;

typedef struct {
	int nPts;/*Number of points*/
	point *pts;/*Points of the polygon*/
	int height;/*Number of hitLists below-- one for each scanline*/
	hitList *lists;/*Intersections of the polygon with scanline*/
} polygon;


polygon *newPoly(void);/*Read polygon from TCL link_poly_x & y lists*/

/*Return a bounding box for the given polygon*/
void polyBounds(polygon *p,int *minX,int *maxX,int *minY,int *maxY);

void rasterizePoly(polygon *p,int height);/*Find 
	intersections of polygon with all scanlines*/

int scanPoly(polygon *p,int yLine,int width,int spanNo,int *min,int *max);/*Find
	intersection spanNo of polygon with single scanline, and return 1 if there are more to come.*/

void deletePoly(polygon *p);

/*********************************************
Selection routines (sel.c)
These return the "current selection", which is composed of the 
full-resolution pixels of the current image included in the current
polygon, or all the image pixels if no polygon is selected.
*/
typedef struct {
	polygon *p;/*Currently selected polygon, or NULL*/
	int startY,startX,wid;
	float *inBuf;/*an image.width-wide buffer*/
} selection;

selection * selBounds(int *wid,int *ht);

/*Extract the given (selection-relative) line of full-resolution
selected pixels to the given array, filling in the non-selected parts
with backgroundFill*/
void selLine(selection *s,int colorBand,int yLine,float *dest,float backgroundFill);

void deleteSel(selection *s);
