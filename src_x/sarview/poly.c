/*SARview poly.c:
The polygon routines.

A polygon is just a list of points which are connected by lines.
We don't assume the polygons are convex.

We rasterize the polygon (convert it to a drawable format) by finding 
the intersections of the lines which make it up with the raster scanlines.
This list of intersections is called a hitList.

Orion Sky Lawlor, 5/16/99
*/
#include "main.h"

polygon *newPoly(void)/*Read polygon's points from TCL link_poly_x & y lists*/
{
	polygon *p;
	int pointNo;
	int nPts;

	/*Figure out how many points there are (by taking the length of the list*/
	Tcl_GlobalEval(interp,"llength $link_poly_x");
	Tcl_GetIntFromObj(interp,Tcl_GetObjResult(interp),&nPts);
	if (nPts<3) 
		return NULL;/*Polygon should have at least three points*/

	/*Allocate polygon structure*/
	p=(polygon *)MALLOC(sizeof(polygon));
	p->nPts=nPts;
	p->pts=(point *)MALLOC(sizeof(point)*nPts);
	p->height=-1;
	p->lists=NULL;

	/*Extract each point of the polygon from TCL's lists*/
	for (pointNo=0;pointNo<nPts;pointNo++)
	{
		char command[500];
		
		sprintf(command,"lindex $link_poly_x %d",pointNo);
		Tcl_GlobalEval(interp,command);
		Tcl_GetDoubleFromObj(interp,Tcl_GetObjResult(interp),&(p->pts[pointNo].x));
	
		sprintf(command,"lindex $link_poly_y %d",pointNo);
		Tcl_GlobalEval(interp,command);
		Tcl_GetDoubleFromObj(interp,Tcl_GetObjResult(interp),&(p->pts[pointNo].y));
	}

	return p;
}

/*Return a bounding box for the given polygon, by
returning a bounding box for the polygon's points*/
void polyBounds(polygon *p,int *minX,int *maxX,int *minY,int *maxY)
{
	int curPt;
	double lX,hX,lY,hY;/*Store lowest and highest X and Y so for*/
	lX=lY=200000000000000.0;
	hX=hY=-200000000000000.0;
	for (curPt=0;curPt<p->nPts;curPt++)
	{
		if (lX>p->pts[curPt].x) lX=p->pts[curPt].x;
		if (hX<p->pts[curPt].x) hX=p->pts[curPt].x;
		if (lY>p->pts[curPt].y) lY=p->pts[curPt].y;
		if (hY<p->pts[curPt].y) hY=p->pts[curPt].y;
	}
	/*Convert double bounds to integer bounds*/
	*minX=(int)floor(lX+0.5);
	*maxX=(int)floor(hX+0.5);
	*minY=(int)floor(lY+0.5);
	*maxY=(int)floor(hY+0.5);
}

void rasterizePoly(polygon *p,int height)/*Find 
	intersections of polygon with all scanlines*/
{
	int lineNo;
	int y,min,max;
	hitList *h;
	p->height=height;
	h=p->lists=(hitList *)MALLOC(sizeof(hitList)*p->height);
/*Zero out hit lists*/
	for (y=0;y<p->height;y++)
		h[y].nHits=0;

/*Rasterize polygon's lines into hit lists*/
	for (lineNo=0;lineNo<p->nPts;lineNo++)
	{
		point cur=p->pts[lineNo],next=p->pts[(lineNo+1)%p->nPts];
	/*We now rasterize the line from cur to next.  The 
	basic form will be   x=slope*y+offset  */
		double denom=(cur.y-next.y);
		if (denom!=0) /*Ignore perfectly horizontal lines (or points)*/
		{
			double slope=(cur.x-next.x)/denom;
			double offset=cur.x-slope*cur.y+0.5;
			double dMin,dMax;
			if (cur.y<next.y) dMin=cur.y,dMax=next.y;
				else dMax=cur.y,dMin=next.y;
			min=(int)(dMin+0.5);
			max=(int)(dMax+0.5);
			if (min<0) continue;
			if (max>=p->height) continue;
			for (y=min;y<max;y++)
				if (h[y].nHits<max_hits)/*Ignore intersections which would overflow the list*/
					h[y].hits[h[y].nHits++]=(int)(slope*y+offset);
		}
	}
}

int scanPoly(polygon *p,int y,int width,int spanNo,int *min,int *max)/*Find
	intersections of polygon with single scanline*/
{
#define swap(a,b) {int tmp=a;a=b;b=tmp;}
	hitList *h=&(p->lists[y]);
	switch (h->nHits)
	{
	case 0:
		return 0;/*Empty scanline*/
	case 2:/*Just two hits (most common)*/
		if (h->hits[0]<h->hits[1])
			*min=h->hits[0],*max=h->hits[1];
		else /* [0]>=[1]*/
			*max=h->hits[0],*min=h->hits[1];
		break;
	case 4:
	case 6:
	case 8:
	case 10:
	case 12:
	case 14:
	case 16:
	case 18:
	case 20:/*Some (even) number of hits.  Return these spans one at a time*/
		if (spanNo==0)
		{/*When asked for the first span, sort the hits
		   into ascending order*/
			int sorted,search;
			for (sorted=0;sorted<h->nHits;sorted++)
				for (search=sorted+1;search<h->nHits;search++)
					if (h->hits[sorted]>h->hits[search])
						swap(h->hits[sorted],h->hits[search]);
		}
		/*Now just return the correct hits for this span*/
		*min=h->hits[spanNo*2];
		*max=h->hits[spanNo*2+1];
		break;
	default:
		/*Odd # of intersections with this scanline--Very bad thing...
		The polygon must not be closed, or something...*/
		abort();
		break;
	}
	if (*min<0) *min=0;
	if (*max>=width) *max=width-1;
	return (spanNo<(h->nHits/2));
}

void deletePoly(polygon *p)
{
	FREE(p->pts);p->pts=NULL;
	FREE(p->lists);p->lists=NULL;
	FREE(p);
}
