/********************************************************************
Routines used by projectGeo:
Output window-related routines.  The output window eventually
ends up in projection coordinates, and describes exactly the
projection bounds of the output image.

O. Lawlor, 3/99  Initial Creation
*********************************************************************/
#include "projectGeo.h"


window *getUserWindow(int pointOfInterest,char *win)/*Read N W S E window*/
{
	char s[5];
	window *w=(window *)MALLOC(sizeof(window));
	double tmp;
	if (5!=sscanf(win,"%s %lg %lg %lg %lg",s,&w->maxY,&w->minX,&w->minY,&w->maxX))
	{/*Something went wrong*/
		sprintf(errbuf, "   ERROR: Wrong window values\n"
				"   %s %lg %lg %lg %lg.\n",s,
				w->maxY, w->minX, w->minY, w->maxX);
		printErr(errbuf);
	}
	/*Otherwise, we read the window.  Check & return it*/
	if (pointOfInterest)
		return w;/*Don't swap values for points of interest*/

	if (w->maxX<w->minX)
	{/*The X values the user specified are in the wrong order--swap*/
		printf("   Swapping out-of-order horizontal output coordinates\n");
		tmp=w->minX;
		w->minX=w->maxX;
		w->maxX=tmp;
	}
	if (w->maxY<w->minY)
	{/*The Y values the user specified are in the wrong order--swap*/
		printf("   Swapping out-of-order vertical output coordinates\n");
		tmp=w->minY;
		w->minY=w->maxY;
		w->maxY=tmp;
	}
	return w;
}

/*Map-project the given output window, taking min/max X->lon; min/max Y->lat*/
void projectWindow(window *w,forward_transform f)
{
	double lat1=w->maxY*D2R,lon1=w->minX*D2R;
	double lat2=w->minY*D2R,lon2=w->maxX*D2R;
	f(lon1,lat1,&w->minX,&w->maxY);
	f(lon2,lat2,&w->maxX,&w->minY);
}
/*Map-project the given output window, as a point of interest:
maxY->lat; minY->lon; maxX->nl; minX->ns*/
void projectPoint(window *w,double pixSize,forward_transform f)
{
	/*Decide how large the output image will be, in projection coords.*/
	int nl=(int)w->maxX,ns=(int)w->minX;
	double spanX=ns*pixSize/2.0,spanY=nl*pixSize/2.0;
	
	/*Map-project user-specified lat & lon*/
	double centerX,centerY;
	double lat=w->maxY*D2R,lon=w->minY*D2R;
	f(lon,lat,&centerX,&centerY);
	
	/*Round user point of interest to even multiple of pixSize*/
	centerX=pixSize*floor(centerX/pixSize);
	centerY=pixSize*floor(centerY/pixSize);
	
	/*Set output window*/
	w->maxY=centerY+spanY;w->minY=centerY-spanY;
	w->maxX=centerX+spanX;w->minX=centerX-spanX;
}

void window_expand(double pixSize,window *w)
{
	w->maxX=pixSize*ceil(w->maxX/pixSize);
	w->maxY=pixSize*ceil(w->maxY/pixSize);
	w->minX=pixSize*floor(w->minX/pixSize);
	w->minY=pixSize*floor(w->minY/pixSize);
}




