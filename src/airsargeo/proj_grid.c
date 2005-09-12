/********************************************************************
Routines used by projectGeo:
Routines dealing with the projection grid--
this is a set of points laid over the input image
which are map-projected.  Later, fit_quadratic
fits a quadratic function to the grid points.

O. Lawlor, 3/99  Initial Creation
*********************************************************************/
#include "projectGeo.h"

void read_grid(const char *in_tie,gridPoint g[],int *nGrid)
/*Read lat,lon,inX,inY*/
{
	int lineNo=0,i=0;
	char inBuf[200];
	FILE *in=FOPEN(in_tie,"r");/*Open ASCII tie point file*/
	/*Read each line of file; extract out info we need*/
	while (NULL!=(fgets(inBuf,200,in)))
	{/*We've read a single line*/
		lineNo++;
		if (4!=sscanf(inBuf,"%lf%lf%lf%lf",
			&g[i].lat,&g[i].lon,&g[i].inX,&g[i].inY))
		{/*We had a read error!*/
			sprintf(errbuf, "   ERROR: Couldn't read line %d of ASCI tie point file %s!\n",lineNo,in_tie);
			printErr(errbuf);
		}
		i++;/*Advance down list of grid points*/
	}
	aLat=g[0].lat;
	aLon=g[0].lon;
	*nGrid=i;
	FCLOSE(in);
}

void project_grid(gridPoint g[],int nGrid,proj_prm *proj)
/*Convert lat,lon to projX,projY*/
{
/*Get the map projection transformation function*/
	forward_transform forward=init_proj(proj);
/*Transform each point*/
	int i;
	for (i=0;i<nGrid;i++)
		forward(g[i].lon*D2R,g[i].lat*D2R,&g[i].projX,&g[i].projY);
}

window *window_grid(gridPoint g[],int nGrid)
/*Get max/min projection coordinates*/
{
	int i;
	window *w=(window *)MALLOC(sizeof(window));
	w->minX=w->minY=10000000000000000000.0;
	w->maxX=w->maxY=-10000000000000000000.0;
	for (i=0;i<nGrid;i++)
	{
		double x=g[i].projX,y=g[i].projY;
		if (w->minX>x) w->minX=x;
		if (w->minY>y) w->minY=y;
		if (w->maxX<x) w->maxX=x;
		if (w->maxY<y) w->maxY=y;
	}
	return w;
}

void convert_grid(double pixSize,const window *w,gridPoint g[],int nGrid)
/*convert projX,projY to outX,outY*/
{
	int i;
	for (i=0;i<nGrid;i++)
	{
		g[i].outX=(g[i].projX - w->minX)/pixSize;
		g[i].outY=(w->maxY - g[i].projY)/pixSize;
	}
}

void write_grid(const char *out_tie,gridPoint g[],int nGrid)
/*Write outX,outY,inX,inY*/
{
	int i=0;
	FILE *out=FOPEN(out_tie,"w");/*Open ASCII tie point file*/
	/*Write each grid point to one line of file*/
	for (i=0;i<nGrid;i++)
		fprintf(out,"%.4f %.4f %.4f %.4f\n",
			g[i].outX,g[i].outY,g[i].inX,g[i].inY);
	FCLOSE(out);
}




