/***************************************************************
ASCII Graphics Support file:
	Allows the user to plot points to standard output
  using ASCII graphics (i.e. a '.' for a point, spaces for
  blank space, etc.

implements:

	void AscGraf_AddPlotPoint(double x,double y);
		Call this procedure to add a point to the plot list.
		
	void AscGraf_PlotPoints(void);
		Call this procedure to print the current plot list
		to the screen.
**************************************************************/
#include <stdio.h>
#include <stdlib.h>


/*Define the ASCII Graphics port type*/
	typedef struct {
		char *buf;
		long mx,my;
	} ascGrafPort, *ascGrafPtr;
	typedef struct {
		long ystart;
		long xstart;
		long yend;
		long xend;
	} ascGrafRect;
ascGrafPtr 	ag_newPort(long numRows,long numCols);/*allocates and returns a new graphics port of spec'd size.*/
void 		ag_eraseRect(ascGrafPtr g,ascGrafRect r);/*fills graphics port with zeros*/
void 		ag_frameRect(ascGrafPtr g,ascGrafRect r);/*draws a box inside rectangle*/
void 		ag_plotUnscaledPoint(ascGrafPtr g,long destY,long destX);
	/*plots an unscaled point to the graphics buffer*/
void		ag_plotCenteredText(ascGrafPtr g,char *string,int centerY,int centerX);
void		ag_plotVerticalText(ascGrafPtr g,char *string,int centerY,int centerX);
void 		ag_printToFile(ascGrafPtr g,FILE *f);/*output graphics port to file*/
void 		ag_disposePort(ascGrafPtr g);/*destroy graphics port*/


/*plotpoint definitions*/
#define HUGE_VAL 10000000000.0
typedef struct plotPoint {
	int *next;
	double x,y;
} plotPoint;
plotPoint *firstPoint=NULL;

double minx=HUGE_VAL,maxx=-HUGE_VAL,miny=HUGE_VAL,maxy=-HUGE_VAL;


void AscGraf_AddPlotPoint(double x,double y);
void AscGraf_AddPlotPoint(double x,double y)
{
/*Store the point*/
	plotPoint *newPt=malloc(sizeof(plotPoint));
	newPt->x=x;
	newPt->y=y;
	newPt->next=(int *)firstPoint;
	firstPoint=newPt;
/*And check it against the image bounds*/
	if (x>maxx) maxx=x;
	if (y>maxy) maxy=y;
	if (x<minx) minx=x;
	if (y<miny) miny=y;
}


void AscGraf_PlotPoints(void);
void AscGraf_PlotPoints(void)
{
#define max_screenRows 33
#define max_screenCols 80
	char out[255];
	ascGrafRect rect={0,0,max_screenRows,max_screenCols};
	plotPoint *pt=firstPoint,*oldPt;
/*Create the ASCII Graphics port and initialize it.*/
	ascGrafPtr g=ag_newPort(max_screenRows,max_screenCols);
	ag_eraseRect(g,rect);
	ag_frameRect(g,rect);
	sprintf(out," Min y=%f ",miny);
	ag_plotCenteredText(g,out,0,g->mx/2);
	sprintf(out," Max y=%f ",maxy);
	ag_plotCenteredText(g,out,g->my-1,g->mx/2);
	sprintf(out," Min x=%f ",minx);
	ag_plotVerticalText(g,out,g->my/2,0);
	sprintf(out," Max x=%f ",maxx);
	ag_plotVerticalText(g,out,g->my/2,g->mx-1);
/*Traverse the point list, plotting and destroying points as we go*/
	while (pt!=NULL)
	{
	/*Plot the current point.*/
		long destX,destY;
		destX=1+(pt->x-minx)/(maxx-minx)*(g->mx-3);
		destY=1+(pt->y-miny)/(maxy-miny)*(g->my-3);
		ag_plotUnscaledPoint(g,destY,destX);
	/*Move on to the next point and destroy the old one.*/
		pt=(plotPoint *)(oldPt=pt)->next;
		free(oldPt);
	}
	ag_printToFile(g,stdout);/*render the buffer to the screen*/
/*Clean up afterwards */
	ag_disposePort(g);
	firstPoint=NULL;
	minx=miny=HUGE_VAL;
	maxx=maxy=-HUGE_VAL;	
}


ascGrafPtr ag_newPort(long numRows,long numCols)
{
	ascGrafPtr g=malloc(sizeof(ascGrafPort));
	g->mx=numCols;
	g->my=numRows;
	g->buf=malloc(sizeof(char)*g->mx*g->my);
	return g;
}
void ag_eraseRect(ascGrafPtr g,ascGrafRect r)
{
	long x,y;
	char *thisLine;
	for (y=r.ystart;y<r.yend;y++)
	{
		thisLine=g->buf+g->mx*y;
		for (x=r.xstart;x<r.xend;x++)
			*thisLine++=0;
	}
}
void ag_frameRect(ascGrafPtr g,ascGrafRect r)
{
	long x,y;
	char *thisLine;
	g->buf[r.ystart*g->mx+r.xstart]='+';
	g->buf[r.ystart*g->mx+r.xend-1]='+';
	g->buf[(r.yend-1)*g->mx+r.xend-1]='+';
	g->buf[(r.yend-1)*g->mx+r.xstart]='+';
	thisLine=g->buf+g->mx*(r.ystart+1)+r.xstart;
	for (y=r.ystart+1;y<(r.yend-1);y++)
		{	*thisLine='|';thisLine+=g->mx;	}
		
	thisLine=g->buf+g->mx*(r.ystart+1)+r.xend-1;
	for (y=r.ystart+1;y<(r.yend-1);y++)
		{	*thisLine='|';thisLine+=g->mx;	}
		
	thisLine=g->buf+g->mx*r.ystart+r.xstart+1;
	for (x=r.xstart+1;x<(r.xend-1);x++)
		*thisLine++='-';
		
	thisLine=g->buf+g->mx*(r.yend-1)+r.xstart+1;
	for (x=r.xstart+1;x<(r.xend-1);x++)
		*thisLine++='-';
}

void ag_plotUnscaledPoint(ascGrafPtr g,long destY,long destX)
{
	char *pt=&g->buf[destY*g->mx+destX];
	if (*pt<9) 
		(*pt)++;
	else *pt='X';
}
#include <string.h>
void ag_plotCenteredText(ascGrafPtr g,char *string,int centerY,int centerX)
{
	char *textStart=&g->buf[centerY*g->mx+centerX-strlen(string)/2];
	int i;
	for (i=0;string[i];i++)
		*textStart++=string[i];
}
void ag_plotVerticalText(ascGrafPtr g,char *string,int centerY,int centerX)
{
	char *textStart=&g->buf[(centerY+strlen(string)/2)*g->mx+centerX];
	int i;
	for (i=0;string[i];i++)
	{
		*textStart=string[i];
		textStart-=g->mx;
	}
}
void ag_printToFile(ascGrafPtr g,FILE *f)
{
	long x,y;
	char outBuf[255]=" .:;456789";
	for (x=10;x<255;x++)
		outBuf[x]=x;
	for (y=g->my-1;y>=0;y--)
	{
		unsigned char *thisLine=(unsigned char *)(g->buf+g->mx*y);
		for (x=0;x<g->mx;x++)
			fprintf(f,"%c",outBuf[*thisLine++]);
		fprintf(f,"\n");
	}
}
void ag_disposePort(ascGrafPtr g)
{
	free(g->buf);
	free(g);
}
