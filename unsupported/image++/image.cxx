#include "main.h"
#include "object.h"
#include "buffer2D.h"
#include "TWZimage.h"

Bool image::needsRedraw(void)
{
	return False;
}
char *image::getName(void)
{
	return "Default Image Name";
}
Bool image::mouseClick(int button,double x,double y)
{
	return False;
}
Bool image::mouseMove(int state,double x,double y)
{
	return False;
}
void image::mouseRelease(int button,double x,double y) {}
Bool image::resizeOK(int sizeX,int sizeY) {return False;}
Bool image::drawIntoBuffer(buffer2D *b,Bool needToDraw)
{
	return False;
}
Bool image::drawSlowly(buffer2D *b,Bool needToDraw,Window w,int sizeX,int sizeY)
{
	return drawIntoBuffer(b,needToDraw);
}
#include <time.h>
void image::initUserWait(Window w)
{
	lastAngle=64*90;
	lastTime=(unsigned long)(time(NULL)-1);
	XSetForeground(display,gc,WhitePixel);
	XFillRectangle(display,w,gc,0,0,30000,30000);
}
void image::tellUserToWait(Window w,int sizeX,int sizeY,int currWhat,int outOfWhat)
{
	int thisAngle=64*90-currWhat*64*361/outOfWhat;
	if (time(NULL)!=lastTime)
	{
		const int border=10;
		int cenX,cenY;
		int size;
		cenX=sizeX/2;
		cenY=sizeY/2;
		if (sizeX>sizeY)
			size=sizeY;
		else size=sizeX;
		size-=2*border;
		XSetForeground(display,gc,0x7Cd07Cd0);
		XFillArc(display,w,gc,cenX-size/2,cenY-size/2,size,size,lastAngle,thisAngle-lastAngle);
		XSync(display,False);
		lastAngle=thisAngle;
		lastTime=time(NULL);
	}
}

double image::getImageValue(double x,double y)
{
	x-=0.5;
	y-=0.5;
	int ix=(int)floor(x),iy=(int)floor(y);
	double dx=x-ix,dy=y-iy;
	double tl=getIntImageValue(ix,iy),
		tr=getIntImageValue(ix+1,iy),
		bl=getIntImageValue(ix,iy+1),
		br=getIntImageValue(ix+1,iy+1);
	double top=tl+dx*(tr-tl),
		bot=bl+dx*(br-bl);
	return top+dy*(bot-top);
}
double image::getIntImageValue(int x,int y)
{
	return 0.0;
}
