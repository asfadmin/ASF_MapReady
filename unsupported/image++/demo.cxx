#include "main.h"
#include "object.h"
#include "buffer2D.h"
#include "TWZimage.h"
image *createDemoImage(void);

typedef struct {
	double x,y;
	double velX,velY;
	long color;
} inertialPt;


class demoImage:public image { public:
	virtual Bool acceptablePt(double x,double y);
	virtual Bool needsRedraw(void);
	virtual Bool resizeOK(int sizeX, int sizeY);
	virtual char * getWindowName(void);
	virtual Bool drawIntoBuffer(buffer2D *b,Bool needToDraw);
	virtual void init0(void);
	virtual void die(void);
private:
	long frameNo;
	inertialPt *pts;
        virtual void initPoints(void);
};
const int NUM_PTS=10000;

image *createDemoImage(void)
{
	image *img=new demoImage;
	img->init0();
	return img;
}
void demoImage::init0(void)
{
	image::init0();
	wid=512;
	height=300;
	pts=(inertialPt *)Malloc(sizeof(inertialPt)*NUM_PTS);
	initPoints();
}
Bool demoImage::resizeOK(int sizeX,int sizeY)
{
	wid=(int)(screenWid*0.95);
	height=(int)(screenHeight*0.95);
	return True;
}
void demoImage::initPoints(void)
{
	double sinWeight=getRand(-2,2);
	double cosWeight=getRand(-2,2);
	double ySlant=getRand(0.2,1.5);
	double xOrigin=getRand(-1000.0,1000.0);
	double yOrigin=getRand(-1000.0,1000.0);
	for (int i=0;i<NUM_PTS;i++)
	{
		double x=0,y=0;
		while (!acceptablePt(x,y))
		{
			x=pts[i].x=getRand(0.0,(float)(wid));
			y=pts[i].y=getRand(0.0,(float)(height));
		}
		x/=wid;
		y/=height;
		pts[i].velX=sinWeight*sin((sin(x)+y+xOrigin)*M_PI*3)+0.0*getRand(-1.0,1.0);
		pts[i].velY=cosWeight*cos((yOrigin+x+y*ySlant)*M_PI*3)+0.0*getRand(-1.0,1.0);
		pts[i].color=rand()|(rand()<<16);
	}
	frameNo=0;
}
Bool demoImage::acceptablePt(double x,double y)
{
	const int messWid=56,messHt=11;
	const char *message=
/*01234567890123456789*/
 "                                                        "
 " xx                                                     "
 " xx                              xx                     "
 " xx                              xx                     "
 " xx                              xx                     "
 " xx          xxxx   xx       xx  xx   xxxxx    xx xxx   "
 " xx        xx   xx  xx       xx  xx  xx   xx   xxx  xx  "
 " xx        xx   xx   xx  x  xx   xx  xx   xx   xx       "
 " xxxxxxxx  xx   xx    xxxxxxx    xx  xx   xx   xx       "
 " xxxxxxxx   xxxx xxx   x   x     xx   xxxxx    xx       "
 "                                                        ";
	Bool xCheck,yCheck;
	if (x==0.0&&y==0.0) 
		return False;
 #if 1
	xCheck=(0==(((int)((x+y)/20))&0x1));
	yCheck=(0==(((int)((x-y)/20))&0x1));
 #else
	xCheck=yCheck=1;
 #endif
	int xScaled=(int)(x/wid*messWid);
	int yScaled=(int)(y/height*messHt);
	return (xCheck?yCheck:!yCheck)&&('x'==message[yScaled*messWid+xScaled]);
}
Bool demoImage::needsRedraw(void) { return True; }
char * demoImage::getWindowName(void) { return "Orion Lawlor Graphics Demo"; }
Bool demoImage::drawIntoBuffer(buffer2D *b,Bool needToDraw)
{
        float velScale=1.0;
	register inertialPt *pt=pts;
	int i;
	b->fill(BlackPixel);
	frameNo++;
	if (frameNo<100)
	  velScale=frameNo/100.0;
	if (frameNo>300)
	  initPoints();
	for (i=0;i<NUM_PTS;i++)
	{
		register Bool xOut,yOut;
		register int x,y;
		register double vx=pt->velX,vy=pt->velY;
		x=(int)(pt->x+=vx*velScale);
		y=(int)(pt->y+=vy*velScale);
		vy+=0.1;
		xOut=(x<0)|(x>=wid);
		yOut=(y<0)|(y>=height);
		if (xOut) vx*=-0.9;
		if (yOut) 
		  {
		    double normX=pt->x/wid*M_PI*2;
		    vx+=1*cos(normX);
		    vy*=-0.9;
		    vy-=0.1*sin(normX);
		  }
		pt->velX=vx;
		pt->velY=vy;
		if (!(xOut||yOut))
		{
			switch(b->depth)
			{
			case 8:
				*(char *)(b->lineStarts[y]+x)=pt->color;
				break;
			case 15:
				*(short *)(b->lineStarts[y]+x*2)=pt->color;
				break;
			case 24:
				*(long *)(b->lineStarts[y]+x*4)=pt->color;
				break;
			}
		}
		pt++;
	}
	return True;
}
void demoImage::die(void)
{
	Free((void *)pts);
	image::die();
}
