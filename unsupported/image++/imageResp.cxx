#include "main.h"
#include "imageResp.h"
void imageDisplayResponder::init3(int argc,char **argv,image *Nimg)
{
	int winX,winY;
	img=Nimg;
	if (!img)
	{
		printf("Internal Error: Image==NULL in imageDisplayResponder::init3.\n");
		exit(1);
	}
	magFactor=1.0;
	winX=img->wid;
	winY=img->height;
	magFactor=1.0;
	while ((winX*magFactor>=screenWid)||(winY*magFactor>=screenHeight))
		magFactor/=2.0;
	while ((winX*magFactor<200)&&(winY*magFactor<200))
		magFactor*=2.0;
	init4(argc,argv,(int)(winX*magFactor),(int)(winY*magFactor));
	offX=offY=lastX=lastY=0;
	buf=NULL;
	drawForReal=True;
}
void imageDisplayResponder::on2off(float *x,float *y)
{
	*x=(*x+offX)/magFactor;
	*y=(*y+offY)/magFactor;
}
void imageDisplayResponder::off2on(float *x,float *y)
{
	*x=(*x)*magFactor-offX;
	*y=(*y)*magFactor-offY;
}
menuBlock * imageDisplayResponder::getMenus(void)
{
	menuBlock *block=new menuBlock;
	block->init12(100,
		new menuItem("About this app",NULL),
		new menuItem("Zoom In",NULL),
		new menuItem("Zoom Out",NULL),
		new menuItem("Quit",NULL)
	);
	return block;
}
char *imageDisplayResponder::getWindowName(void)
{
	char *winName=(char *)Malloc(255*sizeof(char));
	strcpy(winName,"Image - ");
	return strcat(winName,img->getName());
}
void imageDisplayResponder::mouseClick(Window w,int button,int x,int y)
{
	float offx=x,offy=y;
	on2off(&offx,&offy);
	if (img->mouseClick(button,(double)offx,(double)offy))
		return;
	if (button == 2)
	{
		lastX=x;
		lastY=y;
	} else singleWinResponder::mouseClick(w,button,x,y);
}
void imageDisplayResponder::mouseMove(Window w,int state,int x,int y)
{
	float offx=x,offy=y;
	on2off(&offx,&offy);
	if (img->mouseMove(state,offx,offy))
		return;
	if (state&Button2Mask)
	{
		int dx=x-lastX,dy=y-lastY;
		offX-=dx;offY-=dy;
		checkOff();
		drawWindow(win->w);
		lastX=x;
		lastY=y;
	} else singleWinResponder::mouseMove(w,state,x,y);
}

void imageDisplayResponder::mouseRelease(Window w,int button,int x,int y)
{
	float offx=x,offy=y;
	on2off(&offx,&offy);
	img->mouseRelease(button,offx,offy);
	singleWinResponder::mouseRelease(w,button,x,y);
}
void imageDisplayResponder::resizeWindow(Window w,int newX,int newY)
{
	win->wid=newX;
	win->height=newY;
	if (img->resizeOK(newX,newY))
	{
		/*wid=newX;
		height=newY;*/
	}
	checkOff();
}
void imageDisplayResponder::doIdle(void)
{
	singleWinResponder::doIdle();
	drawForReal=False;
	if (img->needsRedraw())
		drawWindow(win->w);
	drawForReal=True;
}
void imageDisplayResponder::drawWindow(Window w)
{
	if (w==win->w)
	{
		Bool drawBuf=img->needsRedraw();
		Bool drawNeeded=True;
		if ((!buf)
			||(buf->wid!=img->wid*magFactor)
			||(buf->height!=img->height*magFactor))
		{
			if (buf)
				buf->die();
			buf=new buffer2D;
			buf->init3((int)(img->wid*magFactor),(int)(img->height*magFactor),screenBits);
			drawBuf=True;
		}
		if (drawBuf)
		{
			if (buf->okToDraw())
			{
				drawNeeded=img->drawSlowly(buf,drawForReal,win->w,win->wid,win->height);
				buf->doneDrawing();
			}
		}
		if (drawNeeded||drawForReal)
		{
		    if(img->needsRedraw())
		    {
			GC myGC=XCreateGC(display,win->w,0,NULL);
			XPutImage(display,win->w,myGC,buf->getImage(),offX,offY,0,0,win->wid,win->height);
			XFreeGC(display,myGC);
		    } else {
			GC myGC=XCreateGC(display,buf->getPixmap(),0,NULL);
			XCopyArea(display,buf->getPixmap(),win->w,myGC,offX,offY,win->wid,win->height,0,0);	
			XFreeGC(display,myGC);
		    }
		}
	}
	if (drawForReal)
		singleWinResponder::drawWindow(w);
}
void imageDisplayResponder::setMag(float factor)
{
	magFactor=factor;
	if (magFactor>100.0) 
		magFactor=100.0;
	if (magFactor<0.0001) 
		magFactor=0.0001;
	checkOff();	
	drawWindow(win->w);
}
int min(int a,int b);
int min(int a,int b)
{
	if (a<b) return a;
	else return b;
}
void imageDisplayResponder::checkOff(void)
{
	Bool stuffMoved=False;
	int maxX=(int)(img->wid*magFactor),
		maxY=(int)(img->height*magFactor);
	
	if ((win->wid>maxX)||(win->height>maxY))
		win->setSize(min(win->wid,maxX),min(win->height,maxY));
	if (offX>=maxX-win->wid) {offX=maxX-win->wid;stuffMoved=True;}
	if (offX<0) {offX=0;stuffMoved=True;}
	if (offY>=maxY-win->height) {offY=maxY-win->height;stuffMoved=True;}
	if (offY<0) {offY=0;stuffMoved=True;}
	if (stuffMoved)
	{
		drawWindow(win->w);
	}
}
void imageDisplayResponder::processMenuItem(int menu,int item)
{
	if (menu==100)
	{
		switch(item)
		{
			case 1:
				printf("An X-Windows program by Orion Lawlor, \n"
				"a member of the ASF STEP software lab.\n"
				"Visit our web site at http://www.images.alaska.edu\n");
				break;
			case 2:/*Zoom in.*/
				setMag(magFactor*2.0);
				break;
			case 3:/*Zoom out.*/
				setMag(magFactor/2.0);
				break;
			case 4:
				done=1;
				break;
			default:
				printf("Internal Error: unknown item %i.\n");
		}
	} else singleWinResponder::processMenuItem(menu,item);
}
void imageDisplayResponder::die(void)
{
	img->die();
	if (buf)
		buf->die();
	singleWinResponder::die();
}
