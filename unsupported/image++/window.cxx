#include "main.h"
#include "object.h"
#include "window.h"

window * createWindow(char *windowName,int maxX,int maxY)
{
	window *win=new window;
	win->init3(windowName,maxX,maxY);
	return win;
}

void window::init3(char *windowName,int maxX,int maxY)
{
	XSetWindowAttributes attributes;
	XSizeHints  sizehints;
	init0();
	wid=maxX;
	height=maxY;
	cmap=(Colormap)NULL;
	strcpy(name,windowName);
	if (screenBits==8)
		installGreyColormap();
	else
		visual=DefaultVisual(display, screen);
	
	sizehints.x = 20; /*Window's top left corner's screen coordinates.*/
	sizehints.y = 50; /*Same thing, vertical.*/
	sizehints.flags=USPosition;
	attributes.colormap = cmap;
	
    /*Create a window with the specified size and colormap.*/
	w = XCreateWindow(display,XDefaultRootWindow(display), 
	    sizehints.x, sizehints.y, wid, height,
	    0,screenBits,InputOutput, visual, (screenBits==8)?CWColormap:0, &attributes);
    
    /*Give the window a name.*/
	XSetStandardProperties(display, w, name,
	    "", None, NULL, 1, &sizehints);
	    
	XSelectInput(display,w,ButtonPressMask|ButtonReleaseMask|ExposureMask|KeyPressMask|
			KeyReleaseMask|Button1MotionMask|Button2MotionMask|Button3MotionMask|ResizeRedirectMask);
			
	XMapWindow(display,w);
}

void window::installGreyColormap(void)
{
  /*Find the screen which is 8-bit palette-ized.*/
	XColor      color;
	Colormap 	oldColor=DefaultColormap(display,screen);/*Hang on to the old colormap.*/
	int 		i,entries;
	XVisualInfo   vTemplate, *visualList;
	int          visualsMatched;
	vTemplate.screen = screen;
	vTemplate.depth = 8;
	vTemplate.c_class = PseudoColor;
	visualList = XGetVisualInfo(display, 
			VisualScreenMask|VisualClassMask|VisualDepthMask,
			&vTemplate, &visualsMatched);
	visual = visualList[0].visual;
  /*Create a colormap on that screen.*/
	cmap = XCreateColormap( display,XDefaultRootWindow(display), visual, AllocAll);

  /*Now fill the bottom part of that colormap with the old colors (to minimize flashing)
	and the upper (majority) part with our new colors (which are shades of grey).*/	
	entries=visual->map_entries;/*Total # of entries in colormap.*/
	for (i=0; i < entries; i++)
	{
		color.flags = DoRed|DoGreen|DoBlue;
		color.pixel=i;
		if (i<startColor)
			XQueryColor(display,oldColor,&color);/*Read in the old color.*/
		else {/*Create a new color-- a shade of grey.*/
			color.red =   (unsigned short)(65535*((float)(i-startColor)/(float)lenColor));
			color.green = (unsigned short)(65535*((float)(i-startColor)/(float)lenColor));
			color.blue =  (unsigned short)(65535*((float)(i-startColor)/(float)lenColor));
		}
		XStoreColor(display, cmap, &color );/*Now write this entry of the colormap out.*/
	}

	XFree((char *)visualList);

}
void window::setSize(int newX,int newY)
{
	XRectangle rect={0,0,30000,30000};
	wid=newX;
	height=newY;
	XResizeWindow(display,w,wid,height);
	rect.width=wid;
#undef height
	rect.height=
#define height frame.bottom
		height;
	XSetClipRectangles(display,gc,0,0,&rect,1,Unsorted);
}
void window::die(void)
{
	XDestroyWindow(display,w);
	object::die();
}


#if 0
void window::mouseClick(window *w,int button,int x,int y)
{
	if (button==1)
	{
		float fx=x,fy=y;
		on2off(w,&fx,&fy);
		printf("%s: \n\t  X=%.6f   Y=%.6f   value=%12.6f\n",
			w->name, fx, fy, getValue(w->f, fx, fy) );
	}
}
void processMenuItem(window *w,int item)
{
	switch(item)
	{
		case 1: /*Program Information.*/
			printf("Image, a LAS 6.0 image display program.  ASF STEP Tools.  By Orion Lawlor.\n");
			break;
		case 2: /*Zoom in*/
			w->magFactor*=2;
			zoomChanged(w);
			break;
		case 3: /*Zoom out*/
			w->magFactor/=2;
			zoomChanged(w);
			break;
		case 4:
			printf("Goodbye!\n");
			done=1;
			break;
	}
}
#endif

