/******************************************************************************
NAME: Image

SYNOPSIS: Bare-Bones X-Windows LAS 6.0 display program.

DESCRIPTION:
	An X-Windows program which will display LAS 6.0 image 
files.  It's also designed to be slightly less annoying than 
xid.  The features which make it less annoying are:

	- can take image input at the command line, so
		scripts, etc. can display pictures more easily.
	- has no restrictions on the filename, extention, etc.
		which you pass to it (no need to avoid capital
		letters!).
	- doesn't mash the colormap (as badly).
	- no copyright restrictions.
	- compiles much faster and is more portable (?), because it
		uses only Xlib.


EXTERNAL ASSOCIATES: X Windows
FILE REFERENCES: X-Windows Headers
PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	     5/97  Orion Lawlor Needed to learn X-Windows,
                                Annoyed with xid.

HARDWARE/SOFTWARE LIMITATIONS: Need X-Windows.

ALGORITHM DESCRIPTION:  Read in LAS 6.0 byte,short, long, or float image.
	Display using the X Windows System to 8, 16, and 32 bit X Terminals.

BUGS: 

******************************************************************************/
#include <stdlib.h>
#include <unistd.h>
#include "main.h"
#include "responder.h"
#define allocDebug 1
#if allocDebug
	#define allocFree(mess) /*mess*/
	// printf(mess ", allocated %i bytes in %i objects.\n",allocedBytes,numObjects);
	#define maxObjectIndex 10000
	void * objectList[maxObjectIndex]={0};
	int numObjects=0,numCxx=0;
	long allocedBytes=0;
	int objectIndex=0;
	
		#ifdef malloc
		#undef malloc
		#endif
	void *Malloc(size_t size)
	{
		return (malloc(size));
		
		void *ret;
		objectList[objectIndex]=ret=malloc(size+sizeof(size_t)+sizeof(long));
		if (ret==NULL)
		{
			printf("Fatal Error: Cannot allocate %lui bytes.\n",size);
			debugger();
		}
		objectIndex++;
		objectIndex%=maxObjectIndex;
		numObjects++;
		allocedBytes+=size;
		size_t *rets=(size_t *)ret;
		*rets=size;
		*(char *)(((char *)(rets+1))+size)=0x00;
		return (void *)(rets+1);
	}
		#ifdef free
		#undef free
		#endif
	void Free(void *p)
	{
		free(p);
		return;
		
		int i=0;
		size_t size;
		if (p==(void *)0)
		{
			printf("A null object is never valid!\n",p);
			debugger();
		}
		while (i<maxObjectIndex)
			if (objectList[i]==(void *)((char *)p-sizeof(size_t)))
				break;
			else 
				i++;
		if (objectList[i]!=(void *)((char *)p-sizeof(size_t)))
		{
			printf("Ack! Bad object 0x%8x passed to delete!\n",p);
			debugger();
		}
		objectList[i]=(void *)0;
		size=*(size_t *)(((char *)p)-sizeof(size_t));
		if (0x00!=*(char *)(((char *)p)+size))
		{
			printf("Write past end of object 0x%8x!\n",p);
			debugger();
		}
		numObjects--;
		allocedBytes-=size;
		free(p);
	}
	
	
	void *operator new(size_t size)
	{
		numCxx++;
		return Malloc(size);
	}
	void operator delete(void *p)
	{
		Free(p);
		numCxx--;
	}
#else
	#define allocFree(mess) /* mess */
	
#endif


Display *display;
Visual *visual;
GC gc;
int screen,screenBits,screenBytes;
int startColor,lenColor;
long BlackPixel,WhitePixel;
long screenWid,screenHeight;
int done = 0;

void usage(void);

main(int argc, char *argv[])
{
	responder *r;
/*	if (fork()) 
		exit(0); Kill the original instance.  Prevents user from having to
				type the "&" in: "image <filename> &" all the time.*/
	display=XOpenDisplay("");
	if (NULL==display)
	{
		printf("Image cannot open your DISPLAY.\n\
Do you have your DISPLAY environment variable set?\n");
		exit(1);
	}
	screen=DefaultScreen(display);
	gc=DefaultGC(display, screen); /*I don't know WHAT these things are (display/screen/gc), but*/
  /*it seems like every drawing call needs several of them.*/
   	screenBits=DefaultDepth(display,screen);
   	switch(screenBits)
   	{
   		case 8:
   			screenBytes=1;
   			startColor=100;
   			lenColor=255-startColor;
   			break;
   		case 15:
   		case 16:
   			screenBytes=2;
   			startColor=0;
   			lenColor=(1<<5)-1;
   			break;
   		case 24:
   		case 32:
   			screenBytes=4;
   			startColor=0;
   			lenColor=(1<<8)-1;
   			break;
   		default:
   			fprintf(stderr,"Image doesn't know how to use %i bits/pixel.\nTry another X-Windows machine.\n",screenBits);
   			exit(1);
   	}
   	BlackPixel=BlackPixel(display,screen);
   	WhitePixel=WhitePixel(display,screen);
   	#ifdef height
   	#undef height
   	#endif
   	screenWid=DisplayWidth(display,screen);
   	screenHeight=DisplayHeight(display,screen);
   	/*printf("Image is initializing on a %i Bit/Pixel (%i Byte/Pixel) display...\n",screenBits,screenBytes);*/
	allocFree("Before createResponder");
	r=createResponder(argc,argv);
	allocFree("After createResponder");

	do {
		XEvent event;
		XButtonEvent *buttonEvt;
		XExposeEvent *exposeEvt;
		/*XKeyEvent *keyEvt;*/
		XMotionEvent *motionEvt;
		XResizeRequestEvent *resizeEvt;
		r->doIdle();
		if(XCheckMaskEvent(display,0xffffffff, &event))
		{
		 /* allocFree("Eventloop"); */
		  switch (event.type) {
		  case ButtonPress:/*A mouse button was pressed.*/
			buttonEvt = (XButtonEvent *) &event;
			r->mouseClick(buttonEvt->window,buttonEvt->button,buttonEvt->x,buttonEvt->y);
			break;
		  case ButtonRelease:/*A mouse button was released.*/
			buttonEvt = (XButtonEvent *) &event;
			r->mouseRelease(buttonEvt->window,buttonEvt->button,buttonEvt->x,buttonEvt->y);
			break;
		  case MotionNotify:
			motionEvt=(XMotionEvent *)&event;
			while (XCheckMaskEvent(display,Button1MotionMask|Button2MotionMask|Button3MotionMask, &event))
				motionEvt=(XMotionEvent *)&event;
			r->mouseMove(motionEvt->window,motionEvt->state,motionEvt->x,motionEvt->y);
			break;
		  case Expose:/*The window needs to be redrawn.*/
			exposeEvt=(XExposeEvent *)&event;
			if (exposeEvt->count==0) /*Doing this will help avoid redundant draws (supposedly).*/
				r->drawWindow(exposeEvt->window); /*Do the actual redrawing.*/
			break;
		  case GraphicsExpose:
		  case NoExpose:/*I'm not terribly sure what either of these events are, so I'm ignoring them.*/
			break;
			
		  case KeyPress:
		  case KeyRelease:
			/*
			printf("*********** Found a key event *************\n");
			keyEvt=(XKeyEvent *)&event;
			if (keyEvt->type==KeyPress)
				printf("\tYou pressed the (%d) key, but I don't care!\n",keyEvt->keycode);
			else
				printf("\tYou released the (%d) key, but I STILL don't care!\n",keyEvt->keycode);
			*/
			break;
		  case ResizeRequest:
			resizeEvt=(XResizeRequestEvent *)&event;
		  #undef height
			r->resizeWindow(resizeEvt->window,resizeEvt->width,resizeEvt->height);
			break;
		  default:
			printf("Unknown event type %d found.  Argh!  Bad!  Ignoring.\n",event.type);
			break;
		  }
		}
	} while (!done);
	allocFree("Before DeleteResponder");
	r->die();
	allocFree("After DeleteResponder");
	XCloseDisplay(display);
	return 0;
}
