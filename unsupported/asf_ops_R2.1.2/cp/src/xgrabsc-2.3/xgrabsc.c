/*========================================================================
 *
 * Name - xgrabsc.c
 *
 * ccs version:	1.31
 *
 * ccsid:	@(#)xgrabsc.c	1.31 - 06/28/93 09:13:50
 * from: 	ccs/s.xgrabsc.c
 * date: 	06/28/93 09:14:49
 *
 * Copyright (c) 1990-93 Bruce Schuchardt.
 * Read the file cpyright.h for full copyright information.
 *
 * XmuClientWindow and TryChildren are Copyright 1989 by the Massachusetts
 * Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this [XmuClientWindow]
 * software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Description:
 *
 * xgrabsc - grab screen images and store in files
 *
 *========================================================================
 */

static char sccsid_xgrabsc_c[] =
    "@(#)xgrabsc.c	1.2 95/07/27 15:04:49";

#if defined(__hpux)
/* the folks at HP have decided to make things more difficult
 */
#define XLIB_ILLEGAL_ACCESS
#endif


#include "cpyright.h"
#include "patchlevel.h"
#include "config.h"
#include "checkvm.h"
#include "cmdopts.h"

#include <stdio.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>

#ifdef VMS
#include "XWDFile.h"
#else
#include <X11/XWDFile.h>
#endif /* VMS */

#ifndef CARD32
#include <X11/Xmd.h>
#endif


#ifndef NO_VIRTUAL_WINDOW
#include "virtual.h"
#endif


#define MAX_CELLS  256
#define TRUE  1
#define FALSE 0


typedef enum {
  NO_DITHER=0,
  FS_DITHER,
  MATRIX_DITHER,
  MATRIX_HALFTONE,
  MAP_BW
} ditherType;


typedef unsigned char byte;
typedef unsigned long dw;
typedef unsigned int  word;



typedef struct {
  XImage *ximage;
  word numcells;
  word red[MAX_CELLS], green[MAX_CELLS], blue[MAX_CELLS];
  byte used[MAX_CELLS];
} imageInfo;


typedef enum {
  sourceRect,
  sourceRoot,
  sourceId,
  sourceWd,
  sourceKey,
  sourceCoords
} sourceType;

typedef enum {
  puzzleFormat,
  psFormat,
  simpleFormat,
  xwdFormat,
  pixmapFormat
} formatType;




#ifdef BCOPY
#ifdef MEMCPY
#undef MEMCPY
#endif
#define memcpy(x,y,c) bcopy(y,x,c)
#define memset(x,c)   bzero(x,c) /* well, I only use it for zeroing out stuff */
#endif

#ifdef MEMCPY
char *memcpy();
char *memset();
#endif


#ifdef VMS
#define rindex strrchr
#endif

static Display *hDisplay;
static int      hScreen;
static Window   hRoot, vRoot;
static int      displayCells;
static char    *programName;
static char    imageName[64];

static char    *version    = XGRABSC_VERSION;
static int      patchLevel = XGRABSC_PATCHLEVEL;
static int      verbose;

static word nr[MAX_CELLS], ng[MAX_CELLS], nb[MAX_CELLS];

static char   hexdigits[] = "0123456789abcdef";

static double pageWidth   = PAPER_WIDTH;
static double pageHeight  = PAPER_HEIGHT;
static double horizMargin = HORIZ_MARGIN;
static double vertMargin  = VERT_MARGIN;
static double horizInset  = PAPER_WIDTH - HORIZ_MARGIN - HORIZ_MARGIN;
static double vertInset   = PAPER_HEIGHT - VERT_MARGIN - VERT_MARGIN;

static char        *outfileName;

ditherType  DitherKind;
int         Halftone;
int         ForceBitmap;

static int needColorImageProc;


/* most functions have been moved to seperate source include files */

#include "process.hc"         /* color processing     */
#include "get.hc"             /* get images           */
#include "mem.hc"             /* memcpy, memset       */
#include "convert.hc"         /* color->bw conversion */
#include "write.hc"           /* output functions     */




static Window xgTryChildren();

/* versions of XmuGetClientWindow and screenNumberOfScreen.
 * GetClientWindow is used so Xmu need not be linked (causes problems on
 *  some platforms and configurations if Xt is not used)
 * ScreenNumberOfScreen is used because some X implementations do not
 *  include this function (HP in particular).
 *
 * The following three functions are from the X11R4 MIT distribution and
 * are copyrighted by MIT (see the file header)
 */

static Window GetClientWindow (win)
    Window win;
{
    Atom WM_STATE;
    Atom type = None;
    int format;
    unsigned long nitems, after;
    unsigned char *data;
    Window inf;

    WM_STATE = XInternAtom(hDisplay, "WM_STATE", True);
    if (!WM_STATE)
	return win;
    XGetWindowProperty(hDisplay, win, WM_STATE, 0, 0, False, AnyPropertyType,
		       &type, &format, &nitems, &after, &data);
    if (type)
	return win;
    inf = xgTryChildren(win, WM_STATE);
    if (!inf)
	inf = win;
    return inf;
}

static Window xgTryChildren (win, WM_STATE)
    Window win;
    Atom WM_STATE;
{
    Window root, parent;
    Window *children;
    unsigned int nchildren;
    unsigned int i;
    Atom type = None;
    int format;
    unsigned long nitems, after;
    unsigned char *data;
    Window inf = 0;

    if (!XQueryTree(hDisplay, win, &root, &parent, &children, &nchildren))
	return 0;
    for (i = 0; !inf && (i < nchildren); i++) {
	XGetWindowProperty(hDisplay, children[i], WM_STATE, 0, 0, False,
			   AnyPropertyType, &type, &format, &nitems,
			   &after, &data);
	if (type)
	    inf = children[i];
    }
    for (i = 0; !inf && (i < nchildren); i++)
	inf = xgTryChildren(children[i], WM_STATE);
    if (children) XFree((char *)children);
    return inf;
}



int ScreenNumberOfScreen (scr)
    register Screen *scr;
{
    register Display *dpy = scr->display;
    /*
    register Screen *dpyscr = dpy->screens;
    */
    register Screen *dpyscr = ScreenOfDisplay(dpy, 0);
    register int i;

    /*
    for (i = 0; i < dpy->nscreens; i++, dpyscr++) {
    */
    for (i = 0; i < ScreenCount(dpy); i++, dpyscr++) {
	if (scr == dpyscr) return i;
    }
    return -1;
}



xgrabsc(argc, argv, env)
  int argc;
  char *argv[];  /* must have at least argv[0] == programName */
  char *env;
{
  char        *args[100], *arg;
  extern char *optarg;
  FILE        *outfile;
  XRectangle  xrect;
  XWindowAttributes xwa;
  imageInfo   image;
  int         argn, argi, polarity;
  int	      cmdi, cmdargi;
  int         doAnd;
  int         doOr;
  int         doReverse;
  int         depth;
  int         noBell;
  int         psColor;
  int         brighten;
  int         grabServer;
  int         compress;
  int         sleepSeconds;
  int         postSelectSleepSeconds;
  int         andBits;
  int         orBits;
  int         encapsulate;
  Window      sourceWindow, childWindow, clientWindow, wmWindow, ignored;
  char       *ptr;
  char       *display;
  int         i, x, y;
  int         brightenFactor;
  sourceType  source;
  formatType  outputFormat;
  int         xpmFormat;
  int         landscape;
  int         binary;
  int	      borders;
  int	      checkLimits;
  int	      preview;
  int         onlyEpsi;
  int         xwdxy;


  outfile     = stdout;
  outfileName = NULL;
  display     = NULL;
  programName = argv[0];

  noBell       = FALSE;
  brighten     = FALSE;
  compress     = TRUE;
  DitherKind   = NO_DITHER;
  doAnd        = FALSE;
  doOr         = FALSE;
  doReverse    = FALSE;
  encapsulate  = FALSE;
  ForceBitmap  = FALSE;
  grabServer   = TRUE;
  Halftone     = FALSE;
  landscape    = FALSE;
  binary       = FALSE;
  sleepSeconds = 0;
  postSelectSleepSeconds = 0;
  verbose      = FALSE;
  borders      = TRUE;
#ifdef NO_PRINTER_MEMORY_CHECKS
  checkLimits  = FALSE;
#else
  checkLimits  = TRUE;
#endif
  preview      = FALSE;
  needColorImageProc = DEFAULT_NEED_COLORIMAGE_PROC;

  source       = sourceRect;
  outputFormat = psFormat;
  psColor      = FALSE;
  onlyEpsi     = FALSE;
  xwdxy	       = FALSE;


  /* merge environment options and command line options */
  args[0] = programName;
  if (env != NULL) {
    args[1] = env;
    for (argn=2; argn<100  &&
                (args[argn]=(char *)strchr(args[argn-1], ' ')) != NULL;
	        argn++) {
      /* remove leading white space */
      while (*args[argn] == ' ' || *args[argn] == 9) {
        *(args[argn]) = '\0';
        args[argn]++;
      }
      if (*args[argn] == '|' || *args[argn] == '>') /* dbx leaves these in the cmd line */
        break;
    }
  }
  else
    argn = 1;

  for (i=1; i<argc && argn<100; argn++, i++)
    args[argn] = argv[i];

  
  for (argi=1; argi<argn; argi++) {
    arg = args[argi];

    polarity = 1;
    if (arg[0] == '-') {
      arg++;
      if (arg[0] == '-') {
        arg++;
        polarity = 0;
      }
      else if (arg[0] == 'n' && arg[1] == 'o') {
        arg += 2;
	polarity = 0;
      }
    }

    for (cmdi=0; cmdi<numCmds; cmdi++) {
      if (strcmp(arg, commands[cmdi].userstr) == 0)
        break;
    }
    if (cmdi >= numCmds) {
      fprintf(stderr, "%s: unknown option '%s'\n", programName, arg);
      exit(3);
    }

    cmdargi = argi+1;
    argi += commands[cmdi].numargs;
    if (argi >= argn) {
      fprintf(stderr, "%s: not enough arguments for '%s'\n", programName, arg);
      exit(3);
    }

    switch (commands[cmdi].command) {
      case CMD_DISPLAY:
        display = args[cmdargi];
        break;
      case CMD_BELL:
        noBell = !polarity;
        break;
      case CMD_GRABSERVER:
        grabServer = polarity;
        break;
      case CMD_OUTPUT:
        outfileName = args[cmdargi];
        break;
      case CMD_PRESLEEP:
        sleepSeconds = atoi(args[cmdargi]);
        if (sleepSeconds < 0) sleepSeconds = 0;
        break;
      case CMD_POSTSLEEP:
        postSelectSleepSeconds = atoi(args[cmdargi]);
        if (postSelectSleepSeconds < 0) postSelectSleepSeconds = 0;
        break;
      case CMD_VERBOSE:
        verbose = polarity;
        break;
      case CMD_BORDERS:
        borders = polarity;
	break;
      case CMD_NOBORDERS:
        borders = !polarity;
	break;


      case CMD_SOURCE_KEY:
        source = sourceKey;
        grabServer = FALSE;
	break;
      case CMD_SOURCE_COORDS:
      { short w,h,x,y;
	source = sourceCoords;
        sscanf(args[cmdargi], "%ix%i+%i+%i", &w, &h, &x, &y);
	xrect.width = w; 
	xrect.height = h;
	xrect.x = x;  
	xrect.y = y;
	break;
      }

      case CMD_SOURCE_ID:
        source = sourceId;
        sourceWindow = 0;
        if (!sscanf(args[cmdargi], "%ix", &sourceWindow)) {
          fprintf(stderr, "%s: invalid window id '%s'\n", programName, args[cmdargi]);
	  exit(3);
        }
        break;
      case CMD_SOURCE_RECT:
	source = sourceRect;
	break;
      case CMD_SOURCE_ROOT:
        source = sourceRoot;
        break;
      case CMD_SOURCE_WD:
        source = sourceWd;
        break;



      case CMD_AND:
        doAnd = polarity;
	if (doAnd)
          andBits = atoi(args[cmdargi]);
        break;
      case CMD_OR:
        doOr = polarity;
	if (doOr)
          orBits = atoi(args[cmdargi]);
        break;
      case CMD_NOT:
        doReverse = polarity;
	break;
      case CMD_BRIGHTEN:
        brighten = polarity;
	if (brighten) {
          brightenFactor = atoi(args[cmdargi]);
          if (brightenFactor <= 0) {
            fprintf(stderr, "%s: brightening factor must be a positive number\n",
              programName);
            exit(3);
          }
	  if (brightenFactor != 100)
            brighten = TRUE;
	  else
	    brighten = FALSE;
	}
        break;

      case CMD_DITHER_MAP:
        ForceBitmap = TRUE;
        Halftone = FALSE;
        break;
      case CMD_DITHER_MATRIX:
        DitherKind = MATRIX_DITHER;
        Halftone = TRUE;
        ForceBitmap = FALSE;
        break;
      case CMD_DITHER_FS:
        if (!polarity) {
          DitherKind = NO_DITHER;
	  Halftone = ForceBitmap = FALSE;
        }
        else {
          DitherKind = FS_DITHER;
          Halftone = TRUE;
          ForceBitmap = FALSE;
        }
        break;
      case CMD_DITHER_HALFTONE:
        DitherKind = MATRIX_HALFTONE;
        Halftone = TRUE;
        ForceBitmap = FALSE;
        break;
      case CMD_DITHER_NONE:
        DitherKind = NO_DITHER;
	Halftone = ForceBitmap = FALSE;
	break;



      case CMD_OUTPUT_PS:
        psColor = FALSE;
        outputFormat = psFormat;
        break;
      case CMD_OUTPUT_CPS:
        psColor = polarity;
        outputFormat = psFormat;
	break;
      case CMD_OUTPUT_SIMPLE:
        outputFormat = simpleFormat;
        break;
      case CMD_OUTPUT_XWD:
        outputFormat = xwdFormat;
	xwdxy = FALSE;
        break;
      case CMD_OUTPUT_XWDXY:
        outputFormat = xwdFormat;
	xwdxy = TRUE;
        break;
      case CMD_OUTPUT_PIXMAP:
        xpmFormat = 1;
        outputFormat = pixmapFormat;
        break;
      case CMD_OUTPUT_XPM2:
        if (polarity) xpmFormat = 2;
	else if (xpmFormat == 2) xpmFormat = 1;
	outputFormat = pixmapFormat;
	break;
      case CMD_OUTPUT_XPM3:
        if (polarity) xpmFormat = 3;
	else if (xpmFormat == 3) xpmFormat = 1;
	outputFormat = pixmapFormat;
	break;

      case CMD_OUTPUT_PUZZLE:
        outputFormat = puzzleFormat;
        break;

      case CMD_LANDSCAPE:
        landscape = polarity;
	outputFormat = psFormat;
        break;
      case CMD_BIN:
        binary = polarity;
        outputFormat = psFormat;
	break;
      case CMD_COMPRESS:
        compress = polarity;
        outputFormat = psFormat;
        break;
      case CMD_EPS:
        encapsulate = polarity;
        outputFormat = psFormat;
	break;
      case CMD_PREVIEWONLY:
        onlyEpsi = polarity;
	preview = polarity;
        outputFormat = psFormat;
	break;
      case CMD_LIMIT:
        checkLimits = polarity;
        outputFormat = psFormat;
	break;
      case CMD_PREVIEW:
        preview = polarity;
        outputFormat = psFormat;
	break;  
      case CMD_PAGE:
        outputFormat = psFormat;
        sscanf(args[cmdargi], "%lfx%lf-%lf-%lf",
	  &pageWidth, &pageHeight, &horizMargin, &vertMargin);
	horizInset = pageWidth - horizMargin - horizMargin;
	vertInset  = pageHeight - vertMargin - vertMargin;
        break;
      case CMD_COLORPROC:
        outputFormat = psFormat;
	needColorImageProc = polarity;
	break;
    }
  }


  if (verbose) {
    fprintf(stderr, "%s: xgrabsc version %s\n", programName, version);
    fprintf(stderr, "%s:         patchlevel %d\n", programName, patchLevel);
    fprintf(stderr, "%s:         %s\n\n", programName, Copyright);
  }

  if (!display) display = (char *)getenv("DISPLAY");
  hDisplay = XOpenDisplay(display);
  if (!hDisplay) {
    fprintf(stderr, "%s: could not open X display\n", programName);
    exit(3);
  }
  hScreen  = DefaultScreen(hDisplay);
  hRoot    = DefaultRootWindow(hDisplay);
#ifndef NO_VIRTUAL_WINDOW
  vRoot    = VirtualRootWindow(hDisplay, hScreen);
#else
  vRoot    = hRoot;
#endif

  depth  = DefaultDepth(hDisplay, hScreen);
  if (DisplayCells(hDisplay, hScreen) > MAX_CELLS) {
    fprintf(stderr, "%s: color table is too big for this program\n",
      programName);
    XCloseDisplay(hDisplay);
    exit(3);
  }

  /* sleep if asked to do so */
  if (sleepSeconds)
    sleep(sleepSeconds);

  /* grab the screen if asked to do so */
  if (grabServer)
    XGrabServer(hDisplay);

  if (source != sourceId)
    sourceWindow = hRoot;

  childWindow = (Window)NULL;

  switch (source) {
    case sourceKey:
      childWindow =
#ifdef SELECTION_MASK
          getWindowWhenKeyIsPressed(SELECTION_MASK)
#else
          getWindowWhenKeyIsPressed(ControlMask);
#endif
      if (!childWindow) {
        fprintf(stderr, "%s: unable to find source window\n", programName);
        XCloseDisplay(hDisplay);
        exit(3);
      }
      break;
    case sourceCoords:
      /* already got the xrect in the command line processing section */
      break;
    case sourceId:
      childWindow = sourceWindow;
      break;
    case sourceWd:
      /* grab the image from the root window so menus will show up on top
       * of the window */
      childWindow=getWindow();
      if (!childWindow) {
        fprintf(stderr, "%s: unable to find source window\n", programName);
        XCloseDisplay(hDisplay);
        exit(3);
      }
      break;
    case sourceRoot:
      xrect.x = xrect.y = 0;
      xrect.width  = DisplayWidth(hDisplay, hScreen);
      xrect.height = DisplayHeight(hDisplay, hScreen);
      break;
    case sourceRect:
    default:
      if (!getRectangle(&xrect)) {
        XCloseDisplay(hDisplay);
        exit(3);
      }
      break;
  }



  clientWindow = (Window)NULL;
  if (!borders  &&
      (source == sourceKey || source == sourceWd || source == sourceId)  &&
      childWindow != hRoot) {
    /* look for a different client window */
    clientWindow = GetClientWindow(childWindow);
    if (clientWindow && clientWindow != childWindow) {
      if (verbose)
	fprintf(stderr, "%s: found subwindow 0x%x\n", programName, clientWindow);
      wmWindow = childWindow;
      childWindow = clientWindow;
    }
  }

  if ((source == sourceKey && childWindow != hRoot) ||
      source == sourceId ||
      source == sourceWd) {

    if (childWindow == hRoot) {
      xrect.x      = 0;
      xrect.y      = 0;
      xrect.width  = DisplayWidth(hDisplay, hScreen);
      xrect.height = DisplayHeight(hDisplay, hScreen);
    }
    else {

      /* take the child window (and optional client/wm window info) and
      * determine the portion of the root window that should be grabbed */

      if (!XGetWindowAttributes(hDisplay, childWindow, &xwa)) {
        fprintf(stderr, "%s: unable to get window coordinates\n", programName);
        XCloseDisplay(hDisplay);
        exit(3);
      }

      /* get the correct screen and root window for the selected window,
        * and if it isn't the default, muck with the global state of the
        * program a bit
        */
      i = ScreenNumberOfScreen(xwa.screen);
      if (i != hScreen) {
        hRoot   = xwa.root;
        hScreen = i;
#ifndef NO_VIRTUAL_WINDOW
        vRoot   = VirtualRootWindow(hDisplay, hScreen);
#else
        vRoot   = hRoot;
#endif
        depth   = DefaultDepth(hDisplay, hScreen);
      }

      sourceWindow = vRoot;
      if (!borders) {
        xrect.x      = 0;
        xrect.y      = 0;
        xrect.width  = xwa.width;
        xrect.height = xwa.height;
      }
      else {
        xrect.x      = -xwa.border_width;
        xrect.y      = -xwa.border_width;
        xrect.width  = xwa.width + (2 * xwa.border_width);
        xrect.height = xwa.height + (2 * xwa.border_width);
      }

      /* translate the rectangle coordinates to root-window coordinates */
      if (!XTranslateCoordinates(hDisplay, childWindow, vRoot,
            (int)(xrect.x), (int)(xrect.y), &x, &y, &ignored)) {
	fprintf(stderr, "%s: unable to translate window coordinates\n", programName);
	XCloseDisplay(hDisplay);
	exit(3);
      }
      xrect.x = (short)x;   /* why did they make rectangle coords shorts? */
      xrect.y = (short)y;


      if (hRoot == vRoot) {
        /* do some clipping here, since it's cheap */
        i = DisplayWidth(hDisplay, hScreen);
        if (xrect.x + xrect.width > i) {
          x = i - (int)(xrect.x);
          if (x < 0) x = 0;
          xrect.width = x;
        }
        i = DisplayHeight(hDisplay, hScreen);
        if (xrect.y + xrect.height > i) {
          x = i - (int)(xrect.y);
          if (x < 0) x = 0;
          xrect.height = x;
        }
      }
    }
  }


  if (verbose)
    fprintf(stderr, "%s: bounding box is [x=%d y=%d w=%d h=%d]\n", programName, xrect.x, xrect.y,
                    xrect.width, xrect.height);

  /* sleep if asked to do so */
  if (postSelectSleepSeconds) {
    if (grabServer) {
      XUngrabServer(hDisplay);
      XSync(hDisplay, FALSE);
    }
    sleep(postSelectSleepSeconds);
    if (grabServer)
      XGrabServer(hDisplay);
  }


  /* get the image bounded by the rectangle from the source window */
  if (!noBell)
    XBell(hDisplay, 50);

  if (!getImage(&xrect, &image, sourceWindow, childWindow)) {
    XCloseDisplay(hDisplay);
    exit(3);
  }
  if (grabServer)
    XUngrabServer(hDisplay);

  if (!noBell) {
    XBell(hDisplay, 20);
    XBell(hDisplay, 30);
  }

  XFlush(hDisplay);



  /* do color image processing/conversions */

  if (depth >= 2) {
    if (brighten)
      brightenColors(&image, brightenFactor);
    if (doAnd)
      alterPlanes(&image, TRUE, andBits);
    if (doOr)
      alterPlanes(&image, FALSE, orBits);
  }
  
  if (doReverse)
    reverseColors(&image);

  if (depth >= 2) {
    if (! (preview && psColor) ) {
      if (ForceBitmap) {
        pixmap2bitmap(&image);
        depth = 1;
      }
      else if (Halftone)
        pixmap2halftone(&image, DitherKind);
      else
        compressColormap(&image);
    }
    else
      compressColormap(&image);
  }


  /* open the output stream */
  if (outfileName) {
    outfile = fopen(outfileName, "w");
    if (!outfile) {
      fprintf(stderr, "%s: ", programName);
      perror(outfileName);
      exit(3);
    }
    /* form an image name based on the file name */
    {
    char *img = rindex(outfileName, '/');
    if (img) strcpy(imageName, img + 1);
    else strcpy(imageName, outfileName);
    ptr = rindex(imageName, '.');  /* Drop the final extension */
    if (ptr) *ptr = '\0';
    }
  }
  else
    strcpy(imageName,"unnamed");


  /* write to the output stream in the requested format */
  switch (outputFormat) {
    case xwdFormat:
      writeXWD(&image, outfile, xwdxy);
      break;
    case simpleFormat:
      writeSimple(&image, outfile);
      break;
    case puzzleFormat:
      writePuzzle(&image, outfile);
      break;
    case pixmapFormat:
      if (image.ximage->depth <= 1)
        writeXYPixmap(&image, outfile);
      else
        writeZPixmap(xpmFormat, &image, outfile);
      break;
    case psFormat:
    default:
      if (onlyEpsi)
        writeOnlyPreview(&image, outfile);
      else if (psColor && image.ximage->depth >= 2)
        writeColorPS(&image, outfile, compress, encapsulate,
	                preview, landscape, binary, checkLimits);
      else
        writePostscript(&image, outfile, compress, encapsulate,
	                preview, landscape, binary, checkLimits);
      break;
  }


  XDestroyImage(image.ximage);
  XCloseDisplay(hDisplay);
  if (outfileName)
    fclose(outfile);

  exit(0);
}





#ifndef NO_MAIN
main(argc, argv)
  int argc;
  char *argv[];
{
  char *env;
  env = (char *)getenv("XGRABSC");
  xgrabsc(argc, argv, env);
}
#endif
