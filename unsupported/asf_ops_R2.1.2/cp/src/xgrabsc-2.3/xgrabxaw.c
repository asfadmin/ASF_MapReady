/*========================================================================
 *
 * Name - xgrabxaw.c
 *
 * Version:	1.16
 *
 * ccsid:	@(#)xgrabxaw.c	1.16 - 06/28/93 09:13:54
 * from: 	ccs/s.xgrabxaw.c
 * date: 	06/28/93 09:14:49
 *
 * Copyright (c) 1990-93 Bruce Schuchardt.
 * Read the file cpyright.h for full copyright information.
 *
 *
 * Description:
 *
 * xgrab.c - interactive front for xgrabsc
 *
 *========================================================================
 */
#include "cpyright.h"
#include "patchlevel.h"
#include "config.h"

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Toggle.h>

static char *fallback_resources[] = {
#include "xgrabxaw_ad.h"
  NULL
  };


static Display *hDisplay;

typedef enum {
  HITWIN = 1,
  STRETCH,
  KEY,
  ROOT,

  XWD,
  XWDXY,
  PS,
  BITMAP,
  BITMAP2,
  BITMAP3,
  PUZZLE,
  SIMPLE,

  DITHER,
  MDITHER,
  HALFTONE,
  MAPBW,

  REVERSE,

  TOFILE,
  TOPRN,

  PS_NORMAL,
  PS_EPS,
  PS_EPSI,
  PS_EPSIONLY,
  PS_LANDSCAPE
  
  } radioType;


Widget selBtns, formatBtns, psColr, encBtns, psComp, sltext, psltext,
       psLim,
       psBin, borderBtn, horizMargin, vertMargin, pgHeight, pgWidth,
       cnvBtns, revBtn, brtext, outBtns, filenm, prncmd, dispBtn, hostnm;
Widget hShell;

XtAppContext appContext;


static char* stradi(ptr, i)    /* Add an integer to a string */
    char* ptr;
    int i; {

    sprintf(ptr + strlen(ptr), "%d", i);
    return ptr;
}

static char* stradw(ptr, s)   /* Add a string to another, with no ws */
    char* ptr;
    char* s; {

    char* ou = ptr + strlen(ptr);

    while (isspace(*s)) {
	++s;
    }
    while (*s && !isspace(*s)) {
	*ou++ = *s++;
    }
    *ou = '\0';

    return ptr;
}


static void doGrab() {
  char tmp[257];
  char cmdargs[200];
  char piper[200];
  char cmd[500];
  char *txt;
  int  s, d, len, num;
  int  sltime, psltime;
  int  bright;
  Boolean state;

  cmdargs[0] = '\0';
  piper[0] = '\0';

  XtVaGetValues(sltext, XtNstring, &txt, NULL);
  sltime = atoi(txt);
  if (sltime < 0) sltime = 0;
  sprintf(tmp, "-sleep %d ", sltime);
  strcat(cmdargs, tmp);

  XtVaGetValues(psltext, XtNstring, &txt, NULL);
  psltime = atoi(txt);
  if (psltime < 0) psltime = 0;
  sprintf(tmp, "-post %d ", psltime);
  strcat(cmdargs, tmp);

  XtVaGetValues(brtext, XtNstring, &txt, NULL);
  bright = atoi(txt);
  sprintf(tmp, "-brighten %d ", bright);
  strcat(cmdargs, tmp);

  XtVaGetValues(borderBtn, XtNstate, &state, NULL);
  if (!state)
    strcat(cmdargs, "-");
  strcat(cmdargs, "-bdrs ");

  XtVaGetValues(revBtn, XtNstate, &state, NULL);
  if (!state)
    strcat(cmdargs, "-");
  strcat(cmdargs, "-reverse ");

  switch ((int)XawToggleGetCurrent(selBtns)) {
    case HITWIN:
      strcat(cmdargs, "-click ");
      break;
    case KEY:
      strcat(cmdargs, "-key ");
      break;
    case ROOT:
      strcat(cmdargs, "-root ");
      break;
    case STRETCH:
    default:
      strcat(cmdargs, "-stretch ");
      break;
  }
  switch ((int)XawToggleGetCurrent(formatBtns)) {
    case XWD:
      strcat(cmdargs, "-xwd ");
      break;
    case XWDXY:
      strcat(cmdargs, "-xwdxy ");
      break;
    case BITMAP:
      strcat(cmdargs, "-bm ");
      break;
    case BITMAP2:
      strcat(cmdargs, "-bm2 ");
      break;
    case BITMAP3:
      strcat(cmdargs, "-bm3 ");
      break;
    case PUZZLE:
      strcat(cmdargs, "-puzzle ");
      break;
    case SIMPLE:
      strcat(cmdargs, "-simple ");
      break;
    case PS:
    default:
      XtVaGetValues(psColr, XtNstate, &state, NULL);
      if (!state)
        strcat(cmdargs, "-ps ");
      else
        strcat(cmdargs, "-cps ");

      switch ((int)XawToggleGetCurrent(encBtns)) {
        case PS_EPS:
          strcat(cmdargs, "-eps ");
	  break;
	case PS_EPSI:
	  strcat(cmdargs, "-preview ");
	  break;
	case PS_EPSIONLY:
	  strcat(cmdargs, "-previewonly ");
	  break;
	case PS_LANDSCAPE:
	  strcat(cmdargs, "-landscape ");
	  break;
	default:
	  break;
      }

      XtVaGetValues(psComp, XtNstate, &state, NULL);
      if (!state)
        strcat(cmdargs, "-");
      strcat(cmdargs, "-compress ");
      XtVaGetValues(psBin, XtNstate, &state, NULL);
      if (!state)
        strcat(cmdargs, "-");
      strcat(cmdargs, "-bin ");

      XtVaGetValues(psLim, XtNstate, &state, NULL);
      if (!state)
        strcat(cmdargs, "-");
      strcat(cmdargs, "-limit ");

      strcat(cmdargs, "-page ");
      XtVaGetValues(pgWidth, XtNstring, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, PAPER_WIDTH);
      strcat(cmdargs, "x");
      XtVaGetValues(pgHeight, XtNstring, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, PAPER_HEIGHT);
      strcat(cmdargs, "-");
      XtVaGetValues(horizMargin, XtNstring, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, HORIZ_MARGIN);
      strcat(cmdargs, "-");
      XtVaGetValues(horizMargin, XtNstring, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, VERT_MARGIN);
      strcat(cmdargs, " ");

      break;
  }
  switch ((int)XawToggleGetCurrent(cnvBtns)) {
    case DITHER:
      strcat(cmdargs, "-dither ");
      break;
    case MDITHER:
      strcat(cmdargs, "-mdither ");
      break;
    case HALFTONE:
      strcat(cmdargs, "-halftone ");
      break;
    case MAPBW:
      strcat(cmdargs, "-bw ");
      break;
    default:
      break;
  }
  switch ((int)XawToggleGetCurrent(outBtns)) {
    case TOPRN:
      XtVaGetValues(prncmd, XtNstring, &txt, NULL);
      if (strlen(txt)) {
        strcat(piper, " | ");
        strcat(piper, txt);
      }
      break;
    case TOFILE:
    default:
      strcat(cmdargs, "-o ");
      XtVaGetValues(filenm, XtNstring, &txt, NULL);
      if (strlen(txt)) {
        strcat(cmdargs, txt);
#ifdef BUMP_FILENAMES
	len = strlen(txt);
	for (s=0; s<len && (txt[s] != '.'); s++)
	{}
	strcpy(tmp, txt);
	tmp[s] = '\0';
	d = s;
	while (d > 0  &&  ('0' <= txt[d-1]  &&  txt[d-1] <= '9'))
	  d--;
	if ('0' <= txt[d]  && txt[d] <= '9')
	  num = atoi(&tmp[d]);
	else
	  num = 0;
	num++;
	sprintf(&tmp[d], "%d", num);
	strcat(tmp, &txt[s]);
	XtVaSetValues(filenm, XtNstring, tmp, NULL);
#endif
      }
      else {
        strcat(cmdargs, "screen.dmp");
	XtVaSetValues(filenm, XtNstring, "screen.dmp", NULL);
      }
      strcat(cmdargs, " ");
      break;
  }

  XtVaGetValues(dispBtn, XtNstate, &state, NULL);
  if (state) {
    XtVaGetValues(hostnm, XtNstring, &txt, NULL);
    strcat(cmdargs, "-d ");
    if (strlen(txt)) {
      strcat(cmdargs, txt);
      strcat(cmdargs, " ");
    }
    else
      strcat(cmdargs, ":0 ");
  }

  sprintf(cmd, "xgrabsc %s%s\n", cmdargs, piper);
  /* fputs(cmd, stderr); */
  /* try to make the window iconic */
  XtUnmapWidget(hShell);
  XSync(hDisplay, False);
  system(cmd);
  XtMapWidget(hShell);
}




static void doDismiss() {
  if (hShell) {
    XtDestroyWidget(hShell);
    hShell = NULL;
    exit(0);
  }
}




static void createWindow() {
  Widget dialog, button, box1, box2, box3, box4, box5, box6;

  dialog = XtVaCreateManagedWidget("dialog", formWidgetClass, hShell,
           NULL);
           XtVaCreateManagedWidget("title",  labelWidgetClass, dialog,
           NULL);


  /* selection options */
  /* input options */
  box1 = XtVaCreateManagedWidget("box1", formWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("inputLbl", labelWidgetClass, box1,
	   NULL);

  dispBtn= XtVaCreateManagedWidget("host", toggleWidgetClass, box1,
	   NULL);

  hostnm = XtVaCreateManagedWidget("hostText", asciiTextWidgetClass, box1,
           XtNeditType, XawtextEdit, NULL);

  if (getenv("DISPLAY"))
    XtVaSetValues(hostnm, XtNstring, getenv("DISPLAY"), NULL);

  selBtns =
           XtVaCreateManagedWidget("click", toggleWidgetClass, box1,
           XtNradioData, HITWIN,
	   NULL);

           XtVaCreateManagedWidget("stretch", toggleWidgetClass, box1,
           XtNradioData, STRETCH,
	   XtNradioGroup, selBtns,
	   NULL);

           XtVaCreateManagedWidget("key", toggleWidgetClass, box1,
           XtNradioData, KEY,
	   XtNradioGroup, selBtns,
	   NULL);

           XtVaCreateManagedWidget("root", toggleWidgetClass, box1,
           XtNradioData, ROOT,
	   XtNradioGroup, selBtns,
	   NULL);

           XtVaCreateManagedWidget("sleeplbl", labelWidgetClass, box1,
           NULL);

  sltext = XtVaCreateManagedWidget("sleeptime", asciiTextWidgetClass, box1,
           XtNeditType, XawtextEdit, NULL);

           XtVaCreateManagedWidget("psleeplbl", labelWidgetClass, box1,
           NULL);

  psltext = XtVaCreateManagedWidget("psleeptime", asciiTextWidgetClass, box1,
           XtNeditType, XawtextEdit, NULL);

  box2 = XtVaCreateManagedWidget("box2", formWidgetClass, dialog, NULL);

  /* output options */
           XtVaCreateManagedWidget("outputFormat", labelWidgetClass, box2,
           NULL);

  formatBtns =
           XtVaCreateManagedWidget("xwd", toggleWidgetClass, box2,
           XtNradioData, XWD,
	   NULL);

           XtVaCreateManagedWidget("ps", toggleWidgetClass, box2,
           XtNradioData, PS,
	   XtNradioGroup, formatBtns,
	   NULL);

           XtVaCreateManagedWidget("puzzle", toggleWidgetClass, box2,
           XtNradioData, PUZZLE,
	   XtNradioGroup, formatBtns,
	   NULL);


           XtVaCreateManagedWidget("xyxwd", toggleWidgetClass, box2,
           XtNradioData, XWDXY,
	   XtNradioGroup, formatBtns,
	   NULL);

           XtVaCreateManagedWidget("xpm", toggleWidgetClass, box2,
           XtNradioData, BITMAP,
	   XtNradioGroup, formatBtns,
	   NULL);

           XtVaCreateManagedWidget("xpm2", toggleWidgetClass, box2,
           XtNradioData, BITMAP2,
	   XtNradioGroup, formatBtns,
	   NULL);

           XtVaCreateManagedWidget("xpm3", toggleWidgetClass, box2,
           XtNradioData, BITMAP3,
	   XtNradioGroup, formatBtns,
	   NULL);


  /* postscript options */
  box3 = XtVaCreateManagedWidget("box3", formWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("psOptions", labelWidgetClass, box3,
           NULL);

  psComp = XtVaCreateManagedWidget("compress", toggleWidgetClass, box3,
	   NULL);

  psColr = XtVaCreateManagedWidget("color", toggleWidgetClass, box3,
	   NULL);

  psBin = XtVaCreateManagedWidget("binary", toggleWidgetClass, box3,
	   NULL);

  psLim = XtVaCreateManagedWidget("limit", toggleWidgetClass, box3,
	   NULL);

  encBtns= XtVaCreateManagedWidget("landscape", toggleWidgetClass, box3,
           XtNradioData, PS_LANDSCAPE,
	   NULL);

           XtVaCreateManagedWidget("epsi", toggleWidgetClass, box3,
           XtNradioData, PS_EPSI,
	   XtNradioGroup, encBtns,
	   NULL);

           XtVaCreateManagedWidget("onlyEpsi", toggleWidgetClass, box3,
           XtNradioData, PS_EPSIONLY,
	   XtNradioGroup, encBtns,
	   NULL);

/*------------
           XtVaCreateManagedWidget("encap", toggleWidgetClass, box3,
           XtNradioData, PS_EPS,
	   XtNradioGroup, encBtns,
	   NULL);
--------------*/


           XtVaCreateManagedWidget("pageWidth", labelWidgetClass, box3,
           NULL);

  pgWidth = XtVaCreateManagedWidget("pageWidthText",asciiTextWidgetClass, box3,
           XtNeditType, XawtextEdit, NULL);

           XtVaCreateManagedWidget("pageHeight", labelWidgetClass, box3,
           NULL);

  pgHeight = XtVaCreateManagedWidget("pageHeightText",asciiTextWidgetClass, box3,
           XtNeditType, XawtextEdit, NULL);

           XtVaCreateManagedWidget("horizMargin", labelWidgetClass, box3,
           NULL);

  horizMargin = XtVaCreateManagedWidget("horizMarginText",asciiTextWidgetClass, box3,
           XtNeditType, XawtextEdit, NULL);

           XtVaCreateManagedWidget("vertMargin", labelWidgetClass, box3,
           NULL);

  vertMargin = XtVaCreateManagedWidget("vertMarginText",asciiTextWidgetClass, box3,
           XtNeditType, XawtextEdit, NULL);


  /* image processing options */
  box4 = XtVaCreateManagedWidget("box4", formWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("prOptions", labelWidgetClass, box4,
           NULL);
  cnvBtns= XtVaCreateManagedWidget("dither", toggleWidgetClass, box4,
           XtNradioData, DITHER,
	   NULL);
           XtVaCreateManagedWidget("mdither", toggleWidgetClass, box4,
           XtNradioData, MDITHER,
	   XtNradioGroup, cnvBtns,
	   NULL);
           XtVaCreateManagedWidget("halftone", toggleWidgetClass, box4,
           XtNradioData, HALFTONE,
	   XtNradioGroup, cnvBtns,
	   NULL);
           XtVaCreateManagedWidget("mapbw", toggleWidgetClass, box4,
           XtNradioData, MAPBW,
	   XtNradioGroup, cnvBtns,
	   NULL);

           XtVaCreateManagedWidget("brightnessLbl", labelWidgetClass, box4,
	   NULL);
  brtext = XtVaCreateManagedWidget("brightnessText", asciiTextWidgetClass, box4,
           XtNeditType, XawtextEdit, NULL);

  revBtn = XtVaCreateManagedWidget("reverse", toggleWidgetClass, box4, NULL);

  borderBtn = XtVaCreateManagedWidget("borders", toggleWidgetClass, box4, NULL);

  box5 = XtVaCreateManagedWidget("box5", formWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("outputLbl", labelWidgetClass, box5,
	   NULL);

  outBtns= XtVaCreateManagedWidget("file", toggleWidgetClass, box5,
           XtNradioData, TOFILE,
	   NULL);

  filenm = XtVaCreateManagedWidget("fileText", asciiTextWidgetClass, box5,
           XtNeditType, XawtextEdit, NULL);

           XtVaCreateManagedWidget("printer", toggleWidgetClass, box5,
           XtNradioData, TOPRN,
	   XtNradioGroup, outBtns,
	   NULL);

  prncmd = XtVaCreateManagedWidget("printerText", asciiTextWidgetClass, box5,
           XtNeditType, XawtextEdit, NULL);


  box6 = XtVaCreateManagedWidget("box6", formWidgetClass, dialog, NULL);

  XtVaCreateManagedWidget("splat", labelWidgetClass, box6, NULL);
  
  button = XtVaCreateManagedWidget("OK", commandWidgetClass, box6, NULL);
  XtAddCallback(button, XtNcallback, doGrab, NULL);

  button = XtVaCreateManagedWidget("Dismiss", commandWidgetClass, box6, NULL);
  XtAddCallback(button, XtNcallback, doDismiss, NULL);

}



int main(argc, argv, envp)
  int argc;
  char *argv[];
  char *envp;
{
  puts("X-Windows Screen Grabber");
  puts(Copyright);
  puts("");


  hShell = XtAppInitialize(&appContext, "XGrab", NULL, 0, &argc, argv,
     fallback_resources, NULL, 0);

  hDisplay = XtDisplay(hShell);
  createWindow();

  XtRealizeWidget(hShell);
  XtAppMainLoop(appContext);
}
