/*========================================================================
 *
 * Name - xgrabxm.c
 *
 * Version:	1.5
 *
 * ccsid:	@(#)xgrabxm.c	1.5 -06/28/93 09:13:55
 * from: 	ccs/s.xgrabxm.c
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
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>

static char *fallback_resources[] = {
#include "xgrabxm_ad.h"
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

  NO_DITHER,
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

radioType encValue = PS_NORMAL;
radioType cnvValue = NO_DITHER;
radioType selValue = STRETCH;
radioType fmtValue = PS;
radioType outValue = TOFILE;



void encValueChanged(widget, newValue, tag)
  Widget widget;
  void *tag;
  radioType newValue;
{
  if (newValue == encValue) /* turning it off */
    encValue = PS_NORMAL;
  else
    encValue = newValue;
}


void cnvValueChanged(widget, newValue, tag)
  Widget widget;
  void *tag;
  radioType newValue;
{
  if (newValue == cnvValue) /* turning it off */
    cnvValue = NO_DITHER;
  else
    cnvValue = newValue;
}


void selValueChanged(widget, newValue, tag)
  Widget widget;
  void *tag;
  radioType newValue;
{
  radioType oldValue = selValue;
  
  if (newValue == selValue) /* turning it off */
    selValue = STRETCH;
  else
    selValue = newValue;
#ifdef DEBUGGING
  fprintf(stderr, "old sel value = %d   new = %d\n", oldValue, selValue);
#endif
}

void fmtValueChanged(widget, newValue, tag)
  Widget widget;
  void *tag;
  radioType newValue;
{
  if (newValue == fmtValue) /* turning it off */
    fmtValue = PS;
  else
    fmtValue = newValue;
}

void outValueChanged(widget, newValue, tag)
  Widget widget;
  void *tag;
  radioType newValue;
{
  if (newValue == encValue) /* turning it off */
    outValue = TOFILE;
  else
    outValue = newValue;
}


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

  XtVaGetValues(sltext, XmNvalue, &txt, NULL);
  sltime = atoi(txt);
  if (sltime < 0) sltime = 0;
  sprintf(tmp, "-sleep %d ", sltime);
  strcat(cmdargs, tmp);

  XtVaGetValues(psltext, XmNvalue, &txt, NULL);
  psltime = atoi(txt);
  if (psltime < 0) psltime = 0;
  sprintf(tmp, "-post %d ", psltime);
  strcat(cmdargs, tmp);

  XtVaGetValues(brtext, XmNvalue, &txt, NULL);
  bright = atoi(txt);
  sprintf(tmp, "-brighten %d ", bright);
  strcat(cmdargs, tmp);

  XtVaGetValues(borderBtn, XmNset, &state, NULL);
  if (!state)
    strcat(cmdargs, "-");
  strcat(cmdargs, "-bdrs ");

  XtVaGetValues(revBtn, XmNset, &state, NULL);
  if (!state)
    strcat(cmdargs, "-");
  strcat(cmdargs, "-reverse ");

  switch (selValue) {
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
  switch (fmtValue) {
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
      XtVaGetValues(psColr, XmNset, &state, NULL);
      if (!state)
        strcat(cmdargs, "-ps ");
      else
        strcat(cmdargs, "-cps ");

      switch (encValue) {
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

      XtVaGetValues(psComp, XmNset, &state, NULL);
      if (!state)
        strcat(cmdargs, "-");
      strcat(cmdargs, "-compress ");
      XtVaGetValues(psBin, XmNset, &state, NULL);
      if (!state)
        strcat(cmdargs, "-");
      strcat(cmdargs, "-bin ");

      XtVaGetValues(psLim, XmNset, &state, NULL);
      if (!state)
        strcat(cmdargs, "-");
      strcat(cmdargs, "-limit ");

      strcat(cmdargs, "-page ");
      XtVaGetValues(pgWidth, XmNvalue, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, PAPER_WIDTH);
      strcat(cmdargs, "x");
      XtVaGetValues(pgHeight, XmNvalue, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, PAPER_HEIGHT);
      strcat(cmdargs, "-");
      XtVaGetValues(horizMargin, XmNvalue, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, HORIZ_MARGIN);
      strcat(cmdargs, "-");
      XtVaGetValues(horizMargin, XmNvalue, &txt, NULL);
      if (strlen(txt))    stradw(cmdargs, txt);
      else                stradi(cmdargs, VERT_MARGIN);
      strcat(cmdargs, " ");

      break;
  }
  switch (cnvValue) {
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
  switch (outValue) {
    case TOPRN:
      XtVaGetValues(prncmd, XmNvalue, &txt, NULL);
      if (strlen(txt)) {
        strcat(piper, " | ");
        strcat(piper, txt);
      }
      break;
    case TOFILE:
    default:
      strcat(cmdargs, "-o ");
      XtVaGetValues(filenm, XmNvalue, &txt, NULL);
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
	XtVaSetValues(filenm, XmNvalue, tmp, NULL);
#endif
      }
      else {
        strcat(cmdargs, "screen.dmp");
	XtVaSetValues(filenm, XmNvalue, "screen.dmp", NULL);
      }
      strcat(cmdargs, " ");
      break;
  }

  XtVaGetValues(dispBtn, XmNset, &state, NULL);
  if (state) {
    XtVaGetValues(hostnm, XmNvalue, &txt, NULL);
    strcat(cmdargs, "-d ");
    if (strlen(txt)) {
      strcat(cmdargs, txt);
      strcat(cmdargs, " ");
    }
    else
      strcat(cmdargs, ":0 ");
  }

  sprintf(cmd, "xgrabsc %s%s\n", cmdargs, piper);
#ifdef DEBUGGING
  fputs(cmd, stderr);
#else
  /* try to make the window iconic */
  XtUnmapWidget(hShell);
  XSync(hDisplay, False);
  system(cmd);
  XtMapWidget(hShell);
#endif
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

  dialog = XtVaCreateManagedWidget("dialog", xmFormWidgetClass, hShell,
           NULL);
           XtVaCreateManagedWidget("title",  xmLabelWidgetClass, dialog,
           NULL);


  /* selection options */
  /* input options */
  box1 = XtVaCreateManagedWidget("box1", xmFormWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("inputLbl", xmLabelWidgetClass, box1,
	   NULL);

  dispBtn= XtVaCreateManagedWidget("host", xmToggleButtonWidgetClass, box1,
	   NULL);

  hostnm = XtVaCreateManagedWidget("hostText", xmTextWidgetClass, box1,
           NULL);

  if (getenv("DISPLAY"))
    XtVaSetValues(hostnm, XmNvalue, getenv("DISPLAY"), NULL);

  selBtns = XmCreateRadioBox(box1, "selectiontype", NULL, 0);
           XtManageChild(selBtns);
	   
  button = XtVaCreateManagedWidget("click", xmToggleButtonWidgetClass, selBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, selValueChanged,
	   HITWIN);

  button = XtVaCreateManagedWidget("stretch", xmToggleButtonWidgetClass, selBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, selValueChanged,
	   STRETCH);

  button = XtVaCreateManagedWidget("key", xmToggleButtonWidgetClass, selBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, selValueChanged,
	   KEY);

  button = XtVaCreateManagedWidget("root", xmToggleButtonWidgetClass, selBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, selValueChanged,
	   ROOT);

           XtVaCreateManagedWidget("sleeplbl", xmLabelWidgetClass, box1,
           NULL);

  sltext = XtVaCreateManagedWidget("sleeptime", xmTextWidgetClass, box1,
           NULL);

           XtVaCreateManagedWidget("psleeplbl", xmLabelWidgetClass, box1,
           NULL);

  psltext = XtVaCreateManagedWidget("psleeptime", xmTextWidgetClass, box1,
           NULL);

  box2 = XtVaCreateManagedWidget("box2", xmFormWidgetClass, dialog, NULL);

  /* output options */
           XtVaCreateManagedWidget("outputFormat", xmLabelWidgetClass, box2,
           NULL);

  formatBtns = XmCreateRadioBox(box2, "formattype", NULL, 0);
	   XtManageChild(formatBtns);
	   
  button = XtVaCreateManagedWidget("xwd", xmToggleButtonWidgetClass, formatBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, fmtValueChanged,
	   XWD);

  button = XtVaCreateManagedWidget("xwdxy", xmToggleButtonWidgetClass, formatBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, fmtValueChanged,
	   XWDXY);

  button = XtVaCreateManagedWidget("ps", xmToggleButtonWidgetClass, formatBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, fmtValueChanged,
	   PS);

  button = XtVaCreateManagedWidget("xpm", xmToggleButtonWidgetClass, formatBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, fmtValueChanged,
	   BITMAP);

  button = XtVaCreateManagedWidget("xpm2", xmToggleButtonWidgetClass, formatBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, fmtValueChanged,
	   BITMAP2);

  button = XtVaCreateManagedWidget("xpm3", xmToggleButtonWidgetClass, formatBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, fmtValueChanged,
	   BITMAP3);

  button = XtVaCreateManagedWidget("puzzle", xmToggleButtonWidgetClass, formatBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, fmtValueChanged,
	   PUZZLE);



  /* postscript options */
  box3 = XtVaCreateManagedWidget("box3", xmFormWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("psOptions", xmLabelWidgetClass, box3,
           NULL);

  psComp = XtVaCreateManagedWidget("compress", xmToggleButtonWidgetClass, box3,
	   NULL);

  psColr = XtVaCreateManagedWidget("color", xmToggleButtonWidgetClass, box3,
	   NULL);

           XtVaCreateManagedWidget("pageWidth", xmLabelWidgetClass, box3,
           NULL);

  pgWidth = XtVaCreateManagedWidget("pageWidthText",xmTextWidgetClass, box3,
           NULL);

           XtVaCreateManagedWidget("pageHeight", xmLabelWidgetClass, box3,
           NULL);

  pgHeight = XtVaCreateManagedWidget("pageHeightText",xmTextWidgetClass, box3,
           NULL);

           XtVaCreateManagedWidget("horizMargin", xmLabelWidgetClass, box3,
           NULL);

  horizMargin = XtVaCreateManagedWidget("horizMarginText",xmTextWidgetClass, box3,
           NULL);

           XtVaCreateManagedWidget("vertMargin", xmLabelWidgetClass, box3,
           NULL);

  vertMargin = XtVaCreateManagedWidget("vertMarginText",xmTextWidgetClass, box3,
           NULL);


  encBtns = XmCreateRadioBox(box3, "postscripttype", NULL, 0);
	   XtManageChild(encBtns);
	   
  button = XtVaCreateManagedWidget("portrait", xmToggleButtonWidgetClass, encBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, encValueChanged,
	   PS_NORMAL);

  button = XtVaCreateManagedWidget("landscape", xmToggleButtonWidgetClass, encBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, encValueChanged,
	   PS_LANDSCAPE);

  button = XtVaCreateManagedWidget("epsi", xmToggleButtonWidgetClass, encBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, encValueChanged,
	   PS_EPSI);

/*------------
  button = XtVaCreateManagedWidget("encap", xmToggleButtonWidgetClass, encBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, encValueChanged,
	   PS_EPS);
---------------*/

  button = XtVaCreateManagedWidget("onlyEpsi", xmToggleButtonWidgetClass, encBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, encValueChanged,
	   PS_EPSIONLY);

  psBin = XtVaCreateManagedWidget("binary", xmToggleButtonWidgetClass, box3,
	   NULL);

  psLim = XtVaCreateManagedWidget("limit", xmToggleButtonWidgetClass, box3,
	   NULL);


  /* image processing options */
  box4 = XtVaCreateManagedWidget("box4", xmFormWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("prOptions", xmLabelWidgetClass, box4,
           NULL);

           XtVaCreateManagedWidget("brightnessLbl", xmLabelWidgetClass, box4,
	   NULL);
  brtext = XtVaCreateManagedWidget("brightnessText", xmTextWidgetClass, box4,
           NULL);

  revBtn = XtVaCreateManagedWidget("reverse", xmToggleButtonWidgetClass, box4, NULL);

  borderBtn = XtVaCreateManagedWidget("borders", xmToggleButtonWidgetClass, box4, NULL);


  box5 = XtVaCreateManagedWidget("box5", xmFormWidgetClass, dialog, NULL);

           XtVaCreateManagedWidget("outputLbl", xmLabelWidgetClass, box5,
	   NULL);

  outBtns = XmCreateRadioBox(box5, "outputtype", NULL, 0);
	   XtManageChild(outBtns);


  button = XtVaCreateManagedWidget("file", xmToggleButtonWidgetClass, outBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, outValueChanged,
	   TOFILE);

  filenm = XtVaCreateManagedWidget("fileText", xmTextWidgetClass, box5,
           NULL);

  button = XtVaCreateManagedWidget("printer", xmToggleButtonWidgetClass, outBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, outValueChanged,
	   TOPRN);

  prncmd = XtVaCreateManagedWidget("printerText", xmTextWidgetClass, box5,
           NULL);


  cnvBtns = XmCreateRadioBox(dialog, "converttype", NULL, 0);

           XtVaSetValues(cnvBtns,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
	   XtManageChild(cnvBtns);
	   
  button = XtVaCreateManagedWidget("nodither", xmToggleButtonWidgetClass, cnvBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, cnvValueChanged,
	   NO_DITHER);

  button = XtVaCreateManagedWidget("dither", xmToggleButtonWidgetClass, cnvBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, cnvValueChanged,
	   DITHER);

  button = XtVaCreateManagedWidget("mdither", xmToggleButtonWidgetClass, cnvBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, cnvValueChanged,
	   MDITHER);

  button = XtVaCreateManagedWidget("halftone", xmToggleButtonWidgetClass, cnvBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, cnvValueChanged,
	   HALFTONE);

  button = XtVaCreateManagedWidget("mapbw", xmToggleButtonWidgetClass, cnvBtns,
	   NULL);
           XtAddCallback(button, XmNvalueChangedCallback, cnvValueChanged,
	   MAPBW);

  box6 = XtVaCreateManagedWidget("box6", xmFormWidgetClass, dialog, NULL);

  XtVaCreateManagedWidget("splat", xmLabelWidgetClass, box6, NULL);
  
  button = XtVaCreateManagedWidget("OK", xmPushButtonWidgetClass, box6, NULL);
  XtAddCallback(button, XmNactivateCallback, doGrab, NULL);

  button = XtVaCreateManagedWidget("Dismiss", xmPushButtonWidgetClass, box6, NULL);
  XtAddCallback(button, XmNactivateCallback, doDismiss, NULL);

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
