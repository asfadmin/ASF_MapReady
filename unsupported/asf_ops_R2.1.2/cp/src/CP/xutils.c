static char sccsid_xutils_c[] = "@(#)xutils.c	1.11 96/08/08 09:25:57";

#define GO_COLOR "#c0dfcf"  /* color used to denote "listening" state */
#define STOP_COLOR "#fdd9d9" /* color used to denote "not listening" state */

#include <stdio.h>   
#include <assert.h>   
#include <sys/stat.h>
#include "utils.h"

#include <Xm/Xm.h>
#include <X11/Shell.h> /* for XmNiconic */
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/MwmUtil.h>
#include "UxXt.h"

extern Widget UxTopLevel;


static char *event_names[] = {
   "",
   "",
   "KeyPress",
   "KeyRelease",
   "ButtonPress",
   "ButtonRelease",
   "MotionNotify",
   "EnterNotify",
   "LeaveNotify",
   "FocusIn",
   "FocusOut",
   "KeymapNotify",
   "Expose",
   "GraphicsExpose",
   "NoExpose",
   "VisibilityNotify",
   "CreateNotify",
   "DestroyNotify",
   "UnmapNotify",
   "MapNotify",
   "MapRequest",
   "ReparentNotify",
   "ConfigureNotify",
   "ConfigureRequest",
   "GravityNotify",
   "ResizeRequest",
   "CirculateNotify",
   "CirculateRequest",
   "PropertyNotify",
   "SelectionClear",
   "SelectionRequest",
   "SelectionNotify",
   "ColormapNotify",
   "ClientMessage",
   "MappingNotify"
};

void printEvent(event)
XEvent(event);
{
   printf("event %s\n", event_names[event.type]);
}

void configure(Widget shell, XtPointer client_data, XEvent *event)
{
    XConfigureEvent *cevent = (XConfigureEvent *) event;
printf("********* inside configure\n");

    if (cevent->type != ConfigureNotify)
        return;
    printf ("Width = %d, Height = %d\n", cevent->width, cevent->height);
    XtVaSetValues (shell,
        XmNminWidth, cevent->width,
        XmNminHeight, cevent->height,
        NULL);
/*    XtRemoveEventHandler (shell, StructureNotifyMask, False, configure, NULL); */
}

void sendEvent(Widget w, int type)
{
  long mask=0;
    Dimension wid, ht;
    XConfigureEvent *cevent = malloc(sizeof (XConfigureEvent *) ) ;
printf("********* inside sendEvent\n");

  if (type != ConfigureNotify)
    return;

  XtVaGetValues(w, XmNwidth, &wid, XmNheight, &ht, NULL);
  printf ("Width = %d, Height = %d\n", wid, ht);
  cevent->type = type;
  cevent->width = wid;
  cevent->height = ht;

  XSendEvent(XtDisplay(UxTopLevel), XtWindow(w), FALSE, mask, (XEvent *) cevent); 
}

Widget findParentShell(Widget child)
{
  Widget w;

  for (w=child; !XtIsShell(w); w = XtParent(w))
    ;

  return(w);
}

void ForceUpdate(Widget w)
{
  Widget diashell, topshell;
  Window diawindow, topwindow;
  XtAppContext cxt = XtWidgetToApplicationContext(w);
  Display *dpy;
  XWindowAttributes xwa;
  XEvent event;

/*
  for (diashell=w; !XtIsShell(diashell); diashell = XtParent(diashell))
    ;
*/
  diashell = findParentShell(w);

  for (topshell=diashell; !XtIsTopLevelShell(topshell); 
         topshell = XtParent(topshell))
    ;

  if (XtIsRealized(diashell) && XtIsRealized(topshell)) {
    dpy = XtDisplay(topshell);
    diawindow = XtWindow(diashell);
    topwindow = XtWindow(topshell);
    while (XGetWindowAttributes(dpy, diawindow, &xwa) &&
           xwa.map_state != IsViewable) {
      if (XGetWindowAttributes(dpy, topwindow, &xwa) &&
           xwa.map_state != IsViewable)
        break;
      XtAppNextEvent(cxt, &event);
      XtDispatchEvent(&event);
    }
  }
  XmUpdateDisplay(topshell);

}

int iconifyWindow(Widget w)
{
  Boolean isIconic;

  assert(XtIsShell(w));

  if (XtIsTopLevelShell(w)) {
    XtVaGetValues(w, XmNiconic, &isIconic, NULL);
    XtVaSetValues(w, XmNiconic, TRUE, NULL);
    XtVaGetValues(w, XmNiconic, &isIconic, NULL);
  }
  else if (XtIsShell(w)) {
    XtPopdown(w);
  }

  return(1);
}

/*----------------------------------------------------------
 * NAME:
 *  makePosVisible
 *
 * DESCRIPTION:
 *  make the desired list position visible in the specified scrolled list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void makePosVisible(Widget listW, int listPos)
{
  int top, visible;

  XtVaGetValues(listW, XmNtopItemPosition, &top, 
                       XmNvisibleItemCount, &visible, NULL);
/* we get a core dump if this printf isn't here. 
   nothing is obvious at the moment, so let's leave the printf in. */
/*printf(" "); */
/*printf("makePosVisible: %s pos %d top is %d visible %d\n", 
XtName(listW), listPos, top, visible); */
  if (listPos < top)
    XmListSetPos(listW, listPos);
  else  if (top+visible <= listPos)
    XmListSetBottomPos(listW, listPos); 

} /* makePosVisible */


/*----------------------------------------------------------
 * NAME:
 *  loadFile
 *
 * DESCRIPTION:
 *  load the contents of a file into the specified text widget
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void loadFile(Widget textW, char *filename)
{
  char *text;

  text = readFile(filename);
  XmTextSetString(textW, text);
  XtFree(text);

} /* loadFile */


/*----------------------------------------------------------
 * NAME:
 *  queryPointer
 *
 * DESCRIPTION:
 *  query the pointer's current x,y position and return these values
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void queryPointer(int *x, int *y)
{
   Display *display;
   int screen;
   Window root_return, child_return;
   int root_x_return, root_y_return;
   int win_x_return, win_y_return;
   unsigned int mask_return;

   display = XOpenDisplay(NULL);
   screen = DefaultScreen(display);
   XQueryPointer(display, RootWindow(display,screen),
          &root_return, &child_return, &root_x_return, &root_y_return,
          &win_x_return, &win_y_return, &mask_return);

   *x = root_x_return;
   *y = root_y_return;
   XCloseDisplay(display); 
}


/*----------------------------------------------------------
 * NAME:
 *  popupMsgDialog
 *
 * DESCRIPTION:
 *  popup a message dialog with the requested features
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
Widget popupMsgDialog(Widget w, char *text, char *title, XtPointer ok_fn, 
   XtPointer ok_data, XtPointer cancel_fn, XtPointer cancel_data, 
   int new_loc)
{
   Widget msgDialog = NULL;
   int x, y, disp_x, disp_y;
   static int last_x=0, last_y=0;
   XmString xmStr, titleXmStr[1];
     
   xmStr = XmStringCreateLtoR(text, XmFONTLIST_DEFAULT_TAG);
   titleXmStr[0] = XmStringCreateLocalized(title);
   
   queryPointer(&x, &y);  
   disp_x = (last_x == x) ? x+100 : x;
   disp_y = (last_y == y) ? y+100 : y;
   msgDialog = XmCreateMessageDialog(w, "", NULL, NULL);
     XtVaSetValues(msgDialog, XmNmessageString, xmStr,
      XmNdialogTitle, titleXmStr[0], 
      XmNdefaultPosition, FALSE,
      XmNdeleteResponse, XmDESTROY,
      XmNx, disp_x, 
      XmNy, disp_y,
      NULL);
   XtVaSetValues(XtParent(msgDialog), 
                  XmNdeleteResponse, XmNONE,
                  XmNmwmFunctions, MWM_FUNC_MOVE + MWM_FUNC_RESIZE,
                  XmNmwmDecorations, MWM_DECOR_ALL + MWM_DECOR_MINIMIZE,
                  NULL);
   last_x = x;
   last_y = y;

   XtUnmanageChild(XmMessageBoxGetChild(msgDialog, XmDIALOG_HELP_BUTTON));
   if (ok_fn)
      XtAddCallback(msgDialog, XmNokCallback, ok_fn, ok_data);
   if (cancel_fn)
      XtAddCallback(msgDialog, XmNcancelCallback, cancel_fn, cancel_data);
   /* if (ok_data)
      XtAddCallback(msgDialog, XmNpopdownCallback, mstrq_free, ok_data); */
   XmStringFree(xmStr); 
   XmStringFree(titleXmStr[0]);
   XtManageChild(msgDialog);
   return(msgDialog);
}



/*----------------------------------------------------------
 * NAME:
 *  popdownMsgDialog
 *
 * DESCRIPTION:
 *  popdown a dialog denoted by a widget
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void popdownMsgDialog(Widget w)
{
  if (w)
    UxPopdownInterface(w);
}


/*----------------------------------------------------------
 * NAME:
 *  display_msg
 *
 * DESCRIPTION:
 *  display a message by popping up a message dialog
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

Widget display_msg(msg, title, ok_fn, ok_data, cancel_fn, cancel_data, new_loc)
char *msg, *title;
int (*ok_fn) ();
char * ok_data;
int (*cancel_fn) ();
char * cancel_data;
int new_loc;
{
   Widget w = NULL;
   if (UxTopLevel)
      w = popupMsgDialog(UxTopLevel, msg, title, ok_fn, ok_data, cancel_fn, 
	 cancel_data, new_loc);
   else
      printf("%s", msg);
   return(w);
}


/*----------------------------------------------------------
 * NAME:
 *  setColor
 *
 * DESCRIPTION:
 *  set the background pixel value of a specified widget.  
 *  if desired, set the shadow colors to match the new background
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void setColor(Widget w, Pixel pixelValue, int doShadows)
{
   Pixel fg=1, bg, top_shadow=1, bottom_shadow=1, select_color=1;
   Colormap cmap;

   XtVaSetValues(w, XmNbackground, pixelValue, NULL);
   if (doShadows) {
      XtVaGetValues(w, XmNbackground, &bg, XmNcolormap, &cmap, NULL);
      XmGetColors(XtScreen(w), cmap, bg, &fg, &top_shadow,
         &bottom_shadow, &select_color);

      XtVaSetValues(w, XmNtopShadowColor, top_shadow,
         XmNbottomShadowColor, bottom_shadow, XmNarmColor, select_color,
         XmNborderColor, fg, NULL);
   }
} /* setColor */


/*----------------------------------------------------------
 * NAME:
 *  setColorStr
 *
 * DESCRIPTION:
 *  set the background color of a specified widget to the value
 *  denoted by the string parameter.
 *  also, set the shadow colors to match the new background
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void setColorStr(Widget w, char *colorString)
{
   Pixel fg, bg, top_shadow, bottom_shadow, select_color;
   Colormap cmap;

   XtVaSetValues(w, XtVaTypedArg, XmNbackground, XmRString,
      colorString, strlen(colorString) + 1, NULL);
   XtVaGetValues(w, XmNbackground, &bg,
      XmNcolormap, &cmap, NULL);
   XmGetColors(XtScreen(w), cmap, bg, &fg, &top_shadow,
      &bottom_shadow, &select_color);
   XtVaSetValues(w, XmNtopShadowColor, top_shadow,
      XmNbottomShadowColor, bottom_shadow, XmNarmColor, select_color,
      XmNborderColor, fg, NULL);
}

/*----------------------------------------------------------
 * NAME:
 *  reverseColor
 *
 * DESCRIPTION:
 *  reverse the foreground and background colors of a widget
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void reverseColor(Widget w)
{
   Pixel fg, bg;

   XtVaGetValues(w, XmNbackground, &bg, XmNforeground, &fg, NULL);
   XtVaSetValues(w, XmNbackground, fg, XmNforeground, bg, NULL);
}

/*----------------------------------------------------------
 * NAME:
 *  showListening
 *
 * DESCRIPTION:
 *  show that the specified widget is in "listening" state.
 *  this means we turn the widget to the "GO" color.  if we
 *  desire to show listening in another way,  just change the
 *  resource we want to use to show the listening state.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void showListening(Widget w)   /* turn the widget to "GO" color */
{                              /* if a list widget, also turn its parent */
                               /* scrolled window to the color of its parent */
   Pixel bg;

   setColorStr(w, GO_COLOR);
   if (XmIsList(w))
     if (XmIsScrolledWindow(XtParent(w)) ) { /* set window bg to parent's bg */
       XtVaGetValues(XtParent(XtParent(w)), XmNbackground, &bg, NULL);
      setColor(XtParent(w), bg, FALSE);
      }


} /* showListening */

/*----------------------------------------------------------
 * NAME:
 *  showNotListening
 *
 * DESCRIPTION:
 *  show that the specified widget is in "not listening" state.
 *  this means we turn the widget to the "STOP" color.  if we
 *  desire to show "not listening" in another way,  just change the
 *  resource we want to use to show the "not listening" state.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void showNotListening(Widget w) /* turn the widget to "STOP" color */
{                               /* if a list widget, also turn its parent */
                                /* scrolled window to the color of its parent */
   Pixel bg;   

   setColorStr(w, STOP_COLOR);
   if (XmIsList(w))
     if (XmIsScrolledWindow(XtParent(w)) ) { /* set window bg to parent's bg */
       XtVaGetValues(XtParent(XtParent(w)), XmNbackground, &bg, NULL);  
      setColor(XtParent(w), bg, FALSE); 
      }

} /* showNotListening */


/*----------------------------------------------------------
 * NAME:
 *  colorToPixel
 *
 * DESCRIPTION:
 *  return the Pixel value for a color string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

Pixel colorToPixel(char *color_string)
{                               
    XrmValue fromVal, toVal;
    Pixel color;

#ifdef ZZZ
  XColor exact_def_return, screen_def_return;

  XLookupColor(XtDisplay(UxTopLevel), 
               DefaultColormap(XtDisplay(UxTopLevel), 0), /* pick screen # 0 */
               color_string,
               &exact_def_return, &screen_def_return);

printf("colorToPixel color for %s returned 0x%x\n", color_string, exact_def_return);
  return(exact_def_return.pixel);

#else
    /*
     * Attempt to convert the string in this cell to a Pixel
     */
    fromVal.size = strlen(color_string) + 1;
    fromVal.addr = color_string;
    toVal.size = sizeof(Pixel);
    toVal.addr = (caddr_t) &color;
    if (!XtConvertAndStore(UxTopLevel, XtRString, &fromVal, XtRPixel, &toVal))
        return((Pixel) 0);

printf("colorToPixel color for %s returned 0x%x\n", color_string, color);
   return(color);  
#endif


} /* colorToPixel */


/*----------------------------------------------------------
 * NAME:
 *  addItemToMenu
 *
 * DESCRIPTION:
 *  add item to menu
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void addItemToMenu(Widget pane, char *name)
{
  Widget pb;

  assert(XmIsRowColumn(pane));

  pb = XtVaCreateManagedWidget( "pb",
                        xmPushButtonGadgetClass,
                        pane,
                        RES_CONVERT( XmNlabelString, name ),
                        NULL );
  
}
