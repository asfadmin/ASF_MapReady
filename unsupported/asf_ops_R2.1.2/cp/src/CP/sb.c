#define SCROLL_FLAG FALSE  /* for XmScrollbarSetValues: notify */

/* #define SB_DEBUG /* */

#include <stdio.h>
#include <sys/stat.h>
#include <assert.h>

static char sccsid_sb_c[] = "@(#)sb.c	1.11 96/08/08 09:24:37";

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include "UxXt.h"

#include "odl.h"

#include "cpdefines.h"
#include "cpworkProc.h"
#include "listcore.h"
#include "listpid.h"
#include "listmstrq.h"

extern Widget UxTopLevel;
extern Widget CPmainQ;

void hsbValueChanged(Widget w,XtPointer client_data, XtPointer call_data)
{
printf("hsbValueChanged\n");
}

/*----------------------------------------------------------
 * NAME:
 *  dispAllSbInfo
 *
 * DESCRIPTION:
 *  display all scroll bar information for the subsystem denoted by namePtr
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void dispAllSbInfo(char *namePtr)
{
  int i=0, val, slider, incr, page_incr, min, max;
  Widget w[MAX_MAIN_LIST_ITEMS];
  Widget hsb[MAX_MAIN_LIST_ITEMS], vsb[MAX_MAIN_LIST_ITEMS];
  Position v_wid, v_ht;
  Position h_wid, h_ht;

#ifdef SB_DEBUG
printf("dispAllSbInfo for %s\n", namePtr);
#endif

  w[i++] = XtParent(GetJobIdListGivenName_PIDLIST(namePtr));
  w[i++] = XtParent(GetPlatRevSeqListGivenName_PIDLIST(namePtr));
  w[i++] = XtParent(GetFrameListGivenName_PIDLIST(namePtr));
  w[i++] = XtParent(GetMediaListGivenName_PIDLIST(namePtr));
  w[i++] = XtParent(GetModeListGivenName_PIDLIST(namePtr));
  w[i++] = XtParent(GetRequestListGivenName_PIDLIST(namePtr));
  w[i++] = XtParent(GetTypeListGivenName_PIDLIST(namePtr));
  w[i++] = XtParent(GetStatusListGivenName_PIDLIST(namePtr));

  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    XtVaGetValues(w[i], XmNverticalScrollBar, &vsb[i], 
                        XmNhorizontalScrollBar, &hsb[i], 
                        NULL);
    XmScrollBarGetValues(vsb[i], &val, &slider, &incr, &page_incr);
    XtVaGetValues(vsb[i], XmNminimum, &min, XmNmaximum, &max, 
                          XmNwidth, &v_wid, XmNheight, &v_ht,
                          NULL);
    XtVaGetValues(hsb[i], XmNwidth, &h_wid, XmNheight, &h_ht,
                          NULL);
#ifdef SB_DEBUG
printf("%27s: val %d slider %d incr %d, pgincr %d min %d max %d\n", 
XtName(w[i]), val, slider, incr, page_incr, min, max);
printf("\tvsb wid %d ht %d ", v_wid, v_ht);
printf("hsb wid %d ht %d\n", h_wid, h_ht);
#endif
  }
}
/*----------------------------------------------------------
 * NAME:
 *  dispAllListInfo
 *
 * DESCRIPTION:
 *  display all scroll bar information for the subsystem denoted by namePtr
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void dispAllListInfo(char *namePtr)
{
  int i=0;
  Widget w[MAX_MAIN_LIST_ITEMS];
  Position wid, ht;

#ifdef SB_DEBUG
printf("dispAllListInfo for %s\n", namePtr);
#endif

  w[i++] = GetJobIdListGivenName_PIDLIST(namePtr);
  w[i++] = GetPlatRevSeqListGivenName_PIDLIST(namePtr);
  w[i++] = GetFrameListGivenName_PIDLIST(namePtr);
  w[i++] = GetMediaListGivenName_PIDLIST(namePtr);
  w[i++] = GetModeListGivenName_PIDLIST(namePtr);
  w[i++] = GetRequestListGivenName_PIDLIST(namePtr);
  w[i++] = GetTypeListGivenName_PIDLIST(namePtr);
  w[i++] = GetStatusListGivenName_PIDLIST(namePtr);

  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    XtVaGetValues(w[i], XmNwidth, &wid, XmNheight, &ht,
                          NULL);
#ifdef SB_DEBUG
printf("%27s: wid %3d ht %3d\n",
XtName(w[i]), wid, ht);
#endif
  }
}



/*----------------------------------------------------------
 * NAME:
 *  moveScrollBar
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void moveScrollBar(Widget mainWid)
{
  char *namePtr;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);

#ifdef SB_DEBUG
printf("moveScrollBar %s\n", namePtr);
#endif

dispAllSbInfo(namePtr);

} /* end of moveScrollBar */

/*----------------------------------------------------------
 * NAME:
 *  syncLists
 *
 * DESCRIPTION:
 *  callback routine to synchronize all scrolled lists to match this one
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void syncLists(Widget w,XtPointer client_data, XtPointer call_data)
{
  static Widget scrolled_w[MAX_MAIN_LIST_ITEMS], vsb[MAX_MAIN_LIST_ITEMS];
  static Widget list_w[MAX_MAIN_LIST_ITEMS];
  Widget sb, mainWid = (Widget) client_data;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
  int i=0, val, incr, slider, page_incr, topPos;
  char namePtr[500], *subsysNamePtr;
#ifdef SB_DEBUG
  Widget hsb[MAX_MAIN_LIST_ITEMS];
  Position wid, ht;
#endif

  subsysNamePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (subsysNamePtr == NULL)
    strcpy(namePtr , "CP");
  else
    strcpy(namePtr , subsysNamePtr);
#ifdef SB_DEBUG
printf("**********into syncLists\n"); dispAllSbInfo(namePtr); dispAllListInfo(namePtr);
/*printf("syncLists main widget %s subsysName %s using %s\n", 
        XtName(mainWid), subsysNamePtr, namePtr); */
#endif

  list_w[i++] = GetJobIdListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetPlatRevSeqListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetFrameListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetMediaListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetModeListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetRequestListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetTypeListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetStatusListGivenName_PIDLIST(namePtr);

  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    scrolled_w[i] = XtParent(list_w[i]);
    XtVaGetValues(scrolled_w[i], XmNverticalScrollBar, &vsb[i], NULL);
#ifdef SB_DEBUG
/*
    XtVaGetValues(scrolled_w[i], XmNhorizontalScrollBar, &hsb[i], NULL);
    XtVaGetValues(vsb[i], XmNwidth, &wid, XmNheight, &ht, NULL); 
    printf("%s vsb %d width %d ht %d\n", 
           XtName(scrolled_w[i]), i, wid, ht);
    XtVaGetValues(hsb[i], XmNwidth, &wid, XmNheight, &ht, NULL); 
    printf("%s hsb %d width %d ht %d\n", 
            XtName(scrolled_w[i]), i, wid, ht); */
#endif

/*    XtVaSetValues(vsb[i], XmNwidth, 0, NULL); */
  }

                                                 /* first sync the lists */
  assert(XmIsList(w));
  XtVaGetValues(w, XmNtopItemPosition, &topPos, NULL);
  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) 
    XtVaSetValues(list_w[i], XmNtopItemPosition, topPos, NULL);

                                                 /* now sync the scroll bars */
  assert(XmIsScrolledWindow(XtParent(w)));
  XtVaGetValues(XtParent(w), XmNverticalScrollBar, &sb, NULL);
  XmScrollBarGetValues(sb, &val, &slider, &incr, &page_incr);
  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    XtVaGetValues(scrolled_w[i], XmNverticalScrollBar, &sb, NULL);
    /*XmScrollBarSetValues(sb, val, slider, incr, page_incr, SCROLL_FLAG); */
    XmScrollBarSetValues(sb, val, slider, 0, 0, SCROLL_FLAG);
  }

                                                 /* select the current line */
    selectLine((Widget) client_data, cbs->item_position); 

} /* syncLists */


/*----------------------------------------------------------
 * NAME:
 *  syncScrollBars
 *
 * DESCRIPTION:
 *  callback routine to synchronize all scroll bars to match this one
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void syncScrollBars(Widget w, XtPointer client_data, XtPointer cb_arg)
{
  static Widget scrolled_w[MAX_MAIN_LIST_ITEMS], vsb[MAX_MAIN_LIST_ITEMS];
  static Widget list_w[MAX_MAIN_LIST_ITEMS];
  Widget sb, mainWid = (Widget) client_data;
  int i=0, val, incr, slider, page_incr, topPos;
  char namePtr[500], *subsysNamePtr;

  subsysNamePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (subsysNamePtr == NULL)
    strcpy(namePtr , "CP");
  else
    strcpy(namePtr , subsysNamePtr);
#ifdef SB_DEBUG
printf("**********into syncScrollBars\n"); dispAllSbInfo(namePtr); dispAllListInfo(namePtr);
/*
printf("syncScrollBars widget %s subsysNamePtr %s using namePtr %s\n", XtName(mainWid), subsysNamePtr, namePtr); */
#endif


  list_w[i++] = GetJobIdListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetPlatRevSeqListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetFrameListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetMediaListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetModeListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetRequestListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetTypeListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetStatusListGivenName_PIDLIST(namePtr);

  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    scrolled_w[i] = XtParent(list_w[i]);
    XtVaGetValues(scrolled_w[i], XmNverticalScrollBar, &vsb[i], NULL);
/*    XtVaSetValues(vsb[i], XmNwidth, 0, NULL);  */
  }

  XmScrollBarGetValues(w, &val, &slider, &incr, &page_incr);
  XtVaGetValues(list_w[MAX_MAIN_LIST_ITEMS-1], XmNtopItemPosition, &topPos, NULL);
#ifdef SB_DEBUG
/* printf("sync scrollbars: \n");
printf("sb %20s: val %d slider %d incr %d, page incr %d top pos %d\n", 
XtName(w), val, slider, incr, page_incr, topPos); */
#endif

               /* now set all the other scrollbars to match this one */
  for (i=0; i < MAX_MAIN_LIST_ITEMS-1; i++) {
/* move the scroll bar */
    XtVaGetValues(scrolled_w[i], XmNverticalScrollBar, &sb, NULL);
    assert(XmIsScrollBar(sb));
    /*XmScrollBarSetValues(sb, val, slider, incr, page_incr, SCROLL_FLAG); */
    XmScrollBarSetValues(sb, val, slider, 0, 0, SCROLL_FLAG);
/* move the list widget too */
    assert(XmIsList(list_w[i]));
    XtVaSetValues(list_w[i], XmNtopItemPosition, topPos, NULL);
  }

} /* syncScrollBars */


/*----------------------------------------------------------
 * NAME:
 *  setupScrollBars
 *
 * DESCRIPTION:
 *  initialization routine to set up parallel scroll list info
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void setupScrollBars(char *namePtr)
{
  static Widget scrolled_w[MAX_MAIN_LIST_ITEMS], vsb[MAX_MAIN_LIST_ITEMS];
  static Widget hsb[MAX_MAIN_LIST_ITEMS];
  static Widget list_w[MAX_MAIN_LIST_ITEMS];
  Widget mainWid;
  int i=0;

  mainWid = GetMainWidGivenName_PIDLIST(namePtr);
  if (mainWid == NULL)   /* for main window, null is returned */
    mainWid = CPmainQ;

  list_w[i++] = GetJobIdListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetPlatRevSeqListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetFrameListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetMediaListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetModeListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetRequestListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetTypeListGivenName_PIDLIST(namePtr);
  list_w[i++] = GetStatusListGivenName_PIDLIST(namePtr);

                           /* set mappedWhenManaged to false for all but
                              the rightmost scrollbar so don't have to
                              watch ALL of them scroll (but we do want to
                              see the last one */
  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++)  {
    scrolled_w[i] = XtParent(list_w[i]);
    XtVaGetValues(scrolled_w[i], XmNverticalScrollBar, &vsb[i], NULL);
    XtVaGetValues(scrolled_w[i], XmNhorizontalScrollBar, &hsb[i], NULL);

    if (i < MAX_MAIN_LIST_ITEMS-1) {  /* unmap all vsb but the rightmost */
      XtVaSetValues(vsb[i], XmNmappedWhenManaged, FALSE, NULL);  
      XtVaSetValues(hsb[i], XmNmappedWhenManaged, FALSE, NULL);  
      XtUnmanageChild(hsb[i]); /* and unmanage all hsb */
    }
    else {                  /* make the rightmost one synchronize the others */
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNvalueChangedCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNdragCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNincrementCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNdecrementCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNpageIncrementCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNpageDecrementCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNtoBottomCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);
      XtAddCallback(vsb[MAX_MAIN_LIST_ITEMS-1], XmNtoTopCallback, 
           (XtCallbackProc) syncScrollBars, mainWid);

/* also unmap the rightmost hsb at all times */

      XtVaSetValues(hsb[MAX_MAIN_LIST_ITEMS-1], XmNmappedWhenManaged, FALSE, 
                    XmNshadowThickness, 7,
                    NULL);  
    }
  }
/* this was a horrible failure
/*  syncLists(list_w[0], NULL, NULL);
  syncScrollBars(scrolled_w[0], NULL, NULL); */

} /* setupScrollbars */

