static char sccsid_cp_copyright_c[] = "@(#)CP_copyright.c	1.3 96/07/29 17:28:26";

#include <Xm/Xm.h>

#include <X11/Shell.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>

#include <Xm/Label.h>

#define RES_CONVERT( res_name, res_value) \
        XtVaTypedArg, (res_name), XmRString, (res_value), strlen(res_value) + 1


XtIntervalId  startupTimer_ID;
#define STARTUP_TIMER_VALUE 5*1000 /* 5 seconds */


#define TOP_MSG "Control Processor (CP) Subsystem"

/*
#define BOTTOM_MSG "Copyright (C) 1996, California Institute of Technology.\nU.S. Government Sponsorship under NASA Contract NAS7-1260 is acknowledged."
*/
#define BOTTOM_MSG "Copyright (C) 1996, California Institute of Technology.\nALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged."


Widget popupStartupBox (Widget toplevel)
{
  Widget CPstartupBox,form, topLabel, bottomLabel;

  CPstartupBox = XtVaCreatePopupShell( "CopyrightInfo", 
           transientShellWidgetClass, toplevel, 
           XmNoverrideRedirect, TRUE,  
           NULL );
  form = XtVaCreateWidget( "form", xmFormWidgetClass, CPstartupBox,
                        XmNresizePolicy, XmRESIZE_GROW,
                        XmNunitType, XmPIXELS,
                        XmNhorizontalSpacing, 10,
                        XmNverticalSpacing, 10,
                        NULL );
  topLabel =  XtVaCreateManagedWidget( "topLabel", xmLabelWidgetClass, form,
          XmNalignment, XmALIGNMENT_CENTER,
          XmNtopAttachment, XmATTACH_FORM,
          XmNleftAttachment, XmATTACH_FORM,
          XmNleftOffset, 20,
          XmNrightAttachment, XmATTACH_FORM,
          XmNrightOffset, 20,
          RES_CONVERT( XmNlabelString, TOP_MSG ),  
          NULL);

  bottomLabel =  XtVaCreateManagedWidget( "bottomLabel", 
          xmLabelWidgetClass, form,
          XmNalignment, XmALIGNMENT_CENTER,
          XmNtopAttachment, XmATTACH_WIDGET,
          XmNtopWidget, topLabel,
          XmNleftAttachment, XmATTACH_FORM,
          XmNleftOffset, 20,
          XmNrightAttachment, XmATTACH_FORM,
          XmNrightOffset, 20,
          RES_CONVERT( XmNlabelString, BOTTOM_MSG ),  
          NULL);


  XtManageChild(form);
  XtPopup(CPstartupBox, XtGrabNone); 
  return(CPstartupBox);

} /* popupStartupBox */

popdownStartupBox(Widget w)
{
  XtRemoveTimeOut(startupTimer_ID);
  XtPopdown(w);
  exit(0);
}

main(int argc, char **argv)
{
    Widget        toplevel, w;
    XtAppContext  app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "CP_copyright", NULL, 0,
        &argc, argv, NULL, NULL);

    w = popupStartupBox(toplevel);
    startupTimer_ID = XtAppAddTimeOut(app, STARTUP_TIMER_VALUE, 
                  (XtTimerCallbackProc)popdownStartupBox, w);

    XtAppMainLoop (app);

}

