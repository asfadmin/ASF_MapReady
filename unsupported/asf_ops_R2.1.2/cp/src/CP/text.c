/* #define CANCEL_BUTTON /* placeholder for adding cancel button to box */

static char sccsid_text_c[] = "@(#)text.c	1.20 97/01/02 08:15:05";

#include <syslog.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/MwmUtil.h>
#include "UxXt.h"

#include "asfcommon.h"
#include "logUtils.h"
#include "memUtils.h"

#include "cpdefines.h"
#include "cplogs.h"
#include "cprtns_xwp.h"
#include "listmstrq.h"
#include "utils.h"
#include "xutils.h"

#include "display.h"
#include "CPmainQ.h"  /* maxQueueSizeReset_pb */


typedef void (*VoidProc)();

#define EXPLANATION_FILE "/tmp/CP_failure_reason.txt"

typedef struct ppsStuff {
  int jobId;
  Widget widget;
  char *subsysName;
} ppsStuff;

extern Widget findParentShell();
extern Widget UxTopLevel;
Widget textBox;

#define isquote(c) (c == '\"' | c == '\'' ? 1 : 0)

void noquotes(text_w, client_data, call_data)
Widget text_w;
XtPointer client_data;
XtPointer call_data;
{
    int len = XmTextGetLastPosition (text_w);
/*    char *label = (char *) client_data; */
    XmTextVerifyCallbackStruct *cbs =
        (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->startPos < cbs->currInsert) /* backspace */
        return;

    for (len = 0; len < cbs->text->length; len++) {
        /* make sure all additions are digits. */
        if (isquote (cbs->text->ptr[len])) {
            /* not a digit-- move all chars down one and
             * decrement cbs->text->length.
             */
            int i;
            for (i = len; (i+1) < cbs->text->length; i++)
                cbs->text->ptr[i] = cbs->text->ptr[i+1];
            cbs->text->length--;
            len--;
        }
    }
    if (cbs->text->length == 0)
        cbs->doit = False;
} /* noquotes */



void TextConfirmCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  ppsStuff *pp = (ppsStuff *) client_data;
  char *text, *productType, *compensated; /* don't free either of these */

 baseListElemType *nPtr;

#ifdef READ_LOCK_DEBUG
printf("TextConfirmCB locking READ_LIST\n"); fflush(stdout);
#endif
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
    return;

    text = XmTextGetString(pp->widget);

    if (strlen(text) == 0) {
      ASFlogMessage(ASF_CP, WP_TEXT_BOX, "%d", pp->jobId);
      unLockNameList_CORE(nPtr);
      return;
    }


    if (GetSubsystemCategoryGivenName(pp->subsysName) == SSP2_CATEGORY_ID) {
      productType = GetProductType_MSTRQLIST(pp->jobId);
      compensated = GetCompensationType_MSTRQLIST(pp->jobId);
  
      if (productType == NULL)
        printfLLog(LOG_ERR, CANNOT_GET, "product type");
      else if (compensated == NULL)
        printfLLog(LOG_ERR, CANNOT_GET, "compensation type");
      else {
        decrementSSP2Time(getSSP2numGivenName(pp->subsysName),
                              getExpectedTime(productType), compensated);
      }
    }

    SetFailureReason_MSTRQLIST(pp->jobId, text);
    if (ChangeStatus__MSTRQLIST(pp->jobId, ASF_CP, Q_M_NEXT_STEP_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);

   doFree(pp);

   UxPopdownInterface(findParentShell(w));
   unLockNameList_CORE(nPtr);
   return;
}

#ifdef CANCEL_BUTTON
void TextCancelCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) call_data;
  ppsStuff *pp = (ppsStuff *) client_data;
  char *subsys = pp->subsysName;
  int jobId = pp->jobId;

  if (ChangeStatus__MSTRQLIST(jobId, subsys, Q_M_PLACED_ON_HOLD_ERROR_ID) == -1)
    printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
  XtFree(pp->subsysName);
}
#endif



void popupTextBox(int jobId, char *subsysName)
{
  ppsStuff *pp = (ppsStuff *) doMalloc(sizeof(ppsStuff));
  int x, y;
  Widget form, howLabel, separator, okButton;
#ifdef CANCEL_BUTTON
  Widget cancelButton;
#endif
  Widget text_w;
  char reason[80];

  queryPointer(&x, &y);

  textBox = XtVaCreatePopupShell( "JobFailureBox",
           xmDialogShellWidgetClass, UxTopLevel, 
           XmNallowShellResize, TRUE, 
           XmNtitle, "Job Failure Explanation",
           XmNx, x, XmNy, y,  
           XmNdefaultPosition, FALSE,  
           XmNmwmFunctions, MWM_FUNC_MOVE + MWM_FUNC_RESIZE,
           XmNmwmDecorations, MWM_DECOR_ALL + MWM_DECOR_MINIMIZE,
           NULL );

  form = XtVaCreateWidget( "form", xmFormWidgetClass, textBox,
                        XmNresizePolicy, XmRESIZE_GROW,
                        XmNhorizontalSpacing, 20,
                        XmNverticalSpacing, 20,
                        XmNunitType, XmPIXELS,
                        XmNhorizontalSpacing, 10,
                        XmNverticalSpacing, 10,
                        NULL );
  sprintf(reason, PPS_FAILURE_PROMPT, jobId);
  howLabel =  XtVaCreateManagedWidget( "howLabel", xmLabelWidgetClass, form,
          XmNalignment, XmALIGNMENT_CENTER,
          XmNtopAttachment, XmATTACH_FORM,
          XmNleftAttachment, XmATTACH_FORM,
          XmNrightAttachment, XmATTACH_FORM,
          RES_CONVERT( XmNlabelString, reason ),  
          NULL);

/**** cleanup choice *****/
    text_w = XtVaCreateManagedWidget ("text", xmTextWidgetClass, form,
          XmNtopAttachment, XmATTACH_WIDGET,
          XmNtopWidget, howLabel,
          XmNleftAttachment, XmATTACH_FORM,
          XmNrightAttachment, XmATTACH_FORM,
/*        XmNeditMode,     XmMULTI_LINE_EDIT, */
/*        XmNvalue,     "", */
        XmNmaxLength, MAX_PPS_COMMENT_LENGTH,
/*        XmNwidth, 100, */
/*        XmNheight, 100, */
        NULL);



  separator = XtVaCreateManagedWidget( "sep", xmSeparatorWidgetClass, form,
          XmNtopAttachment, XmATTACH_WIDGET,
          XmNtopWidget, text_w, 
          XmNleftAttachment, XmATTACH_FORM,
          XmNrightAttachment, XmATTACH_FORM,
          XmNbottomAttachment, XmATTACH_POSITION,
          XmNbottomPosition, 80,
          XmNleftOffset, 2,
          XmNrightOffset, 2,
          NULL );
  okButton = XtVaCreateManagedWidget( "okButton", xmPushButtonWidgetClass, form,
                        XmNwidth, 60, XmNheight, 30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40, 
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, separator,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 2,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        NULL );
  pp->jobId = jobId;
  pp->widget = text_w;
  pp->subsysName = malloc (strlen(subsysName)+1);
  strcpy(pp->subsysName , subsysName);

  XtAddCallback(okButton, XmNactivateCallback, TextConfirmCB,  (XtPointer) pp);
  XtAddCallback(text_w, XmNmodifyVerifyCallback, noquotes,  (XtPointer) pp);
  XtAddCallback(text_w, XmNactivateCallback, TextConfirmCB,  (XtPointer) pp);

#ifdef CANCEL_BUTTON
  cancelButton = XtVaCreateManagedWidget( "cancelButton",
                        xmPushButtonWidgetClass, form,
                        XmNwidth, 60, XmNheight, 30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 65,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, separator,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Cancel" ),
                        NULL );
  pp->jobId = jobId;
  pp->subsysName = malloc (strlen(subsysName)+1);
  strcpy(pp->subsysName , subsysName);

  XtAddCallback(cancelButton,XmNactivateCallback, TextCancelCB, 
                (XtPointer) pp);

  XmProcessTraversal(okButton, XmTRAVERSE_CURRENT);

#endif

  UxPopupInterface(textBox, no_grab);

} /* popupTextBox */



/*----------------------------------------------------------
 * NAME:
 *  check_number
 *
 * DESCRIPTION:
 *  verify number entered in a text field widget
 *
 * NOTES:
 *
 *---------------------------------------------------------*/


void check_number(Widget text_w, XtPointer client_data, XtPointer call_data)
{
    int i, len = XmTextGetLastPosition (text_w);
    VoidProc cb_func  = (VoidProc) client_data;
    XmTextVerifyCallbackStruct *cbs =
        (XmTextVerifyCallbackStruct *) call_data;

#ifdef DEBUG
printf("check_number: reason %d cb_func 0x%x\n", cbs->reason, cb_func);
#endif
    if (cbs->reason == XmCR_ACTIVATE) {
        char *string = XmTextGetString (text_w);

        if (cbs->event) /* will be NULL if value changed programmatically */
          XtSetSensitive(maxQueueSizeReset_pb, FALSE);
#ifdef DEBUG
        printf ("string is: %s\n", string);
#endif
        for (i=0; i < strlen(string); i++) /* if paste 1st digit, can be alpha*/
          if (!isdigit(string[i]))
            return;

#ifdef DEBUG
printf("... calling cb_func with str %s -- as an int %d\n", string, atoi(string)); fflush(stdout);
#endif
        /* (*cb_func)(atoi(string)); */
        setPPSqueueSize(atoi(string)); 
        XtFree (string);
        XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        return;
    }
    else { /* modify verify: */
      if (cbs->event) /* will be NULL if value changed programmatically */
        XtSetSensitive(maxQueueSizeReset_pb, TRUE);
    }



#ifdef DEBUG
printf("\tcurrInsert %d newInsert %d", cbs->currInsert, cbs->newInsert);
printf(" startPos %d endPos %d", cbs->startPos, cbs->endPos);
printf(" doit %d ", cbs->doit);
printf(" len %d\n", cbs->text->length);
#endif
    if (cbs->startPos < cbs->currInsert) /* backspace */
        return;

    /* check that the new additions won't put us over 5 */
    for (len = 0; len < cbs->text->length; len++) {
        /* make sure all additions are digits. */
        if (!isdigit (cbs->text->ptr[len])) {
            /* not a digit-- move all chars down one and
             * decrement cbs->text->length.
             */
            int i;
            for (i = len; (i+1) < cbs->text->length; i++)
                cbs->text->ptr[i] = cbs->text->ptr[i+1];
            cbs->text->length--;
            len--;
        }
    }
    if (cbs->text->length == 0)
        cbs->doit = False;
}
