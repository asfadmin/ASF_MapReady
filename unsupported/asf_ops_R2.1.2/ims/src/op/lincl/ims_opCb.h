/*****************************************************************************
**
** ims_opCb.h
**
** Revision History: 
**     06/05/96  J. Ting - order_save_resultsCb and order_save_results_okCb 
**
**     06/11/96  J. Ting - order_save_results_cancelCb
**     07/12/96  J. Ting - order_restart_itemCb
**     09/06/96  J. Ting - added isApostrophe function 
**
*****************************************************************************/

#ifndef _IMS_OPCB_H
#define _IMS_OPCB_H

static char *sccsOpCb = "@(#)ims_opCb.h	5.5  05/12/97";

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_media.h>
#include <ims_shm.h>
#include <ims_job_control.h>
#include <ims_mediaJob.h>
#include <ims_opCat.h>
#include <ims_acct.h>
#include <ims_op.h>

/*-------------------------------------------------------------*/
/* callback functions for welcome screen 											 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_WELCOMECB_C
void welcome_create_op_interfaces (Widget, XtPointer, XtPointer);
void welcome_orderCb (Widget, XtPointer, XtPointer); 
void welcome_photoCb (Widget, XtPointer, XtPointer); 
void welcome_filmCb (Widget, XtPointer, XtPointer); 
void welcome_accountCb (Widget, XtPointer, XtPointer); 
void welcome_downlinkCb (Widget, XtPointer, XtPointer); 
void welcome_exitCb (Widget, XtPointer, XtPointer); 
#else
extern void welcome_create_op_interfaces (Widget, XtPointer, XtPointer);
extern void welcome_orderCb (Widget, XtPointer, XtPointer); 
extern void welcome_photoCb (Widget, XtPointer, XtPointer); 
extern void welcome_filmCb (Widget, XtPointer, XtPointer); 
extern void welcome_accountCb (Widget, XtPointer, XtPointer); 
extern void welcome_downlinkCb (Widget, XtPointer, XtPointer); 
extern void welcome_exitCb (Widget, XtPointer, XtPointer); 
#endif


/*-------------------------------------------------------------*/
/* callback functions for search screen 											 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_SEARCHCB_C
void search_create_checkbox_togglesCb (Widget, XtPointer, XtPointer);
void search_closeCb (Widget, XtPointer, XtPointer);
int  search_executeQuery (Widget);
void search_executeCb (Widget, XtPointer, XtPointer);
void search_clearCb (Widget, XtPointer, XtPointer);
void search_date_loseFocusCb (Widget, XtPointer, XtPointer);
void search_radiobox_toggledCb (Widget, XtPointer, XtPointer);
void search_optionmenu_toggledCb (Widget, XtPointer, XtPointer);
void search_accountId_validsCb (Widget, XtPointer, XtPointer);
void search_userId_validsCb (Widget, XtPointer, XtPointer);
void search_goto_orderCb (Widget, XtPointer, XtPointer);
void search_goto_welcomeCb (Widget, XtPointer, XtPointer);
void search_check_date (Widget, XtPointer, XtPointer);
void search_printScreenCb (Widget, XtPointer, XtPointer);
#else
extern void search_create_checkbox_togglesCb (Widget, XtPointer, XtPointer);
extern void search_closeCb (Widget, XtPointer, XtPointer);
extern int  search_executeQuery (Widget);
extern void search_executeCb (Widget, XtPointer, XtPointer);
extern void search_clearCb (Widget, XtPointer, XtPointer);
extern void search_date_loseFocusCb (Widget, XtPointer, XtPointer);
extern void search_radiobox_toggledCb (Widget, XtPointer, XtPointer);
extern void search_optionmenu_toggledCb (Widget, XtPointer, XtPointer);
extern void search_accountId_validsCb (Widget, XtPointer, XtPointer);
extern void search_userId_validsCb (Widget, XtPointer, XtPointer);
extern void search_goto_orderCb (Widget, XtPointer, XtPointer);
extern void search_goto_welcomeCb (Widget, XtPointer, XtPointer);
extern void search_check_date (Widget, XtPointer, XtPointer);
extern void search_printScreenCb (Widget, XtPointer, XtPointer);
#endif


/*-------------------------------------------------------------*/
/* callback functions for order screen   											 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_ORDERCB_C
void order_displayResults (Widget);
void order_closeCb (Widget, XtPointer, XtPointer);
void order_goto_searchCb (Widget, XtPointer, XtPointer);
void order_goto_welcomeCb (Widget, XtPointer, XtPointer);
void order_refreshCb (Widget, XtPointer, XtPointer);
void order_scroll_orderListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
void order_orderLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void order_show_orderItemsCb (Widget, XtPointer, XtPointer);
void order_scroll_itemListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
void order_itemLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void order_browse_itemDetailsCb (Widget, XtPointer, XtPointer);
void order_browse_orderDetailsCb (Widget, XtPointer, XtPointer);
void order_update_orderPriorityCb (Widget, XtPointer, XtPointer);
void order_update_itemPriorityCb (Widget, XtPointer, XtPointer);
void order_edit_orderCommentCb (Widget, XtPointer, XtPointer);
void order_edit_itemCommentCb (Widget, XtPointer, XtPointer);
void order_update_orderStatusCb (Widget, XtPointer, XtPointer);
void order_update_itemStatusCb (Widget, XtPointer, XtPointer);
void order_validate_orderCb (Widget, int, XtPointer);
void order_validate_itemCb (Widget, int, XtPointer);
void order_printScreenCb (Widget, XtPointer, XtPointer);
void order_processMediaCb(Widget, int, XtPointer);
void order_addDeviceStatsWidget (Widget, XtPointer, XtPointer);
int start_mediaJob(OP_MEDIA_BATCH_LIST *, DBINT, char *, void (* ) ());
void order_endMediaJobCb (int, int, int, int, char *);
void order_processMediaBatch();
int display_MediaStatusScreen();
void order_mediaStatusUpdateCb (void *);
void order_clearDeviceLabel(Widget, int);
void order_yellowDeviceLabel(Widget, int);
void order_blueDeviceLabel(Widget, int);
void order_redDeviceLabel(Widget, int);
void rollback_batchItemsStatus(DBINT order_id, OP_MEDIA_BATCH_LIST *batchPtr);
void rollback_tapeItemsStatus(OP_TAPE_ITEM_LIST *tapeItemList);
void free_orderList();
void free_itemList();
void free_mediaJob(DBINT);
void free_tapeItemList(OP_TAPE_ITEM_LIST **);
void free_shipItemList(OP_SHIP_ITEM_LIST **);
void free_shipIdList(OP_ORDER_SHIPID_LIST **);
void order_create_shipmentCb (Widget, int, XtPointer);
void order_view_shippingReportsCb (Widget, int, XtPointer);
void order_create_invoiceCb (Widget, int, XtPointer);
void order_view_invoiceCb (Widget, int, XtPointer);
void free_billItemList(OP_BILL_ITEM_LIST **);
void free_billIdList(OP_ORDER_BILLID_LIST **);
void order_save_resultsCb(Widget, XtPointer, XtPointer);
void order_save_results_okCb(Widget, XtPointer, XtPointer);
void order_save_results_cancelCb(Widget, XtPointer, XtPointer);
void order_restart_itemCb(Widget, XtPointer, XtPointer);
#else
extern void order_displayResults (Widget);
extern void order_closeCb (Widget, XtPointer, XtPointer);
extern void order_goto_searchCb (Widget, XtPointer, XtPointer);
extern void order_goto_welcomeCb (Widget, XtPointer, XtPointer);
extern void order_refreshCb (Widget, XtPointer, XtPointer);
extern void order_scroll_orderListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void order_orderLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void order_show_orderItemsCb (Widget, XtPointer, XtPointer);
extern void order_scroll_itemListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void order_itemLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void order_browse_itemDetailsCb (Widget, XtPointer, XtPointer);
extern void order_browse_orderDetailsCb (Widget, XtPointer, XtPointer);
extern void order_update_orderPriorityCb (Widget, XtPointer, XtPointer);
extern void order_update_itemPriorityCb (Widget, XtPointer, XtPointer);
extern void order_edit_orderCommentCb (Widget, XtPointer, XtPointer);
extern void order_edit_itemCommentCb (Widget, XtPointer, XtPointer);
extern void order_update_orderStatusCb (Widget, XtPointer, XtPointer);
extern void order_update_itemStatusCb (Widget, XtPointer, XtPointer);
extern void order_validate_orderCb (Widget, int, XtPointer);
extern void order_validate_itemCb (Widget, int, XtPointer);
extern void order_printScreenCb (Widget, XtPointer, XtPointer);
extern void order_processMediaCb (Widget, int, XtPointer);
extern void order_addDeviceStatsWidget (Widget, XtPointer, XtPointer);
extern int start_mediaJob(OP_MEDIA_BATCH_LIST *, DBINT, char *, void (* ) ());
extern void order_endMediaJobCb (int, int, int, int, char *);
extern void order_processMediaBatch();
extern int display_MediaStatusScreen();
extern void order_mediaStatusUpdateCb (void *);
extern void order_clearDeviceLabel(Widget, int);
extern void order_yellowDeviceLabel(Widget, int);
extern void order_blueDeviceLabel(Widget, int);
extern void order_redDeviceLabel(Widget, int);
extern void rollback_batchItemsStatus(DBINT order_id, OP_MEDIA_BATCH_LIST *batchPtr);
extern void rollback_tapeItemsStatus(OP_TAPE_ITEM_LIST *tapeItemList);
extern void free_orderList();
extern void free_itemList();
extern void free_mediaJob(DBINT);
extern void free_tapeItemList(OP_TAPE_ITEM_LIST **);
extern void free_shipItemList(OP_SHIP_ITEM_LIST **);
extern void free_shipIdList(OP_ORDER_SHIPID_LIST **);
extern void free_billItemList(OP_BILL_ITEM_LIST **);
extern void free_billIdList(OP_ORDER_BILLID_LIST **);
extern void order_create_shipmentCb (Widget, int, XtPointer);
extern void order_view_shippingReportsCb (Widget, int, XtPointer);
extern void order_view_invoiceCb (Widget, int, XtPointer);
extern void order_create_invoiceCb (Widget, int, XtPointer);
extern void order_save_resultsCb(Widget, XtPointer, XtPointer);
extern void order_save_results_okCb(Widget, XtPointer, XtPointer);
extern void order_save_results_cancelCb(Widget, XtPointer, XtPointer);
extern void order_restart_itemCb(Widget, XtPointer, XtPointer);
#endif


/*-------------------------------------------------------------*/
/* callback functions for message dialog box									 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_MSGBOXDLGCB_C
void msgBoxDlg_okCb (Widget, XtPointer, XtPointer);
void msgBoxDlg_photoOkCb (Widget, XtPointer, XtPointer);
void msgBoxDlg_popupCb (Widget, int, char *);
#else
extern void msgBoxDlg_okCb (Widget, XtPointer, XtPointer);
extern void msgBoxDlg_photoOkCb (Widget, XtPointer, XtPointer);
extern void msgBoxDlg_popupCb (Widget, int , char *);
#endif


/*-------------------------------------------------------------*/
/* callback functions for browse dialog box									   */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_BROWSEDLGCB_C
void browseDlg_okCb (Widget, XtPointer, XtPointer);
void browseDlg_popupCb (Widget, char *, char *);
#else
extern void browseDlg_okCb (Widget, XtPointer, XtPointer);
extern void browseDlg_popupCb (Widget, char * , char *);
#endif


/*-------------------------------------------------------------*/
/* callback functions for selection dialog box							   */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_SELECTIONDLGCB_C
void selectionDlg_orderPriority_okCb (Widget, OP_ORDER_LIST *, XtPointer);
void selectionDlg_itemPriority_okCb (Widget, OP_ORDER_ITEM_LIST *, XtPointer);
void selectionDlg_orderStatus_okCb (Widget, OP_ORDER_LIST *, XtPointer);
void selectionDlg_itemStatus_okCb (Widget, OP_ORDER_ITEM_LIST *, XtPointer);
void selectionDlg_photo_okCb (Widget, Widget, XtPointer);
void selectionDlg_fireStatus_okCb (Widget, OP_FIRE_QUEUE_LIST *, XtPointer);
void selectionDlg_popupCb (Widget, void *, int);
#else
extern void selectionDlg_orderPriority_okCb (Widget, OP_ORDER_LIST *, XtPointer);
extern void selectionDlg_itemPriority_okCb (Widget, OP_ORDER_ITEM_LIST *, XtPointer);
extern void selectionDlg_orderStatus_okCb (Widget, OP_ORDER_LIST *, XtPointer);
extern void selectionDlg_itemStatus_okCb (Widget, OP_ORDER_ITEM_LIST *, XtPointer);
extern void selectionDlg_photo_okCb (Widget, Widget, XtPointer);
extern void selectionDlg_fireStatus_okCb (Widget, OP_FIRE_QUEUE_LIST *, XtPointer);
extern void selectionDlg_laserStatus_okCb (Widget, OP_LASER_QUEUE_LIST *, XtPointer);
extern void selectionDlg_popupCb (Widget, void *, int);
#endif


/*-------------------------------------------------------------*/
/* callback functions for comment dialog box	  						   */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_COMMENTDLGCB_C
void commentDlg_cancelCb (Widget, XtPointer, XtPointer);
void commentDlg_popupCb (Widget, void *, int);
void commentDlg_updateCb (Widget, XtPointer, XtPointer);
#else
extern void commentDlg_cancelCb (Widget, XtPointer, XtPointer);
extern void commentDlg_popupCb (Widget, void *, int);
extern void commentDlg_updateCb (Widget, XtPointer, XtPointer);
#endif


/*-------------------------------------------------------------*/
/* callback functions for srchSelDlg 													 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_SRCHSELDLGCB_C
void srchSelDlg_popupCb (Widget, int, XtPointer);
void srchSelDlg_filterCb (Widget, XtPointer, XtPointer);
void srchSelDlg_okCb (Widget, XtPointer, XtPointer);
#else
extern void srchSelDlg_popupCb (Widget, int, XtPointer);
extern void srchSelDlg_filterCb (Widget, XtPointer, XtPointer);
extern void srchSelDlg_okCb (Widget, XtPointer, XtPointer);
#endif


/*-------------------------------------------------------------*/
/* callback functions for ims_opUtilCb 												 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_UTILCB_C
int isDateFieldValid (char *);
int isTimeFieldValid (char *, IMS_NUMERIC_DATE *);
void printScreen (Widget);
void timeOutCursors(int);
int askUser(Widget, char *);
int concatString(CONCAT_STR *, char *);
void forceUpdate(Widget);
void addWinMgrCloseCB(Widget, XtCallbackProc, XtPointer);
int isApostrophe (char *);
#else
extern int isDateFieldValid (char *);
extern int isTimeFieldValid (char *, IMS_NUMERIC_DATE *);
extern void printScreen (Widget);
extern void timeOutCursors(int);
extern int askUser(Widget, char *);
extern int concatString(CONCAT_STR *, char *);
extern void forceUpdate(Widget);
extern void addWinMgrCloseCB(Widget, XtCallbackProc, XtPointer);
extern int isApostrophe (char *);
#endif

/*-------------------------------------------------------------*/
/* callback functions for ims_opPhotoOrderCb									 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_PHOTOORDERCB_C
void photoOrder_closeCb (Widget, XtPointer, XtPointer);
void photoOrder_printCb (Widget, XtPointer, XtPointer);
void photoOrder_photoType_validsCb (Widget, XtPointer, XtPointer);
void photoOrder_searchCb (Widget, XtPointer, XtPointer);
void photoOrder_clearCb (Widget, XtPointer, XtPointer);
void photoOrder_addCb (Widget, XtPointer, XtPointer);
void photoOrder_scroll_queueListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
void photoOrder_queueLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void photoOrder_scroll_jobListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
void photoOrder_jobLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void photoOrder_deleteCb (Widget, XtPointer, XtPointer);
void photoOrder_cancelCb (Widget, XtPointer, XtPointer);
void photoOrder_createCb (Widget, XtPointer, XtPointer);
void photoOrder_printOrderFormCb ();
int get_productID(Widget, OP_PHOTO_QUEUE_LIST *);
int process_photoOrder(int, OP_PHOTO_QUEUE_LIST *);
void goto_photoJobScreen (Widget, XtPointer, XtPointer);
void goto_welcomeScreen (Widget, XtPointer, XtPointer);
#else
extern void photoOrder_closeCb (Widget, XtPointer, XtPointer);
extern void photoOrder_printCb (Widget, XtPointer, XtPointer);
extern void photoOrder_photoType_validsCb (Widget, XtPointer, XtPointer);
extern void photoOrder_searchCb (Widget, XtPointer, XtPointer);
extern void photoOrder_clearCb (Widget, XtPointer, XtPointer);
extern void photoOrder_addCb (Widget, XtPointer, XtPointer);
extern void photoOrder_scroll_queueListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void photoOrder_queueLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void photoOrder_scroll_jobListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void photoOrder_jobLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void photoOrder_deleteCb (Widget, XtPointer, XtPointer);
extern void photoOrder_cancelCb (Widget, XtPointer, XtPointer);
extern void photoOrder_createCb (Widget, XtPointer, XtPointer);
extern void photoOrder_printOrderFormCb ();
extern int get_productID(Widget, OP_PHOTO_QUEUE_LIST *);
extern int process_photoOrder(int, OP_PHOTO_QUEUE_LIST *);
extern void goto_photoJobScreen (Widget, XtPointer, XtPointer);
extern void goto_welcomeScreen (Widget, XtPointer, XtPointer);
#endif

/*-------------------------------------------------------------*/
/* callback functions for ims_opPhotoJobCb										 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_PHOTOJOBCB_C
void photoJob_closeCb (Widget, XtPointer, XtPointer);
void photoJob_printCb (Widget, XtPointer, XtPointer);
void photoJob_photoType_validsCb (Widget, XtPointer, XtPointer);
void photoJob_create_optionMenuCb (Widget, XtPointer, XtPointer);
void photoJob_check_date (Widget, XtPointer, XtPointer);
void photoJob_date_looseFocusCb (Widget, XtPointer, XtPointer);
void photoJob_jobId_validsCb (Widget, XtPointer, XtPointer);
void photoJob_optionmenu_toggledCb (Widget, XtPointer, XtPointer);
void photoJob_searchCb (Widget, XtPointer, XtPointer);
void photoJob_scroll_jobListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
void photoJob_jobLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void photoJob_viewCb (Widget, XtPointer, XtPointer);
void photoJob_scroll_queueListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
void photoJob_queueLists_selectionCb(Widget, XtPointer, XmListCallbackStruct *);
void photoJob_quality_validsCb(Widget, XtPointer, XmListCallbackStruct *);
void photoJob_processCb (Widget, XtPointer, XtPointer);
void photoJob_commentCb (Widget, XtPointer, XtPointer);
void photoJob_completeCb (Widget, XtPointer, XtPointer);
void photoJob_clearCb (Widget, XtPointer, XtPointer);
void goto_photoOrderScreen (Widget, XtPointer, XtPointer);
#else
extern void photoJob_closeCb (Widget, XtPointer, XtPointer);
extern void photoJob_printCb (Widget, XtPointer, XtPointer);
extern void photoJob_photoType_validsCb (Widget, XtPointer, XtPointer);
extern void photoJob_create_optionMenuCb (Widget, XtPointer, XtPointer);
extern void photoJob_check_date (Widget, XtPointer, XtPointer);
extern void photoJob_date_looseFocusCb (Widget, XtPointer, XtPointer);
extern void photoJob_jobId_validsCb (Widget, XtPointer, XtPointer);
extern void photoJob_optionmenu_toggledCb (Widget, XtPointer, XtPointer);
extern void photoJob_searchCb (Widget, XtPointer, XtPointer);
extern void photoJob_scroll_jobListsCb (Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void photoJob_jobLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void photoJob_viewCb (Widget, XtPointer, XtPointer);
extern void photoJob_scroll_queueListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void photoJob_queueLists_selectionCb(Widget, XtPointer, XmListCallbackStruct *);
extern void photoJob_quality_validsCb(Widget, XtPointer, XmListCallbackStruct *);
extern void photoJob_processCb (Widget, XtPointer, XtPointer);
extern void photoJob_commentCb (Widget, XtPointer, XtPointer);
extern void photoJob_completeCb (Widget, XtPointer, XtPointer);
extern void photoJob_clearCb (Widget, XtPointer, XtPointer);
extern void goto_photoOrderScreen (Widget, XtPointer, XtPointer);
#endif

/*-------------------------------------------------------------*/
/* callback functions for media status screen 								 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_MEDIASTATUSCB_C
void mediaStatus_closeCb (Widget, XtPointer, XtPointer);
#else
extern void mediaStatus_closeCb (Widget, XtPointer, XtPointer);
#endif

/*-------------------------------------------------------------*/
/* callback functions for media device screen 								 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_MEDIADEVICECB_C
void mediaDevice_create_deviceListCb (Widget, XtPointer, XtPointer);
void mediaDevice_closeCb (Widget, XtPointer, XtPointer);
#else
extern void mediaDevice_create_deviceListCb (Widget, XtPointer, XtPointer);
extern void mediaDevice_closeCb (Widget, XtPointer, XtPointer);
#endif

/*-------------------------------------------------------------*/
/* callback functions for film generation screen 							 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_FILMGENCB_C
void filmGen_create_optionMenuCb (Widget, XtPointer, XtPointer);
void filmGen_optionmenu_toggledCb (Widget, XtPointer, XtPointer);
void filmGen_scroll_fireListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
void filmGen_fireLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void filmGen_fire_clearCb (Widget, XtPointer, XtPointer);
void filmGen_fire_searchCb (Widget, XtPointer, XtPointer);
void filmGen_fire_addCb (Widget, XtPointer, XtPointer);
void filmGen_fire_updateCb (Widget, XtPointer, XtPointer);
void filmGen_fireStatus_updateCb (Widget, XtPointer, XtPointer);
void filmGen_scroll_ttdlListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
void filmGen_ttdlLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void filmGen_ttdl_deleteCb (Widget, XtPointer, XtPointer);
void filmGen_ttdl_processCb (Widget, XtPointer, XtPointer);
void filmGen_scroll_laserListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
void filmGen_laserLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
void filmGen_laser_clearCb (Widget, XtPointer, XtPointer);
void filmGen_laser_searchCb (Widget, XtPointer, XtPointer);
void filmGen_laser_addCb (Widget, XtPointer, XtPointer);
void filmGen_laserStatus_updateCb (Widget, XtPointer, XtPointer);
void filmGen_printCb (Widget, XtPointer, XtPointer);
void filmGen_closeCb (Widget, XtPointer, XtPointer);
void filmGen_goto_welcomeCb (Widget, XtPointer, XtPointer);
void filmGen_fire_commentCb (Widget, XtPointer, XtPointer);
void filmGen_laser_commentCb (Widget, XtPointer, XtPointer);
void filmGen_fire_regenCb (Widget, XtPointer, XtPointer);
void filmGen_laser_regenCb (Widget, XtPointer, XtPointer);
#else
extern void filmGen_create_optionMenuCb (Widget, XtPointer, XtPointer);
extern void filmGen_optionmenu_toggledCb (Widget, XtPointer, XtPointer);
extern void filmGen_scroll_fireListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void filmGen_fireLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void filmGen_fire_clearCb (Widget, XtPointer, XtPointer);
extern void filmGen_fire_searchCb (Widget, XtPointer, XtPointer);
extern void filmGen_scroll_ttdlListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void filmGen_fire_addCb (Widget, XtPointer, XtPointer);
extern void filmGen_fire_updateCb (Widget, XtPointer, XtPointer);
extern void filmGen_fireStatus_updateCb (Widget, XtPointer, XtPointer);
extern void filmGen_ttdlLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void filmGen_ttdl_deleteCb (Widget, XtPointer, XtPointer);
extern void filmGen_ttdl_processCb (Widget, XtPointer, XtPointer);
extern void filmGen_scroll_laserListsCb(Widget, XtPointer, XmScrollBarCallbackStruct *);
extern void filmGen_laserLists_selectionCb (Widget, XtPointer, XmListCallbackStruct *);
extern void filmGen_laser_clearCb (Widget, XtPointer, XtPointer);
extern void filmGen_laser_searchCb (Widget, XtPointer, XtPointer);
extern void filmGen_laser_addCb (Widget, XtPointer, XtPointer);
extern void filmGen_laserStatus_updateCb (Widget, XtPointer, XtPointer);
extern void filmGen_printCb (Widget, XtPointer, XtPointer);
extern void filmGen_closeCb (Widget, XtPointer, XtPointer);
extern void filmGen_goto_welcomeCb (Widget, XtPointer, XtPointer);
extern void filmGen_fire_commentCb (Widget, XtPointer, XtPointer);
extern void filmGen_laser_commentCb (Widget, XtPointer, XtPointer);
extern void filmGen_fire_regenCb (Widget, XtPointer, XtPointer);
extern void filmGen_laser_regenCb (Widget, XtPointer, XtPointer);
#endif

/*-------------------------------------------------------------*/
/* callback functions for Shipping screen     								 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_SHIPPINGCB_C
void shipping_printFormCb (Widget, XtPointer, XtPointer);
void shipping_editCommentsCb (Widget, XtPointer, XtPointer);
void shipping_cancelFormCb (Widget, XtPointer, XtPointer);
void shipping_closeCb (Widget, XtPointer, XtPointer);
int display_shippingData(OP_SHIPPING_DATA *, int);
#else
extern void shipping_printFormCb (Widget, XtPointer, XtPointer);
extern void shipping_editCommentsCb (Widget, XtPointer, XtPointer);
extern void shipping_cancelFormCb (Widget, XtPointer, XtPointer);
extern void shipping_closeCb (Widget, XtPointer, XtPointer);
extern int display_shippingData(OP_SHIPPING_DATA *, int);
#endif

/*-------------------------------------------------------------*/
/* callback functions for Ship View screen     								 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_SHIPVIEWCB_C
void shipView_viewCb (Widget, XtPointer, XtPointer);
void shipView_closeCb (Widget, XtPointer, XtPointer);
void shipView_browseSelectCb (Widget, XtPointer, XtPointer);
int display_shipViewScreen(DBINT, OP_ORDER_SHIPID_LIST *);
#else
extern void shipView_viewCb (Widget, XtPointer, XtPointer);
extern void shipView_closeCb (Widget, XtPointer, XtPointer);
extern void shipView_browseSelectCb (Widget, XtPointer, XtPointer);
extern int display_shipViewScreen(DBINT, OP_ORDER_SHIPID_LIST *);
#endif

/*-------------------------------------------------------------*/
/* callback functions for Billing  screen     								 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_BILLINGCB_C
void billing_closeCb (Widget, XtPointer, XtPointer);
void billing_cancelFormCb (Widget, XtPointer, XtPointer);
void billing_printFormCb (Widget, XtPointer, XtPointer);
void billing_editCommentsCb (Widget, XtPointer, XtPointer);
int display_billingData(OP_BILLING_DATA *, int);
#else
extern void billing_closeCb (Widget, XtPointer, XtPointer);
extern void billing_cancelFormCb (Widget, XtPointer, XtPointer);
extern void billing_printFormCb (Widget, XtPointer, XtPointer);
extern int display_billingData(OP_BILLING_DATA *, int);
extern void billing_editCommentsCb (Widget, XtPointer, XtPointer);
#endif

/*-------------------------------------------------------------*/
/* callback functions for Bill View screen     								 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_BILLVIEWCB_C
void billView_viewCb (Widget, XtPointer, XtPointer);
void billView_closeCb (Widget, XtPointer, XtPointer);
void billView_browseSelectCb (Widget, XtPointer, XtPointer);
void billing_editCommentsCb (Widget, XtPointer, XtPointer);
int display_billViewScreen(DBINT, OP_ORDER_BILLID_LIST *);
#else
extern void billView_viewCb (Widget, XtPointer, XtPointer);
extern void billView_closeCb (Widget, XtPointer, XtPointer);
extern void billView_browseSelectCb (Widget, XtPointer, XtPointer);
extern void billing_editCommentsCb (Widget, XtPointer, XtPointer);
extern int display_billViewScreen(DBINT, OP_ORDER_BILLID_LIST *);
#endif

/*-------------------------------------------------------------*/
/* callback functions for Login screen        								 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_LOGINCB_C
int  getUserLogin(Widget);
void login_passwdCb (Widget, XtPointer, XtPointer);
void login_moveFocusCb (Widget, XtPointer, XtPointer);
#else
extern int  getUserLogin(Widget);
extern void login_passwdCb (Widget, XtPointer, XtPointer);
extern void login_moveFocusCb (Widget, XtPointer, XtPointer);
#endif

/*-------------------------------------------------------------*/
/* callback functions for Downlink search screen							 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_DLTODTKSRCHCB_C
void dl_search_create_checkbox_togglesCb( Widget, XtPointer, XtPointer ) ;
void dl_search_time_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
void dl_search_time_loseFocusCb( Widget, XtPointer, XtPointer ) ;
void dl_search_rev_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
void dl_search_rev_loseFocusCb( Widget, XtPointer, XtPointer ) ;
void dl_search_sequence_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
void dl_search_sequence_loseFocusCb( Widget, XtPointer, XtPointer ) ;
void dl_search_executeCb( Widget, XtPointer, XtPointer ) ;
void dl_search_clearCb( Widget, XtPointer, XtPointer ) ;
void dl_search_closeCb( Widget, XtPointer, XtPointer ) ;
void dl_search_printScreenCb( Widget, XtPointer, XtPointer ) ;
void dl_search_goto_dl2dtkCb( Widget, XtPointer, XtPointer ) ;
void dl_search_goto_welcomeCb( Widget, XtPointer, XtPointer ) ;
#else
extern void dl_search_create_checkbox_togglesCb( Widget, XtPointer, XtPointer );
extern void dl_search_time_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_time_loseFocusCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_rev_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_rev_loseFocusCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_sequence_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_sequence_loseFocusCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_executeCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_clearCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_closeCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_printScreenCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_goto_dl2dtkCb( Widget, XtPointer, XtPointer ) ;
extern void dl_search_goto_welcomeCb( Widget, XtPointer, XtPointer ) ;
#endif

/*-------------------------------------------------------------*/
/* callback functions for Downlink to Data-take screen				 */
/*-------------------------------------------------------------*/

#ifdef _IMS_OP_DLTODTKCB_C
void dl2dtk_scroll_dlListsCb( Widget, XtPointer, XmScrollBarCallbackStruct * ) ;
void dl2dtk_dlLists_selectionCb( Widget, XtPointer, XmListCallbackStruct * );
void dl2dtk_displayResults( Widget ) ;
void dl2dtk_view_DL_detailsCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_scroll_dtkListsCb( Widget, XtPointer, XmScrollBarCallbackStruct * );
void dl2dtk_dtkLists_selectionCb( Widget, XtPointer, XmListCallbackStruct * );
void dl2dtk_show_downlinkDTKsCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_view_DTK_detailsCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_reset_dtkCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_toggle_proc_auth_flagCb( Widget, XtPointer, XmListCallbackStruct *);
void dl2dtk_updateCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_refreshSearchCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_closeCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_printScreenCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_goto_searchCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_goto_welcomeCb( Widget, XtPointer, XtPointer ) ;
void free_dlList() ;
#else
extern void dl2dtk_scroll_dlListsCb(
						Widget, XtPointer, XmScrollBarCallbackStruct * ) ;
extern void dl2dtk_dlLists_selectionCb(
						Widget, XtPointer, XmListCallbackStruct * );
extern void dl2dtk_displayResults( Widget ) ;
extern void dl2dtk_view_DL_detailsCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_scroll_dtkListsCb(
						Widget, XtPointer, XmScrollBarCallbackStruct * );
extern void dl2dtk_dtkLists_selectionCb(
						Widget, XtPointer, XmListCallbackStruct * ) ;
extern void dl2dtk_show_downlinkDTKsCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_view_DTK_detailsCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_reset_dtkCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_toggle_proc_auth_flagCb(
						Widget, XtPointer, XmListCallbackStruct * ) ;
extern void dl2dtk_updateCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_refreshSearchCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_closeCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_printScreenCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_goto_searchCb( Widget, XtPointer, XtPointer ) ;
extern void dl2dtk_goto_welcomeCb( Widget, XtPointer, XtPointer ) ;
extern void free_dlList() ;
#endif

#endif	/* !_IMS_OPCB_H */
