#ifndef CB_APSLOGIN_H
#define CB_APSLOGIN_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_apslogin.h
Description:	
Creator:	Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_apslogin.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_apslogin.h"

/*==============================================================================
	APS login callback function prototypes
==============================================================================*/

void cb_init_userid(
	Widget widget, XtPointer Dummy_client_data, XtPointer Dummy_cbs) ;

void cb_init_password(
	Widget widget, XtPointer Dummy_client_data, XtPointer Dummy_cbs) ;

void cb_init_database(
	Widget widget, XtPointer Dummy_client_data, XtPointer Dummy_cbs) ;

void cb_save_password(
	Widget widget, XtPointer Dummy_client_data,
	XmTextVerifyCallbackStruct *cbs) ;

void cb_map_login();	/* not a true callback: topLevelShell has no map cb */

void cb_apply_login(
	Widget Dummy_widget, XtPointer Dummy_client_data, XtPointer Dummy_cbs) ;

void cb_reset_login(
	Widget Dummy_widget, XtPointer Dummy_client_data, XtPointer Dummy_cbs) ;

void cb_verify_login(
	Widget widget, XtPointer Dummy_client_data, XtPointer Dummy_cbs) ;

void cb_exit_login(
	Widget widget, XtPointer Dummy_client_data, XtPointer Dummy_cbs) ;

/*==============================================================================
	APS login callback constants/macros
==============================================================================*/

#define MAX_USERNAME_LEN 100

#endif /* CB_APSLOGIN_H */
