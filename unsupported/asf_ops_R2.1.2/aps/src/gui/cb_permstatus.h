#ifndef CB_PERMSTATUS_H
#define CB_PERMSTATUS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:		cb_permstatus.h
Description:	
Creator:		Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_permstatus.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_permstatus.h"

/*==============================================================================
    Gui Multi-user Permission Status Callback Functions
==============================================================================*/

void	cb_init_permstatus( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED ) ;

void	cb_popup_permstatus( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED ) ;

void	cb_quit_permstatus( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED ) ;

void	cb_refresh_perms( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED ) ;

void	cb_autoUpdate_toggled( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED ) ;

void	cb_interval_popup_OK( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED ) ;

void	cb_undo_interval_popup( Widget widget, XtPointer client_data,
		XtPointer cbs_UNUSED ) ;


/*==============================================================================
    Constants/Macros
==============================================================================*/
#define	MU_INTERVAL_RESET	1
#define	MU_INTERVAL_CANCEL	2

#endif	/* CB_PERMSTATUS_H */
