#ifndef CB_APSMENU_H
#define CB_APSMENU_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_apsmenu.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_apsmenu.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_apsmenu.h"

void cb_create_aps_interfaces(
		Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_start_mapper(
        Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_get_pixmap(
		Widget widget, XtPointer client_data, XtPointer cbs ) ;

#endif	/* CB_APSMENU_H */
