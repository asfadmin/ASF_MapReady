#ifndef CB_CNOMORB_H
#define CB_CNOMORB_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_cnomorb.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_cnomorb.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_cnomorb.h"

char * get_satellite_name() ;

void cb_show_orbit_relations(
        Widget widget, XtPointer client_data , XtPointer cbs) ;

void cb_update_cnomorb_form(
        Widget widget, XtPointer client_data , XtPointer cbs) ;

void cb_do_cnom(
        Widget widget, XtPointer client_data , XtPointer cbs) ;

#endif	/* CB_CNOMORB_H */
