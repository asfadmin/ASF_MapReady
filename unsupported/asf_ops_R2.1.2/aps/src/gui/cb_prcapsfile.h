#ifndef CB_PRCAPSFILE_H
#define CB_PRCAPSFILE_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_prcapsfile.h
Description:	
Creator:	unknown
Notes:		
			11/30/95	Teresa McKillop		updated for r1b' major redesign
==============================================================================*/
#pragma ident	"@(#)cb_prcapsfile.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_prcapsfile.h"

void cb_fill_fileproc_list(
		Widget widget, XtPointer client_data, XtPointer cbs );

void cb_process_all_files(
		Widget widget, XtPointer client_data, XtPointer cbs );

void cb_stop_file_processing(
		Widget widget, XtPointer client_data, XtPointer cbs );

void cb_quit_file_processing(
		Widget widget, XtPointer client_data, XtPointer cbs );

#endif	/* CB_PRCAPSFILE_H */
