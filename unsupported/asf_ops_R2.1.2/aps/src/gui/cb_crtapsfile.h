#ifndef CB_CRTAPSFILE_H
#define CB_CRTAPSFILE_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_crtapsfile.h
Description:	
Creator:	unknows
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_crtapsfile.h	5.2 98/03/15 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_crtapsfile.h"

#define PMF_AGGREGATE_NAME  		"CATALOG_METADATA"
#define PMF_CREATION_TIME_KEYWORD   "FILE_CREATION_TIME"
#define PMF_FILENAME_KEYWORD		"FILE_NAME"

/* Extend Times Constants */
#define EXTEND_TIMES_DEF_VALUE		True			/* default value */
#define EXTEND_TIMES_XRES			"extendTimes"	/* X resource name */
#define EXTEND_TIMES_XCLASS			"ExtendTimes"	/* X resource class */


/*==============================================================================
	Create Function Declarations
==============================================================================*/
void cb_update_capsfile_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_show_reports(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void update_reqq_phase_list(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void create_DMAP_file() ;

void cb_create_report(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_capsfile_select_input(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_init_extended_times(
	Widget widget, XtPointer client_data, XtPointer *cbs) ;

void cb_quit_filegen(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

char *remote_STGS_filename(char *pmf_filename) ;

char *remote_REUG_filename(char *pmf_filename) ;

char *remote_REQW_filename(char *pmf_filename) ;

char *remote_REQQ_filename(char *pmf_filename) ;

char *remote_MSGE_filename(char *pmf_filename) ;

char *remote_MSGF_filename(char *pmf_filename) ;

char *remote_wos_filename(char *pmf_filename) ;

char *remote_sv_filename(char *pmf_filename) ;

char *remote_WFF_WOS_filename( char *pmf_filename ) ;

char *remote_WFF_EPH_filename( char *pmf_filename ) ;

char *remote_WFF_REQ_filename(char *pmf_filename) ;

char *CRT_local_filename() ;


/*==============================================================================
	Transfer Function Declarations
==============================================================================*/
char *aps_getPMFKeyWordValue(char* keyword, 
							 char* targetAggregate, 
							 char* pmfFileName);

void cb_faif_filexfer(	Widget widget, 
						XtPointer client_data, 
						XtPointer call_data);

void cb_hc_filexfer(Widget widget, 
					XtPointer client_data, 
					XtPointer call_data);

int xlate_sv_to_wff_format(char* infile, char* outfile);

int xlate_wos_to_wff_format(char* infile, char* outfile);


#endif	/* CB_CRTAPSFILE_H */
