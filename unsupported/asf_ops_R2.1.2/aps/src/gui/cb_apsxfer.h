#ifndef CB_APSXFER_H
#define CB_APSXFER_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_apsxfer.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_apsxfer.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_apsxfer.h"

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

#define PMF_AGGREGATE_NAME  		"CATALOG_METADATA"
#define PMF_CREATION_TIME_KEYWORD   "FILE_CREATION_TIME"
#define PMF_FILENAME_KEYWORD		"FILE_NAME"

#define APS_XFER_CMND_OK      1
#define APS_XFER_CMND_ERR    -1
#define APS_XFER_HOST_ERR    -2
#define APS_XFER_UPASSWD_ERR -3
#define APS_XFER_CMND_CANCEL -4

#endif	/* CB_APSXFER_H */
