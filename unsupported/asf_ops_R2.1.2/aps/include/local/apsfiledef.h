#ifndef _APSFILEDEF_H_
#define _APSFILEDEF_H_

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		apsfiledef.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)apsfiledef.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.apsfiledef.h"


int aps_report_header(
    FILE *fp,
    char *flight_agency_name,
    char *file_type,
    char *filename,
    char *creation_date) ;

int create_pap_file(char *filename, char *start, char *stop) ;
int create_pasp_file(char *filename, char *start, char *stop) ;
int create_wos_file(char *filename, 
	char *start, char *stop, char *where_clause, int flag) ;
int create_ESA_user_req_file(char *filename, char *start, char *stop) ;
int do_j1_reqw(char *filename, char *start, char *stop) ;
int do_j1_reqq(char *filename, char *start, char *stop) ;
int do_j1_msgf(char *filename, char *start, char *stop) ;
int do_eava(char *filename, char *start, char *stop) ;
int create_asfwos_file(char *filename, char *start, char *stop) ;
int create_mcmwos_file(char *filename, char *start, char *stop) ;
int create_csarar_file(char *filename, char *start, char *stop) ;
int rsndaysvf(char *filename, char *start, char *stop) ;

#define APS_RECV 0
#define APS_SEND 1

typedef
	struct _REPORT
	{
		/*
		--	note report and file are semi-synonymous:
		--		report is the fa report
		--		file is the physical file on a host
		*/

		char *type ;					/* report type */
		char *name ;					/* report name */
		int (*func)() ;					/* function for creating report */
		char *create_executable ;		/* create program (ie, executable) */
		int  aps_fullpath_id ;			/* report's index into aps_path_table */
		char *dirmask ;					/* Motif file dialog directory mask */
		char *default_file_name ;		/* default filename */
		void (*transfer_func)() ;		/* function to transfer the file */
		char *metacode ;				/* type/code for the metadata file */

		char *(*mk_local_filename)() ;	/* func. to create local host filename*/
		char *(*mk_remote_filename)() ;	/*func. to create remote host filename*/
		int (*xlate_func)() ;			/* file translation function */
		char *fa_destination ;			/* FAIF destination flight agency */
		char *fa_filetype ;				/* FAIF destination file type */

		char *mu_activityid ;			/* report's Multi-user activity-id */

	} REPORT ;

/*
--	APS report types
*/
#define MAX_RPT_TYPE_LEN				4	/* length of longest report type*/

#define INBOUND_ALL_TYPES				"ALL"	/* special catchall */
#define AWOS_TYPE						"AWOS"
#define MWOS_TYPE						"MWOS"
#define AREQ_TYPE						"AREQ"
#define AE1E_TYPE						"AE1E"
#define AE2E_TYPE						"AE2E"
#define AJ1E_TYPE						"AJ1E"
#define AA1E_TYPE						"AA1E"
#define AR1E_TYPE						"AR1E"
#define ME1E_TYPE						"ME1E"
#define ME2E_TYPE						"ME2E"
#define MR1E_TYPE						"MR1E"
#define STGS_TYPE						"STGS"
#define MSGE_TYPE						"MSGE"
#define REQW_TYPE						"REQW"
#define REQQ_TYPE						"REQQ"
#define MSGF_TYPE						"MSGF"
#define REUG_TYPE						"REUG"
#define CRAR_TYPE						"CRAR"
#define MRAR_TYPE						"MRAR"
#define ADDM_TYPE						"ADDM"
#define MDDM_TYPE						"MDDM"

#define ARES_TYPE						"ARES"
#define MPSG_TYPE						"MPSG"
#define SHQP_TYPE						"SHQP"
#define SHAQ_TYPE						"SHAQ"
#define REQR_TYPE						"REQR"
#define OPL1_TYPE						"OPL1"
#define REQM_TYPE						"REQM"
#define REQA_TYPE						"REQA"
#define MSGN_TYPE						"MSGN"
#define OPLN_TYPE						"OPLN"
#define CRRA_TYPE						"CRRA"
#define CRSA_TYPE						"CRSA"
#define CRRM_TYPE						"CRRM"
#define CRSM_TYPE						"CRSM"

#define GDAR_TYPE						"GDAR"
#define SDAR_TYPE						"SDAR"


#define GACS_DARFILE 0 
#define SAPS_DARFILE 1 

/*
-- filenames can be any length, but this def'n is used
-- as a check in some file processing, so if a longer filename
-- occurs, grow this definition.  [NOTE: this is changed from
-- the r1a' def'n of 14, limited by ACS File Handler]
*/
#define APS_MAX_FILENAME_LENGTH	200

/* these MAX LEN values should be adjusted whenever the length of
-- the longest non-wildcarded dflt name is changed
*/
#define	MAX_INBOUND_DEFAULT_FNAME_LEN	26

extern REPORT dar_files[] ;
extern REPORT reports[] ;
extern REPORT reports_inbound[] ;
extern REPORT cvrg_files[] ;
extern REPORT msge_file[] ;

/*
** Specific return code used by the APS File Creators.
*/
#define APS_REPORT_OK               0 /* should never be used as an exit code */
#define APS_REPORT_NONE             1
#define APS_REPORT_REPLY_FILE       2
#define APS_REPORT_ERROR            3
#define APS_REPORT_FILE_ERROR       4
#define APS_REPORT_DB_ERROR         5
#define APS_REPORT_NO_METADATA      6
#define APS_REPORT_INCOMPLETE       7

/* functions defined in the src code */
char *filename_from_pathname( char *pathname );
char *path_from_pathname( char *pathname );
int create_aps_meta_file( REPORT *apsfile, char *filename,
	char *start, char *stop );

#endif  /* _APSFILEDEF_H_ */
