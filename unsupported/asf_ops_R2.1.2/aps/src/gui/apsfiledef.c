#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		apsfiledef.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)apsfiledef.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.apsfiledef.c"

#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>

#include "aps_exe_names.h"
#include "apspath.h"
#include "mu_utilities.h"
#include "apsfiledef.h"

extern void popup_message() ;
extern char display_string[] ;

/* Callback function to handle file transfer */
extern void cb_hc_filexfer() ;
extern void cb_faif_filexfer() ;
extern void cb_ims_filexfer() ;

/* Translation functions */
extern int xlate_wos_to_wff_format(char *ifname, char *ofname) ;
extern int xlate_sv_to_wff_format(char *ifname, char *ofname) ;

/* Functions to define the remote file name used in file transfer */
extern char * remote_wos_filename(char* pmf_filename) ;
extern char * remote_rfar_filename(char* pmf_filename) ;
extern char * remote_sv_filename(char* pmf_filename) ;
extern char * remote_ADDM_filename(char* pmf_filename) ;
extern char * remote_MDDM_filename(char* pmf_filename) ;
extern char * remote_STGS_filename(char *pmf_filename) ;
extern char * remote_REUG_filename(char *pmf_filename) ;
extern char * remote_REQW_filename(char *pmf_filename) ;
extern char * remote_REQQ_filename(char *pmf_filename) ;
extern char * remote_MSGE_filename(char *pmf_filename) ;
extern char * remote_MSGF_filename(char *pmf_filename) ;
extern char * remote_WFF_WOS_filename( char *pmf_filename ) ;
extern char * remote_WFF_EPH_filename( char *pmf_filename ) ;
extern char * remote_WFF_REQ_filename(char *pmf_filename) ;

/* Function to get the local file name used in file transfer and creation */
extern char * CRT_local_filename() ;

/*
-- The following are dummy functions for use with the aps gui
-- program (aps)... The functions are not called from the
-- gui rather they are called from the create_aps_file application
--
-- we have these here so we don't get an unresolved symbol
-- we'll never call from aps gui program
--
-- If we ever do these would be removed
*/

#ifdef APS_GUI
	/* ARGSUSED0 */
do_j1_msgf(char *filename,char * start, char *stop) { return NULL ; }
	/* ARGSUSED0 */
do_eava(char *filename,char * start, char *stop) { return NULL ; }
	/* ARGSUSED0 */
create_csarar_file(char *filename, char *start, char *stop)
		{ return NULL ; }
#endif

#ifndef APS_GUI_SKIP
void cb_hc_filexfer() { }
void cb_faif_filexfer() { }
void cb_ims_filexfer() { }

	/* ARGSUSED0 */
int xlate_wos_to_wff_format(char *ifname, char *ofname) { return NULL ; }
	/* ARGSUSED0 */
int xlate_sv_to_wff_format(char *ifname, char *ofname) { return NULL ; }

	/* ARGSUSED0 */
char * remote_wos_filename(char* pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_rfar_filename(char* pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_sv_filename(char* pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_ADDM_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_MDDM_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_STGS_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_REUG_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_REQW_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_REQQ_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_MSGE_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_MSGF_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * remote_WFF_WOS_filename( char *pmf_filename ) { return NULL ; }
	/* ARGSUSED0 */
char * remote_WFF_EPH_filename( char *pmf_filename ) { return NULL ; }
	/* ARGSUSED0 */
char * remote_WFF_REQ_filename(char *pmf_filename) { return NULL ; }
	/* ARGSUSED0 */
char * CRT_local_filename() { return NULL ; }
#endif

/*
-- File Generation (Creation) Files
*/
REPORT reports[] =
{
	{AWOS_TYPE, "ASF Weekly Operations Schedule",
		NULL, ODL_FILE_CREATE_CMD, 			/* create func, exe            */
		APS_ASF_WOS,						/* aps_fullpath_id             */
		"*(AWOS|AWOS[^.]+|AWOS*[^M])", "AWOS",
											/* dirmask, dflt name          */
		cb_hc_filexfer, "HC ASF_WOS",		/* transfer func, metacode     */
		CRT_local_filename, remote_wos_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_AWOS},							/* MU activity id              */

	{ADDM_TYPE, "ASF Downlink to Datatake Map File",
		NULL, ODL_FILE_CREATE_CMD,			/* create func, exe            */
		APS_ADDM_FILE,						/* aps_fullpath_id             */
		"ADDM*", "ADDM",					/* dirmask, dflt name          */
		cb_ims_filexfer, "ASF DL TO DTK MAP",		
											/* transfer func, metacode     */
		CRT_local_filename, remote_ADDM_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		"NULL", "NULL",						/* FA dest, FA type            */
		MU_ADDM},							/* MU activity id              */

	{MWOS_TYPE, "McMurdo Weekly Operations Schedule",
		NULL, ODL_FILE_CREATE_CMD,			/* create func, exe            */
		APS_WFF_WOS,						/* aps_fullpath_id             */
		"*(MWOS|MWOS[^.]+|MWOS*[^M])", "MWOS",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "WFF WALPS_WOS",	/* transfer func, metacode     */
		CRT_local_filename, remote_WFF_WOS_filename, xlate_wos_to_wff_format,
											/* lfname(), rfname(), xlate() */
		"WALPS", "WALPS_WOS",				/* FA dest, FA type            */
		MU_MWOS},							/* MU activity id              */

	{MDDM_TYPE, "McMurdo Downlink to Datatake Map File",
		NULL, ODL_FILE_CREATE_CMD,			/* create func, exe            */
		APS_MDDM_FILE,						/* aps_fullpath_id             */
		"MDDM*", "MDDM",					/* dirmask, dflt name          */
		cb_ims_filexfer, "ASF DL TO DTK MAP",		
											/* transfer func, metacode     */
		CRT_local_filename, remote_MDDM_filename, NULL,		
	   										/* lfname(), rfname(), xlate() */
		"NULL", "NULL",						/* FA dest, FA type            */
		MU_MDDM},							/* MU activity id              */

	{AREQ_TYPE, "McMurdo Request For Availability",
		NULL, ODL_FILE_CREATE_CMD,			/* create func, exe            */
		APS_WFF_AREQ,						/* aps_fullpath_id             */
		"*(AREQ|AREQ[^.]+|AREQ*[^M])", "AREQ",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "WFF WALPS_AVAIL_REQ",				
											/* transfer func, metacode     */
		CRT_local_filename, remote_WFF_REQ_filename, xlate_wos_to_wff_format,
											/* lfname(), rfname(), xlate() */
		"WALPS", "WALPS_AVAIL_REQ",			/* FA dest, FA type            */
		MU_AREQ},							/* MU activity id              */

	{AE1E_TYPE, "ASF ERS-1 Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_ASF_E1SV,						/* aps_fullpath_id             */
		"*(AE1E|AE1E[^.]+|AE1E*[^M])", "AE1E",
											/* dirmask, dflt name          */
		cb_hc_filexfer, "HC ASF_STVEC",		/* transfer func, metacode     */
		CRT_local_filename, remote_sv_filename, NULL, 	
											/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_AE1E},							/* MU activity id              */

	{AE2E_TYPE, "ASF ERS-2 Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_ASF_E2SV,						/* aps_fullpath_id             */
		"*(AE2E|AE2E[^.]+|AE2E*[^M])", "AE2E",
											/* dirmask, dflt name          */
		cb_hc_filexfer, "HC ASF_STVEC",		/* transfer func, metacode     */
		CRT_local_filename, remote_sv_filename, NULL, 	
											/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_AE2E},							/* MU activity id              */
		
	{AJ1E_TYPE, "ASF J-ERS-1 Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_ASF_J1SV,						/* aps_fullpath_id             */
		"*(AJ1E|AJ1E[^.]+|AJ1E*[^M])", "AJ1E",
											/* dirmask, dflt name          */
		cb_hc_filexfer, "HC ASF_STVEC",		/* transfer func, metacode     */
		CRT_local_filename, remote_sv_filename, NULL, 	
											/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_AJ1E},							/* MU activity id              */

	{AA1E_TYPE, "ASF ADEOS Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_ASF_A1SV,						/* aps_fullpath_id             */
		"*(AA1E|AA1E[^.]+|AA1E*[^M])", "AA1E",
											/* dirmask, dflt name          */
		cb_hc_filexfer, "HC ASF_STVEC",		/* transfer func, metacode     */
		CRT_local_filename, remote_sv_filename, NULL, 	
											/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_AA1E},							/* MU activity id              */

	{AR1E_TYPE, "ASF RADARSAT Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_ASF_R1SV,						/* aps_fullpath_id             */
		"*(AR1E|AR1E[^.]+|AR1E*[^M])", "AR1E",
											/* dirmask, dflt name          */
		cb_hc_filexfer, "HC ASF_STVEC",		/* transfer func, metacode     */
		CRT_local_filename, remote_sv_filename, NULL, 	
											/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_AR1E},							/* MU activity id              */
		
	{ME1E_TYPE, "McMurdo ERS-1 Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_WFF_E1SV,						/* aps_fullpath_id             */
		"*(ME1E|ME1E[^.]+|ME1E*[^M])", "ME1E",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "WFF WALPS_STVEC",/* transfer func, metacode     */
		CRT_local_filename, remote_WFF_EPH_filename, xlate_sv_to_wff_format, 
											/* lfname(), rfname(), xlate() */
		"WALPS", "WALPS_STVEC",				/* FA dest, FA type            */
		MU_ME1E},							/* MU activity id              */

	{ME2E_TYPE, "McMurdo ERS-2 Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_WFF_E2SV,						/* aps_fullpath_id             */
		"*(ME2E|ME2E[^.]+|ME2E*[^M])", "ME2E",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "WFF WALPS_STVEC",/* transfer func, metacode     */
		CRT_local_filename, remote_WFF_EPH_filename, xlate_sv_to_wff_format, 
											/* lfname(), rfname(), xlate() */
		"WALPS", "WALPS_STVEC",				/* FA dest, FA type            */
		MU_ME2E},							/* MU activity id              */

	{MR1E_TYPE, "McMurdo RADARSAT Predicted State Vectors File",
		NULL, CREATE_SV_FILE_CMD,			/* create func, exe            */
		APS_WFF_R1SV,						/* aps_fullpath_id             */
		"*(MR1E|MR1E[^.]+|MR1E*[^M])", "MR1E",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "WFF WALPS_STVEC",/* transfer func, metacode     */
		CRT_local_filename, remote_WFF_EPH_filename, xlate_sv_to_wff_format, 
											/* lfname(), rfname(), xlate() */
		"WALPS", "WALPS_STVEC",				/* FA dest, FA type            */
		MU_MR1E},							/* MU activity id              */

	{STGS_TYPE, "ADEOS STGS Reply File for REQR",
		NULL, NULL,							/* create func, exe            */
		APS_NASDA_FILES,					/* aps_fullpath_id             */
		"*(STGS|STGS[^.]+|STGS*[^M])", "STGS*",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "ADEOS-1 STGS",	/* transfer func, metacode     */
		CRT_local_filename, remote_STGS_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		"ADEOS", "ADEOS_STGS",				/* FA dest, FA type            */
		MU_REQR_STGS},						/* MU activity id              */

	{MSGE_TYPE,  "J-ERS-1 MSGE Reply File for (REQM) MDR Dumps",
		NULL, NULL,							/* create func, exe            */
		APS_NASDA_FILES,					/* aps_fullpath_id             */
		"*(MSGE|MSGE[^.]+|MSGE*[^M])", "MSGE*",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "JERS-1 MSGE",	/* transfer func, metacode     */
		CRT_local_filename, remote_MSGE_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		"NASDA", "NASDA_MSGE",				/* FA dest, FA type            */
		MU_REQM_msge},						/* MU activity id              */

	{REQW_TYPE, "NASDA J-ERS-1 Weekly Request File",
		NULL, FA_FILE_CREATE_CMD,			/* create func, exe            */
		APS_NASDA_FILES,					/* aps_fullpath_id             */
		"*(REQW|REQW[^.]+|REQW*[^M])", "REQW",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "JERS-1 REQW",	/* transfer func, metacode     */
		CRT_local_filename, remote_REQW_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		"NASDA", "NASDA_REQW",				/* FA dest, FA type            */
		MU_REQW},							/* MU activity id              */

	{REQQ_TYPE, "NASDA J-ERS-1 Quarterly Request File",
		NULL, FA_FILE_CREATE_CMD,			/* create func, exe            */
		APS_NASDA_FILES,					/* aps_fullpath_id             */
		"*(REQQ|REQQ[^.]+|REQQ*[^M])", "REQQ",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "JERS-1 REQQ",	/* transfer func, metacode     */
		CRT_local_filename, remote_REQQ_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		"NASDA", "NASDA_REQQ",				/* FA dest, FA type            */
		MU_REQQ},							/* MU activity id              */

	{MSGF_TYPE, "NASDA ASF Down Times Report",
		do_j1_msgf, NULL,					/* create func, exe            */
		APS_NASDA_FILES,					/* aps_fullpath_id             */
		"*(MSGF|MSGF[^.]+|MSGF*[^M])", "MSGF",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "JERS-1 MSGF",	/* transfer func, metacode     */
		CRT_local_filename, remote_MSGF_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		"NASDA", "NASDA_MSGF",				/* FA dest, FA type            */
		MU_MSGF},							/* MU activity id              */

	{REUG_TYPE, "ESA Unavailability Report",
		do_eava, NULL,						/* create func, exe            */
		APS_ESA_FILES,						/* aps_fullpath_id             */
		"*(REUG|REUG[^.]+|REUG*[^M])", "REUG",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "ERS-2 REUG",		/* transfer func, metacode     */
		CRT_local_filename, remote_REUG_filename, NULL,		
											/* lfname(), rfname(), xlate() */
		"ESA", "ESA_REUG",					/* FA dest, FA type            */
		MU_REUG},							/* MU activity id              */

	{CRAR_TYPE, "ASF Radarsat Reception Availability Report",
		create_csarar_file, NULL,			/* create func, exe            */
		APS_RADARSAT_RAR,					/* aps_fullpath_id             */
		"*(f(0|1|2|3|4|5|6|7|8|9)*[^M].rar)", "f*.rar",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "RADARSAT-1 CSA_RECAVAILRPT",		
											/* transfer func, metacode     */
		CRT_local_filename, NULL, NULL,		/* lfname(), rfname(), xlate() */
		"CSA", "CSA_RECAVAILRPT",			/* FA dest, FA type            */
		MU_CRAR},							/* MU activity id              */

#ifdef CHANGE_PENDING_CSA_ICD_UPDATE
	{MRAR_TYPE, "MCM Radarsat Reception Availability Report",
		create_csarar_file, NULL,			/* create func, exe            */
		APS_RADARSAT_RAR,					/* aps_fullpath_id             */
		"*(m(0|1|2|3|4|5|6|7|8|9)*[^M].rar)", "m*.rar",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "RADARSAT-1 CSA_RECAVAILRPT",		
											/* transfer func, metacode     */
		CRT_local_filename, NULL, NULL,		/* lfname(), rfname(), xlate() */
		"CSA", "CSA_RAR_MCM",				/* FA dest, FA type            */
		MU_CRAR},							/* MU activity id              */
#else
	{MRAR_TYPE, "MCM Radarsat Reception Availability Report",
		create_csarar_file, NULL,			/* create func, exe            */
		APS_RADARSAT_RAR,					/* aps_fullpath_id             */
		"*(f(0|1|2|3|4|5|6|7|8|9)*[^M].rar)", "f*.rar",
											/* dirmask, dflt name          */
		cb_faif_filexfer, "RADARSAT-1 CSA_RECAVAILRPT",		
											/* transfer func, metacode     */
		CRT_local_filename, NULL, NULL,		/* lfname(), rfname(), xlate() */
		"CSA", "CSA_RAR_MCM",				/* FA dest, FA type            */
		MU_CRAR},							/* MU activity id              */
#endif

	{NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	 NULL, NULL, NULL, NULL, NULL}
} ;


/*
-- File Processing Files
*/

REPORT reports_inbound[] =
{
	{ARES_TYPE, "McMurdo Availabilty Response",
		NULL, ODL_FILE_PROC_CMD,			/* create func, exe            */
		APS_WFF_ARES,						/* aps_fullpath_id             */
		"RES_*[0-9]*", "",					/* dirmask, dflt name          */
		NULL, "ARES",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_ARES},							/* MU activity id              */

	{MPSG_TYPE, "E-ERS-1 Reception Request (GAP)", /* type, name           */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_ESA_MPSG,						/* aps_fullpath_id             */
		"MPSG_*", "",						/* dirmask, dflt name          */
		NULL, "MPSG",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_MPSG},							/* MU activity id              */

	{SHQP_TYPE, "E-ERS-1 ESA Preliminary Schedule", /* type, name          */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_ESA_SHQP,						/* aps_fullpath_id             */
		"SHQP_*", "",						/* dirmask, dflt name          */
		NULL, "SHQP",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_SHAQP},							/* MU activity id              */

	{SHAQ_TYPE, "E-ERS-1 Acquisition Schedule", /* type, name              */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_ESA_SHAQ,						/* aps_fullpath_id             */
		"SHAQ_*", "",						/* dirmask, dflt name          */
		NULL, "SHAQ",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_SHAQ},							/* MU activity id              */

	{REQR_TYPE, "ADEOS Data Acquisition Plan", /* type, name               */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_NASDA_REQR,						/* aps_fullpath_id             */
		"REQR*", "",						/* dirmask, dflt name          */
		NULL, "REQR",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_REQR_STGS},						/* MU activity id              */

	{OPL1_TYPE, "ADEOS Observation PlanSchedule", /* type, name            */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_NASDA_OPL1,						/* aps_fullpath_id             */
		"OPL1*", "",						/* dirmask, dflt name          */
		NULL, "OPL1",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_OPL1},							/* MU activity id              */

	{REQM_TYPE,  "J-ERS-1 Request for MDR Dumps", /* type, name            */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_NASDA_REQM,						/* aps_fullpath_id             */
		"REQM_*", "",						/* dirmask, dflt name          */
		NULL, "REQM",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_REQM_msge},						/* MU activity id              */

	{REQA_TYPE,  "J-ERS-1 Tentative Assessment of REQQ", /* type, name     */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_NASDA_REQA,						/* aps_fullpath_id             */
		"REQA_*", "",						/* dirmask, dflt name          */
		NULL, "REQA",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_REQA},							/* MU activity id              */

	{MSGN_TYPE,  "J-ERS-1 Satellite Status Info. Report", /* type, name    */
		NULL, J1_MSGN_FILE_PROC_CMD,		/* create func, exe            */
		APS_NASDA_MSGN,						/* aps_fullpath_id             */
		"MSGN_*", "",						/* dirmask, dflt name          */
		NULL, "MSGN",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_MSGN},							/* MU activity id              */

	{OPLN_TYPE,  "J-ERS-1 Observation Plan/Scheeule", /* type, name        */
		NULL, FA_FILE_PROC_CMD,				/* create func, exe            */
		APS_NASDA_OPLN,						/* aps_fullpath_id             */
		"OPLN_*", "",						/* dirmask, dflt name          */
		NULL, "OPLN",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_OPLN},							/* MU activity id              */

	{CRRA_TYPE,  "Radarsat-1 Reception Requests for ASF", /* type, name    */
		NULL, CSA_FILE_PROC_CMD,			/* create func, exe            */
		APS_CSA_CRRA,						/* aps_fullpath_id             */
		"*.rrq", "",						/* dirmask, dflt name          */
		NULL, "CRRA",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_CRRA},							/* MU activity id              */

	{CRSA_TYPE,  "Radarsat-1 Reception Schedule for ASF", /* type, name    */
		NULL, CSA_FILE_PROC_CMD,			/* create func, exe            */
		APS_CSA_CRSA,						/* aps_fullpath_id             */
		"*.rsh", "",						/* dirmask, dflt name          */
		NULL, "CRSA",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_CRSA},							/* MU activity id              */

	{CRRM_TYPE,  "Radarsat-1 Reception Requests for McMurdo", /* type, name*/
		NULL, CSA_FILE_PROC_CMD,			/* create func, exe            */
		APS_CSA_CRRM,						/* aps_fullpath_id             */
		"*.rrq", "",						/* dirmask, dflt name          */
		NULL, "CRRM",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_CRRM},							/* MU activity id              */

	{CRSM_TYPE,  "Radarsat-1 Reception Schedule for McMurdo", /* type, name*/
		NULL, CSA_FILE_PROC_CMD,			/* create func, exe            */
		APS_CSA_CRSM,						/* aps_fullpath_id             */
		"*.rsh", "",						/* dirmask, dflt name          */
		NULL, "CRSM",						/* transfer func, metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		MU_CRSM},							/* MU activity id              */

	{NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	 NULL, NULL, NULL, NULL, NULL}
} ;


REPORT dar_files[] =
{
	{GDAR_TYPE, "Get Data Acquisition Request File",
		NULL, NULL,							/* create func, exe            */
		APS_ASF_FILES,						/* aps_fullpath_id             */
		"*.ING", "DAR.ING",					/* dirmask, dflt name          */
		NULL, "DQ",							/* transfer func. metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		NULL},								/* MU activity id              */

	{SDAR_TYPE, "Send Data Acquisition Request File",
		NULL, NULL,							/* create func, exe            */
		APS_ASF_FILES,						/* aps_fullpath_id             */
		"*.SYB", "DAR.SYB",					/* dirmask, dflt name          */
		NULL, "DQ",							/* transfer func. metacode     */
		NULL, NULL, NULL,					/* lfname(), rfname(), xlate() */
		NULL, NULL,							/* FA dest, FA type            */
		NULL},								/* MU activity id              */

	{NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	 NULL, NULL, NULL, NULL, NULL}
} ;
