#ifndef APS_EXE_NAMES
#define APS_EXE_NAMES

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	aps_exe_names.h
Description:	contains macros defining the name of each executable
				in the aps subsystem
Creator:	Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)aps_exe_names.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.aps_exe_names.h"

#define APS_ENCRYPT_CMD			"APS_encrypt"
#define CON_ROUNDUP_CMD			"aps_CON_roundup"
#define CSA_FILE_PROC_CMD		"aps_proc_CSAFile"
#define FA_FILE_CREATE_CMD		"aps_crt_FA_File"
#define FA_FILE_PROC_CMD		"aps_proc_FA_File"
#define ODL_FILE_CREATE_CMD		"aps_crt_ODL_File"
#define ODL_FILE_PROC_CMD		"aps_proc_ODLFile"
#define ANT_ROUNDUP_CMD			"aps_ant_roundup"
#define HC_XMITCLIENT_CMD		"aps_aps2hc_xmitClient"
#define IMS_ARCHIVE_CMD			"aps_archive_APS_file"
#define ASAP_EPHEMERIS_CMD		"aps_asapephm"
#define CAT_EPHEMERIS_CMD		"aps_catephm"
#define COVERAGE_WINDOW_CMD		"aps_coverage_window"
#define FILE_CREATION_CMD		"aps_crt_dTimeRpt"
#define CREATE_DTK_OPPS_CMD		"aps_crt_dtk_opps"
#define CREATE_NOM_CVRG_CMD		"aps_crt_nom_cov"
#define CREATE_NOM_ORBIT_CMD	"aps_crt_nom_orb"
#define CREATE_SV_FILE_CMD		"aps_crt_sv_File"
#define DAR_STATS_CMD			"aps_darstats"
#define DO_LIST_CMD				"aps_do_prcapsfile_main"
#define SEG_FILE_LOAD_CMD		"aps_dtkm_segload"
#define FRAME_GEN_CMD			"aps_framegen"
#define APS_GET_FILE_LIST		"aps_get_file_list.pl"		/* perl script */
#define GET_LIST_CMD			"aps_get_prcapsfile_main"
#define APS_GUI_CMD				"aps_gui"
#define J1_MSGN_FILE_PROC_CMD	"aps_j1_msgn"
#define MAPPER_CMD				"aps_mapper"
#define OPLN_PRINT_CMD			"aps_opln_print"
#define WOS_COMPARE_CMD			"aps_WOS_compare"

#endif	/* APS_EXE_NAMES */
