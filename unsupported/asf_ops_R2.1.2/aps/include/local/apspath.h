#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       apspath.h
Description:    
Creator:    unknown
Notes:      
==============================================================================*/
#pragma ident   "@(#)apspath.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.apspath.h"

#ifndef _APSPATH_H_
#define _APSPATH_H_

/*
--
-- Notes:  THIS LIST MUST CORRESPOND TO THE 
--         static APS_PATH_TABLE aps_path_table[] 
--         DEFINITION FOUND IN src/lib_APS/apspath.c
--         THEY ARE INDEX VALUES INTO THIS TABLE.
--    ******
*/
 
 
#define APS_TEMP                 0
#define APS_EPHMIN               1
#define APS_EPHMOUT              2
#define APS_ORBGIN               3
#define APS_ORBGOUT              4
#define APS_CVRG                 4
#define APS_MAPPER               5
#define APS_STOICFILES           6
#define APS_CURRENT_STOICFILE    7
#define APS_UPW                  8
#define APS_STMP                 9
#define APS_GTMP                10
#define APS_ASF_FILES           11
#define APS_NASDA_FILES         12
#define APS_CSA_FILES           13
#define APS_ESA_FILES           14
#define APS_LOGS                15
#define APS_REPORTS             16
#define APS_RADARSAT_SVF        17
#define APS_RADARSAT_RAR        18
#define APS_RADARSAT_RECEPTION  19
#define APS_RADARSAT_ARCHIVE    20
#define APS_ESA_SHAQ            21
#define APS_ESA_SHQP            22
#define APS_ESA_MPSG            23
#define APS_NASDA_OPL1          24
#define APS_NASDA_REQR          25
#define APS_NASDA_OPLN          26
#define APS_NASDA_REQM          27
#define APS_NASDA_REQA          28
#define APS_NASDA_MSGN          29
#define APS_CSA_CRRA            30
#define APS_CSA_CRSA            31
#define APS_CSA_CRRM            32
#define APS_CSA_CRSM            33
#define APS_ASF_WOS             34
#define APS_ASF_E1SV            35
#define APS_ASF_E2SV            36
#define APS_ASF_J1SV            37
#define APS_ASF_A1SV            38
#define APS_ASF_R1SV            39
#define APS_WFF_WOS             40
#define APS_WFF_AREQ            41
#define APS_WFF_E1SV            42
#define APS_WFF_E2SV            43
#define APS_WFF_R1SV            44
#define APS_WFF_ARES            45
#define APS_CONFIG_XLATE        46
#define APS_CONFIG_XRESOURCE    47
#define APS_FA_ERROR_FILES      48
#define APS_FA_INPUT_FILES      49
#define APS_IMS_FILES           50
#define APS_ADDM_FILE           51
#define APS_MDDM_FILE           52

char * aps_fullpath(int id, char *filename) ;

char * aps_pathname2filename (char  *pathname);

#endif  /* _APSPATH_H_ */
