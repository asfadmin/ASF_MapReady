#ifndef    DB_ACTIVE_DAR_ACTIVITIES_H
#define    DB_ACTIVE_DAR_ACTIVITIES_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_active_dar_activities.h
Description:  Header file for APS db table:  active_dar_activities.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Mon Nov 11 11:13:46 PST 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_active_dar_activities.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_active_dar_activities.h"
 
#define ACTIVE_DAR_ACTIVITIES_NODE                     0 
#define ACTIVE_DAR_ACTIVITIES_PROCESS_ID               1 
#define ACTIVE_DAR_ACTIVITIES_KPID                     2 
#define ACTIVE_DAR_ACTIVITIES_COMMAND_NAME             3 
#define ACTIVE_DAR_ACTIVITIES_PERMISSION_ID            4 
#define ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID           5 
#define ACTIVE_DAR_ACTIVITIES_USERID                   6 
#define ACTIVE_DAR_ACTIVITIES_DARID                    7 
#define NUM_ACTIVE_DAR_ACTIVITIES_COLS                 8 

#define CAST_ACTIVE_DAR_ACTIVITIES_NODE         (char *)
#define CAST_ACTIVE_DAR_ACTIVITIES_PROCESS_ID   (char *)
#define CAST_ACTIVE_DAR_ACTIVITIES_KPID         *(DBINT *)
#define CAST_ACTIVE_DAR_ACTIVITIES_COMMAND_NAME          (char *)
#define CAST_ACTIVE_DAR_ACTIVITIES_PERMISSION_ID         *(DBINT *)
#define CAST_ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID        (char *)
#define CAST_ACTIVE_DAR_ACTIVITIES_USERID       (char *)
#define CAST_ACTIVE_DAR_ACTIVITIES_DARID        *(DBINT *)
 
#endif   /*    DB_ACTIVE_DAR_ACTIVITIES_H    */
