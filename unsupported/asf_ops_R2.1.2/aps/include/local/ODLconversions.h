#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   ODLconversions.h
Description:    
Creator:    Lawrence Stevens
Notes:      
==============================================================================*/
#pragma ident   "@(#)ODLconversions.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.ODLconversions.h"


#ifndef __ODLCONVERSIONS_H__
#define __ODLCONVERSIONS_H__

#include <stddef.h>       /* for NULL       */
 
/* FOR ODL PARSE
*/
#include <ODL_defs.h>
#include <odldef.h>

/* 
--  Default Radarsat Sensor for ODL files:  ADDM, MDDM.
--  Use this value if dtk.sensor = SAR, i.e.,  
--  if sensor mode is not known.   
--  [Used in ".../src/lib_interface/ODLconversions.c" line 305]
*/
#define ODL_DEFAULT_R1_SENSOR     "SWB"

extern DB_RECORD            **odl_downlink_dtk_rec ;
extern llist                *odl_obs_dtk_list ;
extern DB_RECORD            **odl_obs_dtk_rec ;

extern ODL_VALUEDEFS        wos_file_valuedefs[];
extern ODL_VALUEDEFS        res_file_valuedefs[];
extern ODL_FILEDEF          wos_file;
extern ODL_FILEDEF          res_file;

extern ODL_CREATE_VALUEDEFS odl_wos_valuedefs[] ;
extern ODL_CREATE_VALUEDEFS odl_dmap_valuedefs[] ;
extern ODL_CREATE_FILENAME  odl_create_file_info[] ;

extern ODL_FILENAME         ODL_files[] ;
extern ODL_FILENAME         ARES_file ;

extern EQUIV_TABLE          Framegen_activity[];

extern EQUIV_TABLE          ODL_dmap_dtkstat[];
extern EQUIV_TABLE          ODL_dmap_mode[];
extern EQUIV_TABLE          ODL_dmap_sensor[];
extern EQUIV_TABLE          ODL_dmap_station_id[];

extern EQUIV_TABLE          WOS_activity[];
extern EQUIV_TABLE          WOS_agency[];
extern EQUIV_TABLE          WOS_destination[];
extern EQUIV_TABLE          WOS_msg_type[];
extern EQUIV_TABLE          WOS_mode[];
extern EQUIV_TABLE          WOS_quicklook[];
extern EQUIV_TABLE          WOS_satellite[];
extern EQUIV_TABLE          WOS_sensor[];
extern EQUIV_TABLE          WOS_transid[];
extern EQUIV_TABLE          WOS_object[];
extern EQUIV_TABLE          WOS_record[];


/* FOR PHASE UTILITIES  */
#include "phase_utilities.h"
 
/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface definitions    */
  
/* FOR APS MACROS, etc...    */
#include "dapps_defs.h"

/* PROTOTYPES */
/* located in .../src/lib_interface/ODLconversions.c   */

int get_aos_los_start_stop(
    void        *unused_pointer,
    DB_RECORD   ***dtk_rec_ptr,      /* read values from this DB_RECORD   */
    char        *result_string ) ;

int write_dmap_obs_dtks(
    void        *unused_pointer,
    llist       **dtk_llist_ptr,      /* read values from this llist   */
    char        *result_string ) ;

/* located in .../src/ODL_dtkf_c/ODL_dtkf_creator.c   */

int odl_dtkfile_create_record(
    int         odl_dtkfile_record_type,
                  /* = ODL_FILE_HEADER | ODL_FILE_RECORD | ODL_FILE_TRAILER */
    ODL_CREATE_VALUEDEFS  *odl_dtkfile,
    DB_RECORD             **input_dtk_rec ,
    FILE                  *fp ) ;             /*  output file pointer   */


#endif   /*  __ODLCONVERSIONS_H__    */
