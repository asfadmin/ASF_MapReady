#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	NASDAconversions.h
Description:header for NASDA conversion routines, which are used to decode/
			translate NASDA File string values.
Creator:	Lawrence Stevens
Notes:		
==============================================================================*/
#pragma ident	"@(#)NASDAconversions.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.NASDAconversions.h"


#ifndef _NASDAconversions_H_
#define _NASDAconversions_H_

#include <stddef.h>       /* for NULL       */

#include "fa_defs.h"

extern EQUIV_TABLE	NASDA_sat[];
extern EQUIV_TABLE	NASDA_dtk_status[];
extern EQUIV_TABLE	NASDA_actid[];
extern EQUIV_TABLE	NASDA_plan_flags[];
extern EQUIV_TABLE	NASDA_station_id[];
extern EQUIV_TABLE	NASDA_sensor[];
extern EQUIV_TABLE	ADEOS_mode_flags[];
extern EQUIV_TABLE	ADEOS_acq_actid[];
extern EQUIV_TABLE	ADEOS_actid_2_sensor[];
extern EQUIV_TABLE	ADEOS_obs_actid[];
extern EQUIV_TABLE	ADEOS_sensor[];

extern FA_FILEDEF 	NASDA_filedef_subrecord_opln ;
extern FA_FILEDEF 	NASDA_filedef_subrecord_opl1 ;
extern FA_FILENAME	NASDA_files[] ;

/* 
-- The following is defined here only because it is not included in 
-- the FA_FILENAME NASDA_files[] array.
*/
extern FA_FILEDEF 	NASDA_filedef_secondary_reqm ;
extern VALUE_DEFS   NASDA_valuedefs_secondary_reqm[] ;

/* FOR PHASE UTILITIES  */
#include "phase_utilities.h"

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface definitions    */

/* FOR APS MACROS, etc...    */
#include "dapps_defs.h"

extern DBPROCESS *FA_dtkf_dbproc;
 
/* FLAG VALUES    */
#define SEARCH_FORWARD        1
#define SEARCH_BACKWARD      -1 

	/* stgs defines, used by stgs_default_strings */
#define  AADEOS    0
#define  ASF      1
#define  HEOC     2
#define  RECLEN   3
#define  FFVDATE  4
#define  FFVNUM   5
#define  ALL      6
#define  A        7
#define  NEWLIN   8
#define  ASTERISK_RSP 9
 
/*  reqq defines, used by reqq_default_strings  */
#define  REQQ     0
#define  ERS1     1
#define  HMMO     2
#define  N        3
#define  RECSIZ   4
 
/*  reqw defines, used by reqw_default_strings  */
#define  REQW     0
#define  ERS1W    1
#define  HMMOW    2
#define  NW       3
#define  RECSIZW  4
 
/* msge defines, used by msge_default_strings */
#define  HEADER_DATE    0
#define  HEADER_MSG     1
#define  HEADER_INFO    2
#define  HEADER_FDATE   3
#define  HEADER_FTIME   4
#define  HEADER_INFO2   5
#define  HEADER_DESCR   6
#define  HEADER_REP_NO  7
#define  HEADER_REP_LEN 8
#define  HEADER_OBS_NO  9
#define  HEADER_OBS_LEN 10
#define  HEADER_LIST    11
#define  RECORD_TITLE   12
#define  RECORD_PLAN_ID 13
#define  RECORD_REPDATE 14
#define  RECORD_RSPINIT 15
#define  RECORD_RSPSTOP 16
#define  RECORD_TRAILER 17
#define  MSGE_TRAILER   18


/* CONDITION CODES    */
#define NASDA_ASFTIME_RSP_ANGLE_2_REV_TIME_OK    1
#define NASDA_REV_ASFTIME_2_RSP_ANGLE_OK         1
#define NASDA_PHASE_REV2RSP_OK                   1
#define NASDA_PHASE_RSP1_OK                      1
#define NASDA_PHASE_RSP2FIRSTREV_OK              1
#define NASDA_ASFDATE_RSP_2_REV_OK               1

/* END OF CONDITION CODES    */

/* FUNCTION PROTOTYPES   */

int NASDAc_asfdate_rsp_2_rev(
    char        *sat,           /* input NASDA satellite                */
    char        *asf_date,      /* input time.                          */
    int         rsp,            /* input rsp                            */
    int         *rev ) ;        /* output rev number                    */

int NASDAc_asftime_rsp_angle_2_rev_time(
	char        *sat,           /* input NASDA satellite                */
	char        *asftime,       /* input time.                          */
	int         rsp,            /* input rsp                            */
	double      rsp_angle,      /* input angle                          */
	int         search_flag,    /* SEARCH_FORWARD or SEARCH_BACKWARD    */
	int         *rev,           /* output rev number                    */
	char        *asftime_out ); /* output asftime                       */

int NASDAc_phase_validate_rsp(
	DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
	int         rsp ) ;

int NASDAc_phase_check_rsp(
	DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
	int        rsp ) ;

int NASDAc_phase_rev2rsp(
	DB_RECORD	**phase_rec,    /* input phase record with info.        */
	int         rev,            /* input rev within phase.              */
	int         *rsp ) ;        /* output RSP path for input rev.       */

int NASDAc_phase_rsp2firstrev(
    DB_RECORD   **phase_rec,    /* phase record which has the data to use. */
    int         input_rsp,      /* input rsp                               */
    int         *output_rev ) ; /* output - first rev in phase with rsp.   */

int NASDAc_phase_check_rsp(
     DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
     int        rsp ) ;

int NASDAc_phase_rsp1(
    DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
    int         *rev1,          /* output rev number of first rev in phase. */
    int         *rsp1 ) ;       /* output rsp of first rev in phase.        */

int NASDAc_rev_asftime_2_rsp_angle(
	char    	*sat,	  		/* input NASDA satellite   			*/
	char		*asftime, 		/* input time.   					*/
	int     	rev,     		/* input rev number     			*/
	int			*rsp,	  		/* output rsp 						*/
	double		*rsp_angle ); 	/* output angle 					*/

int NASDAc_default_subrecords(
	void    *unused_pointer,
	char    *unused_character_pointer,
	int     *opln_subrecords ) ;

int NASDAc_default_observation_size(
	void    *unused_pointer,
	char    *unused_character_pointer,
	int     *opln_observation_recsize ) ;

int NASDAc_default_reqm_size(
	void    *unused_pointer,
	char    *unused_character_pointer,
	int     *reqm_recsize ) ;

int NASDAc_default_darid(
	void    *unused_pointer,
	char    *unused_character_pointer,
	int     *default_darid ) ;

int NASDAc_default_notes(
	void    *unused_pointer,
	char    *unused_character_pointer,
	char    *notes ) ;

int NASDAc_date_time2asftime(
	void	*unused_pointer,
	char 	*YY_MM_DD_hh_mm_ss,  /* YY-MM-DD hh:mm:ss       */
	char 	*asftime ) ;         /* yyyy:ddd:hh:mm:ss.sss   */

int NASDAc_yyyymmdd_hhmmss2asf(
	void	*unused_pointer,
	char 	*yyyymmdd,           /* yyyymmdd                */
	char 	*asftime ) ;         /* yyyy:ddd:hh:mm:ss.sss   */

int NASDAc_yyyymmdd2asftime(
	void	*unused_pointer,
	char 	*yyyymmdd,           /* yyyymmdd                */
	char 	*asftime ) ;         /* yyyy:ddd:hh:mm:ss.sss   */

int NASDAc_yyyymmdd_hhmmss2rev(
	void		*unused_pointer,
	char		*yms,  			/* YYYYMMDD hh:mm:ss       */
	int			*rev ) ;		/* yyyy:ddd:hh:mm:ss.sss   */

int NASDAc_link_time2strttime(
	void        *unused_pointer,
	char        *YYYYMMDDhh_mm_sshh_mm_ss, /* YYYYMMDDhh:mm:sshh:mm:ss  */
	char        *strttime ) ;              /* yyyy:ddd:hh:mm:ss.sss     */

int NASDAc_link_time2stoptime(
	void        *unused_pointer,
	char        *YYYYMMDDhh_mm_sshh_mm_ss, /* YYYYMMDDhh:mm:sshh:mm:ss  */
	char        *stoptime ) ;              /* yyyy:ddd:hh:mm:ss.sss     */

int NASDAc_link_time2rev(
	void        *unused_pointer,
	char        *YYYYMMDDhh_mm_sshh_mm_ss, /* YYYYMMDDhh:mm:sshh:mm:ss  */
	int      	*rev ) ;                   /* Rev number for J1 satellite  */

int NASDAc_headertime2asftime(
	void        *unused_pointer,
	char        *YYYYMMDDhh_mm_ss, /* YYYYMMDDhh:mm:sshh:mm:ss  */
	char        *asftime ) ;       /* yyyy:ddd:hh:mm:ss.sss     */

int NASDAc_inherit_actid(
	void        *unused_pointer,
	char        *unused_string ,
	char        *actid ) ;

int NASDAc_date_rsp2strttime(
	void    *unused_pointer,
	char    *date_rsp,          /* YYYYMMDDNNNXXX.XXNNNXXX.XX        */
	char    *strttime ) ;       /* yyyy:ddd:hh:mm:ss.sss    */

int NASDAc_date_rsp2stoptime(
	void    *unused_pointer,
	char    *date_rsp,          /* YYYYMMDDNNNXXX.XXNNNXXX.XX        */
	char    *stoptime ) ;       /* yyyy:ddd:hh:mm:ss.sss    */

int NASDAc_date_rsp2rev(
	void    *unused_pointer,
	char    *date_rsp,          /* YYYYMMDDNNNXXX.XXNNNXXX.XX        */
	int  	*rev ) ;  

int NASDAc_check_fadtkid(
    void    *unused_pointer,
    char    *fa_dtkid_string,
    char    *unused_pointer2 ) ;

int NASDAc_reqa_rsp_date2strttime(
	void    *unused_pointer,
	char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD        */
	char    *strttime ) ;       /* yyyy:ddd:hh:mm:ss.sss    */

int NASDAc_reqa_rsp_date2stoptime(
	void    *unused_pointer,
	char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD        */
	char    *stoptime ) ;       /* yyyy:ddd:hh:mm:ss.sss    */

int NASDAc_reqa_rsp_date2rev(
	void    *unused_pointer,
	char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD        */
	int  	*rev ) ;  

int NASDAc_reqa_rsp_date2trig(
	void    *unused_pointer,
	char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD        */
	int  	*trigger ) ;  

int NASDAc_query_subrecord(
	void    *unused_pointer,
	char    *unused_pointer2,
	int		*subrecords  ) ;

int NASDAc_inherit_fa_sch_link(
	void        *unused_pointer,
	char        *unused_pointer2,
	char        *schedule_link ) ;

int NASDAc_populate_sch_link(
    void        *unused_pointer,
    char        *sensor_segment_strings,
    char        *unused_destination_pointer ) ;

int NASDAc_search_inherit_link(
    void        *unused_pointer,
    char        *sensor_segment_key,
    char        *schedule_link ) ;

/*  stgs function prototypes  */

int stgs_filename( char *dtk_notes , char *buf , int length );
int file_creation_date( char *unusedptr , char *buf , int length );
int inttoascii( int *integer , char *buf , int length );
int inttoblankascii( int *integer , char *buf , int length );
int c2yyyymmdd( char *stxxtime , char *buf , int length );
int beginendrsp( char *dtk_notes , char *buf , int length) ;
int temp_revecho( char *dtk_notes , char *buf , int length) ;

/*  reqq function prototypes  */

int file_creation_date_ns( char *unusedptr , char *buf , int length );
int reqq_set_trigger    (int *trigger,  char *unusedptr, int unused_length) ;
int sensorfld(char *sensor , char *buf , int length );
int opsgainmode(char *sensor , char *buf , int length );
int calcrsp( int *dtk_rev , char *buf , int length );

/* msge function prototypes */

int write_current_datetime	( char *arg1, char *arg2, int arg3 );
int write_date_mask_yyyymmdd( char *arg1, char *arg2, int arg3 );
int write_time_mask			( char *arg1, char *arg2, int arg3 );
int write_begin_rsp			( char *arg1, char *arg2, int arg3 );
int write_end_rsp			( char *arg1, char *arg2, int arg3 );
int write_inttoblankascii	( int  *arg1, char *arg2, int arg3 );

#endif /* _NASDAconversions_ */
