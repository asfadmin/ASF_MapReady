/* *********************************************************
**
** File:        ims_DLtoDTKAux.h
**
** Function:    Include file for ims_DLtoDTKAux.c
**
** Author:      David Pass
**
** Date:        3/13/97
**
************************************************************ */

#ifndef _IMS_DLTODTK_H
#define _IMS_DLTODTK_H

static char *sccsDLtoDTKAux = "@(#)ims_DLtoDTKAux.h	1.2  05/15/97";


/*
** Define the datatake structure.  this is referenced in the
**      downlink structure, and is a linked list.
*/
typedef  struct  ims_datatake__t *pnt_datatake_t;
typedef  struct  ims_datatake__t {
    char  platform[3];  /* platform: shuold be same as datatakes */
    char  sensor[2]; /* set unknown Z for downlink  */
    int   revolution;
    short sequence; /*  plat, sensor, rev, seq are unique: aps  */
    char  dt_sensor[2]; /* should be correct value  */
    int   dt_revolution; /* should be same as downlink */
    short dt_sequence; /*  plat, sensor, rev, seq are unique: aps  */
    char  dt_platform[3];  /* datatake platform: shuold be same as
                downlink */
    char  time_on[22]; /* all time_on, off values in datatakes must
                be within downlink values,  can be outside */
    char  time_off[22];
    char  site_name[31];
    char  mode[6];
    char  quicklook_flag[4];
    char  process_auth_flag[4];
    char  frame_mode[11];
    pnt_datatake_t  pnt_next;
} ims_datatake_t;


/*
** Define the downlink structure.  this points to the first
**      datatake structure.
*/
typedef  struct  ims_downlink__t *pnt_downlink_t;
typedef  struct  ims_downlink__t {
    char  platform[3];  /* platform: shuold be same as datatakes */
    char  sensor[2]; /* set unknown Z for downlink  */
    int   revolution;
    short sequence; /*  plat, sensor, rev, seq are unique: aps  */
    char  time_on[22]; /* all time_on, off values in datatakes must
                be within these times,  not necessarily all within */
    char  time_off[22];
    char  activity_id[11];
    char  station_id[5];
    char  antenna_id[17];
    char  transmitter_id[17];
    char  fa_schedule_link[12];
    char  time_aos[22];
    char  time_los[22];
    char  downlink_status[11];
    int   number_of_dtk_entry;  /*  number of dtatake links  */
    pnt_datatake_t  pnt_dtk_1st; /* pointer to first datatake */
    pnt_downlink_t  pnt_dl_next; /* pointer to next downlink */
} ims_downlink_t;



#endif  /* !_IMS_DLTODTK_H */
