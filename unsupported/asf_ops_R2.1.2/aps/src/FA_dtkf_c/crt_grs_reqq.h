#ifndef CRT_GRS_REQQ_H
#define CRT_GRS_REQQ_H

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif


/*==============================================================================
Filename:       crt_grs_reqq.h
Description:    
Creator:        Brian J Griglak
Notes:          
==============================================================================*/

#pragma ident	"@(#)crt_grs_reqq.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.crt_grs_reqq.h"


/*
    _/_/_/    _/_/  _/    _/ _/_/_/_/ _/_/_/ _/    _/ _/_/_/  _/_/_/ 
   _/   _/  _/  _/ _/    _/    _/      _/   _/_/  _/ _/     _/      
  _/_/_/   _/  _/ _/    _/    _/      _/   _/ _/ _/ _/_/_/  _/_/   
 _/   _/  _/  _/ _/    _/    _/      _/   _/  _/_/ _/         _/  
_/    _/  _/_/   _/_/_/     _/    _/_/_/ _/    _/ _/_/_/ _/_/_/  

*/
#include "list_handling.h"    /* for PATH   */

void REQQ_append(
    FILE *reqq_fp,     /* reqq file pointer        */
    long  dar,         /* input dar.             */
    PATH  *head,       /* input grs list.        */
    int   outphase,    /* reqq_phase number.     */
    int   outimage,    /* number of images (number of requests) for each   */
                       /* GRS in the input grs list (head)                 */
    char  *observe_period[],
        /*
        -- Note:  observe_period[] has n entries, where n = outimage.  Each of
        --        these entries is a time bracket in this format:
        --        "yyyymmddYYYYMMDD"      which is the desired REQQ format.
        */
    FILE  *logfp ) ;

void boundingbox(double nw[2], double ne[2], double se[2], double sw[2],
    double lat[2], double lon[2]) ;

int create_grs_list( long darid, PATH *grslist, FILE *logfp  )  ;

int crt_grs_reqq(
    FILE    *reqq_fp,      /* pointer to REQQ file.                   */
    FILE    *logfp,        /* log file.  could be stdout              */
    char    *phase_start,  /* start time of request period (phase).   */
    char    *phase_end,    /* end time of request period (phase)      */
    int     phase ) ;      /* phase number                            */

/* returns number of time periods calculated.  */
int get_observe_times(
    char *phase_start,   /* input phase time period.  */
    char *phase_end,
    char *dar_start,     /* input dar time period.    */
    char *dar_end,
    int  J1_obs_freq_days,/* input no. days per image during dar time period.*/
    char *observe_time[],  /* array to store all the time brackets     */
                           /* each member is like this:  yyyymmddYYYYMMDD   */
                           /* providing the start and end of time period.   */
    int  observe_time_size ); /* number of members in array observe_time[] */

int load_dar(
    long      darid,
    char      *shapestr,
    double    *nw,
    double    *ne,
    double    *se,
    double    *sw,
    FILE      *logfp ) ;

void encode(long digit, char *digitchar) ;
long decode(char digitchar) ;

void reqqid_encode(
    long darid,
    int path,
    int row,
    int phase,
    int image,
    char reqqid[]  ) ;

void reqqid_decode(
    char reqqid[],
    long *darid,
    int *path,
    int *row,
    int *phase,
    int *image  ) ;

int station_mask_flag(
    char shape,
    double dar_radius,
    double dar_nw[2],
    double dar_ne[2],
    double dar_se[2],
    double dar_sw[2],
    double station_xyz[3],
    double *radius,
    FILE *logfp  ) ;

int thin(int row) ;

void latlon2grs(double lat_D, double lon_0, int *path, int *row) ;

/* routines in lib_APSmath    */
/*
        _/     _/_/_/   _/_/_/         _/    _/     _/     _/_/_/_/ _/  _/ 
      _/_/    _/   _/ _/              _/_/_/_/    _/_/       _/    _/  _/ 
    _/  _/   _/_/_/   _/_/           _/ _/ _/   _/  _/      _/    _/_/_/ 
  _/_/_/_/  _/          _/          _/    _/  _/_/_/_/     _/    _/  _/ 
_/      _/ _/      _/_/_/          _/    _/ _/      _/    _/    _/  _/ 
*/
extern void ll2xyz_(double *lat_lon, double *xyz) ;
extern void gcdist_(double *dist, double *xyz1, double *xyz2 ) ;
extern void gcpinq_(
    int    *dist, 
    double *xyz1, 
    double *xyz2, 
    double *xyz3, 
    double *xyz4, 
    double *xyz_test ) ;

extern void gcentc_(
    int    *iflag, 
    double *xin, 
    double *xyz1, 
    double *xyz2, 
    double *center_xyz,
    double *radius ) ;

extern void sangld_(
    double *adeg,     /* angle in degrees.  */
    double *xyz1, 
    double *xyz2, 
    double *xyz3 ) ;

extern void mps_cgen_(
    float *lat,           /* input lat of circle center.  */
    float *lon,           /* input lon of circle center.  */
    float *radius_km,     /* input circle radius          */
    int   *n_points,        /* input number of desired points on the circle */
    float *latitudes,     /* output latitudes     */
    float *longitudes  ) ;/* output longitudes    */

extern void mpsintrp_(
    float *p1lat,         /* input lat of first point.  */
    float *p1lon,         /* input lon of first point.  */
    float *p2lat,         /* input lat of second point.  */
    float *p2lon,         /* input lon of second point.  */
    int   *n_points,      /* input number of desired points incl p1, p2  */
    float *latitudes,     /* output latitudes     */
    float *longitudes  ) ;/* output longitudes    */

/*

       _/_/_/ _/_/_/   _/_/_/    _/_/  _/_/_/    _/_/_/ 
      _/     _/   _/  _/   _/  _/  _/ _/   _/  _/      
     _/_/_/ _/_/_/   _/_/_/   _/  _/ _/_/_/    _/_/   
    _/     _/   _/  _/   _/  _/  _/ _/   _/      _/  
   _/_/_/ _/    _/ _/    _/  _/_/  _/    _/ _/_/_/  

*/

/* use this to create a string from error code:  */

extern char *calc_grs_errors[] ;

#define CALC_GRS_ERROR_MESSAGE( code ) \
           calc_grs_errors[ -(code) ]

#define LOAD_DAR_ERROR_BAD_ENDTIME                                 -1
#define LOAD_DAR_ERROR_BAD_STRTTIME                                -2
#define LOAD_DAR_ERROR_DARID_LE_ZERO                               -3
#define LOAD_DAR_ERROR_DAR_NOT_FOUND                               -4
#define LOAD_DAR_ERROR_DAR_PERMISSION_NOT_GRANTED                  -5
#define LOAD_DAR_ERROR_DB_QUERY_ERROR                              -6
#define LOAD_DAR_ERROR_DTKSTAT_NOT_PLN_SUB_OR_SCH                  -7
#define LOAD_DAR_ERROR_GT_ONE_DAR_FOUND                            -8
#define LOAD_DAR_ERROR_PLANNING_PERMISSION_NOT_GRANTED             -9
#define LOAD_DAR_ERROR_SAT_NOT_J1                                 -10
#define LOAD_DAR_ERROR_SENSOR_NOT_SAR                             -11
#define LOAD_DAR_ERROR_BAD_SHAPE                                  -12
#define LOAD_STATION_ERROR_DB_QUERY_ERROR                         -13
#define LOAD_STATION_ERROR_STATION_NOT_FOUND                      -14
#define LOAD_STATION_ERROR_GT_ONE_STATION_FOUND                   -15
#define STATION_MASK_FLAG_CIRCLE_IN_MASK                          -16
#define STATION_MASK_FLAG_QUAD_IN_MASK                            -17
#define STATION_MASK_FLAG_MASK_IN_QUAD_DAR                        -18
#define REQQ_PHASE_ERROR_PHASES_DO_NOT_MATCH                      -19
#define CALC_GRS_ERROR_NO_GRS_COORDS                              -20
#define STATION_MASK_FLAG_QUAD_OVERLAPS_MASK                      -21
#define STATION_MASK_FLAG_CIRCLE_OVERLAPS_MASK                    -22
#define LOAD_DAR_ERROR_CHECK_LOG_FILE                             -23
#endif  /* CRT_GRS_REQQ_H    */




