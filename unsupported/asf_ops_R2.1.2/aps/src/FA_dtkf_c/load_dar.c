#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       load_dar.c

Description:    contains load_dar()

External Functions Defined:

Notes:
        This file written with a 4-character tab setting. 

==============================================================================*/
#pragma ident	"@(#)load_dar.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.load_dar.c"

#include <stdio.h>
#include <stdlib.h>         /* for exit() etc.                            */
#include <string.h>         /* for strcmp() etc.                          */
#include <db_sybint.h>      /* for DBPROCESS                              */
#include <timeconv.h>       /* for tc_validate_asf_datetime() etc.        */
#include <phase_utilities.h>/* for asftime2rev(), asftime_2_phase() etc.  */
#include <aps_db_table.h>   /* for DAR, DTK                               */
#include <db_dar.h>         /* for CAST_DAR_SAT etc.                      */
#include "crt_grs_reqq.h"   /* for CALC_GRS_ERROR_MESSAGE() etc.       */
#include <aps_log_msg.h>    /* for APS_CRITICAL, etc.                     */
#include <mu_utilities.h>   /* for MU_DAR_ACTIVITY_TYPE, etc.             */


/*==============================================================================
Function:      load_dar()

Description:   Given a DAR id number, enters the APS database and pulls out
                 all the information of the DAR.  It then returns the geometry
                 important to the dar2grs() routines: shape, lat/lon of 
                 four corners, and radius.  These numbers cannot all be non-
                 zero, because circular ragion (shape='P') uses northwest
                 coords as it's center coords, and the other three corners
                 are zero's, and for quadrilaterals, the radius is zero.
               Note: for circles, radius is returned under variable name
                 ne[0].

Parameters:
    Input Parameters:
    darid               long           the id number for desired DAR

    Output Parameters:
    shapestr            char*          pointer to char for shape, pass
                                         the address of normal char

    nw, ne, se, sw      float*[2]      array of pointers for lat/lon
                                         coords.  if shape='P', then
                                         ne[0] is radius

    observe_time        *char []       double pointer to string holding the
                                         observation period of the DAR in it.
                                         passed back to main, then into
                                         REQQ_append, because needed for
                                         each record.

Returns:       int

Creator:       Brian J Griglak

Creation Date: Mon Jul 21 16:05:57 PDT 1997
==============================================================================*/

int load_dar(
    long      darid,
    char      *shapestr,
    double    *nw,
    double    *ne,
    double    *se,
    double    *sw,
    FILE      *logfp )


{
    DB_RECORD **dar_rec;  /* rest of this copied from other code, including */
    llist     *dar_list;
    cursor    *dar_list_ptr;
    char      shape;

    if (darid <= 0)
    {
        if( logfp ) 
            (void) fprintf( logfp, "ERROR:  %s %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_DARID_LE_ZERO),
                where_clause ) ;
        return LOAD_DAR_ERROR_DARID_LE_ZERO;
    }

    (void) sprintf(where_clause, "where %s = %ld ",
        APS_COL(DAR, DAR_DARID), darid);

    dar_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DAR),
        where_clause, NULL, APS_CDEFS(DAR), ALL_COLS);

    if (dar_list == NULL)
    {
        if( logfp ) 
            (void) fprintf( logfp, "ERROR:  %s %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_DB_QUERY_ERROR),
                where_clause ) ;
        return LOAD_DAR_ERROR_DB_QUERY_ERROR ;
    }

    if( NUMELTS( dar_list ) <= 0 )
    {
        if( logfp )
            (void) fprintf( logfp, "ERROR:  %s %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_DAR_NOT_FOUND),
                where_clause ) ;
        DEL_LIST( dar_list ) ;
        return LOAD_DAR_ERROR_DAR_NOT_FOUND ;
    }

    if( NUMELTS( dar_list ) > 1 )
    {
        if( logfp )
            (void) fprintf( logfp, "ERROR:  %s %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_GT_ONE_DAR_FOUND),
                where_clause ) ;
        DEL_LIST( dar_list ) ;
        return LOAD_DAR_ERROR_GT_ONE_DAR_FOUND ;
    }

    dar_rec = FIRST(dar_list, dar_list_ptr) ;
/*
    if (logfp)
    { 
        (void) fprintf( logfp, "VALUES IN THE INPUT DAR RECORD:\n" ) ;
        db_fprint_record( logfp, dar_rec, APS_CDEFS(DAR) ) ;
        (void) fprintf( logfp, "\n" ) ;
    }
*/
    /* check satellite */
    if( strcmp( CAST_DAR_SAT dar_rec[DAR_SAT], "J1" ) != 0 )
    {
        if( logfp )
            (void) fprintf( logfp, "ERROR:  %s \nsat = %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_SAT_NOT_J1),
                CAST_DAR_SAT dar_rec[DAR_SAT] ) ;
        DEL_LIST( dar_list ) ;
        return LOAD_DAR_ERROR_SAT_NOT_J1;
    }

    /* check sensor */
    if( strcmp( CAST_DAR_SENSOR dar_rec[DAR_SENSOR], "SAR" ) != 0 )
    {
        if( logfp )
            (void) fprintf( logfp, "ERROR:  %s \nsat = %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_SENSOR_NOT_SAR),
                CAST_DAR_SENSOR dar_rec[DAR_SENSOR] ) ;
        DEL_LIST( dar_list ) ;
        return LOAD_DAR_ERROR_SENSOR_NOT_SAR;
    }

    /* check start time  */
    if ( tc_validate_asf_datetime( CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] )
         != TRUE )
    {
        /* error.  */
        if( logfp )
            (void) fprintf( logfp, "ERROR:  %s   \nstrttime = %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_BAD_STRTTIME ),
                CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] ) ;

        DEL_LIST( dar_list ) ;
        return LOAD_DAR_ERROR_BAD_STRTTIME;
    }

    /* check end time  */
    if ( tc_validate_asf_datetime( CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] )
         != TRUE )
    {
        /* error.  */
        if( logfp )
            (void) fprintf( logfp, "ERROR:  %s   \nendtime = %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_BAD_ENDTIME ),
                CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] ) ;

        DEL_LIST( dar_list ) ;
        return LOAD_DAR_ERROR_BAD_ENDTIME; 
    }

    shape = CAST_DAR_SHAPE dar_rec[DAR_SHAPE];
    shapestr[0] = (char) shape;

    if ((shape != 'P') && (shape != 'Q'))
    {
        if (logfp)
            (void) fprintf(logfp, "ERROR: %s \nshape = %c\n",
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_BAD_SHAPE), shape);
        DEL_LIST( dar_list );
        return LOAD_DAR_ERROR_BAD_SHAPE;
    }

    nw[0]  = CAST_DAR_NWLAT dar_rec[DAR_NWLAT];
    nw[1]  = CAST_DAR_NWLON dar_rec[DAR_NWLON];
    ne[0]  = CAST_DAR_NELAT dar_rec[DAR_NELAT];
    ne[1]  = CAST_DAR_NELON dar_rec[DAR_NELON];
    se[0]  = CAST_DAR_SELAT dar_rec[DAR_SELAT];
    se[1]  = CAST_DAR_SELON dar_rec[DAR_SELON];
    sw[0]  = CAST_DAR_SWLAT dar_rec[DAR_SWLAT];
    sw[1]  = CAST_DAR_SWLON dar_rec[DAR_SWLON];

    if (shape == 'P')
        ne[0] = CAST_DAR_RADIUS dar_rec[DAR_RADIUS] ;

    DEL_LIST( dar_list ) ;
    return 1;
}
