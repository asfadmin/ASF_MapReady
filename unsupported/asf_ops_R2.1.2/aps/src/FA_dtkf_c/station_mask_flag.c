#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       station_mask_flag.c

Description:    

External Functions Defined:

Notes:
        This file written with a 4-character tab setting. 

==============================================================================*/
#pragma ident	"@(#)station_mask_flag.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.station_mask_flag.c"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <db_sybint.h>      /* for DBPROCESS, where_clause, etc.. */
#include <aps_db_table.h>   /* for DAR, DTK */
#include <db_station.h>
#include <dapps_list.h>

#include "crt_grs_reqq.h"

int station_mask_flag(
    char shape,
    double dar_radius,
    double dar_nw[2],
    double dar_ne[2],
    double dar_se[2],
    double dar_sw[2],
    double station_xyz[3],
    double *radius,
    FILE *logfp  )
{
    DB_RECORD **station_rec;
    llist *station_list;
    cursor *station_list_ptr;
    double earth_radius = 6356.757;
    double station_center[2];
    double station_radius;
    double nwxyz[3];
    double nexyz[3];
    double sexyz[3];
    double swxyz[3];
    double dist;
    int    inflag;
    double xin[3];
    int    tally = 0;

    (void) sprintf(where_clause, "where %s = 'ASF' and %s = 'J1'",
        APS_COL(STATION, STATION_STATIONID),
        APS_COL(STATION, STATION_SAT));

    station_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(STATION),  where_clause, NULL, APS_CDEFS(STATION), ALL_COLS);

    if (station_list == NULL)
        return LOAD_STATION_ERROR_DB_QUERY_ERROR;

    if( NUMELTS(station_list ) <= 0 )
    {
        (void) printf( "ERROR:  %s %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_STATION_ERROR_STATION_NOT_FOUND),
                where_clause );
        DEL_LIST(station_list);
        return LOAD_STATION_ERROR_STATION_NOT_FOUND;
    }

    if( NUMELTS( station_list ) > 1 )
    {
        if(logfp)
            (void) printf( "ERROR:  %s %s\n", 
               CALC_GRS_ERROR_MESSAGE( LOAD_STATION_ERROR_GT_ONE_STATION_FOUND),
                where_clause ) ;
        DEL_LIST( station_list ) ;
        return LOAD_STATION_ERROR_GT_ONE_STATION_FOUND ;
    }
    station_rec = FIRST(station_list, station_list_ptr);

    station_center[0] = CAST_STATION_STLAT station_rec[STATION_STLAT];
    station_center[1] = CAST_STATION_STLON station_rec[STATION_STLON];
    station_radius = CAST_STATION_STRADIUSKM station_rec[STATION_STRADIUSKM];

    DEL_LIST(station_list);

    /* Circle */
    if (shape == 'P')
    {
        *radius = station_radius / earth_radius;
        ll2xyz_(dar_nw, nwxyz);
        ll2xyz_(station_center, station_xyz);
        gcdist_(&dist, nwxyz, station_xyz);
        dist *= earth_radius;
        if (dist > dar_radius + station_radius)
            return 1;
        else if (dist < station_radius - dar_radius)
            return STATION_MASK_FLAG_CIRCLE_IN_MASK;
        else
            return STATION_MASK_FLAG_CIRCLE_OVERLAPS_MASK;
    }
    else if (shape == 'Q')
    {
        *radius = station_radius / earth_radius;
        ll2xyz_(dar_nw, nwxyz);
        ll2xyz_(station_center, station_xyz);
        gcdist_(&dist, nwxyz, station_xyz);
        if (dist < *radius)
            tally++;

        ll2xyz_(dar_ne, nexyz);
        gcdist_(&dist, nexyz, station_xyz);
        if (dist < *radius)
            tally++;

        ll2xyz_(dar_se, sexyz);
        gcdist_(&dist, sexyz, station_xyz);
        if (dist < *radius)
            tally++;

        ll2xyz_(dar_sw, swxyz);
        gcdist_(&dist, swxyz, station_xyz);
        if (dist < *radius)
            tally++;

        if (tally == 4)
            return STATION_MASK_FLAG_QUAD_IN_MASK;
        if (tally > 0)
            return STATION_MASK_FLAG_QUAD_OVERLAPS_MASK;

        gcentc_(&inflag, xin, nwxyz, nexyz, station_xyz, radius);
        if (inflag != 0)
            tally++;

        gcentc_(&inflag, xin, nexyz, sexyz, station_xyz, radius);
        if (inflag != 0)
            tally++;

        gcentc_(&inflag, xin, sexyz, swxyz, station_xyz, radius);
        if (inflag != 0)
            tally++;

        gcentc_(&inflag, xin, swxyz, nwxyz, station_xyz, radius);
        if (inflag != 0)
            tally++;

        if (tally > 0)
            return STATION_MASK_FLAG_QUAD_OVERLAPS_MASK;

        gcpinq_(&inflag, nwxyz, nexyz, sexyz, swxyz, station_xyz);
        if (inflag != -1)
            return STATION_MASK_FLAG_MASK_IN_QUAD_DAR;

        return 1;
    }
    else
        return LOAD_DAR_ERROR_BAD_SHAPE;
}
        



