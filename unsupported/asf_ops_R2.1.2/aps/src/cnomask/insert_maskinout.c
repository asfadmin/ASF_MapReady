#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       insert_maskinout.c

Description:    insert_maskinout()

External Functions Defined:
                insert_maskinout()
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
==============================================================================*/
#pragma ident   "@(#)insert_maskinout.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/cnomask/SCCS/s.insert_maskinout.c"

#include <string.h>
#include <stdlib.h>
 
/* FOR SYBASE INTERFACES  */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_maskinout.h"   /* for the maskinout relation.              */


/*==============================================================================
Function:       insert_maskinout()

Description:    GIVEN A SAT, station, rev, time, and enter/exit data, etc,
                insert a maskinout record which is a satellite 
                enter/exit mask event.  

Parameters:     

   Input Parameters:
   Name         Type    Definition
   APS_dbproc   *DBPROCESS  pointer to a previously established 
                Sybase process.  
   stationid    *CH*3    station NAME:  ASF etc.
   sat          *CH*2    SATELLITE NAME:  E1, J1, OR RS, etc.
   rev      *int    rev number of the data
   ejdate   *real*8 time (ephemeris Julian date) of the data, real 
            Julian days ephemeris time.
   inout    *CH*3   the value is IN or OUT according to whether the 
            satellite is going in or out of the mask.
 
   Output Parameters:
   nrecs    *int    the number of records appended.  normally 1.
            the run must terminate if the insert fails.

Returns:        void

Creator:        Lawrence Stevens

Creation Date:  Mon Oct  2 14:50:58 PDT 1995

Notes:      
    This routine is called from Fortran.
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
void insert_maskinout(
    DBPROCESS   *APS_dbproc, 
    char        *stationid,
    char        *sat, 
    int     *rev,
    double      *ejdate,
    char        *inout,

    int     *nrecs  )
{

DB_RECORD       **maskinout_rec ;


*nrecs = 0;

#ifdef DEBUG
printf("insert_maskinout:  APS_dbproc = %x\n", APS_dbproc );
printf("     stationid = %3.3s, sat = %2.2s, rev = %d, \
 ejdate = %20.9f\n", stationid, sat, *rev, *ejdate );
printf("     inout = %3.3s\n", inout);
#endif

/* create a DB_RECORD, put in the values, then call db_insert_single_record.  */
maskinout_rec =  new_table_record(APS_CDEFS(MASKINOUT)) ;

strcpy( CAST_MASKINOUT_STATIONID maskinout_rec[MASKINOUT_STATIONID], 
    stationid ) ;
CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE] = *ejdate ;
strcpy( CAST_MASKINOUT_SAT maskinout_rec[MASKINOUT_SAT], sat ) ;
CAST_MASKINOUT_REV maskinout_rec[MASKINOUT_REV] = *rev ;
strcpy( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT], inout ) ;

#ifdef DEBUG
printf("\ninsert_maskinout:  maskinout record = \n" ) ;
db_print_record(maskinout_rec, APS_CDEFS(MASKINOUT) ) ;
#endif

*nrecs = db_insert_single_record(APS_dbproc,
    maskinout_rec, APS_TABLE(MASKINOUT), APS_CDEFS(MASKINOUT) ) ;

/* free the DB_RECORD memory.  */
free_db_record(maskinout_rec) ;

if(*nrecs != 1)
{
        printf("\n\ninsert_maskinout: INSERT FAILED for maskinout relation.\n");
        printf("              check to see that the relation exists.\n");
        printf("              Also check to see that there is only  \n");
        printf("              one run for this satellite going at a time.\n");
        printf("              satellite = %s\n", sat);
        printf("              nrecs = %d\n", *nrecs);
        printf("              Now terminating this run.\n");
        (void) system("banner ERROR");
        exit(1);
}

#ifdef DEBUG
printf("insert_maskinout:  returning:   nrecs = %d\n", *nrecs);
#endif

}
