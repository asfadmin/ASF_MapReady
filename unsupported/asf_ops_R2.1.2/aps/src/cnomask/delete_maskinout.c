#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       delete_maskinout.c

Description:    has routine delete_maskinout()

==============================================================================*/
#pragma ident   "@(#)delete_maskinout.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/cnomask/SCCS/s.delete_maskinout.c"



/*==============================================================================
Function:       delete_maskinout()

Description:    GIVEN A SAT, rev bracket and time bracket, 
                this routine deletes maskinout static data

Parameters:
  Input Parameters:
  Name         Type    Definition
   APS_dbproc   *DBPROCESS  pointer to a previously established 
            Sybase process.  
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   rev1     *int    rev1 and rev2 delimit the rev bracket.  The revs 
   rev2         *int    rev1 and rev2 are included in the bracket.  
   t1       *double t1 and t2 delimit the time bracket.  The times
   t2       *double are NOT included in the time bracket.  
 
   Output Parameters:
   n_maskinout  *int    the  number of records deleted from the maskinout
                        relation.  

Creator:        Lawrence Stevens

Creation Date:  Fri May  2 22:37:44 PDT 1997

==============================================================================*/
#include <string.h>
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */

#include "db_maskinout.h"           /* for the maskinout relation.              */
 
void delete_maskinout(
    DBPROCESS   *APS_dbproc, 
    char        *SAT, 
    int         *rev1,
    int         *rev2,
    double      *t1,
    double      *t2,
    DBINT       *n_maskinout      )
{

DBCHAR      sat[3];

/* initialize output record counts in case of an early return. */
*n_maskinout = 0;

/* for safety,  copy the Fortran strings locally and supply 
   the string termination.  */

strncpy(sat, SAT, 2);
sat[2] = '\0';

printf("delete_maskinout:  sat = %s,\n", sat );
printf("             rev1 = %d, rev2 = %d, \n", *rev1, *rev2);
printf("             t1 = %20.9f, t2 = %20.9f\n", *t1, *t2);

sprintf(where_clause, "where %s = '%2.2s' and \
    %s >= %d and %s <= %d and \
    %s > %20.9f and %s < %20.9f", 
    APS_COL(MASKINOUT, MASKINOUT_SAT), sat,    
    APS_COL(MASKINOUT, MASKINOUT_REV), *rev1,  
    APS_COL(MASKINOUT, MASKINOUT_REV), *rev2, 
    APS_COL(MASKINOUT, MASKINOUT_EJDATE), *t1, 
    APS_COL(MASKINOUT, MASKINOUT_EJDATE), *t2 );

printf("delete_maskinout:  where_clause = >%s<\n", where_clause);

*n_maskinout = db_delete_records( APS_dbproc, APS_TABLE(MASKINOUT),  
    where_clause ) ;

printf("delete_maskinout:  returning:  n_maskinout = %d \n", 
    (int) *n_maskinout );

}
