#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   tcycle.c

Description:    holds tcycle()

External Functions Defined:
tcycle()
    
Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident	"@(#)tcycle.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.tcycle.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>

#include "timeconv.h"
#include "phase_utilities.h"
#include "aps_db_table.h"


/*==============================================================================
Function:       tcycle()

Description:    Compute the start and end of the first ground track repeat 
                cycle within the input time bracket and within the current 
                phase.  also, compute the earlier of the end of phase and 
                the current time bracket.  See NOTES for special cases 
                and illustrations.  


Parameters:     
   Input Parameters:
   Name         Type    Definition
   dbproc   *DBPROCESS  sybase process info. 
   SAT      *CHAR*2 SATELLITE NAME
   TIME1    *REAL*8 FIRST TIME IN THE INPUT TIME INTERVAL
   TIME2    *REAL*8 LAST TIME IN THE INPUT TIME INTERVAL
   Output Parameters:
   Name         Type    Definition
   TIMEC1   *REAL*8 FIRST TIME IN ONE CYCLE STARTING WITH TIME1
            OR: THE FIRST TIME IN THE NEXT PERIOD
            OR: -1 IF NO COVERAGE AT ALL.  (BETWEEN PHASES)
            this would happen if there were no coverage in the 
            time bracket TIME1, TIME2.  
            IF NO RECS IN PHASE RELATION, THEN = TIME1.
   TIMEC2   *REAL*8 LAST TIME IN ONE CYCLE STARTING WITH TIME1
            OR: TIME2
            OR: TIMEP
            WHICHEVER OF THE THREE IS SMALLER.  
            IF NO RECS IN PHASE RELATION, THEN = TIME2.
   TIMEP    *REAL*8 LAST TIME IN THE CURRENT PHASE OR TIME2, 
            WHICHEVER OF THE TWO IS SMALLER.  
            IF NO RECS IN PHASE RELATION, THEN = TIME2.  
   TIMEP1   *REAL*8 FIRST TIME IN THE NEXT PHASE 
            OR :  -1 IF TIME2 IS NOT WITHIN THE NEXT PHASE
                    OR IF NO NEXT PHASE WAS FOUND.  
   CYDAYS   *INT    NUMBER OF DAYS IN THE CURRENT CYCLE
   CYREVS   *INT    NUMBER OF REVS IN THE CURRENT CYCLE

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Oct  6 15:11:48 PDT 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

        WITHIN THE INPUT TIME INTERVAL, COMPUTE THE FIRST AND LAST 
        TIME OF ONE CYCLE STARTING AT TIME1 (OR THE FIRST TIME IN THE 
        NEXT PHASE, IF THERE IS NO CURRENT PHASE), AND PROVIDE 
        THE LAST TIME OF THE CURRENT PHASE STARTING 
        AT TIME1.  NOTE THAT THE COMPUTED TIMES 
        COULD BE THE SAME, AND THAT THERE MIGHT NOT BE ENOUGH 
        TIME LEFT IN THE CURRENT PHASE TO ALLOW FOR ONE CYCLE, 
        AND THAT THE END OF THE PHASE MIGHT BE AFTER THE INPUT TIME 
        INTERVAL, IN WHICH JUST THE LAST TIME OF THE INTERVAL IS 
        RETURNED.  
 
        The above is cryptic, and I want to try again to explain
        this.  The routine is used as a driver for tcopy() in specific 
        site coverage (compute data-take opportunities).  

        tcopy(), when appropriate, previously calculated data-take 
        opportunities.  The replication is done according to the repeat 
        cycle of the current phase.  

ILLUSTRATION: 
    The following illustration shows the input parameters TIME1, TIME2 
    and the meaning of the output parameters TIMEC1, TIMEC2, and TIMEP.  
    This is the "normal" case; special cases are described later:  

    TIME-->

    input
    create_dtkopps
    time bracket:   | TIME1
    create_dtkopps  |                                                   | TIME2
    is to create    |                                                   | 
    sscvrg recs     |                                                   |
    in this time    |                                                   |
    bracket:        |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
                    |                                                   |
    however, it     |          |                                        | 
    first creates   |          |                                        | 
    them in this    |          |                                        | 
    one-cycle time  |          |                                        | 
    bracket:        |          |                                        | 
                    |          |                                        | 
    one cycle in the|          |                                        | 
    ground track    |          |                                        | 
    repeat cycle    |<--    -->|                                        | 
                    |          |                                        | 
    tcopy()         | 1st      |                                        | 
        replicates  | cycle    |                                        | 
        these data- |cccccccccc|   -->    ---.                          | 
        takes:      |          |              \                         | 
                    |          |               |                        | 
                    |          |               V                        | 
                    |          |               |                        | 
                    |          |               |                        | 
    into this time  |          |               V                        | 
        bracket:    |          |rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr| 
    using the ground|          |   2nd    |   3nd    |    4nd   | ...   | 
    track repeat    |          |  cycle   |  cycle   |   cycle  | cycle | 
    cycle info      |          |                                        | 
                    |          |                                        | 
    TIMEC1, TIMEC2, |          |                                        | TIMEP
    and TIMEP       |          | TIMEC2
    are computed    | TIMEC1
    here:           | 

    CONSTRAINTS:
    sscvrg records will be computed into (TIMEC1, TIMEC2) then these records 
    will be replicated into (TIMEC2, TIMEP)
    Therefore:  
    (TIMEC1, TIMEC2) must be inside the current phase.  
    (TIMEC1, TIMEC2) must be inside the create_dtkopps time bracket 
                    (TIME1, TIME2)
    TIMEP            must be inside the current time bracket and phase.  

    SPECIAL CASE OF 
    TIME BRACKET EXTENDING INTO THE NEXT PHASE:
    Note that if the create_dtkopps time bracket extends from the middle 
    of one phase into the middle of another phase, the ground track 
    repeat cycle will probably change.  For this reason the run is broken 
    up into separate runs.  

    input
    create_dtkopps
    time bracket:   | TIME1
    create_dtkopps  |                                                   | TIME2
    is to create    |                                                   | 
    sscvrg recs     |                                                   |
    in this time    |                                                   |
    bracket:        |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|
                    |                                                   |
                    |                                                   |
                    |                                                   |
                    |                                                   |
                    |                  | <-- end of phase               |
                    |                  |    | <-- start of next phase   |
                    |                  |    |                           |
                    |                  |    |                           |
                    |                  |    | TIMEP1                    |
    TIMEC1, TIMEC2, |        |         | TIMEP                          |
    and TIMEP       |        | TIMEC2  |    |                           |
    are computed    | TIMEC1 |         |    |                           |
    here:           |cccccccc|         |    |                           |
                    |        |         |    |                           |
                    |        |rrrrrrrrr|    |                           |
                    |        |         |    |                           |
    FIRST PART OF   |                  |    |                           |
    RUN             |111111111111111111|    |                           |
                    |                  |    |                           |
    SECOND PART OF  |                  |    |                           |
    RUN             |                  |    |222222222222222222222222222|

    TIMEP1 is the start of the next phase, needed only if the input time 
    bracket (TIME1, TIME2) extends into the next phase.  Otherwise, TIMEP1 
    is not needed and its value is -1.  

    CONSTRAINTS:
        It is not approprate to replicate a data-take opportunity 
        from the current phase beyond the end of that phase.  
        
        It is not appropriate to replicate a data-take opportunity past the 
        stop time of the current run (TIME2).  

        The input time interval is the time for which the 
        data-take opportunities will be created.  (TIME1, TIME2)
 
        The program will compute them starting at the begining of the 
        time interval (TIME1) and continuing for one ground track 
        repeat cycle.  (TIMEC2)

        But for the rest of the phase/time interval (TIMEC2, TIMEP), 
        these data-take opportunities will be replicated from them 
        according to the ground track repeat cycle.  Replication is 
        easier; they are copied into new records; the times and revs are 
        increased by the ground track repeat cycle time and revs.
 
        But the input time interval could start in one phase and 
        continue into another.  The program would then make the 
        data-take opportunities starting at the beginning of the input 
        time interval and continue to the end of the first phase.  
        Then the program would start at the beginning of the 
        next phase and calculate the data-take opportunities from scratch.
        
        So:  TIMEC1 and TIMEC2 bracket the first ground track 
        repeat cycle in the input time bracket.  Typically, 
        TIMEC1 will be equal to the start of the input time 
        bracket.  If the repeat cycle is short, TIMEC2 will 
        be one cycle after TIMEC1.  If the repeat cycle is longer
        than the input time period, TIMEC2 will be equal to the 
        end of the time period.  In this case, there would be 
        no replication. 
 
        TIMEP is used to help replicate the data-take opportunities
        found during the TIMEC1-TIMEC2 bracket.  The TIMEC2-TIMEP 
        bracket is the time during which they can be replicated and 
        not computed geometrically.  
        
        TIMEP1 is an indicator of whether or not there will be 
        later computing of data-take opportunities under a 
        different orbit in the next phase.  If more computing 
        is required, then TIMEP1 will be the start of the 
        next phase.  If not, TIMEP1 will have a stop indicator
        value of -1.  
==============================================================================*/
void tcycle( 
    DBPROCESS   *dbproc, 
    char        *SAT,
    double      *TIME1,            /* NOTE:  this routine is called by FORTRAN*/
    double      *TIME2,            /* and it was easier to adapt this C code  */
    double      *TIMEC1,           /* to accommodate the calling routine.     */
    double      *TIMEC2,
    double      *TIMEP,
    double      *TIMEP1,
    int         *CYDAYS,
    int         *CYREVS  )
{
double          et1, et2, tnode;
int             found ;

DB_RECORD       **phase_rec ;
llist           *phase_list = NULL ;
cursor          phase_list_ptr ;

RETCODE         return_code;

#ifdef PRINT_DIAG
printf("tcycle.c:  dbproc = %x, sat = %2.2s,\n\t\tTIME1 = %.9f, TIME2 = %.9f\n",
        dbproc, SAT, *TIME1, *TIME2 );
#endif

/* 
-- initialize the output parameters, assuming a very 
-- short (TIME1, TIME2):
*/
*TIMEC1 = *TIME1;
*TIMEC2 = *TIME2;
*TIMEP  = *TIME2;
*TIMEP1 = -1;

/*  RETRIEVE PHASE INFO FROM THE PHASE TABLE.  */
/* this routine creates the linked list; we need to delete it later.  */
return_code = phase_get_list(SAT, &phase_list ) ;
if ( return_code != PHASE_GET_LIST_OK )
{
    printf("tcycle.c:  %s\n", PHASE_ERROR_MESSAGE( return_code ) ) ;
    printf("              satellite = %s\n", SAT ) ;
    printf("              terminating this run.\n");
    return_code = banner_exit(1) ;
}

/* 
--  we want to look at the "current" phase record, the one that includes 
--  *TIME2.  From that, we get the end of phase time which might limit 
--  the replication of records.  We also get the repeat cycle info used 
--  when replicating the records.  
--
--  In addition, we want to look at the next record, if any, after it.  
--  we need the start time of the next phase from that record.  
*/

/*      
--  find the "current" phase record:  
--  retrieve loop on the phase relation:          
*/
found = 0 ;
for (   phase_rec = (DB_RECORD **) FIRST(phase_list, phase_list_ptr);
        phase_rec != NULL ;
        phase_rec = (DB_RECORD **) NEXT(phase_list, phase_list_ptr)   )
{
    /* 
    -- if this phase record is too early for us, skip 
    -- to the next one.  Check by computing the end of phase.
    */
    /* 
    -- to tell if the phase is too early, compute the end of the 
    -- phase and compare with *TIME2
    */
    if (!tc_asf2et(CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], &et1 ) )
    {
        printf("tcycle.c:  error; phase start time not legal. \n");

        printf("           sat = %s, phase_name = %c\n", 
            CAST_PHASE_SAT phase_rec[PHASE_SAT], 
            CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME] ) ;

        printf("           phase_start = %s\n",
            CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START] ) ;
        printf("              terminating this run.\n");
        return_code = banner_exit(1) ;
    }

    /* 
    -- compute the nodal period, the time delta for one revolution, 
    -- from the repeat cycle info.  
    */
    tnode = (double) CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] /
            (double) CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

    /* compute end time of the current phase.  */
    et2 = et1 + tnode * CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] ;

    /* 
    -- if the start time of the run is within this phase, stick with 
    -- this record.  
    */
    if ( *TIME1 < et2 )
    {
        found = 1 ;
        break ;
    }
}

if ( found == 0 )
{
    /* 
    -- no current phase record was found after going through the 
    -- whole relation.  
    */
    printf("tcycle.c:  no current phase record was found.\n" ) ;
    printf("              satellite = %s\n", SAT ) ;
    printf("              terminating this run.\n");
    return_code = banner_exit(1) ;
}

/* 
-- we now have the current phase record.  
*/
#ifdef PRINT_DIAG
printf("\n%s(%d):  current print rec = \n", __FILE__, __LINE__ ) ;
db_print_record(phase_rec, APS_CDEFS(PHASE) ) ;
#endif

/* a double check on this record:  */
if ( et1 > *TIME2 )
{
    /* no relevant phase found.  */
    printf("tcycle.c:  no current phase record was found.\n" ) ;
    printf("              satellite = %s\n", SAT ) ;
    printf("              terminating this run.\n");
    return_code = banner_exit(1) ;
}

/* 
-- we now have the current phase record.  assign output repeat cycle info.
*/
*CYDAYS = CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] ;
*CYREVS = CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

/* 
-- set up the cycle time bracket.  
-- TIMEC1 is to be the later of TIME1, phase start time.  
-- TIMEC2 is the end of one cycle later than TIMEC1, but within (TIME1, TIME2)
*/
*TIMEC1 = MAX( *TIME1, et1 ) ;
*TIMEC2 = MIN(  *TIMEC1 + CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS], 
                *TIME2 ) ;

/* 
-- TIMEP  is the end time of the current phase, but within (TIME1, TIME2).  
*/
*TIMEP  = MIN( et2, *TIME2) ;

/* 
-- appropriateness:  TIMEC2, the end time of a cycle in this phase, 
-- can't be beyond TIMEP, which is the end time for records in this phase.  
*/
*TIMEC2  = MIN ( *TIMEC2,  *TIMEP ) ;

/* now get then next phase record after this one:  */
phase_rec = (DB_RECORD **) NEXT(phase_list, phase_list_ptr) ;

if ( phase_rec == NULL )
{
    /* if no record, then the work is done.  */
    DEL_LIST( phase_list ) ;
    return ;
}

/* 
-- set the start time of the next 
-- phase.  
*/
if (!tc_asf2et(CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], &et1 ) )
{
    printf("tcycle.c:  error; phase start time not legal. \n");

    printf("           sat = %s, phase_name = %c\n", 
        CAST_PHASE_SAT phase_rec[PHASE_SAT], 
        CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME] ) ;

    printf("           phase_start = %s\n",
        CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START] ) ;
    printf("              terminating this run.\n");
    return_code = banner_exit(1) ;
}

*TIMEP1 = et1 ;

/* 
-- if TIMEP1 is not within the time bracket of our run, (TIME1, TIME2), 
-- then it is not needed.  
*/
if ( *TIMEP1 >= *TIME2 ) 
    *TIMEP1 = -1 ;

/* clean up.  */
DEL_LIST( phase_list ) ;

return ;

}
