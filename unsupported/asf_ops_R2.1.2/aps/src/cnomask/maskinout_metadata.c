#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       maskinout_metadata.c

Description:    holds maskinout_metadata() 

==============================================================================*/
#pragma ident   "@(#)maskinout_metadata.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/cnomask/SCCS/s.maskinout_metadata.c"

#include <string.h>
#include <libgen.h>     /* for the basename function.   */
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_maskinout_mdat.h"    /* for the maskinout_mdat relation.  */


/*==============================================================================
Function:       maskinout_metadata()

Description:    Write the db record to the maskinout_mdat relation which 
                has the run flags and values for a create_nominal_coverage 
                run.  

Parameters:     
   Input Parameters:
   Name         Type    Definition
   APS_dbproc   *DBPROCESS  pointer to a previously established 
            Sybase process.  
   NOWTIME      *CH*21  ASF time identifying the time of the run.  
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   T1       *ch*21  t1 and t2 delimit the time bracket.  The times
   T2       *ch*21  are NOT included in the time bracket.  
   efilename    *CH*100  name of the ephemeris file.   
 
   Output Parameters:

Returns:    void

Creator:    Lawrence Stevens

Creation Date:  Sun Oct  1 22:38:44 PDT 1995

Notes:      
    This routine is called from Fortran

==============================================================================*/
void maskinout_metadata(
    DBPROCESS   *APS_dbproc, 
    char        *NOWTIME, 
    char        *SAT, 
    char        *T1,
    char        *T2,
    char        *efilename )
{

char        nowtime[22];
char        sat[3];
char        t1[22];
char        t2[22];
char        *efile_basename;        /* basename of ephemeris filename    */
char        fname[100];         /* file name to go into the rec.    */

DB_RECORD   **maskinout_mdat_rec ;
int         n_maskinout_mdat;

/* for safety,  copy the Fortran strings locally and supply 
   the string termination.  */

strncpy(nowtime, NOWTIME, 21);
nowtime[21] = '\0';

strncpy(sat, SAT, 2);
sat[2] = '\0';

strncpy(t1, T1, 21);
t1[21] = '\0';

strncpy(t2, T2, 21);
t2[21] = '\0';

printf("maskinout_metadata:  APS_dbproc = %x, nowtime = %s,\n", 
    (int)APS_dbproc, nowtime );
printf("                sat = %s, \n", sat );
printf("                t1 = %s, t2 = %s\n", t1, t2);
printf("                efilename = \n>%s<\n", efilename);

efile_basename = basename(efilename);
if(strlen(efile_basename) < 3)
    strcpy(fname, efilename);
else
    strcpy(fname, efile_basename);

/* prepare a DB_RECORD to insert.  start by allocating storage:  */
maskinout_mdat_rec =  new_table_record(APS_CDEFS(MASKINOUT_MDAT) ) ;

/* now set values into the new record.  */
strcpy(CAST_MASKINOUT_MDAT_SAT maskinout_mdat_rec[MASKINOUT_MDAT_SAT], sat ) ;
strcpy(CAST_MASKINOUT_MDAT_START_TIME 
    maskinout_mdat_rec[MASKINOUT_MDAT_START_TIME], t1 ) ;
strcpy(CAST_MASKINOUT_MDAT_STOP_TIME 
    maskinout_mdat_rec[MASKINOUT_MDAT_STOP_TIME], t2 ) ;
strcpy(CAST_MASKINOUT_MDAT_GEN_TIME 
    maskinout_mdat_rec[MASKINOUT_MDAT_GEN_TIME], nowtime ) ;
strcpy(CAST_MASKINOUT_MDAT_VERSION 
    maskinout_mdat_rec[MASKINOUT_MDAT_VERSION], "1.00" ) ;
strcpy(CAST_MASKINOUT_MDAT_EPHEMERIS 
    maskinout_mdat_rec[MASKINOUT_MDAT_EPHEMERIS], fname ) ;

printf("\nmaskinout_metadata:  new record = \n" ) ;
db_print_record(maskinout_mdat_rec, APS_CDEFS(MASKINOUT_MDAT) ) ;

n_maskinout_mdat = db_insert_single_record(APS_dbproc,
    maskinout_mdat_rec, APS_TABLE(MASKINOUT_MDAT), APS_CDEFS(MASKINOUT_MDAT)) ;
if ( n_maskinout_mdat != 1 )
{
    printf("maskinout_metadata:  maskinout_mdat insert rec failed.\n");
    printf("maskinout_metadata:  new record was supposed to be: \n" ) ;
    db_print_record(maskinout_mdat_rec, APS_CDEFS(MASKINOUT_MDAT) ) ;
    printf("              terminating the run.\n");
    (void) system("banner ERROR");
    /* free the memory allocated.  */
    free_db_record( maskinout_mdat_rec ) ;
    exit (1);
}
else
    printf ("maskinout_metadata:  %d  row inserted in maskinout_mdat. \n", 
        n_maskinout_mdat) ;

/* free the memory allocated.  */
free_db_record( maskinout_mdat_rec ) ;

printf("maskinout_metadata:  returning  \n" );

return ;

}
