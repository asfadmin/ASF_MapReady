#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	tcopy.c

Description:	holds tcopy()

External Functions Defined:
tcopy()
	
Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)tcopy.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.tcopy.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

#include "timeconv.h"

#include "db_sybint.h"   	/* for APS sybase interface routines. 	*/
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_sscvrg.h"   	/* for sscvrg table                  	*/
 

/*==============================================================================
Function:       tcopy()

Description:    COPIES SPECIFIC SITE COVERAGE DATA FROM ONE CYCLE TO THE END OF 
				THE CURRENT PHASE, THUS TAKING ADVANTAGE OF THE PERIODICITY OF 
				THE ORBIT.

Parameters:     

   Input Parameters:
   Name         Type    Definition
   APS_dbproc	*DBPROCESS pointer to sybase info structure.  
   IBATCH	*INT*4	BATCH FLAG =0: INTERACTIVE; =1: BATCH
   DARID	*INT*4	DAR ID.
   SITENAME	*CHAR*32	NAME OF SITE
   SAT		*CHAR*2	SATELLITE NAME
   SENSOR	*CHAR*3	SENSOR
   ASCDSC	*CHAR*1	ASCDSC
   TIMEC1	*REAL*8	FIRST TIME IN THE EXISTING SSCV TO COPY
   TIMEC2	*REAL*8	LAST TIME IN THE EXISTING SSCV TO COPY
   TIMEP	*REAL*8	LAST TIME IN THE CURRENT PHASE TO RECEIVE THE COPY
   CYDAYS	*INT*4	NUMBER OF DAYS IN 1 CYCLE
   CYREVS	*INT*4	NUMBER OF REVS IN 1 CYCLE

   Output Parameters:
   Name         Type    Definition
   NRECS	*INTEGER	NUMBER OF RECORDS APPENDED.  
   ISTAT	*INTEGER	STATUS FLAG: =0: O.K.  =1: ERROR.

Returns:        void

Creator:        Lawrence Stevens

Creation Date:  Wed Oct  4 08:41:30 PDT 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
void tcopy (
	DBPROCESS   *APS_dbproc,
	int			*IBATCH,
	int			*DARID,
	char		*SITENAME,
	char		*SAT,
	char		*SENSOR,
	char		*ASCDSC,
	double		*TIMEC1,
	double		*TIMEC2,
	double		*TIMEP,
	int			*CYDAYS,
	int			*CYREVS,
	int			*NRECS,
	int			*ISTAT	)
{

int		stat;
char	sitename_buf[65];

/*	sscvrg records	*/
DB_RECORD       **sscvrg_rec ;
llist           *sscvrg_list = NULL ;
cursor          sscvrg_list_ptr ;
double			stopet ;			/* used to help define a loop bracket.  */
int				jrec = 0 ;
int				n_cycles = 0 ;

/*	sscvrg records to copy	*/
DB_RECORD       **copy_sscvrg_rec ;
llist           *copy_sscvrg_list = NULL ;

RETCODE		return_code;

int 		print_flag = 0 ;

if(print_flag == 1)
{
	printf("tcopy.c:  APS_dbproc = %x, ibatch = %d, darid = %d\n", 
	APS_dbproc, *IBATCH, *DARID);
	printf("     sitename = %.32s, sat = %.2s, sensor = %.3s, ascdsc = %.1s\n",
	SITENAME, SAT, SENSOR, ASCDSC );
	printf("     timec1 = %.9f, timec2 = %.9f, \n          timep = %.9f\n", 
	*TIMEC1, *TIMEC2, *TIMEP );
	printf("     cydays = %d, cyrevs = %d\n", *CYDAYS, *CYREVS ); 
	printf("tcopy.c:  NULL = %x\n", NULL);
}

*NRECS = 0;
*ISTAT = 0;

/* 
-- loop through records within the time bracket timec1, timec2 and 
-- replicate them into the time bracket timec2, timep by adding the 
-- repeat cycle days to the times and by adding the repeat cycle revs 
-- to the rev field.  							
*/

/* 
-- prepare a retrieve loop on the sscvrg relation based 
-- partly on sitename, which could have a quote in it.  
-- this is trouble because quotes are used as delimeters in 
-- isql for character strings.  the remedy is to double each 
-- single quote, so that "Mark's favorite site 'goofy' "
-- becomes:  "Mark''s favorite site ''goofy'' ".   
*/
/* allow for a quote ' in the sitename:        */
stat = quote_doubler(SITENAME, sitename_buf, sizeof(sitename_buf));
if(stat == -1)
{
	printf("tcopy.c:  could not malloc.  out of memory.\n");
	banner_exit(1);
}
if(stat == -2)
{
	printf("tcopy.c:  sitename_buf too small.\n");
	printf("                  error in code.\n");
	banner_exit(2);
}
if(stat < 0)
{
	printf("tcopy.c:  error in code.\n");
	banner_exit(3);
}

sprintf(where_clause, "where %s = %d and \
	%s = '%s'   and \
	%s = '%.2s' and \
	%s = '%.3s' and \
	%s = '%.1s' and \
	%s >= %.10f  and \
	%s <= %.10f ",
	APS_COL(SSCVRG, SSCVRG_DARID), *DARID, 
	APS_COL(SSCVRG, SSCVRG_SITENAME), sitename_buf, 
	APS_COL(SSCVRG, SSCVRG_SAT), SAT, 
	APS_COL(SSCVRG, SSCVRG_SENSOR), SENSOR, 
	APS_COL(SSCVRG, SSCVRG_ASCDSC), ASCDSC, 
	APS_COL(SSCVRG, SSCVRG_STRTET), *TIMEC1, 
	APS_COL(SSCVRG, SSCVRG_STRTET), *TIMEC2 ) ;

if(print_flag == 1)
	printf("tcopy:  where_clause = \n%s\n", where_clause);

sprintf(orderby_cols, "%s", APS_COL(SSCVRG, SSCVRG_STRTET) ) ;

if(print_flag == 1)
	printf("tcopy:  orderby_cols = \n%s\n", orderby_cols) ;

sscvrg_list = db_get_records(APS_dbproc, APS_TABLE(SSCVRG),
	where_clause, orderby_cols, APS_CDEFS(SSCVRG), ALL_COLS) ;
if (sscvrg_list == NULL)
{
	printf("tcopy:  %s(%d):  DB query failed on %s relation.\n", 
		__FILE__, __LINE__, APS_TABLE(SSCVRG) ) ;
	printf("tcopy:  where_clause = \n%s\n", where_clause) ;
	printf("tcopy:  orderby_cols = \n%s\n", orderby_cols) ;
	printf("Terminating run.  \n") ;
	banner_exit(1);
}

if ( NUMELTS(sscvrg_list) <= 0 )
{
	/* clean up; no work to do here.  */
	DEL_ALL( sscvrg_list ) ;
	return ;
}

/* 
-- there is some work to do.  
-- for each record in the list, copy it into the next repeat cycle.  
-- since each orbit repeats, if there is a sscvrg record for an 
-- orbit, there would be another valid one exactly one repeat cycle 
-- for it.  
-- however do not copy any records into a time that is outside of the 
-- time bracket we are working in.  do not copy any records with 
-- an end time after *TIMEP, an input parameter.  
*/
/* create the list for the records to copy.  */
copy_sscvrg_list = create_dyn_llist() ;
jrec = 0 ;
for ( sscvrg_rec = (DB_RECORD **) FIRST(sscvrg_list, sscvrg_list_ptr) ;
	  sscvrg_rec != NULL ;
	  sscvrg_rec = (DB_RECORD **) NEXT(sscvrg_list, sscvrg_list_ptr) )
{
	if(print_flag == 1)
	{
		printf("tcopy:  processing retrieved sscvrg coverage record %d\n", 
			++jrec);
		db_print_record( sscvrg_rec, APS_CDEFS(SSCVRG) ) ;
	}

	/* 
	-- now set up loop to copy this record as many 
	-- times as indicated by the input parameters	
	*/

	/* 
	-- in the next loop, we take each retrieved sscvrg rec 
	-- and add one cycle of days/revs to each time/rev field.
	-- then insert the result into the sscvrg db table.  we 
	-- then add one more cycle to this result and then insert
	-- that, too, into the db table.  we keep doing this as 
	-- long as the time bracket of the inserted record is 
	-- inside the destination time bracket, i.e., BEFORE 
	-- the input paramete value *TIMEP.		
	*/

	stopet = CAST_SSCVRG_STOPET sscvrg_rec[SSCVRG_STOPET] ;

#ifdef PRINT_DIAG
	printf("%s(%d):  stopet at start = %.10f\n", __FILE__, __LINE__, stopet ) ;
	printf("         *CYDAYS at start = %d\n", *CYDAYS ) ;
	printf("         *TIMEP at start = %.10f\n", *TIMEP ) ;
#endif

	n_cycles = 0 ;
	while (  (stopet += *CYDAYS )  <  *TIMEP )
	{
		n_cycles ++ ;

		#ifdef PRINT_DIAG
		printf("%s(%d):  stopet at LOOP = %.10f\n", 
			__FILE__, __LINE__, stopet ) ;
		#endif

		/* 
		-- create a new dbrecord which will be a copy of the 
		-- one we are working with, except that we will change 
		-- the time and rev values to be one or more repeat cycles 
		-- forward in time.  
		*/
		copy_sscvrg_rec = new_table_record(APS_CDEFS(SSCVRG)) ;
		return_code = db_copy_record ( APS_CDEFS(SSCVRG), 
			copy_sscvrg_rec, sscvrg_rec ) ;

		/* 
		-- in this loop, we add n_cycles number of cycles to the 
		-- the data fields then APPEND the record to 
		-- the copy list.  time fields are incremented
		-- by *CYDAYS, the repeat cycle in days.  
		-- the rev field is incremented by *CYREVS
		-- the other fields are not adjusted; they 
		-- are what repeats during the repeat cycle, 
		-- or are constant. 		
		*/

		/* 
		-- we already added the repeat cycle to stopet for the 
		-- insertion of the sscvrg record.  this happened in the 
		-- for loop statement.  
		*/
		CAST_SSCVRG_STOPET copy_sscvrg_rec[SSCVRG_STOPET] = stopet ;

		/* 
		-- now complete incrementing the time and rev 
		-- parameters by the appropriate repeat cycle 
		-- and then insert a new record.  
		*/
		CAST_SSCVRG_STRTET copy_sscvrg_rec[SSCVRG_STRTET] =
			CAST_SSCVRG_STRTET copy_sscvrg_rec[SSCVRG_STRTET] + 
			n_cycles * (*CYDAYS) ;

		if(!tc_et2asf( CAST_SSCVRG_STRTET copy_sscvrg_rec[SSCVRG_STRTET], 
						CAST_SSCVRG_STRTTIME copy_sscvrg_rec[SSCVRG_STRTTIME] ))
		{
			printf("%s(%d):  bad computed strtet:  %20.10f\n",
				__FILE__, __LINE__, 
				CAST_SSCVRG_STRTET copy_sscvrg_rec[SSCVRG_STRTET] ) ;
			*ISTAT = 1 ;
			/* clean up.  */
			DEL_ALL( sscvrg_list ) ;
			DEL_ALL( copy_sscvrg_list ) ;
			return ;
		}
		if(!tc_et2asf( CAST_SSCVRG_STOPET copy_sscvrg_rec[SSCVRG_STOPET], 
						CAST_SSCVRG_STOPTIME copy_sscvrg_rec[SSCVRG_STOPTIME] ))
		{
			printf("%s(%d):  bad computed stopet:  %20.10f\n",
				__FILE__, __LINE__, 
				CAST_SSCVRG_STOPET copy_sscvrg_rec[SSCVRG_STOPET] ) ;
			*ISTAT = 1 ;
			/* clean up.  */
			DEL_ALL( sscvrg_list ) ;
			DEL_ALL( copy_sscvrg_list ) ;
			return ;
		}

		CAST_SSCVRG_REV copy_sscvrg_rec[SSCVRG_REV] =
			CAST_SSCVRG_REV copy_sscvrg_rec[SSCVRG_REV] + n_cycles * (*CYREVS) ;

		/*
		-- the start and stop ET and ASF times have been incremented 
		-- by the repeat cycle in days; the rev number has been 
		-- incremented by the repeat cycle in revs.  
		-- 
		-- now append the copy_sscvrg record to the 
		-- copy list.    
		*/

		#ifdef PRINT_DIAG
		printf("%s(%d)  appending record:  \n", __FILE__, __LINE__ ) ;
		db_print_record(copy_sscvrg_rec, APS_CDEFS(SSCVRG) ) ;
		#endif

		printf(
		"%s(%d):  appending sscvrg rec to list\n\t%s %s %ld  %s   %s  %c\n",
			__FILE__, __LINE__,
			CAST_SSCVRG_SAT copy_sscvrg_rec[SSCVRG_SAT],
			CAST_SSCVRG_SENSOR copy_sscvrg_rec[SSCVRG_SENSOR],
			CAST_SSCVRG_REV copy_sscvrg_rec[SSCVRG_REV],
			CAST_SSCVRG_STRTTIME copy_sscvrg_rec[SSCVRG_STRTTIME],
			CAST_SSCVRG_STOPTIME copy_sscvrg_rec[SSCVRG_STOPTIME], 
			CAST_SSCVRG_ASCDSC copy_sscvrg_rec[SSCVRG_ASCDSC] ) ;

		APPEND( copy_sscvrg_list, copy_sscvrg_rec, 
			free_db_record,       copy_sscvrg_rec) ;

	} /* end of  while (  (stopet = stopet + *CYDAYS)  <  *TIMEP )    */

}  /* end of:  for ( sscvrg_rec = .... */

/* 
-- we have processed each relevant sscvrg rec already in the database.  
-- we have computed copies of each, if possible, and placed them into 
-- the copy_sscvrg_list which can now be inserted into the database.  
*/

if( NUMELTS( copy_sscvrg_list ) > 0 )
{
	printf("%s(%d):  inserting %d sscvrg recs from list to db.\n", 
		__FILE__, __LINE__, NUMELTS(copy_sscvrg_list) ) ;
	*NRECS = db_insert_records(APS_dbproc,
		copy_sscvrg_list, APS_TABLE(SSCVRG), APS_CDEFS(SSCVRG)) ;
}

if( *NRECS != NUMELTS( copy_sscvrg_list ) )
{
	printf("%s(%d):  not all records in the list were inserted.\n", 
		__FILE__, __LINE__ ) ;
	printf("recs in list:  %d; recs inserted:  %d\n", 
		NUMELTS( copy_sscvrg_list ), *NRECS ) ;
	*ISTAT = 1 ;
}

/* clean up.  */
DEL_ALL( sscvrg_list ) ;
DEL_ALL( copy_sscvrg_list ) ;

/* normal end	*/
return ;

}
