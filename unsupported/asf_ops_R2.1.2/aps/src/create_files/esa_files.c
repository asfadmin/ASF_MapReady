#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*============================================================================
Filename:
 
Description:
 
External Functions Defined:
   
File Scope Functions:
   
External Variables Defined:
   
File Scope Variables:
 
Notes:
 
==============================================================================
*/
#pragma ident   "@(#)esa_files.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.esa_files.c"

#include <stdio.h>

#include "dapps_list.h"
#include "nmalloc.h" 

#include "db_sybint.h" 
#include "db_sybextern.h"
#include "aps_extern.h"

#include "apsfiledef.h"
#include "aps_log_msg.h"	/* for APS logging						*/
#include "file_utilities.h" /* for APS logging, also				*/

#include "aps_db_table.h"
#include "db_dtk.h"
#include "db_dar.h"
#include "db_seg.h"
#include "db_schunavail.h"
#include "db_rgs_down_times.h"

#ifdef FORGET_THE_CONFLICTS /* MPS has never generated any conflicts rows */
#include "db_asf_perf.h"
#endif

#ifdef FORGET_THE_CONFLICTS /* MPS has never generated any conflicts rows */
#include "db_conflicts.h"
#endif

#include "timeconv.h"

#include "mps.h"

typedef
	struct _UNAVAILABLE_RECORD
	{
		char facid[3] ;
		char prareid[12] ;
		char syssubstat[7] ;
		char utype ;
		char ureason ;
		char non_esa ;
		char reserved[3] ;
		char x_day_t1[25] ;
		char x_day_t2[25] ;
		char remarks[61] ;

		/* 
		-- the following fields are not part of the
		-- record but are used to sort the times
		*/
		char asf_strttime[22] ;
		char asf_stoptime[22] ;
	} UNAVAILABLE_RECORD ;


static char blank[] = " " ;

char 		display_string[1024] ;

extern mpsintrp_ (
		float *p1lat, float *p1lon, 
		float *p2lat, float *p2lon,
		int *n, float *plats, float *plons) ;

extern mps_cgen_ (
		float *lat, float *lon, float *rkm, 
		int *npts, float *lats, float *lons) ;	



typedef
	struct _DATATAKE_RECORD
	{	
		char sat[3] ;
		char sensor[4] ;
		int rev ;
		int dtkid ;
		int e1 ;
		double jstart ;
		double jstop ;
	} DATATAKE_RECORD ;


static int time_compare(UNAVAILABLE_RECORD *event1, UNAVAILABLE_RECORD *event2)
{
	return(strcmp(event1->asf_strttime, event2->asf_strttime)) ;
}
 


/**********************************************************************
*
*  Name : DO_EAVA
*
*  Purpose: creates and opens the file, formats the data, and writes
*           it to the file.
*
*  Input:
*	name		type	descrip
*	scht2		c*	pointer to temporary table
*	sfile		c*	pointer to filename to be created
*
*  Local variables:
*       errno		i	error number
*	rcount		i	record count
*	stat		i	return status
*	flag		i	flag to indicate error within loop
*	begint		c15	begin time
*	endt		c15	end time
* >>>  FIELDS IN THE UNAVAILABILITY REPORT.  ADD 1 CHAR FOR A NULL AT THE END
*	facid		c3	facility id
*	prareid		c12	PRARE Station identifier
*	syssubstat	c7	System/Subsystem Status
*	utype		c2	unavailability type
*	ureason		c2	unavailability reason
*	non_esa		c2	Non-ESA Station priority
*	reserved	c3	Reserved
*	x_day_t1	c25	Unavailability Start date/time.
*				dd-MMM-yyyy hh:mm:ss.sss
*				01-JAN-1990 23:59:59.999
*	x_day_t2	c25	Unavailability End date/time.
*	remarks		c61	remarks
*
*	fp		FILE*	file pointer
*
**********************************************************************/
int do_eava(char *filename, char *start, char *stop)
{
	int rcount, stat, flag, icount ;
	int year, decade, day, hour, min, sec, msec ;
    int mon, mday ;
 
	/*
	--  UNAVAILABILITY REPORT FIELDS: 
	--  Add 1 char for a null at the end 
	*/

	char x_day_t1[25] ;
	char x_day_t2[25] ;
	char current_time[] = "yyyy:ddd:hh:mm:ss.ccc" ;

	llist *default_values ;
	llist *down_times ; 
	llist *datatakes ; 
	llist *combined_datatakes ; 
	ord_llist *esa_unavailable_times ; 

	cursor ptr ;	
	cursor dtk_ptr ;
	cursor cdtk_ptr ;
	cursor conflict_ptr ;

	char strttime[22] ;
	char stoptime[22] ;

	UNAVAILABLE_RECORD *esa_unavailable_time_rec  ;
	UNAVAILABLE_RECORD *prev_esa_unavailable_time_rec  ;

	UNAVAILABLE_RECORD *unavailable_time  ;

	DATATAKE_RECORD *combined_dtk_rec ;
	DB_RECORD **default_rec ;
	DB_RECORD **dtk_rec ;
	DB_RECORD **conflict_rec ;
	DB_RECORD **asf_dn_times_rec ;

	FILE *fp ;


/* START CODE */

	/*
	--  get the asf_dn_times
	-- the embedded QUEL here is from the function 
    -- stat = ava_asf_dn(scht2,start,stop,reccnt);
	*/

#ifdef EMBEDDED_QUEL
/* get the default values for the record from the unavailability rel. */

## retrieve (
##	facid = a.#facid ,
##	prareid = a.#prareid ,
##	syssubstat = a.#syssubstat ,
##	non_esa = a.#non_esa ,
##	reserved = a.#reserved )
## {
## }
## inquire_equel(rcount=rowcount,errno=errorno)
   if (errno != 0) {
##   prompt ("Error retrieving default values for record. Press <RETURN>",ans)
     return 1;                           
   }
   
## APPEND TO scht2 (
##	#facid = facid , 
##	#prareid = prareid ,
##	#syssubstat = syssubstat ,
##	#non_esa = non_esa ,
##	#reserved = reserved ,
##	#remarks  = asf_dn_times.#remarks ,
##	#utype    = asf_dn_times.#utype ,
##	#ureason  = asf_dn_times.#ureason ,
##	#strttime = asf_dn_times.#strttime , 
##	#stoptime = asf_dn_times.#stoptime   )
## where    asf_dn_times.#strttime < tstop
## 	and asf_dn_times.#stoptime > tstart
#endif

	default_values = db_get_records(APS_dbproc, APS_TABLE(SCHUNAVAIL), 
		NULL, NULL, APS_CDEFS(SCHUNAVAIL), ALL_COLS) ;


	if (!default_values || NUMELTS(default_values) != 1)
	{
		sprintf (file_util_msg, 
		"Default Database Relation: schunavail contains %d records\n",
			NUMELTS(default_values)) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Should contain only one set of default values\n",
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_DB_ERROR) ;
	}

	default_rec = FIRST(default_values, ptr) ;

	sprintf(where_clause, 
		"where %s < '%s' and %s > '%s'",
		APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STRTTIME), stop,
		APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STOPTIME), start) ;

	down_times = db_get_records(APS_dbproc, APS_TABLE(RGS_DOWN_TIMES), 
		where_clause, NULL, APS_CDEFS(RGS_DOWN_TIMES), ALL_COLS) ;

	asf_dn_times_rec = FIRST(down_times, ptr) ;

	esa_unavailable_times = (ord_llist *) create_dyn_ord_llist(time_compare) ;

	while (asf_dn_times_rec)
	{ 
		unavailable_time = (UNAVAILABLE_RECORD *) 
			NEW(sizeof(UNAVAILABLE_RECORD)) ;

		strcpy(unavailable_time->facid, (char *) 
			default_rec[SCHUNAVAIL_FACID]) ;

		strcpy(unavailable_time->prareid, (char *) 
			default_rec[SCHUNAVAIL_PRAREID]) ;

		strcpy(unavailable_time->syssubstat, (char *) 
			default_rec[SCHUNAVAIL_SYSSUBSTAT]) ;

		unavailable_time->non_esa = CAST_SCHUNAVAIL_NON_ESA 
			default_rec[SCHUNAVAIL_NON_ESA] ;

		strcpy(unavailable_time->reserved, (char *)
			default_rec[SCHUNAVAIL_RESERVED]) ;

		unavailable_time->utype = CAST_RGS_DOWN_TIMES_UTYPE 
			asf_dn_times_rec[RGS_DOWN_TIMES_UTYPE] ;

		unavailable_time->ureason = CAST_RGS_DOWN_TIMES_UREASON 
			asf_dn_times_rec[RGS_DOWN_TIMES_UREASON] ;

		strcpy(unavailable_time->remarks, 
			(char *) asf_dn_times_rec[RGS_DOWN_TIMES_REMARKS]) ;

		strcpy(unavailable_time->asf_strttime, 
			(char *) asf_dn_times_rec[RGS_DOWN_TIMES_STRTTIME]) ;

		strcpy(unavailable_time->asf_stoptime, 
			(char *) asf_dn_times_rec[RGS_DOWN_TIMES_STOPTIME]) ;


		/* 
		-- fill these in later when the file is written 
			char x_day_t1[25] ;
			char x_day_t2[25] ;
		*/

		INS_BY_KEY(esa_unavailable_times, unavailable_time, NULL, NULL) ;

		asf_dn_times_rec = NEXT(down_times, ptr) ;
	} /* while asf_dn_times_rec */

#ifdef FORGET_THE_CONFLICTS /* MPS has never generated any conflicts rows */

	/* now add the unavailable times due to datatakes */

	/* convert the start/stop times to Julian */
	if (!(tc_asf2et(start, &jstart))
	{
		sprintf (file_util_msg, 
			"Error converting start time '%s' to Julian days", start) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_ERROR) ;
	} 

	if (!tc_asf2et(stop, &jstop))
	{
		sprintf (file_util_msg, 
			"Error converting stop time '%s' to Julian days", stop) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_ERROR) ;
	} 
                
	/*
	--
	-- Process the dtk.e1 = 0/1/3 data-takes

	-- dtk.e1 = 0 indicates that the datatake has not yet been reported
	-- to the FA that the time is unavailable.  It also indicates 
	-- data-takes that have been increased.

	-- dtk.e1 = 1 indicates that the datatake has not changed.  

	-- dtk.e1 = 2 indicates that the datatake has been deleted and 
	--            no unavailability will result.  

	-- dtk.e1 = 3 indicates that the datatake has been reduced.  

	-- ALL OF THE UNAVAILABILITY TIMES WILL BE REPORTED.  THIS MEANS 
	-- THAT ALL FUTURE DATA-TAKES FOR J1 AND RS MUST BE REPORTED AS
	-- UNAVAILABLE TIMES.  
	*/

	/*
	-- Get the amount of time it takes to move the asf dish to a data-take
	-- unit is in real minutes  
	*/

#ifdef EMBEDDED_QUEL
## retrieve ( j1 = asf_perf.#t_aim)
## inquire_equel(errno=errorno,rcount=rowcount)
   if (errno != 0) {
##   prompt ("Error retrieving ASF dish aim time from db.  Press <RETURN> ",ans)
     return 1;
   }
   if (rcount != 1) {
##   prompt ("Error.  #recs in asf_perf db relation != 1.  Press <RETURN> ",ans)
     return 1;
   }
#endif

	default_values = db_get_records(APS_dbproc, APS_TABLE(ASF_PERF),
		NULL, NULL, APS_CDEFS(ASF_PERF), ALL_COLS) ;

	if (NUMELTS(default_values) != 1)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Incorrect number of records in relation 'asf_perf'\n",
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_DB_ERROR) ;
	}

	default_rec = FIRST(default_values, ptr) ;

	j1 = CAST_ASF_PERF_T_AIM default_rec[ASF_PERF_T_AIM] ;
	
	j1 = j1 / 24.0 / 60.0;	/* change to real days.  */

	/*
	--  the conflicts relation, gives times when more than 
	--  one satellite is in the asf mask
	--
	-- retrieve the conflicts that:
	-- fall within the specified start and stop times
	-- have E1 contained in config field
	*/

#ifdef EMBEDDED_QUEL
## retrieve (acon[i].config=l.#config,
##   acon[i].start=l.#strttime,acon[i].stop=l.#stoptime,
##   dumf=l.#strttime)
##   where l.#config="*E1*" and
##   l.#strttime <= jstop and l.#stoptime >= jstart
#endif

	sprintf(where_clause, 
		"where %s like '%%E1%%'\n\
		and %s <= %f and %s >= %f",
			APS_COL(CONFLICTS, CONFLICTS_CONFIG),
			APS_COL(CONFLICTS, CONFLICTS_STRTET), jstop,
			APS_COL(CONFLICTS, CONFLICTS_STOPET), jstart) ;

	sprintf(orderby_cols, "%s", 
		APS_COL(CONFLICTS, CONFLICTS_STRTET)) ;

	conflict_list = db_get_records(APS_dbproc, APS_TABLE(CONFLICTS), 
		where_clause, orderby_cols, APS_CDEFS(CONFLICTS), ALL_COLS) ;

	/*
	-- if conflicts exist...
	-- retrieve data-takes that are for satellites other than E1,
	-- have E1 = 0/1/3, are within the times specified on the form,
	-- and are planned or scheduled.  They must invlove the ASF dish.
    */

	if (NUMELTS(conflict_list) > 0)
	{
		sprintf(where_clause, 
			"where dtk.sat != 'E1'\n\
			and dtk.strttime <= '%s' and dtk.stoptime >= '%s'\n\
			and (dtk.dtkstat = 'PLN' or dtk.dtksat = 'SCH')\n\
			and (actid like 'RTS%%' or actid like 'RTO%%' \n\
			  or actid like 'RTV%%' or actid like 'DMP%%')",

			stop, start) ;

		sprintf(orderby_cols, "sat, strttime") ;	

		datatakes = db_get_records(APS_dbproc, APS_TABLE(DTK), 
			where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;


		/* TO DO add comments */

		if (NUMELTS(datatakes) > 0)
		{
			combined_datatakes = create_dyn_llist() ;

			dtk_rec = FIRST(datatakes, ptr) ;
			while (dtk_rec)
			{
				/* convert the start/stop time to Julian time */
				if (!tc_asf2et(dtk_rec[DTK_STRTTIME], &jstart))
				{
					sprintf (file_util_msg, 
						"Error converting DTK start time '%s'", 
						dtk_rec[DTK_STRTTIME]) ;
					aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
						DO_SYSLOG, DO_PRINT);
					dtk_rec = NEXT(datatakes, ptr) ;
					continue ;
				}

				if (!(tc_asf2et(dtk_rec[DTK_STOPTIME], &jstop))
				{
					sprintf (file_util_msg, 
						"Error converting DTK stop time '%s'", 
						dtk_rec[DTK_STOPTIME]) ;
					aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
						DO_SYSLOG, DO_PRINT);
					dtk_rec = NEXT(datatakes, ptr) ;
					continue ;
				}
				
				/* pad the data-take by the time it takes to move the dish */
				jstart = jstart - j1 ;
				jstop  = jstop  + j1 ;

				if (combined_dtk_rec = LAST(combined_datatakes, cdtk_ptr))
				{
					if (strcmp(combined_dtk_rec->sat, (char *) dtk_rec[DTK_SAT]) == 0
					&& combined_dtk_rec->rev == CAST_DTK_REV dtk_rec[DTK_REV])
					{
						/*
						-- this data-take has the same sat and rev as the 
						-- previous one.  we should combine this one and the 
						-- last one into the same entry in the array by 
						-- updating the stop time of the previous entry 
						-- instead of adding a new entry.
						--
						-- the point being that the ASF dish will not take data 
						-- from one satellite, then switch to a second 
						-- satellite and then return to the first satellite in 
						-- the same rev.  rather, the ASF dish will stay 
						-- with the first satellite through the 2 data-takes 
						-- and not do the middle one.
						-- rendering the 2 data-takes on the same rev and
						-- the time between them unavailable to ESA. 
						*/
	
        				if (combined_dtk_rec->jstop < jstop) 
							combined_dtk_rec->jstop  = jstop ;
					}
					else
					{
        				/* add this dtk to the combined dtks list */
						combined_dtk_rec = (DATATAKE_RECORD *) 
							NEW(sizeof(DATATAKE_RECORD)) ;

						strcpy(combined_dtk_rec->sat, (char *) dtk_rec[DTK_SAT]) ;
						combined_dtk_rec->rev = CAST_DTK_REV dtk_rec[DTK_REV] ;
						combined_dtk_rec->jstart = jstart ;
						combined_dtk_rec->jstop  = jstop ;

						/* TO DO add DELETE function */
						APPEND(combined_datatakes, combined_dtk_rec, NULL, NULL) ;
					}
				} /* if combined_record exists */
				dtk_rec = NEXT(datatakes, ptr) ;
			} /* while datatakes */
		} /* if datatakes exist */
	} /* if conflicts exist */

	conflict_rec = FIRST(conflict_list, conflict_ptr) ;
	while (conflict_rec)
	{
		dtk_rec = FIRST(datatakes, dtk_ptr) ;

		while (dtk_rec)
		{
			/* if the datatake and conflict records overlap... */
			if (combined_dtk_rec->jstart < 
				CAST_CONFLICTS_STOPET conflict_rec[CONFLICTS_STOPET]
			&& combined_dtk_rec->jstop > 
				CAST_CONFLICTS_STRTET conflict_rec[CONFLICTS_STRTET])
			{
				/* 
				-- 	note that each data-take in the array has already been 
				-- combined with other data-takes with the same rev and sat.  
				-- in addition, each data-take in the array has already been 
				-- padded with the time it takes to move the ASF dish.
				--
				-- so that each data-take in the array is an unavailability 
				-- and can be appended to the temp relation. 
				*/
				unstrtt = combined_dtk_rec->jstart ;
				unstopt = combined_dtk_rec->jstop ;
               
				/* add to the list of unavailable times */

				if (unstrtt == 0.0
				||  unstopt == 0.0) 
				{
					aps_log_msg(file_util_progname, APS_ERROR, 
						"Start/Stop time cannot be 0.0 Julian date\n",
						DO_SYSLOG, DO_PRINT);
     				return(APS_REPORT_ERROR) ;
				}
	
				/* convert the start and stop times to ASF format */

				if (!(tc_et2asf(unstrtt, &strttime)) 
				{
					aps_log_msg(file_util_progname, APS_ERROR, 
						"Error converting Julian to ASF date\n",
						DO_SYSLOG, DO_PRINT);
					return(APS_REPORT_ERROR)  ;
				}

				if (!tc_et2asf(unstopt, &stoptime)) != 0) 
				{
					aps_log_msg(file_util_progname, APS_ERROR, 
						"Error converting Julian to ASF date\n",
						DO_SYSLOG, DO_PRINT);
					return(APS_REPORT_ERROR) ;
				}

#ifdef EMBEDDED_QUEL
## REPEAT APPEND TO scht2 (
##	#facid = facid, 
##	#prareid = prareid ,
##	#syssubstat = syssubstat ,
##	#non_esa = non_esa ,
##	#reserved = reserved ,
##	#remarks = remarks ,
##	#utype = @utype, 
##	#ureason = @ureason,
##	#strttime = @strttime, 
##	#stoptime = @stoptime   )
#endif

				unavailable_time = (UNAVAILABLE_RECORD *)
					NEW(sizeof(UNAVAILABLE_RECORD)) ;

				strcpy(unavailable_time->facid, (char *) 
					default_rec[SCHUNAVAIL_FACID]) ;

				strcpy(unavailable_time->prareid, (char *) 
					default_rec[SCHUNAVAIL_PRAREID]) ;

				strcpy(unavailable_time->syssubstat, (char *) 
					default_rec[SCHUNAVAIL_SYSSUBSTAT]) ;

				unavailable_time->non_esa = 
					CAST_SCHUNAVAIL_NON_ESA 
					default_rec[SCHUNAVAIL_NON_ESA] ;

				strcpy(unavailable_time->reserved, (char *)
					default_rec[SCHUNAVAIL_RESERVED]) ;

				strcpy(unavailable_time->remarks, 
					(char *) asf_dn_times_rec[RGS_DOWN_TIMES_REMARKS]) ;

				unavailable_time->utype = 
					CAST_SCHUNAVAIL_UTYPE 
					default_rec[SCHUNAVAIL_UTYPE] ;

				unavailable_time->ureason = 
					CAST_SCHUNAVAIL_UREASON 
					default_rec[SCHUNAVAIL_UREASON] ;

				strcpy(unavailable_time->asf_strttime, strttime) ;
				strcpy(unavailable_time->asf_stoptime, stoptime) ;
		
				INS_BY_KEY(esa_unavailable_times, unavailable_time,
					NULL, NULL) ;
			} /* end if conflict & data-take overlap */
			dtk_rec = NEXT(datatakes, dtk_ptr) ;

		} /* while dtk_rec */

		conflict_rec = NEXT(conflict_list, conflict_ptr) ;

	} /* while conflict_rec */


	/* COPY TO HERE */

#endif /* THIS IS THE ENDIF FOR FORGET_THE_CONFLICTS */

                
	if ((fp = fopen(filename, "w")) == NULL) 
	{
		sprintf (file_util_msg, 
			"Error opening Unavailability Report file: %s\n", filename) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
    	return(APS_REPORT_FILE_ERROR) ;
	}

	aps_log_msg(file_util_progname, APS_INFO, 
		"Formatting data...\n\n",
		DO_SYSLOG, DO_PRINT);
		
	/*
	-- Get current system date and time, then:
	-- Write the header for this file
	*/
	tc_systime2asf(current_time) ;
	if (!tc_parse_asftime(current_time, &year, &decade, &day, &hour, &min, &sec, &msec))
		return (APS_REPORT_ERROR) ;
    if (!tc_doy2cal(year, day, &mon, &mday))
        return (APS_REPORT_ERROR) ;

	/*
	-- NOTE: we hardcode REUG and E1 and EC values in the header, because
	-- E1 is used in the fetching of db records, and REUG is the only report
	-- that uses this routine, and the European Space Agency is the only agency
	-- that receives the REUG.
	*/
	fprintf(fp,
		"REUG_%02d%02d%02dAFEC0000.E1%02d:%02d:%02d",
        year, mon, mday, hour, min, sec);

	flag = 0 ;
	icount = 0 ;

#ifdef EMBEDDED_QUEL
## retrieve (
##	p_facid 	= s2.#facid,
##	p_prareid 	= s2.#prareid ,
##	p_syssubstat= s2.#syssubstat ,
##	p_utype 	= s2.#utype,
##	p_ureason 	= s2.#ureason,
##	p_non_esa 	= s2.#non_esa ,
##	p_reserved 	= s2.#reserved ,
##	p_strttime 	= s2.#strttime,
##	p_stoptime 	= s2.#stoptime,
##	p_remarks 	= s2.#remarks	)
##   sort by #p_strttime
## {
#endif

	prev_esa_unavailable_time_rec = FIRST(esa_unavailable_times, ptr) ;
	esa_unavailable_time_rec = NEXT(esa_unavailable_times, ptr) ;

	while (prev_esa_unavailable_time_rec && esa_unavailable_time_rec)
	{
		if (strcmp(prev_esa_unavailable_time_rec->asf_strttime,
		esa_unavailable_time_rec->asf_stoptime) <= 0)
		{
			if (strcmp(prev_esa_unavailable_time_rec->asf_stoptime,
			esa_unavailable_time_rec->asf_stoptime) > 0)
			{
				/* 
				-- just update the stoptime because the new one is later. 
				-- this combines the unavailabilities and prevents us from 
				-- sending to ESA time periods which overlap.  we prevent  
				-- that because ESA said that overlapping records are ignored.
				*/
				strcpy(esa_unavailable_time_rec->asf_stoptime,
					prev_esa_unavailable_time_rec->asf_stoptime) ;


				/*
				-- if reasons are different for the unavailability
				-- at each time add it to the remarks
				*/
				if (strcmp(prev_esa_unavailable_time_rec->remarks,
				esa_unavailable_time_rec->remarks) != 0)
					strcpy(esa_unavailable_time_rec->remarks,
						"a combination of different reasons") ;
			}
		}

		prev_esa_unavailable_time_rec = esa_unavailable_time_rec ;
		esa_unavailable_time_rec = NEXT(esa_unavailable_times, ptr) ;
	}

	/* now write the records */

	rcount = 0 ;

	esa_unavailable_time_rec = FIRST(esa_unavailable_times, ptr) ;
	while (esa_unavailable_time_rec)
	{
		/* convert the start time to ESA format */
		stat = asf_esad(esa_unavailable_time_rec->asf_strttime, &x_day_t1) ;
		if (!stat) 
		{
			aps_log_msg(file_util_progname, APS_ERROR, 
				"Error formatting start time...Record skipped\n",
				DO_SYSLOG, DO_PRINT);
			flag = 1;
			esa_unavailable_time_rec = NEXT(esa_unavailable_times, ptr) ;
			continue ;
		}

		/* convert the stop time to ESA format */
		stat = asf_esad(esa_unavailable_time_rec->asf_stoptime, &x_day_t2) ;
		if (!stat) 
		{
			aps_log_msg(file_util_progname, APS_ERROR, 
				"Error formatting stop time...Record skipped\n",
				DO_SYSLOG, DO_PRINT);
			flag = 1;
			esa_unavailable_time_rec = NEXT(esa_unavailable_times, ptr) ;
			continue ;
		}

		/* write the data to the file */
		fprintf(fp,
			"%2.2s%11.11s%6.6s%c%c%c%2.2s%-24.24s%-24.24s%-60.60s",
			esa_unavailable_time_rec->facid,
			esa_unavailable_time_rec->prareid,
			esa_unavailable_time_rec->syssubstat,
			esa_unavailable_time_rec->utype,
			esa_unavailable_time_rec->ureason,
			esa_unavailable_time_rec->non_esa,
			esa_unavailable_time_rec->reserved,
			x_day_t1, x_day_t2, 
			esa_unavailable_time_rec->remarks) ;

		rcount ++ ;
		esa_unavailable_time_rec = NEXT(esa_unavailable_times, ptr) ;
	}

	fclose(fp) ;
	if (flag) 
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error formatting data\n",
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_ERROR) ;
	}
	sprintf (file_util_msg, 
		"Num of unavailable times records written: %d\n", rcount) ;
	aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
		DO_SYSLOG, DO_PRINT);

	if (!rcount)
		return(APS_REPORT_NONE) ;

	return(APS_REPORT_OK) ;
}

/**********************************************************************
*  Name : ASF_ESAD.C
*  Module Type: SUBROUTINE	Language: C
*  $Logfile:   ACS003:[BLD.MPS.SCHD.SRC]SCHAVAIL.QCV  $
*  Purpose: Convert ASF time format to ESA X_DATE_TIME format
*	ASF:	1990:001:23:59:59.999		21 characters
*	ESAD:	01-JAN-1990 23:59:59.999	24 characters
*  Functions: 
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
**********************************************************************/
int asf_esad(char *asft, char *esat)
{
#ifdef DONTUSE
	static char asf_time_separaters[] = ":." ;
	
	char asf_time_str[22] ;

	struct tm t1 ;
	int year ;
	int doy ;
	int hour ;
	int min ;
	int sec ;
	int msec ;

	strcpy (asf_time_str, asft) ;

	year = atoi(strtok(asf_time_str, asf_time_seperaters)) ;
	doy  = atoi(strtok(NULL, asf_time_seperaters)) ;
	hour = atoi(strtok(NULL, asf_time_seperaters)) ;
	min  = atoi(strtok(NULL, asf_time_seperaters)) ;
	sec  = atoi(strtok(NULL, asf_time_seperaters)) ;
	msec = atoi(strtok(NULL, asf_time_seperaters)) ;

#ifdef MIGUEL_COMMENT_OUT
	sprintf (file_util_msg, "TIME: %s\n", asft) ;
	aps_log_msg(file_util_progname, APS_DEBUG, file_util_msg, 
		DO_SYSLOG, DO_PRINT);
	sprintf (file_util_msg, 
		"%d %d %d %d %d %d\n"m year, doy, hour, min, sec, msec) ;
	aps_log_msg(file_util_progname, APS_DEBUG, file_util_msg, 
		DO_SYSLOG, DO_PRINT);
#endif
		
	/* load the tm structure */
	t1.sec  = sec ;
	t1.min  = min ;
	t1.hour = hour ;
	t1.year = atoi(strtok(asf_time_str, asf_time_seperaters)) ;
	t1.doy  = atoi(strtok(NULL, asf_time_seperaters)) ;
	msec = atoi(strtok(NULL, asf_time_seperaters)) ;
#endif
	

	int stat ;
	char xtime[15], xmonth[3];

	stat = 0 ;

	/* convert the ASF time  to ESA format */
	/*	THIS FORMAT IS yyyymmddhhmmss	*/
	if (!tc_asf2esa(asft, xtime))
		return(APS_REPORT_ERROR) ;

	/* NOW MOVE THE DAY TO THE RESULT.  */
	esat[0] = xtime[6] ;
	esat[1] = xtime[7] ;

	/* SEPARATOR */
	esat[2] = '-' ;

	/* NOW MOVE THE MONTH TO A VARIABLE.  */
	xmonth[0] = xtime[4] ;
	xmonth[1] = xtime[5] ;
	xmonth[2] = '\0' ;

	/* TRANSLATE DIGITS TO MONTH.		*/
	if      ( strcmp(xmonth,"01") == 0 ) strcpy( &esat[3],"JAN");
	else if ( strcmp(xmonth,"02") == 0 ) strcpy( &esat[3],"FEB");
	else if ( strcmp(xmonth,"03") == 0 ) strcpy( &esat[3],"MAR");
	else if ( strcmp(xmonth,"04") == 0 ) strcpy( &esat[3],"APR");
	else if ( strcmp(xmonth,"05") == 0 ) strcpy( &esat[3],"MAY");
	else if ( strcmp(xmonth,"06") == 0 ) strcpy( &esat[3],"JUN");
	else if ( strcmp(xmonth,"07") == 0 ) strcpy( &esat[3],"JUL");
	else if ( strcmp(xmonth,"08") == 0 ) strcpy( &esat[3],"AUG");
	else if ( strcmp(xmonth,"09") == 0 ) strcpy( &esat[3],"SEP");
	else if ( strcmp(xmonth,"10") == 0 ) strcpy( &esat[3],"OCT");
	else if ( strcmp(xmonth,"11") == 0 ) strcpy( &esat[3],"NOV");
	else if ( strcmp(xmonth,"12") == 0 ) strcpy( &esat[3],"DEC");
	else return 2;

	/* SEPARATOR */
	esat[6] = '-' ;

	/* NOW MOVE THE YEAR TO THE RESULT.  */
	strncpy(&esat[7], xtime, 4);

	/* SEPARATOR */
	esat[11] = ' ' ;

	/* NOW MOVE THE HOURS TO THE RESULT.  */
	esat[12] = xtime[8] ;
	esat[13] = xtime[9] ;

	/* SEPARATOR */
	esat[14] = ':' ;

	/* NOW MOVE THE MINUTES TO THE RESULT.  */
	esat[15] = xtime[10] ;
	esat[16] = xtime[11] ;

	/* SEPARATOR */
	esat[17] = ':' ;

	/* NOW MOVE THE SECONDS TO THE RESULT FROM ASFTIME.  */
	strncpy( &esat[18],&asft[15],6);

	/* NOW TERMINATE THE STRING */
	esat[24] = '\0' ;
	return(TRUE) ;
}
