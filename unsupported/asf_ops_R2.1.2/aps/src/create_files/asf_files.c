#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:

==============================================================================*/
#pragma ident   "@(#)asf_files.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.asf_files.c"

#include <stdio.h>

#include "nmalloc.h"
#include "mps.h"

#include "db_sybint.h" 
#include "db_sybextern.h"

#include "aps_db_table.h"
#include "db_dtk.h"
#include "db_cvrg.h"
#include "db_satsensor.h"

#include "timeconv.h"

#include "aps_extern.h"
#include "apsfiledef.h"
#include "aps_log_msg.h"	/* for syslogging	*/
#include "file_utilities.h"	/* for syslogging	*/

#define BLANK " "

typedef
	struct _PAP_EVENT
	{
		char *sat ;
		char *sensor ;
		DBINT rev ;
		DBINT dtkid ;
		char *actid ;
		char *strttime ;
		char *stoptime ;
		char *transid ;  
		DBFLT8 nrlat1 ;
		DBFLT8 nrlon1 ;
		DBFLT8 farlat1 ;
		DBFLT8 farlon1 ;
		DBFLT8 nrlat2 ;
		DBFLT8 nrlon2 ;
		DBFLT8 farlat2 ;
		DBFLT8 farlon2 ;
		char *dtkstat ;
		char *fadtkid ;
	} PAP_EVENT ;


static char blank[] = " " ;

static char mtables[256] ;  /* for multiple table definitions */

char display_string[1024] ;

#define CAST_REC_COUNT *(DBINT *)
#define REC_COUNT 0



/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green

Creation Date:  Thu May 25 11:16:39 PDT 1995

Notes:		
==============================================================================*/
static char * recode_transid(char *transid)
{
	static char F1[] = "F1" ;
	static char F2[] = "F2" ;
	static char CB[] = "CB" ;

   	/* 
	-- if necessary RE-CODE transid 
	--  from 01 to F1 
	--    or 02 to F2 
	--    or 03 to CB 
	*/

	if (strncmp(transid, "01", 2) == 0 
	|| strncmp(transid, " 1", 2) == 0)
		transid = F1 ;
	else if (strncmp(transid, "02", 2) == 0 
	|| strncmp(transid, " 2", 2) == 0)
		transid = F2 ;
	else if (strncmp(transid, "03", 2) == 0 
	|| strncmp(transid, " 3", 2) == 0)
		transid = CB ;

	return(transid) ;
}



/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green

Creation Date:  Thu May 25 11:16:49 PDT 1995

Notes:		
==============================================================================*/
static int time_compare(PAP_EVENT *event1, PAP_EVENT *event2)
{
	return(strcmp(event1->strttime, event2->strttime)) ;
}



/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green

Creation Date:  Thu May 25 11:16:55 PDT 1995

Notes:		
==============================================================================*/
int create_pasp_file(char *filename, char *start, char *stop)
{
	llist *datatakes ;
	DB_RECORD **dtk_rec ;
	cursor ptr ;

	FILE *fp ;

	/* get the planned, scheduled, and deleted datatakes from the db */

	sprintf(where_clause, 
		"where (%s >= '%s') and (%s <= '%s') \n\
		and (%s = 'PLN' or %s = 'SCH')",
		APS_COL(DTK, DTK_STRTTIME), start, 
		APS_COL(DTK, DTK_STOPTIME), stop,
		APS_COL(DTK, DTK_DTKSTAT),
		APS_COL(DTK, DTK_DTKSTAT)) ;

	sprintf(orderby_cols, "%s", APS_COL(DTK, DTK_STRTTIME)) ;

	datatakes = db_get_records(APS_dbproc, APS_TABLE(DTK), 
		where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;

	if (!datatakes)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error retrieving datatakes...\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_DB_ERROR) ;
	}

	if (!NUMELTS(datatakes))
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"No datatakes found...\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_NONE) ;
	}

	if ((fp = fopen(filename, "w")) == NULL)
	{
		sprintf (file_util_msg, 
			"Error opening PAP file '%s'\n", filename) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_FILE_ERROR) ;
	}

	dtk_rec = FIRST(datatakes, ptr) ;
	while (dtk_rec)
	{
		/* write the REC to the file */

   		fprintf(fp,"%2.2s/%1.1s/%05d.%02d ",
			dtk_rec[DTK_SAT], dtk_rec[DTK_SENSOR], 
			CAST_DTK_REV dtk_rec[DTK_REV], 
			CAST_DTK_DTKID dtk_rec[DTK_DTKID]) ;

		fprintf(fp, 
			"%-7.6s%-5.4s%-17.16s%-5.4s%-17.16s%-3.2s%-33.32s%37.s",
			dtk_rec[DTK_ACTID], 
			dtk_rec[DTK_STRTTIME], (char *) (dtk_rec[DTK_STRTTIME])+5, 
			dtk_rec[DTK_STOPTIME], (char *) (dtk_rec[DTK_STOPTIME])+5,
			dtk_rec[DTK_TRANSID], dtk_rec[DTK_SITENAME], BLANK) ;

		dtk_rec = NEXT(datatakes, ptr) ;
	}
	fclose(fp) ;
	return(APS_REPORT_OK) ;
}




/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green

Creation Date:  Thu May 25 11:17:22 PDT 1995

Notes:		
==============================================================================*/
int create_pap_file(char *filename, char *start, char *stop)
{
	llist *datatakes ;
	DB_RECORD **dtk_rec ;

	llist *coverages ;
	DB_RECORD **cvrg_rec ;
	cursor ptr ;

	int num_nc_records ;
	int rec_count = 0 ;

	int stat ;
	double startet ;
	double stopet ;
	char starttime[22] ;
	PAP_EVENT *pap_event ;
	ord_llist *pap_events ;

	char *transid ;
	FILE *fp ;

	/* get the planned, scheduled, and deleted datatakes from the db */

	sprintf(where_clause, 
		"where (strttime >= '%s') and (stoptime <= '%s') \n\
		and (dtkstat = status) and (pap = 'Y')",
		start, stop) ;
	sprintf(orderby_cols, "strttime") ;

	sprintf(mtables, "%s, schstat", APS_TABLE(DTK)) ;

	/* Copy the datatakes for the requested time span from the database
		into a linked list in memory */
	datatakes = db_get_records(APS_dbproc, mtables,
		 where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;

	if (!datatakes)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error retrieving datatakes...\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_DB_ERROR) ;
	}

	/* Convert the start and stop times from asf format to ephemeris */
	if (!tc_asf2et(start, &startet))
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error converting start time to Julian...\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_ERROR) ;
	}	

	if (!tc_asf2et(stop, &stopet))
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error converting stop time to Julian...\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_ERROR) ;
	}

	sprintf(where_clause, 
		"where (crossflag = 'N') \n\
		and (mjdate >= %8.3f) and (mjdate <= %8.3f)", startet, stopet) ;

	/*
	-- TO DO this check may not need to be done why is the check done 
	-- anyway.  Was it a memory limitation on the VAX or that the report 
	-- shouldn't have more than 5000 records
	*/

	num_nc_records = db_num_records(APS_dbproc, APS_TABLE(CVRG), where_clause) ;
	if (num_nc_records > 5000)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Your time bracket for the PAP is too large...\n", 
			DO_SYSLOG, DO_PRINT);
		aps_log_msg(file_util_progname, APS_ERROR, 
			" Break into two smaller reports\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_ERROR) ;
	}
	else if (num_nc_records == 0)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"No nodal crossing records found.   DONE.\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_NONE) ;
	}

#ifdef DEBUG
	sprintf (file_util_msg, 
		"REC COUNT %d\n", num_nc_records) ;
	aps_log_msg(file_util_progname, APS_DEBUG, file_util_msg, 
		DO_SYSLOG, DO_PRINT);
#endif

	sprintf(orderby_cols, "mjdate") ;

	/*
	-- Copy the nodal-crossing coverage rows for the requested time span 
	-- from the database into a linked list in memory 
	*/

	coverages = db_get_records(APS_dbproc, APS_TABLE(CVRG), 
		where_clause, orderby_cols, APS_CDEFS(CVRG), ALL_COLS) ;

	if (!coverages)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error retrieving coverage records...\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_DB_ERROR) ;
	}


#ifdef DEBUG
		sprintf (file_util_msg, 
			"RECS %d\n", NUMELTS(coverages)) ;
		aps_log_msg(file_util_progname, APS_DEBUG, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
#endif

	/* Copy the coverage rows into a new ordered linked list */

	pap_events = (ord_llist *) create_dyn_ord_llist(time_compare) ;
	cvrg_rec = FIRST(coverages, ptr) ;
	while (cvrg_rec)
	{
		pap_event = (PAP_EVENT *)  NEW(sizeof(PAP_EVENT)) ;

		pap_event->sat      = (char *) CAST_CVRG_SAT cvrg_rec[CVRG_SAT] ;
		pap_event->sensor   = (char *) CAST_CVRG_SENSOR  cvrg_rec[CVRG_SENSOR] ;
		pap_event->rev      = CAST_CVRG_REV cvrg_rec[CVRG_REV]  ;
		pap_event->dtkid    = 0 ;
		pap_event->actid    = blank ;
		/* Create a space to put the start time in and point to it */	
		pap_event->strttime = (char *) NEW(22);
		/* Convert ephemeris time to asf 21 character string */
		if (!tc_et2asf(
				(CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE]), pap_event->strttime))
		{
			aps_log_msg(file_util_progname, APS_ERROR, 
				"Error converting ephemeris time to ASF\n", 
				DO_SYSLOG, DO_PRINT);
			return(APS_REPORT_ERROR) ;
		}

		pap_event->stoptime = BLANK ;
		pap_event->transid  = BLANK ;  

		pap_event->nrlat1   = CAST_CVRG_SUBLAT cvrg_rec[CVRG_SUBLAT] ;
		pap_event->nrlon1   = CAST_CVRG_SUBLON cvrg_rec[CVRG_SUBLON] ;

		pap_event->farlat1  = 0 ;
		pap_event->farlon1  = 0 ;
		pap_event->nrlat2   = 0 ;
		pap_event->nrlon2   = 0 ;
		pap_event->farlat2  = 0 ;
		pap_event->farlon2  = 0 ;
		pap_event->dtkstat  = "NOD" ;
		pap_event->fadtkid  = BLANK ;

		INS_BY_KEY(pap_events, pap_event, NULL, NULL) ;
		 
		cvrg_rec = NEXT(coverages, ptr) ;
	}
	
	/* now add the datatake records to the ordered linked list */
	dtk_rec = FIRST(datatakes, ptr) ;
	while (dtk_rec)
	{
		pap_event = (PAP_EVENT *)  NEW(sizeof(PAP_EVENT)) ;

		transid = (char *) CAST_DTK_TRANSID dtk_rec[DTK_TRANSID] ;
		transid = recode_transid(transid) ;

		pap_event->sat      = (char *) CAST_DTK_SAT dtk_rec[DTK_SAT] ;
		pap_event->sensor   = (char *) CAST_DTK_SENSOR  dtk_rec[DTK_SENSOR] ;
		pap_event->rev      = CAST_DTK_REV dtk_rec[DTK_REV]  ;
		pap_event->dtkid    = CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;
		pap_event->actid    = (char *) CAST_DTK_ACTID dtk_rec[DTK_ACTID] ;
		pap_event->strttime = (char *) dtk_rec[DTK_STRTTIME] ;
		pap_event->stoptime = (char *) dtk_rec[DTK_STOPTIME] ;
		pap_event->transid  = transid ;  

		pap_event->nrlat1   = CAST_DTK_NRLAT1 dtk_rec[DTK_NRLAT1] ;
		pap_event->nrlon1   = CAST_DTK_NRLON1 dtk_rec[DTK_NRLON1] ;
		pap_event->farlat1  = CAST_DTK_FARLAT1 dtk_rec[DTK_FARLAT1] ;
		pap_event->farlon1  = CAST_DTK_FARLON1 dtk_rec[DTK_FARLON1] ;
		pap_event->nrlat2   = CAST_DTK_NRLAT2 dtk_rec[DTK_NRLAT2] ;
		pap_event->nrlon2   = CAST_DTK_NRLON2 dtk_rec[DTK_NRLON2] ;
		pap_event->farlat2  = CAST_DTK_FARLAT2 dtk_rec[DTK_FARLAT2] ;
		pap_event->farlon2  = CAST_DTK_FARLON2 dtk_rec[DTK_FARLON2] ;
		pap_event->dtkstat  = (char *) dtk_rec[DTK_DTKSTAT] ;
		pap_event->fadtkid  = (char *) dtk_rec[DTK_FADTKID] ;
		 
		INS_BY_KEY(pap_events, pap_event, NULL, NULL) ;

		dtk_rec = NEXT(datatakes, ptr) ;
	}

	if ((fp = fopen(filename, "w")) == NULL)
	{
		sprintf (file_util_msg, 
			"Error opening PAP file '%s'\n", filename) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_FILE_ERROR) ;
	}

	/*
	-- check datatake values to see if they are within bounds 
	-- if so place in the PAP file 
	*/

	pap_event = FIRST(pap_events, ptr) ;
	while (pap_event)
	{
		/* from mps_lib:mps.h  */
		if (pap_event->rev    < REVMIN || pap_event->rev     > REVMAX 
		|| pap_event->dtkid   < IDMIN  || pap_event->dtkid   > IDMAX 
		|| pap_event->nrlat1  < LATMIN || pap_event->nrlat1  > LATMAX 
		|| pap_event->nrlon1  < LONMIN || pap_event->nrlon1  > LONMAX 
		|| pap_event->farlat1 < LATMIN || pap_event->farlat1 > LATMAX 
		|| pap_event->farlon1 < LONMIN || pap_event->farlon1 > LONMAX 
		|| pap_event->nrlat2  < LATMIN || pap_event->nrlat2  > LATMAX 
		|| pap_event->nrlon2  < LONMIN || pap_event->nrlon2  > LONMAX 
		|| pap_event->farlat2 < LATMIN || pap_event->farlat2 > LATMAX 
		|| pap_event->farlon2 < LONMIN || pap_event->farlon2 > LONMAX) 
		{
			sprintf (file_util_msg, 
				"Record %.2s/%.1s/%05d.%02d field out of bounds",
				pap_event->sat, pap_event->sensor,
				pap_event->rev, pap_event->dtkid) ;
			aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
				DO_SYSLOG, DO_PRINT);
		}
		else
		{
			

			/* write the REC to the file */
 	  		
			fprintf(fp,"%2.2s/%1.1s/%05d.%02d ",
				pap_event->sat, pap_event->sensor, 
				pap_event->rev, pap_event->dtkid) ;

			fprintf(fp, "%-7.6s%-5.4s%-17.16s%-5.4s%-17.16s%-3.2s",
				(char *) pap_event->actid,
				pap_event->strttime, (char *) ((pap_event->strttime)+5), 
				pap_event->stoptime, (char *) ((pap_event->stoptime)+5),
				pap_event->transid) ;

			fprintf(fp, "%-8.2f%-8.2f%-8.2f%-8.2f%-8.2f%-8.2f%-8.2f%-8.2f",
				pap_event->nrlat1, pap_event->nrlon1,
				pap_event->farlat1, pap_event->farlon1,
				pap_event->nrlat2, pap_event->nrlon2,
				pap_event->farlat2, pap_event->farlon2) ;

			fprintf(fp, "%-2.1s%-21.20s",
				pap_event->dtkstat, pap_event->fadtkid) ;
			rec_count++ ;		
		}
		pap_event = (PAP_EVENT *) NEXT(pap_events, ptr) ;
	}
	fclose(fp) ;

	if (rec_count)
	{
		sprintf (file_util_msg, 
			"Records written to PAP file '%s' : %d    DONE.\n", 
			filename, rec_count) ;
		aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
	}
	return(APS_REPORT_OK) ;
}



/*==============================================================================
Function:       create_asfwos_file

Description:    Create a WOS whose destination is ASF
            	
Parameters:     
            	char *filename
            	char *starttime
            	char *stoptime

Returns:        TRUE/FALSE

Creator:        Gus Faist

Creation Date:  Thu May 25 11:11:21 PDT 1995

Notes:		
==============================================================================*/
int create_asfwos_file(char *filename, char *start, char *stop )
{
	int	status ;
	int	mcmflag ;

/* Build asf where_clause */

	sprintf(where_clause, 
		"where (strttime >= '%s') and (stoptime <= '%s') \n\
		and (actid like 'RTS%%' or actid like 'RTO%%' \n\
  	 		or actid like 'RTV%%' or actid like 'DMP%%') \n\
	  and (dtkstat = status) and (wos = 'Y') and (station_id = 'ASF')",
		start, stop) ;


/* Call generic wos file maker */

	mcmflag = FALSE ;
	status = create_wos_file(filename, start, stop, where_clause, mcmflag ) ;

	return status ;
}



/*==============================================================================
Function:       create_mcmasfwos_file

Description:    Create a WOS whose destination is McMurdo
            	
Parameters:     
            	char *filename
            	char *starttime
            	char *stoptime

Returns:        TRUE/FALSE

Creator:        Gus Faist

Creation Date:  Thu May 25 11:11:21 PDT 1995

Notes:		
==============================================================================*/
int create_mcmwos_file(char *filename, char *start, char *stop )
{
	int	status ;
	int	mcmflag ;

/* Build mcm where_clause */

	sprintf(where_clause, 
		"where (strttime >= '%s') and (stoptime <= '%s') \n\
		and (actid like 'RTS%%' or actid like 'RTO%%' \n\
		  or actid like 'RTV%%' or actid like 'DMP%%') \n\
		and (dtkstat = status) and (wos = 'Y') and (station_id = 'MCM')",
		start, stop) ;


/* Call generic wos file maker */

	mcmflag = TRUE ;
	status = create_wos_file(filename, start, stop, where_clause, mcmflag ) ;

	return status ;
}



/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green

Creation Date:  Thu May 25 11:18:50 PDT 1995

Notes:		
==============================================================================*/
int create_wos_file(char *filename, char *start, char *stop, char *where_clause,
	 int mcmurdo_wos_flag)
{
	llist *datatakes ;
	DB_RECORD **dtk_rec ;
	cursor ptr ;

	int rec_count = 0 ;

	char year[] = "1995" ;
	char date[] = "DDD:HH:MM:SS:CCC" ;
	char buf[512] ;

	char *transid ;
	FILE *fp ;

	sprintf(mtables, "%s, schstat", APS_TABLE(DTK)) ;

	sprintf(orderby_cols, "%s", APS_COL(DTK, DTK_STRTTIME)) ;

	datatakes = db_get_records(APS_dbproc, mtables, where_clause, 
		orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;

	if (!datatakes)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error retrieving datatakes...\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_DB_ERROR) ;
	}

	if (!NUMELTS(datatakes))
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"No datatakes found...    DONE.\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_NONE) ;
	}

	/* records exist so let's open the wos file */
	if ((fp = fopen(filename, "w")) == NULL)
	{
		sprintf (file_util_msg, 
			"Error opening WOS file '%s'\n", filename) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_FILE_ERROR) ;
	}

	/* Build and write a header if processing the WFF file */

	if (mcmurdo_wos_flag == TRUE)
	{
		if (!tc_systime2yr_date(year, date))
		{
			aps_internal_error(stderr, __FILE__, __LINE__,
				"SYSTIME error while building mwos\n") ;
			return(APS_REPORT_ERROR) ;
		}

		sprintf(buf, "%s %s WO WFF APS %17s\n",
			year, date, " ") ;
	
		fprintf(fp, buf) ;	
	}

	dtk_rec = FIRST(datatakes, ptr) ;
	while (dtk_rec)
	{
		if (CAST_DTK_REV dtk_rec[DTK_REV] < REVMIN 
		|| CAST_DTK_REV dtk_rec[DTK_REV] > REVMAX 
		|| CAST_DTK_DTKID dtk_rec[DTK_DTKID] < IDMIN 
		|| CAST_DTK_DTKID dtk_rec[DTK_DTKID] > IDMAX)
		{
			sprintf(display_string,
				"Error: Datatake %.2s/%.1s/%05d.%02d field out of bounds",
				dtk_rec[DTK_SAT], 
				CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
				CAST_DTK_REV dtk_rec[DTK_REV], 
				CAST_DTK_DTKID dtk_rec[DTK_DTKID]) ;

			aps_log_msg(file_util_progname, APS_ERROR, 
				"Record will not be written to the WOS:", 
				DO_SYSLOG, DO_PRINT);
			aps_log_msg(file_util_progname, APS_ERROR, display_string, 
				DO_SYSLOG, DO_PRINT);
		}
		else
		{
			/* write the REC to the file */

   			fprintf(fp,"%2.2s/%1.1s/%05d.%02d ",
				dtk_rec[DTK_SAT], dtk_rec[DTK_SENSOR], 
				CAST_DTK_REV dtk_rec[DTK_REV], 
				CAST_DTK_DTKID dtk_rec[DTK_DTKID]) ;

			transid = (char *) CAST_DTK_TRANSID dtk_rec[DTK_TRANSID] ;
			transid = recode_transid(transid) ;

			fprintf(fp, 
				"%-7.6s%-5.4s%-17.16s%-5.4s%-17.16s%-3.2s%-33.32s%37.s",
				dtk_rec[DTK_ACTID], 
				dtk_rec[DTK_STRTTIME], (char *) (dtk_rec[DTK_STRTTIME])+5, 
				dtk_rec[DTK_STOPTIME], (char *) (dtk_rec[DTK_STOPTIME])+5,
				transid, dtk_rec[DTK_SITENAME], BLANK) ;

			if (mcmurdo_wos_flag)
				fprintf(fp,"\n") ;			

			rec_count++ ;
		}
		dtk_rec = NEXT(datatakes, ptr) ;
	}

	if (rec_count)
	{
		sprintf (file_util_msg, 
			"Records written to WOS file '%s' : %d    DONE.\n", 
			filename, rec_count) ;
		aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
	}
	return(APS_REPORT_OK) ;
}
