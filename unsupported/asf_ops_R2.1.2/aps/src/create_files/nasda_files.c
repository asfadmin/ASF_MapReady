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
#pragma ident   "@(#)nasda_files.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.nasda_files.c"

#include <stdio.h>

#include "dapps_list.h"
#include "nmalloc.h"

#include "db_sybint.h" 
#include "db_sybextern.h"

#include "aps_db_table.h"
#include "db_dtk.h"
#include "db_seg.h"
#include "db_rgs_down_times.h"
#include "aps_extern.h"
#include "apsfiledef.h"
#include "aps_log_msg.h"		/* for syslogging				*/
#include "file_utilities.h"		/* for syslogging				*/

#include "timeconv.h"


#define BLANK " "
static char blank_str[] = " " ;
static char blank_chr = ' ' ;

extern int j1rt2rsp_(void *dbproc, 
	int *irev, char *asft_start, char *asft_end,
    int *irsp, double *xangl1, double *xangl2, int *ier) ;

typedef
	struct _J1_REQW
	{
		char  requestid[9] ;
		char  acq_date[9] ;
		int   rsp_path ;
		float angle1 ;
		float angle2 ;
		char  sensor[4] ;
		char  ops_gain[2] ;
	} J1_REQW ;


int date_path_angle_compare(J1_REQW *request1, J1_REQW *request2)
{
	static char date_path_angle_key_format[] = "%8.8s%03.3d%06.2f" ;
	char str1[24], str2[24] ;  /* make sure big enough to handle chars */

	/*
	-- for easier comparison create a string out
	-- of the keys and compare those strings
	*/

	sprintf(str1, date_path_angle_key_format,
		request1->acq_date,
		request1->rsp_path,
		request1->angle1) ;

	sprintf(str2, date_path_angle_key_format,
		request2->acq_date,
		request2->rsp_path,
		request2->angle1) ;

	return(strcmp(str1, str2)) ;
}


/**********************************************************************
*
*  Name : DO_J1_REQW
*
*  Purpose:
*	Create a REQW file for NASDA.  
*
*  Input:
*	name		type	descrip
*	sfile		c*	pointer to filename to be created
*	start		c*	pointer to start time
*	stop		c*	pointer to stop time
*	reqw		c*	pointer to temporary REQW table name
*
*	fd		int	file descriptor
*                            
**********************************************************************/
/* NOW obsolete:  int do_j1_reqw(char *filename, char *start, char *stop)  */


/********************************************************************
*  Name : DO_J1_REQQ
*  Module Type: integer function Language: QC
*  $Logfile:   ACS003:[BLD.MPS.SCHD.SRC]DO_J1_REQQ.QCV  $
*  Purpose:
*	Create a REQQ file for NASDA.  
*
*  Input Parameters:
*  Name         Type    Definition
*  sfile	ch*	pointer to filename to be created
*  start	ch*	pointer to start time
*  stop		ch*	pointer to stop time
*  Output Parameters:
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                            
**********************************************************************/
/* NOW OBSOLETE:  int do_j1_reqq(char *filename, char *start, char *stop)  */
 /* do_j1_reqq */



/**********************************************************************
*
*  Name : DO_J1_MSGF
*
*  Purpose:
*	Create a MSGF file for NASDA.  
*
*  Input:
*	name		type	descrip
*	sfile		c*	pointer to filename to be created
*	dstart		c*	pointer to start date.  1990:123
*	dstop		c*	pointer to stop date.   1990:124
*
*                            
**********************************************************************/
int do_j1_msgf(char *filename, char *dstart, char *dstop)
{
	int errloop ;

	char buf[100] ;
	char start[22], stop[22] ;
	char asf_now[22], esa_now[15] ;

	/*	MSGF header		*/
	char file_date[9] ;		/* YYYYMMDD	*/
	char file_time[9] ;		/* hh:mm:ss	*/

	/*	MSGF data	*/
	char start_date[9] ;	/* YYYYMMDD */
	char end_date[9] ;
	char start_time[9] ;	/* hh:mm:ss */
	char end_time[9] ;		/* hh:mm:ss */

	/*	down time data from the asf_dn_times relation */
	char strttime[22], stoptime[22] ;

	static char begin_of_day[] = ":00:00:00.000" ;
	static char end_of_day[]   = ":23:59:59.999" ;

	llist *dn_times ;
	DB_RECORD **dn_time_rec ;
	cursor ptr ;

	FILE *fp ;

	aps_log_msg(file_util_progname, APS_INFO, 
		"Processing MSGF file...\n\n", 
		DO_SYSLOG, DO_PRINT);

	/* convert the input dates to ASF time and then to J1 dates */
	strncpy(start,  dstart, 8) ;
	strcpy(start+8, begin_of_day) ;

	strncpy(stop, dstop, 8) ;
	strcpy(stop+8, end_of_day) ;

	if ((fp = fopen(filename, "w")) == NULL)
	{
		sprintf (file_util_msg, 
			"Error opening NASDA ASF Down Times Report: '%s'\n", filename) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_FILE_ERROR) ;
	}

	/* 
	-- get today's date
	-- and convery to esa format: yyyymmddhhmmss 
    */
	tc_systime2asf(asf_now) ;
	tc_asf2esa(asf_now, esa_now) ;

	/* convert to j1 MSGF header date:  yyyymmdd */
	sprintf(file_date, "%.8s", esa_now) ;

	/* move the time */
	/* convert to j1 MSGF header time:  hh:mm:ss */
	sprintf(file_time, "%.8s", asf_now + 9) ;

	/* count the records to go into the MSGF file. */


	/* the end date is included in the time period. */
	strcpy(stop+8, end_of_day) ;

	/* Get the ASF down times from the MPSDB */
	aps_log_msg(file_util_progname, APS_INFO, 
		"Retrieving ASF down times...\n\n", 
		DO_SYSLOG, DO_PRINT);

	sprintf(where_clause, "where %s < '%s' and %s > '%s'",
		APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STRTTIME), stop, 
		APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STOPTIME), start) ;

	sprintf(orderby_cols, "%s", APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STRTTIME)) ;

	dn_times = db_get_records(APS_dbproc,APS_TABLE(RGS_DOWN_TIMES), 
		where_clause, orderby_cols, APS_CDEFS(RGS_DOWN_TIMES), ALL_COLS) ;

	/* write the header record. */
	sprintf(buf, "MSGF    ERS1FAISHMMO%8.8s%8.8sN0034%05.5d%82s",
		file_date, file_time, NUMELTS(dn_times), " ") ;
	/* write (fd, buf, 128 */
	fprintf(fp, "%128s", buf) ;

	if (NUMELTS(dn_times) == 0)
	{
		sprintf (file_util_msg, 
			"No down times scheduled for period: %s - %s\n",dstart, dstop);
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
	}

	errloop = 0 ;

	dn_time_rec = FIRST(dn_times, ptr) ;
	while (dn_time_rec)
	{
		strcpy(strttime, CAST_RGS_DOWN_TIMES_STRTTIME dn_time_rec[RGS_DOWN_TIMES_STRTTIME]) ;
		strcpy(stoptime, CAST_RGS_DOWN_TIMES_STOPTIME dn_time_rec[RGS_DOWN_TIMES_STOPTIME]) ;

		/* convert time to J1 date YYYYMMDD	*/
		if (!tc_asf2yyyymmdd (strttime, start_date))
		{
			sprintf (file_util_msg, 
				"strttime = %s, \n", strttime) ;
			aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
				DO_SYSLOG, DO_PRINT);
			aps_log_msg(file_util_progname, APS_ERROR, 
				"Error converting asf_dn_times.strttime to YYYYMMDD", 
				DO_SYSLOG, DO_PRINT);
			errloop = 1 ;
			break ;
/* ##		endretrieve */
		}

		/* convert time to J1 date YYYYMMDD	*/
		if (!tc_asf2yyyymmdd (stoptime, end_date))
		{
			sprintf (file_util_msg, 
				"stoptime = %s\n", stoptime) ;
			aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
				DO_SYSLOG, DO_PRINT);
			aps_log_msg(file_util_progname, APS_ERROR, 
				"Error converting asf_dn_times.stoptime to YYYYMMDD", 
				DO_SYSLOG, DO_PRINT);
					errloop = 1 ;
					break ;
		/* ##		endretrieve */
				}

				/* copy start_time (hh:mm:ss) from strttime */
				strncpy(start_time, strttime+9, 8) ;
				start_time[8] = '\0' ;

				/* copy end_time (hh:mm:ss) from stoptime */
				strncpy (end_time, stoptime+9, 8) ;
				end_time[8] = '\0' ;

				/* TODO format doesn't match length 35 vs 34 */
				sprintf(buf, "%8.8s %8.8s%8.8s %8.8s", 
							start_date, start_time, end_date, end_time) ;
				fprintf(fp, "%34s", buf) ;
				/* write(fd, buf, 34) ;*/

				dn_time_rec = NEXT(dn_times, ptr) ;
			} /* while dn_time_rec */

			close(fp);

		   if (errloop) 
		   {
				aps_log_msg(file_util_progname, APS_ERROR, 
					"Error in MSGF file processing...\n", 
					DO_SYSLOG, DO_PRINT);
				return(APS_REPORT_ERROR) ;
		   }

	aps_log_msg(file_util_progname, APS_INFO, 
		"MSGF file was processed normally...\n", 
		DO_SYSLOG, DO_PRINT);
	return(APS_REPORT_OK) ;
}
