#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       csa_files.c

Description:    creates files to send to CSA.  

File Scope Variables:
    
==============================================================================*/
#pragma ident   "@(#)csa_files.c	5.2 98/03/15 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.csa_files.c"

/*====================================================================
Function:     create_csarar_file 

Description:
   This function searches the station down times relation for reception
   down times that have not been reported to CSA and generates a
   Reception Availability Report file for each unreported down time event.
   These file are passed to the FAIF for transmission to CSA.

Parameters: filename, starttime, stoptime
(Note: The above parameters are included to make the call to this function  
      standard.  They are not used.)

Returns:  1 = no records to process
          0 = normal return

Creator:  Gus Faist (Inspired by R. Green)

Creation Date: March '95

Notes:

====================================================================*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "apspath.h"
#include "db_rgs_down_times.h"
#include "db_csa_data.h"
#include "CSAparse.h"
#include "timeconv.h"
#include "GENconversions.h"
#include "APSpmfutilities.h"

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */

/* FOR SYSLOGGING       */
#include    "aps_log_msg.h"
#include    "file_utilities.h"


typedef
    struct _TBLE
    {
    char id;
    char *text;
    }TABLE;

typedef
    struct _TBLE2
    {
    char *idtext;
    char *text;
    }TABLE2;

TABLE disp[] =
    {
    {'N', "NEW   "}, 
    {'C', "CANCEL"}, 
    {NULL, NULL}
    };  

TABLE2 staid[] =
    {
    {"ASF", "F"}, 
    {"MCM", "M"}, 
    {NULL, NULL}
    };
    

DBPROCESS *APS_dbproc;
static llist *down_times;
static llist *count_ptr;

#ifdef DRIVER

void main() /* TEMPORARY driver for function */
{
    char filetype[4]      = "rar" ;
    char strt[22]         = "1995:000:00:00:00.000" ;
    char stop[22]         = "1995:200:00:00:00.000" ;
    char dbname[4]        = "gus";
    char dbuserid[4]      = "gus";
    char dbpassword[9]    = "dappsdev"; 
    int  status;

/* Test Driver: Open the data base */

    APS_dbproc = db_open(dbname, "create_csarar_file", dbuserid, dbpassword, 
        NULL, NULL, &status);
    if(status != DB_OPEN_OK)
    {
    printf("db open failure!\n");
    return ;
    }

/* Test Driver: Call function */

    create_csarar_file(filetype, strt, stop) ;

}/* End Test Driver: main */

#endif

/*======================================================================*/


/*==============================================================================
Function:     create_csarar_file  

Description:    

Returns:        

Creator:        

Creation Date: 

Notes:  This routine uses global variable fa_file_type.
==============================================================================*/
int create_csarar_file(rar, strt, stop)
    char *rar, *strt, *stop ;
{
    char     asftime[22], csatime[22], *pasf, *dispo, *staido ;
    char     *fullpath_filename ;
    char     *metafilename ;
    char     *pathmark ;
    long int tens_year, day ;
    char     filename[13], header_string[42];
    char     column_string[60] ; 
    CSA_STMT temp ;
    CSA_STMT tree_root;
    int      seq_cnt, nrecs, ue_counter, j ;

    PMF_FILENAME    *pmf_descriptors ;
    APSPMF_metadata *PMF_struct ;
    DB_RECORD       **stadntime_rec;
    cursor ptr;

	/*
	-- Quick error checking.  
	-- File type must be CRAR_TYPE or MRAR_TYPE only.
	*/
	if (strcmp(CRAR_TYPE, fa_file_type) != 0
	&&  strcmp(MRAR_TYPE, fa_file_type) != 0)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"Error, file type other than CRAR or MRAR encountered.\n", 
			DO_SYSLOG, DO_PRINT);
		return(APS_REPORT_ERROR) ;
	}

    /*
    -- Special handling:  since there may be many CRAR/MRAR files generated,
    -- we may also have to generate many corresponding PMF files.
	-- NOTE: both CRAR_TYPE and MRAR_TYPE file have identical pmf_descriptors,
	--       Hence a single match on CRAR_TYPE is sufficient.
    */
    if(!identify_PMF_file(CRAR_TYPE, PMF_files, &pmf_descriptors ) )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
"No PMF format found for Radarsat Reception Availability Report.\n", 
            DO_SYSLOG, DO_PRINT);
        return(APS_REPORT_NO_METADATA) ;
    }
    else
    {
        PMF_struct = pmf_descriptors->file_descriptor ;
        strncpy (fa_sat_id, R_SAT, strlen(fa_sat_id) );


        if( !(tc_asf2odl(strt,  fa_file_start_time)
            &&  tc_asf2odl(stop, fa_file_stop_time) )
          )
        {
            sprintf (file_util_msg, 
                "Error converting start/stop time for File Type: %s\n",
                fa_file_type) ;
            aps_log_msg(file_util_progname, APS_ERROR, 
				file_util_msg, DO_SYSLOG, DO_PRINT);
            return(APS_REPORT_ERROR) ;
        }

    }

    /* Read records for reception downtimes that have not been sent to CSA */   

	if (strcmp(CRAR_TYPE, fa_file_type) == 0)
		/*
		-- We are creating a CRAR_TYPE file.
		*/
		sprintf(where_clause, 
		"where %s ='N' and %s ='%s'", 
		APS_COL(RGS_DOWN_TIMES,RGS_DOWN_TIMES_FA_NOTIFICATION), 
		APS_COL(RGS_DOWN_TIMES,RGS_DOWN_TIMES_STATION_ID), 
		"ASF");
	else
		/*
		-- We are creating a MRAR_TYPE file.
		*/
		sprintf(where_clause, 
		"where %s ='N' and %s ='%s'", 
		APS_COL(RGS_DOWN_TIMES,RGS_DOWN_TIMES_FA_NOTIFICATION), 
		APS_COL(RGS_DOWN_TIMES,RGS_DOWN_TIMES_STATION_ID), 
		"MCM");

    down_times = db_get_records(APS_dbproc, APS_TABLE(RGS_DOWN_TIMES), 
        where_clause, NULL, APS_CDEFS(RGS_DOWN_TIMES), ALL_COLS) ;

    /* Exit if no records */

    if (NUMELTS(down_times) == 0)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No reception down times to send to CSA.\n", 
            DO_SYSLOG, DO_PRINT);
        return(APS_REPORT_NONE) ;
    }

    /* Get file sequence number from data base for building file name */

    count_ptr=db_get_records(APS_dbproc, APS_TABLE(CSA_DATA), NULL, 
        NULL, APS_CDEFS(CSA_DATA), ALL_COLS);
    if (NUMELTS(count_ptr) == 0)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No CSA_DATA daily counter in the database.\n", 
            DO_SYSLOG, DO_PRINT);
        return(APS_REPORT_NONE) ;
    }

    stadntime_rec= (DB_RECORD **)FIRST(count_ptr, ptr);
    seq_cnt= CAST_CSA_DATA_RAR_DAILY_COUNTER
        stadntime_rec[CSA_DATA_RAR_DAILY_COUNTER];

    /* Set llist pointer to first unavailability record */

    stadntime_rec = (DB_RECORD **) FIRST(down_times, ptr) ;
 
    /* Loop as long as record pointer is not NULL */

    while (stadntime_rec)
    {
        /* Get system time in asf format and in csa format */

        if (!tc_systime2asf(asftime))
            return(APS_REPORT_ERROR) ;

        if (!tc_asf2csa(asftime, csatime))
            return(APS_REPORT_ERROR) ;

        /* Increment file name sequence number */

        ++seq_cnt ;

        /* Build file name */

        pasf      = asftime + 2 ;
        tens_year = atoi(pasf) ;
        pasf     += 3 ;
        day       = atoi(pasf) ;

		if (strcmp(CRAR_TYPE, fa_file_type) == 0)
			sprintf(filename, "f%02d%03d%02d.rar", tens_year, day, seq_cnt) ;
		else
			sprintf(filename, "m%02d%03d%02d.rar", tens_year, day, seq_cnt) ;

        if (rar[0]  == '/')
        {
            fullpath_filename = malloc( strlen(rar) + strlen(filename) ) ;
            strcpy (fullpath_filename, rar) ;
            pathmark = strrchr (fullpath_filename, '/') ; 
            strcpy (pathmark+1, filename) ;
        }
        else
            fullpath_filename = aps_fullpath(APS_RADARSAT_RAR, filename);
        metafilename =   malloc(strlen(fullpath_filename) +2 + 1) ;
        sprintf(metafilename,"%s.M", fullpath_filename) ;

    
        /* Create file header */

		if (strcmp(CRAR_TYPE, fa_file_type) == 0)
			tree_root = (CSA_STMT) create_header(
				filename, "RADARSAT_1", csatime, "FBDROC", "MCS", 
				"RECEPTION_AVAIL_REPORT") ;
		else
			tree_root = (CSA_STMT) create_header(
				filename, "RADARSAT_1", csatime, "MMDROC", "MCS", 
				"RECEPTION_AVAIL_REPORT") ;

        /* Read columns into display strings and create keyword/value pairs */

        /* Get station id */

        sprintf(column_string, "%s", 
            CAST_RGS_DOWN_TIMES_STATION_ID
            stadntime_rec[RGS_DOWN_TIMES_STATION_ID]);
    
        j=0;
        staido = NULL ;
        while(staid[j].text)
        {
            if((strcmp(column_string, staid[j].idtext)) == 0)
            {
                staido = staid[j].text;
                break;
            }
            j++;
        }
        if (!staido) return (APS_REPORT_ERROR) ;

        temp  = (CSA_STMT) create_keyword_value("UNAVAIL_FACILITY_ID", staido) ;
        append_statement(temp, tree_root) ;


        /* Get unavailable event counter */

        ue_counter = *(DBINT *)
            stadntime_rec[RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER];

        sprintf(column_string, "%d", ue_counter);

        temp  = (CSA_STMT) create_keyword_value("UNAVAIL_EVENT_CYCLIC_COUNTER", 
            column_string) ;
        append_statement(temp, tree_root) ;


        /* Use canned resource */

		if (strcmp(CRAR_TYPE, fa_file_type) == 0)
			temp  = (CSA_STMT) create_keyword_value("RSRC_ID", "RTRCR_03") ;
		else
			temp  = (CSA_STMT) create_keyword_value("RSRC_ID", "RTRCR_02") ;

        append_statement(temp, tree_root) ;
    
        /* Get disposition */

        j = 0;
        dispo = NULL ;
        while(disp[j].text)
        {
            if (disp[j].id == CAST_RGS_DOWN_TIMES_DISPOSITION
                stadntime_rec[RGS_DOWN_TIMES_DISPOSITION])
            {
                dispo = disp[j].text;
                break;
            }
            j++;
        }
        if (!dispo) return (APS_REPORT_ERROR) ;

        temp  = (CSA_STMT) create_keyword_value("AVAIL_RPT_TYPE", dispo) ;
        append_statement(temp, tree_root) ;

        /* 
        -- compare using db value as a precaution against 
        -- an FA change.  
        */
        if ( 'C' != CAST_RGS_DOWN_TIMES_DISPOSITION 
                  stadntime_rec[RGS_DOWN_TIMES_DISPOSITION] )
        {
            /* 
            -- this is NOT a cancellation.  
            -- put in the next few fields.  
            -- these fields are NOT for cancellations: 
            */

            /* Get start of down time */
            sprintf(asftime, "%s", 
                CAST_RGS_DOWN_TIMES_STRTTIME 
                    stadntime_rec[RGS_DOWN_TIMES_STRTTIME]) ;
            if (!tc_asf2csa(asftime, csatime)) return (APS_REPORT_ERROR) ;
            temp  = (CSA_STMT) create_keyword_value("UNAVAIL_TIME", csatime) ;
            append_statement(temp, tree_root) ;

        
            /* Get end of down time */
            sprintf(asftime, "%s", 
                CAST_RGS_DOWN_TIMES_STOPTIME 
                    stadntime_rec[RGS_DOWN_TIMES_STOPTIME]) ;
            if (!tc_asf2csa(asftime, csatime)) return (APS_REPORT_ERROR) ;
            temp  = (CSA_STMT) create_keyword_value("AVAIL_TIME", csatime) ;
            append_statement(temp, tree_root) ;


            /* Get reason for down time */
            sprintf(column_string, "%s", 
                CAST_RGS_DOWN_TIMES_REMARKS
                stadntime_rec[RGS_DOWN_TIMES_REMARKS]);
            temp  = (CSA_STMT) create_keyword_value(
                "UNAVAIL_REMARKS", column_string) ;
            append_statement(temp, tree_root) ;
        }

        /* Insert END-OF-FILE marker */
        temp = (CSA_STMT) create_comment(";###END_OF_FILE");
        append_statement(temp, tree_root) ;


            /* Create file */
        print_tree(tree_root, fullpath_filename) ;

        /* 
        -- Create corresponding PMF file 
        */
        strncpy(PMF_struct[FILE_NAME].field, filename, strlen(filename) );

        if( !APS_create_pmf( metafilename, PMF_struct) )
        {
            sprintf (file_util_msg, 
                "Error creating IMS PMF file for File Type: %s '%s'\n",
                fa_file_type, metafilename) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            return(APS_REPORT_NO_METADATA) ;
        }


        /* Set fa_notification fields to "Y" */ 

        sprintf(fields_to_set, "%s = '%s'", 
            APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_FA_NOTIFICATION), "Y");

        sprintf(where_clause, "\nwhere %s = %d\n", 
            APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER), 
            CAST_RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER
                stadntime_rec[RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER]); 

        nrecs = db_update_records(APS_dbproc, APS_TABLE(RGS_DOWN_TIMES), 
            fields_to_set, where_clause);

        /* Delete tree */

        delete_tree(tree_root) ;
        if (nrecs != 1) return (APS_REPORT_DB_ERROR) ;

        /* Point to next record */

        stadntime_rec = (DB_RECORD **) NEXT(down_times, ptr) ;
    } /* End while */

    /* Save file name sequence number in data base */

    sprintf(fields_to_set, "%s = %d", 
        APS_COL(CSA_DATA, CSA_DATA_RAR_DAILY_COUNTER), seq_cnt);
    nrecs = db_update_records(APS_dbproc, APS_TABLE(CSA_DATA), 
        fields_to_set, NULL);

    if (nrecs != 1) return (APS_REPORT_DB_ERROR) ;

    /* Normal Return */

    return(APS_REPORT_OK) ;

} /* End of create_csarar_file */
