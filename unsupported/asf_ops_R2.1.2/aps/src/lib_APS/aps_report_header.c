#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#include <stdio.h>
#include "dapps_defs.h"
#include "fa_defs.h"

#define PRINT_DIAG 1
#undef	PRINT_DIAG

EQUIV_TABLE FA_agency_name[] =
{
	{ "ESA",	"European Space Agency (ESA)"},
	{ "NASDA",	"National Space Development Agency (NASDA)"},
	{ "CSA",	"Canadian Space Agency (CSA)"},
	{ "WFF",	"Wallops Flight Facility (WFF)"},
	{ "ASF",	"Alaska SAR Facility (ASF)"}, /* used by WOS comparison tool */
	{NULL,NULL}
} ;

EQUIV_TABLE FA_file_type[] =
{
	{"AWOS","Scheduled Activities"},/* ASF */
	{"MWOS","Scheduled Activities"},/* WFF */
	{"CRRA",	"Requests"},		/* CSA */
	{"CRRM",	"Requests"},		/* CSA */
	{"CRSA",	"Schedules"},		/* CSA */
	{"CRSM",	"Schedules"},		/* CSA */
	{"ARES",	"Requests"},		/* CSA */
	{"SHAQ",	"Schedules"},		/* ESA */
	{"SHQP",	"Schedules"},		/* ESA */
	{"MPSG",	"Requests"},		/* ESA, also known as the GAP file */
	{"REQA",	"Requests"},		/* NASDA */ 
	{"REQM",	"Requests"},		/* NASDA */
	{"OPLN",	"Schedules"},		/* NASDA */
	{"REQR",	"Requests"},		/* NASDA ADEOS */
	{"OPL1",	"Schedules"},		/* NASDA ADEOS */
	{NULL,NULL}
} ;



/*==============================================================================
Function:       aps_report_header

Description:    This function create and writes the aps report header records.

Parameters:     
            	FILE *fp 
            	char *FlightAgencyID  
            	char *FileTypeID
            	char *filename
            	char *creation_date

Returns:        TRUE/FALSE 
            	TRUE  = Successful Header Creation
            	FALSE = Error No Header Created

Creator:        Gus Faist

Creation Date:  Thu Jun  8 14:58:45 PDT 1995

Notes:	This module was originally named dtk_report_header 
==============================================================================*/
#pragma ident	"@(#)aps_report_header.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.aps_report_header.c"

int aps_report_header(
	FILE *fp, 
	char *flight_agency_name, 
	char *file_type, 
	char *filename, 
	char *creation_date)
{
	int j ;

	static char header[] =
"\n\
Flight Agency (FA)     : %s\n\
FA File Processed/Type : %s / %s\n\
\n\
FA File Creation Date  : %s\n\
APS Processing Date    : %s\n\
-------------------------------------------------------------------------------\
\n\n" ;

	char curr_asftime[] = "1995:002:00:00:00.000" ;
	char *file_type_name ;
	char *full_flight_agency_name ;

	if (!fp)
		return(FALSE) ;

	/* transate the flight_agency_name to full_flight_agency_name */
	j = 0 ;
	full_flight_agency_name = NULL ;
	while (FA_agency_name[j].fa_string)
	{
        if (strncmp(flight_agency_name, FA_agency_name[j].fa_string,
                strlen(FA_agency_name[j].fa_string)) == 0)
        {
            full_flight_agency_name = FA_agency_name[j].aps_string ;
#ifdef PRINT_DIAG
            printf ("aps_report_header: translated |%s|to|%s|\n",
                FA_agency_name[j].fa_string,FA_agency_name[j].aps_string);
#endif
            break;
        }
		j++ ;

	}


	/* if no agency name matches the id, return FALSE */
	if (!full_flight_agency_name)
		return(FALSE) ;
	


	/* transate the FileType ID to the FileType Text */
	j = 0 ;
	file_type_name = NULL ;
	while (FA_file_type[j].fa_string)
	{
        if (strncmp(file_type, FA_file_type[j].fa_string,
                strlen(FA_file_type[j].fa_string)) == 0)
        {
            file_type_name = FA_file_type[j].aps_string ;
#ifdef PRINT_DIAG
            printf ("aps_report_header: translated |%s|to|%s|\n",
                FA_file_type[j].fa_string,FA_file_type[j].aps_string);
#endif
            break;
        }
		j++ ;

	}

	/* if no file type matches the id, return FALSE */
	if (!file_type_name)
		return(FALSE) ;



	/* get the current time for this processing */
	tc_systime2asf(curr_asftime) ;
	if (tc_validate_asf_datetime(curr_asftime) != TRUE) 
		return(FALSE) ;	

	/* Now write the header */
	fprintf(fp, header, 
		full_flight_agency_name, filename, file_type_name, 
		creation_date, curr_asftime) ;

#ifdef MAIN
	printf(header, 
		agency_name, filename, file_type, 
		creation_date, curr_asftime) ;
#endif
	return (TRUE) ;
} 

