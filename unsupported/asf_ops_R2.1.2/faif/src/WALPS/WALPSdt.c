/*==============================================================================
Filename:	WALPSdt.c

Description:
	WALLOPS datatake file parser function.  This module contains the 
function for parsing parts of a WALLOPS datatake file.  The specification 
of the WALLOPS file format is described in the Alaska SAR Facility (ASF) 
to Wallops Flight Facility (WFF) Interface Specification Document (ISD), 
JPL-D11871.

External Functions:
	alloc_WALPS_DNL_record
	extract_WALPS_DNL_record
        extract_dnl_downlinks
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  Oct. 96 - RH
    Updated to ISD Version 2.3
2.  July '97 - R. Hoffman
    Reverse linked list output.
    Guard against empty SENSOR or SEQUENCE values.
3.  July '97 - R. Hoffman
    Fix SYSTEM_ACTIVITY_ID
    Change dl_ to ds_ (for part of PR 2684)
4.  Aug. '97 - R. Hoffman
    Fix times for "no data" cases.
==============================================================================*/

static char SccsFile[] = "WALPSdt.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)WALPSdt.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <syslog.h>
#include "odldef.h"
#include "odlinter.h"
#include "dapps_list.h"
#include "nmalloc.h"
#include "WALPS.h"
#include "WALPShdr.h"
#include "WALPSdt.h"

#ifdef __STDC__
WALPS_DT_Record *alloc_WALPS_DNL_record(void) ;
int         extract_WALPS_DNL_record(FILE *, WALPS_DT_Record *, AGGREGATE) ;
int         extract_dnl_downlinks(WALPS_Header_Record *WALPShdr, 
                      char *path, AGGREGATE top, llist *bar);
#else
WALPS_DT_Record *alloc_WALPS_DNL_record() ;
int         extract_WALPS_DNL_record() ;
int         extract_dnl_downlinks() ;
#endif

extern void *util_do_malloc() ;
extern int   extract_param_value() ;


/*==============================================================================
Function:	WALPS_DT_Record *alloc_WALPS_DNL_record(void)

Description:	
	Allocate space for a WALPS datatake record.  The record contains 
various fields storing information obtained from a WALPS file datatake
ODL object.  Once the datatake record is allocated successfully, the field
values are initialized and a pointer to the record is returned.  If the
allocation did not succeed, NULL is returned.

Parameters: 	None
Returns:	pointer to newly allocated DT record or NULL 
Creator:	Norbert Piega	
Creation Date:	10/19/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
WALPS_DT_Record *
alloc_WALPS_DNL_record(void)
#else
WALPS_DT_Record *
alloc_WALPS_DNL_record()
#endif
{
   WALPS_DT_Record *dnl_record = NULL ;

   dnl_record = (WALPS_DT_Record *)util_do_malloc(sizeof(WALPS_DT_Record)) ;
   if (dnl_record != NULL)
   {
      *dnl_record->file_name = NULL ;
      *dnl_record->satellite = NULL ;
   }

   return(dnl_record) ;

} /* alloc_WALPS_DNL_record */




/*==============================================================================
Function:	int extract_WALPS_DNL_record(FILE *WALPS_fp, 
		   WALPS_DT_Record *WALPS_DNL_rec, AGGREGATE top)

Description:	
	This function parses the input file represented by the file
pointer WALPS_fp to obtain and store datatake record information.  The 
table WALPS_Hdr_Keyword_Table is consulted during the parsing to identify 
valid WALPS identifiers and expected symbols (ex.  start of a comment line).
The datatake information obtained are stored in the passed datatake
record WALPS_DNL_rec.  If the datatake record was filled successfully,
ACCEPT is returned, otherwise, REJECT is returned.  In case of errors
during parsing, ERROR is returned.

Parameters:
	FILE *WALPS_fp - pointer to WALPS input file stream
	WALPS_DT_Record *WALPS_DNL_rec - WALPS datatake record where 
	   extracted datatake record information will be stored
        AGGREGATE top - topmost aggregate object for input file

Returns:	
	ACCEPT - all datatake record information obtained successfully
	REJECT - default
	ERROR - memory allocation errors, unable to complete datatake record

Creator:	Norbert Piega	
Creation Date:	10/19/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_WALPS_DNL_record(FILE *WALPS_fp, WALPS_DT_Record *WALPS_DNL_rec, 
			AGGREGATE top)
#else
int
extract_WALPS_DNL_record(WALPS_fp, WALPS_DNL_rec, top)
   FILE *WALPS_fp ;
   WALPS_DT_Record *WALPS_DNL_rec ;
   AGGREGATE top ;
#endif
{
   AGGREGATE object ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int  num_dnl_recs ; 

   if (top == NULL)
   {
      syslog(LOG_DEBUG, "ERROR, ODL top node is NULL\n") ;
      return(ERROR) ;
   }

   if ((object = FindObject(top, WALPS_DTRECORD_NAME, NULL)) == NULL)
   {
      sprintf(logmsg, 
         "ERROR, unable to find datatake record for %s\n", 
	  WALPS_DNL_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(REJECT) ;
   }

   /* Look for the keywords expected inside a DOWNLINK_RECORD.  */

   /*  All downlink records in a DNL file have the same satellite,  */
   if (extract_param_value(object, WALPS_DNL_rec->satellite, 
	 SATELLITE_KEYWD) == ERROR)
      return(ERROR) ;

   return(ACCEPT) ;

} /* extract_WALPS_DNL_record */




/*==============================================================================
Function:	int extract_dnl_downlinks(WALPS_Header_Record WALPShdr, 
                      char *path, AGGREGATE top, llist *bar);

Description:	
	This function uses the ODL tree, top, to obtain and store tape
downlink records from a WFF Downlink file.  If successful, ACCEPT is returned, 
otherwise, ERROR is returned.

Parameters:
        WALPS_Header_Record WALPShdr - Header information
        char *path - of incoming WFF DNL file
        AGGREGATE top - topmost aggregate object for input file
        llist *bar - linked list of files to send to IMS, which will
           include the downlink files extracted here

Returns:	
	OK     - all downlink record information obtained successfully
	ERROR  - any problems

Creator:	Rodney Hoffman	
Creation Date:	November 1996
Notes:		
==============================================================================*/
#ifdef __STDC__
int extract_dnl_downlinks(WALPS_Header_Record *WALPShdr, 
                      char *path, AGGREGATE top, llist *bar)
#else
int extract_dnl_downlinks(WALPShdr, path, top, bar)
   WALPS_Header_Record *WALPShdr ;
   char                *path;
   AGGREGATE            top ;
   llist               *bar;
#endif
{
   ASF_Data_Seq_Msg_Record     dsm;
   AGGREGATE                   root, dnl_tree;
   FILE                       *dsm_file_ptr ;
   char                        dsm_path[80] ;
   PMF_FILE_LIST              *filerecord ;
   char                       *dsmfile;
   char                        dname[31];
   char                        wff_filetype[4] = "DNL";
   char                        recorded[4];
   int                         i, status, num_downlinks;
   time_t                      current_time;
   time_t                     *null_ptr = NULL;
   clock_t                     ticks;
   char                        full_platform[11];     /* for dataset name */

   /* Set up the time to use for START_TIME and END_TIME when DNL shows
      no data recorded. */
   struct Value_Data           temp_value_data;
   struct ODLDate              no_data_time;
   char                       *temp_bogus_time;
   temp_bogus_time = (char *)util_do_malloc(sizeof(char) * 22);
   /* Use the value HC uses: the "beginning of time" according to SUN Solaris */
   strcpy (temp_bogus_time, "1970-001T00:00:00.000");
   temp_value_data = ODLConvertDateTime (temp_bogus_time, 21);
   no_data_time = temp_value_data.value.date_time;
   free (temp_bogus_time);
 
   dsmfile = (char *)util_do_malloc(sizeof(char) * (strlen(path)+ 35));
   /* Fill in constant portions of dsm */
   strcpy (dsm.destination, "IMS");
   dsm.number_of_records = 1;
   strcpy (dsm.msg_type, "DATA_SEQUENCE_MESSAGE");
   strcpy (dsm.source, "FAIF");
   strcpy (dsm.system_activity_id, "FAIF_0");
   strcpy (dsm.system_activity_type, "DOWNLINK");
   strcpy (dsm.media_id, "");
   strcpy (dsm.media_id_type_name, "ARCHIVE_SIGNAL");
   dsm.media_dog_tag = 0;
   dsm.generation = 1;
   strcpy (dsm.media_id_alias, "");
   strcpy (dsm.recorder_id, "D1");
   strcpy (dsm.recorder_type, "DCRSI");
   strcpy (dsm.recorder_media_type, "AMPEX_HED_733");
   strcpy (dsm.data_direction, "UNKNOWN");
   strcpy (dsm.channelization, "N/A");
   strcpy (dsm.bit_rate, "105000000");
   strcpy (dsm.status, "");
   dsm.start_address = 0;
   dsm.stop_address = 0;
   dsm.tdrs_number_of_ids = 0;         /* for DNL file dsm */

      dnl_tree = FindObject(top, "DOWNLINK_RECORD", NULL);

      for (i = 0; i < WALPShdr->num_records; i++)
      {
         if ((dnl_tree == NULL) || (dnl_tree == top)) break;

         /* Fill in remainder of dsm */
         strcpy (dsm.platform, "");
         extract_param_value(dnl_tree, &dsm.platform, "PLATFORM");
         strcpy (full_platform, "");
         if (strcmp(dsm.platform, "E1") == 0)
             strcpy (full_platform, "ERS-1");
         else if (strcmp(dsm.platform, "E2") == 0)
             strcpy (full_platform, "ERS-2");
         else if (strcmp(dsm.platform, "J1") == 0)
	 {
             strcpy (full_platform, "JERS-1");
             strcpy (dsm.bit_rate, "85000000");
	 }
         else if (strcmp(dsm.platform, "R1") == 0)
             strcpy (full_platform, "RADARSAT-1");
         else if (strcmp(dsm.platform, "A1") == 0)
             strcpy (full_platform, "ADEOS-1");
         extract_param_value(dnl_tree, recorded, "RECORDED");
         if (strcmp(recorded, "YES") == 0)
	 {
            strcpy (dsm.status, "HST_S_OK");
            extract_param_value(dnl_tree, &dsm.start_time, "START_TIME");
            extract_param_value(dnl_tree, &dsm.end_time, "END_TIME");
	 }
         else
	 {
            strcpy (dsm.status, "HST_S_PROBLEM");
            dsm.start_time = no_data_time;
            dsm.end_time = no_data_time;
	 }

         extract_param_value(dnl_tree, &dsm.revolution, "REVOLUTION");

         strcpy (dsm.sensor, "");
         extract_param_value(dnl_tree, &dsm.sensor, "SENSOR");
         if (strlen(dsm.sensor) == 0)
	 {
            syslog(LOG_ERR, "ERROR: Missing SENSOR value in DNL file.\n");
            return (ERROR);
	 }

         dsm.sequence = 0;
         extract_param_value(dnl_tree, &dsm.sequence, "SEQUENCE");
         if (dsm.sequence == 0)
	 {
            syslog(LOG_ERR, "ERROR: Missing SEQUENCE value in DNL file.\n");
            return (ERROR);
	 }

         /* Write out the dsm contents into the dsm file */

         current_time = (time_t)time(null_ptr);
         status = cftime(dsm.time, "%Y-%jT%H:%M:%S", &current_time);

         strcpy (dsmfile, "");
         ticks = clock()/1000;
         sprintf (dname, "ds_%s%05d%c%02d%s_%06d.M", dsm.platform, 
                  dsm.revolution, dsm.sensor[0], dsm.sequence, 
                  wff_filetype, ticks);
         sprintf (dsmfile, "%s/PMF/%s", path, dname);

         dsm_file_ptr = fopen(dsmfile,"w");
         if (dsm_file_ptr == (FILE *)NULL)
         {
            syslog(LOG_ERR, "ERROR, Unable to open downlink file\n") ;
            return(ERROR) ;
         }

         root = (AGGREGATE)create_dsm_PMF(&dsm);
         WriteLabel (dsm_file_ptr, root);
         fclose(dsm_file_ptr);
         RemoveAggregate(root) ;

         /* Add this file to the linked list */
         filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
         *filerecord->file_name = NULL; /* metadata only */
         sprintf(filerecord->PMF_file_name,"%s", dname);
         /*  For Data Sequence Messages, the dataset_suffix handed back to
             router is, in fact, the full dataset name, including platform. */
         sprintf(filerecord->dataset_suffix,"%s RAW SIGNAL SEGMENT", 
                  full_platform);
         filerecord->orig_not_tran = TRUE ;
         PREPEND(bar, filerecord, free, filerecord);

         /* Find the next tape_downlink_record */
         dnl_tree = dnl_tree->right_sibling;

       }   /* end of num_downlinks loop */

   free(dsmfile);
   return (OK);

}  /* extract_dnl_downlinks */

/* End of file */
