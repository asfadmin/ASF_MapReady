/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	WALPS_sr_tpr.c

Description:
	WALLOPS Shipment Report and TDRS Playback Report file parser function.
The specification of the WALLOPS file format is described in the Alaska SAR
Facility (ASF) to Wallops Flight Facility (WFF) Interface Specification
Document (ISD), JPL-D11871.

External Functions:
	locate_WALPS_SR_TPR_record
	extract_msh_tpr_downlinks

Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  July '97 - R. Hoffman
    Reverse linked list output.
    Guard against empty SENSOR or SEQUENCE values.
2.  July '97 - R. Hoffman
    Change dl_ to ds_ (for part of PR 2684)
==============================================================================*/

static char SccsFile[] = "WALPSsr.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)WALPSsr.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include "odldef.h"
#include "odlinter.h"
#include "dapps_list.h"
#include "nmalloc.h"
#include "WALPS.h"
#include "WALPShdr.h"

int llist_errno;   /* to make make happy ?? */

#ifdef __STDC__
int              locate_WALPS_SR_TPR_record(AGGREGATE, char *) ;
int              extract_msh_tpr_downlinks(WALPS_Header_Record, 
                      char *, AGGREGATE, llist *);
#else
int              locate_WALPS_SR_TPR_record() ;
int              extract_msh_tpr_downlinks() ;
#endif

extern void *util_do_malloc() ;
extern int   extract_param_value() ;


/*==============================================================================
Function:	int locate_WALPS_SR_TPR_record(AGGREGATE top, char *obj_name)

Description:	
	This function looks into the ODL tree to find a specified object.

Parameters:
        AGGREGATE  top      - topmost aggregate object for input file
        char      *obj_name - keyword value for desired object record

Returns:	
	ACCEPT - record found
	ERROR  - unable to locate SR_record

Creator:	Norbert Piega	
Creation Date:	10/19/1994
Notes:		
1.  Oct. '96 - RH
    Changed extract_WALPS_SR_record() into locate_WALPS_SR_TPR_record()
==============================================================================*/
#ifdef __STDC__
int locate_WALPS_SR_TPR_record(AGGREGATE top, char *obj_name)
#else
int
locate_WALPS_SR_TPR_record(top, obj_name)
   AGGREGATE top ;
   char *obj_name;
#endif
{
   AGGREGATE object ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   if (top == NULL)
   {
      syslog(LOG_ERR, "ERROR, ODL top node is NULL\n") ;
      return(ERROR) ;
   }

   if ((object = FindObject(top, obj_name, NULL)) == NULL)
   {
      sprintf(logmsg, "ERROR, unable to find ship-rpt record in file.\n"); 
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   return(ACCEPT) ;

} /* locate_WALPS_SR_record */



/*==============================================================================
Function:	int extract_msh_tpr_downlinks(WALPS_Header_Record WALPShdr, 
                      char *path, AGGREGATE top, llist *bar);

Description:	
	This function uses the ODL tree, top, to obtain and store tape
downlink records from a WFF Shipment Report or WFF TDRS Playback Report.  
If successful, ACCEPT is returned, otherwise, ERROR is returned.

Parameters:
        WALPS_Header_Record WALPShdr - Header information
        char *path - of incoming WFF MSH or TPR file
        AGGREGATE top - topmost aggregate object for input file
        llist *bar - linked list of files to send to IMS, which will
           include the downlink files extracted here

Returns:	
	OK     - all downlink record information obtained successfully
	ERROR  - memory allocation errors, unable to complete ship-rpt record

Creator:	Rodney Hoffman	
Creation Date:	October 1996
Notes:		
==============================================================================*/
#ifdef __STDC__
int extract_msh_tpr_downlinks(WALPS_Header_Record WALPShdr, 
                      char *path, AGGREGATE top, llist *bar)
#else
int extract_msh_tpr_downlinks(WALPShdr, path, top, bar)
   WALPS_Header_Record  WALPShdr ;
   char                *path;
   AGGREGATE            top ;
   llist               *bar;
#endif
{
   ASF_Data_Seq_Msg_Record     dsm;
   AGGREGATE                   root, tape_tree, dsm_tree;
   FILE                       *dsm_file_ptr ;
   char                        dsm_path[80] ;
   PMF_FILE_LIST              *filerecord ;
   char                        tape_tree_str[30];
   char                       *dsmfile;
   char                        dname[31];
   char                        wff_filetype[4];
   char                        mm_recorder_id[3];
   int                         i, status, num_downlinks;
   time_t                      current_time;
   time_t                     *null_ptr = NULL;
   clock_t                     ticks;
   char                        full_platform[11];     /* for dataset name */

   dsmfile = (char *)util_do_malloc(sizeof(char) * (strlen(path)+ 35));
   /* Fill in constant portions of dsm */
   strcpy (dsm.destination, "IMS");
   dsm.number_of_records = 1;
   strcpy (dsm.msg_type, "DATA_SEQUENCE_MESSAGE");
   strcpy (dsm.source, "FAIF");
   strcpy (dsm.system_activity_id, "FAIF_0");
   strcpy (dsm.system_activity_type, "DOWNLINK");
   strcpy (dsm.status, "HST_S_OK");
   strcpy (dsm.media_id, "");
   strcpy (dsm.media_id_type_name, "ARCHIVE_SIGNAL");
   dsm.media_dog_tag = 0;
   dsm.generation = 1;
   strcpy (dsm.recorder_type, "DCRSI");
   strcpy (dsm.recorder_media_type, "AMPEX_HED_733");
   strcpy (dsm.data_direction, "UNKNOWN");
   strcpy (dsm.channelization, "N/A");
   strcpy (dsm.bit_rate, "105000000");
   dsm.tdrs_number_of_ids = 0;         /* for shipment record dsm */

   if (WALPShdr.file_type_id == WALPS_MSH)
   {
      sprintf (tape_tree_str, "%s", WALPS_MSH_TAPE_STR);
      sprintf (wff_filetype, "MSH");
   }
   else if (WALPShdr.file_type_id == WALPS_TPR)
   {
      sprintf (tape_tree_str, "%s", WALPS_TPR_TAPE_STR);
      sprintf (wff_filetype, "TPR");
   }
   else 
   {
      syslog(LOG_ERR, "ERROR, Wrong file type.\n") ;
      return (ERROR);
   }

   /* Start with the first tape record */
   tape_tree = FindObject(top, tape_tree_str, NULL);

   while (TRUE)
   {
      if ((tape_tree == NULL) || (tape_tree == top)) break;

      /* Fill in metadata from the tape record */
      strcpy (dsm.media_id_alias, "");
      extract_param_value (tape_tree, &dsm.media_id_alias, "MGS_MEDIA_ID");
      strcpy (mm_recorder_id, "");
      if (WALPShdr.file_type_id == WALPS_MSH)
         extract_param_value (tape_tree, mm_recorder_id, "RECORDER_ID");
      if (strlen(mm_recorder_id) > 0)
         sprintf (dsm.recorder_id, "MM-%s", mm_recorder_id);
      else 
         strcpy (dsm.recorder_id, "MM");
      extract_param_value (tape_tree, &num_downlinks, 
                           "NUMBER_OF_DOWNLINK_RECORDS");

      /* Get the first tape_downlink_record within this MGS_tape_record */
      dsm_tree = FindObject(tape_tree, "TAPE_DOWNLINK_RECORD", NULL);

      for (i = 0; i < num_downlinks; i++)
      {
         if ((dsm_tree == NULL) || (dsm_tree == tape_tree)) break;

         /* Fill in remainder of dsm */
         strcpy (dsm.platform, "");
         extract_param_value(dsm_tree, &dsm.platform, "PLATFORM");
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
         extract_param_value(dsm_tree, &dsm.revolution, "REVOLUTION");
         extract_param_value(dsm_tree, &dsm.start_address, "MGS_START_ADDRESS");
         extract_param_value(dsm_tree, &dsm.stop_address, "MGS_STOP_ADDRESS");

         strcpy (dsm.sensor, "");
         extract_param_value(dsm_tree, &dsm.sensor, "SENSOR");
         if (strlen(dsm.sensor) == 0)
	 {
            syslog(LOG_ERR, "ERROR: Missing SENSOR value in MSH file.\n");
            return (ERROR);
	 }

         dsm.sequence = 0;
         extract_param_value(dsm_tree, &dsm.sequence, "SEQUENCE");
         if (dsm.sequence == 0)
	 {
            syslog(LOG_ERR, "ERROR: Missing SEQUENCE value in MSH file.\n");
            return (ERROR);
	 }

         if (WALPShdr.file_type_id == WALPS_TPR)
            dsm.tdrs_number_of_ids = 
               (dsm.stop_address - dsm.start_address) / TDRS_ID_BLOCKS ;
         if (WALPShdr.file_type_id == WALPS_MSH)
	 {
           extract_param_value(dsm_tree, &dsm.start_time, "MGS_START_TIME");
           extract_param_value(dsm_tree, &dsm.end_time, "MGS_END_TIME");
         }
         if (WALPShdr.file_type_id == WALPS_TPR)
	 {
           dsm.start_time.year = 0;
           dsm.end_time.year = 0;
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
         dsm_tree = dsm_tree->right_sibling;

       }   /* end of num_downlinks loop */

   /* Find the next tape record, if any */
   tape_tree = tape_tree->right_sibling;
   
   }   /* end of loop through Tape Records */

   free(dsmfile);
   return (OK);

}  /* extract_msh_tpr_downlinks */

/* End of file */
