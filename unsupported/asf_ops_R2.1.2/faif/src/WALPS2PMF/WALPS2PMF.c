/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	WALPS2PMF.c

Description:    Read WFF files and create PMF for them. 
                Extract downlink messages from MSH and TPR files.
                Translate files to ASF format.

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
1.  Oct. '96 - RH
    Updated for ISD Version 2.3
    Added make_WALPS_TPR_PMF()
==============================================================================*/

static char SccsFile[] = "WALPS2PMF.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "24 Feb 1996";
static char SccsLastChanger[] = "@(#)WALPS2PMF.c	1.2";
static char SccsState[] = "1.2";


#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h> 	    /* for getopt		    	    */
#include <unistd.h>
#include <sys/types.h>
#include <syslog.h>
#include <sys/stat.h>
#include <time.h>

#include "WALPS.h"
#include "WALPShdr.h"
#include "WALPSar.h"
#include "WALPSdt.h"
#include "odldef.h"
#include "odlinter.h"

#include "faifdefs.h"
#include "PMF.h"
#include "llist_err.h"
#include "dapps_list.h"
#include "nmalloc.h"
#include "syslog.h"

int     llist_errno ;

ODL_Catalog_Metadata PMF_struct;

llist *gen_WALPS_PMF(char *, char *, int) ;
llist *make_WALPS_DNL_PMF        (char *, char *, int) ;
llist *make_WALPS_RES_PMF(char *, char *, int) ;
int check_WALPS_translation(char *, char *, char *, char **, int) ;
int xlate(char *, char *, char *);



/*==============================================================================
Function:	make_WALPS_RES_PMF
Description:	
Parameters:
Returns:	
Creator:	Rich Norman
Creation Date:	Tue Oct 17 08:44:08 PDT 1995
Notes:		
==============================================================================*/
llist *make_WALPS_RES_PMF(char *path, char *walps_filename, int file_type)
{     
   AGGREGATE     root ;
   AGGREGATE     top ;
   FILE          *PMF_file_ptr ;
   char          PMF_path[80] ;
   int	         i ;	
   int           status ;        /* return code for fa_ascii_rec_processor() */
   FILE          *infp;
   llist         *bar ;
   PMF_FILE_LIST *filerecord ;
   llist         *err_ptr= NULL;
   char          *infile;
   char          *pmffile;

   WALPS_Header_Record *WALPS_hdr = NULL ;
   WALPS_AR_Record *WALPS_AR_rec = NULL ;

   char logmsg[MAX_SYSLOG_MSGLEN + 1];
   char tmp_start_str[255];
   char tmp_end_str[255];
   char tmp_arr_str[22];

   struct stat fstat_buf;

   infile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+
					 strlen(walps_filename)+ 1));
   strcpy(infile,path);
   strcat(infile,"/");
   strcat(infile, walps_filename);

   strcpy(PMF_struct.FA_file_type , WALPS_RES_STR);
   strcpy(PMF_struct.gen_file_type, GFTYPE_RGSRESP      );

   /* Allocate root aggregate node for Wallops label file */
   top = NewAggregate(NULL, KA_OBJECT, "root", NULL);

   if ((infp = fopen(infile, "r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR,
       "ERROR, Unable to open input Wallops file\n") ;
      return(err_ptr) ;
   }

   /* Read Wallops label. Obtain associated tree */
   if (ReadLabel(infp, top) == 0)
   {
      syslog(LOG_DEBUG, 
       "ODL routine ReadLabel returned error. Aborting conversion\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   /* Allocate Wallops header record -- Extract header data from input file */
   if ((WALPS_hdr = (WALPS_Header_Record *)alloc_WALPS_header_record()) 
    == NULL)
   {
      syslog(LOG_DEBUG, "Unable to allocate Wallops header record\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }
   strcpy(WALPS_hdr->file_name, infile) ;
   if ((status = extract_WALPS_header(WALPS_hdr, top)) == ERROR)
   {
      syslog(LOG_ERR, 
       "ERROR, Wallops header extraction error.  Aborting conversion\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   /* Ready to extract avail_resp record/ODL object
    -- Store extracted info in WALPS_AVAIL_RESP_rec */
   if ((WALPS_AR_rec = (WALPS_AR_Record *)alloc_WALPS_AR_record()) ==
       (WALPS_AR_Record *)NULL)
   {
      syslog(LOG_DEBUG, "ERROR, Unable to allocate WALPS AVAIL RESP record\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr) ;
   }
   strcpy(WALPS_AR_rec->file_name, infile) ;
   if ((status = extract_WALPS_AR_record
                 (infp, WALPS_AR_rec, top, WALPS_hdr->num_records)) == ERROR)
   {
      syslog(LOG_ERR, 
       "Wallops avail_resp record extraction error.  Aborting conversion\n") ;
      return(err_ptr);
   }

   /* Fill in the structure for the PMF */
   strcpy (PMF_struct.file_name,walps_filename);
   status = stat(infile, &fstat_buf);
   status = cftime(tmp_arr_str, "%Y-%jT%H:%M:%S.000", &fstat_buf.st_mtime);
   strcpy (PMF_struct.file_arrival_time, tmp_arr_str);
   strcpy (PMF_struct.format, ORIG_STR);
   strcpy (PMF_struct.number_of_records, NUMRECS_IN_PMF);
   strcpy (PMF_struct.file_source, WALPS_SRCDEST_WALPS);
   strcpy (PMF_struct.file_dest,WALPS_hdr->file_dest);
   strcpy (PMF_struct.gen_file_type, GFTYPE_RGSRESP);
   strcpy (PMF_struct.file_creation_time, WALPS_hdr->file_creation_time);

   strcpy (PMF_struct.satellite, "") ;
   strcpy (PMF_struct.start_rev, "");
   strcpy (PMF_struct.end_rev, "");
   sprintf(tmp_start_str,"%04d-%03dT%02d:%02d:%02d.%03d",
    WALPS_AR_rec->time_on.year,
    WALPS_AR_rec->time_on.doy,
    WALPS_AR_rec->time_on.hours,
    WALPS_AR_rec->time_on.minutes,
    WALPS_AR_rec->time_on.seconds,
    WALPS_AR_rec->time_on.nanoseconds/1000000);
   strcpy (PMF_struct.valid_start_time, tmp_start_str);
   sprintf(tmp_end_str,"%04d-%03dT%02d:%02d:%02d.%03d",
    WALPS_AR_rec->time_off.year,
    WALPS_AR_rec->time_off.doy,
    WALPS_AR_rec->time_off.hours,
    WALPS_AR_rec->time_off.minutes,
    WALPS_AR_rec->time_off.seconds,
    WALPS_AR_rec->time_off.nanoseconds/1000000);
   strcpy (PMF_struct.valid_end_time, tmp_end_str);

   /* Create the PMF */
   root = (AGGREGATE)create_PMF(&PMF_struct);

   /* Make a linked list that has filename and PMF filename */
   bar = create_dyn_llist();
   filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
   strcpy (filerecord->file_name, walps_filename);
   sprintf(filerecord->PMF_file_name,"%s.%s",walps_filename, PMF_FILE_EXT);
   sprintf(filerecord->dataset_suffix,"%s",WALPS_RES_SUFFIX);
   filerecord->orig_not_tran = TRUE ;
   APPEND(bar, filerecord, free, filerecord);

   /* Write the PMF out to the PMF file and close both files */

   pmffile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+ 5+
					 strlen(walps_filename)+2));
   strcpy(pmffile,path);
   strcat(pmffile,"/PMF/");
   strcat(pmffile, walps_filename);
   strcat(pmffile,".M");

   PMF_file_ptr = fopen(pmffile,"w");
   if (PMF_file_ptr == (FILE *)NULL)
   {
      syslog(LOG_ERR,
       "ERROR, Unable to open output RES PMF file\n") ;
      return(err_ptr) ;
   }

   WriteLabel (PMF_file_ptr, root);
   fclose(infp);
   fclose(PMF_file_ptr);
   RemoveAggregate(root) ;
   free(WALPS_hdr) ;

   /* Return the linked list */
   return(bar);

} /* make_WALPS_RES_PMF */




/*==============================================================================
Function:	make_WALPS_DNL_PMF
Description:	
Parameters:
Returns:	
Creator:	Rich Norman
Creation Date:	Tue Oct 17 08:44:39 PDT 1995
Notes:		
==============================================================================*/
llist *make_WALPS_DNL_PMF(char *path, char *walps_filename, int file_type)
{     
   AGGREGATE     root ;
   AGGREGATE     top ;
   FILE          *PMF_file_ptr ;
   char          PMF_path[80] ;
   int	         i ;	
   int           status ;        /* return code for fa_ascii_rec_processor() */
   FILE          *infp;
   llist         *bar ;
   PMF_FILE_LIST *filerecord ;
   llist         *err_ptr = NULL;
   char          *infile;
   char          *pmffile;
   WALPS_Header_Record *WALPS_hdr = NULL ;
   WALPS_DT_Record *WALPS_DNL_rec = NULL ;

   char logmsg[MAX_SYSLOG_MSGLEN + 1];
   char tmp_start_str[255];
   char tmp_end_str[255];
   char tmp_arr_str[22];

   struct stat fstat_buf;
   time_t      current_time;
   time_t      *null_ptr = NULL;

   infile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+
					 strlen(walps_filename)+ 1));
   strcpy(infile,path);
   strcat(infile,"/");
   strcat(infile, walps_filename);

   strcpy(PMF_struct.FA_file_type , WALPS_DNL_STR  );
   strcpy(PMF_struct.gen_file_type, GFTYPE_DATAMSG);

   /* Allocate root aggregate node for Wallops label file */
   top = NewAggregate(NULL, KA_OBJECT, "root", NULL);

   if ((infp = fopen(infile, "r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR,
       "ERROR, Unable to open input Wallops file\n") ;
      return(err_ptr) ;
   }

   /* Read Wallops label. Obtain associated tree */
   if (ReadLabel(infp, top) == 0)
   {
      syslog(LOG_DEBUG, 
       "ODL routine ReadLabel returned error. Aborting conversion\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   /* Allocate Wallops header record -- Extract header data from input file */
   if ((WALPS_hdr = (WALPS_Header_Record *)alloc_WALPS_header_record()) 
    == NULL)
   {
      syslog(LOG_DEBUG, "Unable to allocate Wallops header record\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }
   strcpy(WALPS_hdr->file_name, infile) ;
   if ((status = extract_WALPS_header(WALPS_hdr, top)) == ERROR)
   {
      syslog(LOG_ERR, 
       "ERROR, Wallops header extraction error.  Aborting conversion\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   /* Ready to extract datatake record/ODL object
    -- Store extracted info in WALPS_DNL_rec */
   if ((WALPS_DNL_rec = (WALPS_DT_Record *)alloc_WALPS_DNL_record()) == (WALPS_DT_Record *)NULL)
   {
      syslog(LOG_DEBUG, "ERROR, Unable to allocate WALPS DT record\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr) ;
   }
   strcpy(WALPS_DNL_rec->file_name, infile) ;
   if ((status = extract_WALPS_DNL_record(infp, WALPS_DNL_rec, top)) == ERROR)
   {
      syslog(LOG_ERR, 
       "Wallops datatake record extraction error.  Aborting conversion\n") ;
      return(err_ptr);
   }

   /* Fill in the structure for the PMF */
   strcpy (PMF_struct.file_name,walps_filename);
   strcpy (PMF_struct.start_rev, "");
   strcpy (PMF_struct.end_rev, "");
   PMF_struct.valid_start_time[0] = '\0';
   PMF_struct.valid_end_time[0] = '\0';
   status = stat(infile, &fstat_buf);
   status = cftime(tmp_arr_str, "%Y-%jT%H:%M:%S.000", &fstat_buf.st_mtime);
   strcpy (PMF_struct.file_arrival_time, tmp_arr_str);
   strcpy (PMF_struct.format, ORIG_STR);
   strcpy (PMF_struct.number_of_records, NUMRECS_IN_PMF);
   strcpy (PMF_struct.file_source, WALPS_SRCDEST_WALPS);
   strcpy (PMF_struct.file_dest,WALPS_hdr->file_dest);
   strcpy (PMF_struct.gen_file_type, GFTYPE_DATAMSG);
   strcpy (PMF_struct.file_creation_time, WALPS_hdr->file_creation_time);
   strcpy (PMF_struct.satellite, WALPS_DNL_rec->satellite) ;

   /* Create the PMF */
   root = (AGGREGATE)create_PMF(&PMF_struct);

   /* Make a linked list that has filename and PMF filename */
   bar = create_dyn_llist();
   filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
   strcpy (filerecord->file_name, walps_filename);
   sprintf(filerecord->PMF_file_name,"%s.%s",walps_filename, PMF_FILE_EXT);
   sprintf(filerecord->dataset_suffix,"%s",WALPS_DT_SUFFIX);
   filerecord->orig_not_tran = TRUE ;
   APPEND(bar, filerecord, free, filerecord);

   /* Write the PMF out to the PMF file and close both files */

   pmffile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+ 5+
					 strlen(walps_filename)+2));
   strcpy(pmffile,path);
   strcat(pmffile,"/PMF/");
   strcat(pmffile, walps_filename);
   strcat(pmffile,".M");

   PMF_file_ptr = fopen(pmffile,"w");
   if (PMF_file_ptr == (FILE *)NULL)
   {
      syslog(LOG_ERR,
       "ERROR, Unable to open output datatake PMF file\n") ;
      return(err_ptr) ;
   }

   WriteLabel (PMF_file_ptr, root);
   fclose(infp);
   fclose(PMF_file_ptr);
   RemoveAggregate(root) ;

   /* Extract each downlink record and add (as PMF files) to the linked list. */
   if (extract_dnl_downlinks(WALPS_hdr, path, top, bar) == ERROR)
   {
      syslog (LOG_ERR, "ERROR extracting DNL downlink records.");
      free(WALPS_hdr) ;
      DEL_ALL(bar);
      return(err_ptr);
   }

   free(WALPS_hdr) ;

   /* Return the linked list */
   return(bar);

} /* make_WALPS_DNL_PMF */




/*==============================================================================
Function:	make_WALPS_MSH_PMF
Description:	
Parameters:
Returns:	
Creator:	Rich Norman
Creation Date:	Tue Oct 17 08:45:33 PDT 1995
Notes:		
==============================================================================*/
llist *make_WALPS_MSH_PMF(char *path, char *walps_filename, int file_type)
{     
   AGGREGATE     root ;
   AGGREGATE     top ;
   FILE          *PMF_file_ptr ;
   char          PMF_path[80] ;
   int	         i ;	
   int           status ;        /* return code for fa_ascii_rec_processor() */
   FILE          *infp;
   llist         *bar ;
   PMF_FILE_LIST *filerecord ;
   llist         *err_ptr= NULL;
   char          *infile;
   char          *pmffile;

   WALPS_Header_Record *WALPS_hdr = NULL ;

   char logmsg[MAX_SYSLOG_MSGLEN + 1];
   char tmp_arr_str[22];

   struct stat fstat_buf;

   infile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+
					 strlen(walps_filename)+ 1));
   strcpy(infile,path);
   strcat(infile,"/");
   strcat(infile, walps_filename);

   strcpy(PMF_struct.FA_file_type , WALPS_MSH_STR);
   strcpy(PMF_struct.gen_file_type, GFTYPE_SHIP_REP   );

   /* Allocate root aggregate node for Wallops label file */
   top = NewAggregate(NULL, KA_OBJECT, "root", NULL);

   if ((infp = fopen(infile, "r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to open input Wallops file\n") ;
      return(err_ptr) ;
   }

   /* Read Wallops label. Obtain associated tree */
   if (ReadLabel(infp, top) == 0)
   {
      syslog(LOG_ERR, 
       "ERROR, ODL routine ReadLabel returned error. Aborting.\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   /* Allocate Wallops header record -- Extract header data from input file */
   if ((WALPS_hdr = (WALPS_Header_Record *)alloc_WALPS_header_record()) 
    == NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to allocate Wallops header record\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }
   strcpy(WALPS_hdr->file_name, infile) ;
   if ((status = extract_WALPS_header(WALPS_hdr, top)) == ERROR)
   {
      syslog(LOG_ERR, 
       "ERROR, Wallops header extraction error.  Aborting.\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   if ((status = locate_WALPS_SR_TPR_record(top, WALPS_SRRECORD_NAME)) == ERROR)
   {
      syslog(LOG_ERR, 
       "ERROR, Unable to find Shipment Report object.  Aborting\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr);
   }

   /* Fill in the structure for the PMF */
   strcpy (PMF_struct.file_name,walps_filename);
   status = stat(infile, &fstat_buf);
   status = cftime(tmp_arr_str, "%Y-%jT%H:%M:%S.000", &fstat_buf.st_mtime);
   strcpy (PMF_struct.file_arrival_time, tmp_arr_str);
   strcpy (PMF_struct.format, ORIG_STR);
   strcpy (PMF_struct.number_of_records, NUMRECS_IN_PMF);
   strcpy (PMF_struct.file_source, WALPS_SRCDEST_WALPS);
   strcpy (PMF_struct.file_dest,WALPS_hdr->file_dest);
   strcpy (PMF_struct.gen_file_type, GFTYPE_SHIP_REP);
   strcpy (PMF_struct.file_creation_time, WALPS_hdr->file_creation_time);

   /* Create the PMF */
   root = (AGGREGATE)create_PMF(&PMF_struct);

   /* Make a linked list that has filename and PMF filename */
   bar = create_dyn_llist();
   filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
   strcpy (filerecord->file_name, walps_filename);
   sprintf(filerecord->PMF_file_name,"%s.%s",walps_filename, PMF_FILE_EXT);
   sprintf(filerecord->dataset_suffix,"%s",WALPS_SR_SUFFIX);
   filerecord->orig_not_tran = TRUE ;
   APPEND(bar, filerecord, free, filerecord);

   /* Write the PMF out to the PMF file and close both files */

   pmffile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+ 5+
					 strlen(walps_filename)+2));
   strcpy(pmffile,path);
   strcat(pmffile,"/PMF/");
   strcat(pmffile, walps_filename);
   strcat(pmffile,".M");

   PMF_file_ptr = fopen(pmffile,"w");
   if (PMF_file_ptr == (FILE *)NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to open MSH PMF file\n") ;
      free(WALPS_hdr) ;
      fclose(infp);
      RemoveAggregate(root) ;
      DEL_ALL(bar);
      return(err_ptr) ;
   }

   WriteLabel (PMF_file_ptr, root);
   fclose(PMF_file_ptr);

   /* Extract each downlink record and add (as PMF files) to the linked list. */
   if (extract_msh_tpr_downlinks(WALPS_hdr, path, top, bar) == ERROR)
   {
      syslog (LOG_ERR, "ERROR extracting MSH downlink records.");
      free(WALPS_hdr) ;
      fclose(infp);
      RemoveAggregate(root) ;
      DEL_ALL(bar);
      return(err_ptr);
   }

   free(WALPS_hdr) ;
   fclose(infp);
   RemoveAggregate(root) ;

   /* Return the linked list */
   return(bar);

} /* make_WALPS_MSH_PMF */



/*==============================================================================
Function:	make_WALPS_TPR_PMF
Description:	
Parameters:
Returns:	
Creator:	Rodney Hoffman
Creation Date:	October 1996
Notes:		Nearly identical to make_WALPS_MSH_PMF()
==============================================================================*/
llist *make_WALPS_TPR_PMF(char *path, char *walps_filename, int file_type)
{     
   AGGREGATE     root ;
   AGGREGATE     top ;
   FILE          *PMF_file_ptr ;
   char          PMF_path[80] ;
   int	         i ;	
   int           status ;        /* return code for fa_ascii_rec_processor() */
   FILE          *infp;
   llist         *bar ;
   PMF_FILE_LIST *filerecord ;
   llist         *err_ptr= NULL;
   char          *infile;
   char          *pmffile;

   WALPS_Header_Record *WALPS_hdr = NULL ;

   char logmsg[MAX_SYSLOG_MSGLEN + 1];
   char tmp_arr_str[22];

   struct stat fstat_buf;

   infile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+
					 strlen(walps_filename)+ 1));
   strcpy(infile,path);
   strcat(infile,"/");
   strcat(infile, walps_filename);

   strcpy(PMF_struct.FA_file_type , WALPS_TPR_STR);
   strcpy(PMF_struct.gen_file_type, GFTYPE_SHIP_REP   );

   /* Allocate root aggregate node for Wallops label file */
   top = NewAggregate(NULL, KA_OBJECT, "root", NULL);

   if ((infp = fopen(infile, "r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to open input Wallops file\n") ;
      return(err_ptr) ;
   }

   /* Read Wallops label. Obtain associated tree */
   if (ReadLabel(infp, top) == 0)
   {
      syslog(LOG_ERR, 
       "ERROR, ODL routine ReadLabel returned error. Aborting.\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   /* Allocate Wallops header record -- Extract header data from input file */
   if ((WALPS_hdr = (WALPS_Header_Record *)alloc_WALPS_header_record()) 
    == NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to allocate Wallops header record\n") ;
      fclose(infp) ;
      return(err_ptr) ;
   }
   strcpy(WALPS_hdr->file_name, infile) ;
   if ((status = extract_WALPS_header(WALPS_hdr, top)) == ERROR)
   {
      syslog(LOG_ERR, 
       "ERROR, Wallops header extraction error.  Aborting.\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr) ;
   }

   if ((status = locate_WALPS_SR_TPR_record(top, WALPS_TPRRECORD_NAME)) 
      == ERROR)
   {
      syslog(LOG_ERR, "ERROR, Unable to find WALPS TPR object.  Aborting\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(err_ptr);
   }

   /* Fill in the structure for the PMF */
   strcpy (PMF_struct.file_name,walps_filename);
   status = stat(infile, &fstat_buf);
   status = cftime(tmp_arr_str, "%Y-%jT%H:%M:%S.000", &fstat_buf.st_mtime);
   strcpy (PMF_struct.file_arrival_time, tmp_arr_str);
   strcpy (PMF_struct.format, ORIG_STR);
   strcpy (PMF_struct.number_of_records, NUMRECS_IN_PMF);
   strcpy (PMF_struct.file_source, WALPS_SRCDEST_WALPS);
   strcpy (PMF_struct.file_dest,WALPS_hdr->file_dest);
   strcpy (PMF_struct.gen_file_type, GFTYPE_SHIP_REP);
   strcpy (PMF_struct.file_creation_time, WALPS_hdr->file_creation_time);

   /* Create the PMF */
   root = (AGGREGATE)create_PMF(&PMF_struct);

   /* Make a linked list that has filename and PMF filename */
   bar = create_dyn_llist();
   filerecord = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
   strcpy (filerecord->file_name, walps_filename);
   sprintf(filerecord->PMF_file_name,"%s.%s",walps_filename, PMF_FILE_EXT);
   sprintf(filerecord->dataset_suffix,"%s",WALPS_SR_SUFFIX);
   filerecord->orig_not_tran = TRUE ;
   APPEND(bar, filerecord, free, filerecord);

   /* Write the PMF out to the PMF file and close both files */

   pmffile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+ 5+
					 strlen(walps_filename)+2));
   strcpy(pmffile,path);
   strcat(pmffile,"/PMF/");
   strcat(pmffile, walps_filename);
   strcat(pmffile,".M");

   PMF_file_ptr = fopen(pmffile,"w");
   if (PMF_file_ptr == (FILE *)NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to open TPR PMF file\n") ;
      free(WALPS_hdr) ;
      fclose(infp);
      RemoveAggregate(root) ;
      DEL_ALL(bar);
      return(err_ptr) ;
   }

   WriteLabel (PMF_file_ptr, root);
   fclose(PMF_file_ptr);

   /* Extract each downlink record and add (as PMF files) to the linked list. */
   if (extract_msh_tpr_downlinks(WALPS_hdr, path, top, bar) == ERROR)
   {
      syslog (LOG_ERR, "ERROR extracting TPR downlink records.");
      free(WALPS_hdr) ;
      fclose(infp);
      RemoveAggregate(root) ;
      DEL_ALL(bar);
      return(err_ptr);
   }

   free(WALPS_hdr) ;
   fclose(infp);
   RemoveAggregate(root) ;

   /* Return the linked list */
   return(bar);

} /* make_WALPS_TPR_PMF */



/*==============================================================================
Function:	gen_WALPS_PMF
Description:	
Parameters:
Returns:	
Creator:	Rich Norman
Creation Date:	Tue Oct 17 08:45:50 PDT 1995
Notes:		
==============================================================================*/
llist *gen_WALPS_PMF(char *path, char *walps_filename, int file_type)
{     
   llist         *bar ;
   llist         *err_ptr= NULL;

   char logmsg[MAX_SYSLOG_MSGLEN + 1];

   switch(file_type)
   {
      case WALPS_DNL:
         bar = (llist *)make_WALPS_DNL_PMF(path, walps_filename, file_type);
         break ;
      case WALPS_RES:
         bar = (llist *)make_WALPS_RES_PMF(path, walps_filename, file_type);
         break ;
      case WALPS_MSH:
         bar = (llist *)make_WALPS_MSH_PMF(path, walps_filename, file_type);
         break ;
      case WALPS_TPR:
      /*
         bar = (llist *)make_WALPS_TPR_PMF(path, walps_filename, file_type); 
         break ;    
      */
         syslog (LOG_ERR, 
                 "ERROR, Unable to process WFF TPR files at this time.\n");
         return (err_ptr);
      default :
         syslog(LOG_ERR,
          "ERROR, Unrecognized WALLOPS file type=%d\n",file_type) ;
         return(err_ptr) ;
   }

   return (bar);

} /* gen_WALPS_PMF */
