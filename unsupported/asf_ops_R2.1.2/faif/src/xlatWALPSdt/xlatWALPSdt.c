/*==============================================================================
Filename:	xlatWALPSdt.c

Description:
	This module contains the routines used to for translating ODL format 
Wallops Datatake Message files to ASF format.  Wallops defined identifiers
are in WALPS.h and WALPSdt.h while their ASF defined counterparts are in ASF.h.

External Functions:
	translate_WALPS_dt
	
Static Functions:
	write_ASF_DTrec_from_WALPS
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  Oct. '96 - RH
    Remove infp from parameter list in call to extract_WALPS_header()
    Change WALPS_DT to WALPS_DNL
==============================================================================*/

static char SccsFile[] = "xlatWALPSdt.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)xlatWALPSdt.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <syslog.h>
#include "odldef.h"
#include "odlinter.h"
#include "WALPShdr.h"
#include "WALPSdt.h"
#include "ASF.h"
#include "ASFhdr.h"
#include "xlatASFtoWALPS.h"


#ifdef __STDC__
int        translate_WALPS_dt(char *, char *, char **) ;
static int write_ASF_DTrec_from_WALPS(FILE *, WALPS_DT_Record *) ;
#else
int        translate_WALPS_dt() ;
static int write_ASF_DTrec_from_WALPS() ;
#endif
 

extern WALPS_Header_Record *alloc_WALPS_header_record() ;
extern ASF_Header_Record   *alloc_ASF_hdr_record() ;
extern int                  write_ASF_hdr_record() ;
extern int                  write_ASF_svmeta_record() ;
extern WALPS_DT_Record     *alloc_WALPS_DT_record() ;
extern int                  extract_WALPS_DT_record() ;

extern Map_Table_Entry Map_Satellite_Table[] ;
extern Map_Table_Entry Map_SrcDest_Table[] ;
extern Map_Table_Entry Map_Sensor_Table[] ;
extern Map_Table_Entry Map_Activity_Table[] ;
extern Map_Table_Entry Map_Agency_Table[] ;



/*==============================================================================
Function:	int translate_WALPS_dt(char *infile, char *outfile,
				       char **ASFfilename)

Description:	
	This function translates a Wallops datatake file in ODL (Object
Description Language) format into the ASF format (byte stream).  In addition
to the input and output file parameters, a third parameter is supplied for
passing back an ASF standard filename for datatake files.  The filename is
generated from data obtained from the input file.  The generated ASF 
filename is provided to the caller to allow renaming of the output file as
desired.

Parameters:
	char *infile - input Wallops datatake filename
	char *outfile - output ASF filename
	char **ASFfilename - generated ASF DT file filename passed back

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	10/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
translate_WALPS_dt(char *infile, char *outfile, char **ASFfilename)
#else
int
translate_WALPS_dt(infile, outfile, ASFfilename)
   char *infile ;
   char *outfile ;
   char **ASFfilename ;
#endif
{
   FILE *infp, *outfp ;
   AGGREGATE top ;
   int filetype ;
   int status = OK ;
   int ii ;
   WALPS_Header_Record *WALPS_hdr = NULL ;
   WALPS_DT_Record *WALPS_DT_rec = NULL ;
   ASF_Header_Record *ASF_hdr = NULL ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   char tempstr[MAXLINE] ;

   if ((filetype = parse_WALPS_hdr(infile)) != WALPS_DNL)
   {
      syslog(LOG_ERR, 
	 "WARNING, Input WALLOPS file is not a Datatake Message. \
	 \nAborting conversion\n") ;
      return(ERROR) ;
   }


   /* Allocate root aggregate node for Wallops label file
   */
   top = NewAggregate(NULL, KA_OBJECT, "root", NULL);

   if ((infp = fopen(infile, "r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR,
	 "WARNING, Unable to open input Wallops datatake file\n") ;
      return(ERROR) ;
   }


   /* Read Wallops label. Obtain associated tree
   */
   if (ReadLabel(infp, top) == 0)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, ODL routine ReadLabel returned error.  Aborting conversion\n") ;
      fclose(infp) ;
      return(ERROR) ;
   }


   /* Allocate Wallops header record
   -- Extract header data from input file
   */
   if ((WALPS_hdr = (WALPS_Header_Record *)alloc_WALPS_header_record()) 
	== NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate Wallops header record\n") ;
      fclose(infp) ;
      return(ERROR) ;
   }
   strcpy(WALPS_hdr->file_name, infile) ;
   if ((status = extract_WALPS_header(WALPS_hdr, top)) == ERROR)
   {
      syslog(LOG_ERR, 
	 "WARNING, Wallops header extraction error.  Aborting conversion\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(ERROR) ;
   }


   /* Allocate ASF header record;  Assign header data
   -- as obtained from input file; Write header data 
   -- to output file
   */
   if ((ASF_hdr = alloc_ASF_hdr_record()) == (ASF_Header_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate ASF header record\n") ;
      free(WALPS_hdr) ;
      fclose(infp) ;
      return(ERROR) ;
   }
   sprintf(ASF_hdr->year, "%4d", WALPS_hdr->date_time.year) ; 
   sprintf(ASF_hdr->time, "%03d:%02d:%02d:%02d.%03d", 
	      WALPS_hdr->date_time.doy,
	      WALPS_hdr->date_time.hours,
	      WALPS_hdr->date_time.minutes,
	      WALPS_hdr->date_time.seconds,
	      WALPS_hdr->date_time.nanoseconds/1000000) ; 
   if (WALPS_hdr->file_type_id == WALPS_DNL)
      strcpy(ASF_hdr->msg_type, ASF_MSGTYPE_DT) ;
   else
   {
      syslog(LOG_ERR,
	 "WARNING, Input file is not a datatake file. Cannot convert\n") ;
      free(WALPS_hdr) ;
      free(ASF_hdr) ;
      fclose(infp) ;
      return(ERROR) ;
   }


   /* Destination 
   */
   for (ii=0; Map_SrcDest_Table[ii].fa != (char *)NULL; ii++)
      if (strcmp(WALPS_hdr->file_dest, Map_SrcDest_Table[ii].fa) == 0)
      {
         strcpy(ASF_hdr->dest, Map_SrcDest_Table[ii].asf) ;
	 break ;
      }
   if (Map_SrcDest_Table[ii].fa == (char *)NULL)
   {
      syslog(LOG_ERR,
	 "WARNING, DESTINATION in Wallops file is invalid. Aborting conversion\n") ;
      free(WALPS_hdr) ;
      free(ASF_hdr) ;
      fclose(infp) ;
      return(ERROR) ;
   }


   /* Source
   */
   for (ii=0; Map_SrcDest_Table[ii].fa != (char *)NULL; ii++)
      if (strcmp(WALPS_hdr->file_source, Map_SrcDest_Table[ii].fa) == 0)
      {
         strcpy(ASF_hdr->origin, Map_SrcDest_Table[ii].asf) ;
	 break ;
      }
   if (Map_SrcDest_Table[ii].fa == (char *)NULL)
   {
      syslog(LOG_ERR,
	 "WARNING, SOURCE in Wallops file is invalid. Aborting conversion\n") ;
      free(WALPS_hdr) ;
      free(ASF_hdr) ;
      fclose(infp) ;
      return(ERROR) ;
   }


   if ((outfp = fopen(outfile, "w")) == (FILE *)NULL)
   {
      syslog(LOG_ERR, "WARNING, Unable to open output ASF file\n") ;
      free(WALPS_hdr) ;
      free(ASF_hdr) ;
      fclose(infp) ;
      return(ERROR) ;
   }


   /* Write ASF Header record to ASF output file
   */
   write_ASF_hdr_record(outfp, ASF_hdr) ;


   /* Ready to extract datatake record/ODL object
   -- Store extracted info in WALPS_DT_rec
   */
   if ((WALPS_DT_rec = (WALPS_DT_Record *)alloc_WALPS_DT_record()) ==
       (WALPS_DT_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate WALPS DT record\n") ;
      free(WALPS_hdr) ;
      free(ASF_hdr) ;
      fclose(infp) ;
      fclose(outfp) ;
      return(ERROR) ;
   }
   strcpy(WALPS_DT_rec->file_name, infile) ;
   if ((status = extract_WALPS_DT_record(infp, WALPS_DT_rec, top)) == ERROR)
      syslog(LOG_ERR, 
	 "WARNING, Wallops datatake record extraction error.  Aborting conversion\n") ;

   if ((status != REJECT) && (status != ERROR))
   {
      /* Generate ASF standard filename for the datatake 
      -- where the output of this translation should be
      -- stored.  The filename is generated from data
      -- from datatake record/ODL object values
      */
      *ASFfilename = (char *)util_do_malloc(sizeof(char)*
         (ASF_DT_FNAME_LEN + ASF_DT_FEXT_LEN + 1)) ; 

      for (ii=0; Map_Satellite_Table[ii].fa != (char *)NULL; ii++)
         if (strcmp(WALPS_DT_rec->satellite,
		    Map_Satellite_Table[ii].fa) == 0)
         {
            strcpy(*ASFfilename, Map_Satellite_Table[ii].asf) ;
	    break ;
         }
      if (Map_Satellite_Table[ii].fa == (char *)NULL)
      {
         syslog(LOG_ERR, 
	    "WARNING, Invalid SATELLITE value in Wallops datatake file. \
	    \nAborting conversion\n") ;
         status = ERROR ;
      }

      if (status != ERROR)
      {
         sprintf(tempstr, "%05d", WALPS_DT_rec->rev) ;
         strcat(*ASFfilename, tempstr) ;
         sprintf(tempstr, "%02d", WALPS_DT_rec->num_datatakes) ;
         strcat(*ASFfilename, tempstr) ;
         strcat(*ASFfilename, WALPS_DT_rec->recorder_id) ;
         strcat(*ASFfilename, ASF_DT_FEXT) ;


         /* Try to write out ASF version of
	 -- WALPS dt record
	 */
         if ((status = write_ASF_DTrec_from_WALPS(outfp, WALPS_DT_rec)) 
	      == ERROR)
	    syslog(LOG_ERR, 
	      "WARNING, Datatake file conversion error.  Aborting conversion\n") ; 
      }
   }

   free(ASF_hdr) ;
   free(WALPS_hdr) ;
   free(WALPS_DT_rec) ;
   fclose(infp) ;
   fclose(outfp) ;

   return(status) ;

} /* translate_WALPS_dt */





/*==============================================================================
Function:	int write_ASF_DTrec_from_WALPS(FILE *ASF_fp, 
		   WALPS_DT_Record *WALPS_DT_rec)
Description:	
	This writes datatake message record data stored in the Wallops
datatake message record WALPS_DT_rec to the output ASF file stream ASF_fp.
This function takes care of the details of translating data obtained from
the Wallops ODL file to their ASF equivalent representation.

Parameters:
	FILE *ASF_fp - pointer to output ASF file stream 
	WALPS_DT_Record *WALPS_DT_rec - pointer to Wallops datatake message
record.  This may contain data parsed from an input Wallops file.

Returns:	OK, ERROR	
Creator:	Norbert Piega	
Creation Date:  10/21/1994	
Notes:		
==============================================================================*/
#ifdef __STDC__
static int
write_ASF_DTrec_from_WALPS(FILE *ASF_fp, WALPS_DT_Record *WALPS_DT_rec)
#else
static int
write_ASF_DTrec_from_WALPS(ASF_fp, WALPS_DT_rec)
   FILE *ASF_fp ;
   WALPS_DT_Record *WALPS_DT_rec ;
#endif
{
   int status ;
   int i, ii ;
   LOGICAL digitfound = FALSE ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   if (WALPS_DT_rec == (WALPS_DT_Record *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL Wallops DT record in write_ASF_DTrec_from_WALPS\n") ;
      return(ERROR) ;
   }

   status = OK ;


   /* Datatake ID
   */
   for (ii=0; Map_Satellite_Table[ii].fa != (char *)NULL; ii++)
      if (strcmp(WALPS_DT_rec->satellite, Map_Satellite_Table[ii].fa) == 0)
      {
         fprintf(ASF_fp, Map_Satellite_Table[ii].asf) ;
         break ;
      }
   if (Map_Satellite_Table[ii].fa == (char *)NULL)
   {
      sprintf(logmsg,
	 "WARNING, Invalid Wallops satellite id %s in file %s\n",
	  WALPS_DT_rec->satellite, WALPS_DT_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }
   fprintf(ASF_fp, "/") ;

   for (ii=0; Map_Sensor_Table[ii].fa != (char *)NULL; ii++)
      if (strcmp(WALPS_DT_rec->sensor, Map_Sensor_Table[ii].fa) == 0)
      {
         fprintf(ASF_fp, "%c", Map_Sensor_Table[ii].asf[0]) ;
	 break ;
      }
   if (Map_Sensor_Table[ii].fa == (char *)NULL)
   {
      sprintf(logmsg, 
	 "WARNING, Invalid Wallops sensor id in %s\n", 
	  WALPS_DT_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }
   fprintf(ASF_fp, "/") ;
   fprintf(ASF_fp, "%05d.%02d ", WALPS_DT_rec->rev, 
                                 WALPS_DT_rec->num_datatakes) ;


   /* Activity ID
   */
   for (ii=0; Map_Activity_Table[ii].fa != (char *)NULL; ii++)
      if (strcmp(WALPS_DT_rec->activity_id, Map_Activity_Table[ii].fa) == 0)
      {
         fprintf(ASF_fp, Map_Activity_Table[ii].asf) ;
	 break ;
      }
   if (Map_Activity_Table[ii].fa == (char *)NULL)
   {
      sprintf(logmsg,
	 "WARNING, Invalid Wallops activity id in %s\n",
	  WALPS_DT_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }


   /* Agency ID
   */
   for (ii=0; Map_Agency_Table[ii].fa != (char *)NULL; ii++)
      if (strcmp(WALPS_DT_rec->agency, Map_Agency_Table[ii].fa) == 0)
      {
         fprintf(ASF_fp, Map_Agency_Table[ii].asf) ;
	 break ;
      }
   if (Map_Agency_Table[ii].fa == (char *)NULL)
   {
      sprintf(logmsg,
	 "WARNING, Invalid Wallops agency id in %s\n", 
	  WALPS_DT_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }
   fprintf(ASF_fp, " ") ;


   /* Taken
   */
   if (strcmp(WALPS_DT_rec->recorded, WALPS_DT_TAKEN_N) == 0)
      fprintf(ASF_fp, ASF_DT_TAKEN_N) ;

   else if (strcmp(WALPS_DT_rec->recorded, WALPS_DT_TAKEN_Y) == 0)
      fprintf(ASF_fp, ASF_DT_TAKEN_Y) ;

   else
   {
      sprintf(logmsg,
	 "WARNING, Invalid value for RECORDED in %s\n",
	  WALPS_DT_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }
   fprintf(ASF_fp, " ") ;


   /* Media ID
   -- Use "WS" for Media type
   */
   fprintf(ASF_fp, ASF_MTYPE_WS) ;
   for (i=0; i<(int)strlen(WALPS_DT_rec->media_id); i++)
      if (isdigit(WALPS_DT_rec->media_id[i]))
      {
         digitfound = TRUE ;
	 break ;
      }
   if (digitfound == TRUE)
      fprintf(ASF_fp, "%4s ", WALPS_DT_rec->media_id+i) ;
   else
   {
      sprintf(logmsg,
	 "WARNING, Unknown sequence number in value of MEDIA_ID in %s\n",
	  WALPS_DT_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }


   /* Startape and Endtape
   */
   fprintf(ASF_fp, "%08d", WALPS_DT_rec->start_addr) ;
   for (i=0; i<9; i++)
      fprintf(ASF_fp, " ") ;
   fprintf(ASF_fp, "%08d", WALPS_DT_rec->end_addr) ;
   for (i=0; i<9; i++)
      fprintf(ASF_fp, " ") ;


   /* On year, on time
   -- Off year, off time
   */
   fprintf(ASF_fp, "%4d ", WALPS_DT_rec->start_time.year) ;
   fprintf(ASF_fp, "%03d:%02d:%02d:%02d.%03d ", 
		   WALPS_DT_rec->start_time.doy,
                   WALPS_DT_rec->start_time.hours,
                   WALPS_DT_rec->start_time.minutes,
                   WALPS_DT_rec->start_time.seconds,
                   WALPS_DT_rec->start_time.nanoseconds/1000000) ; 

   fprintf(ASF_fp, "%4d ", WALPS_DT_rec->end_time.year) ;
   fprintf(ASF_fp, "%03d:%02d:%02d:%02d.%03d ", 
		   WALPS_DT_rec->end_time.doy,
                   WALPS_DT_rec->end_time.hours,
                   WALPS_DT_rec->end_time.minutes,
                   WALPS_DT_rec->end_time.seconds,
                   WALPS_DT_rec->end_time.nanoseconds/1000000) ; 


   /* Recorder ID
   */
   fprintf(ASF_fp, "%2s ", WALPS_DT_rec->recorder_id) ;

   /* Foreign Agency tape start counter (blanks)
   -- Foreign Agency tape end counter (blanks)
   */
   for (i=0; i<10; i++)
      fprintf(ASF_fp, " ") ;


   return(status) ;
   
} /* write_ASF_DTrec_from_WALPS */


/* End of File */
