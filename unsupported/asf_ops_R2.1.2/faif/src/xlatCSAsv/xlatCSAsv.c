/*==============================================================================
Filename:	xlatCSAsv.c

Description:	
	Translation of CSA state vector file to ASF format

	This module contains the CSA dependent state vector translation
routines.

	The specification of the CSA state vector file is described in
the CSA RADARSAT Mission Control System to US and Foreign Data
Reception Operation Centers Interface Control Document [MCS to
UFDROC].

External Functions:
	translate_CSA_stvec
	check_CSA_keyval_stmt
	check_CSA_datetime
	check_CSA_realnum_vector

Static Functions:
	None
	
External Variables Defined:
	CSA_keyword_table
	
File Scope Static Variables:
	None
	
Notes:
  1.  Jan. '96 -- R. Hoffman -- 
      Added section to handle the NORAD 2-line element (as specified in CSA 
      Interface Control Document referred to above).
  2.  Jan. '96 -- R. Hoffman --
      Added the "else" in front of the "if" after the NORAD section.
  3.  Feb. '96 -- R. Hoffman --
  (a)  Print only one log message per vector for out-of-range numbers.
  (b)  At least for now, comment out even that log message.
  4.  Feb. '96 -- R. Hoffman --
  (a)  Added adjusted check in translate_CSA_stvec().
  (b)  Deleted openlog() in translate_CSA_stvec(). 
  5.  Aug. '96 -- R. Hoffman --
      Write only one vector to the translation file if this is a 
      predicted state vector file.  (Use new logicals "predicted"
      and "written_one_vector".) 
==============================================================================*/

static char SccsFile[] = "xlatCSAsv.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "01 Mar 1996";
static char SccsLastChanger[] = "@(#)xlatCSAsv.c	1.4";
static char SccsState[] = "1.4";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <syslog.h>
#include "ASFsv.h"
#include "timerec.h"
#include "keyvalrec.h"
#include "ASFhdr.h"
#include "CSAhdr.h"

#ifdef __STDC__
int translate_CSA_stvec(char *, char *) ;
int check_CSA_keyval_stmt(Keyword_Value_Stmt *, State_Vector_Record *, Time_Record *) ;
int check_CSA_datetime(char *, State_Vector_Record *) ;
int check_CSA_realnum_vector(char *, State_Vector_Record *) ;
#else
int translate_CSA_stvec() ;
int check_CSA_keyval_stmt() ;
int check_CSA_datetime() ;
int check_CSA_realnum_vector() ;
#endif


extern void *               util_do_malloc() ;
extern CSA_Header_Record *  alloc_CSA_header_record() ;
extern int                  extract_CSA_header() ;
extern Time_Record *        alloc_time_record() ;
extern Keyword_Value_Stmt * alloc_keyval_record() ;
extern State_Vector_Record *alloc_stvec_record() ;
extern ASF_Header_Record   *alloc_ASF_hdr_record() ;
extern ASF_SVMeta_Record   *alloc_ASF_SVmeta_record() ;
extern int                  parse_keyval_stmt() ;
extern int                  reset_stvec_record() ;
extern int                  write_ASF_SV_record() ;
extern int                  write_ASF_hdr_record() ;
extern int                  write_ASF_SVmeta_record() ;
extern int                  assign_ASF_hdr_values() ;
extern int                  is_real_number() ;
extern int                  is_datetime() ;
extern int                  convert_date_string() ;
extern int                  assign_stvec_precision() ;
extern CSA_Keywords         CSA_hdr_keyword_table[] ;

/* CSA Valid Keywords Table
-- Note: These keywords are primarily
-- specific to state vector files.
*/
CSA_Keywords CSA_keyword_table[MAX_KEYTABLE_SIZE] =
{
   { SATELLITE_NAME, "RADARSAT"},
   { GENTIME_KEYWORD, "GENERATION_TIME"},
   { ORBITNUM_KEYWORD, "ORBIT_NUMBER"},
   { GREENWCHA_KEYWORD, "GREENWICH_ANGLE"},
   { 255, NULL}
} ;






/*==============================================================================
Function:	int translate_CSA_stvec(char *infile, char *outfile)

Description:	
	Translate CSA format state vector file into standard ASF format

	This is the CSA state vector translator library function.  It
accepts an input CSA state vector file name and an output ASF format
state vector file name.  The input file is opened and parsed line by
line and the output file is written as state vector records as obtained
successfully from the input.   The input parse involves extraction of
the CSA header information and then a line by line extraction of state
vector data values.

	The state vector metadata such as generation time, orbit number
(rev) are obtained from keyword value statements in the top data lines
of the input file.

	Data structures representing CSA header and ASF header and
metadata records are allocated and used to store data as they are 
obtained.  A state vector record stores state vector data encountered
in the input file.

	At the end of the translation process, allocated structures are
freed.

Parameters:
	char *infile - input CSA state vector file to be translated
	char *outfile - translation output ASF state vector

Returns:
	Count of translated vector records - on successful translation
of records; returned even if only partially translated the input file
	ERROR - unrecoverable errors
	REJECT - unable to translate any state vector records

Creator:        Norbert Piega 
Creation Date:  03/03/1994 
Notes:
	The specification of the CSA state vector file is described in
the CSA RADARSAT Mission Control System to US and Foreign Data
Reception Operation Centers Interface Control Document [MCS to
UFDROC].
==============================================================================*/
#ifdef __STDC__
int
translate_CSA_stvec(char *infile, char *outfile)
#else
int
translate_CSA_stvec(infile, outfile)
   char *infile ;
   char *outfile ;
#endif
{
   FILE *ifp, *ofp ;                    /* input and output file pointers */
   CSA_Header_Record *header ;                       /* CSA header record */
   State_Vector_Record *stvec ;                    /* state vector record */
   Keyword_Value_Stmt *keyval ;                   /* keyword value record */
   Time_Record *gentime, *vectime ; /* generated time, time of 1st vector */
   ASF_Header_Record *hdr ;                          /* ASF header record */
   ASF_SVMeta_Record *meta ;         /* ASF State vectore metadata record */
   LOGICAL assigned_meta = FALSE ;             /* flag: assigned metadata */
   int status ;                                          /* return status */
   int i, count = 0 ;                      /* count of translated vectors */
   char inline[MAXLINE+1] ;                            /* file input line */
   char *start, *ptr ;                            /* ptrs used in parsing */
   char *spare = NULL ;                         /* ASF spare field string */
   LOGICAL first_vector = TRUE ;               /* flag: 1st vector found? */
   LOGICAL predicted = FALSE;             /* predicted state vector file? */
   LOGICAL written_one_vector = FALSE;       /* written one state vector? */
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;                /* syslog msg string */
   int adjusted = 0;                           /* vector divided by 1000? */

   /* Make sure infile and outfile are not the same names
   */
   if (strcmp(infile, outfile) == 0)
   {
      syslog(LOG_ERR, 
	"WARNING, Input and output files to translation may not be the same\n") ;
      return(ERROR) ;
   }

   /* Open input and output file streams
   */
   if ((ifp = fopen(infile, "r")) == (FILE *)NULL) 
   {
      sprintf(logmsg, "WARNING, Unable to open %s for reading\n", infile) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }
   if ((ofp = fopen(outfile, "w")) == (FILE *)NULL) 
   {
      syslog(LOG_ERR, "WARNING, Unable to open %s for writing\n", outfile) ;
      fclose(ifp) ;
      return(ERROR) ;
   }

   /* Allocate new CSA header record
   */
   if ((header = (CSA_Header_Record *)alloc_CSA_header_record()) == 
       (CSA_Header_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate CSA Header record.\n") ;
      fclose(ifp) ;
      fclose(ofp) ;
      return(ERROR) ;
   }

   /* Read file header information and store in header record
   */
   if (extract_CSA_header(ifp, header) == ERROR)
   {
      syslog(LOG_ERR, 
	 "WARNING, Error extracting CSA file header info from file %s\n",
         infile) ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(header) ;
      return(ERROR) ;
   }

   /* Check if file type specified in header is correct
   */
   if (strcmp(header->file_type, CSA_FTYPE_ORBDATA) != 0)
   {
      syslog(LOG_ERR,
         "WARNING, Error in file %s, file type specified in header is wrong type.\n",
         infile) ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(header) ;
      return(ERROR) ;
   }

   /* Allocate new state vector record
   */
   if ((stvec = alloc_stvec_record()) == (State_Vector_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate State Vector record.\n") ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(header) ;
      return(ERROR) ;
   }

   /* Allocate new time record to store generation time
   */
   if ((gentime = (Time_Record *)alloc_time_record()) == (Time_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate Time record.\n") ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(stvec) ;
      free(header) ;
      return(ERROR) ;
   }

   /* Allocate new Keyword Value statement record to store keyval data
   */
   if ((keyval = (Keyword_Value_Stmt *)alloc_keyval_record()) ==
      (Keyword_Value_Stmt *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, Unable to allocate Keyword Value Stmt. record.\n") ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(header) ;
      free(stvec) ;
      free(gentime) ;
      return(ERROR) ;
   }

   /* Allocate new ASF header record for ASF file header info 
   */
   if ((hdr = (ASF_Header_Record *)alloc_ASF_hdr_record()) ==
       (ASF_Header_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate ASF header record.\n") ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(header) ;
      free(stvec) ;
      free(gentime) ;
      free(keyval) ;
      return(ERROR) ;
   }

   /* Allocate new ASF state vector metadata record 
   */
   if ((meta = (ASF_SVMeta_Record *)alloc_ASF_SVmeta_record()) ==
       (ASF_SVMeta_Record *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, Unable to allocate ASF state vector metadata record.\n") ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(header) ;
      free(stvec) ;
      free(gentime) ;
      free(keyval) ;
      free(hdr) ;
      return(ERROR) ;
   }

   /* Create spare field string
   */
   spare = (char *)util_do_malloc(sizeof(char)*(ASFHDR_SPARE+1)) ;
   for (i=0; i<ASFHDR_SPARE; i++)
      spare[i] = ' ' ;
   spare[ASFHDR_SPARE] = '\0' ;

   /* Assign header record values for output ASF translation
   */
   status = assign_ASF_hdr_values(hdr, ASF_MSGTYPE_SV, 
                                  ASF_SRCDEST_FAIF, ASF_SRCDEST_APS, spare) ;

   /* Print output ASF file header
   */
   status = write_ASF_hdr_record(ofp, hdr) ;
   if (status == REJECT || status == ERROR)
   {
      syslog(LOG_ERR, "WARNING, Error writing ASF header.\n") ;
      fclose(ifp) ;
      fclose(ofp) ;
      free(header) ;
      free(stvec) ;
      free(gentime) ;
      free(keyval) ;
      free(hdr) ;
      free(meta) ;
      return(ERROR) ;
   }

   /* Add extraneous newline char for State Vector file
   */
   fprintf(ofp, "\n") ;

   /* Assign satellite ID in state vector record
   */
   strcpy(stvec->sat_ID, CSA_keyword_table[SATELLITE_NAME].keyword) ;

   /* Assign part of state vector metadata info
   */
   strcpy(meta->sat, ASF_SATNAME_RS) ;
   strcpy(meta->coordsys, ASF_SVPRECIS_TE) ;

   /* Read each CSA input file line and extract and translate in the process
   */
   while (fgets(inline, MAXLINE, ifp) != NULL) 
   {
      /* Skip blanks 
      */
      start = inline ;
      while (isspace(*start))
         start++ ;

      /* BLANK line
      */
      if (*start == '\0')
         continue ;

      /* COMMENT line
      */
      else if (*start == ';')
      {
         if (strncmp(start, CSA_hdr_keyword_table[HEADER_START].keyword,
            strlen(CSA_hdr_keyword_table[HEADER_START].keyword)) == 0) 
         {
            if (strncmp(start, CSA_hdr_keyword_table[ENDFILE_LINE].keyword,
               strlen(CSA_hdr_keyword_table[ENDFILE_LINE].keyword)) == 0) 
            {
               if ((stvec->assigned_time == TRUE) && 
                   ((stvec->assigned_pos == FALSE) ||
                    (stvec->assigned_pos == FALSE)))
               {
                  syslog(LOG_WARNING, 
		     "WARNING, Unable to translate state vector record\n") ;
                  status = ERROR ;
	       }
               break ;
            }
         }
      }

      else
      {
         /* POSSIBLE DATA line 
         */

         /* KEYWORD VALUE statement? 
	 */
         ptr = strchr(start, '=') ;
         if (ptr != NULL)
         {
            if ((status = parse_keyval_stmt(start, keyval)) == ACCEPT)
            {
               if ((status = 
		      check_CSA_keyval_stmt(keyval, stvec, gentime)) != ERROR)
               {
                  if (keyval->keyword != (char *)NULL)
                     free(keyval->keyword) ;

                  if (keyval->value_string != (char *)NULL)
                     free(keyval->value_string) ;

                  continue ;

               } /* Endif */
            }

            /* Reset stvec record values for next state vector 
	    */
            reset_stvec_record(stvec) ;

            syslog(LOG_WARNING,
	       "WARNING, Unrecognized/Unexpected input line %s. Ignored.\n",
		inline) ;
            continue ;

         } /* Endif ptr not NULL */

         /* NORAD 2-line element stuff?
         */
         else if (first_vector &&
                  (((start[0] == '1') && (start[1] == ' '))  ||
                   ((start[0] == '2') && (start[1] == ' ')))) 
	 {
            /* do nothing */
         }

         /* DATE/TIME VALUE? 
	 */
         else if ((status = check_CSA_datetime(start, stvec)) == ACCEPT)
         {
            if ((stvec->assigned_pos == TRUE)  ||
                (stvec->assigned_vel == TRUE))
            {
		sprintf(logmsg, 
		   "WARNING, Unexpected time input line %s\n", inline) ;
		syslog(LOG_ERR, logmsg) ;
                status = ERROR ;
                break ;
	    }

            if (first_vector) 
            {
               /* Allocate new time record to store generation time 
	       */
               if ((vectime = (Time_Record *)alloc_time_record()) ==
                   (Time_Record *)NULL)
               {
                  syslog(LOG_DEBUG, 
		     "WARNING, Unable to allocate Time record.\n") ; 
                  status = ERROR ;
                  break ;
               }

               convert_date_string(stvec->start_time, vectime) ;
               assign_stvec_precision(stvec, gentime, vectime) ;
	       if (assigned_meta == FALSE)
	       {
                  switch(stvec->pred_or_rest_flag)
                  {
                     case PREDICTED: 
			strcpy(meta->precision, "PREDICTED") ;
                        predicted = TRUE;
                        break ;
                     case RESTITUTED:
                        strcpy(meta->precision, "RESTITUTED") ;
                        break ;
                     default:
                        syslog(LOG_ERR,
			   "WARNING, Invalid precision data in state vector record.") ;
                        return(ERROR) ;
                  } /* endswitch */

		  write_ASF_SVmeta_record(ofp, meta) ;
		  assigned_meta = TRUE ;
	       }
               first_vector = FALSE ;

            } /* Endif first vector */
         } /* Endif check data time OK */


         /* REAL NUMBER VECTOR? 
	 */
         else if ((status = check_CSA_realnum_vector(start, stvec)) != ERROR)
         {
	    if (status == REJECT)
	    {
               sprintf(logmsg, "WARNING, Ignored input line %s\n", inline) ;
               syslog(LOG_WARNING, logmsg) ;
	       continue ;
	    }

	    if ((status == ACCEPT) && (stvec->assigned_time == FALSE))
	    {
               sprintf(logmsg,
		  "WARNING, Unexpected vector input line %s\n", inline) ;
               syslog(LOG_ERR, logmsg) ;
	       status = ERROR ;
	       break ;
	    }

            if ((stvec->assigned_time == TRUE) &&
                (stvec->assigned_pos == TRUE)  &&
                (stvec->assigned_vel == TRUE))
            {
	       if (assigned_meta == FALSE)
	       {
		  syslog(LOG_ERR, 
		     "WARNING, state vector metadata incomplete.\n") ;
		  status = REJECT ;
		  break ;
	       }

               /* Only write the first vector in a predicted file,
                  otherwise:
                  Write state vector record to output file.
               -- Increment record count
               */
               if (predicted && written_one_vector) { /* do nothing here */ }
               else if ((status = write_ASF_SV_record(ofp, stvec, &adjusted)) 
                    == ACCEPT)
	       {
                  if (!adjusted)
		  {
                    /* CSA state vectors must be scaled by dividing each
                       number by 1000.  The scaling is done in 
                       write_ASF_SV_record() by comparisons to ASFSV_MAXPOS,
                       etc.  If those comparisons don't catch it, the vector
                       won't be scaled and won't be correct. */ 
		    syslog(LOG_ERR, 
		       "WARNING, CSA state vector scaling failure.\n") ;
		    status = ERROR ;
		    break ;
                  }
                  else
                  {
                    count++ ;
                    adjusted = 0;
                    written_one_vector = TRUE;
                  }
	       }
               else
                  break ;

               /* Reset stvec record values for next state vector 
	       */
               reset_stvec_record(stvec) ;
            }
         }


         /* UNRECOGNIZED INPUT LINE 
	 */
         else
         {
            sprintf(logmsg, "Unrecognized/Unexpected input line %s\n", inline) ;
            syslog(LOG_WARNING, logmsg) ;
            if ((stvec->assigned_time == TRUE) &&
                ((stvec->assigned_pos == FALSE) ||
                 (stvec->assigned_pos == FALSE)))
            {
               syslog(LOG_ERR,
                  "WARNING, Unable to obtain complete translate state vector record. \
		  \nResetting values.\n") ;
	       status = ERROR ;
	       break ;
            }

         } /* Unrecognized line */

      } /* DATA line? */

   } /* endwhile */

   /* Close file streams.  Free allocated structures. 
   */
   fclose(ifp) ;
   fclose(ofp) ;

   free(header) ;
   free(keyval) ;
   free(gentime) ;
   free(hdr) ;
   free(meta) ;

   if (vectime != NULL)
      free(vectime) ;
   if (stvec->xpos_str != NULL)
      free(stvec->xpos_str) ;
   if (stvec->ypos_str != NULL)
      free(stvec->ypos_str) ;
   if (stvec->zpos_str != NULL)
      free(stvec->zpos_str) ;
   if (stvec->xvel_str != NULL)
      free(stvec->xvel_str) ;
   if (stvec->yvel_str != NULL)
      free(stvec->yvel_str) ;
   if (stvec->zvel_str != NULL)
      free(stvec->zvel_str) ;
   if (stvec->coordinate != NULL)
      free(stvec->coordinate) ;
   free(stvec) ;

   if (status != ERROR)
      return(count) ;
   else
      return(status) ; 

} /* translate_CSA_stvec */






/*==============================================================================
Function:	int check_CSA_datetime(char *dataline, 
			State_Vector_Record *stvec)
Description:	
	Check if a data line contains a valid date/time string

	This function parses the dataline to check for a date/time
sub-string.  If it is found, assigned_time is set to TRUE in the
corresponding state vector record and ACCEPT is returned.

Parameters:
	char *dataline - input line from a CSA file
	State_Vector_Record *stvec - state vector record

Returns:	
	ACCEPT - date/time obtained successfully
	REJECT - default return
	ERROR - error in input

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
check_CSA_datetime(char *dataline, State_Vector_Record *stvec)
#else
int
check_CSA_datetime(dataline, stvec)
   char *dataline ;
   State_Vector_Record *stvec ;
#endif
{
   int status = REJECT ;
   char *ptr, *dest ;
   char string1[MAXLINE+1] ;

   if (dataline == (char *)NULL || stvec == (State_Vector_Record *)NULL)
      return(ERROR) ;

   ptr = dataline ;

   /* Obtain first substring from input line 
   */
   dest = string1 ;
   while (!isspace(*ptr))
       *dest++ = *ptr++ ;
   *dest = '\0' ;

   if (*string1 == NULL)
      return(REJECT) ;

   if ((status = is_datetime(string1)) == ACCEPT)
   {
      if (strlen(string1) != TIME_STRING_LEN)
         return(REJECT) ;
      strcpy(stvec->start_time, string1) ;
      stvec->assigned_time = TRUE ;
   }
   
   return(status) ;

} /* check_CSA_datetime */






/*==============================================================================
Function:	int check_CSA_realnum_vector(char *dataline,
			State_Vector_Record *stvec)
Description:	
	Check if dataline contains a valid real number vector

	This function parses the dataline to check if it contains a
valid real number vector. If the vector is obtained successfully, the
numbers are assigned to the state vector record and ACCEPT is
returned.

Parameters:
	char *dataline - input line from a CSA file
	State_Vector_Record *stvec - state vector record

Returns:	
	ACCEPT - real number vector obtained successfully
	REJECT - default return
	ERROR - error in parsing input

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
check_CSA_realnum_vector(char *dataline, State_Vector_Record *stvec)
#else
int
check_CSA_realnum_vector(dataline, stvec)
   char *dataline ;
   State_Vector_Record *stvec ;
#endif
{
   int status = REJECT ;
   char *ptr, *dest ;
   char string1[MAXLINE+1] ;
   char string2[MAXLINE+1] ;
   char string3[MAXLINE+1] ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int out_of_range_flag = 0;

   if (dataline == (char *)NULL || stvec == (State_Vector_Record *)NULL)
      return(ERROR) ;

   ptr = dataline ;

   /* Copy 1st substring 
   */
   dest = string1 ;
   while (!isspace(*ptr))
      *dest++ = *ptr++ ;
   *dest = '\0' ;

   /* Skip blanks 
   */
   while (isspace(*ptr))
      ptr++ ;

   /* Copy 2nd substring 
   */
   dest = string2 ;
   while (!isspace(*ptr))
      *dest++ = *ptr++ ;
   *dest = '\0' ;

   /* Skip blanks 
   */
   while (isspace(*ptr))
      ptr++ ;

   /* Copy 3rd substring 
   */
   dest = string3 ;
   while (!isspace(*ptr))
      *dest++ = *ptr++ ;
   *dest = '\0' ;

   if ((*string1 == NULL) || (*string2 == NULL) || (*string3 == NULL))
      return(REJECT) ;
            
   if (is_real_number(string1) == ACCEPT &&
       is_real_number(string2) == ACCEPT &&
       is_real_number(string3) == ACCEPT)
   {
      if (stvec->assigned_time == TRUE)
      {
         if (stvec->assigned_pos == FALSE)
         {
            stvec->xpos_str =
               (char *)util_do_malloc((strlen(string1)+1)*sizeof(char)) ;
            strcpy(stvec->xpos_str, string1) ;
	    stvec->xpos = atof(string1) ;
	    if (stvec->xpos > ASFSV_MAXPOS || stvec->xpos < ASFSV_MINPOS)
              out_of_range_flag = 1;
/*  Comment out this section....
    (similar sections for the other 5 numbers have been deleted)
    Do not print a log warning for each number 
            {
                sprintf(logmsg, 
		   "WARNING, Out of range X position %s in state vector\n",
		   string1) ;
                syslog(LOG_WARNING, logmsg) ;
            }
    .....  end extended comment
*/

            stvec->ypos_str =
               (char *)util_do_malloc((strlen(string2)+1)*sizeof(char)) ;
            strcpy(stvec->ypos_str, string2) ;
	    stvec->ypos = atof(string2) ;
	    if (stvec->ypos > ASFSV_MAXPOS || stvec->ypos < ASFSV_MINPOS)
              out_of_range_flag = 1;

            stvec->zpos_str =
               (char *)util_do_malloc((strlen(string3)+1)*sizeof(char)) ;
            strcpy(stvec->zpos_str, string3) ;
	    stvec->zpos = atof(string3) ;
	    if (stvec->zpos > ASFSV_MAXPOS || stvec->zpos < ASFSV_MINPOS)
              out_of_range_flag = 1;

            stvec->assigned_pos = TRUE ;
         }
         else
         {
            stvec->xvel_str =
               (char *)util_do_malloc((strlen(string1)+1)*sizeof(char)) ;
            strcpy(stvec->xvel_str, string1) ;
	    stvec->xvel = atof(string1) ;
	    if (stvec->xvel > ASFSV_MAXVEL || stvec->xvel < ASFSV_MINVEL)
              out_of_range_flag = 1;

            stvec->yvel_str =
               (char *)util_do_malloc((strlen(string2)+1)*sizeof(char)) ;
            strcpy(stvec->yvel_str, string2) ;
	    stvec->yvel = atof(string2) ;
	    if (stvec->yvel > ASFSV_MAXVEL || stvec->yvel < ASFSV_MINVEL)
              out_of_range_flag = 1;

            stvec->zvel_str =
               (char *)util_do_malloc((strlen(string3)+1)*sizeof(char)) ;
            strcpy(stvec->zvel_str, string3) ;
	    stvec->zvel = atof(string3) ;
	    if (stvec->zvel > ASFSV_MAXVEL || stvec->zvel < ASFSV_MINVEL)
              out_of_range_flag = 1;

            stvec->assigned_vel = TRUE ;
         }

         if (out_of_range_flag)
            {
              sprintf(logmsg, 
	        "NOTICE, Out of range values will be adjusted in state vector for %s\n",
		stvec->start_time) ;
/*  When the following syslog is enabled, it prints one message per vector.
    When several CSA state vector files are being processed at once, this
    overloads the log file, and messages are skipped!  Until we fix that,
    leave this commented out. */
/*              syslog(LOG_DEBUG, logmsg) ;    */
            }

         return(ACCEPT) ;

      } /* Endif assigned time already */

      else
      {
         syslog(LOG_ERR,
            "WARNING, Error in CSA orbit data; Invalid or missing time values for %s\n",
            dataline) ;
         return(ERROR) ; 
      }

   } /* Endif strings 1, 2 and 3 are real number strings */

   else if (stvec->assigned_time == TRUE)
   {
      syslog(LOG_ERR, 
	 "WARNING, Error in CSA orbit data; Invalid Vector values.\n") ;
      return(ERROR) ; 
   }

   return(status) ;

} /* check_CSA_realnum_vector */






/*==============================================================================
Function:	int check_CSA_keyval_stmt(Keyword_Value_Stmt *keyval,
			State_Vector_Record *stvec, Time_Record *gentime)
Description:	
	Check for CSA relevant data in keyword value statement record 

	This function tries to match valid CSA keywords (from CSA
keyword table) with the keyword string in the keyword value statement
record.  When a match is found, the value string in the keyword value
statement record is converted as necessary or assigned to the
appropriate state vector record field.

Parameters:
	Keyword_Value_Stmt *keyval - keyword value statement record
	State_Vector_Record *stvec - state vector record
	Time_Record *gentime - generated time record
	
Returns:	
	ACCEPT - Matched a valid CSA keyword
	REJECT - default
	ERROR - NULL input(s)

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int 
check_CSA_keyval_stmt(
   Keyword_Value_Stmt *keyval, 
   State_Vector_Record *stvec,
   Time_Record *gentime)
#else
int
check_CSA_keyval_stmt(keyval, stvec, gentime)
   Keyword_Value_Stmt *keyval;
   State_Vector_Record *stvec ;
   Time_Record *gentime ;
#endif
{
   int status  = REJECT ;

   if (keyval == (Keyword_Value_Stmt *)NULL ||
       stvec == (State_Vector_Record *)NULL ||
       gentime == (Time_Record *)NULL)
       return(ERROR) ;

   if (strcmp(keyval->keyword, 
	      CSA_keyword_table[GENTIME_KEYWORD].keyword) == 0)
      status = convert_date_string(keyval->value_string, gentime) ;

   else if (strcmp(keyval->keyword,
                   CSA_keyword_table[ORBITNUM_KEYWORD].keyword) == 0)
   {
      stvec->rev = atoi(keyval->value_string) ;
      status = ACCEPT ;
   }

   else if (strcmp(keyval->keyword,
                   CSA_keyword_table[GREENWCHA_KEYWORD].keyword) == 0)
      status = ACCEPT ; 

   return(status) ;

} /* check_CSA_keyval_stmt */

/* End of file */
