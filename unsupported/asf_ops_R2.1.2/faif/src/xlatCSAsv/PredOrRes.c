/*==============================================================================
Filename:	PredOrRes.c

Description:	
	Determine if CSA state vector file is Predicted or Restituted 

	The specification of the CSA state vector file is described in
the CSA RADARSAT Mission Control System to US and Foreign Data
Reception Operation Centers Interface Control Document [MCS to
UFDROC].

External Functions:
	get_CSAstvec_precision

Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
  1.  Jan. '96 -- R. Hoffman -- 
      Added section to handle the NORAD 2-line element (as specified in CSA 
      Interface Control Document referred to above).
  2.  Feb. '96 -- R. Hoffman --
      Always clean up before quitting.
==============================================================================*/

static char SccsFile[] = "PredOrRes.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "23 Feb 1996";
static char SccsLastChanger[] = "@(#)PredOrRes.c	1.2";
static char SccsState[] = "1.2";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "ASFsv.h"
#include "timerec.h"
#include "keyvalrec.h"
#include "CSAhdr.h"


#ifdef __STDC__
int        get_CSAstvec_precision(char *) ;
#else
int        get_CSAstvec_precision() ;
#endif


extern int                  check_CSA_keyval_stmt() ;
extern int                  check_CSA_datetime() ;
extern int                  check_CSA_realnum_vector() ;
extern int                  extract_CSA_header() ;
extern CSA_Header_Record *  alloc_CSA_header_record() ;
extern Time_Record *        alloc_time_record() ;
extern Keyword_Value_Stmt * alloc_keyval_record() ;
extern State_Vector_Record *alloc_stvec_record() ;
extern int                  parse_keyval_stmt() ;
extern int                  reset_stvec_record() ;
extern int                  is_real_number() ;
extern int                  is_datetime() ;
extern int                  convert_date_string() ;
extern int                  assign_stvec_precision() ;
extern CSA_Keywords         CSA_hdr_keyword_table[MAX_KEYTABLE_SIZE] ;
extern CSA_Keywords         CSA_keyword_table[MAX_KEYTABLE_SIZE] ;




/*==============================================================================
Function:	int get_CSAstvec_precision(char *infile)

Description:	

Parameters:
	char *infile - input CSA state vector file to be translated

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
get_CSAstvec_precision(char *infile)
#else
int
get_CSAstvec_precision(infile)
   char *infile ;
#endif
{
   FILE *ifp ;                                      /* input file pointer */
   CSA_Header_Record *header ;                       /* CSA header record */
   State_Vector_Record *stvec ;                    /* state vector record */
   Keyword_Value_Stmt *keyval ;                   /* keyword value record */
   Time_Record *gentime, *vectime ; /* generated time, time of 1st vector */
   int status ;                                          /* return status */
   char inline[MAXLINE+1] ;                            /* file input line */
   char *start, *ptr ;
   LOGICAL first_vector = TRUE ;               /* flag: 1st vector found? */
   
   /* Open input file stream
   */
   if ((ifp = fopen(infile, "r")) == (FILE *)NULL) 
      return(ERROR) ;

   /* Allocate new CSA header record
   */
   if ((header = (CSA_Header_Record *)alloc_CSA_header_record()) == 
       (CSA_Header_Record *)NULL)
   {
      status = ERROR;
      goto clean_up;
   }

   /* Read file header information and store in header record
   */
   if (extract_CSA_header(ifp, header) == ERROR)
   {
      status = ERROR;
      goto clean_up;
   }

   /* Check if file type specified in header is correct
   */
   if (strcmp(header->file_type, CSA_FTYPE_ORBDATA) != 0)
   {
      status = ERROR;
      goto clean_up;
   }

   /* Allocate new state vector record
   */
   if ((stvec = alloc_stvec_record()) == (State_Vector_Record *)NULL)
   {
      status = ERROR;
      goto clean_up;
   }

   /* Allocate new time record to store generation time
   */
   if ((gentime = (Time_Record *)alloc_time_record()) == (Time_Record *)NULL)
   {
      status = ERROR;
      goto clean_up;
   }

   /* Allocate new Keyword Value statement record to store keyval data
   */
   if ((keyval = (Keyword_Value_Stmt *)alloc_keyval_record()) ==
      (Keyword_Value_Stmt *)NULL)
   {
      status = ERROR;
      goto clean_up;
   }

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
            if (strncmp(start, CSA_hdr_keyword_table[ENDFILE_LINE].keyword,
               strlen(CSA_hdr_keyword_table[ENDFILE_LINE].keyword)) == 0) 
               if ((stvec->assigned_time == TRUE) && 
                   ((stvec->assigned_pos == FALSE) ||
                    (stvec->assigned_pos == FALSE)))
              {
                status = ERROR;
                goto clean_up;
              }
      }

      else
      {  /* POSSIBLE DATA line 
         */

         /* KEYWORD VALUE statement? 
	 */
         ptr = strchr(start, '=') ;
         if (ptr != NULL)
         {
            if ((status = parse_keyval_stmt(start, keyval)) == ACCEPT)
               if ((status = 
		      check_CSA_keyval_stmt(keyval, stvec, gentime)) != ERROR)
               {
                  if (keyval->keyword != (char *)NULL)
                     free(keyval->keyword) ;
                  if (keyval->value_string != (char *)NULL)
                     free(keyval->value_string) ;
                  continue ;
               }

            /* Reset stvec record values for next state vector 
	    */
            reset_stvec_record(stvec) ;

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
              status = ERROR;
              goto clean_up;
            }

            if (first_vector) 
            {
               /* Allocate new time record to store generation time 
	       */
               if ((vectime = (Time_Record *)alloc_time_record()) ==
                   (Time_Record *)NULL)
               {
                 status = ERROR;
                 goto clean_up;
               }

               convert_date_string(stvec->start_time, vectime) ;
               assign_stvec_precision(stvec, gentime, vectime) ;
               first_vector = FALSE ;
               switch(stvec->pred_or_rest_flag)
               {
                  case PREDICTED: 
                  {
                    status = TRUE;
                    goto clean_up;
                  }
                  case RESTITUTED:
                  {
                    status = FALSE;
                    goto clean_up;
                  }
                  default:
                  {
                    status = ERROR;
                    goto clean_up;
                  }
	       }
            }
         }


         /* REAL NUMBER VECTOR? 
	 */
         else if ((status = check_CSA_realnum_vector(start, stvec)) != ERROR)
         {
	    if (status == REJECT)
	       continue ;

	    if ((status == ACCEPT) && (stvec->assigned_time == FALSE))
            {
              status = ERROR;
              goto clean_up;
            }

            if ((stvec->assigned_time == TRUE) &&
                (stvec->assigned_pos == TRUE)  &&
                (stvec->assigned_vel == TRUE))
               /* Reset stvec record values for next state vector 
	       */
               reset_stvec_record(stvec) ;
         }

         /* UNRECOGNIZED INPUT LINE 
	 */
         else
         {
           status = ERROR;
           goto clean_up;
         }

      } /* DATA line? */

   } /* endwhile */

   /* Close file streams.  Free allocated structures. 
   */
clean_up:
   fclose(ifp) ;

   if (header != NULL) free(header) ;
   if (keyval != NULL) free(keyval) ;
   if (gentime != NULL) free(gentime) ;

   if (vectime != NULL)
      free(vectime) ;
   if (stvec != NULL)
   {
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
   }

   return(status) ; 

} /* get_CSAstvec_precision */


/* End of file */
