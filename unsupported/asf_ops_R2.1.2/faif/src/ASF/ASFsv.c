/*==============================================================================
Filename:	ASFsv.c

Description:	ASF State Vector file manipulation functions

	This module contains the functions that operate on state vector
records found in a ASF state vector file.  It includes allocation of data 
structures, initialization and extraction from the input ASF file.

External Functions:
	alloc_ASF_SVmeta_record
	extract_ASF_SVmeta_record
	write_ASF_SVmeta_record
	alloc_stvec_record
	alloc_ASF_SV_record
	extract_ASF_SV_record
	write_ASF_SV_record
	reset_stvec_record
	reset_ASF_SV_record

Static Functions:
	None

External Variables Defined:
	None

File Scope Static Variables:
	None
	
Notes:
1.  Feb. '96 - R. Hoffman
    Added adjusted_flag in write_ASF_SV_record().
==============================================================================*/

static char SccsFile[] = "ASFsv.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "31 Mar 1996";
static char SccsLastChanger[] = "@(#)ASFsv.c	1.3";
static char SccsState[] = "1.3";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include "faifdefs.h"
#include "ASF.h"
#include "ASFsv.h"   /* ASF State vector record typedefs, etc */


#ifdef __STDC__
ASF_SVMeta_Record   *alloc_ASF_SVmeta_record(void) ;
int                  extract_ASF_SVmeta_record(FILE *, ASF_SVMeta_Record *) ;
int                  write_ASF_SVmeta_record(FILE *, ASF_SVMeta_Record *) ;
State_Vector_Record *alloc_stvec_record(void) ;
ASF_SV_Record       *alloc_ASF_SV_record(void) ;
int                  extract_ASF_SV_record(FILE *, ASF_SV_Record *) ;
int                  write_ASF_SV_record(FILE *, ASF_SV_Record *, int *) ;
int                  reset_stvec_record(State_Vector_Record *) ;
int                  reset_ASF_SV_record(ASF_SV_Record *) ;
#else
ASF_SVMeta_Record   *alloc_ASF_SVmeta_record() ;
int                  extract_ASF_SVmeta_record() ;
int                  write_ASF_SVmeta_record() ;
State_Vector_Record *alloc_stvec_record() ;
ASF_SV_Record       *alloc_ASF_SV_record() ;
int                  extract_ASF_SV_record() ;
int                  write_ASF_SV_record() ;
int                  reset_stvec_record() ;
int                  reset_ASF_SV_record() ;
#endif

extern void *util_do_malloc() ;



/*==============================================================================
Function:	ASF_SVMeta_Record *alloc_ASF_SVmeta_record(void)

Description:	
	Allocate and initialize a state vector metadata record 

	This function allocates a state vector metadata record and then
initializes its fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to a state vector metadata record or NULL	
Creator:	Norbert Piega	
Creation Date:	07/26/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
ASF_SVMeta_Record *
alloc_ASF_SVmeta_record(void)
#else
ASF_SVMeta_Record *
alloc_ASF_SVmeta_record()
#endif
{
   ASF_SVMeta_Record *svmeta = NULL ;

   svmeta = (ASF_SVMeta_Record *) util_do_malloc(sizeof(ASF_SVMeta_Record)) ;
   if (svmeta != (ASF_SVMeta_Record *) NULL)
   {
      *svmeta->sat = NULL ;
      *svmeta->precision = NULL ;
      *svmeta->coordsys = NULL ;
   }

   return(svmeta) ;

} /* alloc_ASF_SVmeta_record */





/*==============================================================================
Function:	int write_ASF_SVmeta_record(FILE *outfp, 
			ASF_SVMeta_Record *meta)
Description:	
	Write out the contents of the state vector metadata record to the
output file stream specified via outfp.

Parameters:
	FILE *outfp - file pointer representing the output file stream
	ASF_SVMeta_Record *meta - pointer to state vector metadata record
to write

Returns:	
	ACCEPT - meta record was written successfully
	REJECT - NULL meta record fields 
	ERROR - NULL meta record or NULL file pointer

Creator:	Norbert Piega	
Creation Date:	08/01/1994
Notes:		
	The format of the ASF header record is documented in the ASF 
Software Interface Specification, JPL-D5267.
==============================================================================*/
#ifdef __STDC__
int
write_ASF_SVmeta_record(FILE *outfp, ASF_SVMeta_Record *meta)
#else
int
write_ASF_SVmeta_record(outfp, meta)
   FILE *outfp ;
   ASF_SVMeta_Record *meta ;
#endif
{
   int i ;

   if (meta == (ASF_SVMeta_Record *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL ASF meta record input in metadata record write\n");
      return(ERROR) ;
   }

   if (outfp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL file input writing ASF metadata record\n");
      return(ERROR) ;
   }

   if (*meta->sat != (char)NULL)
   {
      fprintf(outfp, "%s", meta->sat) ;
      for (i=1; i<= ASFSVMETA_SAT+1-(int)strlen(meta->sat); i++)
	 fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   if (*meta->precision != (char)NULL)
   {
      fprintf(outfp, "%s", meta->precision) ;
      for (i=1; i<=ASFSVMETA_PREC+1-(int)strlen(meta->precision); i++)
	 fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   if (*meta->coordsys != (char)NULL)
   {
      fprintf(outfp, "%s", meta->coordsys) ;
      for (i=1; i<=ASFSVMETA_CRDSYS+1-(int)strlen(meta->coordsys); i++)
	 fprintf(outfp, " ") ;
      fprintf(outfp, "\n") ;
   }
   else
      return(REJECT) ;

   return(ACCEPT) ;

} /* write_ASF_SVmeta_record */






/*==============================================================================
Function:	int extract_ASF_SVmeta_record(FILE *ASF_fp,
					      ASF_SVmeta_Record *ASF_svmeta)
Description:	
	This function parses an input ASF file and stores the obtained data
in the ASF State Vector metadata record ASF_sv.

Parameters:
	FILE *ASF_fp - pointer to input ASF file stream
	ASF_SVmeta_Record *ASF_svmeta - pointer to ASF state vector record 
where state vector metadata information parsed from the input file are stored

Returns:	
	ACCEPT - able to extract data and store in record
	REJECT - data read failed
	ERROR - error, bad input

Creator:	Norbert Piega	
Creation Date:	10/24/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_ASF_SVmeta_record(FILE *ASF_fp, ASF_SVMeta_Record *ASF_svmeta)
#else
int
extract_ASF_SVmeta_record(ASF_fp, ASF_svmeta)
   FILE *ASF_fp ;
   ASF_SVMeta_Record *ASF_svmeta ;
#endif
{
   char *svmeta_str ;
   char tempstr[MAXLINE] ;
   char *newline = NULL ;

   if (ASF_fp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL input file stream in extract_ASF_SVmeta_record\n") ;
      return(ERROR) ;
   } 

   svmeta_str = (char *)util_do_malloc(sizeof(char)*(ASFSVMETA_RECLEN)) ;
   if (fgets(svmeta_str, (size_t)ASFSVMETA_RECLEN+1, ASF_fp) == NULL)
      return(REJECT) ;

   if ((newline = strchr(svmeta_str, '\n')) != (char *)NULL)
   {
      syslog(LOG_WARNING, 
	 "WARNING, Unexpected newline encountered prior to state vector \
	 \nmetadata.  Ignoring newline\n") ;
      if ((int)strlen(svmeta_str) < ASFSVMETA_RECLEN)
	 if (fgets(svmeta_str, (size_t)ASFSVMETA_RECLEN+1, ASF_fp) == NULL)
            return(REJECT) ;
   }

   strncpy(ASF_svmeta->sat, svmeta_str, ASFSVMETA_SAT) ;
   ASF_svmeta->sat[ASFSVMETA_SAT] = '\0' ;
   svmeta_str += (ASFSVMETA_SAT+1) ;

   strncpy(ASF_svmeta->precision, svmeta_str, ASFSVMETA_PREC) ;
   ASF_svmeta->precision[ASFSVMETA_PREC] = '\0' ;
   svmeta_str += (ASFSVMETA_PREC+1) ;

   strncpy(ASF_svmeta->coordsys, svmeta_str, ASFSVMETA_CRDSYS) ;
   ASF_svmeta->coordsys[ASFSVMETA_CRDSYS] = '\0' ;
   svmeta_str += (ASFSVMETA_CRDSYS+1) ;

   return(OK) ;

} /* extract_ASF_SVmeta_record */






/*==============================================================================
Function:	State_Vector_Record *alloc_stvec_record(void)
Description:	
	Allocate and initialize a state vector record data structure

	This function allocates a state vector value record and then
initializes its fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to newly allocated record or NULL
Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
State_Vector_Record *
alloc_stvec_record(void)
#else
State_Vector_Record *
alloc_stvec_record()
#endif
{
   State_Vector_Record *stvec = NULL ;

   stvec = (State_Vector_Record *) util_do_malloc(sizeof(State_Vector_Record)) ;
   if (stvec != (State_Vector_Record *) NULL)
   {
      *stvec->sat_ID = NULL ;
      stvec->rev = -1 ;
      *stvec->start_time = NULL ;
      stvec->pred_or_rest_flag = UNASSIGNED ;
      stvec->xpos_str = NULL ;
      stvec->ypos_str = NULL ;
      stvec->zpos_str = NULL ;
      stvec->xvel_str = NULL ;
      stvec->yvel_str = NULL ;
      stvec->zvel_str = NULL ;
      stvec->xpos = 0.0 ;
      stvec->ypos = 0.0 ;
      stvec->zpos = 0.0 ;
      stvec->xvel = 0.0 ;
      stvec->yvel = 0.0 ;
      stvec->zvel = 0.0 ;
      stvec->coordinate = NULL ;
      stvec->assigned_time = FALSE ;
      stvec->assigned_pos = FALSE ;
      stvec->assigned_vel = FALSE ;
   }

   return(stvec) ;

} /* alloc_stvec_record */





/*==============================================================================
Function:	ASF_SV_Record *alloc_ASF_SV_record(void)

Description:	Allocate and initialize an ASF State Vector record

	This function allocates an ASF State Vector record and then
initializes its fields by the call to the generic routine alloc_stvec_record.
If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to the allocated ASF State Vector record or NULL
Creator:	Norbert Piega	
Creation Date:	10/24/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
ASF_SV_Record *
alloc_ASF_SV_record(void)
#else
ASF_SV_Record *
alloc_ASF_SV_record()
#endif
{
   ASF_SV_Record *stvec = NULL ;

   stvec = (ASF_SV_Record *)alloc_stvec_record() ;

   return(stvec) ;

} /* alloc_ASF_SV_record */




/*==============================================================================
Function:	int extract_ASF_SV_record(FILE *ASF_fp,
					  ASF_SV_Record *ASF_sv)
Description:	
	This function parses an input ASF file and stores the obtained data
in the ASF State Vector record ASF_sv.

Parameters:
	FILE *ASF_fp - pointer to input ASF file stream
	ASF_SV_Record *ASF_sv - pointer to ASF state vector record where
state vector information parsed from the input file are stored

Returns:	
	ACCEPT - able to extract data and store in record
	REJECT - data read failed
	ERROR - error, bad input

Creator:	Norbert Piega	
Creation Date:	10/24/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_ASF_SV_record(FILE *ASF_fp, ASF_SV_Record *ASF_sv)
#else
int
extract_ASF_SV_record(ASF_fp, ASF_sv)
   FILE *ASF_fp ;
   ASF_SV_Record *ASF_sv ;
#endif
{
   char *svstr ;
   char tempstr[MAXLINE] ;
   char *newline = NULL ;

   if (ASF_fp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL input file stream in extract_ASF_sv_record\n") ;
      return(ERROR) ;
   }

   svstr = (char *)util_do_malloc(sizeof(char)*(ASFSV_RECLEN)) ;
   if (fgets(svstr, (size_t)ASFSV_RECLEN+1, ASF_fp) == NULL)
      return(REJECT) ;

   if ((newline = strchr(svstr, '\n')) != (char *)NULL)
   {
      syslog(LOG_WARNING, 
	 "WARNING, Unexpected newline encountered prior to state vector \
	 \nrecord data.  Ignoring newline\n") ;
      if ((int)strlen(svstr) < ASFSV_RECLEN)
	 if (fgets(svstr, (size_t)ASFSV_RECLEN+1, ASF_fp) == NULL)
            return(REJECT) ;
   }

   strncpy(tempstr, svstr, ASFSV_REV) ;
   tempstr[ASFSV_REV] = '\0' ;
   ASF_sv->rev = atol(svstr) ;
   svstr += (ASFSV_REV+1) ;

   strncpy(ASF_sv->start_time, svstr, TIME_STRING_LEN) ;
   ASF_sv->start_time[TIME_STRING_LEN] = '\0' ;
   svstr += (TIME_STRING_LEN+1) ;

   ASF_sv->xpos_str = 
      (char *)util_do_malloc(sizeof(char)*(ASFSV_POSITION+1)) ;
   strncpy(ASF_sv->xpos_str, svstr, ASFSV_POSITION) ;
   ASF_sv->xpos_str[ASFSV_POSITION] = '\0' ;
   svstr += (ASFSV_POSITION+1) ;

   ASF_sv->ypos_str = 
      (char *)util_do_malloc(sizeof(char)*(ASFSV_POSITION+1)) ;
   strncpy(ASF_sv->ypos_str, svstr, ASFSV_POSITION) ;
   ASF_sv->ypos_str[ASFSV_POSITION] = '\0' ;
   svstr += (ASFSV_POSITION+1) ;

   ASF_sv->zpos_str = 
      (char *)util_do_malloc(sizeof(char)*(ASFSV_POSITION+1)) ;
   strncpy(ASF_sv->zpos_str, svstr, ASFSV_POSITION) ;
   ASF_sv->zpos_str[ASFSV_POSITION] = '\0' ;
   svstr += (ASFSV_POSITION+1) ;

   ASF_sv->xvel_str = 
      (char *)util_do_malloc(sizeof(char)*(ASFSV_VELOCITY+1)) ;
   strncpy(ASF_sv->xvel_str, svstr, ASFSV_VELOCITY) ;
   ASF_sv->xvel_str[ASFSV_VELOCITY] = '\0' ;
   svstr += (ASFSV_POSITION+1) ;

   ASF_sv->yvel_str = 
      (char *)util_do_malloc(sizeof(char)*(ASFSV_VELOCITY+1)) ;
   strncpy(ASF_sv->yvel_str, svstr, ASFSV_VELOCITY) ;
   ASF_sv->yvel_str[ASFSV_VELOCITY] = '\0' ;
   svstr += (ASFSV_VELOCITY+1) ;

   ASF_sv->zvel_str = 
      (char *)util_do_malloc(sizeof(char)*(ASFSV_VELOCITY+1)) ;
   strncpy(ASF_sv->zvel_str, svstr, ASFSV_VELOCITY) ;
   ASF_sv->zvel_str[ASFSV_VELOCITY] = '\0' ;
   svstr += (ASFSV_VELOCITY+1) ;

   return(ACCEPT) ;

} /* extract_ASF_sv_record */





/*==============================================================================
Function:	int write_ASF_SV_record(FILE *outfp, 
					State_Vector_Record *stvec,
                                        int *adjusted_flag)
Description:	
	Allocate and initialize a keyword value record data structure

	This function writes an output state vector record to the
output file stream.  ACCEPT is returned after writing all fields.  If
the write cannot be competed, REJECT is returned.  ERROR is returned if
NULL inputs are passed.  Note that state vector elements may be out of
the valid range; this function ignores out of range errors and writes out
the state vector elements anyway. 

Parameters:
	FILE *outfp - output file stream to write the state vector record to
	State_Vector_Record *stvec - state vector record to write
        int *adjusted_flag - whether or not the vector gets divided by 1000

Returns:	
	ACCEPT - completed record write
	REJECT - incomplete record write
	ERROR  - erroneous input(s)

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
write_ASF_SV_record(FILE *outfp, State_Vector_Record *stvec, 
                    int *adjusted_flag)
#else
int
write_ASF_SV_record(outfp, stvec, adjusted_flag)
   FILE *outfp ;
   State_Vector_Record *stvec ;
   int *adjusted_flag;
#endif
{
   char *cptr = NULL ;
   double double_num ;
   int i ;

   *adjusted_flag = 0;
   if (outfp == (FILE *)NULL || stvec == (State_Vector_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL write_state_vector_record inputs\n");
      return(ERROR) ;
   }

   if (stvec->rev > 0)
   {
      if (stvec->rev < 0 || stvec->rev > 99999)
         return(REJECT) ;
      fprintf(outfp, "%-6d", stvec->rev) ;
   }
   else
      return(REJECT) ;

   if (*stvec->start_time != NULL)
   {
      /* Start_time field has year and time separated by a colon.
      -- Replace colon with a space
      */
      if ((int)strlen(stvec->start_time) > (ASFSV_YEAR)+(ASFSV_TIME)+2)
	 return(REJECT) ;

      cptr = strchr(stvec->start_time, '-') ;
      if (cptr != (char *)NULL)
      {
         *cptr = ' ' ;
         cptr = strchr(stvec->start_time, '-') ;
         if (cptr != (char *)NULL)
	    *cptr = ':' ;
      }
      else
      {
	 syslog(LOG_ERR, "WARNING, Error in state vector time data\n") ;
	 return(ERROR) ;
      }
      fprintf(outfp, "%s", stvec->start_time) ;
      for (i=1; i<=((ASFSV_YEAR)+(ASFSV_TIME)+2)
		   -(int)strlen(stvec->start_time); i++)
         fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   /* If State vector contains any out of range values,
   -- write it out anyway
   */
   if (stvec->xpos > ASFSV_MAXPOS || stvec->xpos < ASFSV_MINPOS ||
       stvec->ypos > ASFSV_MAXPOS || stvec->ypos < ASFSV_MINPOS ||
       stvec->zpos > ASFSV_MAXPOS || stvec->zpos < ASFSV_MINPOS || 
       stvec->xvel > ASFSV_MAXVEL || stvec->xvel < ASFSV_MINVEL ||      
       stvec->xvel > ASFSV_MAXVEL || stvec->xvel < ASFSV_MINVEL ||      
       stvec->xvel > ASFSV_MAXVEL || stvec->xvel < ASFSV_MINVEL) 
   {
      /* Note:  Write the position vector in kilometers (from meters)
      -- and velocity in meters per second (from mm/sec)
      */
      fprintf(outfp, "%-12.5f", stvec->xpos/1000) ;
      fprintf(outfp, "%-12.5f", stvec->ypos/1000) ;
      fprintf(outfp, "%-12.5f", stvec->zpos/1000) ;
      fprintf(outfp, "%-12.5f", stvec->xvel/1000) ;
      fprintf(outfp, "%-12.5f", stvec->yvel/1000) ;
      fprintf(outfp, "%-12.5f\n", stvec->zvel/1000) ;
      *adjusted_flag = 1;
   }
   else
   {
      /* Note:  Write the position vector in kilometers 
      -- and velocity as is (meters per second)
      */
      double_num = atof(stvec->xpos_str) ;
      fprintf(outfp, "%-12.5f", double_num/1000) ;

      double_num = atof(stvec->ypos_str) ;
      fprintf(outfp, "%-12.5f", double_num/1000) ;

      double_num = atof(stvec->zpos_str) ;
      fprintf(outfp, "%-12.5f", double_num/1000) ;

      double_num = atof(stvec->xvel_str) ;
      fprintf(outfp, "%-12.5f", double_num/1000) ;

      double_num = atof(stvec->yvel_str) ;
      fprintf(outfp, "%-12.5f", double_num/1000) ;

      double_num = atof(stvec->zvel_str) ;
      fprintf(outfp, "%-12.5f", double_num/1000) ;

      fprintf(outfp, "\n") ;
   }

   return(ACCEPT) ;

} /* write_ASF_SV_record */






/*==============================================================================
Function:	int reset_stvec_record(State_Vector_Record *stvec)
Description:	
	Reset the values of a state vector record's fields

	This function resets the values of some of the fields of a
state vector record.  Note that the satellite ID, Rev number, Precision
and Coordinate Type are not modified.  They are assumed to hold
previously assigned valid values.

Parameters:
	State_Vector_Record *stvec - state vector record to reset

Returns:
	ERROR - input record is NULL
	OK - reset successful

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
   The satellite ID, Rev number, Precision and Coordinate Type are
not modified.  They are assumed to hold previously assigned valid
values.  These values are constant per state vector set in a single
state vector file.
==============================================================================*/
#ifdef __STDC__
int
reset_stvec_record(State_Vector_Record *stvec)
#else
int
reset_stvec_record(stvec)
   State_Vector_Record *stvec ;
#endif
{
   /* Note: Do not change assigned satellite ID, Rev number, precision
   -- coordinate type
   */

   if (stvec == (State_Vector_Record *)NULL)
      return(ERROR) ;

   *stvec->start_time = NULL ;

   if (stvec->xpos_str != NULL)
      free(stvec->xpos_str) ;
   stvec->xpos_str = NULL ;
   stvec->xpos = 0.0 ;

   if (stvec->ypos_str != NULL)
      free(stvec->ypos_str) ;
   stvec->ypos_str = NULL ;
   stvec->ypos = 0.0 ;

   if (stvec->zpos_str != NULL)
      free(stvec->zpos_str) ;
   stvec->zpos_str = NULL ;
   stvec->zpos = 0.0 ;

   if (stvec->xvel_str != NULL)
      free(stvec->xvel_str) ;
   stvec->xvel_str = NULL ;
   stvec->xvel = 0.0 ;

   if (stvec->yvel_str != NULL)
      free(stvec->yvel_str) ;
   stvec->yvel_str = NULL ;
   stvec->yvel = 0.0 ;

   if (stvec->zvel_str != NULL)
      free(stvec->zvel_str) ;
   stvec->zvel_str = NULL ;
   stvec->zvel = 0.0 ;

   stvec->assigned_time = FALSE ;
   stvec->assigned_pos = FALSE ;
   stvec->assigned_vel = FALSE ;

   return(OK) ;

} /* reset_stvec_record */





/*==============================================================================
Function:	int reset_ASF_SV_record(ASF_SV_Record *stvec)
Description:	
	Reset the values of a state vector record's fields

	This function resets the values of some of the fields of an
ASF state vector record by calling the generic function reset_stvec_record.
Note that the satellite ID, Rev number, Precision and Coordinate Type are 
not modified.  They are assumed to hold previously assigned valid values.

Parameters:
	ASF_SV_Record *stvec - state vector record to reset

Returns:
	ERROR - input record is NULL
	OK - reset successful

Creator:	Norbert Piega	
Creation Date:	11/01/1994
Notes:		
   The satellite ID, Rev number, Precision and Coordinate Type are
not modified.  They are assumed to hold previously assigned valid
values.  These values are constant per state vector set in a single
state vector file.
==============================================================================*/
#ifdef __STDC__
int
reset_ASF_SV_record(ASF_SV_Record *stvec)
#else
int
reset_ASF_SV_record(stvec)
   ASF_SV_Record *stvec ;
#endif
{
   return(reset_stvec_record(stvec)) ;

} /* reset_ASF_SV_record */


/* End of file */
