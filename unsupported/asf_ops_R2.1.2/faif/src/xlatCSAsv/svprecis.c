/*==============================================================================
Filename:	svprecis.c

Description:
	This module contains the function which determines the precision
of a state vector file by comparing generated time and the time of the
first state vector in the file.

External Functions:
	assign_stvec_precision

Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ASFsv.h"      /* State Vector record typedefs, etc */
#include "timerec.h"    /* Time record typedef */
#include "keyvalrec.h"  /* Keyword Value Stmt record typedef */


#ifdef __STDC__
int assign_stvec_precision(State_Vector_Record *, 
			   Time_Record *, Time_Record *) ;
#else
int assign_stvec_precision() ;
#endif

 

/*==============================================================================
Function:	int assign_stvec_precision(State_Vector_Record *stvec,
			Time_Record *gentime, Time_Record *vectime)
Description:	
	Determine and assign if CSA state vectors are PREDICTED or RESTITUTED

Parameters:
	State_Vector_Record *stvec - state vector record for which precision
will be determined
	Time_Record *gentime - state vector's generated time data
	Time_Record *vectime - time data for 1st state vector in file

Returns:	
	OK - precision assigned successfully
	ERROR - erroneous input(s)

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int 
assign_stvec_precision(
   State_Vector_Record *stvec,
   Time_Record *gentime,
   Time_Record *vectime)
#else
int
assign_stvec_precision(stvec, gentime, vectime)
   State_Vector_Record *stvec ;
   Time_Record *gentime ;
   Time_Record *vectime ;
#endif
{
   if (stvec == (State_Vector_Record *)NULL ||
       gentime == (Time_Record *)NULL ||
       vectime == (Time_Record *)NULL)
      return(ERROR) ;

   if (vectime->year != gentime->year)
      if (vectime->year > gentime->year)
         stvec->pred_or_rest_flag = PREDICTED ;
      else
         stvec->pred_or_rest_flag = RESTITUTED ;
 
   else if (vectime->day != gentime->day)
      if (vectime->day > gentime->day)
         stvec->pred_or_rest_flag = PREDICTED ;
      else
         stvec->pred_or_rest_flag = RESTITUTED ;
 
   else if (vectime->hour != gentime->hour)
      if (vectime->hour > gentime->hour)
         stvec->pred_or_rest_flag = PREDICTED ;
      else
         stvec->pred_or_rest_flag = RESTITUTED ;
 
   else if (vectime->minute != gentime->minute)
      if (vectime->minute > gentime->minute)
         stvec->pred_or_rest_flag = PREDICTED ;
      else
         stvec->pred_or_rest_flag = RESTITUTED ;
 
   else if (vectime->seconds != gentime->seconds)
      if (vectime->seconds > gentime->seconds)
         stvec->pred_or_rest_flag = PREDICTED ;
      else
         stvec->pred_or_rest_flag = RESTITUTED ;

   return(OK) ;

} /* assign_stvec_precision */
 

/* End of file */
