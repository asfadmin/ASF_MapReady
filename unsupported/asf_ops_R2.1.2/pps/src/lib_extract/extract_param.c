/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

static char SccsFileId[] = "@(#)extract_param.c	1.1  11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "odldef.h"
#include "odlinter.h"
#include "PPSdefs.h"
#include "PPSextern.h"
#include "PPSerr.h"

/*==============================================================================
Function:	int PPS_extract_param_value(
                                            AGGREGATE object,
                                            void *record_field, 
                                            char *keyword,
                                            int length)

Description:	
	This function searches for the keyword inside the ODL aggregation
represented by object.  If the keyword is found, the value of the keyword
is obtained and assigned to record_field.  Note that record field has t
be cast to the proper type for this assignment to work.

Parameters:
	AGGREGATE object - the ODL aggregate inside which
                               the keyword is searched
	void *record_field - the variable to assign the
                               keyword value obtained
	char *keyword - the keyword string to search for in
                               the specified object/aggregate
        int length - the maximum length for symbols or strings

Returns:	ER_NO_ERROR
                ER_KEYWORD_VALUE_TOO_LONG
		ER_INVALID_KEYWORD_VALUE_TYPE
		ER_KEYWORD_NOT_FOUND
Notes:		
	Note that record field is treated as a pointer to an integer if
the value to assign is an integer, a pointer to a char if the value to
assign is a string, and a pointer to a struct ODLDate if the value to
assign is a struct ODLDate.  These are the only types expected.
==============================================================================*/
#ifdef __STDC__
int
PPS_extract_param_value(
AGGREGATE object, void *record_field, char *keyword, short length)
#else
int
PPS_extract_param_value(object, record_field, keyword, length)
   AGGREGATE object ;
   void      *record_field ;
   char      *keyword ;
   short     length;
#endif
{
   PARAMETER parameter ;
   VALUE param_value ;
   int status = ER_NO_ERROR ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   if ((parameter = FindParameter(object, keyword)) != NULL)
   {
      if ((param_value = FirstValue(parameter)) != NULL)
      {
	 switch(param_value->item.type)
	 {
	    case TV_INTEGER:
               *((int *)record_field) = param_value->item.value.integer.number ;
	       break ;

	    case TV_REAL:
               *((float *)record_field) = param_value->item.value.real.number ;
	       break ;

            case TV_SYMBOL:
               if (param_value->item.length > length)
               {
	           status = ER_KEYWORD_VALUE_TOO_LONG ;
                   break;
               }
	       strcpy((char *)record_field, param_value->item.value.string) ;
	       break ;

            case TV_STRING:
               if (param_value->item.length > length)
               {
	           status = ER_KEYWORD_VALUE_TOO_LONG ;
                   break;
               }
	       strcpy((char *)record_field, param_value->item.value.string);
	       break ;

            case TV_DATE_TIME:
	       *((struct ODLDate *)record_field) = 
		                 param_value->item.value.date_time ;
	       break ;
    
	    default:
	       status = ER_INVALID_KEYWORD_VALUE_TYPE ;
	 }
      }
      else
      {
	 status = ER_KEYWORD_NOT_FOUND ;
      }
   }
   else
   {
	 status = ER_KEYWORD_NOT_FOUND ;
   }

   return(status) ;

} /* PPS_extract_param_value */
