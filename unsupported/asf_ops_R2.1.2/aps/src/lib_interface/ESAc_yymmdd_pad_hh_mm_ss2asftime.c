#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		ESAc_yymmdd_pad_hh_mm_ss2asftime.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)ESAc_yymmdd_pad_hh_mm_ss2asftime.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.ESAc_yymmdd_pad_hh_mm_ss2asftime.c"

#include <string.h>           /* for strncpy(), strlen(), strcpy()        */
#include <timeconv.h>         /* for tc_esadaytime2asf(), tc_esa2asf()    */

/*==============================================================================
Function:       ESAc_yymmdd_pad_hh_mm_ss2asftime  

Description:    translate embedded ESA time string into asf time string

Parameters:     
Type		Name        		Definition
char 		*source_string 		string containing embedded ESA time
char 		*result_string		asf time string

Returns:        
Type		  Name        		Definition
int								TRUE = success.
int								FALSE = error.

Creator:        Miguel Siu

Creation Date:  Wed Jul  5 15:53:06 PDT 1995

Notes:	THIS ROUTINE IS MOST DEFINITELY HARD-CODED TO SHAQ HEADER LOCATIONS.  
		This call allows us to access routine tc_yyyymmdd_hhmmss2asf() while 
		maintaining the function prototype for ESAc_* conversion routines.
		Time conversion returns success = 1   errors = 0
==============================================================================*/
int ESAc_yymmdd_pad_hh_mm_ss2asftime(
	void		*unused_pointer, 
	char 		*source_string, 
	char 		*result_string)
{
	char		date_string[] = "19yymmdd" ;
	char		time_string[] = "hh:mm:ss" ;

	strncpy (date_string+2, 	source_string,		6) ;
	strncpy (time_string,		source_string+17,	8) ;

    return ( tc_yyyymmdd_hhmmss2asf(date_string, time_string, result_string) ) ;
}
