#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		mu_print_error_msg.c

	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)mu_print_error_msg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.mu_print_error_msg.c"


/*==============================================================================
Function:       mu_print_error_msg()

Description:    print error multi-user library error message

Parameters:     return code, which is < 0.  

Creator:        Lawrence Stevens

Creation Date:  Thu Jan 16 18:29:39 PST 1997

Notes:		
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  
==============================================================================*/
#include <mu_utilities.h>
void mu_print_error_msg( int error_code )
{

	if( error_code >= 0 )
		printf("Error code = %d\n" ) ;

	if( error_code < 0 )
		printf("%s\n", MU_ERROR_MESSAGE( error_code ) ) ;

	return ;
}
