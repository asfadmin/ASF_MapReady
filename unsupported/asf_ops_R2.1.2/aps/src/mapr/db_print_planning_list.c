#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		db_print_planning_list.c

Description:	print the active_planning_activities list.  Fortran 
				doesn't allow names longer that 32 characters, which 
				we do have here.  

Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)db_print_planning_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.db_print_planning_list.c"


/*==============================================================================
Function:       db_print_planning_list()

Description:	print the active_planning_activities list.  Fortran 
				doesn't allow names longer that 32 characters, which 
				we do have here.  

Creator:        Lawrence Stevens

Creation Date:  Fri Jan 17 20:52:41 PST 1997

Note:		
	I tried this routine with the name db_print_planning_list() 
	and couldn't get it to hook up during the linking step.  
==============================================================================*/
#include <db_sybint.h>
#include <aps_db_table.h>
#include <db_active_planning_activities.h>
void db_print_planning_list( llist	*planning_activities_llist )
{

	db_print_list(planning_activities_llist, 
		APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;

	return ;

}
