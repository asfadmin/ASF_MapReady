#ifndef GUI_MU_UTILS_H
#define GUI_MU_UTILS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:		gui_mu_utils.h
Description:	include file for using the gui mu utilities.
Creator:		Teresa McKillop
Notes:			
==============================================================================*/
#pragma ident	"@(#)gui_mu_utils.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.gui_mu_utils.h"


/*==============================================================================
    Function Prototypes
==============================================================================*/
int		gui_get_planning_permission( char *muActivityId,
			char *startTime, char *stopTime, char *stationId ) ;

int		gui_get_dar_permission( char *muActivityId, int dar_id ) ;

int		gui_get_single_permission( char *muActivityId ) ;

int		gui_free_permission( int permId,
			char *muActivityId, char *muActivityType ) ;


#endif	/* GUI_MU_UTILS_H */

