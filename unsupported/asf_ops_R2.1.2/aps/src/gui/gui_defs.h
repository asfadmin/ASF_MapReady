#ifndef GUI_DEFS_H
#define GUI_DEFS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	gui_defs.h
Description:	
Creator:	Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)gui_defs.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.gui_defs.h"

#define APS_GUI_DISPLAY_STR_SIZE	1024

/* shape values for DARs and Create DTK Opps (ie, site coverage) */
#define POINT_CHAR	'P'
#define POINT_STR	"P"
#define	QUAD_CHAR	'Q'
#define	QUAD_STR	"Q"
#define RECT_CHAR	'R'
#define RECT_STR	"R"

/* Multi-user values for gui processing */
#define SECOND_TO_MILLISECS	1000	/*  factor for converting sec to millisec */
#define MINUTE_TO_MILLISECS	60000	/*  factor for converting min to millisec */
#define MU_DEFAULT_INTERVAL	10	/* default interval value (in seconds) */
/*
--	*** NOTE: MU_DEFAULT_TIMEOUT must be greater than (MU_TIMEOUT_WARNING + 1)
*/
#define MU_DEFAULT_TIMEOUT	10	/* default permission expire (in minutes) */
#define MU_TIMEOUT_WARNING	1	/* # min.s for the warning preceding timeout */

#endif	/* GUI_DEFS_H */
