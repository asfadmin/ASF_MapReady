
#ifndef	LOG_MAIN_H
/* @(#)log_main.h	1.7 93/07/08 17:49:45  */

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_main.h
Type:           Local header
Title:			Main common header file.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Description:	This provides the routines, macros and global variables
				that are to be used by the other modules and that don't
				really belong to any one of those modules.


*************************************************************************** */
#endif

#define	LOG_MAIN_H


/*  =========================  Includes  =========================  */

/*  standard C includes  */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <malloc.h>
#include <memory.h>

/* Motif Includes */
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

/*  Log Includes  */
#include "log_alert.h"
#include "log_browse.h"
#include "log_file.h"
#include "log_filter.h"
#include "log_list.h"
#include "log_menu.h"
#include "log_print.h"
#include "log_sound.h"
#include "log_status.h"


/*  =========================  Defines  =========================  */

#define	MAX_ENTRY_LEN	400

/*  Exit Codes  */
#define	LOG_EXIT_OK		0
#define	LOG_NO_LOGSPEC	1
#define	LOG_BAD_LOGSPEC	2
#define	LOG_BAD_LOGDIR	3
#define	LOG_OPEN_ERROR	4
#define	LOG_READ_ERROR	5
#define	LOG_ALLOC_ERROR	6


/*  =========================  Types  =========================  */


typedef	struct	{
		char		*logFile ;
		char		*filter ;
		char		*warnTag ;
		char		*critTag ;
		char		*eoLogTag ;
		
		Boolean		debug ;
		unsigned	delay ;

		Boolean		soundOn ;
		Boolean		browseMode ;
		Boolean		firstLog ;	/*  o.w., last is assumed to be  */

}	resource_t, *resource_ptr_t ;


/*  =========================  Prototypes  =========================  */

extern	void	LogErrorMessage(/* msg, status */) ;
extern	void	PopupMessage(/* msg */) ;

extern	void	FinalCleanupCB(/* w, client, call */) ;

extern	int		FontHeight(/* w */) ;
extern	int		FontWidth(/* w */) ;	

/*  =========================  Global Variables  =========================  */

extern	Widget		TopLevel ;
extern	resource_t	Resources ;

/*  =========================  Macros  =========================  */

#define	MIN(A,B)			(A < B ? A :B)
#define	MAX(A,B)			(A > B ? A :B)

#define	DEBUG_MODE			(Resources.debug == True)
#define	SOUND_ON			(Resources.soundOn == True)
#define	BROWSE_MODE			(Resources.browseMode == True)
#define	UPDATE_MODE			(Resources.browseMode == False)
#define	FIRST_IS_CURRENT	(Resources.firstLog == True)
#define	LAST_IS_CURRENT		(Resources.firstLog == False)

#ifdef COMMENT
/* -- Changed by M. Tankenson 10aug93 */
#define	LOG_MESSAGE(M)		LogErrorMessage(M, NormalLogEntry)
#define	LOG_ERROR(M)		LogErrorMessage(M, WarningLogEntry)
#define	LOG_DISASTER(M)		LogErrorMessage(M, CriticalLogEntry)
#endif /* COMMENT */

#define	LOG_MESSAGE(M)		LogErrorMessage(M)
#define	LOG_ERROR(M)		LogErrorMessage(M)
#define	LOG_DISASTER(M)		LogErrorMessage(M)

#define	FINAL_CLEANUP(X)	FinalCleanupCB(NULL, X, NULL)

#endif	/*  LOG_MAIN_H  */
/*  --- Do not add anything below this line ---  */

