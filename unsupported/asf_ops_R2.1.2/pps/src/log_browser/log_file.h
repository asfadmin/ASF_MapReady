
#ifndef	LOG_FILE_H
#define	LOG_FILE_H

#pragma ident "@(#)log_file.h	1.1  12/19/96"

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_file.h
Type:           Local header
Title:			Provides the interface to the log file related routines.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92        Bill West      Initial Delivery
------------------------------------------------------------------------------

Description:	This provides the interface to the log file related routines.

*************************************************************************** */
#endif



/*  =========================  Includes  =========================  */

#include <stdio.h>

/*  =========================  Defines  =========================  */

#define	MAX_PATH_NAME_LEN	200
#define	MAX_LOG_NAME_LEN	100

#define	MAX_FILE_NAME_LEN	(MAX_PATH_NAME_LEN+MAX_LOG_NAME_LEN)


/*  =========================  Types  =========================  */

/*  =========================  Global Variables  =========================  */

extern	FILE	*Log ;
extern	char	 LogPath[] ;
extern	char	 LogName[] ;
extern	char	 LogSpec[] ;


/*  =========================  Prototypes  =========================  */

extern	void	 SetLogResources() ;
extern	Boolean	 GetCurrentLog() ;

extern	void	 CreateLogPopup(/* parent */) ;
extern	void	 PopupLog(/* w, client, call */) ;

extern	Boolean	 OpenLog() ;
extern	Boolean	 LogHasDataPending() ;
extern	Boolean	 ReadLog(/*  entry  */) ;
extern	void	 CloseLog() ;

/*  =========================  Macros  =========================  */

#define	LOG_SELECTED	(Log!=NULL)
#define	NO_LOG			(Log==NULL)

#endif	/*  LOG_FILE_H  */
/*  --- Do not add anything below this line ---  */

