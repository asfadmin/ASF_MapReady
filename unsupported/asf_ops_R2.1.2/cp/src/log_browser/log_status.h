
/* @(#)log_status.h	1.5 93/05/06 15:39:03  */

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_status.h
Type:           Local header
Title:			Status Bar interface.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Description:	Provides interface to functions which create and manage the 
				Status Bar, which in turn displays labels which evince the 
				current state of various aspects of the program's idiom.


*************************************************************************** */
#endif

#ifndef	LOG_STATUS_H
#define	LOG_STATUS_H


/*  =========================  Includes  =========================  */


/*  =========================  Defines  =========================  */


/*  =========================  Types  =========================  */


/*  =========================  Prototypes  =========================  */

extern	Widget	CreateStatusBar(/* parent */) ;

extern	void	UpdateStatusBar() ;


/*  =========================  Macros  =========================  */


#endif	/*  LOG_STATUS_H  */
/*  --- Do not add anything below this line ---  */

