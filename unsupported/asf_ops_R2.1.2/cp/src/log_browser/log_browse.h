
#ifndef	LOG_BROWSE_H
#define	LOG_BROWSE_H

/* @(#)log_browse.h	1.5 93/05/06 15:38:57  */

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_browse.h
Type:           Local header
Title:			Browse mode toggle interface.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Description:	Provides the access to the browse mode toggle for the 
				application, including creation, popping it up, and
				setting the state.

*************************************************************************** */
#endif


/*  =========================  Includes  =========================  */


/*  =========================  Defines  =========================  */

#define POP_UP_BROWSE  (-1)


/*  =========================  Types  =========================  */


/*  =========================  Prototypes  =========================  */

extern	void	CreateBrowseModePopup(/* parent */) ;

extern	void	PopupBrowseMode(/* w, reason, call */) ;

/*  =========================  Macros  =========================  */

#endif	/*  LOG_BROWSE_H  */
/*  --- Do not add anything below this line ---  */

