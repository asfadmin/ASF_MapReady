
#ifndef	LOG_LIST_H
#pragma ident "@(#)log_list.h	1.1  12/19/96"

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_list.h
Type:           Local header
Title:			Log List Header
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Description:	Provides the constants, prototypes and macros for all
				Scrolled List related functions in the log browser.

*************************************************************************** */
#endif

#define	LOG_LIST_H


/*  =========================  Includes  =========================  */


/*  =========================  Defines  =========================  */


/*  =========================  Types  =========================  */


/*  =========================  Prototypes  =========================  */

extern	void	CreateList(/* parent, sibling */) ;

extern	void	FillList() ;

extern	Boolean	EntriesSelected(/*  first, last  */) ;

extern	int		TotalEntries() ;

extern	void	ShowListBottom() ;

extern	void	SetListUpdateTimer() ;

extern	void	UnsetListUpdateTimer() ;

/*  =========================  Macros  =========================  */


#endif	/*  LOG_LIST_H  */
/*  --- Do not add anything below this line ---  */

