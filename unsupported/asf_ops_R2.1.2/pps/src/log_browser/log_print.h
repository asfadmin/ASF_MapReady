
#pragma ident "@(#)log_print.h	1.1  12/19/96"

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_print.h
Type:           Local header
Title:			Interface to print-related routines.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Description:	This provides the constants and prototypes to be used for
				sending log files to the printer.

*************************************************************************** */
#endif

#ifndef	LOG_PRINT_H
#define	LOG_PRINT_H


/*  =========================  Includes  =========================  */


/*  =========================  Defines  =========================  */

#define	PRINT_ALL	0
#define	PRINT_PART	1


/*  =========================  Types  =========================  */


/*  =========================  Prototypes  =========================  */

extern	void	CreatePrintPopup(/* parent */) ;
extern	void	PopupPrint(/* w, client, call */) ;


/*  =========================  Macros  =========================  */


#endif	/*  LOG_PRINT_H  */
/*  --- Do not add anything below this line ---  */

