
/* @(#)log_alert.h	1.6 93/07/08 17:46:09  */

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_alert.h
Type:           Local header
Title:
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
07/08/93		Bill West		Added RegisterAlert call.
------------------------------------------------------------------------------

Description:    Interface to constants and routines needed to handle
				alerting the user of critical log entries.

*************************************************************************** */
#endif


#ifndef	LOG_ALERT_H
#define	LOG_ALERT_H


/*  ===========================  PROTOTYPES  =========================== */

extern	void    CreateAlertPopup(/*  Widget parent  */) ;
extern	void	RegisterAlert(/* char *alert */) ;
extern  void    PopupAlert() ;

#endif	/*  LOG_ALERT_H  */
