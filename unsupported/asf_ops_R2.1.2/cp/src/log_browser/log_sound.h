
/* @(#)log_sound.h	1.5 93/05/06 15:39:03  */

#ifndef	LOG_SOUND_H
#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_sound.h
Type:           Local header
Title:			Sound interface.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Description:	This provides the interface to the sound-related functions.

*************************************************************************** */
#endif

#define	LOG_SOUND_H


/*  =========================  Includes  =========================  */


/*  =========================  Defines  =========================  */

#define POP_UP_SOUND	(-1)


/*  =========================  Types  =========================  */


/*  =========================  Prototypes  =========================  */

extern	void	CreateSoundPopup(/* parent */) ;

extern	void	PopupSound(/* w, reason, call */) ;

extern	void	Beep(/* w */) ;


/*  =========================  Macros  =========================  */


#endif	/*  LOG_SOUND_H  */
/*  --- Do not add anything below this line ---  */

