
/*******************************************************************************
       vc_conrndup.h
       This header file is included by vc_conrndup.c

*******************************************************************************/

#ifndef	_VC_CONRNDUP_INCLUDED
#define	_VC_CONRNDUP_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef	struct
{
	Widget	UxConRoundupForm;
	Widget	Uxlabel36;
	Widget	Uxlabel58;
	Widget	Uxlabel61;
	Widget	UxTF_CON_RND_STRTTIME;
	Widget	UxTF_CON_RND_STOPTIME;
	Widget	UxsubMenu_conRnd_stnid;
	Widget	UxsubMenu_conRnd_stnid_asf;
	Widget	UxoptionMenu_conRnd_stnid;
	Widget	Uxpb_ConRnd_OK;
	Widget	Uxpb_ConRnd_QUIT;
	Widget	Uxpb_ConRnd_CLEAR;
	swidget	UxUxParent;
} _UxCConRoundupForm;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCConRoundupForm      *UxConRoundupFormContext;
#define ConRoundupForm          UxConRoundupFormContext->UxConRoundupForm
#define label36                 UxConRoundupFormContext->Uxlabel36
#define label58                 UxConRoundupFormContext->Uxlabel58
#define label61                 UxConRoundupFormContext->Uxlabel61
#define TF_CON_RND_STRTTIME     UxConRoundupFormContext->UxTF_CON_RND_STRTTIME
#define TF_CON_RND_STOPTIME     UxConRoundupFormContext->UxTF_CON_RND_STOPTIME
#define subMenu_conRnd_stnid    UxConRoundupFormContext->UxsubMenu_conRnd_stnid
#define subMenu_conRnd_stnid_asf UxConRoundupFormContext->UxsubMenu_conRnd_stnid_asf
#define optionMenu_conRnd_stnid UxConRoundupFormContext->UxoptionMenu_conRnd_stnid
#define pb_ConRnd_OK            UxConRoundupFormContext->Uxpb_ConRnd_OK
#define pb_ConRnd_QUIT          UxConRoundupFormContext->Uxpb_ConRnd_QUIT
#define pb_ConRnd_CLEAR         UxConRoundupFormContext->Uxpb_ConRnd_CLEAR
#define UxParent                UxConRoundupFormContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_ConRoundupForm( swidget _UxUxParent );

#endif	/* _VC_CONRNDUP_INCLUDED */
