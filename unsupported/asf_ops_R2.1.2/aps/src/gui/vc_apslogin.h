
/*******************************************************************************
       vc_apslogin.h
       This header file is included by vc_apslogin.c

*******************************************************************************/

#ifndef	_VC_APSLOGIN_INCLUDED
#define	_VC_APSLOGIN_INCLUDED

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
	Widget	UxAPSLoginLabel1;
	Widget	UxAPSLoginLabel2;
	Widget	UxLoginLabel;
	Widget	UxPasswordLabel;
	Widget	UxDatabaseLabel;
	Widget	UxOk_PushButton;
	Widget	UxReset_PushButton;
	Widget	UxExit_PushButton;
	Widget	UxAPSLoginAsterisk;
	Widget	UxlabelCopyright_line1;
	swidget	UxUxParent;
} _UxCAPSLoginForm;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAPSLoginForm        *UxAPSLoginFormContext;
#define APSLoginLabel1          UxAPSLoginFormContext->UxAPSLoginLabel1
#define APSLoginLabel2          UxAPSLoginFormContext->UxAPSLoginLabel2
#define LoginLabel              UxAPSLoginFormContext->UxLoginLabel
#define PasswordLabel           UxAPSLoginFormContext->UxPasswordLabel
#define DatabaseLabel           UxAPSLoginFormContext->UxDatabaseLabel
#define Ok_PushButton           UxAPSLoginFormContext->UxOk_PushButton
#define Reset_PushButton        UxAPSLoginFormContext->UxReset_PushButton
#define Exit_PushButton         UxAPSLoginFormContext->UxExit_PushButton
#define APSLoginAsterisk        UxAPSLoginFormContext->UxAPSLoginAsterisk
#define labelCopyright_line1    UxAPSLoginFormContext->UxlabelCopyright_line1
#define UxParent                UxAPSLoginFormContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	APSLoginForm;
extern Widget	LoginTextField;
extern Widget	PasswordTextField;
extern Widget	DatabaseTextField;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_APSLoginForm( swidget _UxUxParent );

#endif	/* _VC_APSLOGIN_INCLUDED */
