
/*******************************************************************************
       ims_opLogin.h
       This header file is included by ims_opLogin.c

*******************************************************************************/

#ifndef	_IMS_OPLOGIN_INCLUDED
#define	_IMS_OPLOGIN_INCLUDED

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
	Widget	Uxlogin;
	Widget	Uxlabel1;
	Widget	Uxlabel2;
	Widget	UxuserIdText;
	Widget	UxpasswdText;
	Widget	Uxlabel3;
	Widget	Uxlabel206;
	Widget	Uxseparator34;
	Widget	UxokPB;
	Widget	UxcancelPB;
	Widget	Uxlabel150;
	swidget	UxUxParent;
} _UxClogin;

#ifdef CONTEXT_MACRO_ACCESS
static _UxClogin               *UxLoginContext;
#define login                   UxLoginContext->Uxlogin
#define label1                  UxLoginContext->Uxlabel1
#define label2                  UxLoginContext->Uxlabel2
#define userIdText              UxLoginContext->UxuserIdText
#define passwdText              UxLoginContext->UxpasswdText
#define label3                  UxLoginContext->Uxlabel3
#define label206                UxLoginContext->Uxlabel206
#define separator34             UxLoginContext->Uxseparator34
#define okPB                    UxLoginContext->UxokPB
#define cancelPB                UxLoginContext->UxcancelPB
#define label150                UxLoginContext->Uxlabel150
#define UxParent                UxLoginContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_login( swidget _UxUxParent );

#endif	/* _IMS_OPLOGIN_INCLUDED */
