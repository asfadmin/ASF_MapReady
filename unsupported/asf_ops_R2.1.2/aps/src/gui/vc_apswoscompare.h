
/*******************************************************************************
       vc_apswoscompare.h
       This header file is included by vc_apswoscompare.c

*******************************************************************************/

#ifndef	_VC_APSWOSCOMPARE_INCLUDED
#define	_VC_APSWOSCOMPARE_INCLUDED

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
	Widget	Uxlabel73;
	Widget	UxtextField1;
	Widget	Uxlabel76;
	Widget	UxpushButton_OK;
	Widget	UxpushButton_QUIT;
	Widget	UxscrolledWindowText10;
	Widget	UxscrolledText_woscompare;
	Widget	Uxseparator14;
	Widget	Uxlabel74;
	swidget	UxUxParent;
} _UxCAPSWOSCompare;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAPSWOSCompare       *UxAPSWOSCompareContext;
#define label73                 UxAPSWOSCompareContext->Uxlabel73
#define textField1              UxAPSWOSCompareContext->UxtextField1
#define label76                 UxAPSWOSCompareContext->Uxlabel76
#define pushButton_OK           UxAPSWOSCompareContext->UxpushButton_OK
#define pushButton_QUIT         UxAPSWOSCompareContext->UxpushButton_QUIT
#define scrolledWindowText10    UxAPSWOSCompareContext->UxscrolledWindowText10
#define scrolledText_woscompare UxAPSWOSCompareContext->UxscrolledText_woscompare
#define separator14             UxAPSWOSCompareContext->Uxseparator14
#define label74                 UxAPSWOSCompareContext->Uxlabel74
#define UxParent                UxAPSWOSCompareContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	APSWOSCompare;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_APSWOSCompare( swidget _UxUxParent );

#endif	/* _VC_APSWOSCOMPARE_INCLUDED */
