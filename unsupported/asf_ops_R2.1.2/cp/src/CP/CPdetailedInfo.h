
/*******************************************************************************
       CPdetailedInfo.h
       (Generated from interface file CPdetailedInfo.i)
       This header file is included by CPdetailedInfo.c

*******************************************************************************/

#ifndef	_CPDETAILEDINFO_INCLUDED
#define	_CPDETAILEDINFO_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
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
	Widget	Uxmenu1;
	Widget	Uxmenu1_p1;
	Widget	Uxmenu1_p1_b1;
	Widget	Uxmenu1_top_b1;
	Widget	UxDetailedJob_RC;
	Widget	Uxlabel1;
	Widget	Uxlabel2;
	Widget	UxCPdecodedFiles_label;
	Widget	UxCPproductFiles_label;
	swidget	UxUxParent;
} _UxCCPdetailedInfo;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCPdetailedInfo      *UxCPdetailedInfoContext;
#define menu1                   UxCPdetailedInfoContext->Uxmenu1
#define menu1_p1                UxCPdetailedInfoContext->Uxmenu1_p1
#define menu1_p1_b1             UxCPdetailedInfoContext->Uxmenu1_p1_b1
#define menu1_top_b1            UxCPdetailedInfoContext->Uxmenu1_top_b1
#define DetailedJob_RC          UxCPdetailedInfoContext->UxDetailedJob_RC
#define label1                  UxCPdetailedInfoContext->Uxlabel1
#define label2                  UxCPdetailedInfoContext->Uxlabel2
#define CPdecodedFiles_label    UxCPdetailedInfoContext->UxCPdecodedFiles_label
#define CPproductFiles_label    UxCPdetailedInfoContext->UxCPproductFiles_label
#define UxParent                UxCPdetailedInfoContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	CPdetailedInfo;
extern Widget	CPdetailedMainWindow;
extern Widget	CPdetailedForm;
extern Widget	CPdetailedJobId;
extern Widget	CPdetailedSubsys;
extern Widget	CPdetailedScrolledWin;
extern Widget	CPdetailedOdlText;
extern Widget	CPdetailedInputFiles;
extern Widget	CPdetailedOutputFiles;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CPdetailedInfo();

#endif	/* _CPDETAILEDINFO_INCLUDED */
