
/*******************************************************************************
       ims_opSrchSelDlg.h
       This header file is included by ims_opSrchSelDlg.c

*******************************************************************************/

#ifndef	_IMS_OPSRCHSELDLG_INCLUDED
#define	_IMS_OPSRCHSELDLG_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
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
	Widget	UxsrchSelDlg;
	Widget	UxfilterForm;
	Widget	UxfilterLB;
	Widget	UxfilterTF;
	swidget	UxUxParent;
} _UxCsrchSelDlg;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCsrchSelDlg          *UxSrchSelDlgContext;
#define srchSelDlg              UxSrchSelDlgContext->UxsrchSelDlg
#define filterForm              UxSrchSelDlgContext->UxfilterForm
#define filterLB                UxSrchSelDlgContext->UxfilterLB
#define filterTF                UxSrchSelDlgContext->UxfilterTF
#define UxParent                UxSrchSelDlgContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_srchSelDlg( swidget _UxUxParent );

#endif	/* _IMS_OPSRCHSELDLG_INCLUDED */
