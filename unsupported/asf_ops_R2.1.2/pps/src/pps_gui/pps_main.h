
/*******************************************************************************
       pps_main.h
       This header file is included by pps_main.c

*******************************************************************************/

#ifndef	_PPS_MAIN_INCLUDED
#define	_PPS_MAIN_INCLUDED

#include <stdio.h>
#include "UxLib.h"

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
	swidget	Uxf_main;
	swidget	Uxl_main_bear;
	swidget	Uxl_main_title;
	swidget	Uxpb_main_query;
	swidget	Uxpb_main_plan;
	swidget	Uxpb_main_policy;
	swidget	Uxmb_main;
	swidget	Uxpb_main_file;
	swidget	Uxpb_main_print_screen;
	swidget	Uxpb_main_exit;
	swidget	Uxmb_main_file;
	swidget	Uxlabel1;
	swidget	UxUxParent;
} _UxCpps_main;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCpps_main            *UxPps_mainContext;
#define f_main                  UxPps_mainContext->Uxf_main
#define l_main_bear             UxPps_mainContext->Uxl_main_bear
#define l_main_title            UxPps_mainContext->Uxl_main_title
#define pb_main_query           UxPps_mainContext->Uxpb_main_query
#define pb_main_plan            UxPps_mainContext->Uxpb_main_plan
#define pb_main_policy          UxPps_mainContext->Uxpb_main_policy
#define mb_main                 UxPps_mainContext->Uxmb_main
#define pb_main_file            UxPps_mainContext->Uxpb_main_file
#define pb_main_print_screen    UxPps_mainContext->Uxpb_main_print_screen
#define pb_main_exit            UxPps_mainContext->Uxpb_main_exit
#define mb_main_file            UxPps_mainContext->Uxmb_main_file
#define label1                  UxPps_mainContext->Uxlabel1
#define UxParent                UxPps_mainContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern swidget	pps_main;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

swidget	create_pps_main( swidget _UxUxParent );

#endif	/* _PPS_MAIN_INCLUDED */
