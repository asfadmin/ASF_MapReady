#ifndef GUI_UTILS_H
#define GUI_UTILS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	gui_utils.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)gui_utils.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.gui_utils.h"

typedef enum
{
	AG_TEXT,
	AG_TEXTFIELD
} AG_widget_type ;

char *	gui_trimstring(char *) ;

char *	gui_TF_string(Widget widget) ;

char *	gui_optionMenu_string(Widget widget) ;

void	gui_setEditable( Widget widgetName,
			AG_widget_type widgetType, Boolean editability ) ;

int		gui_XmList_selected_item(Widget widget) ;

void	gui_display_message_widget(Widget scrolledTextWidget, char *text) ;

char *	gui_filebox_filename(
			Widget widget,  XmFileSelectionBoxCallbackStruct *cbs) ;

int		gui_ask_file_replacement(char *file)  ;

void	TimeoutCursors(
			Boolean on, Boolean interruptable, Widget window) ;

int		gui_AddToTopLevelInterfaces(Widget widget) ;

Widget	gui_GetShellWidget( Widget widget ) ;

int		AskUser( Widget parent, char *qtext, int default_ans) ;

void	gui_aps_internal_error( int dialog_type, char *source_file,
			int line_number, char *error_message ) ;

Boolean	CheckForInterrupt();

void	DisplayXCursor(Boolean on);


#define ASK_OK		1
#define YES			2
#define NO			3

#define MAX_TOPLEVEL_WIDGETS 20

extern char question[] ;
extern char blank_char ;
extern char blank_str[] ;

#endif /* GUI_UTILS_H */
