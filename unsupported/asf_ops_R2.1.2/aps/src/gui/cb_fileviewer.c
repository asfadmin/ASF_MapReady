#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_fileviewer.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)cb_fileviewer.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_fileviewer.c"

#include <stdio.h>
#include <sys/stat.h>

#include <Xm/Text.h>
#include <Xm/TextF.h>

#include "UxXt.h"

#define CONTEXT_MACRO_ACCESS 1
#include "vc_fileviewer.h"
#undef CONTEXT_MACRO_ACCESS

extern void		popup_message() ;

extern Widget	File_viewer ;
extern char		display_string[] ;


/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Nadia Adhami

Creation Date:	12/12/1994

Notes:		
==============================================================================*/
static void
init_FileViewer( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData;
	int		*UxCallbackArg;

{
	_UxCFileViewer          *UxSaveCtx ;

	UxSaveCtx = UxFileViewerContext;
	UxFileViewerContext = 
			(_UxCFileViewer *) UxGetContext( UxWidget );
	{
		FILE    *fp ;
		struct stat statb ;
		char *text, *filename ;
		char *buf ;
		int i,j, buf_size;

		filename = (char *) UxClientData ;

        if (!(fp = fopen(filename, "r")))
		{
			(void) sprintf( display_string,
				 "Unable to open file %s\n", filename ) ;
			popup_message(XmDIALOG_ERROR, "APS:FILE ERROR",
				 display_string, XtGrabNone) ;
			*UxCallbackArg =  -1;
			return ;
		}

		/* make sure the file is a "regular" file (might be binary) */
		if (stat(filename, &statb) == -1 
		|| (statb.st_mode & S_IFMT) != S_IFREG )
		{
			(void) fclose( fp ) ;
			if ((statb.st_mode & S_IFMT) == S_IFREG)
			{
				(void) sprintf(display_string,
					"Filename: %s\nCannot be read", filename) ;
			}
			else
			{
				(void) sprintf (display_string, 
					"Filename: %s\n    is not a regular file\n", filename) ;
			}

			popup_message(XmDIALOG_ERROR, "APS:FILE ERROR",
				display_string, XtGrabNone) ;
			*UxCallbackArg = -1;
			return ;
		}

		/*allocate memory for a new line character per line
		and a null teminator*/
		buf_size = (unsigned)statb.st_size + 
			((unsigned)statb.st_size / 80) /*'\n' per line*/
			+ 1   /*'\n' for last line*/
			+ 1 ; /*null terminator*/

#ifdef DEBUG
		(void) printf("statb.st_size=%d buf_size = %d\n", 
			statb.st_size, buf_size);
#endif

		if (!(buf = XtMalloc (buf_size)))
		{
			(void) sprintf(display_string, 
				"Can't alloc enough space for %s", filename) ;
			popup_message(XmDIALOG_ERROR, "APS:FILE ERROR",
				display_string, XtGrabNone) ;
			*UxCallbackArg = -1;
			(void) fclose (fp) ;
			return ;
		}

		/* put the contents of the file in the Text widget by allocating
		* enough space for the entire file, reading the file into the
		* allocated space, and using XmTextFieldSetString() to show the 
		* file.
		*/
		if (!(text = XtMalloc ((unsigned)(statb.st_size + 1))))
		{
			(void) sprintf(display_string, 
				"Can't alloc enough space for %s", filename) ;
			popup_message(XmDIALOG_ERROR, "APS:FILE ERROR",
				display_string, XtGrabNone) ;
			XtFree ( buf ) ;
			(void) fclose (fp) ;
			return ;
		}

		if (!fread (text, sizeof (char), statb.st_size + 1, fp))
		{
			if (ferror( fp ))
			{
				(void) fprintf (stderr,
						"Warning: may not have read entire file!\n") ;
			}
			else
			{
				(void) fprintf(stderr,
						"Warning: Viewing an empty file\n") ;
			}
		}

		text[statb.st_size] = 0 ; /* be sure to NULL-terminate */

		/*
		-- Check if a newline is in the file...
		-- if so the file has its own carriage control
		-- therefore we don't have to manually place 
		-- new lines in every N columns
		*/
		if (strchr(text, '\n'))
		{
			/* insert file contents in Text widget */
			XmTextSetString (scrolledText_messages, text) ;
		}
		else
		{
			/*copy text into buffer*/
			for ( i=0,j=0 ; j+80<statb.st_size ; i+=81,j+=80 )
			{
				(void) memcpy(buf+i, text+j, 80);
				buf[i+80] = '\n';
			}
			/*copy the last line*/
			(void) memcpy(buf+i, text+j, statb.st_size - j);

#ifdef DEBUG
			(void) printf("last i=%d j=%d statb.st_size-j=%d\n",
				i,j,statb.st_size - j);
#endif

			/*make sure last line is null terminated*/
			buf[buf_size - 1] = '\0';

			XmTextSetString (scrolledText_messages, buf) ;
		}

		/* set the cursor to the beginning of the text chap.15.4.1*/
		XmTextShowPosition(scrolledText_messages, 1) ;

		*UxCallbackArg = 0;

		(void) fclose ( fp ) ;
		XtFree(buf);
		XtFree( text ) ;
	}

	UxFileViewerContext = UxSaveCtx;
}



/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Nadia Adhami

Creation Date:	12/xx/1994

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_FileViewer_popup( UxWidget, UxClientData, call_data )
	Widget UxWidget ;
	XtPointer UxClientData;
	XmListCallbackStruct *call_data ;
{
	char	*filename;
	int	status;

	filename = (char*) XmTextFieldGetString((Widget)UxClientData) ;
	if (!filename || (filename[0] == '\0'))
		popup_message(XmDIALOG_ERROR, "APS:FILE ERROR",
			"Select a file first.", XtGrabNone) ;
	else
	{
		init_FileViewer(File_viewer, filename, &status) ;
		if (  status != 0 )
			return;
		XtPopup(XtParent(File_viewer), XtGrabNone) ;
	}
}
 

