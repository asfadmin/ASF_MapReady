static char *sccs = "@(#)ims_opDLtoDTKsrchCb.c	1.2  06/27/97";
/*******************************************************************************

  File:     ims_opDLtoDTKsrchCb.c

  Function: Callback functions for downlink (to data-take) search screen

  Author:   Teresa McKillop

  Date:     3/1997

  Revision: 6/11/97 D. Ting
						Made 	createQuery( Widget wgt ) and dl_executeQuery( Widget wgt ) globals

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <UxXt.h>

#define _IMS_OP_DLTODTKSRCHCB_C
#include "ims_opCb.h"
#include <ims_timeConv.h>

#define CONTEXT_MACRO_ACCESS 1
#include <ims_opDLtoDTKsrch.h>
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       Defines, and Global variables
*******************************************************************************/
#define	MIN_REV						1
#define	MAX_REV						99999
#define	MIN_SEQUENCE			1
#define	MAX_SEQUENCE			89

extern OP_GLOBAL_DATA			glbData;
extern Widget							dl2dtkPB ;

/* NOTE: storage needs to be allocated for array of toggles */
static OP_CHECKBOX_TOGGLE	*platform_toggleA					= NULL ;
static OP_CHECKBOX_TOGGLE	*sensor_toggleA						= NULL ;
static OP_CHECKBOX_TOGGLE	*activity_id_toggleA			= NULL ;
static OP_CHECKBOX_TOGGLE	*station_id_toggleA				= NULL ;
static OP_CHECKBOX_TOGGLE	*antenna_id_toggleA				= NULL ;
static OP_CHECKBOX_TOGGLE	*transmitter_id_toggleA		= NULL ;
static OP_CHECKBOX_TOGGLE	*downlink_status_toggleA	= NULL ;

static int								platform_toggleCount ;
static int								sensor_toggleCount ;
static int								activity_id_toggleCount ;
static int								station_id_toggleCount ;
static int								antenna_id_toggleCount ;
static int								transmitter_id_toggleCount ;
static int								downlink_status_toggleCount ;


/*******************************************************************************
       Callback functions.
*******************************************************************************/
void dl_search_create_checkbox_togglesCb( Widget, XtPointer, XtPointer ) ;
void dl_search_time_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
void dl_search_time_loseFocusCb( Widget, XtPointer, XtPointer ) ;
void dl_search_rev_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
void dl_search_rev_loseFocusCb( Widget, XtPointer, XtPointer ) ;
void dl_search_sequence_modifyVerifyCb( Widget, XtPointer, XtPointer ) ;
void dl_search_sequence_loseFocusCb( Widget, XtPointer, XtPointer ) ;
void dl_search_executeCb( Widget, XtPointer, XtPointer ) ;
void dl_search_clearCb( Widget, XtPointer, XtPointer ) ;
void dl_search_closeCb( Widget, XtPointer, XtPointer ) ;
void dl_search_printScreenCb( Widget, XtPointer, XtPointer ) ;
void dl_search_goto_welcomeCb( Widget, XtPointer, XtPointer ) ;


/*******************************************************************************
       Local functions.
*******************************************************************************/
static int	is_rev_in_range( int ) ;
static void	validate_rev( char *, char * ) ;
static int	is_sequence_in_range( int ) ;
static void	validate_sequence( char *, char * ) ;
int	createQuery( Widget wgt ) ;
int	dl_executeQuery( Widget wgt ) ;


/*===========================================================================*
**
** Function Name:	dl_search_create_checkbox_togglesCb
**
** Description:		create toggles for the following checkboxes:
**									platform, sensor, activity id, station id, antenna id,
**									and transmitter id.
**
** Arguments:			wgt -	Widget that is calling this callback
**								cd     -	not used
**								cbs    -	not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED1 */
void
dl_search_create_checkbox_togglesCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cbs )
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	XmFontList				fontList;
	Dimension					indicatorSize;
	Pixel							selectColor;
	int								i ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/* Get resources from dummyTB, then unmanage dummyTB */
		XtVaGetValues(dummyTB,
						XmNfontList, &fontList,
						XmNindicatorSize, &indicatorSize,
						XmNselectColor, &selectColor,
						NULL);
		XtUnmanageChild (dummyTB);

		if (platform_toggleA == NULL && (platform_toggleA =
				malloc( glbData.platform_count * sizeof(OP_CHECKBOX_TOGGLE) )) == NULL)
		{
			/* stderr msg & EXIT */
			glbData.msgDesc->stderrFlag = 1 ;
			ims_msg( glbData.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;

			exit (IMS_FATAL) ;
			return ;
		}

		for (i = 0 ; i < glbData.platform_count ; i++)
		{
			platform_toggleA[i].toggle_w =
					XtVaCreateManagedWidget( glbData.platform[i].value,
							xmToggleButtonGadgetClass, platformRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL );

			(void) strcpy (platform_toggleA[i].name,
							glbData.platform[i].value);
		}
		platform_toggleCount = glbData.platform_count ;

		if (sensor_toggleA == NULL && (sensor_toggleA =
				malloc( glbData.sensor_count * sizeof(OP_CHECKBOX_TOGGLE) )) == NULL)
		{
			/* stderr msg & EXIT */
			glbData.msgDesc->stderrFlag = 1 ;
			ims_msg( glbData.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;

			exit (IMS_FATAL) ;
			return ;
		}

		for (i = 0 ; i < glbData.sensor_count ; i++)
		{
			sensor_toggleA[i].toggle_w =
					XtVaCreateManagedWidget( glbData.sensor[i].value,
							xmToggleButtonGadgetClass, sensorRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL );

			(void) strcpy (sensor_toggleA[i].name,
							glbData.sensor[i].value);
		}
		sensor_toggleCount = glbData.sensor_count ;

		if (activity_id_toggleA == NULL && (activity_id_toggleA =
				malloc( glbData.activity_id_count * sizeof(OP_CHECKBOX_TOGGLE) ))
				== NULL)
		{
			/* stderr msg & EXIT */
			glbData.msgDesc->stderrFlag = 1 ;
			ims_msg( glbData.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;

			exit (IMS_FATAL) ;
			return ;
		}

		for (i = 0 ; i < glbData.activity_id_count ; i++)
		{
			activity_id_toggleA[i].toggle_w =
					XtVaCreateManagedWidget( glbData.activity_id[i].value,
							xmToggleButtonGadgetClass, activityRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL );

			(void) strcpy (activity_id_toggleA[i].name,
							glbData.activity_id[i].value);
		}
		activity_id_toggleCount = glbData.activity_id_count ;

		if (station_id_toggleA == NULL && (station_id_toggleA =
				malloc( glbData.station_id_count * sizeof(OP_CHECKBOX_TOGGLE) ))
				== NULL)
		{
			/* stderr msg & EXIT */
			glbData.msgDesc->stderrFlag = 1 ;
			ims_msg( glbData.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;

			exit (IMS_FATAL) ;
			return ;
		}

		for (i = 0 ; i < glbData.station_id_count ; i++)
		{
			station_id_toggleA[i].toggle_w =
					XtVaCreateManagedWidget( glbData.station_id[i].value,
							xmToggleButtonGadgetClass, stationRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL );

			(void) strcpy (station_id_toggleA[i].name,
							glbData.station_id[i].value);
		}
		station_id_toggleCount = glbData.station_id_count ;

		if (antenna_id_toggleA == NULL && (antenna_id_toggleA =
				malloc( glbData.antenna_id_count * sizeof(OP_CHECKBOX_TOGGLE) ))
				== NULL)
		{
			/* stderr msg & EXIT */
			glbData.msgDesc->stderrFlag = 1 ;
			ims_msg( glbData.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;

			exit (IMS_FATAL) ;
			return ;
		}

		for (i = 0 ; i < glbData.antenna_id_count ; i++)
		{
			antenna_id_toggleA[i].toggle_w =
					XtVaCreateManagedWidget( glbData.antenna_id[i].value,
							xmToggleButtonGadgetClass, antennaRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL );

			(void) strcpy (antenna_id_toggleA[i].name,
							glbData.antenna_id[i].value);
		}
		antenna_id_toggleCount = glbData.antenna_id_count ;

		if (transmitter_id_toggleA == NULL && (transmitter_id_toggleA =
				malloc( glbData.transmitter_id_count * sizeof(OP_CHECKBOX_TOGGLE) ))
				== NULL)
		{
			/* stderr msg & EXIT */
			glbData.msgDesc->stderrFlag = 1 ;
			ims_msg( glbData.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;

			exit (IMS_FATAL) ;
			return ;
		}

		for (i = 0 ; i < glbData.transmitter_id_count ; i++)
		{
			transmitter_id_toggleA[i].toggle_w =
					XtVaCreateManagedWidget( glbData.transmitter_id[i].value,
							xmToggleButtonGadgetClass, transmitterRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL );

			(void) strcpy (transmitter_id_toggleA[i].name,
							glbData.transmitter_id[i].value);
		}
		transmitter_id_toggleCount = glbData.transmitter_id_count ;

		if (downlink_status_toggleA == NULL && (downlink_status_toggleA =
				malloc( glbData.downlink_status_count * sizeof(OP_CHECKBOX_TOGGLE) ))
				== NULL)
		{
			/* stderr msg & EXIT */
			glbData.msgDesc->stderrFlag = 1 ;
			ims_msg( glbData.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;

			exit (IMS_FATAL) ;
			return ;
		}

		for (i = 0 ; i < glbData.downlink_status_count ; i++)
		{
			downlink_status_toggleA[i].toggle_w =
					XtVaCreateManagedWidget( glbData.downlink_status[i].value,
							xmToggleButtonGadgetClass, statusRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL );

			(void) strcpy (downlink_status_toggleA[i].name,
							glbData.downlink_status[i].value);
		}
		downlink_status_toggleCount = glbData.downlink_status_count ;
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl_search_time_modifyVerifyCb
**
** Description:		Callback when a time text widget is modified:
**								removes itself and sets up the lose focus callback.
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- client data: used to match callback in list
**								3. cb 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED2 */
void
dl_search_time_modifyVerifyCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb )
{
	_UxCDLtoDTKsearch						*UxSaveCtx ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		XtRemoveCallback( wgt, XmNmodifyVerifyCallback,
			(XtCallbackProc) dl_search_time_modifyVerifyCb, cd ) ;

		XtAddCallback( wgt, XmNlosingFocusCallback,
			(XtCallbackProc) dl_search_time_loseFocusCb, cd ) ;
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return;
}


/*===========================================================================*
**
** Function Name: dl_search_time_loseFocusCb
**
** Description:		Time validation function: this is called whenever
**								the focus is moved away from a time text widget.
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- client data: used to match callback in list
**								3. cb 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED2 */
void
dl_search_time_loseFocusCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb )
{
	_UxCDLtoDTKsearch						*UxSaveCtx ;

	char												*textValue;
	char												Msg[IMS_COL1024_LEN+1];

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/*
		** Adjust the callback routines
		*/

		XtRemoveCallback( wgt, XmNlosingFocusCallback,
			(XtCallbackProc) dl_search_time_loseFocusCb, cd ) ;

		XtAddCallback( wgt, XmNmodifyVerifyCallback,
			(XtCallbackProc) dl_search_time_modifyVerifyCb, cd ) ;


		/*
		** Verify the time text
		*/

		textValue = XmTextGetString( wgt );

		if (isTimeFieldValid( textValue, NULL ) < IMS_OK)
		{
			/* Display error messages */
			(void) sprintf(Msg, "Invalid Time: \"%s\"!", textValue);
			msgBoxDlg_popupCb (glbData.dlSearchW, IMS_ERROR, Msg);

			XtFree(textValue);
			return;
		}

		XtFree(textValue);
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return;
}


/*===========================================================================*
**
** Function Name: is_rev_in_range
**
** Description:		checks a numeric rev to see if it is in range.
**
** Arguments:			1. rev	- the rev
**
** Return Value:	IMS_TRUE if is in range, otherwise IMS_FALSE
**
** Revision History:
**
**==========================================================================*/
static int
is_rev_in_range( int	rev )
{
	if (rev < MIN_REV || rev > MAX_REV)
		return (IMS_FALSE) ;

	return (IMS_TRUE) ;
}


/*===========================================================================*
**
** Function Name: validate_rev
**
** Description:		rev string validation function: verifies that each
**								character is an integer and that the resultant numeric
**								rev is within range.
**
**								NOTE: an empty string is illegal here.
**
** Arguments:			1. revStr	- the rev string
**								2. Msg		- buffer for storing messages (or NULL)
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
static void
validate_rev(
	char	*revStr,
	char	*Msg )
{
	int									inError = IMS_FALSE ;
	char								*ptr ;

	if (Msg)
		*Msg = '\0' ;

	if (!revStr)
	{
		if (Msg)
		{
			(void) strcat( Msg, "Invalid Rev: <null>" ) ;
		}
		inError = IMS_TRUE ;
	}

	/* check for valid characters */
	if (inError != IMS_TRUE)
	{
		for (ptr = revStr ; *ptr && isdigit( *ptr ) ; ptr++ )
			;	/* deliberately empty block */

		if (*ptr)		/* ERROR: non-digit character */
		{
			if (Msg)
			{
				(void) sprintf( Msg, "Invalid Rev: \"%s\"!\n(non-digit character)",
						revStr );
			}
			inError = IMS_TRUE ;
		}
	}

	/* check if the value is in range */
	if (inError != IMS_TRUE)
	{
		if (is_rev_in_range( atoi( revStr ) ) == IMS_FALSE)
		{
			if (Msg)
			{
				(void) sprintf( Msg,
						"Out-of-range Rev: \"%s\"!\n(range is %d thru %d)",
						revStr, MIN_REV, MAX_REV );
			}
			inError = IMS_TRUE ;
		}
	}

	return;
}


/*===========================================================================*
**
** Function Name: dl_search_rev_modifyVerifyCb
**
** Description:		Callback when a rev text widget is modified:
**								removes itself and sets up the lose focus callback.
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- client data: used to match callback in list
**								3. cb 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED2 */
void
dl_search_rev_modifyVerifyCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb )
{
	_UxCDLtoDTKsearch		*UxSaveCtx ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		XtRemoveCallback( wgt, XmNmodifyVerifyCallback,
			(XtCallbackProc) dl_search_rev_modifyVerifyCb, cd ) ;

		XtAddCallback( wgt, XmNlosingFocusCallback,
			(XtCallbackProc) dl_search_rev_loseFocusCb, cd ) ;
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return;
}


/*===========================================================================*
**
** Function Name: dl_search_rev_loseFocusCb
**
** Description:		Rev validation function: this is called whenever
**								the focus is moved away from a rev text widget.
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- client data: used to match callback in list
**								3. cb 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED2 */
void
dl_search_rev_loseFocusCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb )
{
	_UxCDLtoDTKsearch		*UxSaveCtx ;

	char								*revStr ;
	char								Msg[IMS_COL1024_LEN+1];

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/*
		** Adjust the callback routines
		*/

		XtRemoveCallback( wgt, XmNlosingFocusCallback,
			(XtCallbackProc) dl_search_rev_loseFocusCb, cd ) ;

		XtAddCallback( wgt, XmNmodifyVerifyCallback,
			(XtCallbackProc) dl_search_rev_modifyVerifyCb, cd ) ;


		/*
		** Validate the rev string
		*/

		revStr = XmTextGetString( wgt );
		ims_truncStr (revStr);

		/* empty string is ok */
		if (*revStr == '\0')
		{
			XtFree( revStr );
			return ;
		}

		Msg[0] = '\0';
		validate_rev( revStr, Msg ) ;

		if (Msg[0])		/* if there are any messages */
		{
			/* Display error message */
			msgBoxDlg_popupCb (glbData.dlSearchW, IMS_ERROR, Msg);
		}

		XtFree( revStr );
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return;
}


/*===========================================================================*
**
** Function Name: dl_search_sequence_modifyVerifyCb
**
** Description:		Callback when a sequence text widget is modified:
**								removes itself and sets up the lose focus callback.
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- client data: used to match callback in list
**								3. cb 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED2 */
void
dl_search_sequence_modifyVerifyCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb )
{
	_UxCDLtoDTKsearch		*UxSaveCtx ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		XtRemoveCallback( wgt, XmNmodifyVerifyCallback,
			(XtCallbackProc) dl_search_sequence_modifyVerifyCb, cd ) ;

		XtAddCallback( wgt, XmNlosingFocusCallback,
			(XtCallbackProc) dl_search_sequence_loseFocusCb, cd ) ;
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return;
}


/*===========================================================================*
**
** Function Name: dl_search_sequence_loseFocusCb
**
** Description:		Rev validation function: this is called whenever
**								the focus is moved away from a sequence text widget.
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- client data: used to match callback in list
**								3. cb 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED2 */
void
dl_search_sequence_loseFocusCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb )
{
	_UxCDLtoDTKsearch		*UxSaveCtx ;

	char								*sequenceStr ;
	char								Msg[IMS_COL1024_LEN+1];

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/*
		** Adjust the callback routines
		*/

		XtRemoveCallback( wgt, XmNlosingFocusCallback,
			(XtCallbackProc) dl_search_sequence_loseFocusCb, cd ) ;

		XtAddCallback( wgt, XmNmodifyVerifyCallback,
			(XtCallbackProc) dl_search_sequence_modifyVerifyCb, cd ) ;


		/*
		** Validate the sequence string
		*/

		sequenceStr = XmTextGetString( wgt );
		ims_truncStr (sequenceStr);

		/* empty string is ok */
		if (*sequenceStr == '\0')
		{
			XtFree( sequenceStr );
			return ;
		}

		Msg[0] = '\0';
		validate_sequence( sequenceStr, Msg ) ;

		if (Msg[0])		/* if there are any messages */
		{
			/* Display error message */
			msgBoxDlg_popupCb (glbData.dlSearchW, IMS_ERROR, Msg);
		}

		XtFree( sequenceStr );
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return;
}


/*===========================================================================*
**
** Function Name: is_sequence_in_range
**
** Description:		checks a numeric sequence to see if it is in range.
**
** Arguments:			1. sequence	- the sequence
**
** Return Value:	IMS_TRUE if is in range, otherwise IMS_FALSE
**
** Revision History:
**
**==========================================================================*/
static int
is_sequence_in_range( int	sequence )
{
	if (sequence < MIN_SEQUENCE || sequence > MAX_SEQUENCE)
		return (IMS_FALSE) ;

	return (IMS_TRUE) ;
}


/*===========================================================================*
**
** Function Name: validate_sequence
**
** Description:		sequence string validation function: verifies that each
**								character is an integer and that the resultant numeric
**								sequence is within range.
**
** Arguments:			1. sequenceStr	- the sequence string
**								2. Msg					- buffer for storing messages (or NULL)
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
static void
validate_sequence(
	char	*sequenceStr,
	char	*Msg )
{
	int									inError = IMS_FALSE ;
	char								*ptr ;

	if (Msg)
		*Msg = '\0' ;

	if (!sequenceStr)
	{
		if (Msg)
		{
			(void) strcat( Msg, "Invalid Sequence: <null>" ) ;
		}
		inError = IMS_TRUE ;
	}

	/* check for valid characters */
	if (inError != IMS_TRUE)
	{
		for (ptr = sequenceStr ; *ptr && isdigit( *ptr ) ; ptr++ )
			;	/* deliberately empty block */

		if (*ptr)		/* ERROR: non-digit character */
		{
			if (Msg)
			{
				(void) sprintf( Msg, "Invalid Sequence: \"%s\"!\n(non-digit character)",
						sequenceStr );
			}
			inError = IMS_TRUE ;
		}
	}

	/* check if the value is in range */
	if (inError != IMS_TRUE)
	{
		if (is_sequence_in_range( atoi( sequenceStr ) ) == IMS_FALSE)
		{
			if (Msg)
			{
				(void) sprintf( Msg,
						"Out-of-range Sequence: \"%s\"!\n(range is %d thru %d)",
						sequenceStr, MIN_SEQUENCE, MAX_SEQUENCE );
			}
			inError = IMS_TRUE ;
		}
	}

	return;
}


/*===========================================================================*
**
** Function Name: createQuery
**
** Description:		Create Downlink Search Query
**
** Arguments:			1. wgt - Widget that is calling the original callback
**
** Return Value:	IMS_OK or IMS_FATAL
**
** Revision History:
**
**==========================================================================*/
int
createQuery( Widget wgt )
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	OP_DL2DTK_DATA		*clientData;
	OP_QUERY_STRUCT		*sql;
	IMS_NUMERIC_DATE	*timei ;
	int								firstSetToggle, secondSetToggle ;
	int								i, count;
	char							*sqlPtr;
	char							*str ;
	char							*startText, *endText;
	char							startTime[IMS_DATETIME_LEN+1], endTime[IMS_DATETIME_LEN+1] ;
	char							Msg[IMS_COL1024_LEN+1];

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		clientData = &(glbData.dlClientData);
		sql = &(clientData->queryStruct);

		/*
		** Initialize op_query_struct
		*/
		sql->select[0]	= sql->from[0] = sql->where[0] = sql->sqlBuf[0] = '\0';
		sql->sPtr				= sql->select;
		sql->fPtr				= sql->from;
		sql->wPtr				= sql->where;
		sqlPtr					= sql->sqlBuf;

		(void) strcpy( sql->sPtr,
				"distinct t1.PLATFORM, "
				"t1.SENSOR, "
				"t1.REVOLUTION, "
				"t1.SEQUENCE, "
				"t1.DOWNLINK_STATUS, "
				"t1.ACTIVITY_ID, "
				"t1.STATION_ID, "
				"t1.ANTENNA_ID, "
				"t1.TRANSMITTER_ID, \n"
				"t1.FA_SCHEDULE_LINK, "
				"t1.TIME_ON, "
				"t1.TIME_OFF, "
				"t1.TIME_AOS, "
				"t1.TIME_LOS, "
				"t1.NUMBER_OF_DTK_ENTRY, "
				"convert (char(20), t1.received_time, 100) " ) ;
		sql->sPtr += strlen(sql->sPtr);

		(void) strcpy (sql->fPtr, "downlink_entry t1 ");
		sql->fPtr += strlen(sql->fPtr);


		/*
		** Get start and end revolution texts
		** Need to validate each individually and together
		*/
		startText = XmTextGetString( dlSearchRevStartText );
		ims_truncStr( startText );
		if (*startText)													/* not an empty string */
		{
			Msg[0] = '\0';
			validate_rev( startText, Msg ) ;
			if (*Msg)
			{
				msgBoxDlg_popupCb( glbData.dlSearchW, IMS_FATAL, Msg );

				XtFree( startText );
				return (IMS_FATAL);
			}
		}

		endText = XmTextGetString( dlSearchRevEndText );
		ims_truncStr( endText );
		if (*endText)														/* not an empty string */
		{
			Msg[0] = '\0';
			validate_rev( endText, Msg ) ;
			if (*Msg)
			{
				msgBoxDlg_popupCb( glbData.dlSearchW, IMS_FATAL, Msg );

				XtFree( startText );
				XtFree( endText );
				return (IMS_FATAL);
			}
		}

		if (*startText && *endText)
		{
			if (atoi( startText ) > atoi( endText ))
			{
				/* Display error message */
				(void) sprintf( Msg, "Revolution:\nStart exceeds End!" );
				msgBoxDlg_popupCb( glbData.dlSearchW, IMS_FATAL, Msg );

				XtFree( startText );
				XtFree( endText );
				return (IMS_FATAL);
			}
		}

		if (*startText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.REVOLUTION >= %s", startText );
			sql->wPtr += strlen( sql->wPtr );
		}

		if (*endText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.REVOLUTION <= %s", endText );
			sql->wPtr += strlen( sql->wPtr );
		}

		XtFree( startText );
		XtFree( endText );

		/*
		** Get start and end sequence texts
		** Need to validate each individually and together
		*/
		startText = XmTextGetString( dlSearchSequenceStartText );
		ims_truncStr( startText );
		if (*startText)													/* not an empty string */
		{
			Msg[0] = '\0';
			validate_sequence( startText, Msg ) ;
			if (*Msg)
			{
				msgBoxDlg_popupCb( glbData.dlSearchW, IMS_FATAL, Msg );

				XtFree( startText );
				XtFree( endText );
				return (IMS_FATAL);
			}
		}

		endText = XmTextGetString( dlSearchSequenceEndText );
		ims_truncStr( endText );
		if (*endText)														/* not an empty string */
		{
			Msg[0] = '\0';
			validate_sequence( endText, Msg ) ;
			if (*Msg)
			{
				msgBoxDlg_popupCb( glbData.dlSearchW, IMS_FATAL, Msg );

				XtFree( startText );
				XtFree( endText );
				return (IMS_FATAL);
			}
		}

		if (*startText && *endText)
		{
			if (atoi( startText ) > atoi( endText ))
			{
				/* Display error message */
				(void) sprintf( Msg, "Sequence:\nStart exceeds End!" );
				msgBoxDlg_popupCb( glbData.dlSearchW, IMS_FATAL, Msg );

				XtFree( startText );
				XtFree( endText );
				return (IMS_FATAL);
			}
		}

		if (*startText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.SEQUENCE >= %s", startText );
			sql->wPtr += strlen( sql->wPtr );
		}

		if (*endText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.SEQUENCE <= %s", endText );
			sql->wPtr += strlen( sql->wPtr );
		}

		XtFree( startText );
		XtFree( endText );

		/*
		** Get selected platform toggles
		*/
		for (i = count = 0 ; i < platform_toggleCount ; i++)
		{
			if (XmToggleButtonGetState(platform_toggleA[i].toggle_w))
			{
				switch (++count)
				{
					case 1 :	/* first one, not in sql stmt yet */
						firstSetToggle = i ;

						if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
						{
							(void) strcpy (sql->wPtr, " and\n");
							sql->wPtr += strlen( sql->wPtr );
						}
						break ;

					case 2 :
						secondSetToggle = i ;	/* save this one, not in sql stmt yet */
						break ;

					case 3 :	/* now can set in sql stmt */
						(void) sprintf( sql->wPtr, "t1.PLATFORM in ('%s','%s','%s'",
											platform_toggleA[firstSetToggle].name,
											platform_toggleA[secondSetToggle].name,
											platform_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;

					default :	/* > 3 set toggles */
						(void) sprintf( sql->wPtr, ",'%s'", platform_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;
				}
			}
		}
		switch( count )
		{
			case 0 :
				break ;

			case 1 :	/* platform not yet written to sql stmt */
				(void) sprintf (sql->wPtr, "t1.PLATFORM = '%s' ",
									platform_toggleA[firstSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			case 2 :	/* platforms not yet written to sql stmt */
				(void) sprintf( sql->wPtr, "t1.PLATFORM in ('%s','%s')",
									platform_toggleA[firstSetToggle].name,
									platform_toggleA[secondSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			default :
				(void) strcpy (sql->wPtr, ")");
				sql->wPtr += strlen( sql->wPtr );
				break ;
		}

		/*
		** Get selected sensor toggles
		*/
		for (i = count = 0 ; i < sensor_toggleCount ; i++)
		{
			if (XmToggleButtonGetState(sensor_toggleA[i].toggle_w))
			{
				switch (++count)
				{
					case 1 :	/* first one, not in sql stmt yet */
						firstSetToggle = i ;

						if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
						{
							(void) strcpy (sql->wPtr, " and\n");
							sql->wPtr += strlen( sql->wPtr );
						}
						break ;

					case 2 :
						secondSetToggle = i ;	/* save this one, not in sql stmt yet */
						break ;

					case 3 :	/* now can set in sql stmt */
						(void) sprintf( sql->wPtr, "t1.SENSOR in ('%s','%s','%s'",
											sensor_toggleA[firstSetToggle].name,
											sensor_toggleA[secondSetToggle].name,
											sensor_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;

					default :	/* > 3 set toggles */
						(void) sprintf( sql->wPtr, ",'%s'", sensor_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;
				}
			}
		}
		switch( count )
		{
			case 0 :
				break ;

			case 1 :	/* sensor not yet written to sql stmt */
				(void) sprintf (sql->wPtr, "t1.SENSOR = '%s' ",
									sensor_toggleA[firstSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			case 2 :	/* sensors not yet written to sql stmt */
				(void) sprintf( sql->wPtr, "t1.SENSOR in ('%s','%s')",
									sensor_toggleA[firstSetToggle].name,
									sensor_toggleA[secondSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			default :
				(void) strcpy (sql->wPtr, ")");
				sql->wPtr += strlen( sql->wPtr );
				break ;
		}

		/*
		** Get selected activity_id toggles
		*/
		for (i = count = 0 ; i < activity_id_toggleCount ; i++)
		{
			if (XmToggleButtonGetState(activity_id_toggleA[i].toggle_w))
			{
				switch (++count)
				{
					case 1 :	/* first one, not in sql stmt yet */
						firstSetToggle = i ;

						if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
						{
							(void) strcpy (sql->wPtr, " and\n");
							sql->wPtr += strlen( sql->wPtr );
						}
						break ;

					case 2 :
						secondSetToggle = i ;	/* save this one, not in sql stmt yet */
						break ;

					case 3 :	/* now can set in sql stmt */
						(void) sprintf( sql->wPtr, "t1.ACTIVITY_ID in ('%s','%s','%s'",
											activity_id_toggleA[firstSetToggle].name,
											activity_id_toggleA[secondSetToggle].name,
											activity_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;

					default :	/* > 3 set toggles */
						(void) sprintf( sql->wPtr, ",'%s'", activity_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;
				}
			}
		}
		switch( count )
		{
			case 0 :
				break ;

			case 1 :	/* activity_id not yet written to sql stmt */
				(void) sprintf (sql->wPtr, "t1.ACTIVITY_ID = '%s' ",
									activity_id_toggleA[firstSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			case 2 :	/* activity_ids not yet written to sql stmt */
				(void) sprintf( sql->wPtr, "t1.ACTIVITY_ID in ('%s','%s')",
									activity_id_toggleA[firstSetToggle].name,
									activity_id_toggleA[secondSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			default :
				(void) strcpy (sql->wPtr, ")");
				sql->wPtr += strlen( sql->wPtr );
				break ;
		}

		/*
		** Get selected station_id toggles
		*/
		for (i = count = 0 ; i < station_id_toggleCount ; i++)
		{
			if (XmToggleButtonGetState(station_id_toggleA[i].toggle_w))
			{
				switch (++count)
				{
					case 1 :	/* first one, not in sql stmt yet */
						firstSetToggle = i ;

						if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
						{
							(void) strcpy (sql->wPtr, " and\n");
							sql->wPtr += strlen( sql->wPtr );
						}
						break ;

					case 2 :
						secondSetToggle = i ;	/* save this one, not in sql stmt yet */
						break ;

					case 3 :	/* now can set in sql stmt */
						(void) sprintf( sql->wPtr, "t1.STATION_ID in ('%s','%s','%s'",
											station_id_toggleA[firstSetToggle].name,
											station_id_toggleA[secondSetToggle].name,
											station_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;

					default :	/* > 3 set toggles */
						(void) sprintf( sql->wPtr, ",'%s'", station_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;
				}
			}
		}
		switch( count )
		{
			case 0 :
				break ;

			case 1 :	/* station_id not yet written to sql stmt */
				(void) sprintf (sql->wPtr, "t1.STATION_ID = '%s' ",
									station_id_toggleA[firstSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			case 2 :	/* station_ids not yet written to sql stmt */
				(void) sprintf( sql->wPtr, "t1.STATION_ID in ('%s','%s')",
									station_id_toggleA[firstSetToggle].name,
									station_id_toggleA[secondSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			default :
				(void) strcpy (sql->wPtr, ")");
				sql->wPtr += strlen( sql->wPtr );
				break ;
		}

		/*
		** Get selected antenna_id toggles
		*/
		for (i = count = 0 ; i < antenna_id_toggleCount ; i++)
		{
			if (XmToggleButtonGetState(antenna_id_toggleA[i].toggle_w))
			{
				switch (++count)
				{
					case 1 :	/* first one, not in sql stmt yet */
						firstSetToggle = i ;

						if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
						{
							(void) strcpy (sql->wPtr, " and\n");
							sql->wPtr += strlen( sql->wPtr );
						}
						break ;

					case 2 :
						secondSetToggle = i ;	/* save this one, not in sql stmt yet */
						break ;

					case 3 :	/* now can set in sql stmt */
						(void) sprintf( sql->wPtr, "t1.ANTENNA_ID in ('%s','%s','%s'",
											antenna_id_toggleA[firstSetToggle].name,
											antenna_id_toggleA[secondSetToggle].name,
											antenna_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;

					default :	/* > 3 set toggles */
						(void) sprintf( sql->wPtr, ",'%s'", antenna_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;
				}
			}
		}
		switch( count )
		{
			case 0 :
				break ;

			case 1 :	/* antenna_id not yet written to sql stmt */
				(void) sprintf (sql->wPtr, "t1.ANTENNA_ID = '%s' ",
									antenna_id_toggleA[firstSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			case 2 :	/* antenna_ids not yet written to sql stmt */
				(void) sprintf( sql->wPtr, "t1.ANTENNA_ID in ('%s','%s')",
									antenna_id_toggleA[firstSetToggle].name,
									antenna_id_toggleA[secondSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			default :
				(void) strcpy (sql->wPtr, ")");
				sql->wPtr += strlen( sql->wPtr );
				break ;
		}

		/*
		** Get selected transmitter_id toggles
		*/
		for (i = count = 0 ; i < transmitter_id_toggleCount ; i++)
		{
			if (XmToggleButtonGetState(transmitter_id_toggleA[i].toggle_w))
			{
				switch (++count)
				{
					case 1 :	/* first one, not in sql stmt yet */
						firstSetToggle = i ;

						if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
						{
							(void) strcpy (sql->wPtr, " and\n");
							sql->wPtr += strlen( sql->wPtr );
						}
						break ;

					case 2 :
						secondSetToggle = i ;	/* save this one, not in sql stmt yet */
						break ;

					case 3 :	/* now can set in sql stmt */
						(void) sprintf( sql->wPtr, "t1.TRANSMITTER_ID in ('%s','%s','%s'",
											transmitter_id_toggleA[firstSetToggle].name,
											transmitter_id_toggleA[secondSetToggle].name,
											transmitter_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;

					default :	/* > 3 set toggles */
						(void)sprintf( sql->wPtr, ",'%s'", transmitter_id_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;
				}
			}
		}
		switch( count )
		{
			case 0 :
				break ;

			case 1 :	/* transmitter_id not yet written to sql stmt */
				(void) sprintf (sql->wPtr, "t1.TRANSMITTER_ID = '%s' ",
									transmitter_id_toggleA[firstSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			case 2 :	/* transmitter_ids not yet written to sql stmt */
				(void) sprintf( sql->wPtr, "t1.TRANSMITTER_ID in ('%s','%s')",
									transmitter_id_toggleA[firstSetToggle].name,
									transmitter_id_toggleA[secondSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			default :
				(void) strcpy (sql->wPtr, ")");
				sql->wPtr += strlen( sql->wPtr );
				break ;
		}

		/*
		** Get selected (downlink) status toggles
		*/
		for (i = count = 0 ; i < downlink_status_toggleCount ; i++)
		{
			if (XmToggleButtonGetState(downlink_status_toggleA[i].toggle_w))
			{
				switch (++count)
				{
					case 1 :	/* first one, not in sql stmt yet */
						firstSetToggle = i ;

						if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
						{
							(void) strcpy (sql->wPtr, " and\n");
							sql->wPtr += strlen( sql->wPtr );
						}
						break ;

					case 2 :
						secondSetToggle = i ;	/* save this one, not in sql stmt yet */
						break ;

					case 3 :	/* now can set in sql stmt */
						(void) sprintf( sql->wPtr,
											"t1.DOWNLINK_STATUS in ('%s','%s','%s'",
											downlink_status_toggleA[firstSetToggle].name,
											downlink_status_toggleA[secondSetToggle].name,
											downlink_status_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;

					default :	/* > 3 set toggles */
						(void) sprintf( sql->wPtr, ",'%s'",
											downlink_status_toggleA[i].name );
						sql->wPtr += strlen( sql->wPtr );
						break ;
				}
			}
		}
		switch( count )
		{
			case 0 :
				break ;

			case 1 :	/* status not yet written to sql stmt */
				(void) sprintf (sql->wPtr, "t1.DOWNLINK_STATUS = '%s' ",
									downlink_status_toggleA[firstSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			case 2 :	/* status not yet written to sql stmt */
				(void) sprintf( sql->wPtr, "t1.DOWNLINK_STATUS in ('%s','%s')",
									downlink_status_toggleA[firstSetToggle].name,
									downlink_status_toggleA[secondSetToggle].name ) ;
				sql->wPtr += strlen( sql->wPtr );
				break ;

			default :
				(void) strcpy (sql->wPtr, ")");
				sql->wPtr += strlen( sql->wPtr );
				break ;
		}

		/*
		** Get start and end "time on" texts
		** Need to validate each individually and together
		*/
		if ((timei = malloc( sizeof(IMS_NUMERIC_DATE) )) == NULL)
		{
			(void) sprintf( Msg,
					"Cannot Validate Times:\nInternal Memory allocation problem." );
			msgBoxDlg_popupCb( glbData.dlSearchW, IMS_FATAL, Msg );

			return (IMS_FATAL);
		}

		startText = XmTextGetString( dlSearchTimeOnStartText );
		ims_truncStr( startText );
		if (*startText)														/* not an empty string */
		{
			if (isTimeFieldValid( startText, timei ) < IMS_OK)
			{
				(void) sprintf(Msg, "Invalid Time-On Start: \"%s\"!", startText);
				msgBoxDlg_popupCb (glbData.dlSearchW, IMS_ERROR, Msg);

				XtFree( startText ) ;
				free( timei ) ;
				return (IMS_FATAL);
			}
			(void)sprintf( startTime, "%04u-%03uT%02u:%02u:%02u.%03u",
					timei->year, timei->doy,
					timei->hours, timei->minutes, timei->seconds, timei->msecs ) ;
		}

		endText = XmTextGetString( dlSearchTimeOnEndText );
		ims_truncStr( endText );
		if (*endText)														/* not an empty string */
		{
			if (isTimeFieldValid( endText, timei ) < IMS_OK)
			{
				(void) sprintf(Msg, "Invalid Time-On End: \"%s\"!", endText);
				msgBoxDlg_popupCb (glbData.dlSearchW, IMS_ERROR, Msg);

				XtFree( startText ) ;
				XtFree( endText ) ;
				free( timei ) ;
				return (IMS_FATAL);
			}
			(void)sprintf( endTime, "%04u-%03uT%02u:%02u:%02u.%03u",
					timei->year, timei->doy,
					timei->hours, timei->minutes, timei->seconds, timei->msecs ) ;
		}

		if ( (*startText && *endText) && (strcmp( startTime, endTime ) > 0) )
		{
			(void) sprintf( Msg, "Time On:\nStart exceeds End!" ) ;
			msgBoxDlg_popupCb( glbData.dlSearchW, IMS_ERROR, Msg );

			XtFree( startText ) ;
			XtFree( endText ) ;
			free( timei ) ;
			return (IMS_FATAL);
		}

		if (*startText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.TIME_ON >= '%s'", startTime );
			sql->wPtr += strlen( sql->wPtr );
		}

		if (*endText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.TIME_ON <= '%s'", endTime );
			sql->wPtr += strlen( sql->wPtr );
		}

		XtFree( startText ) ;
		XtFree( endText ) ;

		/*
		** Get start and end "time off" texts
		** Need to validate each individually and together
		*/
		startText = XmTextGetString( dlSearchTimeOffStartText );
		ims_truncStr( startText );
		if (*startText)														/* not an empty string */
		{
			if (isTimeFieldValid( startText, timei ) < IMS_OK)
			{
				(void) sprintf(Msg, "Invalid Time-Off Start: \"%s\"!", startText);
				msgBoxDlg_popupCb (glbData.dlSearchW, IMS_ERROR, Msg);

				XtFree( startText ) ;
				free( timei ) ;
				return (IMS_FATAL);
			}
			(void)sprintf( startTime, "%04u-%03uT%02u:%02u:%02u.%03u",
					timei->year, timei->doy,
					timei->hours, timei->minutes, timei->seconds, timei->msecs ) ;
		}

		endText = XmTextGetString( dlSearchTimeOffEndText );
		ims_truncStr( endText );
		if (*endText)														/* not an empty string */
		{
			if (isTimeFieldValid( endText, timei ) < IMS_OK)
			{
				(void) sprintf(Msg, "Invalid Time-Off End: \"%s\"!", endText);
				msgBoxDlg_popupCb (glbData.dlSearchW, IMS_ERROR, Msg);

				XtFree( startText ) ;
				XtFree( endText ) ;
				free( timei ) ;
				return (IMS_FATAL);
			}
			(void)sprintf( endTime, "%04u-%03uT%02u:%02u:%02u.%03u",
					timei->year, timei->doy,
					timei->hours, timei->minutes, timei->seconds, timei->msecs ) ;
		}

		if ( (*startText && *endText) && (strcmp( startTime, endTime ) > 0) )
		{
			(void) sprintf( Msg, "Time Off:\nStart exceeds End!" ) ;
			msgBoxDlg_popupCb( glbData.dlSearchW, IMS_ERROR, Msg );

			XtFree( startText ) ;
			XtFree( endText ) ;
			free( timei ) ;
			return (IMS_FATAL);
		}

		free( timei ) ;

		if (*startText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.TIME_OFF >= '%s'", startTime );
			sql->wPtr += strlen( sql->wPtr );
		}

		if (*endText)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.TIME_OFF <= '%s'", endTime );
			sql->wPtr += strlen( sql->wPtr );
		}

		/*
		** Get fa schedule link text
		*/
		str = XmTextFieldGetString( dlSearchScheduleLinkTF );
		ims_truncStr( str );

		if (*str)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy( sql->wPtr, " and\n" );
				sql->wPtr += strlen( sql->wPtr );
			}
			(void) sprintf( sql->wPtr, "t1.FA_SCHEDULE_LINK = '%s'", str );
			sql->wPtr += strlen( sql->wPtr );

			XtFree( str );
		}


		/*
		** complete the sql statement
		*/

		if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
		{
			(void) sprintf( sqlPtr, "select %s\nfrom %s\nwhere %s",
								sql->select, sql->from, sql->where );
		}
		else
		{
			(void) sprintf( sqlPtr, "select %s\nfrom %s", sql->select, sql->from );
		}
		sqlPtr += strlen(sqlPtr);
		/* NOTE: sensor is included for backwards compatibility only */
		(void) sprintf( sqlPtr,
								"\norder by t1.PLATFORM,t1.SENSOR,t1.REVOLUTION,t1.SEQUENCE" );
		sqlPtr += strlen(sqlPtr);

	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return (IMS_OK) ;
}


/*===========================================================================*
**
** Function Name: dl_executeQuery
**
** Description:		execute Downlink Search Query
**
** Arguments:			1. wgt - Widget that is calling the original callback
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
int
dl_executeQuery( Widget wgt )
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	OP_DL2DTK_DATA		*clientData;
	OP_CAT_STRUCT			*catReq;
	int								dlCount;
	char							Msg[IMS_COL1024_LEN+1];


	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/*
		** Initialize catalog request structure
		*/
		clientData			= &(glbData.dlClientData);
		catReq					= &(clientData->catReq);
		catReq->item[0]	= (int *)&dlCount;	/* assign tmp storage to item[0] ptr */
		catReq->item[1]	= (char *)clientData->queryStruct.sqlBuf;

		/* free up the current clientData->dlList first */
		free_dlList();

		if (ims_opCat (catReq, OP_GETDOWNLINKLIST) < IMS_OK)
		{
			/* Display error messages */
			(void) sprintf(Msg, "Internal Error: downlink retrieval failed.");
			msgBoxDlg_popupCb (glbData.dlSearchW, IMS_FATAL, Msg);
			return (IMS_FATAL);
		}
		else
		{
			/* assign returned downlink to glbData.dlClientData->dlList */
			clientData->dlCount = *(int *)catReq->item[0];
			clientData->dlList = (OP_DL_LIST *)catReq->item[2];
			/* reset other glbData.dlClientData items */
			clientData->currDL = NULL ;
			clientData->dlWindowTop = 0 ;
			clientData->dtkWindowTop = 0 ;
		}
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return (IMS_OK);
}


/*===========================================================================*
**
** Function Name: dl_search_executeCb
**
** Description:		Execute Downlink Search Function
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- not used
**								3. cbs 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED1 */
void
dl_search_executeCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/* Change cursor to watch cursor */
		timeOutCursors( True );

		if (createQuery( glbData.dlSearchW ) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors( False );

			return;
		}
		else
		{
			if (dl_executeQuery( glbData.dlSearchW ) < IMS_OK)
			{
				/* Change cursor back to normal*/
				timeOutCursors( False );

				return;
			}
			else
			{
				/* Change cursor back to normal*/
				timeOutCursors( False );

				/* popup downlink display screen */
				dl2dtk_displayResults( glbData.orderW ) ;

			}
		}
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl_search_clearCb
**
** Description:		Clear downlink search screen
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- not used
**								3. cbs 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED1 */
void
dl_search_clearCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	int								i;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/* Clear all the text field and text widgets */
		XmTextFieldSetString( dlSearchScheduleLinkTF, "" );
		XmTextSetString( dlSearchRevStartText, "" );
		XmTextSetString( dlSearchRevEndText, "" );
		XmTextSetString( dlSearchSequenceStartText, "" );
		XmTextSetString( dlSearchSequenceEndText, "" );
		XmTextSetString( dlSearchTimeOnStartText, "" );
		XmTextSetString( dlSearchTimeOnEndText, "" );
		XmTextSetString( dlSearchTimeOffStartText, "" );
		XmTextSetString( dlSearchTimeOffEndText, "" );

		/* Deselect all toggles in the checkboxes */
		for( i = 0; i < platform_toggleCount; i++ )
			XmToggleButtonSetState( platform_toggleA[i].toggle_w, False, False );
		for( i = 0; i < sensor_toggleCount; i++ )
			XmToggleButtonSetState( sensor_toggleA[i].toggle_w, False, False );
		for( i = 0; i < activity_id_toggleCount; i++ )
			XmToggleButtonSetState( activity_id_toggleA[i].toggle_w, False, False );
		for( i = 0; i < station_id_toggleCount; i++ )
			XmToggleButtonSetState( station_id_toggleA[i].toggle_w, False, False );
		for( i = 0; i < antenna_id_toggleCount; i++ )
			XmToggleButtonSetState( antenna_id_toggleA[i].toggle_w, False, False );
		for( i = 0; i < transmitter_id_toggleCount; i++ )
			XmToggleButtonSetState( transmitter_id_toggleA[i].toggle_w, False, False);
		for( i = 0; i < downlink_status_toggleCount; i++ )
			XmToggleButtonSetState(downlink_status_toggleA[i].toggle_w, False, False);
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl_search_closeCb
**
** Description:		Exit from the Downlink Search Screen.
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd 	- client data: passed to clear callback
**								3. cbs 	- callback data: passed to clear callback
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
void
dl_search_closeCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	/*
	** data structure cleanup and database
	** close connection must be done here.
	*/
	dl_search_clearCb( glbData.dlSearchW, cd, cb );

	XtPopdown( XtParent( glbData.dlSearchW ) );
	glbData.dlSearchFlag = 0;
}


/*===========================================================================*
**
** Function Name: dl_search_printScreenCb
**
** Description:		print the current downlink search screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED1 */
void
dl_search_printScreenCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		printScreen(glbData.dlSearchW);

		/* Change cursor back to normal */
		timeOutCursors (False);
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl_search_goto_dl2dtkCb
**
** Description:		Pop up the dl2dtk screen from the downlink search screen
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd		- not used
**								3. cbs 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED1 */
void
dl_search_goto_dl2dtkCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		XtPopup( XtParent( glbData.dl2dtkW ), XtGrabNone );
		glbData.dl2dtkFlag = 1;
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl_search_goto_welcomeCb
**
** Description:		Pop up the welcome screen from the downlink search screen
**
** Arguments:			1. wgt	- Widget that is calling this callback
**								2. cd		- not used
**								3. cbs 	- not used
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED1 */
void
dl_search_goto_welcomeCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCDLtoDTKsearch	*UxSaveCtx ;

	UxSaveCtx								= UxDLtoDTKsearchContext;
  UxDLtoDTKsearchContext	= (_UxCDLtoDTKsearch *) UxGetContext( wgt );
	{
		XtPopup( XtParent( glbData.welcomeW ), XtGrabNone );
		glbData.welcomeFlag = 1;
	}
	UxDLtoDTKsearchContext = UxSaveCtx;

	return ;
}
