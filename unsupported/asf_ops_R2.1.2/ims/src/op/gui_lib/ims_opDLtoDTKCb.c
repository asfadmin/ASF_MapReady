static char *sccs = "@(#)ims_opDLtoDTKCb.c	1.4  10/27/97";
/*******************************************************************************

  File:     ims_opDLtoDTKCb.c

  Function: Callback functions for downlink to data-take screen

  Author:   Teresa McKillop

  Date:     4/1997

  Revision:

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include <Xm/List.h>

#include <UxXt.h>

#define _IMS_OP_DLTODTKCB_C
#include "ims_opCb.h"

#define CONTEXT_MACRO_ACCESS 1
#include <ims_opDLtoDTK.h>
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       Defines, and Global variables
*******************************************************************************/
extern OP_GLOBAL_DATA	glbData;

static int						dlRows       = -1 ;	/* # visible rows in DL lists */
static int						dtkRows      = -1 ;	/* # visible rows in DTK lists */

/* list# of 1st DL on screen (list starts at 0) */
static int						dlWindowTop  = 0;
static OP_DL_LIST			*dlWinTopPtr ;			/* ptr to the dlWindowTop DL */

/* list# of 1st DTK on screen (list starts at 0) */
static int						dtkWindowTop = 0;
static OP_DL_DTK_LIST	*dtkWinTopPtr ;			/* ptr to the dtkWindowTop DTK */

static int						numSelectedDTKs = 0 ;	/* # currently selected DTKs */

static Boolean				procFootnoteOn = False ;	/*shows if footnote is visible*/

/*
** 5/97 NOTE: since proc auth flag is the only changeable field and it
**	can only be changed from "NO" to "YES",
**	design is to change the field in the list of DTKs AND to keep
**	track of which DTKs have been changed, via an array of flags
**	in glbData; the value in the array of flags is the index into
**	the following array of the current setting for the proc auth flag.
**	(Alternate design would have been to keep a list of the original
**	DTKs or proc auth flags and restore the original as needed)
*/
static char						*procAuthValues[] = { "NO", "YES" } ;


/*******************************************************************************
       Callback functions.
*******************************************************************************/
void dl2dtk_scroll_dlListsCb( Widget, XtPointer, XmScrollBarCallbackStruct * ) ;
void dl2dtk_dlLists_selectionCb( Widget, XtPointer, XmListCallbackStruct * );
void dl2dtk_displayResults( Widget ) ;
void dl2dtk_view_DL_detailsCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_scroll_dtkListsCb( Widget, XtPointer, XmScrollBarCallbackStruct * );
void dl2dtk_dtkLists_selectionCb( Widget, XtPointer, XmListCallbackStruct * );
void dl2dtk_show_downlinkDTKsCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_view_DTK_detailsCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_reset_dtkCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_toggle_proc_auth_flagCb( Widget, XtPointer, XmListCallbackStruct *);
void dl2dtk_updateCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_refreshSearchCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_closeCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_printScreenCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_goto_searchCb( Widget, XtPointer, XtPointer ) ;
void dl2dtk_goto_welcomeCb( Widget, XtPointer, XtPointer ) ;


/*******************************************************************************
       Callable functions.
*******************************************************************************/
void free_dlList() ;


/*******************************************************************************
       Local functions.
*******************************************************************************/
static void 		setDLbuttons( Boolean ) ;
static void 		setDTKselectButtons( Boolean ) ;
static void 		setDTKupdateButtons( Boolean ) ;
static Boolean	procAuthFlagIsChangeable( OP_DL_LIST *, char * ) ;
static void			free_dtkList( OP_DL_DTK_LIST * ) ;
static void			resetDTK( OP_DL_DTK_LIST *, int ) ;
static int			intcmp( const void *, const void * ) ;


/*===========================================================================*
**
** Function Name: setDLbuttons
**
** Description:		Sets the sensitivity of the downlink buttons (both screen
**								and menu bar) to the proper state.
**
** Arguments:			1. SetFlag	- True if buttons are to be set for DL active
**														- False if buttons are to be reset.
**
** Return Value: 	None
**
** Revision History: 6/11/97 D. Ting
**									 dl2dtk_refreshSearchCb added , so make it sensitive
**
**==========================================================================*/
static void
setDLbuttons( Boolean SetFlag )
{
	XtSetSensitive( viewDownlinkDetailsMPB,	SetFlag ) ;
	XtSetSensitive( viewDTKsMPB,						SetFlag ) ;

	/*XtSetSensitive( refreshSearchMPB,					SetFlag ) ;*/
	XtSetSensitive( dl2dtkRefreshSearchPB,		SetFlag ) ;

	return ;
}


/*===========================================================================*
**
** Function Name: setDTKselectButtons
**
** Description:		Sets the sensitivity of the DTK buttons.
**
** Arguments:			1. SetFlag	- True if buttons are to be set for DTK active
**														- False if buttons are to be reset.
**
** Return Value: 	None
**
** Revision History:
**
**==========================================================================*/
static void
setDTKselectButtons( Boolean SetFlag )
{
	XtSetSensitive( toggleProcAuthFlagMPB,	SetFlag ) ;
	XtSetSensitive( resetDTK_MPB,						SetFlag ) ;

	if (!SetFlag || numSelectedDTKs == 1)
		XtSetSensitive( viewDTKDetailsMPB,			SetFlag ) ;
	else if (numSelectedDTKs > 1)	/* SetFlag is True, want to desensitize */
		XtSetSensitive( viewDTKDetailsMPB,			!SetFlag ) ;

	return ;
}


/*===========================================================================*
**
** Function Name: setDTKupdateButtons
**
** Description:		Sets the sensitivity of the DTK buttons (both screen and
**								menu bar).
**
** Arguments:			1. SetFlag	- True if buttons are to be set for DTK active
**														- False if buttons are to be reset.
**
** Return Value: 	None
**
** Revision History:
**
**==========================================================================*/
static void
setDTKupdateButtons( Boolean SetFlag )
{
	XtSetSensitive( saveChangesMPB,					SetFlag ) ;
	XtSetSensitive( dl2dtkUpdatePB,					SetFlag ) ;

	return ;
}


/*===========================================================================*
**
** Function Name: procAuthFlagIsChangeable
**
** Description:		Determines if a DTK's process_auth_flag may be
**								changed/toggled.
**								(5/97: is changeable if the DL status is "ACQUIRED" and
**								the DTK process_auth_flag is "NO".)
**
** Arguments:			1. DL -	the DL to which the DTK belongs
** 								2. procAuthFlag	- setting of the DTK's procAuthFlag
**
** Return Value: 	True, if may be changed, otherwise False.
**
** Revision History:
**
**==========================================================================*/
static Boolean
procAuthFlagIsChangeable( OP_DL_LIST *DL, char *procAuthFlag )
{
	int					retVal = True ;

	if (strcmp( procAuthFlag, "NO" ) || strcmp( DL->downlink_status, "ACQUIRED" ))
		retVal = False ;

	return (retVal) ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_scroll_dlListsCb
**
** Description:		Updates the Downlink list widgets when the user scrolls.
**								Scrolling method is application-defined.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStruct
**
** Return Value: 	None
**
** Revision History: 6/11/97 D. Ting
**									 Added total number downlinks count.
**
**==========================================================================*/
void
dl2dtk_scroll_dlListsCb(
	Widget widget,
	XtPointer cd,
	XmScrollBarCallbackStruct *cbs)
{
	_UxCDLtoDTK			*UxSaveCtx ;
	OP_DL2DTK_DATA	*clientData;
	OP_DL_LIST			*dlPtr;
	int							dlCount, count ;
	int							highLightPos=0;	/* row in list which is currently selected */
	int							i, temp;
	Widget					sbar;
	XmStringTable		platformStr, sensorStr, revolutionStr, sequenceStr,
									activityStr, stationStr, antennaStr,
									timeonStr, timeoffStr, dlstatusStr, numDTKsStr ;
	char						buffer[IMS_COL255_LEN+1];

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( glbData.dl2dtkW );
	{
		/* assign client to dlClientData from glbData structure */
		clientData							= &(glbData.dlClientData);
		dlPtr										= clientData->dlList;
		dlCount									= clientData->dlCount;
		clientData->dlWindowTop	= dlWindowTop = cbs->value;

		count = dlCount - dlWindowTop;
		if (dlRows < 0)
			XtVaGetValues( dlPlatformLIST, XmNvisibleItemCount, &dlRows, NULL );
		if (count > dlRows)
			count = dlRows;

		/* If no rows to display, clean up DL lists and desensitize push buttons */
		if ((dlCount == 0) || (dlPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems ( dlPlatformLIST ) ;
			XmListDeleteAllItems ( dlSensorLIST ) ;
			XmListDeleteAllItems ( dlRevLIST ) ;
			XmListDeleteAllItems ( dlSequenceLIST ) ;
			XmListDeleteAllItems ( dlActivityLIST ) ;
			XmListDeleteAllItems ( dlStationLIST ) ;
			XmListDeleteAllItems ( dlAntennaLIST ) ;
			XmListDeleteAllItems ( dlTimeOnLIST ) ;
			XmListDeleteAllItems ( dlTimeOffLIST ) ;
			XmListDeleteAllItems ( dlStatusLIST ) ;
			XmListDeleteAllItems ( dlNumDTKsLIST ) ;
			XmListDeleteAllItems ( dlDummyLIST ) ;
		  XmTextFieldSetString (dlTotalDlksTF, "0"); 
			setDLbuttons( True ) ;
	    XtSetSensitive( refreshSearchMPB,					True ) ;

			return;
		}
		 /* need to display total number of dlks in textfield widget */ 
		sprintf (buffer, "%d", dlCount);
		XmTextFieldSetString (dlTotalDlksTF, buffer);


		/* Allocate memory for Motif Compound Strings */
		platformStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		sensorStr			= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		revolutionStr	= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		sequenceStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		activityStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		stationStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		antennaStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		timeonStr			= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		timeoffStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		dlstatusStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		numDTKsStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;

		/* get the DL that should be at the top based on the scrollbar position */
		while ((dlPtr != NULL) && (dlPtr->position != dlWindowTop))
			dlPtr = dlPtr->next;
		dlWinTopPtr = dlPtr ;

		/* set up all the lists' string arrays */
		for (i = 0; i < count && dlPtr != NULL; i++, dlPtr = dlPtr->next)
		{
			if (dlPtr->platform)
				platformStr[i] = XmStringCreateLocalized( dlPtr->platform );
			else
				platformStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->sensor)
				sensorStr[i] = XmStringCreateLocalized( dlPtr->sensor );
			else
				sensorStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->revolution >= 0)
			{
				(void) sprintf( buffer, "%d", dlPtr->revolution );
				revolutionStr[i] = XmStringCreateLocalized( buffer );
			}
			else
				revolutionStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->sequence >= 0)
			{
				(void) sprintf( buffer, "%d", dlPtr->sequence );
				sequenceStr[i] = XmStringCreateLocalized( buffer );
			}
			else
				sequenceStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->activity_id)
				activityStr[i] = XmStringCreateLocalized( dlPtr->activity_id );
			else
				activityStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->station_id)
				stationStr[i] = XmStringCreateLocalized( dlPtr->station_id );
			else
				stationStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->antenna_id)
				antennaStr[i] = XmStringCreateLocalized( dlPtr->antenna_id );
			else
				antennaStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->time_on)
				timeonStr[i] = XmStringCreateLocalized( dlPtr->time_on );
			else
				timeonStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->time_off)
				timeoffStr[i] = XmStringCreateLocalized( dlPtr->time_off );
			else
				timeoffStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->downlink_status)
				dlstatusStr[i] = XmStringCreateLocalized( dlPtr->downlink_status );
			else
				dlstatusStr[i] = XmStringCreateLocalized( "" );

			if (dlPtr->number_of_dtk_entry >= 0)
			{
				(void) sprintf( buffer, "%d", dlPtr->number_of_dtk_entry );
				numDTKsStr[i] = XmStringCreateLocalized( buffer );
			}
			else
				numDTKsStr[i] = XmStringCreateLocalized( "" );


			if (dlPtr->selectFlag)
				highLightPos = i+1;
		}

		/* Load all the synchronized arrays into the DL list widgets */
		XtVaSetValues(
				dlPlatformLIST, XmNitems, platformStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlSensorLIST, XmNitems, sensorStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlRevLIST, XmNitems, revolutionStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlSequenceLIST, XmNitems, sequenceStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlActivityLIST, XmNitems, activityStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlStationLIST, XmNitems, stationStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlAntennaLIST, XmNitems, antennaStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlTimeOnLIST, XmNitems, timeonStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlTimeOffLIST, XmNitems, timeoffStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlStatusLIST, XmNitems, dlstatusStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlNumDTKsLIST, XmNitems, numDTKsStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dlDummyLIST, XmNitems, sensorStr, XmNitemCount, count, NULL);


		/* Free compound strings */
		for (i = 0 ; i < count ; i++)
		{
			XmStringFree( platformStr[i] ) ;
			XmStringFree( sensorStr[i] ) ;
			XmStringFree( revolutionStr[i] ) ;
			XmStringFree( sequenceStr[i] ) ;
			XmStringFree( activityStr[i] ) ;
			XmStringFree( stationStr[i] ) ;
			XmStringFree( antennaStr[i] ) ;
			XmStringFree( timeonStr[i] ) ;
			XmStringFree( timeoffStr[i] ) ;
			XmStringFree( dlstatusStr[i] ) ;
			XmStringFree( numDTKsStr[i] ) ;
		}

		XtFree( (char *) platformStr ) ;
		XtFree( (char *) sensorStr ) ;
		XtFree( (char *) revolutionStr ) ;
		XtFree( (char *) sequenceStr ) ;
		XtFree( (char *) activityStr ) ;
		XtFree( (char *) stationStr ) ;
		XtFree( (char *) antennaStr ) ;
		XtFree( (char *) timeonStr ) ;
		XtFree( (char *) timeoffStr ) ;
		XtFree( (char *) dlstatusStr ) ;
		XtFree( (char *) numDTKsStr ) ;


		/* Update Scrollbar position */
		XtVaGetValues( dlDummySW, XmNverticalScrollBar, &sbar, NULL );

		temp = dlCount ;

		XtVaSetValues(sbar,
			XmNmaximum, temp,
			XmNvalue, dlWindowTop,
			NULL);

		/*
		** reselect the row that was highlighted on entry to here
		*/

		XmListDeselectAllItems( dlPlatformLIST ) ;
		XmListDeselectAllItems( dlSensorLIST ) ;
		XmListDeselectAllItems( dlRevLIST ) ;
		XmListDeselectAllItems( dlSequenceLIST ) ;
		XmListDeselectAllItems( dlActivityLIST ) ;
		XmListDeselectAllItems( dlStationLIST ) ;
		XmListDeselectAllItems( dlAntennaLIST ) ;
		XmListDeselectAllItems( dlTimeOnLIST ) ;
		XmListDeselectAllItems( dlTimeOffLIST ) ;
		XmListDeselectAllItems( dlStatusLIST ) ;
		XmListDeselectAllItems( dlNumDTKsLIST ) ;
		XmListDeselectAllItems( dlDummyLIST ) ;

		if (highLightPos)
		{
			XmListSelectPos( dlPlatformLIST, highLightPos, False ) ;
			XmListSelectPos( dlSensorLIST, highLightPos, False ) ;
			XmListSelectPos( dlRevLIST, highLightPos, False ) ;
			XmListSelectPos( dlSequenceLIST, highLightPos, False ) ;
			XmListSelectPos( dlActivityLIST, highLightPos, False ) ;
			XmListSelectPos( dlStationLIST, highLightPos, False ) ;
			XmListSelectPos( dlAntennaLIST, highLightPos, False ) ;
			XmListSelectPos( dlTimeOnLIST, highLightPos, False ) ;
			XmListSelectPos( dlTimeOffLIST, highLightPos, False ) ;
			XmListSelectPos( dlStatusLIST, highLightPos, False ) ;
			XmListSelectPos( dlNumDTKsLIST, highLightPos, False ) ;
			XmListSelectPos( dlDummyLIST, highLightPos, False ) ;
		}
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_dlLists_selectionCb
**
** Description:		Callback for browse select (first click).  Updates the
**								selection of the downlink in all the downlink
**								list widgets, synchronously and clears out the old DTKs.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. cd 				- not used
**								3. cbs 				- XmListCallbackStruct
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
void
dl2dtk_dlLists_selectionCb(
		Widget widget,
		XtPointer cd,
		XmListCallbackStruct *cbs )
{
	_UxCDLtoDTK						*UxSaveCtx ;
	OP_DL2DTK_DATA				*clientData;
	OP_DL_LIST						*dlPtr;
	OP_DL_LIST						*selPtr;
	OP_DL_LIST						*tempPtr;
	char									Msg[IMS_COL1024_LEN+1];
	int										dlPos ;
	int										i ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		/* Clean up DTK lists and desensitive push buttons */
		XmListDeleteAllItems( dtkPlatformLIST ) ;
		XmListDeleteAllItems( dtkSensorLIST ) ;
		XmListDeleteAllItems( dtkRevLIST ) ;
		XmListDeleteAllItems( dtkSequenceLIST ) ;
		XmListDeleteAllItems( dtkSensorModeLIST ) ;
		XmListDeleteAllItems( dtkTimeOnLIST ) ;
		XmListDeleteAllItems( dtkTimeOffLIST ) ;
		XmListDeleteAllItems( dtkSiteNameLIST ) ;
		XmListDeleteAllItems( dtkFrameModeLIST ) ;
		XmListDeleteAllItems( dtkQuicklookLIST ) ;
		XmListDeleteAllItems( dtkProcAuthLIST );
		XmListDeleteAllItems( dtkDummyLIST ) ;
		dtkWindowTop		= 0;
		dtkWinTopPtr		= NULL ;
		numSelectedDTKs = 0 ;
		setDTKselectButtons( False ) ;
		setDTKupdateButtons( False ) ;

		/* assign client to dlClientData from glbData structure */
		clientData	= &(glbData.dlClientData);
		dlPtr				= clientData->dlList ;
		dlPos				= cbs->item_position - 1 ;	/* decrement for its "C" value */

  	/* Locate the selected downlink */
		selPtr = dlWinTopPtr;
		for (i = 0 ; i < dlPos && selPtr != NULL ; i++)
			selPtr = selPtr->next ;
		if (selPtr == NULL)
		{
			/* Should never get here: No DL is selected, error message, return */
			(void) sprintf( Msg, "Internal Error in Selecting a Downlink." ) ;
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_ERROR, Msg ) ;

			clientData->currDL = NULL ;
			return;
		}

		/*
		** if there was a different DL selected and there are some
		** unsaved changes, inform the user
		*/
		if (!(selPtr->selectFlag) && (tempPtr = clientData->currDL) != NULL)
		{
			/* Note: selPtr is the newly selected DL; tempPtr is the previous one */

			int		*dtkChanged ;

			if ((dtkChanged = tempPtr->dtkChanged) != NULL)
			{
				for (i = 0 ; i < tempPtr->number_of_dtk_entry ; i++, dtkChanged++)
					if (*dtkChanged) break ;

				if (i < tempPtr->number_of_dtk_entry)
				{
					(void) sprintf( Msg,
							"*** Some changes were made, but NOT SAVED ***\n\n"
							"    to the downlink:\n"
							"    platform:  %s    sensor:  %s    rev:  %d   seq:  %d\n\n"
							"To save the changes (or undo them), re-Select that downlink",
							tempPtr->platform, tempPtr->sensor,
							tempPtr->revolution, tempPtr->sequence ) ;
					msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_INFO, Msg ) ;
				}
			}
		}

		XmListDeselectAllItems( dlPlatformLIST ) ;
		XmListDeselectAllItems( dlSensorLIST ) ;
		XmListDeselectAllItems( dlRevLIST ) ;
		XmListDeselectAllItems( dlSequenceLIST ) ;
		XmListDeselectAllItems( dlActivityLIST ) ;
		XmListDeselectAllItems( dlStationLIST ) ;
		XmListDeselectAllItems( dlAntennaLIST ) ;
		XmListDeselectAllItems( dlTimeOnLIST ) ;
		XmListDeselectAllItems( dlTimeOffLIST ) ;
		XmListDeselectAllItems( dlStatusLIST ) ;
		XmListDeselectAllItems( dlNumDTKsLIST ) ;
		XmListDeselectAllItems( dlDummyLIST ) ;

		/*
		** Reset the selectFlag for each item in the dlList
		*/
		for ( tempPtr = dlPtr ; tempPtr != NULL ; tempPtr = tempPtr->next)
			tempPtr->selectFlag = 0;

		/* set the selectFlag for the newly selected one */
		selPtr->selectFlag = 1;
		clientData->currDL = selPtr ;

		/* Select this downlink across all downlink list widgets */
		dlPos++ ;	/* increment for its "X" value */
		XmListSelectPos( dlPlatformLIST, dlPos, False  ) ;
		XmListSelectPos( dlSensorLIST, dlPos, False  ) ;
		XmListSelectPos( dlRevLIST, dlPos, False  ) ;
		XmListSelectPos( dlSequenceLIST, dlPos, False  ) ;
		XmListSelectPos( dlActivityLIST, dlPos, False  ) ;
		XmListSelectPos( dlStationLIST, dlPos, False  ) ;
		XmListSelectPos( dlAntennaLIST, dlPos, False  ) ;
		XmListSelectPos( dlTimeOnLIST, dlPos, False  ) ;
		XmListSelectPos( dlTimeOffLIST, dlPos, False  ) ;
		XmListSelectPos( dlStatusLIST, dlPos, False  ) ;
		XmListSelectPos( dlNumDTKsLIST, dlPos, False  ) ;
		XmListSelectPos( dlDummyLIST, dlPos, False  ) ;
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_displayResults
**
** Description:		Sets up the dl2dtk screen with the results from a
**								downlink search.  Function dl_scroll_dlListsCb is called
**								to display results in the downlink portion.
**
** Arguments:			1. widget	- Widget that is calling the original callback
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED0 */
void
dl2dtk_displayResults( Widget widget )
{
	_UxCDLtoDTK								*UxSaveCtx ;
	XmScrollBarCallbackStruct	*cbs;


	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( glbData.dl2dtkW );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/*
		** sensitize all DL menu items
		*/
		setDLbuttons( True ) ;
	  XtSetSensitive( refreshSearchMPB,					True ) ;

		/* call dl2dtk_scroll_dlListsCb to load data into the DL list widgets */
		cbs	= (XmScrollBarCallbackStruct *)
					malloc( sizeof(XmScrollBarCallbackStruct) );
		cbs->value	= 0;
		dl2dtk_scroll_dlListsCb( widget, NULL, cbs );
		free( cbs );

		/* Clean up DTK lists and desensitive push buttons */
		XmListDeleteAllItems( dtkPlatformLIST ) ;
		XmListDeleteAllItems( dtkSensorLIST ) ;
		XmListDeleteAllItems( dtkRevLIST ) ;
		XmListDeleteAllItems( dtkSequenceLIST ) ;
		XmListDeleteAllItems( dtkSensorModeLIST ) ;
		XmListDeleteAllItems( dtkTimeOnLIST ) ;
		XmListDeleteAllItems( dtkTimeOffLIST ) ;
		XmListDeleteAllItems( dtkSiteNameLIST ) ;
		XmListDeleteAllItems( dtkFrameModeLIST ) ;
		XmListDeleteAllItems( dtkQuicklookLIST ) ;
		XmListDeleteAllItems( dtkProcAuthLIST );
		XmListDeleteAllItems( dtkDummyLIST ) ;
		XmTextFieldSetString( dtkTotalDksTF,"");
		dtkWindowTop		= 0;
		dtkWinTopPtr		= NULL ;
		numSelectedDTKs = 0 ;
		setDTKselectButtons( False ) ;
		setDTKupdateButtons( False ) ;

		XtPopup( XtParent(glbData.dl2dtkW), XtGrabNone ) ;
		glbData.dl2dtkFlag = 1 ;

		/* Change cursor back to normal */
		timeOutCursors( False ) ;
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_view_DL_detailsCb
**
** Description:		Display the details of the "double-clicked" DL, ie,
**								this is the "default" callback.
**
** Arguments:			1. widget	- Widget that is calling the original callback
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
dl2dtk_view_DL_detailsCb( Widget widget, XtPointer cd, XtPointer cbs )
{
	_UxCDLtoDTK						*UxSaveCtx ;
	OP_DL2DTK_DATA				*clientData;
	OP_DL_LIST						*dlPtr;					/* current DL on the current screen */
	char									Msg[IMS_COL1024_LEN+1];
	char									*buffer;
	char									*text;
	char									label[IMS_COL30_LEN+1];

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		/* in the list of DLs, find the DL using this callback */
		clientData = &(glbData.dlClientData);

		if ((dlPtr = clientData->currDL) == NULL)
		{
			/* Should never get here: No DL is selected, error message, return */
			(void) sprintf( Msg, "Select a downlink first." ) ;
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_INFO, Msg ) ;
			return;
		}

		if ((buffer = XtMalloc ((unsigned int) 4049)) == (char *)NULL)
		{
			/* display error message, return */
			(void) sprintf( Msg, "Could not allocate space for text buffer." ) ;
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_FATAL, Msg ) ;
			return;
		}

		/* Write downlink information */
		text = buffer;
		(void) sprintf (text,
				"\n    Platform: %2s    Sensor: %s    Rev: %d    Sequence: %d\n\n",
				dlPtr->platform, dlPtr->sensor,
				dlPtr->revolution, (int) dlPtr->sequence ) ;
		text = text + strlen(text);

		(void) sprintf (text, "%s\n\n",
				"====================================================================");
		text = text + strlen(text);

		(void) sprintf (text, "Downlink Status:\t\t\t%s\n\n",
				dlPtr->downlink_status);
		text = text + strlen(text);

		(void) sprintf (text, "Activity ID:\t\t\t%s\n\n", dlPtr->activity_id);
		text = text + strlen(text);

		(void) sprintf (text, "Station ID:\t\t\t%s\n\n", dlPtr->station_id);
		text = text + strlen(text);

		(void) sprintf (text, "antenna ID:\t\t\t%s\n\n", dlPtr->antenna_id);
		text = text + strlen(text);

		(void) sprintf (text, "Transmitter ID:\t\t\t%s\n\n", dlPtr->transmitter_id);
		text = text + strlen(text);

		(void) sprintf (text, "FA Schedule Link:\t\t%s\n\n",
				dlPtr->fa_schedule_link);
		text = text + strlen(text);

		(void) sprintf (text, "Time On:\t\t\t%s\n\n", dlPtr->time_on);
		text = text + strlen(text);

		(void) sprintf (text, "Time Off:\t\t\t%s\n\n", dlPtr->time_off);
		text = text + strlen(text);

		(void) sprintf (text, "Time Acquire Signal (AOS):\t%s\n\n",
				dlPtr->time_aos);
		text = text + strlen(text);

		(void) sprintf (text, "Time Lose Signal (LOS):\t\t%s\n\n", dlPtr->time_los);
		text = text + strlen(text);

		(void) sprintf (text, "Number of DTK Entries:\t\t%d\n\n",
				dlPtr->number_of_dtk_entry);
		text = text + strlen(text);

		(void) sprintf (text, "Downlink Entry Received Time:\t%s\n\n",
				dlPtr->received_time);
		text = text + strlen(text);

		(void) strcpy (label, "Downlink    Detailed    Information");
		browseDlg_popupCb (glbData.dl2dtkW, buffer, label);

		XtFree (buffer);
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_scroll_dtkListsCb
**
** Description:		Updates the DTK list widgets when the user scrolls.
**								Scrolling method is application-defined.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStruct
**
** Return Value: 	None
**
** Revision History: 6/11/97 D. Ting
**									 Initialise procFootnoteOn to False
**									 Added Total number of dtks
**
**==========================================================================*/
void
dl2dtk_scroll_dtkListsCb(
	Widget widget,
	XtPointer cd,
	XmScrollBarCallbackStruct *cbs)
{
	_UxCDLtoDTK			*UxSaveCtx ;
	OP_DL2DTK_DATA	*clientData;
	OP_DL_LIST			*dlPtr ;
	OP_DL_DTK_LIST	*dtkPtr;
	int							dtkCount, count ;
	static int			*highLightPos=NULL;	/* alloc: array of selected rows in list*/
	int							highLightNum = 0 ;
	Widget					sbar;
	XmStringTable		platformStr, sensorStr, revolutionStr, sequenceStr,
									sensormodeStr, timeonStr, timeoffStr, sitenameStr,
									framemodeStr, quicklookStr, procauthStr;
	char						buffer[IMS_COL255_LEN+1];
	int							*changedPtr ;
	int							i, temp;

  procFootnoteOn = False; 
	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( glbData.dl2dtkW );
	{
		/* initialize */
		if (highLightPos == NULL)
		{
			if ((highLightPos = malloc( dtkRows * sizeof(int) )) == NULL)
			{
				(void) sprintf( buffer, "Out of Memory." ) ;
				msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_FATAL, buffer ) ;
				return;
			}
		}

		XtUnmapWidget( ProcFlagFootnote ) ;

		/* assign client to dlClientData from glbData structure */
		clientData								= &(glbData.dlClientData);
		dlPtr											= clientData->currDL ;
		changedPtr								= dlPtr->dtkChanged + dtkWindowTop ;
		changedPtr								= (changedPtr != NULL)
																	? changedPtr + dtkWindowTop : changedPtr ;
		dtkPtr										= dlPtr->dtkList;
		dtkCount									= dlPtr->number_of_dtk_entry;

		clientData->dtkWindowTop	=
		dtkWindowTop							= cbs->value;

		count											= dtkCount - dtkWindowTop;
		if (dtkRows < 0)
			XtVaGetValues( dtkPlatformLIST, XmNvisibleItemCount, &dtkRows, NULL );
		if (count > dtkRows)
			count = dtkRows;

		/* If no rows to display, clean up DTK lists and desensitize push buttons */
		if ((dtkCount == 0) || (dtkPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (dtkPlatformLIST);
			XmListDeleteAllItems (dtkSensorLIST);
			XmListDeleteAllItems (dtkRevLIST);
			XmListDeleteAllItems (dtkSequenceLIST) ;
			XmListDeleteAllItems (dtkSensorModeLIST);
			XmListDeleteAllItems (dtkTimeOnLIST);
			XmListDeleteAllItems (dtkTimeOffLIST);
			XmListDeleteAllItems (dtkSiteNameLIST);
			XmListDeleteAllItems (dtkFrameModeLIST);
			XmListDeleteAllItems (dtkQuicklookLIST);
			XmListDeleteAllItems (dtkProcAuthLIST);
			XmListDeleteAllItems (dtkDummyLIST);
		  XmTextFieldSetString (dtkTotalDksTF, ""); 
			dtkWindowTop		= 0;
			dtkWinTopPtr		= NULL ;
			setDTKupdateButtons( False ) ;

			return;
		}
		 /* need to display total number of dlks in textfield widget */ 
		sprintf (buffer, "%d", dtkCount);
		XmTextFieldSetString (dtkTotalDksTF, buffer);

		/* Allocate memory for Motif Compound Strings */

		platformStr			= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		sensorStr				= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		revolutionStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		sequenceStr			= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		sensormodeStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		timeonStr				= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		timeoffStr			= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		sitenameStr			= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		framemodeStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		quicklookStr		= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;
		procauthStr			= (XmStringTable) XtMalloc( count *sizeof(XmString *) ) ;

		/* get the DTK that should be at the top based on the scrollbar position */
		while ((dtkPtr != NULL) && (dtkPtr->position != dtkWindowTop))
			dtkPtr = dtkPtr->next;
		dtkWinTopPtr = dtkPtr ;

		/* set up all the lists' string arrays */
		for (i = 0 ; i < count && dtkPtr != NULL ; i++, dtkPtr = dtkPtr->next)
		{
			if (dtkPtr->platform)
				platformStr[i] = XmStringCreateLocalized( dtkPtr->platform );
			else
				platformStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->sensor)
				sensorStr[i] = XmStringCreateLocalized( dtkPtr->dt_sensor );
			else
				sensorStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->revolution >= 0)
			{
				(void) sprintf( buffer, "%d", dtkPtr->dt_revolution );
				revolutionStr[i] = XmStringCreateLocalized( buffer );
			}
			else
				revolutionStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->sequence >= 0)
			{
				(void) sprintf( buffer, "%d", dtkPtr->dt_sequence );
				sequenceStr[i] = XmStringCreateLocalized( buffer );
			}
			else
				sequenceStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->mode)
				sensormodeStr[i] = XmStringCreateLocalized( dtkPtr->mode );
			else
				sensormodeStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->time_on)
				timeonStr[i] = XmStringCreateLocalized( dtkPtr->time_on );
			else
				timeonStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->time_off)
				timeoffStr[i] = XmStringCreateLocalized( dtkPtr->time_off );
			else
				timeoffStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->site_name)
				sitenameStr[i] = XmStringCreateLocalized( dtkPtr->site_name );
			else
				sitenameStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->frame_mode)
				framemodeStr[i] = XmStringCreateLocalized( dtkPtr->frame_mode );
			else
				framemodeStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->quicklook)
				quicklookStr[i] = XmStringCreateLocalized( dtkPtr->quicklook );
			else
				quicklookStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->process_auth_flag)
			{
				/* use only the first char of process_auth_flag */
				if ((changedPtr != NULL && *changedPtr) ||
						procAuthFlagIsChangeable( dlPtr, dtkPtr->process_auth_flag ))
				{
					(void) sprintf( buffer, "%c", *dtkPtr->process_auth_flag ) ;
				}
				else
				{
					(void) sprintf( buffer, "%c*", *dtkPtr->process_auth_flag ) ;

					if (!procFootnoteOn)
					{
						procFootnoteOn = True ;
						XtMapWidget( ProcFlagFootnote ) ;
					}
				}
				procauthStr[i] = XmStringCreateLocalized( buffer );
			}
			else
				procauthStr[i] = XmStringCreateLocalized( "" );

			if (dtkPtr->selectFlag)
				highLightPos[highLightNum++] = i+1;

			if (changedPtr != NULL) changedPtr++ ;
		}

		/* Load all the synchronized arrays into the DTK list widgets */
		XtVaSetValues(
				dtkPlatformLIST, XmNitems, platformStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkSensorLIST, XmNitems, sensorStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkRevLIST, XmNitems, revolutionStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkSequenceLIST, XmNitems, sequenceStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkSensorModeLIST, XmNitems, sensormodeStr,
				XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkTimeOnLIST, XmNitems, timeonStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkTimeOffLIST, XmNitems, timeoffStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkSiteNameLIST, XmNitems, sitenameStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkFrameModeLIST, XmNitems, framemodeStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkQuicklookLIST, XmNitems, quicklookStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkProcAuthLIST, XmNitems, procauthStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				dtkDummyLIST, XmNitems, sensorStr, XmNitemCount, count, NULL);


		/* Free compound strings */
		for (i = 0 ; i < count ; i++)
		{
			XmStringFree( platformStr[i] ) ;
			XmStringFree( sensorStr[i] ) ;
			XmStringFree( revolutionStr[i] ) ;
			XmStringFree( sequenceStr[i] ) ;
			XmStringFree( sensormodeStr[i] ) ;
			XmStringFree( timeonStr[i] ) ;
			XmStringFree( timeoffStr[i] ) ;
			XmStringFree( sitenameStr[i] ) ;
			XmStringFree( framemodeStr[i] ) ;
			XmStringFree( quicklookStr[i] ) ;
			XmStringFree( procauthStr[i] ) ;
		}

		XtFree( (char *) platformStr ) ;
		XtFree( (char *) sensorStr ) ;
		XtFree( (char *) revolutionStr ) ;
		XtFree( (char *) sequenceStr ) ;
		XtFree( (char *) sensormodeStr ) ;
		XtFree( (char *) timeonStr ) ;
		XtFree( (char *) timeoffStr ) ;
		XtFree( (char *) sitenameStr ) ;
		XtFree( (char *) framemodeStr ) ;
		XtFree( (char *) quicklookStr ) ;
		XtFree( (char *) procauthStr ) ;


		/* Update Scrollbar position */
		XtVaGetValues( dtkDummySW, XmNverticalScrollBar, &sbar, NULL );

		temp = dtkCount;

		XtVaSetValues(sbar,
			XmNmaximum, temp,
			XmNvalue, dtkWindowTop,
			NULL);

		/*
		** reselect any rows that were highlighted on entry to here
		*/

		XmListDeselectAllItems( dtkPlatformLIST ) ;
		XmListDeselectAllItems( dtkSensorLIST ) ;
		XmListDeselectAllItems( dtkRevLIST ) ;
		XmListDeselectAllItems( dtkSequenceLIST ) ;
		XmListDeselectAllItems( dtkSensorModeLIST ) ;
		XmListDeselectAllItems( dtkTimeOnLIST ) ;
		XmListDeselectAllItems( dtkTimeOffLIST ) ;
		XmListDeselectAllItems( dtkSiteNameLIST ) ;
		XmListDeselectAllItems( dtkFrameModeLIST ) ;
		XmListDeselectAllItems( dtkQuicklookLIST ) ;
		XmListDeselectAllItems( dtkProcAuthLIST ) ;
		XmListDeselectAllItems( dtkDummyLIST ) ;

		if (highLightNum > 1)
		{
			/* have to change selection policy in order to select multiple rows */
			XtVaSetValues( dtkPlatformLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSensorLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkRevLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSequenceLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSensorModeLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkTimeOnLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkTimeOffLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSiteNameLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkFrameModeLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkQuicklookLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkProcAuthLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkDummyLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
		}

		for (i = highLightNum ; i-- ; )
		{
			XmListSelectPos( dtkPlatformLIST,		highLightPos[i], False ) ;
			XmListSelectPos( dtkSensorLIST,			highLightPos[i], False ) ;
			XmListSelectPos( dtkRevLIST,				highLightPos[i], False ) ;
			XmListSelectPos( dtkSequenceLIST,		highLightPos[i], False ) ;
			XmListSelectPos( dtkSensorModeLIST,	highLightPos[i], False ) ;
			XmListSelectPos( dtkTimeOnLIST,			highLightPos[i], False ) ;
			XmListSelectPos( dtkTimeOffLIST,		highLightPos[i], False ) ;
			XmListSelectPos( dtkSiteNameLIST,		highLightPos[i], False ) ;
			XmListSelectPos( dtkFrameModeLIST,	highLightPos[i], False ) ;
			XmListSelectPos( dtkQuicklookLIST,	highLightPos[i], False ) ;
			XmListSelectPos( dtkProcAuthLIST,		highLightPos[i], False ) ;
			XmListSelectPos( dtkDummyLIST,			highLightPos[i], False ) ;
		}

		if (highLightNum > 1)
		{
			/* change the selection policy back to the desired */
			XtVaSetValues( dtkPlatformLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSensorLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkRevLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSequenceLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSensorModeLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkTimeOnLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkTimeOffLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSiteNameLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkFrameModeLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkQuicklookLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkProcAuthLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkDummyLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
		}
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_dtkLists_selectionCb
**
** Description:		Updates the selection of one or more DTKs in all the
**								DTK list widgets, synchronously.
**
** Arguments:			1. widget	- Widget that is calling the original callback
**								2. cd 		- not used
**								3. cbs 		- list callback structure
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
void
dl2dtk_dtkLists_selectionCb(
		Widget widget,
		XtPointer cd,
		XmListCallbackStruct *cbs )
{
	_UxCDLtoDTK				*UxSaveCtx ;
	OP_DL2DTK_DATA		*clientData;
	OP_DL_DTK_LIST		*dtkPtr ;
	OP_DL_DTK_LIST		*tempPtr;
	int								dtkCount;
	int								*selectedItemPos;
	int								count;
	int								i, j ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		clientData					= &(glbData.dlClientData);
		dtkCount						= clientData->currDL->number_of_dtk_entry;
		dtkPtr							= dtkWinTopPtr ;
		selectedItemPos			= cbs->selected_item_positions;
		numSelectedDTKs			= cbs->selected_item_count;

		count								= dtkCount - dtkWindowTop;
		if (dtkRows < 0)
			XtVaGetValues( dtkPlatformLIST, XmNvisibleItemCount, &dtkRows, NULL );
		if (count > dtkRows)
			count = dtkRows;

		/*
		** deselect all items, then reselect the ones calling this callback
		*/

		XmListDeselectAllItems( dtkPlatformLIST ) ;
		XmListDeselectAllItems( dtkSensorLIST ) ;
		XmListDeselectAllItems( dtkRevLIST ) ;
		XmListDeselectAllItems( dtkSequenceLIST ) ;
		XmListDeselectAllItems( dtkSensorModeLIST ) ;
		XmListDeselectAllItems( dtkTimeOnLIST ) ;
		XmListDeselectAllItems( dtkTimeOffLIST ) ;
		XmListDeselectAllItems( dtkSiteNameLIST ) ;
		XmListDeselectAllItems( dtkFrameModeLIST ) ;
		XmListDeselectAllItems( dtkQuicklookLIST ) ;
		XmListDeselectAllItems( dtkProcAuthLIST ) ;
		XmListDeselectAllItems( dtkDummyLIST ) ;
		for (tempPtr = clientData->currDL->dtkList ; tempPtr != NULL
				; tempPtr = tempPtr->next)
		{
			tempPtr->selectFlag = 0;
		}


		/*
		** Set selectionPolicy of item list widgets to MultipleSelect
		** (have to change selection policy in order to select multiple rows)
		*/

		if (numSelectedDTKs > 1)
		{
			XtVaSetValues( dtkPlatformLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSensorLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkRevLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSequenceLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSensorModeLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkTimeOnLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkTimeOffLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkSiteNameLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkFrameModeLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkQuicklookLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkProcAuthLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
			XtVaSetValues( dtkDummyLIST,
					XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);
		}

		/*
		** Set the selectFlag for the selected DTKs only
		** (have to reset the selectFlag for each non-selected DTK)
		*/

		/* just in case: sort the selectedItemPos positions in ascending order */
		qsort( selectedItemPos, numSelectedDTKs, sizeof(int), intcmp ) ;

		j = 0 ;
		for (i = 1, tempPtr = dtkPtr ; i <= count && tempPtr != NULL
				; i++, tempPtr = tempPtr->next)
		{
				if (i != selectedItemPos[j])	/* j = 0 is safe, always has 1 sel. */
					tempPtr->selectFlag = 0;
				else
				{
					tempPtr->selectFlag = 1;

					/* i == selectedItemPos[j]: can use "i" in the XmListSelectPos call */
					XmListSelectPos( dtkPlatformLIST,		i, False ) ;
					XmListSelectPos( dtkSensorLIST,			i, False ) ;
					XmListSelectPos( dtkRevLIST,				i, False ) ;
					XmListSelectPos( dtkSequenceLIST,		i, False ) ;
					XmListSelectPos( dtkSensorModeLIST, i, False ) ;
					XmListSelectPos( dtkTimeOnLIST,			i, False ) ;
					XmListSelectPos( dtkTimeOffLIST,		i, False ) ;
					XmListSelectPos( dtkSiteNameLIST,		i, False ) ;
					XmListSelectPos( dtkFrameModeLIST,	i, False ) ;
					XmListSelectPos( dtkQuicklookLIST,	i, False ) ;
					XmListSelectPos( dtkProcAuthLIST,		i, False ) ;
					XmListSelectPos( dtkDummyLIST,			i, False ) ;

					if (++j >= numSelectedDTKs)
						j = 0 ;
				}
		}

		if (numSelectedDTKs > 1)
		{
			XtVaSetValues( dtkPlatformLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSensorLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkRevLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSequenceLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSensorModeLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkTimeOnLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkTimeOffLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkSiteNameLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkFrameModeLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkQuicklookLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkProcAuthLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
			XtVaSetValues( dtkDummyLIST,
					XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
		}

		/* sensitize DTK select menu items */
		setDTKselectButtons( True ) ;
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_show_downlinkDTKsCb
**
** Description:		display the DTKs for the selected downlink
**
** Arguments:			1. widget	- Widget that is calling the original callback
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
dl2dtk_show_downlinkDTKsCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK								*UxSaveCtx ;
	XmScrollBarCallbackStruct	*cbs;
	OP_CAT_STRUCT							*catReq;
	OP_DL2DTK_DATA						*clientData;
	OP_DL_LIST								*dlPtr;
	OP_DL_DTK_LIST						*tempPtr;
	int												dtkCount;
	char											Msg[IMS_COL1024_LEN+1];

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		/* assign client to dlClientData from glbData structure */
		clientData	= &(glbData.dlClientData);

		/* Get the the currently selected DL */
		if ((dlPtr = clientData->currDL) == NULL)
		{
			/* No DL is selected, display message, return */
			(void) sprintf( Msg, "select a Downlink first." );
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_INFO, Msg );
			return;
		}

		/* reset the DTK selected/update items */
		numSelectedDTKs = 0 ;
		setDTKselectButtons( False ) ;
		setDTKupdateButtons( False ) ;

		/* if not already "gotten," get the DTKs for this DL */
		if (dlPtr->dtkList == NULL)
		{
			/* Change cursor to watch cursor */
			timeOutCursors (True);

			/* Initialize catalog request structure  */
			catReq					= &(clientData->catReq);
			catReq->item[0]	= (OP_DL_LIST *)dlPtr;
			catReq->item[1]	= (int *)&dtkCount;

			/*
			** Get the DTK list for the DL
			*/
			if (ims_opCat (catReq, OP_GETDOWNLINKDTKLIST) < IMS_OK)
			{
				/* call dl2dtk_scroll_dtkListsCb to clear the DTK list widgets */
				cbs = (XmScrollBarCallbackStruct *)
						malloc(sizeof(XmScrollBarCallbackStruct));
				cbs->value = 0;
				dl2dtk_scroll_dtkListsCb( widget, NULL, cbs );
				free (cbs);

				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error message */
				(void) sprintf( Msg, "Internal Error: dtk retrieval failed." );
				msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_FATAL, Msg );

				return;
			}

			/* assign returned DTKs */
			dlPtr->number_of_dtk_entry = *(int *)catReq->item[1];
			dlPtr->dtkList = (OP_DL_DTK_LIST *)catReq->item[2];

			/* Initialize the dtkChanged array */
			if (dlPtr->number_of_dtk_entry > 0)
			{
				dlPtr->dtkChanged =
						(int *)calloc( dlPtr->number_of_dtk_entry, sizeof(int) ) ;
				if (dlPtr->dtkChanged == NULL)
				{
					free_dtkList( dlPtr->dtkList ) ;
					dlPtr->dtkList = NULL ;

					/* Change cursor back to normal */
					timeOutCursors (False);

					/* Display error message */
					(void)sprintf( Msg,
							"Out of space; can't allocate DTK changed array." );
					msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_FATAL, Msg ) ;

					return ;
				}
			}
		}


		/* if there are DTKs to display, initialize the DTK items */
		if (dlPtr->dtkList != NULL && dlPtr->number_of_dtk_entry > 0)
		{
			/* sensitize DTK update menu items */
			setDTKupdateButtons( True ) ;

			/* initialize DTK selectFlag */
			for (tempPtr = dlPtr->dtkList ; tempPtr != NULL ; tempPtr = tempPtr->next)
				tempPtr->selectFlag = 0;
		}

		/* call dl2dtk_scroll_dtkListsCb to load data in DTK list widgets */
		cbs = (XmScrollBarCallbackStruct *)
				malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		dl2dtk_scroll_dtkListsCb( widget, NULL, cbs );
		free (cbs);

		/* Change cursor back to normal */
		timeOutCursors (False);
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_view_DTK_detailsCb
**
** Description:		Display the details of the "double-clicked" DTK, ie,
**								this is the "default" callback.
**
** Arguments:			1. widget	- Widget that is calling the callback
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
dl2dtk_view_DTK_detailsCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK					*UxSaveCtx ;
	OP_DL2DTK_DATA			*clientData;
	OP_DL_DTK_LIST			*dtkPtr;
	char								Msg[IMS_COL1024_LEN+1];
	char								*buffer;
	char								*text;
	char								label[IMS_COL40_LEN+1];
	int									dtkCount ;
	int									count ;
	int									i ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		/*
		** in the list of DTKs, find the DTK using this callback
		*/

		clientData	= &(glbData.dlClientData);
		dtkPtr			= dtkWinTopPtr ;
		dtkCount		= clientData->currDL->number_of_dtk_entry;

		count = dtkCount - dtkWindowTop;
		if (dtkRows < 0)
			XtVaGetValues( dtkPlatformLIST, XmNvisibleItemCount, &dtkRows, NULL );
		if (count > dtkRows)
			count = dtkRows;

		/* position on the selected DTK on the current screen */
		for (i = 0 ; dtkPtr != NULL && dtkPtr->selectFlag == 0 && i < count ; i++)
			dtkPtr = dtkPtr->next;
		if (dtkPtr == NULL || i >= count)
		{
			/* Should never get here: No DTK is selected, error message, return */
			(void) sprintf( Msg, "Select a data-take first." ) ;
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_INFO, Msg ) ;
			return;
		}

		if ((buffer = XtMalloc ((unsigned int) 4049)) == (char *)NULL)
		{
			/* display error message, return */
			(void) sprintf( Msg, "Could not allocate space for text buffer." ) ;
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_FATAL, Msg ) ;
			return;
		}

		/* Write data-take information */
		text = buffer;
		(void) sprintf( text,
				"\n  Platform: %2s    Sensor: %s     Rev: %d     Sequence: %d\n",
				dtkPtr->platform, dtkPtr->sensor,
				dtkPtr->revolution, (int) dtkPtr->sequence ) ;
		text = text + strlen(text);
		(void) sprintf( text,
				"\n     DT Sensor: %s    DT Rev: %d    DT Sequence: %d\n\n",
				dtkPtr->dt_sensor, dtkPtr->dt_revolution,
				(int) dtkPtr->dt_sequence ) ;
		text = text + strlen(text);

		(void) sprintf (text, "%s\n\n",
				"====================================================================");
		text = text + strlen(text);

		(void) sprintf (text, "DT Platform:\t\t%s\n\n", dtkPtr->dt_platform);
		text = text + strlen(text);

		(void) sprintf (text, "Quicklook Flag:\t\t%s\n\n", dtkPtr->quicklook);
		text = text + strlen(text);

		(void) sprintf (text, "Process Auth. Flag:\t%s\n\n",
				dtkPtr->process_auth_flag);
		text = text + strlen(text);

		(void) sprintf (text, "Mode (sensor):\t\t%s\n\n", dtkPtr->mode);
		text = text + strlen(text);

		(void) sprintf (text, "Frame Mode:\t\t%s\n\n", dtkPtr->frame_mode);
		text = text + strlen(text);

		(void) sprintf (text, "Time On:\t\t%s\n\n", dtkPtr->time_on);
		text = text + strlen(text);

		(void) sprintf (text, "Time Off:\t\t%s\n\n", dtkPtr->time_off);
		text = text + strlen(text);

		(void) sprintf (text, "Site Name:\t\t%s\n\n", dtkPtr->site_name);
		text = text + strlen(text);

		(void) sprintf (text, "Last Updated Time:\t%s\n\n", dtkPtr->updated_time);
		text = text + strlen(text);

		(void) strcpy (label, "Data-take    Detailed    Information");
		browseDlg_popupCb (glbData.dl2dtkW, buffer, label);

		XtFree (buffer);
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_reset_dtkCb
**
** Description:		Reset all the selected DTKs to their original values.
**
** Arguments:			1. widget	- Widget that is calling the callback
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
dl2dtk_reset_dtkCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK					*UxSaveCtx ;
	OP_DL2DTK_DATA			*clientData;
	OP_DL_LIST					*dlPtr;
	OP_DL_DTK_LIST			*dtkPtr ;
	char								Msg[IMS_COL1024_LEN+1];
	int									*changedPtr ;
	int									dtkCount ;
	int									count ;
	int									i ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		clientData	= &(glbData.dlClientData);
		dlPtr				= clientData->currDL ;
		changedPtr	= dlPtr->dtkChanged + dtkWindowTop ;
		dtkCount		= dlPtr->number_of_dtk_entry;

		count				= dtkCount - dtkWindowTop;
		if (dtkRows < 0)
			XtVaGetValues( dtkPlatformLIST, XmNvisibleItemCount, &dtkRows, NULL );
		if (count > dtkRows)
			count = dtkRows;

		/* see if any DTKs are selected */
		if (!numSelectedDTKs)
		{
			/* No DL or DTKs are selected, display message, return */
			(void) sprintf( Msg, "Select Data-take(s) first." );
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_INFO, Msg );

			return;
		}

		/* on the current screen, reset each selected DTK to its db setting */

		for (i = 0, dtkPtr = dtkWinTopPtr
				; i < count && dtkPtr != NULL
				; i++, dtkPtr = dtkPtr->next, changedPtr++)
		{
			if (dtkPtr->selectFlag && *changedPtr)
			{
				resetDTK( dtkPtr, i + 1 ) ;
				*changedPtr = 0 ;
			}
		}
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_toggle_proc_auth_flagCb
**
** Description:		Toggle the processing flag for all selected DTKs.  If
**								reason is XmCR_EXTENDED_SELECT (ie, and not XmCR_ACTIVATE)
**								also do the extended selection callback for DTK lists.
**
** Arguments:			1. widget	- Widget that is calling the callback
**								2. cd 		- not used
**								3. cbs 		- list callback structure
**
** Return Value:	None
**
** Revision History:
**
**==========================================================================*/
void
dl2dtk_toggle_proc_auth_flagCb(
		Widget widget,
		XtPointer cd,
		XmListCallbackStruct *cb )
{
	_UxCDLtoDTK					*UxSaveCtx ;
	OP_DL2DTK_DATA			*clientData;
	OP_DL_LIST					*dlPtr;
	OP_DL_DTK_LIST			*dtkPtr ;
	char								Msg[IMS_COL1024_LEN+1];
	int									*changedPtr ;
	int									dtkCount ;
	int									count ;
	int									i ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		/* if reason called is extended select, do the selecting/highlighting */
		if (cb->reason == XmCR_EXTENDED_SELECT)
			dl2dtk_dtkLists_selectionCb( widget, cd, cb ) ;

		/* see if any DTKs are selected */
		if (!numSelectedDTKs)
		{
			/* No DTKs are selected, display message, return */
			(void) sprintf( Msg, "Select Data-take(s) first." );
			msgBoxDlg_popupCb( glbData.dl2dtkW, IMS_INFO, Msg );

			return;
		}

		clientData	= &(glbData.dlClientData);
		dlPtr				= clientData->currDL ;
		changedPtr	= dlPtr->dtkChanged + dtkWindowTop ;
		dtkCount		= dlPtr->number_of_dtk_entry;

		count				= dtkCount - dtkWindowTop;
		if (dtkRows < 0)
			XtVaGetValues( dtkPlatformLIST, XmNvisibleItemCount, &dtkRows, NULL );
		if (count > dtkRows)
			count = dtkRows;

		/* toggle the flag for all eligible selected DTKs on the current screen */

		/* set so can select more than one */
		XtVaSetValues( dtkProcAuthLIST,
				XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);

		for (i = 0, dtkPtr = dtkWinTopPtr
				; i < count && dtkPtr != NULL
				; i++, dtkPtr = dtkPtr->next, changedPtr++)
		{
			if (dtkPtr->selectFlag)
			{
				if (*changedPtr
						|| procAuthFlagIsChangeable( dlPtr, dtkPtr->process_auth_flag ))
				{
					XmString	x_str ;
					char			buffer[IMS_COL2_LEN] ;
					int				pos ;

					*changedPtr = !(*changedPtr) ;
					(void) strcpy( dtkPtr->process_auth_flag,
							procAuthValues[*changedPtr] );

					/* update the value in the proc auth list widget */
					(void) sprintf( buffer, "%c", *(dtkPtr->process_auth_flag) ) ;
					x_str = XmStringCreateLocalized( buffer );
					pos		= i + 1 ;
					XmListReplacePositions (dtkProcAuthLIST, &pos, &x_str, 1) ;
					if (!XmListPosSelected( dtkProcAuthLIST, pos ))
						XmListSelectPos( dtkProcAuthLIST,	pos, False ) ;
					XmStringFree( x_str ) ;
				}
			}
		}

		/* reset to extended selection */
		XtVaSetValues( dtkProcAuthLIST,
				XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_updateCb
**
** Description:		Saves all of the changed DTKs for the current Downlink
**								to the database (5/97: the only changes are that the
**								PROCESS_AUTH_FLAG may change from "NO" to "YES"), and
**								if there were any changes, issues a scan request for
**								this Downlink.
**
**								NOTE: if errors occur trying to save the DTKs to the
**								database, the database is not changed and all of the
**								changed DTKs for this Downlink are reset to their
**								original values.
**
**								NOTE: if errors occur trying to issue a Scan Request,
**								no request is issued, but the database has all the
**								DTK changes.  In this case, a Scan Request would have
**								to be issued manually.
**
** Arguments:			1. widget	- Widget that is calling the callback
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
dl2dtk_updateCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK					*UxSaveCtx ;
	OP_DL2DTK_DATA			*clientData;
	OP_CAT_STRUCT				*catReq;
	IMS_MSG_STRUCT			*msgDesc;
	OP_DL_LIST					*dlPtr;
	OP_DL_DTK_LIST			*dtkPtr ;
	char								*quicklook = "N" ;	/* for issuing the scan request */
	char								Msg[IMS_COL1024_LEN+1];
	Boolean							doScan = False ;	/* indicates if should issue a scan */
	Boolean							errResetAllDTKs=False ;	/* err: reset DTKs on the screen*/
	int									*changedPtr ;
	int									dtkCount ;
	int									i ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		clientData	= &(glbData.dlClientData) ;
		catReq			= &(clientData->catReq);
		msgDesc			= catReq->msgDesc;
		dlPtr				= clientData->currDL ;
		dtkPtr			= dlPtr->dtkList ;
		changedPtr	= dlPtr->dtkChanged ;
		dtkCount		= dlPtr->number_of_dtk_entry ;

		for (i = 0 ; i < dtkCount && dtkPtr != NULL
				; i++, dtkPtr = dtkPtr->next, changedPtr++)
		{
			if (!strcmp( dtkPtr->quicklook, "YES" ))	quicklook = "Y" ;

			if (*changedPtr)	/* see if there is at least one changed DTK */
			{
				doScan = True ;
				break;
			}
		}

		/*
		**	If there are any changed DTKs, save them in a single transaction.
		*/

		if (i < dtkCount && dtkPtr != NULL)
		{
			if (ims_opCat (catReq, OP_BEGINTRANSACTION) < IMS_OK)
			{
				/* Display error messages */
				sprintf( Msg, "Internal Error: Datatake_entry Process_auth_flag\n"
						"OP_BEGINTRANSACTION failed for\n"
						"    Platform: %s Sensor: %s Rev: %d Sequence: %d\n\n"
						"\n*** NO SCAN REQUEST issued for the downlink ***",
						dlPtr->platform, dlPtr->sensor, dlPtr->revolution, dlPtr->sequence);
				(void) ims_msg (msgDesc, IMS_FATAL, Msg ) ;
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg);

				errResetAllDTKs	= True ;
				doScan					= False ;
			}

			if (!errResetAllDTKs && ims_opCat (catReq, OP_GETAUXILIARYLOCK) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(Msg, "Internal Error:\nDatatake_entry Process_auth_flag,\n"
						"OP_GETAUXILIARYLOCK failed for\n",
						"    Platform: %s Sensor: %s Rev: %d Sequence: %d\n\n"
						"\n*** NO SCAN REQUEST issued for the downlink ***",
						dlPtr->platform, dlPtr->sensor, dlPtr->revolution, dlPtr->sequence);
				(void) ims_msg (msgDesc, IMS_FATAL, Msg ) ;
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg);

				errResetAllDTKs	= True ;
				doScan					= False ;
			}

			if (!errResetAllDTKs)
			{
				/* save all changed DTKs to the db (already positioned on the first) */
				for ( ; i < dtkCount && dtkPtr != NULL
						; i++, dtkPtr = dtkPtr->next, changedPtr++)
				{
					if (!strcmp( dtkPtr->quicklook, "YES" ))	quicklook = "Y" ;

					if (*changedPtr)
					{
						catReq->item[0] = (DBCHAR *) dtkPtr->platform ;
						catReq->item[1] = (DBCHAR *) dtkPtr->dt_sensor ;
						catReq->item[2] = (DBINT *) &dtkPtr->dt_revolution ;
						catReq->item[3]	= (DBSMALLINT *) &dtkPtr->dt_sequence ;
						catReq->item[4]	= (DBCHAR *) dtkPtr->process_auth_flag ;
							
						if (ims_opCat (catReq, OP_UPDATEDTKPROCESSAUTHFLAG) < IMS_OK)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Display error messages */
							sprintf(Msg,
									"Internal Error:\nUpdate Datatake_entry Process_auth_flag,\n"
									"OP_UPDATEDTKPROCESSAUTHFLAG failed for\n",
									"    Platform: %s Sensor: %s Rev: %d Sequence: %d\n\n"
									"    DT Sensor: %s DT Rev: %d DT Sequence: %d\n"
									"\n*** NO SCAN REQUEST issued for the downlink ***",
									dtkPtr->platform, dtkPtr->sensor, dtkPtr->revolution,
									dtkPtr->sequence, dtkPtr->dt_sensor,
									dtkPtr->dt_revolution, dtkPtr->dt_sequence ) ;
							(void) ims_msg (msgDesc, IMS_FATAL, Msg ) ;
							msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg);

							errResetAllDTKs	= True ;
							doScan					= False ;
							break ;
						}
					}
				}	/* for(), saving all the changed DTKs to the db */
			}

			if (!errResetAllDTKs &&
					ims_opCat (catReq, OP_COMMITTRANSACTION) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(Msg,
						"Internal Error:\nUpdate Datatake_entry Process_auth_flag,\n"
						"OP_COMMITTRANSACTION failed for\n",
						"    Platform: %s Sensor: %s Rev: %d Sequence: %d\n\n"
						"\n*** NO SCAN REQUEST issued for the downlink ***",
						dlPtr->platform, dlPtr->sensor, dlPtr->revolution, dlPtr->sequence);
				(void) ims_msg (msgDesc, IMS_FATAL, Msg ) ;
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg);

				errResetAllDTKs	= True ;
				doScan					= False ;
			}

			/* changes have been applied: clear out the changed array */
			/* if the DTK is currently visible, need to show it as unchangeable */
			if (!errResetAllDTKs)
			{
				XmString	x_str ;
				char			buffer[IMS_COL2_LEN] ;
				int				pos ;

				for (i = 0, dtkPtr = dlPtr->dtkList, changedPtr = dlPtr->dtkChanged
						; i < dtkCount && dtkPtr != NULL
						; i++, dtkPtr = dtkPtr->next, changedPtr++)
				{
					if (*changedPtr)
					{
						/* do this for any visible DTK */
						if (i >= dtkWindowTop && i <
								((dtkCount - dtkWindowTop) < (dtkWindowTop +dtkRows)) ?
								dtkCount - dtkWindowTop : dtkWindowTop +dtkRows)
						{
							if (!procFootnoteOn)
							{
								procFootnoteOn = True ;
								XtMapWidget( ProcFlagFootnote ) ;
							}

							pos = i - dtkWindowTop + 1 ;	/* "+ 1" for the "X" list position*/
							(void) sprintf( buffer, "%c*", *dtkPtr->process_auth_flag ) ;
							x_str = XmStringCreateLocalized( buffer );
							XmListReplacePositions (dtkProcAuthLIST, &pos, &x_str, 1) ;
							if (dtkPtr->selectFlag &&
										!XmListPosSelected( dtkProcAuthLIST, pos )
									|| !(dtkPtr->selectFlag) &&
										XmListPosSelected( dtkProcAuthLIST, pos ))
							{
								XmListSelectPos( dtkProcAuthLIST,	pos, False ) ;
							}
							XmStringFree( x_str ) ;
						}

						*changedPtr = 0 ;
					}
				}
			}
		}	/* if: there are any changed DTKs */

		/*
		** if need to reset all the changed DTKs
		*/

		if (errResetAllDTKs)
		{
			for (i = 0, dtkPtr = dlPtr->dtkList, changedPtr = dlPtr->dtkChanged
					; i < dtkCount && dtkPtr != NULL
					; i++, dtkPtr = dtkPtr->next, changedPtr++)
			{
				if (*changedPtr)
				{
					resetDTK( dtkPtr, i + 1 ) ;
					*changedPtr = 0 ;
				}
			}
		}

		if (doScan)
		{
			catReq->item[2] = (DBINT *) &dlPtr->revolution ;
			catReq->item[3] = (DBSMALLINT *) &dlPtr->sequence ;
			catReq->item[0] = (DBCHAR *) dlPtr->platform ;
			catReq->item[1] = (DBCHAR *) dlPtr->sensor ;
			catReq->item[4] = (DBCHAR *) quicklook ;

			if (ims_opCat (catReq, OP_ISSUEDOWNLINKSCANREQUEST) < IMS_OK)
			{
				/* Display error messages */
				sprintf( Msg, "Internal Error: Issue Downlink Scan Request\n"
						"OP_ISSUEDOWNLINKSCANREQUEST failed for\n"
						"    Platform: %s Sensor: %s Rev: %d Sequence: %d\n\n"
						"\nDTKs have been updated, *** BUT NO SCAN REQUEST issued ***\n"
						"Must manually issue a Scan Request",
						dlPtr->platform, dlPtr->sensor, dlPtr->revolution, dlPtr->sequence);
				(void) ims_msg (msgDesc, IMS_FATAL, Msg ) ;
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg);

				return ;
			}
		}
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}
/*===========================================================================*
**
** Function Name: dl2dtk_refreshSearchCb
**
** Description:		Callback for refresh button
**
** Arguments:			1. widget	- Widget that is calling the callback
**								2. cd 		- not used
**								3. cb 		- not used
**
** Return Value:	None
**
** Revision History:
**
**===========================================================================*/

void
dl2dtk_refreshSearchCb( Widget widget, XtPointer cd, XtPointer cb ) 
{
	_UxCDLtoDTK					*UxSaveCtx, *UxContext ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= UxContext = (_UxCDLtoDTK *) UxGetContext( widget );
	XtUnmapWidget( ProcFlagFootnote ) ;
  XmTextFieldSetString (dtkTotalDksTF, ""); 
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
	UxDLtoDTKContext = UxSaveCtx;

			return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_closeCb
**
** Description:		Clear out the downlink to DTK screen and exit it.
**
** Arguments:			1. widget	- Widget that is calling the callback
**								2. cd 		- not used
**								3. cbs 		- not used
**
** Return Value:	None
**
** Revision History: 6/9/97 D. Ting
**                   Correct for 'close' core dump by replacing widget by glbData.dl2dtkW.
**									 Added Total number count
**
**==========================================================================*/
/* ARGSUSED1 */
void
dl2dtk_closeCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK					*UxSaveCtx, *UxContext ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= UxContext = (_UxCDLtoDTK *) UxGetContext( glbData.dl2dtkW );
	{
		/* Delete the List Widgets */

		XmListDeleteAllItems (dtkPlatformLIST);
		XmListDeleteAllItems (dtkSensorLIST);
		XmListDeleteAllItems (dtkRevLIST);
		XmListDeleteAllItems (dtkSequenceLIST) ;
		XmListDeleteAllItems (dtkSensorModeLIST);
		XmListDeleteAllItems (dtkTimeOnLIST);
		XmListDeleteAllItems (dtkTimeOffLIST);
		XmListDeleteAllItems (dtkSiteNameLIST);
		XmListDeleteAllItems (dtkFrameModeLIST);
		XmListDeleteAllItems (dtkQuicklookLIST);
		XmListDeleteAllItems (dtkProcAuthLIST);
		XmListDeleteAllItems (dtkDummyLIST);

		XmListDeleteAllItems ( dlPlatformLIST ) ;
		XmListDeleteAllItems ( dlSensorLIST ) ;
		XmListDeleteAllItems ( dlRevLIST ) ;
		XmListDeleteAllItems ( dlSequenceLIST ) ;
		XmListDeleteAllItems ( dlActivityLIST ) ;
		XmListDeleteAllItems ( dlStationLIST ) ;
		XmListDeleteAllItems ( dlAntennaLIST ) ;
		XmListDeleteAllItems ( dlTimeOnLIST ) ;
		XmListDeleteAllItems ( dlTimeOffLIST ) ;
		XmListDeleteAllItems ( dlStatusLIST ) ;
		XmListDeleteAllItems ( dlNumDTKsLIST ) ;
		XmListDeleteAllItems ( dlDummyLIST ) ;
		XmTextFieldSetString (dlTotalDlksTF, "");
		XmTextFieldSetString (dtkTotalDksTF, "");


		/* Desensitize the appropriate buttons */

		setDLbuttons( False ) ;
	  XtSetSensitive( refreshSearchMPB,					False ) ;
		setDTKselectButtons( False ) ;
		setDTKupdateButtons( False ) ;


		/* clean up the client data */

		free_dlList() ;


		/* exit this screen */

		XtPopdown (XtParent(glbData.dl2dtkW));
		glbData.dl2dtkFlag = 0;
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_printScreenCb
**
** Description:		print the current downlink to data-take screen
**
** Arguments:			1. widget	- Widget that is calling the callback
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
dl2dtk_printScreenCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK					*UxSaveCtx ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		printScreen(glbData.dl2dtkW);

		/* Change cursor back to normal */
		timeOutCursors (False);
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_goto_searchCb
**
** Description:		Pop up the downlink search screen from the downlink screen
**
** Arguments:			1. widget	- Widget that is calling the callback
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
dl2dtk_goto_searchCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK					*UxSaveCtx ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
    XtPopup(XtParent(glbData.dlSearchW), XtGrabNone);
    glbData.dlSearchFlag = 1;
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name: dl2dtk_goto_welcomeCb
**
** Description:		Pop up the welcome screen from the downlink screen
**
** Arguments:			1. widget	- Widget that is calling the callback
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
dl2dtk_goto_welcomeCb( Widget widget, XtPointer cd, XtPointer cb )
{
	_UxCDLtoDTK					*UxSaveCtx ;

	UxSaveCtx					= UxDLtoDTKContext;
  UxDLtoDTKContext	= (_UxCDLtoDTK *) UxGetContext( widget );
	{
    XtPopup(XtParent(glbData.welcomeW), XtGrabNone);
    glbData.welcomeFlag = 1;
	}
	UxDLtoDTKContext = UxSaveCtx;

	return ;
}


/*===========================================================================*
**
** Function Name:	free_dlList
**
** Description:		Function to free the downlink list and associated DTK lists
**
** Return Value: 	None
**
** Revision History:
**
**==========================================================================*/
void
free_dlList()
{
	OP_DL2DTK_DATA	*clientData;
	OP_DL_LIST			*dlPtr, *dlNextPtr;

	clientData = &(glbData.dlClientData);

	if ((dlPtr = clientData->dlList) != (OP_DL_LIST *)NULL)
	{
		/* free up the clientData downlink list and associated DTK lists */
		while (dlPtr != (OP_DL_LIST *)NULL)
		{
			/* free up the DTK list associated with the DL */
			free_dtkList( dlPtr->dtkList ) ;
			dlPtr->dtkList	= NULL ;

			/* free up the dtkChanged array associated with the DL */
			free( dlPtr->dtkChanged ) ;
			dlPtr->dtkChanged	= NULL ;

			dlNextPtr = dlPtr->next;
			free(dlPtr);
			dlPtr = dlNextPtr;
		}

		clientData->dlList	= NULL ;
		clientData->currDL	= NULL ;
		clientData->dlCount	= 0;

		dlWinTopPtr								= NULL ;
		dtkWinTopPtr							= NULL ;
		clientData->dlWindowTop		=
					dlWindowTop				  = 0 ;
		clientData->dtkWindowTop	=
					dtkWindowTop				= 0 ;
		numSelectedDTKs						= 0 ;
	}

	return ;
}


/*===========================================================================*
**
** Function Name:	free_dtkList
**
** Description:		Function to free the DTK list.
**
** Return Value: 	None
**
** Revision History:
**
**==========================================================================*/
static void
free_dtkList( OP_DL_DTK_LIST *dtkList )
{
	OP_DL_DTK_LIST	*dtkPtr ;
	OP_DL_DTK_LIST	*dtkNextPtr ;

	dtkPtr = dtkList;
	while (dtkPtr != (OP_DL_DTK_LIST *)NULL)
	{
		dtkNextPtr = dtkPtr->next;
		free(dtkPtr);
		dtkPtr = dtkNextPtr;
	}

	return ;
}


/*===========================================================================*
**
** Function Name:	resetDTK
**
** Description:		resets one DTK to its original.
**
** Return Value: 	None
**
** Revision History:
**
**==========================================================================*/
static void
resetDTK( OP_DL_DTK_LIST *dtkPtr, int pos )
{
	XmString	x_str ;
	char			buffer[IMS_COL2_LEN] ;

	/* set so can select more than one */
	XtVaSetValues( dtkProcAuthLIST,
			XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);

	(void) strcpy( dtkPtr->process_auth_flag, procAuthValues[0] );

	/* update the value in the proc auth list widget */
	(void) sprintf( buffer, "%c", *(procAuthValues[0]) ) ;
	x_str = XmStringCreateLocalized( buffer );
	XmListReplacePositions (dtkProcAuthLIST, &pos, &x_str, 1) ;
	if (!XmListPosSelected( dtkProcAuthLIST, pos ))
		XmListSelectPos( dtkProcAuthLIST,	pos, False ) ;
	XmStringFree( x_str ) ;

	/* reset to extended selection */
	XtVaSetValues( dtkProcAuthLIST,
			XmNselectionPolicy, XmEXTENDED_SELECT, NULL);

	return ;
}


/*===========================================================================*
**
** Function Name: intcmp
**
** Description:		compares two integers
**
** Arguments:			1. inta		- first integer
**								2. intb 	- second integer
**
** Return Value:	-1 if inta is less than intb
**								 0 if inta equals intb
**								 1 if inta is greater than intb
**
** Revision History:
**
**==========================================================================*/
static int
intcmp( const void *a, const void *b )
{
	int		inta, intb ;

	inta = *(int *)a ;
	intb = *(int *)b ;

	if (inta < intb) return (-1) ;

	if (inta > intb) return (1) ;

	return (0) ;

}
