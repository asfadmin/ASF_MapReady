

#ifndef lint
static char sccsid[] = "@(#)log_filter.c	1.1  12/19/96";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_filter.c
Title:			Log Filtering
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
05/04/93		Bill West		Added Exclusion filtering.
------------------------------------------------------------------------------

Module Overview:	This provides log filtering for two reasons:
					user-selectable filters, and filters checking for
					and end-of-log indicator.

Procedures/Functions called from other modules:
	SetFilterResources() ;
	CreateFilterPopup() ;
	PopupFilter() ;
	EntryMeetsFilterCriteria() ;
	EntryIsWarning() ;
	EntryIsCritical() ;
	EntryIsEoLogTag() ;

Procedures/Functions called in other modules:
	PopupMessage() ;

*************************************************************************** */
#endif

/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <Xm/Form.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>

#include <ctype.h>

/*  =========================  Defines  =========================  */

#define	VISIBLE_LEN	(MAX_FILTER_LEN)


/*  =========================  Types  =========================  */

typedef	struct	{
	Widget				filter[NUM_FILTERS] ;
	filter_criteria_t	criterion ;
}	workingFilter_t ;


/*  =========================  Global Variables  =========================  */

filter_criteria_t	FilterCriteria = none ;
filter_criteria_t	eoLogTagCriteria = none ;

Boolean		Filter_on[NUM_FILTERS] = {False, False} ;
Boolean		eoLogTag_on[NUM_EO_LOG_TAGS] = {False, False} ;

int			Filter_type = INCLUSION_FILTER ;

char		Filter[NUM_FILTERS][MAX_FILTER_LEN] ;
char		eoLogTag[NUM_EO_LOG_TAGS][MAX_FILTER_LEN] ;

static	workingFilter_t	workingFilter = {NULL, NULL, or} ;	

static	int				interimFilterType = INCLUSION_FILTER ;

static	Widget	dialogWidget ;

static	char	helpText[] = {"\
By entering data in this form, you may filter the log so as to \n\
**clude only certain entries.  To specify which entries you \n\
you would like to **clude, click the mouse over either of the \n\
two blank boxes, then begin typing characters.\n\n\
Then, if a log entry contains the same characters, that entry will \n\
be **cluded in the new filtered log.  Note:  the filters are case-\n\
insensitive.  That is to say, if you type the characters \"SPC\"\n\
the log will **clude any entries with \"SPC\", as well as those \n\
with \"Spc\" or \"spc\".\n\n\
To erase a single character in the criterion, use Back Space to \n\
erase backwards, and Del to erase forwards.  To quickly erase \n\
the entire criterion you have entered, you may press the \n\
\"Clear\" button.\n\n\
If you have entered two filter criteria and have pressed the \n\
button marked \"Or\", then an entry will be **cluded in the log \n\
if EITHER of the criteria you've entered are in the entry.\n\
If you press \"And\", then the entry will only be **cluded \n\
if BOTH of the criteria are found in the entry.\n\n\
When you have completed your specifications, press \"Ok\" to filter \n\
the current log (as well as any other logs you view without first \n\
clearing the filter).  If you wish to discard your filter settings, \n\
press \"Cancel\".\
"} ;


/*  =========================  Prototypes  =========================  */

static	Widget	createFilter(/* parent, filterNum */) ;
static	void	blankTextCB(/* w, filNum, call */) ;
static	void	changeCriterionCB(/* w, criNum, call */) ;
static	void	userDoneCB(/* w, ok_hit, call */) ;
static	void	userHelpCB(/* w, ok_hit, call */) ;
static	void	lopOffTrailingBlanks(/* str */) ;
static	void	disectCombinedString(/*inStr,outStr1,outStr2,criterion,type*/) ;
static	int		strstrCaseInsensitive(/* str1, str2 */) ;


/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void SetFilterResources ()

Description:	This takes the filter-related data out of the command line
				resources, disects it and saves it.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
		set the default filter state initially
		make sure the Filter string is not too long
		make sure the e-o-Log tag string is not too long
		disect the filter and e-o-Log tags by dividing them into components

*************************************************************************** */
#endif

void	SetFilterResources()
{
	int	i ;

	/*  set the default filter state initially  */
	for (i=0 ; i<NUM_FILTERS ; i++)
	{
		Filter[i][0] = eoLogTag[i][0] = '\0' ;
		Filter_on[i] = eoLogTag_on[i] = False ;
	}
	FilterCriteria = eoLogTagCriteria = False ;	
	Filter_type = INCLUSION_FILTER ;

	if (Resources.filter)
	{
		/*  make sure the Filter string is not too long  */
		Resources.filter[COMMAND_LINE_FILTER_LEN-1] = '\0' ;
		/*  now, pull it apart  */
		disectCombinedString(Resources.filter,
								Filter[0], Filter[1], 
								&FilterCriteria, &Filter_type) ;
		for (i=0 ; i<NUM_FILTERS ; i++)
			if (Filter[i][0])
				Filter_on[i] = True ;
	}

	if (Resources.eoLogTag)
	{
		/*  make sure the e-o-Log tag string is not too long  */
		Resources.eoLogTag[COMMAND_LINE_FILTER_LEN-1] = '\0' ;
		/*  now, pull it apart  */
		disectCombinedString(Resources.eoLogTag,
								eoLogTag[0], eoLogTag[1], 
								&eoLogTagCriteria, &i) ;
		for (i=0 ; i<NUM_FILTERS ; i++)
			if (eoLogTag[i][0])
				eoLogTag_on[i] = True ;
	}


}	/*  SetFilterResources  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CreateFilterPopup (parent)

Description:  Creates the filter popup widgets.

Arguments:
        parent --	parent widget.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	create the message strings
	create the dialog box
	create the OK, Cancel, and Help Buttons
	now, do away with the parts we don't need
	create a horizontal dialog to contain everything in a row
	create the first filter
	create the criteria radio box
	create the second filter
	set the callback for the OK and CXL buttons
	clean up XmStrings

*************************************************************************** */
#endif

void	CreateFilterPopup(parent)
Widget	parent ;
{
	Widget		buttons, rc, filter[NUM_FILTERS], radio, parts ;
	XmString	orstr, andstr ;
	Arg			myArgs[20] ;
	int			i ;

	/*  create the message strings  */
	orstr = XmStringCreateSimple(" Or ") ;
	andstr = XmStringCreateSimple(" And") ;

	/*  create the dialog box  */
	i = 0 ;
	XtSetArg(myArgs[i], XmNresizePolicy, XmRESIZE_NONE) ; i++ ;
    XtSetArg(myArgs[i], XmNnoResize, True) ; i++ ;
	dialogWidget = XmCreateFormDialog(parent, "FilterPopup",
										myArgs, i) ;

	/*  create the OK, Cancel, and Help Buttons  */
	i = 0 ;
	XtSetArg(myArgs[i], XmNbottomAttachment, XmATTACH_FORM) ; i++ ;
	XtSetArg(myArgs[i], XmNleftAttachment, XmATTACH_FORM) ; i++ ;
	XtSetArg(myArgs[i], XmNrightAttachment, XmATTACH_FORM) ; i++ ;
	buttons = XmCreateMessageBox(dialogWidget, "FilterPopup",
									myArgs, i) ;
	XtManageChild(buttons) ;

	/*  now, do away with the parts we don't need  */
	parts = XmMessageBoxGetChild(buttons, XmDIALOG_MESSAGE_LABEL) ;
	XtUnmanageChild(parts) ;

	/*  create a horizontal dialog to contain everything in a row  */
	rc = XtVaCreateManagedWidget("FilterPopup",
			xmRowColumnWidgetClass,	dialogWidget,
			XmNtopAttachment,		XmATTACH_FORM,
			XmNleftAttachment,		XmATTACH_FORM,
			XmNrightAttachment,		XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_WIDGET,
			XmNbottomWidget,		buttons,
			XmNorientation,			XmHORIZONTAL,
			NULL) ;

	/*  create the first filter  */
	filter[0] = createFilter(rc, 0) ;
	
	/*  create the criteria radio box  */
	radio = XmVaCreateSimpleRadioBox(
				rc, "FilterPopup", 0, changeCriterionCB,
				XmVaRADIOBUTTON, orstr, 'O', NULL, NULL,
				XmVaRADIOBUTTON, andstr, 'A', NULL, NULL,
				NULL) ;
	XtManageChild(radio) ;

	/*  create the second filter  */
	filter[1] = createFilter(rc, 1) ;
	
	/*  set the callback for the OK and CXL buttons  */
	XtAddCallback(buttons, XmNokCallback, userDoneCB, (caddr_t)1) ;
	XtAddCallback(buttons, XmNcancelCallback, userDoneCB, (caddr_t)0) ;
	XtAddCallback(buttons, XmNhelpCallback, userHelpCB, (caddr_t)NULL) ;
	
	/*  clean up XmStrings  */
	XmStringFree(orstr) ;
	XmStringFree(andstr) ;

}	/*  CreateFilterPopup  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void PopupFilter (w, filterType, call)

Description:	Pops up the filter specification box.

Arguments:
        w --			unused.
        filterType --	Inclusion or Exclusion.
        call --			unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:	
	Save the filter type, in case the user hits "OK" 
	Create the dialog's title
	Manage the dialog created in CreateFilterPopup.

*************************************************************************** */
#endif

void	PopupFilter(w, filterType, call)
Widget		w ;
int			filterType ;
XtPointer	call ;
{
	XmString	ttl ;

	/*  Save the filter type, in case the user hits "OK"  */
	interimFilterType = filterType ;

	/*  Create the dialog's title  */
	if (filterType == INCLUSION_FILTER)
		ttl = XmStringCreateSimple("Specify Log Inclusion Filter Criteria") ;
	else
		ttl = XmStringCreateSimple("Specify Log Exclusion Filter Criteria") ;
	XtVaSetValues(dialogWidget,
					XmNdialogTitle,	ttl,
					NULL) ;
	XmStringFree(ttl) ;

	/*  Manage the dialog created in CreateFilterPopup.  */
	XtManageChild(dialogWidget) ;

}	/*  PopupFilter  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean EntryMeetsFilterCriteria (entry)

Description:	Given a log entry string, this determines if it meets the
				current filter criteria.

Arguments:
        entry --	log entry string.

Return Value:	True if it does; False if it doesn't.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	For each of the filters, 
		see if the filter is a substring in entry
	Check the filter criteria to determine the return value

*************************************************************************** */
#endif

Boolean	EntryMeetsFilterCriteria(entry)
char	*entry ;
{
	Boolean	match[NUM_FILTERS], entryOK ;
	int		f ;

	/*  for each of the filters, see if the filter is a substring in entry  */
	for (f=0 ; f<NUM_FILTERS ; f++)
	{
		/*  initialize the booleans  */
		if (Filter_on[f] == True)
		{
			if (strstrCaseInsensitive(entry, Filter[f]))
				match[f] = True ;
			else
				match[f] = False ;
		}
		else
			match[f] = True ;
	}	/*  for  */

	/*  Check the filter criteria to determine the return value  */
  	if (FilterCriteria == or)
		entryOK = match[0] || match[1] ;
	else
		entryOK = match[0] && match[1] ;

	/*  Lastly, handle the exclusion filter case  */
	if (Filter_type == EXCLUSION_FILTER)
		entryOK = (entryOK == True) ? False : True ;

	return(entryOK) ;

}	/*  EntryMeetsFilterCriteria  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean EntryIsWarning (entry)

Description:	Given a log entry string, this determines if it constitutes
				a warning entry.

Arguments:
        entry --	the log entry.

Return Value:	True if warning; False if not.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	see if a warning tag was even specified as a resource
	if so, see if the warning tag is contained in the entry
	Return the right value

*************************************************************************** */
#endif

Boolean	EntryIsWarning(entry)
char	*entry ;
{
	Boolean	warning ;

	/*  see if a warning tag was even specified as a resource  */
	if (!Resources.warnTag)
		warning = False ;
	else
	{
		/*  if so, see if the warning tag is contained in the entry  */
		if (strstr(entry, Resources.warnTag))
			warning = True ;
		else
			warning = False ;
	}

	/*  Return the right value  */
	return(warning) ;

}	/*  EntryIsWarning  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean EntryIsCritical (entry)

Description:	Given a log entry string, this determines if it constitutes
				a critical entry.

Arguments:
        entry --	the log entry.

Return Value:	True if critical; False if not.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	see if a critical tag was even specified as a resource
	if so, see if the critical tag is contained in the entry
	Return the right value

*************************************************************************** */
#endif

Boolean	EntryIsCritical(entry)
char	*entry ;
{
	Boolean	critical ;

	/*  see if a critical tag was even specified as a resource  */
	if (!Resources.critTag)
		critical = False ;
	else
	{
		/*  if so, see if the critical tag is contained in the entry  */
		if (strstr(entry, Resources.critTag))
			critical = True ;
		else
			critical = False ;
	}

	/*  Return the right value  */
	return(critical) ;

}	/*  EntryIsCritical  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean EntryIsEoLogTag (entry)

Description:	Given a log entry string, this determines if it constitutes
				an end-of-log entry.

Arguments:
        entry --	the log entry.

Return Value:	True if e-o-log; False if not.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	For each of the two possible e-o-log tags:
		see if that e-o-log tag was even specified as a resource
		if so, see if the e-o-log tag is contained in the entry
	make sure the correct criteria is applied
	Return the right value

*************************************************************************** */
#endif

Boolean	EntryIsEoLogTag(entry)
char	*entry ;
{
	Boolean	match[NUM_EO_LOG_TAGS], entryEoL ;
	int		e ;

	/* For each of the two possible e-o-log tags: */
	for (e=0 ; e<NUM_EO_LOG_TAGS ; e++)
	{
		/* see if that e-o-log tag was even specified as a resource */
		if (eoLogTag_on[e] == True)
		{
			/* if so, see if the e-o-log tag is contained in the entry */
			if (strstr(entry, eoLogTag[e]))
				match[e] = True ;
			else
				match[e] = False ;
		}
		else
			match[e] = True ;
	}	/*  for  */

	/*  make sure the correct criteria is applied  */
  	if (eoLogTagCriteria == or)
		entryEoL = match[0] || match[1] ;
	else
		entryEoL = match[0] && match[1] ;

	/*  return the right value  */
	return(entryEoL) ;

}	/*  EntryIsEoLogTag  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static Widget createFilter (parent, filterNum)

Description:	Creates a text widget (for the user to enter a filter) 
				and a corresponding button widget (for the user to
				clear the filter).

Arguments:
        parent --		parent widget.
        filterNum --	which of the two filters.

Return Value:	A Row column containing the text and button widgets.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	create the "clear" label
	create the vertical row column
	create the text widget
	create the blanking button
	free up the string
	add the callback to the button
	return the row column

*************************************************************************** */
#endif

static	Widget	createFilter(parent, filterNum)
Widget	parent ;
int		filterNum ;
{
	Widget		rc, text, blanker ;
	XmString	clear ;


	/*  create the "clear" label  */
	clear = XmStringCreateSimple("Clear") ;

	/*  create the vertical row column  */
	rc = XtVaCreateManagedWidget("Filter",
			xmRowColumnWidgetClass,	parent,
			XmNorientation,			XmVERTICAL,
			XmNpacking,				XmPACK_TIGHT,
			XmNentryAlignment,		XmALIGNMENT_CENTER,
			NULL) ;

	/*  create the text widget  */
	workingFilter.filter[filterNum] = XtVaCreateManagedWidget("Filter",
				xmTextFieldWidgetClass,	rc,
				XmNcolumns,				VISIBLE_LEN,
				XmNmaxLength,			MAX_FILTER_LEN-1,
				NULL) ;

	/*  create the blanking button  */
	blanker = XtVaCreateManagedWidget("Filter",
				xmPushButtonWidgetClass,	rc,
				XmNlabelString,				clear,
				XmNalignment,				XmALIGNMENT_CENTER,
				NULL) ;

	/*  free up the string  */
	XmStringFree(clear) ;

	/*  add the callback to the button  */
	XtAddCallback(blanker, XmNactivateCallback, blankTextCB, (caddr_t)filterNum) ;

	/*  return the row column  */
	return(rc) ;

}	/*  createFilter  */
	

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void blankTextCB (w, filNum, call)

Description:	This gets called when the user presses the clear button
				on the filter.  It clears the text area.

Arguments:
        w --		unused.
        filNum --	Which filter is this clear button for?
        call --		unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	blank out the text for the specified filter

*************************************************************************** */
#endif

static	void	blankTextCB(w, filNum, call)
Widget		w ;
int			filNum ;
XtPointer	call ;
{
	/*  blank out the text for the specified filter  */
	XmTextFieldSetString(workingFilter.filter[filNum], "") ;

}	/*  blankTextCB  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void changeCriterionCB (w, criNum, call)

Description:	This gets called when the users presses the "And" or the
				"Or" button on the filter popup.

Arguments:
        w --		unused.
        criNum --	0 for "Or"; 1 for "And"
        call --		unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	set the specified criterion

*************************************************************************** */
#endif

static	void	changeCriterionCB(w, criNum, call)
Widget		w ;
int			criNum ;
XtPointer	call ;
{
	/*  set the specified criterion  */
	workingFilter.criterion = (filter_criteria_t) criNum ;

}	/*  changeCriterionCB  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void userDoneCB (w, ok_hit, call)

Description:	This is called when the user hits "OK" or "Cancel" on the
				Filter popup.

Arguments:
        w --		unused.
        ok_hit --	0 for "Cancel"; 1 for "OK"
        call --		unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	pop down the pop up...
	If the user hit "OK"
		apply the working filters to the global ones
		apply the filter criterion
		save the working filter type
		make sure the user sees it all
	else
		blank all the filters

*************************************************************************** */
#endif

static	void	userDoneCB(w, ok_hit, call)
Widget		w ;
int			ok_hit ;
XtPointer	call ;
{
	char	*philter[NUM_FILTERS] ;
	int		 f ;

	/*  pop down the pop up...  */
	XtUnmanageChild(dialogWidget) ;

	/*  If the user hit "OK"  */
	if (ok_hit) 
	{
		/*  apply the working filters to the global ones  */
		for (f=0 ; f<NUM_FILTERS ; f++)
		{
			philter[f] = XmTextFieldGetString(workingFilter.filter[f]) ;
			strcpy(Filter[f], philter[f]) ;
			if (strspn(Filter[f], " ") == strlen(Filter[f]))
				Filter_on[f] = False ;
			else
				Filter_on[f] = True ;
		}
		/*  apply the filter criterion  */
		if (DOUBLE_FILTERED)
			FilterCriteria = workingFilter.criterion ;
		else
			FilterCriteria = none ;
		
		/*  save the working filter type  */
		Filter_type = interimFilterType ;

		/*  make sure the user sees it all  */
		FillList() ;
		UpdateStatusBar() ;
	}
	else
	{
		/*  blank all the filters  */
		for (f=0 ; f<NUM_FILTERS ; f++)
			blankTextCB(NULL, f, NULL) ;
	}

}	/*  userDoneCB  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void userHelpCB (w, client, call)

Description:	Displays some help information for filtering.

Arguments:
        w --		unused.
        client --	unused.
        call --		unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	pop up the help text in the regular message dialog

*************************************************************************** */
#endif

static	void	userHelpCB(w, client, call)
Widget		w ;
XtPointer	client ;
XtPointer	call ;
{
	char	*tempHelpText ;
	char	 string[5], *pointer ;

	/*  make a copy of the base help text  */
	tempHelpText = (char *)strdup(helpText) ;

	/*  change the text to match the working filter type  */
	if (interimFilterType == INCLUSION_FILTER)
		strcpy(string, "in") ;
	else
		strcpy(string, "ex") ;
	
	while (pointer = strstr(tempHelpText, "**"))
		strncpy(pointer, string, 2) ;

	/* pop up the help text in the regular message dialog  */
	PopupMessage(tempHelpText) ;

	free(tempHelpText) ;

}	/*  userHelpCB  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void lopOffTrailingBlanks (str)

Description:	Lops trailing blanks off of a string.

Arguments:
        str --	string which may have trailing blanks

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	This is currenly unused.  I had written it thinking that
					the user may have inadvertantly added blanks to the 
					end of a filter specification.  I have since decided
					that if s/he did, it was on purpose and they should
					be included as part of the filter.

Pseudo Code:	For each character starting from the end until a non-blank one:
					replace the blank with and e-o-string marker.

*************************************************************************** */
#endif

static	void	lopOffTrailingBlanks(str)
char	*str ;
{
	int		c ;

	/*  for each of the characters from the end... */
	c = strlen(str)-1 ;
	while (c>=0 && str[c]==' ')
		/*  change the blank to an end-of-string  */
		str[c--] = '\0' ;

}	/*  lopOffTrailingBlanks  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void disectCombinedString (inStr, outStr1, outStr2,
        criterion, type)

Description:	This takes a command line string in one of the following formats:
					"<str1>"
					"<str1> || <str2>"
					"<str1> && <str2>"
					"! <str1>"
					"! <str1> || <str2>"
					"! <str1> && <str2>"
				determines if it's an exclusion filter (has the "! "), 
				extracts <str1>, <str2> (if extant), and the "And" or "Or"
				criteria between them (if extant).

Arguments:
        inStra --		the command line (combined) string.
        outStr1 --		the first substring.
        outStr2 --		the second substring (if any).
        criterion --	the criterion between the substrings (if any).
		type --			the type (inclusion filter or exclusion)

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	figure out the criterion if any
	if the first characters are "! "
		then this is an exclusion filter,
	else
		it's an inclusion filter.
	if it's all one string (no criterion)
		copy inStr to outStr1
	else
		(it's two strings divided by " && " or " || ")
		disect out the string before the criterion
		disect out the string after the criterion

*************************************************************************** */
#endif

static	void	disectCombinedString(inStr,outStr1,outStr2,criterion,type)
char				*inStr ;
char				*outStr1 ;
char				*outStr2 ;
filter_criteria_t	*criterion ;
int					*type ;
{
	int		 outLen1, outLen2 ;
	char	*inString, *criPtr, *str2Start ;

	/*  figure out if this is an inclusion or exclusion filter  */
	if (inStr[0]=='!' && inStr[1]==' ')
	{
		*type = EXCLUSION_FILTER ;
		inString = inStr + 2 ;
	}
	else
	{
		*type = INCLUSION_FILTER ;
		inString = inStr ;
	}

	/*  figure out the criterion if any  */
	criPtr = strstr(inString, " && ") ;
	if (criPtr && criPtr>inString)
		*criterion = and ;
	else 
	{
		criPtr = strstr(inString, " || ") ;
		if (criPtr && criPtr>inString)
			*criterion = or ;
		else
			*criterion = none ;
	}

		
	/*  see if it's all one string  */
	if (*criterion==none)
	{
		/*  it's all one string  */
		strncpy(outStr1, inString, MAX_FILTER_LEN) ;
		outStr1[MAX_FILTER_LEN-1] = '\0' ;	/*  force e-o-string  */
		outStr2[0] = '\0' ;	/*  blank  */
	}
	else
	{
		/*  it's two strings divided by " && " or " || "  */
		str2Start = criPtr + 4 ;

		/*  disect out the string before the criterion  */
		outLen1 = criPtr - inString ;
		outLen1 = MIN(outLen1, MAX_FILTER_LEN-1) ;
		strncpy(outStr1, inString, outLen1) ;
		outStr1[outLen1] = '\0' ;	/*  e-o-string  */

		/*  disect out the string after the criterion  */
		outLen2 = strlen(str2Start) ;
		outLen2 = MIN(outLen2, MAX_FILTER_LEN-1) ;
		strncpy(outStr2, str2Start, outLen2) ;
		outStr2[outLen2] = '\0' ;	/*  e-o-string  */
	}
		
}	/*  disectCombinedString  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static int	 strstrCaseInsensitive (str1, str2)

Description:	searches to see if one string is a substring of another, but
				case sensitivity is turned off.

Arguments:
        str1 --	test string
        str2 --	potential substring.

Return Value:	1 if str2 is in str1; 0 if not.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	check to make sure neither string is the NULL pointer
	check for obvious non-matches.
	for each possible substring in str1, 
		convert both str1 and str2 to upper case and compare

*************************************************************************** */
#endif

static	int		strstrCaseInsensitive(str1, str2)
char	*str1 ;
char	*str2 ;
{
	int		len1, len2, matchedChars, testChar ;
	char	test1, test2 ;

	/*  check to make sure neither string is the NULL pointer  */
	if (!str1 || !str2)
		return(0) ;

	len1 = strlen(str1) ;
	len2 = strlen(str2) ;

	/*  check for obvious non-matches.  */
	if (!len1 || !len2 || len2>len1)
		return(0) ;

	testChar = matchedChars = 0 ; 

	/*  for each possible substring in str1, 
	 *	convert both str1 and str2 to upper case and compare  
	 */
	while (testChar<len1 && matchedChars<len2)
	{
		/*  extract the characters to compare, converting them to upper 
		 *	case if necessary.
		 */
		test2 = str2[matchedChars] ;
		if (islower(test2))
			test2 = (char) toupper(test2) ;
		test1 = str1[testChar] ;
		if (islower(test1))
			test1 = (char) toupper(test1) ;

		if (test2==test1)
		{
			/*  see if the next characters in both strings match  */
			testChar++ ;
			matchedChars++ ;
		}
		else
		{
			if (matchedChars > 0)
				/*  retry the same character in str1 against the 
				 *	first character in str2
				 */
				matchedChars = 0 ;
			else
				/*  try the next character in str1 against the
				 *	first character in str2
				 */
				testChar++ ;
		}
	}	/*  while  */

	if (matchedChars==len2)
		return(1) ;
	else
		return(0) ;

}	/*  strstrCaseInsensitive  */
