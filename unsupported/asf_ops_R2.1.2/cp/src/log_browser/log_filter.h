
#ifndef	LOG_FILTER_H
/* @(#)log_filter.h	1.9 93/05/06 15:38:59  */

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_filter.h
Type:           Local header
Title:			Log Filtering
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
05/04/93		Bill West		Added Exclusion filtering.
------------------------------------------------------------------------------

Description:	Defines constants, types, variables and macros used in the 
				setting and testing of log filters.  Log filters include:
					o	user defined filters (2).
					o	end-of-log markers (2).
					o	warning log entry tag (1).
					o	critical log entry tag (1).

*************************************************************************** */
#endif

#define	LOG_FILTER_H


/*  =========================  Includes  =========================  */


/*  =========================  Defines  =========================  */

#define	NUM_FILTERS				2
#define	NUM_EO_LOG_TAGS			2

#define	MAX_FILTER_LEN			40
#define	MAX_CRITERIA_LEN		10

#define	COMMAND_LINE_FILTER_LEN	(MAX_FILTER_LEN*2 + MAX_CRITERIA_LEN) 

#define	INCLUSION_FILTER		0
#define	EXCLUSION_FILTER		1

/*  =========================  Types  =========================  */

typedef	enum	{none=-1, or=0, and=1}	filter_criteria_t ;


/*  =========================  Global Variables  =========================  */

extern	filter_criteria_t	FilterCriteria ;
extern	filter_criteria_t	eoLogTagCriteria ;

extern	Boolean		Filter_on[NUM_FILTERS] ;
extern	Boolean		eoLogTag_on[NUM_EO_LOG_TAGS] ;

extern	int			Filter_type ;

extern	char		Filter[NUM_FILTERS][MAX_FILTER_LEN] ;
extern	char		eoLogTag[NUM_EO_LOG_TAGS][MAX_FILTER_LEN] ;

/*  =========================  Prototypes  =========================  */

extern	void	SetFilterResources() ;

extern	void	CreateFilterPopup(/* parent */) ;
extern	void	PopupFilter(/* w, filterType, call */) ;

extern	Boolean	EntryMeetsFilterCriteria(/* entry */) ;
extern	Boolean	EntryIsWarning(/* entry */) ;
extern	Boolean	EntryIsCritical(/* entry */) ;
extern	Boolean	EntryIsEoLogTag(/* entry */) ;


/*  =========================  Macros  =========================  */

#define	LOG_FILTERED	(Filter_on[0] || Filter_on[1])
#define	DOUBLE_FILTERED	(Filter_on[0] && Filter_on[1])

#define	EOL_LOG_CHECK	(eoLogTag_on[0] || eoLogTag_on[1])


#endif	/*  LOG_FILTER_H  */
/*  --- Do not add anything below this line ---  */

