

/* @(#)log_resrc.h	1.6 93/05/06 15:39:02  */

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_resrc.h
Type:           Local header
Title:			Log Browser Resources
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Description:	This defines the special log browser-related resources defined
				for this application.  It also defines the command line option
				syntax needed to specify these resources from the command line.

*************************************************************************** */
#endif

#ifndef LOG_RESRC_H
#define LOG_RESRC_H


/*  ============================  INCLUDES  ============================  */

#include "log_main.h"


/*  ============================  DEFINES  ============================  */

#define	DEFAULT_UPDATE_FREQ	1000

/* Application-specific resource definitions and structure initialization */

#define	XtNlogFile			"logFile"
#define	XtCLogFile 			"LogFile"
#define	XtNfilter			"filter"
#define	XtCFilter			"Filter"
#define	XtNwarnTag			"warnTag"
#define	XtCWarnTag			"WarnTag"
#define	XtNcritTag			"critTag"
#define	XtCCritTag			"CritTag"
#define	XtNeoLogTag			"eoLogTag" 
#define	XtCEoLogTag			"EoLogTag"
#define	XtNdebug			"debug"
#define	XtCDebug			"Debug"
#define	XtNdelay			"delay"
#define	XtCDelay			"Delay"
#define	XtNsoundOn			"soundOn"
#define	XtCSoundOn			"SoundOn"
#define	XtNbrowseMode		"browseMode"
#define	XtCBrowseMode		"browseMode"
#define	XtNfirstLogCurrent	"firstLog"
#define	XtCFirstLogCurrent	"FirstLog"


/*  ============================  VARIABLES  ============================  */

resource_t	Resources ;


static XtResource resources[] = {
	{
		XtNlogFile,
		XtCLogFile,
		XtRString,
		sizeof(String),
		XtOffset(resource_ptr_t, logFile),
		XtRString,
		(XtPointer) NULL,
	},
	{
		XtNfilter,
		XtCFilter,
		XtRString,
		sizeof(String),
		XtOffset(resource_ptr_t, filter),
		XtRString,
		(XtPointer) NULL,
	},
	{
		XtNwarnTag,
		XtCWarnTag,
		XtRString,
		sizeof(String),
		XtOffset(resource_ptr_t, warnTag),
		XtRString,
		(XtPointer) NULL,
	},
	{
		XtNcritTag,
		XtCCritTag,
		XtRString,
		sizeof(String),
		XtOffset(resource_ptr_t, critTag),
		XtRString,
		(XtPointer) NULL,
	},
	{
		XtNeoLogTag,
		XtCEoLogTag,
		XtRString,
		sizeof(String),
		XtOffset(resource_ptr_t, eoLogTag),
		XtRString,
		(XtPointer) NULL,
	},
	{
		XtNdebug,
		XtCDebug,
		XtRBoolean,
		sizeof(Boolean),
		XtOffset(resource_ptr_t, debug),
		XtRImmediate,
		(XtPointer) FALSE,
	},
	{
		XtNdelay,
		XtCDelay,
		XtRInt,
		sizeof(unsigned int),
		XtOffset(resource_ptr_t, delay),
		XtRImmediate,
		(XtPointer) DEFAULT_UPDATE_FREQ,
	},
	{
		XtNsoundOn,
		XtCSoundOn,
		XtRBoolean,
		sizeof(Boolean),
		XtOffset(resource_ptr_t, soundOn),
		XtRImmediate,
		(XtPointer) FALSE,
	},
	{
		XtNbrowseMode,
		XtCBrowseMode,
		XtRBoolean,
		sizeof(Boolean),
		XtOffset(resource_ptr_t, browseMode),
		XtRImmediate,
		(XtPointer) FALSE,
	},
	{
		XtNfirstLogCurrent,
		XtCFirstLogCurrent,
		XtRBoolean,
		sizeof(Boolean),
		XtOffset(resource_ptr_t, firstLog),
		XtRImmediate,
		(XtPointer) FALSE,
	},
};

static XrmOptionDescRec options[] = {
  	{"-log",		"*logFile",			XrmoptionSepArg,	NULL},
  	{"-filter",	 	"*filter",			XrmoptionSepArg,	NULL},
  	{"-warn",	 	"*warnTag",			XrmoptionSepArg,	NULL},
  	{"-crit",	 	"*critTag",			XrmoptionSepArg,	NULL},
  	{"-endlog",	 	"*eoLogTag",		XrmoptionSepArg,	NULL},
  	{"-dbg", 		"*debug",			XrmoptionNoArg,		"True"},
  	{"-debug", 		"*debug",			XrmoptionNoArg,		"True"},
  	{"-delay", 		"*delay",			XrmoptionSepArg,	NULL},
  	{"-sound",	 	"*soundOn",			XrmoptionNoArg,		"True"},
  	{"-silent", 	"*soundOn",			XrmoptionNoArg,		"False"},
  	{"-browse", 	"*browseMode",		XrmoptionNoArg,		"True"},
  	{"-update", 	"*browseMode",		XrmoptionNoArg,		"False"},
  	{"-first",		"*firstLog",		XrmoptionNoArg,		"True"},
  	{"-last", 		"*firstLog",		XrmoptionNoArg,		"False"},
};

#endif /* LOG_RESRC_H */

/* DO NOT ADD ANYTHING BELOW THIS LINE */
