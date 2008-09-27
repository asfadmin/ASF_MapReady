#ifndef _ASF_VERSION_H_
#define _ASF_VERSION_H_


// SAR Training Processor
#define STP_VERSION_STRING "1.1.7-dev"
#define STP_VERSION 1.1

// Acquisition Planner
#define AP_VERSION_STRING "1.0.1-beta"
#define AP_VERSION 1.0

// Request Generator
#define REQ_VERSION_STRING "2.0.6-dev"
#define REQ_VERSION 2.0

// MapReady
#define MAPREADY_VERSION_STRING "2.1.5-dev"
#define MAPREADY_VERSION 2.0

// Convert To Vector
#define C2V_VERSION_STRING "2.0.3-dev"
#define C2V_VERSION 2.0

// JPL Mosaic Suite
#define JPL_VERSION_STRING "1.0.1-dev"
#define JPL_VERSION 1.0

// Set Tool suite stuff based on given build define
#if defined (_PKG_STP)
#define TOOL_SUITE_NAME			"STP"
#define TOOL_SUITE_VERSION_STRING	STP_VERSION_STRING
#define TOOL_SUITE_VERSION		STP_VERSION

#elif defined (_PKG_AP)
#define TOOL_SUITE_NAME			"AcquisitionPlanner"
#define TOOL_SUITE_VERSION_STRING	AP_VERSION_STRING
#define TOOL_SUITE_VERSION		AP_VERSION

#elif defined (_PKG_REQ)
# define TOOL_SUITE_NAME		"REQ"
# define TOOL_SUITE_VERSION_STRING	REQ_VERSION_STRING
# define TOOL_SUITE_VERSION		REQ_VERSION

#elif defined (_PKG_C2V)
# define TOOL_SUITE_NAME		"ConvertToVector"
# define TOOL_SUITE_VERSION_STRING	C2V_VERSION_STRING
# define TOOL_SUITE_VERSION		C2V_VERSION

#elif defined (_PKG_JPL)
# define TOOL_SUITE_NAME		"JPLMosaicSuite"
# define TOOL_SUITE_VERSION_STRING	JPL_VERSION_STRING
# define TOOL_SUITE_VERSION		JPL_VERSION

// Default to MapReady
#else //defined (_PKG_MAPREADY)
# define TOOL_SUITE_NAME		"MapReady"
# define TOOL_SUITE_VERSION_STRING	MAPREADY_VERSION_STRING
# define TOOL_SUITE_VERSION		MAPREADY_VERSION

#endif



#include "svn_rev.h"

#endif
