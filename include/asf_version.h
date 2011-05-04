#ifndef _ASF_VERSION_H_
#define _ASF_VERSION_H_


// SAR Training Processor
#define STP_VERSION_STRING "1.2.0-dev"
#define STP_VERSION 1.1

// Acquisition Planner
#define AP_VERSION_STRING "1.0.8-dev"
#define AP_VERSION 1.0

// Request Generator
#define REQ_VERSION_STRING "2.1.1"
#define REQ_VERSION 2.1

// MapReady
#define MAPREADY_VERSION_STRING "2.4.10-test"
#define MAPREADY_VERSION 2.4

// Convert To Vector
#define C2V_VERSION_STRING "2.3.0-dev"
#define C2V_VERSION 2.2

// JPL Mosaic Suite
#define JPL_VERSION_STRING "1.0.1-dev"
#define JPL_VERSION 1.0

// Create_thumbs
#define CREATE_THUMBS_VERSION_STRING "2.1.1-dev"
#define CREATE_THUMBS_VERSION 2.1

// Data_qc
#define DATA_QC_VERSION_STRING "1.0.2"
#define DATA_QC_VERSION 1.0

// Faraday prediction
#define FARADAY_VERSION_STRING "1.0.1"
#define FARADAY_VERSION 1.0

// Set Tool suite stuff based on given build define
// The values of the TOOL_SUITE_SHARE_DIR value must match what is
// used in the top-level Makefile
#if defined (_PKG_STP)
#define TOOL_SUITE_NAME			"STP"
#define TOOL_SUITE_SHARE_DIR            "stp"
#define TOOL_SUITE_VERSION_STRING	STP_VERSION_STRING
#define TOOL_SUITE_VERSION		STP_VERSION

#elif defined (_PKG_AP)
#define TOOL_SUITE_NAME			"AcquisitionPlanner"
#define TOOL_SUITE_SHARE_DIR            "ap"
#define TOOL_SUITE_VERSION_STRING	AP_VERSION_STRING
#define TOOL_SUITE_VERSION		AP_VERSION

#elif defined (_PKG_REQ)
#define TOOL_SUITE_NAME		        "REQ"
#define TOOL_SUITE_SHARE_DIR            "req"
#define TOOL_SUITE_VERSION_STRING	REQ_VERSION_STRING
#define TOOL_SUITE_VERSION		REQ_VERSION

#elif defined (_PKG_C2V)
#define TOOL_SUITE_NAME			"ConvertToVector"
#define TOOL_SUITE_SHARE_DIR            "c2v"
#define TOOL_SUITE_VERSION_STRING	C2V_VERSION_STRING
#define TOOL_SUITE_VERSION		C2V_VERSION

#elif defined (_PKG_JPL)
#define TOOL_SUITE_NAME			"JPLMosaicSuite"
#define TOOL_SUITE_SHARE_DIR            "jpl"
#define TOOL_SUITE_VERSION_STRING	JPL_VERSION_STRING
#define TOOL_SUITE_VERSION		JPL_VERSION

#elif defined (_PKG_CREATE_THUMBS)
#define TOOL_SUITE_NAME                 "CreateThumbs"
#define TOOL_SUITE_SHARE_DIR            "create_thumbs"
#define TOOL_SUITE_VERSION_STRING       CREATE_THUMBS_VERSION_STRING
#define TOOL_SUITE_VERSION              CREATE_THUMBS_VERSION

#elif defined (_PKG_DATA_QC)
#define TOOL_SUITE_NAME                 "DataQC"
#define TOOL_SUITE_SHARE_DIR            "data_qc"
#define TOOL_SUITE_VERSION_STRING       DATA_QC_VERSION_STRING
#define TOOL_SUITE_VERSION              DATA_QC_VERSION

#elif defined (_PKG_FARADAY)
#define TOOL_SUITE_NAME                 "FaradayPrediction"
#define TOOL_SUITE_SHARE_DIR            "faraday_prediction"
#define TOOL_SUITE_VERSION_STRING       DATA_QC_VERSION_STRING
#define TOOL_SUITE_VERSION              DATA_QC_VERSION

// Default to MapReady
#else //defined (_PKG_MAPREADY)
#define TOOL_SUITE_NAME			"MapReady"
#define TOOL_SUITE_SHARE_DIR            "mapready"
#define TOOL_SUITE_VERSION_STRING	MAPREADY_VERSION_STRING
#define TOOL_SUITE_VERSION		MAPREADY_VERSION

#endif



#include "svn_rev.h"

#endif
