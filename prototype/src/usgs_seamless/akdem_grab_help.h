#ifndef _AKDEM_GRAB_HELP_H_
#define _AKDEM_GRAB_HELP_H_

// NOTES:
//   1. Don't put a '\n' at the end of the string definitions.  Formatting
//      is handled by usage() and print_help()
//   2. To match the formatting found in the other ASF include files and tool
//      output, use the examples below, i.e. a) paragraphs are 3 spaces from
//      the left, b) second line of options starts 13 spaces from the left,
//      c) option descriptions start 8 spaces from the left, etc.
//   3. License, version, and contact info are defined elsewhere (see asf_version.h,
//      svn_rev.h, asf_contact.h, and asf_license.h.  These files are included by
//      help.c
//

// TOOL_NAME is required
#ifdef  TOOL_NAME
#undef  TOOL_NAME
#endif
#define TOOL_NAME \
        "akdem_grab"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
        TOOL_NAME" <lat lo> <lat hi>  <lon lo> <lon hi> [NED1 | SRTM1 | SRTM3]\n" \
        "             [-license] [-version] [-help]"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
        "   Given a bounding set of latitudes, longitudes, and\n" \
	"   a DEM type, "TOOL_NAME" builds and issues a query for the\n" \
	"   USGS Seamless at http://seamless.usgs.gov to retrieve a DEM\n" \
	"   which covers the defined area of interest."

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        ""

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
        "   The DEM will download into a file named \"saved.zip\""

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
        "   - NED1, SRTM1, or SRTM3\n" \
	"        Specifically request a NED-1 (or -2), SRTM-1, or SRTM-3\n" \
	"        DEM.  If the selected type is not available, an error\n" \
	"        message will result.\n" \
	"   -license\n" \
	"        Print copyright and license for this software then exit.\n" \
	"   -version\n" \
	"        Print version and copyright then exit.\n" \
	"   -help\n" \
	"        Print this help file and quit."

// TOOL_EXAMPLES is required but is allowed to be an empty string
#ifdef  TOOL_EXAMPLES
#undef  TOOL_EXAMPLES
#endif
#define TOOL_EXAMPLES \
        "   To obtain a DEM containing the Fairbanks, Alaska region,\n" \
	"   you could issue the following command:\n\n" \
	"        "TOOL_NAME" 63.0 66.0 -150.5 -144.0"

// TOOL_LIMITATIONS is required but is allowed to be an empty string
#ifdef  TOOL_LIMITATIONS
#undef  TOOL_LIMITATIONS
#endif
#define TOOL_LIMITATIONS \
        "   Shuttle Radar Topography Mission (SRTM) DEMs are only\n" \
	"   available between latitudes north of -56.0 and south of +60.0 degrees."

// TOOL_SEE_ALSO is required but is allowed to be an empty string
#ifdef  TOOL_SEE_ALSO
#undef  TOOL_SEE_ALSO
#endif
#define TOOL_SEE_ALSO \
        ""

#define ASF_COPYRIGHT_STRING \
        "Copyright (c) %d, University of Alaska Fairbanks, Alaska Satellite Facility.\n" \
        "All rights reserved.\n"

// PROTOTYPES
int checkForOption(char *key, int argc, char* argv[]);
void check_for_help(int argc, char* argv[]);
void usage();
void print_help();

#endif // _AKDEM_GRAB_HELP_H_

