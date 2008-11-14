#ifndef _DESKEW_HELP_H_
#define _DESKEW_HELP_H_

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
        "deskew"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
        TOOL_NAME" <input file> <input metadata file> <output file> <output metadata file>\n" \
        "             [-license] [-version] [-help]"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
    "   Deskew removes squint-induced skew from ground range images.\n" \
    "   The squint angle of an image along with the look angle is used\n" \
    "   to determine the amount of parallelogram shift skew that has\n" \
    "   been introduced in an image due to the doppler centroid selection\n" \
    "   during image processing.  It then remaps the image, using a bi-\n" \
    "   linear interpolation to remove this skew."

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        "   <input file>  An ASF Internal format file (.img, .ddr) that has\n" \
        "        not been deskewed (check the 'deskewed:' flag in the metadata\n" \
        "        file sar block)\n" \
        "   <input metadata file>  The metadata file that is associated\n" \
        "        with the input file."

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
        "   <output file>  An ASF Internal format file (.img, .ddr) that\n" \
        "        has been deskewed.\n" \
        "   <output metadata file>  The metadata file associated with the\n" \
        "        output file."

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
    "   -license\n" \
    "        Print copyright and license for this software then exit.\n" \
    "   -version\n" \
    "        Print version and copyright then exit.\n" \
    "   -help\n" \
    "        Print this help information and then exit."

// TOOL_EXAMPLES is required but is allowed to be an empty string
#ifdef  TOOL_EXAMPLES
#undef  TOOL_EXAMPLES
#endif
#define TOOL_EXAMPLES \
    ""

// TOOL_LIMITATIONS is required but is allowed to be an empty string
#ifdef  TOOL_LIMITATIONS
#undef  TOOL_LIMITATIONS
#endif
#define TOOL_LIMITATIONS \
    ""

// TOOL_SEE_ALSO is required but is allowed to be an empty string
#ifdef  TOOL_SEE_ALSO
#undef  TOOL_SEE_ALSO
#endif
#define TOOL_SEE_ALSO \
        "   sr2gr (slant range to ground range converter), and\n" \
	"   gr2sr (ground range to slant range converter)"

// Prototypes
void check_for_help(int argc, char* argv[]);
void usage();
void print_help();

#endif // _DESKEW_HELP_H_

