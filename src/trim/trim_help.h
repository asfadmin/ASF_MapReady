#ifndef _TRIM_HELP_H_
#define _TRIM_HELP_H_

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
    "trim"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
    TOOL_NAME" [-log <logfile>] [-height height] [-width width] \n "\
    "        [-to <metadata file>] [-license] [-version] [-help]\n"\
    "        [-lat1 <lat> -lat2 <lat> -lon1 <lon> -lon2 <lon>]\n"\
    "        <infile> <outfile> <new_top_line> <new_left_sample>\n"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
    "   trim allows you to trim/add any number of line or samples to an image\n" \
    "   (in ASF Internal format), and updates the metadata accordingly."

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
    "   <infile>  The basename of the input data file.  Must be in ASF Internal\n" \
    "        format (.img)\n"\
    "   <new_top_line> Line in input that will be the top of output.  Can be\n"\
    "        negative, the top lines in the output image will be zeros.\n"\
    "   <new_top_sample> Sample in input that will be left side of output.  Can\n"\
    "        be negative, the left side of the output will be zeros.\n"\
    "   When the -to option is used, the new_top_line and new_top_sample arguments\n"\
    "   are not needed.\n"

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
    "   <outfile>  The basename of the output file.  The output file will be stored\n" \
    "        in ASF Internal format (.img and .meta)"

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
    "   -log <logfile>\n" \
    "        Save all tool output in <logfile>\n" \
    "   -height <height>\n" \
    "        Height of the output file.  If the height extends below the bottom\n" \
    "        of the image, it is padded with zeros.\n" \
    "   -width <width>\n" \
    "        Width of the output file.  If the width extends beyond the right\n" \
    "        edge of the image, it is padded with zeros.\n" \
    "   -to <metadata file>\n" \
    "        Trims the input file to the extent of the metadata file.  With this\n" \
    "        option you would not use -width, -height, or the new start line and\n" \
    "        sample values.\n"\
    "   -lat1 <lat> -lat2 <lat> -lon1 <lon> -lon2 <lon>\n"\
    "        Specifies a lat/lon boundary box to use for trimming.  With these\n"\
    "        options you would not use -width, -height, or the new start lien and\n"\
    "        sample vaules.\n"\
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
    "   Remove a 100x100 section, starting at line 50, sample 200\n"\
    "   from the file \"test.img\", and \"test.meta\".\n"\
    "   > trim -width 100 -height 100 test test_trimmed 50 200\n"\
    "\n"\
    "   Trim test.img to the extent of \"section.meta\".\n"\
    "   > trim -to section test test_trimmed\n"\
    "\n"\
    "   Trim test.img to the lat/lon area (64,-146) - (65,-147)\n"\
    "   > trim -lat1 64 -lat2 65 -lon1 -146 -lon2 -147 test test_trimmed\n"

// TOOL_LIMITATIONS is required but is allowed to be an empty string
#ifdef  TOOL_LIMITATIONS
#undef  TOOL_LIMITATIONS
#endif
#define TOOL_LIMITATIONS \
    "   Only works on ASF Internal files."

// TOOL_SEE_ALSO is required but is allowed to be an empty string
#ifdef  TOOL_SEE_ALSO
#undef  TOOL_SEE_ALSO
#endif
#define TOOL_SEE_ALSO \
    ""

// Prototypes
void check_for_help(int argc, char* argv[]);
void usage();
void print_help();

#endif // _TRIM_HELP_H_

