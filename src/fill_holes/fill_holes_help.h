#ifndef _FILL_HOLES_HELP_H_
#define _FILL_HOLES_HELP_H_

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
        "fill_holes"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
        TOOL_NAME" [-log <logfile>] [-quiet] [-cutoff <height>] <infile> <outfile>\n" \
        "              [-license] [-version] [-help]"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
    "   The fill_holes tool scans an image for regions of data below the given\n" \
    "   cut-off height, then when found, interpolates the surrounding values\n" \
    "   over the hole to 'patch' it with the most realistic values that it can.\n" \
    "   The most useful application of this tools is for patching holes in DEMs\n" \
    "   since holes in DEMs result in visible defects in terrain-corrected images."

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        "   <infile>  The basename of the input data file.  Must be in ASF Internal\n" \
        "        format (.img)"

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
    "        Save all tool output in <logfile>\n\n" \
    "   -quiet\n" \
    "        Suppress all output\n\n" \
    "   -cutoff <height>\n" \
    "        Use <height>, in meters, as the minimum valid data value in the image.\n" \
    "        Values below <height> will be considered to be no-data values (a hole)\n" \
    "        and will be patched as described.  The default cutoff value is -900\n" \
    "        meters.\n\n" \
    "        The default -900 is a good choice for SRTM dems.\n\n" \
    "   -license\n" \
    "        Print copyright and license for this software then exit.\n\n" \
    "   -version\n" \
    "        Print version and copyright then exit.\n\n" \
    "   -help\n" \
    "        Print this help information and then exit."

// TOOL_EXAMPLES is required but is allowed to be an empty string
#ifdef  TOOL_EXAMPLES
#undef  TOOL_EXAMPLES
#endif
#define TOOL_EXAMPLES \
    "    This example fills holes in the dem 'dem.img' (with metadata 'dem.meta'),\n"\
    "    creating 'dem_fixed.img' and 'dem_fixed.meta'.  A hole will be any DEM\n"\
    "    values lower than 0 meters:\n"\
    "      > fill_holes -cutoff 0 dem dem_fixed"

// TOOL_LIMITATIONS is required but is allowed to be an empty string
#ifdef  TOOL_LIMITATIONS
#undef  TOOL_LIMITATIONS
#endif
#define TOOL_LIMITATIONS \
    "    At this time, fill_holes only works on ASF Internal format files (.img)"

// TOOL_SEE_ALSO is required but is allowed to be an empty string
#ifdef  TOOL_SEE_ALSO
#undef  TOOL_SEE_ALSO
#endif
#define TOOL_SEE_ALSO \
    "    asf_terrcorr\n" \
    "    asf_mapready\n" \
    "    mapready"

// Prototypes
void check_for_help(int argc, char* argv[]);
void usage();
void print_help();

#endif // _FILL_HOLES_HELP_H_

