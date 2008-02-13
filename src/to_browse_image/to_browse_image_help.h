#ifndef _TO_BROWSE_IMAGE_HELP_H_
#define _TO_BROWSE_IMAGE_HELP_H_

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
        "to_browse_image"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
        TOOL_NAME" [-input-format <stf|ceos>] [-output-format <tiff|jpeg>]\n" \
        "                [-scale <scale factor>] [-width <pixel width>] [-height <pixel height>]" \
        "                <input basename> [-output <output basename>]\n" \
        "                [-license] [-version] [-help]"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
    "   to_browse_image reads the input image, and if necessary will apply\n" \
    "   a range-doppler algorithm to it (and a conversion to ground range),\n" \
    "   then will resample and export it to the selected graphics file format.\n" \
    "   See default settings below."

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        "   <input basename>  Currently only Level 0 STF format files are supported.\n" \
        "        The basename is the part of the file name which does not include the\n" \
        "        final file extension, i.e.\n\n" \
        "        The basename of r11014804st7036201.001 and r11014804st7036201.001.par (etc)\n" \
        "        is just the \"r11014804st7036201.001\" or \"r11014804st7036201\" portion of\n" \
        "        the name.  NOTE: When multiple files are required, i.e. the .000 and .000.par (etc)\n" \
        "        files, both are required for processing to proceed.\n\n" \

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
        "   Output will be to <input basename>.ext where the extension is appropriate to the type\n" \
        "   graphics file format selected, i.e. r11014804st7036201.001.tif or r11014804st7036201.001.jpg.\n" \
        "   NOTE: This is the default action.  See the -output option below for more information."

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
    "   -input-format\n" \
    "        Either stf or ceos.  This option tells to_browse_image what format the input data is in.\n" \
    "        NOTE: Only STF Level 0 format is currently supported.  The default is stf.\n" \
    "   -output-format\n" \
    "        Use this option to select either tiff or jpeg graphics file formats for the browse image.\n" \
    "        The default is tiff.\n"
    "   -scale\n" \
    "        Use this option to specify a fixed scale for scaling down the original image into a browse\n" \
    "        image, i.e. a scale factor of 4 will create a browse image 1/4th the size of the original\n"
    "        in both dimensions.  The default is 8.  NOTE: Cannot be used together with the -width or -height\n" \
    "        options.\n" \
    "   -width\n" \
    "        Use this option to specify a width, in pixels, for the browse image.  If the -height option is\n" \
    "        not specified as well, then the image height will be scaled at the same rate as the width.\n" \
    "        There is no default pixel width.  NOTE: Cannot be used together with the -scale option.\n" \
    "   -height\n" \
    "        Use this option to specify a height, in pixels, for the browse image.  If the -width option is\n" \
    "        not specified as well, then the image width will be scaled at the same rate as the height.\n" \
    "        There is no default pixel height.  NOTE: Cannot be used together with the -scale option.\n" \
    "   -output\n" \
    "        Use this option to specify an output filename for the browse image.  The default is described\n" \
    "        in the tool output section above, i.e. input basename plus appropriate file extension.\n" \
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
    ""

// Prototypes
void check_for_help(int argc, char* argv[]);
void usage();
void print_help();

#endif // _TO_BROWSE_IMAGE_HELP_H_

