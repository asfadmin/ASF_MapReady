#ifndef _RESAMPLE_HELP_H_
#define _RESAMPLE_HELP_H_

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
        "resample"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
        TOOL_NAME"  [\n"\
        "                -square <pixsiz> | \n"\
        "                -scale <scale factor> | \n"\
        "                -scalex <x scale factor> -scaley <y scale factor> |\n"\
        "                <x pixel size> <y pixel size> \n"\
        "             ]\n"\
        "             [ -nearest_neighbor ]\n"\
        "             <infile> <outfile>\n" \
        "             [-license] [-version] [-help]"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
    "   The resample tool resamples the input file to produce a new\n" \
    "   file with the given pixel size, or scaled by the given scale\n" \
    "   factor(s).\n\n" \
    "   If the input file is multi-banded, resample will apply the\n" \
    "   resampling to each band separately.\n"

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        "   <infile>  The basename of the input data file.\n\n"\
        "   You have four different ways of specifying the size of the\n"\
        "   output image:\n\n"\
        "     X and Y Pixel Sizes:\n"\
        "       resample <x pixel size> <y pixel size> infile outfile\n"\
        "     Square Pixels (Equal X and Y Pixel Sizes):\n"\
        "       resample -square <pixel size> infile outfile\n"\
        "     X and Y Scale Factors:\n"\
        "       resample -scalex <x scale factor> -scaley <y scale factor>\n"\
        "                infile outfile\n"\
        "     Uniform Scaling (Equal X and Y Scale Factors):\n"\
        "       resample -scale <scale factor> infile outfile\n\n"

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
        "   <outfile>  The basename of the output file.\n"

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
    "   -square <pixel size, in meters>\n"\
    "        Creates an output image with square pixels of the given size.\n"\
    "        May not be used with -scale,-scalex, or -scaley.\n"\
    "   -scale <scale factor>\n"\
    "        Creates an ouput image that is larger by the given factor, in\n"\
    "        each direction.  For example, using a scale factor of 1/2 will\n"\
    "        result in an image with half as many lines, and half as many\n"\
    "        samples.  May not be used with -square,-scalex, or -scaley.\n"\
    "   -scalex <x scale factor>\n"\
    "        Creates an ouput image that is larger by the given factor, in\n"\
    "        the x direction.  You must also specify a y scale factor when\n"\
    "        using this option.  (-scaley)\n"\
    "   -scaley <y scale factor>\n"\
    "        Creates an ouput image that is larger by the given factor, in\n"\
    "        the y direction.  You must also specify a x scale factor when\n"\
    "        using this option.  (-scalex)\n"\
    "   -nearest_neighbor ( -nn )\n"\
    "        Don't average pixels when interpolating, use the nearest.\n"\
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
    "   Produce an output image (b) with 100x100 meter pixels:\n"\
    "   > resample -square 100 a b\n\n"\
    "   Produce an output image with 100x200 meter pixels:\n"\
    "   > resample 100 200 a b\n\n"\
    "   Create an image 1/8 as large in each direction (1/64):\n"\
    "   > resample -scale 8 a b\n"

// TOOL_LIMITATIONS is required but is allowed to be an empty string
#ifdef  TOOL_LIMITATIONS
#undef  TOOL_LIMITATIONS
#endif
#define TOOL_LIMITATIONS \
    "   resample only works on ASF Internal format files (.img)"

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

#endif // _RESAMPLE_HELP_H_

