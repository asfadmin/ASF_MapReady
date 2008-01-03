#ifndef _CONVERT2VECTOR_HELP_H_
#define _CONVERT2VECTOR_HELP_H_

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
        "convert2vector"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
        TOOL_NAME" [-list] <input format> <output format> <input file> <output file>\n" \
        "             [-license] [-version] [-help]"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
    "   The convert2vector tool transforms point information into various external\n" \
    "   such as KML (for Google Earth) and shapefiles (for GIS).  It is useful\n" \
    "   for visualizing the geographical location of data by plotting corner\n" \
    "   coordinates in commonly available tools.  The ASCII text format, point\n" \
    "   and polygon, are used to define areas of interest.  The shapefile to text\n" \
    "   conversion is a simple text dump for a given shapefile.\n" \
    "   \n" \
    "   NOTE: This tool is still somewhat in beta.  Not all data format combinations\n" \
    "   are supported.  The following table shows which formats are (and are not)\n" \
    "   supported:\n" \
    "   \n" \
    "   Input Type          Output Type            Supported\n" \
    "   ----------       -------------------       ---------\n" \
    "    meta             polygon shapefile            Y\n" \
    "    meta             kml                          Y\n" \
    "    leader           polygon shapefile            Y\n" \
    "    shapefile        kml                          N\n" \
    "    leader           kml                          Y\n" \
    "    point            point shapefile              N\n" \
    "    point            kml                          N\n" \
    "    polygon          polygon shapefile            N\n" \
    "    polygon          kml                          N\n" \
    "    shape            text                         Y"

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        "   <input format>  This indicates the format of the data in the input file.\n" \
        "        Valid values: <point | polygon | meta | leader | shape>.  The types\n" \
        "        are defined as follows: 'point' means 'points defined in a text file',\n" \
        "        'polygon' means 'polygons define in a text file', 'meta' means\n" \
        "        'ASF Internal format metadata (.meta) file', 'leader' means 'CEOS format\n" \
        "        leader data file (.L, LED-), and 'shape' means 'shapes defined in a text file'.\n" \
        "   <input file>  The full name of th file containing the corner coordinate\n" \
        "        information.  Alternatively, the input file can contain a list of\n" \
        "        files that contain coordinates to convert (see the -list option below.)"

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
        "   <output format>  This indicates the desired output format based on\n" \
        "        how you would like to view the data.\n" \
        "        valid values: <shape | kml | text> where 'shape' means 'shapefile',\n" \
        "        'kml' means 'Google Earth file (.kml)', and 'text' means 'ASCII dump\n" \
        "        of a shapefile'.\n" \
        "   <output file>  The full name of the output file."

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
    "   -list\n" \
    "        This option flags the fact that the input file contains a list of\n" \
    "        filenames, each containing coordinates to convert, rather than\n" \
    "        being the data-containing file itself.\n" \
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
    "        To be determined.  In the case of the input file format for files\n" \
    "        containing points, polygons, and shapes, the format has not been\n" \
    "        completely decided upon.  When this has occurred then examples will\n" \
    "        be provided here (in a future release.)"

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

#endif // _CONVERT2VECTOR_HELP_H_

