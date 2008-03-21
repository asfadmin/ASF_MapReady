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
        TOOL_NAME" [-list] [-input-format <input format>] [-output-format <output format>]\n" \
        "                  <input file> <output file>\n" \
        "                  [-license] [-version] [-help]\n\n" \
        "   Where the input format is one of the following:\n" \
        "        meta      (ASF Internal Format metadata file)\n" \
        "        leader    (CEOS format leader data file, .L, LED-, etc)\n" \
        "        shape     (ESRI format shapefile, a set of files)\n" \
        "        point     (ASF-style CSV text file describing a set of points, use -help for more info)\n" \
        "        polygon   (ASF-style CSV text file describing a single polygon (per file), use -help for more info)\n" \
        "        geotiff   (GeoTIFF file)\n\n" \
        "   And the output format is one of the following:\n" \
        "        shape     (ESRI format shapefile output)\n" \
        "        kml       (Google Earth(tm) kml file output for viewing in Google Earth(tm) - DEFAULT)\n" \
        "        text      (An ASF-style polygon CSV text file (.csv) or a shape file dump (.txt)\n" \
        "                   depending on the type of input file)\n"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
    "   The convert2vector tool transforms point information into various other\n" \
    "   formats that are compatible with external applications such as Google Earth(tm) (KML\n" \
    "   files), GIS software (shapefiles), text editors, and spreadsheets (CSV text files).  It\n" \
    "   is useful for visualizing the geographical location of data by plotting location\n" \
    "   coordinates in commonly available tools, or in the case of shape files, seeing a summary\n" \
    "   of the shape file contents.  The ASF-style CSV text format (point and polygon)\n" \
    "   can used to define areas of interest or bounding polygons which can then be visualized\n" \
    ".  in Google Earth(tm) (etc).  The shapefile to text conversion is a simple text dump for a given\n" \
    "   shapefile (not an ASF-style CSV file containing point locations or polygon corners.)\n" \
    "   \n" \
    "   NOTE: With the exception of using an ASF-style text file containing point data, all shapefile\n" \
    "   type output is into polygon type shapefiles (not point, line, polyline etc).  Shapefiles\n" \
    "   derived from point text files will be point type shapefiles.  See I/O matrix below.\n" \
    "   \n" \
    "   NOTE: Choosing 'text' as the output format will produce a polygon type CSV file\n" \
    "   as described in the Examples section below, but if you wish the text file to be of\n" \
    "   point type instead, you may edit the first line in the text file which describes\n" \
    "   the file type, i.e. change the word 'polygon' to 'point'.  Note that when using\n" \
    "   convert2vector to convert a shapefile to a text file, that an ASCII text dump is\n" \
    "   is provided instead (see Examples below.)\n" \
    "   \n" \
    "   Input Type          Output Type            Supported\n" \
    "   ----------       -------------------       ---------\n" \
    "    meta             shape (polygon type)         Y\n" \
    "    meta             kml                          Y\n" \
    "    meta             text                         Y\n" \
    "    leader           shape (polygon type)         Y\n" \
    "    leader           kml                          Y\n" \
    "    leader           text                         Y\n" \
    "    point            shape (point type)           Y\n" \
    "    point            kml                          Y\n" \
    "    point            text                   Not applicable\n" \
    "    polygon          shape (polygon type)         Y\n" \
    "    polygon          kml                          Y\n" \
    "    polygon          text                   Not applicable\n" \
    "    shape            shape                  Not applicable\n" \
    "    shape            kml                          Y\n" \
    "    shape            text                         Y\n" \
    "    geotiff          shape (polygon type)         Y\n" \
    "    geotiff          kml                          Y\n" \
    "    geotiff          text                         Y\n"

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        "   <input format>  This indicates the format of the data in the input file.\n" \
        "        Valid values: <point | polygon | meta | leader | shape | geotiff>.  These\n" \
        "        types are defined as follows: 'point' means 'points defined in a text file',\n" \
        "        (see examples below), 'polygon' means 'a single polygon defined in a text file'\n" \
        "        (see examples below), 'meta' means an 'ASF Internal format metadata (.meta) file',\n" \
        "        'leader' means 'CEOS format leader data file (.L, LED-), 'shape' means 'shapes\n" \
        "        defined in a standard ESRI-type shapefile', and 'geotiff' means 'A map-projected\n" \
        "        GeoTIFF (.tif) file'.\n" \
        "   <input file>  The full name of th file containing the location coordinate\n" \
        "        information.  All file formats are used for extracting only corner coordinate\n" \
        "        except for the point and polygon text file formats.  Obviously, these two formats\n" \
        "        can contain far more than just corner coordinate.\n" \
        "\n" \
        "        Alternatively, the input file can contain a list of\n" \
        "        filenames (one per line), each containing coordinates to convert (see the -list\n" \
        "        option below.)  In this case, all output will be into a single output file.\n"

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
        "   <output format>  This indicates the desired output format based on\n" \
        "        how you would like to view or utilize the location coordinate data.\n" \
        "        Valid values include: <shape | kml | text> where 'shape' means 'shapefile',\n" \
        "        'kml' means 'Google Earth(tm) file (.kml)', and 'text' means either an 'ASF-style\n" \
        "        CSV text file (.csv)' dump of the points or polygons found in the input file or\n" \
        "        a shapefile text dump (.txt) if the input file was a shape file.\n" \
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
    "   -input-format\n" \
    "        By default, the input format will be determined from the contents of the\n" \
    "        input file itself if it is possible.  If not, then an error will be printed\n" \
    "        asking that the -input-format option be utilized to explicity describe the input\n" \
    "        format.  The input format may be one of the following:\n" \
    "             meta      (ASF Internal Format metadata file)\n" \
    "             leader    (CEOS format leader data file, .L, LED-, etc)\n" \
    "             shape     (ESRI format shapefile, a set of files)\n" \
    "             point     (ASF-style CSV text file describing a set of points, use -help for more info)\n" \
    "             polygon   (ASF-style CSV text file describing a single polygon (per file), use -help for more info)\n" \
    "             geotiff   (GeoTIFF file)\n" \
    "   -output-format\n" \
    "        By default, the output file format will be for Google Earth(tm) (a kml file).  If some\n" \
    "        other output format is desired, then the -output-format option should be used.  The\n" \
    "        output format may be one of the following:\n" \
    "             shape     (ESRI format shapefile output)\n" \
    "             kml       (Google Earth(tm) kml file output for viewing in Google Earth(tm) - DEFAULT)\n" \
    "             text      (An ASF-style polygon CSV text file or shape file dump)\n" \
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
    "   Input text file (point, polygon) rules:\n\n" \
    "   1. Lines beginning with the '#' character are comment lines\n" \
    "      You may add comment lines anywhere else in the file as long as\n" \
    "      they are after the first 2 (required) comment lines.\n" \
    "   2. The first 2 lines are comment lines, those with 'file type' and \"ID\"\n" \
    "      in them, and are REQUIRED\n" \
    "   3  The required line containing \"# ID, latitude, longitude\" defines the columns.\n" \
    "      At this time, data must be in latitude, longitude order (not vice versa).\n" \
    "   4. Values and text strings shall be separated by a comma, ','\n" \
    "   5. A line terminating in a comma is OK\n" \
    "   6. Only one point or coordinate may be listed per line\n" \
    "   7. Polygon text files do NOT need to list a closing point that is\n" \
    "      the same as the first (beginning) point.  Polygons are automatically\n" \
    "      closed by convert2vector.\n" \
    "   8. ID numbers will be interpreted as integers (truncated if necessary) and\n" \
    "      data values will be interpreted as floating point.\n" \
    "   9. Blank lines and white space before/after comma characters will be ignored\n" \
    "\n" \
    "   Example ASF-style text file containing point data:\n" \
    "\n" \
    "      # file type,point\n" \
    "      # ID,latitude,longitude\n" \
    "      # Created by Alaska Satellite Facility - Geophysical Institute - University of Alaska Fairbanks\n" \
    "      # 28-February-2008\n" \
    "       1, 41.9839,  -124.263  \n" \
    "       2, 41.8375,  -120.9325 \n" \
    "       3, 41.89028, -122.79667\n" \
    "       4, 42.29389, -112.64056\n" \
    "       5, 41.885,   -110.49084\n" \
    "       6, 40.98084, -110.79889\n" \
    "       7, 38.80667, -110.76278\n" \
    "       8, 38.07528, -111.36472\n" \
    "       9, 36.97917, -111.73167\n" \
    "      10, 37.00639, -112.49084\n" \
    "      11, 34.88334, -114.48278\n" \
    "      12, 34.22472, -114.15056\n" \
    "      13, 32.77028, -114.48084\n" \
    "      14, 32.53389, -114.86584\n" \
    "      15, 32.4125,  -117.16   \n" \
    "      16, 33.09667, -117.36472\n" \
    "      17, 33.51278, -117.84361\n" \
    "      18, 33.69,    -118.43389\n" \
    "      19, 33.95778, -118.97195\n" \
    "      20, 34.19695, -119.56583\n" \
    "\n" \
    "   Example ASF-style text file containing polygon data:\n" \
    "\n" \
    "      # File type,polygon,\n" \
    "      # ID,latitude,longitude\n" \
    "      1,41.9839,-124.263\n" \
    "      2,41.8375,-120.9325\n" \
    "      3,41.89028,-122.79667\n" \
    "      4,42.29389,-112.64056\n" \
    "      5,41.885,-110.49084\n" \
    "      6,40.98084,-110.79889\n" \
    "      7,38.80667,-110.76278\n" \
    "      8,38.07528,-111.36472\n" \
    "      # Note: The following point is only approximate\n" \
    "      9,36.97917,-111.73167\n" \
    "      10,37.00639,-112.49084\n" \
    "      11,34.88334,-114.48278\n" \
    "      12,34.22472,-114.15056\n" \
    "      13,32.77028,-114.48084\n" \
    "      14,32.53389,-114.86584\n" \
    "      15,32.4125,-117.16\n" \
    "      16,33.09667,-117.36472\n" \
    "      17,33.51278,-117.84361\n" \
    "\n" \
    "   Example of a shapefile text dump:\n" \
    "\n" \
    "      NAME OF SHAPEFILE: CITIES\n" \
    "      Number of structures: 1\n" \
    "      Shape type: Polygon\n" \
    "\n" \
    "      Structure: 1\n" \
    "\n" \
    "      Number of fields: 4\n" \
    "      CITY_FIPS: 82130\n" \
    "      CITY_NAME: WALDEN\n" \
    "      SHAPE_Leng:    4848.79682739000\n" \
    "      SHAPE_Area:  900379.91342500004\n\n"

// TOOL_LIMITATIONS is required but is allowed to be an empty string
#ifdef  TOOL_LIMITATIONS
#undef  TOOL_LIMITATIONS
#endif
#define TOOL_LIMITATIONS \
    "   See I/O matrix in the Description section above.\n"

// TOOL_SEE_ALSO is required but is allowed to be an empty string
#ifdef  TOOL_SEE_ALSO
#undef  TOOL_SEE_ALSO
#endif
#define TOOL_SEE_ALSO \
    "   c2v - Convert 2 Vector graphical user interface (GUI) version of convert2vector\n"

// Prototypes
void check_for_help(int argc, char* argv[]);
void usage(char *msg);
void print_help();
int checkForOption(char* key, int argc, char* argv[]);
int getDoubleOption(char *key, int argc, char* argv[], double *val, double def);
int getIntegerOption(char *key, int argc, char* argv[], int *val, int def);
int getStringOption(char *key, int argc, char *argv[], char *val, char *def);

#endif // _CONVERT2VECTOR_HELP_H_

