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
  TOOL_NAME" [-list] [-input-format <format>] [-output-format <format>]\n" \
   "                  [-time] [-config <configuration file>]\n" \
   "                  <input filename> <output filename>\n" \
   "                  [-log <filename>] [-license] [-version] [-help]\n\n" \
   "   Where the input format is one of the following:\n" \
   "        meta      (ASF Internal Format metadata file)\n" \
   "        leader    (CEOS format leader data file, .L, LED-, etc)\n" \
   "        shape     (ESRI format shapefile, a set of files)\n" \
   "        point     (ASF-style CSV text file describing a set of points,\n"\
   "                       use -help for more info)\n" \
   "        polygon   (ASF-style CSV text file describing a single polygon\n"\
   "                       (per file), use -help for more info)\n" \
   "        geotiff   (GeoTIFF file)\n" \
   "        terrasar  (TerraSAR-X metadata file (.xml))\n" \
   "        csv       (Comma-separated value file)\n" \
   "        stf       (.PAR file for the Sky Telemetry Format)\n" \
   "\n"\
   "   And the output format is one of the following:\n" \
   "        shape     (ESRI format shapefile output)\n" \
   "        kml       (for viewing in Google Earth(tm) - DEFAULT)\n" \
   "        text      (An ASF-style polygon CSV text file (.csv)\n"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
   "   The convert2vector tool transforms point information into various\n"\
   "   other formats that are compatible with external applications such as\n"\
   "   Google Earth(tm) (KML files), GIS software (shapefiles), text\n"\
   "   editors and spreadsheets (CSV text files).  It is useful for\n"\
   "   visualizing the geographical location of data by plotting location\n" \
   "   coordinates in commonly available tools.  The ASF-style CSV text\n"\
   "   format (point and polygon) can used to define areas of interest or\n"\
   "   bounding polygons which can then be visualized in these external\n" \
   "   applications.\n"\
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
   "    geotiff          text                         Y\n" \
   "    terrasar         shape (polygon type)         Y\n" \
   "    terrasar         kml                          Y\n" \
   "    terrasar         text                         Y\n" \
   "    csv              shape (polygon or point)     Y\n" \
   "    csv              kml                          Y\n" \
   "    csv              text                    Not required\n" \
   "    stf              shape (polygon or point)     Y\n" \
   "    stf              kml                          Y\n" \
   "    stf              text                         Y\n"

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
   "   The input format indicates the format of the data in the input file.\n" \
   "   Valid values: point, polygon, meta, leader, shape, geotiff, csv, stf.\n"\
   "\n"\
   "   These types are defined as follows:\n"\
   "     point: points defined in a text file' (see examples below)\n"\
   "     polygon: single polygon defined in a text file (see examples below)\n"\
   "     meta: an ASF Internal format metadata (.meta) file\n" \
   "     leader: CEOS format leader data file (.L, LED-, etc)\n" \
   "     shape: shapes defined in a standard ESRI-type shapefile\n"\
   "     geotiff: a map-projected GeoTIFF (.tif) file\n"\
   "     terrasar: TerraSAR-X metadata file (.xml)\n"\
   "     csv: a text comma-separated value file\n" \
   "     stf: Sky Telemetry Fromat metadata file\n" \
   "\n"\
   "   The input file is the full name of the file containing the location\n"\
   "   coordinate information.  All file formats are used for extracting\n"\
   "   only corner coordinates except for the point and polygon text file\n"\
   "   formats (these two formats can contain more than just corner\n"\
   "   coordinates).\n" \
   "\n"\
   "   Alternatively, the input file can contain a list of\n" \
   "   filenames (one per line), each containing coordinates to convert\n"\
   "   (see the -list option, below).  In this case, all output will be\n"\
   "   combined into a single output file.\n"

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
   "   The output format indicates the desired output format based on\n" \
   "   how you would like to view or utilize the location coordinate data.\n" \
   "\n"\
   "   Valid output formats are: shape, kml, text.\n"\
   "     shape: standard ESRI-type shapefile\n" \
   "     kml: Google Earth(tm) file\n"\
   "     text: an ASF-style CSV text file (.csv) dump of the points or\n"\
   "           polygons found in the input file.\n" \
   "\n"\
   "   The output file is the full name of the desired output file.\n"\
   "\n"\
   "   NOTE: Choosing 'text' as the output format will produce a polygon\n"\
   "   type CSV file as described in the Examples section below, but if you\n"\
   "   wish the text file to be of point type instead, you may edit the\n"\
   "   first line in the text file which describes the file type, i.e.\n"\
   "   change the word 'polygon' to 'point'.\n"

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
   "   -list\n" \
   "        This option flags the fact that the input file contains a list\n"\
   "        of filenames (one per line), each containing coordinates to\n"\
   "        convert, rather than being the data-containing file itself.\n" \
   "\n"\
   "   -input-format\n" \
   "        By default, the input format will be determined from the\n"\
   "        contents of the input file itself, if it is possible.  If not,\n"\
   "        then an error will be printed asking that the -input-format\n"\
   "        option be used to explicity describe the input format.\n" \
   "        format.\n"\
   "\n"\
   "   -output-format\n" \
   "        By default, the output file format will be for Google Earth(tm)\n"\
   "        (a kml file).  If some other output format is desired, then the\n"\
   "        -output-format option should be used.\n"\
   "\n"\
   "   -time\n" \
   "        This KML specific option treats a list of input files as a time\n"\
   "        series. Each file in the list will have a time stamp associated\n"\
   "        with it.\n"\
   "   -config\n" \
   "        This option allows the user to specify format options and the\n"\
   "        list flag. Furthermore, options for drawing the boundary in KML\n"\
   "        files can be specified.\n" \
   "\n"\
   "   -log <log file>\n"\
   "        Output will be written to a specified log file.\n"\
   "\n"\
   "   -license\n" \
   "        Print copyright and license for this software then exit.\n" \
   "\n"\
   "   -version\n" \
   "        Print version and copyright then exit.\n" \
   "\n"\
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
    "   2. The first 2 lines are comment lines, those with 'file type' and\n"\
    "      \"ID\" in them, and are REQUIRED\n" \
    "   3  The required line containing \"# ID, latitude, longitude\"\n"\
    "      defines the columns.  At this time, data must be in latitude,\n"\
    "      longitude order (not vice versa).\n" \
    "   4. Values and text strings shall be separated by a comma, ','\n" \
    "   5. A line terminating in a comma is OK\n" \
    "   6. Only one point or coordinate may be listed per line\n" \
    "   7. Polygon text files do NOT need to list a closing point that is\n" \
    "      the same as the first (beginning) point.  Polygons are\n"\
    "      automatically closed by convert2vector.\n" \
    "   8. ID numbers will be interpreted as integers (truncated if\n"\
    "      necessary) and data values will be interpreted as floating point.\n"\
    "   9. Blank lines and white space before/after comma characters will be\n"\
    "      ignored\n" \
    "\n" \
    "   Example ASF-style text file containing point data:\n" \
    "\n" \
    "      # file type,point\n" \
    "      # ID,latitude,longitude\n" \
    "      # Created by ASF - Geophysical Institute - UAF\n" \
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
    "   CSV file notes:\n\n" \
    "   1. The first line in the file is the header line, which defines the\n"\
    "      data columns in the file.  All subseqent lines are the data\n"\
    "      lines.\n"\
    "   2. Lines beginning with the '#' character are comment lines\n" \
    "      You may add comment lines anywhere else in the file as long as\n" \
    "      they are after the first line.\n" \
    "   3. Point or polygon data is taken from columns which start with the\n"\
    "      characters 'lat' or 'lon'.  For example: 'Latitude1' or 'Lon_UL'\n"\
    "      There must be an equal number of lat and lon columns.\n" \
    "   4. Values and text strings shall be separated by a comma, ','\n" \
    "   5. Values and text strings may be quoted with double-quotes.  The\n" \
    "      quotes will be discarded.  To include the quote character in a\n" \
    "      quoted string, use two double-quotes in a row.\n"\
    "   7. Polygon text files do NOT need to list a closing point that is\n" \
    "      the same as the first (beginning) point.  Polygons are\n"\
    "      automatically closed by convert2vector.\n" \
    "   8. Blank lines and white space before/after comma characters will be\n"\
    "      ignored, unless surrounded by quotes.\n" \
    "   9. In the output, all columns are exported as attributes for the\n"\
    "      point or polygon.\n"\
    "  10. convert2vector attempts to determine the data type of each column\n"\
    "      by inspecting the column values.  At this time, the detected data\n"\
    "      types are: string, double, integer, logical.\n"\
    "\n" \
    "   Example CSV file:\n" \
    "\n" \
    "       ID,DATE,LAT1,LON1,LAT2,LON2,LOOK ANGLE\n" \
    "       1,11/27/07,41.9839,-124.263,41.8375,-120.9325,23.3\n" \
    "       2,11/30/07,41.89028,-122.79667,42.29389,-112.64056,23.3\n" \
    "       3,12/1/07,42.29389,-112.64056,41.885,-110.49084,37.7\n" \
    "\n" \
    "   In this file, the LAT/LON 1/2 columns define a 2-point polygon\n"\
    "   on each line. (Normally, you would have 4 or more points per\n"\
    "   polygon.)  The other columns are polygon attributes, ID would\n"\
    "   be set to an integer attribute, DATE a string, and LOOK ANGLE\n"\
    "   a double.\n"\
    "\n\n"

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
   "   c2v - Convert 2 Vector graphical user interface (GUI) version\n"\
   "   of convert2vector\n\n"

// Prototypes
void check_for_help(int argc, char* argv[]);
void usage(char *msg);
void print_help();
int checkForOption(char* key, int argc, char* argv[]);
int getDoubleOption(char *key, int argc, char* argv[], double *val, double def);
int getIntegerOption(char *key, int argc, char* argv[], int *val, int def);
int getStringOption(char *key, int argc, char *argv[], char *val, char *def);

#endif // _CONVERT2VECTOR_HELP_H_

