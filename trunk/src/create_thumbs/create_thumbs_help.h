#ifndef _CREATE_THUMBS_HELP_H_
#define _CREATE_THUMBS_HELP_H_

// Uncomment to enable JAXA Level 0 support
#define JL0_GO

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
        "create_thumbs"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#ifdef JL0_GO
#define TOOL_USAGE \
        TOOL_NAME" [-log <logfile>] [-quiet] [-verbose] [-size <size>]\n"\
"                 [-recursive] [-out-dir <dir>]\n"\
"                 [-L0 <stf|ceos|jaxa_L0>] [-output-format <tiff|jpeg>]\n"\
"                 [-scale <scale_factor>] [-browse] [-save-metadata] [-help]\n"\
"                 <files>"
#else
#define TOOL_USAGE \
        TOOL_NAME" [-log <logfile>] [-quiet] [-verbose] [-size <size>]\n"\
"                 [-recursive] [-out-dir <dir>]\n"\
"                 [-L0 <stf|ceos>] [-output-format <tiff|jpeg>]\n"\
"                 [-scale <scale_factor>] [-browse] [-save-metadata] [-help]\n"\
"                 <files>"
#endif

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
"     This program takes any number of files and generates thumbnail (or\n"\
"     browse) images.  If a directory is specified, all files in that\n"\
"     directory are processed.  If -R is specified, any subdirectories\n"\
"     are also processed, recursively.\n\n"\
"     The generated thumbnails have the same basename as the input\n"\
"     file but with '_thumb.jpg' or '_thumb.tif' added.  If -browse\n"\
"     is specified, the output file name will be the basename with\n"\
"     just '.jpg' or '.tif' added."

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
"     At least one input file or directory is required.\n"

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
"     The program will produce one thumbnail for each input file found.\n"

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#ifdef JL0_GO
#define TOOL_OPTIONS \
"     -size <size>\n"\
"          Generate thumbnails (or browse images) of the given size in pixels.\n"\
"          The default is 1024 pixels.  If the input image isn't square, the longer\n"\
"          side will be scaled to the given size, the other dimension will be\n"\
"          determined so as to keep the same aspect ratio.\n"\
"          NOTE: Cannot be used together with the -scale option.\n"\
"\n"\
"     -recursive (-R, -r)\n"\
"          Recurse into subdirectories, looking for additional CEOS files\n"\
"          to generate thumbnails for.\n"\
"\n"\
"     -out-dir (-output-dir, -o)\n"\
"          Specify a directory where all thumbnails are placed.  Without\n"\
"          this option, all thumbnails are placed in the same directory as\n"\
"          the CEOS file.\n"\
"\n"\
"     -L0 <stf|ceos|jaxa_L0>\n"\
"          Force Level 0 (zero) processing.  'stf' or 'ceos' indicates input file\n"\
"          format.  Level 0 stf or ceos processing results in the following additional\n"\
"          processing steps:\n\n"\
"            - import into ASF Internal Format\n"\
"            - range-doppler (ardop) processing\n"\
"            - conversion of amplitude output from slant range to ground range\n"\
"            - scale ground range amplitude image to size indicated by -size or -scale\n"\
"              command line parameters\n"\
"            - image is flipped/rotated to north-up, west-left orientation\n"\
"            - image is exported to the selected output graphics file format\n"\
"          jaxa_L0 Level 0 processing refers to JAXA AVNIR-2 Level 0 (optical)\n"\
"          processing, and the processing steps are therefore:\n"\
"            - import into ASF Internal Format\n"\
"            - scale image to size indicated by -size or -scale\n"\
"              command line parameters\n"\
"            - (image is NOT flipped/rotated to north-up, west-left orientation)\n"\
"              (... this capability is coming soon however.)\n"\
"            - image is exported to the selected output graphics file format\n"\
"\n"\
"     -output-format <tiff|jpeg>\n"\
"          Choose graphics file format for output file.  Default is JPEG.\n"\
"\n"\
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -quiet (-q)\n"\
"          Supresses all non-error output.\n"\
"\n"\
"     -verbose (-v)\n"\
"          Prints out files that were ignored (i.e., not CEOS files).\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version (-v)\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -scale (-s)\n"\
"          Downscale factor, i.e. -scale 8 will result in an output image\n"\
"          scaled to 1/8th the original size.  Scaling will use the closest\n"\
"          integer value.\n"\
"\n"\
"     -browse (-b)\n"\
"          For browse images that should be named <basename>.ext rather\n"\
"          than <basename>_thumb.ext.  This option prevends '_thumb' from\n"\
"          being appended to the output basename.\n"\
"\n"\
"     -save-metadata\n"\
"          Results in all metadata files (intermediate and final) to be saved\n"\
"          in the output directory.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit."
#else
#define TOOL_OPTIONS \
"     -size <size>\n"\
"          Generate thumbnails (or browse images) of the given size in pixels.\n"\
"          The default is %d pixels.  If the input image isn't square, the longer\n"\
"          side will be scaled to the given size, the other dimension will be\n"\
"          determined so as to keep the same aspect ratio.\n"\
"          NOTE: Cannot be used together with the -scale option.\n"\
"\n"\
"     -recursive (-R, -r)\n"\
"          Recurse into subdirectories, looking for additional CEOS files\n"\
"          to generate thumbnails for.\n"\
"\n"\
"     -out-dir (-output-dir, -o)\n"\
"          Specify a directory where all thumbnails are placed.  Without\n"\
"          this option, all thumbnails are placed in the same directory as\n"\
"          the CEOS file.\n"\
"\n"\
"     -L0 <stf|ceos>\n"\
"          Force Level 0 (zero) processing.  'stf' or 'ceos' indicates input file\n"\
"          format.  Level 0 stf or ceos processing results in the following additional\n"\
"          processing steps:\n\n"\
"            - import into ASF Internal Format\n"\
"            - range-doppler (ardop) processing\n"\
"            - conversion of amplitude output from slant range to ground range\n"\
"            - scale ground range amplitude image to size indicated by -size or -scale\n"\
"              command line parameters\n"\
"            - image is flipped/rotated to north-up, west-left orientation\n"\
"            - image is exported to the selected output graphics file format\n"\
"\n"\
"     -output-format <tiff|jpeg>\n"\
"          Choose graphics file format for output file.  Default is JPEG.\n"\
"\n"\
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -quiet (-q)\n"\
"          Supresses all non-error output.\n"\
"\n"\
"     -verbose (-v)\n"\
"          Prints out files that were ignored (i.e., not CEOS files).\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version (-v)\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -scale (-s)\n"\
"          Downscale factor, i.e. -scale 8 will result in an output image\n"\
"          scaled to 1/8th the original size.\n"\
"\n"\
"     -browse (-b)\n"\
"          For browse images that should be named <basename>.ext rather\n"\
"          than <basename>_thumb.ext.  This option prevends '_thumb' from\n"\
"          being appended to the output basename.\n"\
"\n"\
"     -save-metadata\n"\
"          Results in all metadata files (intermediate and final) to be saved\n"\
"          in the output directory.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit."
#endif

// TOOL_EXAMPLES is required but is allowed to be an empty string
#ifdef  TOOL_EXAMPLES
#undef  TOOL_EXAMPLES
#endif
#define TOOL_EXAMPLES \
"     Generate thumbnails for all files in the current directory:\n"\
"     > "TOOL_NAME" .\n\n"\
"     Generate thumbnails for files in the directory n60s:\n"\
"     > "TOOL_NAME" n60s\n\n"\
"     Generate thumbnails for all files in the current directory,\n"\
"     and all subdirectories.\n"\
"     > "TOOL_NAME" -r .\n\n"\
"     Generate a large thumbnail for the single file file1.D:\n"\
"     > "TOOL_NAME" -size 1024 file.D\n\n" \
"     Generate a browse image for an STF Level 0 file:\n"\
"     > "TOOL_NAME" -L0 stf -browse -scale 8 file.000\n\n"

// TOOL_LIMITATIONS is required but is allowed to be an empty string
#ifdef  TOOL_LIMITATIONS
#undef  TOOL_LIMITATIONS
#endif
#define TOOL_LIMITATIONS \
"     1. The output file naming convention is not user-customizable."

// TOOL_SEE_ALSO is required but is allowed to be an empty string
#ifdef  TOOL_SEE_ALSO
#undef  TOOL_SEE_ALSO
#endif
#define TOOL_SEE_ALSO \
"    mapready\n"\
"    asf_mapready"

// Prototypes
void check_for_help(int argc, char* argv[]);
void usage();
void print_help();

#endif // _CREATE_THUMBS_HELP_H_

