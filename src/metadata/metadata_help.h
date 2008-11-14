#ifndef _METADATA_HELP_H_
#define _METADATA_HELP_H_

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
        "metadata"

// TOOL_USAGE is required
#ifdef  TOOL_USAGE
#undef  TOOL_USAGE
#endif
#define TOOL_USAGE \
        TOOL_NAME" <option> <infile> [-license] [-version] [-help]\n"

// TOOL_DESCRIPTION is required
#ifdef  TOOL_DESCRIPTION
#undef  TOOL_DESCRIPTION
#endif
#define TOOL_DESCRIPTION \
        "   The metadata program retrieves data record contents from CEOS format\n" \
        "   metadata files (a.k.a. \"leader files\") ...if the requested record\n" \
        "   exists."

// TOOL_INPUT is required but is allowed to be an empty string
#ifdef  TOOL_INPUT
#undef  TOOL_INPUT
#endif
#define TOOL_INPUT \
        "   <infile>  The basename of the input metadata (leader) file"

// TOOL_OUTPUT is required but is allowed to be an empty string
#ifdef  TOOL_OUTPUT
#undef  TOOL_OUTPUT
#endif
#define TOOL_OUTPUT \
        "   If the -save option is used, then a set of files named after the\n" \
        "   basename, but with a record identifier suffix appended as a file\n" \
        "   extension will be produced, i.e. basename.dssr"

// TOOL_OPTIONS is required but is allowed to be an empty string
#ifdef  TOOL_OPTIONS
#undef  TOOL_OPTIONS
#endif
#define TOOL_OPTIONS \
    "   -save        Write the records to an output file\n" \
    "   -all         All records\n" \
    "   -dssr        Data Set Summary record\n" \
    "   -shr         Scene Header record\n" \
    "   -mpdr        Map Projection Data Record\n" \
    "   -ppdr        Platform Position Data record\n" \
    "   -atdr        Attitude Data record\n" \
    "   -ampr        ALOS Map Projection Data\n" \
    "   -radr        Radiometric Data record\n" \
    "   -ardr        ALOS Radiometric Data record\n" \
    "   -rcdr        Radiometric Compensation Data record\n" \
    "   -dqsr        Data Quality Summary record\n" \
    "   -pdhr        Processed Data Histograms record\n" \
    "   -sdhr        Signal Data Histograms record\n" \
    "   -rasr        Range Spectra record\n" \
    "   -ppr         Processing Parameter record\n" \
    "   -facdr       ASF or ESA Facility Related Data record\n" \
    "   -asf_facdr   Force ASF Facility Related Data record\n" \
    "   -esa_facdr   Force ESA Facility Related Data record\n" \
    "   -jaxa_facdr  Force JAXA Facility Related Data record\n" \
    "   -ifdr        Image File Descriptor record\n" \
    "   -lfdr        Leader File Descriptor record\n" \
    "   -tfdr        Trailer File Descriptor record\n" \
    "   -meta        Generate ASF internal metadata file\n\n" \
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

#endif // _METADATA_HELP_H_

