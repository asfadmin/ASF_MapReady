#define ASF_NAME_STRING "populate_meta_field"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <meta_file> <field_name> <value>\n"

#define ASF_DESCRIPTION_STRING \
"     This program populates a metadata field with a specified value.\n"

#define ASF_INPUT_STRING \
"     All command line items are required.\n"\

#define ASF_OUTPUT_STRING \
"     There is no separate output file, the specified metadata file is\n"\
"     updated with the specified value.\n"

#define ASF_OPTIONS_STRING \
"     <field_name>\n"\
"          Separate the block name with the actual field by a colon (:)\n"\
"          For example, insar:master_image.\n\n"\
"          Not all fields are supported, yet.  Here are the fields that\n"\
"          can be updated using this tool:\n"\
"            insar:master_image\n"\
"            insar:slave_image\n"\
"            insar:master_acquisition_date\n"\
"            insar:slave_acquisition_date\n"\
"            general:center_longitude\n"\
"\n"\
"     <value>\n"\
"          The value to populate with.  Specifying a string for a numerical\n"\
"          field will result in a 0 for the field's value.  A special case\n"\
"          is general:center_longitude, where you may specify 'CALC' and\n"\
"          this value will be calculatedi from the state vectors (if\n"\
"          available) and then populated.  If the value cannot be calculated\n"\
"          the value will be left unchanged.\n"\
"\n"\
"     -quiet\n"\
"          Supresses all non-essential output.\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit.\n"

#define ASF_EXAMPLES_STRING \
"     > "ASF_NAME_STRING" <.meta file> <field name> <field value>\n"\
"     > "ASF_NAME_STRING" test.meta insar:master_image R1_64077_0151\n"\
"     > "ASF_NAME_STRING" test.meta general:center_longitude CALC\n"

#define ASF_SEE_ALSO_STRING \
"     \n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

// Print minimalistic usage info & exit
static void usage(const char *name)
{
  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
  exit(EXIT_FAILURE);
}

// Print the help info & exit
static void print_help(void)
{
  asfPrintStatus(
      "\n"
      "Tool name:\n   " ASF_NAME_STRING "\n\n"
      "Usage:\n" ASF_USAGE_STRING "\n"
      "Description:\n" ASF_DESCRIPTION_STRING "\n"
      "Input:\n" ASF_INPUT_STRING "\n"
      "Output:\n"ASF_OUTPUT_STRING "\n"
      "Options:\n" ASF_OPTIONS_STRING "\n"
      "Examples:\n" ASF_EXAMPLES_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_SUCCESS);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}

// Main program body.
int
main (int argc, char *argv[])
{
  char *metaFile, *fieldName;
  char fieldValue[130];
  int currArg = 1;
  int NUM_ARGS = 3;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<3)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else {
      --currArg;
      break;
    }
  }

  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.  Expected %d, got %d.\n",
           NUM_ARGS, argc-currArg);
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  metaFile = appendExt(argv[currArg], ".meta");
  fieldName = argv[currArg+1];
  strncpy_safe(fieldValue, argv[currArg+2], 128);

  if (!fileExists(metaFile))
    asfPrintError("Not found: %s\n", metaFile);

  meta_parameters *meta = meta_read(metaFile);
  if (!meta) asfPrintError("Failed to read metadata: %s\n", metaFile);

  if (strcmp_case(fieldName, "insar:master_image")==0) {
    if (!meta->insar) 
      asfPrintError("Cannot populate insar:master_image, no insar block!");
    strcpy(meta->insar->master_image, fieldValue);
  }
  else if (strcmp_case(fieldName, "insar:slave_image")==0) {
    if (!meta->insar) 
      asfPrintError("Cannot populate insar:slave_image, no insar block!");
    strcpy(meta->insar->slave_image, fieldValue);
  }
  else if (strcmp_case(fieldName, "insar:master_acquisition_date")==0) {
    if (!meta->insar) 
      asfPrintError("Cannot populate insar:master_acquisition_date, "
                    "no insar block!");
    strcpy(meta->insar->master_acquisition_date, fieldValue);
  }
  else if (strcmp_case(fieldName, "insar:slave_acquisition_date")==0) {
    if (!meta->insar) 
      asfPrintError("Cannot populate insar:slave_acquisition_date, "
                    "no insar block!");
    strcpy(meta->insar->slave_acquisition_date, fieldValue);
  }
  else if (strcmp_case(fieldName, "general:center_longitude")==0) {
    if (strcmp_case(fieldValue, "CALC")==0) {
      int nl = meta->general->line_count;
      int ns = meta->general->sample_count;
      double lat, lon;
      meta_get_latLon(meta, nl/2, ns/2, 0.0, &lat, &lon);
      if (meta_is_valid_double(lon)) {
        asfPrintStatus("Original center longitude: %f\n",
                       meta->general->center_longitude);
        asfPrintStatus("Populating center longitude: %f\n", lon);
        meta->general->center_longitude = lon;
      }
    }
    else {
      meta->general->center_longitude = atof(fieldValue);
    }
  }
  else {
    asfPrintError("Unsupported field name: %s\n", fieldName);
  }

  meta_write(meta, metaFile);

  asfPrintStatus("Done.\n");
  return EXIT_SUCCESS;
}
