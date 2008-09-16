#define ASF_NAME_STRING "plan"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet]\n"\
"          <satellite> <beam-mode> <start-date> <end-date>\n"\
"          <lat-min> <lat-max> <lon-min> <lon-max>\n"\
"          <tle-file> <out file>\n"

#define ASF_DESCRIPTION_STRING \
"     This program is an acquisition planner.\n"

#define ASF_INPUT_STRING \
"     Inputs are:\n"\
"       <satellite>  :  e.g., ALOS\n"\
"       <beam-mode>  :  e.g., FN1\n"\
"       <start-date> :  format is YYYYMMDD e.g., 20071231\n"\
"       <end-date>   :  format is YYYYMMDD e.g., 20071231\n"\
"       <lat-min>    :  in degrees\n"\
"       <lat-max>    :  in degrees\n"\
"       <lon-min>    :  in degrees\n"\
"       <lon-max>    :  in degrees\n"\
"       <tle-file>   :  Two-Line Element filename\n"

#define ASF_OUTPUT_STRING \
"     Output:\n"\
"       <out file>   :  output file\n"

#define ASF_OPTIONS_STRING \
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
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
"     > "ASF_NAME_STRING" blah blah blah\n\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_sar.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <libasf_proj.h>
#include <polygon.h>
#include <dateUtil.h>
#include <plan.h>

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
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_FAILURE);
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
  int currArg = 1;
  int NUM_ARGS = 10;

  // process log/quiet/license/etc options
  handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<=1)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();
  else if (argc<=NUM_ARGS)
      usage(ASF_NAME_STRING);

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else {
        --currArg;
        break;
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  char *satellite = argv[currArg];
  char *beam_mode = argv[currArg+1];
  long startdt = atol(argv[currArg+2]);
  if (!is_valid_date(startdt))
    asfPrintError("Invalid date: %s\nFormat should be: YYYYMMDD\n",
                  argv[currArg+2]);
  long enddt = atol(argv[currArg+3]);
  if (!is_valid_date(enddt))
    asfPrintError("Invalid date: %s\nFormat should be: YYYYMMDD\n",
                  argv[currArg+3]);
  double lat_min = atof(argv[currArg+4]);
  double lat_max = atof(argv[currArg+5]);
  double lon_min = atof(argv[currArg+6]);
  double lon_max = atof(argv[currArg+7]);
  char *tleFile = argv[currArg+8];
  //char *outFile = argv[currArg+9];

  double clat = (lat_max+lat_min)/2.;
  double clon = (lon_max+lon_min)/2.;

  // create lat/lon polygon in UTM coords
  double x[4], y[4];

  int zone;
  if (clat > 80)
    zone = 999;
  else if (clat < -80)
    zone = -999;
  else
    zone = utm_zone(clon);

  ll2pr(lat_min, lon_min, zone, &x[0], &y[0]);
  ll2pr(lat_min, lon_min, zone, &x[1], &y[1]);
  ll2pr(lat_min, lon_min, zone, &x[2], &y[2]);
  ll2pr(lat_min, lon_min, zone, &x[3], &y[3]);
  Poly *box = polygon_new_closed(4, x, y);

  char *err;
  PassCollection *pc;
  int num_found = plan(satellite, beam_mode, 28, startdt, enddt,
                       lat_min, lat_max, zone, clat, clon, 0, box,
                       tleFile, &pc, &err);

  polygon_free(box);

  if (num_found < 0) {
    asfPrintError("Plan error: %s\n", err);
  } else {
    asfPrintStatus("Found %d acquisition%s.\n",
                   num_found, num_found==1?"":"s");
    asfPrintStatus("Done.\n\n");

    int i;
    for (i=0; i<pc->num; ++i) {
      asfPrintStatus("#%d: %s (%d%%)\n", i, 
                     pc->passes[i]->start_time_as_string,
                     pc->passes[i]->total_pct);
    }

    pass_collection_free(pc);
  }

  return EXIT_SUCCESS;
}
