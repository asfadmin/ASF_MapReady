#define ASF_NAME_STRING "asf_calpol"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-c <classification file>]\n"\
"          [-debug] <in_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program decomposes SLC quad-pole data into data required\n"\
"     to build some common polarimetric decompositions.\n\n"\
"     Without the -c option, the output is a nine-band image:\n"\
"       band 0: Amplitude (HH)\n"\
"       band 1: HH - VV (even bounce) [Pauli red]\n"\
"       band 2: 2*HV (rotated dihedral) [Pauli green]\n"\
"       band 3: HH + VV (odd bounce) [Pauli blue]\n"\
"       band 4: Entropy\n"\
"       band 5: Anisotropy\n"\
"       band 6: Alpha\n"\
"       band 7: HH [Sinclair red]\n"\
"       band 8: (HV+VH)/2 [Sinclair green]\n"\
"       band 9: VV [Sinclair blue]\n\n"\
"     When used with the -c option, the output is a 2-band image:\n"\
"       band 0: Amplitude (HH)\n"\
"       band 1: Classification band\n"\
"     With the -debug option, debug bands are added.\n\n"

#define ASF_INPUT_STRING \
"     The input file is required, and should be in ASF Internal format.\n"

#define ASF_OUTPUT_STRING \
"     The output file, also required, will be in ASF Internal format.\n"

#define ASF_OPTIONS_STRING \
"     -c <classification file>\n"\
"          Uses the entropy, anisotropy and alpha values to classify\n"\
"          the output according to the classification scheme defined\n"\
"          in the given file.  The file may be in the current directory\n"\
"          or in the ASF share directory.\n\n"\
"          cloude8.cla and cloude16.cla are available for producing\n"\
"          Cloude-Pottier 8 and 16 class classifications.\n"\
"\n"\
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
"     > "ASF_NAME_STRING" in_file out_file\n\n"

#define ASF_SEE_ALSO_STRING \
"     asf_export\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_sar.h>
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
  char *inFile, *outFile;
  int currArg = 1;
  int NUM_ARGS = 2;

  // process log/quiet/license/etc options
  handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  char classFile[255];
  int classify = extract_string_options(&argc,&argv,classFile,"-c",NULL);
  int debug = extract_flag_options(&argc,&argv,"-debug","-d",NULL);

  if (argc<=1)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();
  else if (argc<=2)
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

  inFile = argv[currArg];
  outFile = argv[currArg+1];

  if (debug) {
    cpx2debug(inFile,outFile);
  }
  else if (classify) {
    polarimetric_decomp(inFile,outFile,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        classFile,1);    
  }
  else {
    polarimetric_decomp(inFile,outFile,0,1,2,3,4,5,6,7,8,9,NULL,-1);
  }


  asfPrintStatus("Done.\n");
  return EXIT_SUCCESS;
}
