#define ASF_NAME_STRING "asf_calpol"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-c <classification file>]\n"\
"          [-pauli] [-sinclair] [-freeman] [-make-feasible-boundary <size>]\n"\
"          <in_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program decomposes SLC quad-pol data into data required\n"\
"     to build some common polarimetric decompositions.\n\n"\
"     Without the -c, -pauli, or -sinclair options, the output is a\n"\
"     nine-band image:\n"\
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
"       band 1: Classification band\n\n"\
"     When used with the -pauli option, the output is a 4-band image:\n"\
"       band 0: Amplitude (HH)\n"\
"       band 1: HH - VV (even bounce) [Pauli red]\n"\
"       band 2: 2*HV (rotated dihedral) [Pauli green]\n"\
"       band 3: HH + VV (odd bounce) [Pauli blue]\n\n"\
"     When used with the -sinclair option, the output is a 3-band image:\n"\
"       band 0: HH [Sinclair red]\n"\
"       band 1: (HV+VH)/2 [Sinclair green]\n"\
"       band 2: VV [Sinclair blue]\n\n"\
"     When used with the -freeman option, the output is a 4-band image with\n"\
"     an amplitude band, and the three Freeman-Durden bands:\n"\
"       band 0: Amplitude (HH)\n"\
"       band 1: Ps (single-bounce, Freeman-Durden Blue)\n"\
"       band 2: Pd (double-bounce, Freeman-Durden Red)\n"\
"       band 3: Pv (volume scatterer, Freeman-Durden Green)\n\n"

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
"          Cannot be used with -sinclair, -pauli, or -freeman\n"\
"\n"\
"     -pauli, -p\n"\
"          Outputs bands required to generate the Pauli decomposition.\n"\
"          Cannot be used with -c, -freeman, or -sinclair.\n"\
"\n"\
"     -sinclair, -s\n"\
"          Outputs bands required to generate the Sinclair decomposition.\n"\
"          Cannot be used with -c, -freeman, or -pauli.\n"\
"\n"\
"     -freeman, -f\n"\
"          Outputs bands required to generate the Freeman/Durden\n"\
"          decomposition.  Cannot be used with -c, -sinclair, or -pauli.\n"\
"\n"\
"     -make-feasible-boundary <size>\n"\
"          This is an option generally used internally only.\n\n"\
"          It generates a csv file with <size> points, containing the \n"\
"          bounary of the feasible region in entropy-alpha space.\n"\
"          This curve is independent of any particular data set, and can \n"\
"          be used without any input or output files.  The generated file\n"\
"          is called ea_boundary.txt.  If this file is present in the\n"\
"          classifications subdirectory of the ASF share directory during\n"\
"          processing, it will be added to the _ea_hist and _class_map\n"\
"          temporary files.\n"\
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
"     > "ASF_NAME_STRING" -pauli in_file out_file\n\n"

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
  int pauli = extract_flag_options(&argc,&argv,"-pauli","-p",NULL);
  int sinclair = extract_flag_options(&argc,&argv,"-sinclair","-s",NULL);
  int freeman = extract_flag_options(&argc,&argv,"-freeman","-f",NULL);

  int sz;
  int make_boundary_file =
    extract_int_options(&argc,&argv,&sz,"-make-feasible-boundary",NULL);

  if (make_boundary_file) {
    const char *fname = "ea_boundary.txt";
    asfPrintStatus("Generating entropy/alpha boundary curve file: %s\n",
                   fname);
    asfPrintStatus("Number of points: %d\n", sz);
    make_entropy_alpha_boundary(fname, sz);

    // if no other arguments were supplied, we are done - can exit
    // without making a fuss.
    if (argc==1) {
      asfPrintStatus("Done.\n");
      exit(1);
    }
  }

  if (classify + pauli + sinclair + freeman > 1) {
    asfPrintError("Use only one of the -pauli, -sinclair, -freeman "
                  "or -c options.\n");
  }

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
    asfPrintError("Debug mode no longer available.\n");
    //cpx2debug(inFile,outFile);
  }
  else if (classify) {
    polarimetric_decomp(inFile,outFile,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        classFile,1);    
  }
  else if (pauli) {
    cpx2pauli(inFile,outFile,TRUE);
  }
  else if (sinclair) {
    cpx2sinclair(inFile,outFile,FALSE);
  }  
  else if (freeman) {
    cpx2freeman_durden(inFile,outFile,TRUE);
  }  
  else {
    polarimetric_decomp(inFile,outFile,0,1,2,3,4,5,6,7,8,9,-1,-1,-1,NULL,-1);
  }


  asfPrintStatus("Done.\n");
  return EXIT_SUCCESS;
}
