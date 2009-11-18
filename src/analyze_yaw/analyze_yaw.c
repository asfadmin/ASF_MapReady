#define ASF_NAME_STRING "analyze_yaw"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <leader_file_list> <output_file\n"

#define ASF_DESCRIPTION_STRING \
"   This program extracts yaw and Doppler information out of a list of\n"\
"   leader files.\n"

#define ASF_INPUT_STRING \
"   The input file is required. The list of leader files is a plain ASCII\n"\
"   that contains a file name per line.\n"

#define ASF_OUTPUT_STRING \
"   The output file, also required, will be a command separated value\n"\
"   file containing columns for file name, yaw value and Doppler centroid\n"\
"   values in range and azimuth.\n"

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
  char *leader_file_list, *output_file, file[255];
  struct dataset_sum_rec *dssr;
  struct VFDRECV *facdr;
  int currArg = 1;
  int NUM_ARGS = 2;

  // process log/quiet/license/etc options
  handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

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
  
  leader_file_list = argv[currArg];
  output_file = argv[currArg+1];

  // Read granule information
  FILE *fpIn = FOPEN(leader_file_list, "r");
  FILE *fpOut = FOPEN(output_file, "w");
  fprintf(fpOut, "Leader_file, Yaw, Doppler_range, Doppler_azimuth\n");
  dssr = (struct dataset_sum_rec *) MALLOC(sizeof(struct dataset_sum_rec));
  facdr = (struct VFDRECV *) MALLOC(sizeof(struct VFDRECV));
  while (fgets(file, 1024, fpIn)) {
    file[strlen(file)-1] = '\0';
    if (get_dssr(file, dssr) < 0) {
      asfPrintWarning("Leader file (%s) does not contain data set summary"
		      " record\n", file);
      continue;
    }
    if (get_asf_facdr(file, facdr) < 0) {
      asfPrintWarning("Leader file (%s) does not contain facility related data"
		      " record\n", file);
      continue;
    }
    fprintf(fpOut, "%s, %.4lf, %.3lf, %.3lf\n", 
	    file, facdr->scyaw, dssr->crt_dopcen[0], dssr->alt_dopcen[0]);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(dssr);
  FREE(facdr);
  
  asfPrintStatus("Done.\n");
  return EXIT_SUCCESS;
}
