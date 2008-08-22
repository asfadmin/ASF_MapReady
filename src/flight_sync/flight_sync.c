#define ASF_NAME_STRING "flight_sync"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] <image list> <output>\n"

#define ASF_DESCRIPTION_STRING \
"     This program extracts time information out of a list of image (JPEG \n"\
"     format).\n\n"

#define ASF_INPUT_STRING \
"     The time stamps are extracted from a text file containing all image\n"\
"     file names. They can also contain full path information.\n"

#define ASF_OUTPUT_STRING \
"     The output is written into a command delimited text file.\n"

#define ASF_OPTIONS_STRING \
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit.\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <libexif/exif-data.h>
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
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " SVN_REV "\n\n");
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

// Look up location of a particular EXIF tag
static void lookup_exif_tag(ExifData *exif, ExifTag tag, int *ifd, int *entry)
{
  int ii, kk;

  for (ii=0; ii<EXIF_IFD_COUNT; ii++) {
    if (exif->ifd[ii] && exif->ifd[ii]->count) {
      for (kk=0; kk<exif->ifd[ii]->count; kk++) {
	if (exif->ifd[ii]->entries[kk]->tag == tag) {
	  *ifd = ii;
	  *entry = kk;
	}
      }
    }
  }
}

// Get value for EXIF tag
char *get_exif_tag(ExifData *exif, ExifTag tag)
{
  int ifd, entry;
  char value[1024];

  lookup_exif_tag(exif, tag, &ifd, &entry);
  return 
    exif_entry_get_value(exif->ifd[ifd]->entries[entry], value, sizeof(value));
}

// Main program body.
int
main (int argc, char *argv[])
{
  int currArg = 1;
  int kernel_size = -1;
  int NUM_ARGS = 2;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<=1)
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

  char *inFile, *outFile;
  inFile = argv[currArg];
  outFile = argv[currArg+1];

  asfPrintStatus("Extracting time stamps for image list (%s)\n\n", inFile);

  FILE *fpImage, *fpOut;
  char image[1024], *shortImage;

  fpImage = FOPEN(inFile, "r");
  fpOut = FOPEN(outFile, "w");
  while (fgets(image, 1024, fpImage)) {
    image[strlen(image)-1] = '\0';
    shortImage = get_basename(image);
    ExifData *exif = exif_data_new_from_file(image);
    ExifTag tag = EXIF_TAG_DATE_TIME_ORIGINAL;
    printf("%s, %s\n", shortImage, get_exif_tag(exif, tag));
    fprintf(fpOut, "%s, %s\n", shortImage, get_exif_tag(exif, tag));
    exif_data_free(exif);
  }

  FCLOSE(fpImage);
  FCLOSE(fpOut);
  asfPrintStatus("\nDone.\n");
  FREE(shortImage);

  return EXIT_SUCCESS;
}
