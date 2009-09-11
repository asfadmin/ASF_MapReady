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

// Look up EXIF entry
ExifEntry *get_exif_entry(ExifData *exif, ExifTag tag)
{
  int ii, kk;

  for (ii=0; ii<EXIF_IFD_COUNT; ii++) {
    if (exif->ifd[ii] && exif->ifd[ii]->count) {
      for (kk=0; kk<exif->ifd[ii]->count; kk++) {
        if (exif->ifd[ii]->entries[kk]->tag == tag) {
	  return exif->ifd[ii]->entries[kk];
        }
      }
    }
  }
  return exif_entry_new();
}

// Get value for EXIF tag
char *get_exif_tag(ExifData *exif, ExifTag tag)
{
  char *value = (char *) MALLOC(sizeof(char)*1024);
  const char *read;

  ExifEntry *entry = get_exif_entry(exif, tag);
  read = exif_entry_get_value(entry, value, 1024);
  return value;
}

// Main program body.
int
main (int argc, char *argv[])
{
  int currArg = 1;
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

  asfPrintStatus("Extracting acquisition for image list (%s)\n\n", inFile);

  FILE *fpImage, *fpOut;
  int width, height;
  double lat, lon, altitude, deg, min, sec;
  char image[1024], *shortImage, date_time[35];
  char lat_str[25], lon_str[25];
  char aperture[25], exposure[25], iso_speed[25], focal_length[25];

  fpImage = FOPEN(inFile, "r");
  fpOut = FOPEN(outFile, "w");
  fprintf(fpOut, "# Format: thermokarst\n");
  fprintf(fpOut, "# ID,width,height,date,lat,lon,altitude,aperture,exposure,iso_speed,focal_length\n");
  while (fgets(image, 1024, fpImage)) {
    image[strlen(image)-1] = '\0';
    shortImage = get_basename(image);
    ExifData *exif = exif_data_new_from_file(image);
    // Dimensions
    width = atoi(get_exif_tag(exif, EXIF_TAG_PIXEL_X_DIMENSION));
    height = atoi(get_exif_tag(exif, EXIF_TAG_PIXEL_Y_DIMENSION));
    //printf("Width: %d, Height: %d\n",width, height);
    // Date/time
    strcpy(date_time, get_exif_tag(exif, EXIF_TAG_DATE_TIME_ORIGINAL));
    //printf("Acquisition: %s\n", date_time);
    // Lat/lon/height
    strcpy(lat_str, get_exif_tag(exif, EXIF_TAG_GPS_LATITUDE));
    sscanf(lat_str, "%lf, %lf, %lf\n", &deg, &min, &sec);
    lat = deg + min/60.0 + sec/3600.0;
    if (strncmp_case(get_exif_tag(exif, EXIF_TAG_GPS_LATITUDE_REF), "S", 1) == 0)
      lat *= -1.0;
    strcpy(lon_str, get_exif_tag(exif, EXIF_TAG_GPS_LONGITUDE));
    sscanf(lon_str, "%lf, %lf, %lf\n", &deg, &min, &sec);
    lon = deg + min/60.0 + sec/3600.0;
    if (strncmp_case(get_exif_tag(exif, EXIF_TAG_GPS_LONGITUDE_REF), "W", 1) == 0)
      lon *= -1.0;
    altitude = atof(get_exif_tag(exif, EXIF_TAG_GPS_ALTITUDE));
    //printf("Lat: %.4lf deg, Lon: %.4lf deg, Altitude: %.0lf m\n", 
    //lat, lon, altitude);
    // Camera parameters
    strcpy(aperture, get_exif_tag(exif, EXIF_TAG_MAX_APERTURE_VALUE));
    strcpy(exposure, get_exif_tag(exif, EXIF_TAG_EXPOSURE_TIME));
    strcpy(iso_speed, get_exif_tag(exif, EXIF_TAG_ISO_SPEED_RATINGS));
    strcpy(focal_length, get_exif_tag(exif, EXIF_TAG_FOCAL_LENGTH));
    //printf("Aperture: %s\n", aperture);
    //printf("Exposure: %s\n", exposure);
    //printf("ISO speed: %s\n", iso_speed);
    //printf("Focal length: %s\n", focal_length);
    exif_data_free(exif);
    fprintf(fpOut, "\"%s\",%d,%d,\"%s\",%.4lf,%.4lf,%.0lf,",
                   shortImage, width, height, date_time, lat, lon, altitude);
    fprintf(fpOut, "\"%s\",\"%s\",\"%s\",\"%s\"\n", 
                   aperture, exposure, iso_speed, focal_length);
  }

  FCLOSE(fpImage);
  FCLOSE(fpOut);
  asfPrintStatus("\nDone.\n");
  FREE(shortImage);

  return EXIT_SUCCESS;
}
