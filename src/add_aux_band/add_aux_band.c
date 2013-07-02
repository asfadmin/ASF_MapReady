#define ASF_NAME_STRING "add_aux_band"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] <kind of values to calculate>\n"\
"          <in_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program writes an output image containing values of the\n"\
"     requested type as an additional band.\n"

#define ASF_INPUT_STRING \
"     The input file is required, and should be in ASF Internal format.\n"\
"     The kind of data to calculate is also required.  The supported types of\n"\
"     data that can be calculated:\n"\
"        incidence_angle      Ellipsoidal incidence angle (degrees)\n"\
"        look_angle           Look angle from the platform (degrees)\n"\
"        slant_range          Distance from the platform (m)\n"\
"        time                 Time from the beginning of acquisition (s)\n"\
"        doppler              Doppler value\n"\
"        yaw                  Yaw angle\n"\
"        lat                  Latitude\n"\
"        lon                  Longitude\n"\
"\n"\
"     Note that the lat,lon calculations can be very slow.  You may wish\n"\
"     to use the options in asf_geocode instead in order to save those\n"\
"     values.\n"\
"\n"\
"     Note also that some information cannot be calculated in some\n"\
"     images.  For example, imagery with no georeferencing information\n"\
"     cannot calculate lat/lon information.  Also, some images already\n"\
"     projected to ground range cannot recover doppler information.\n"\
"     Most images that have already been geocoded cannot recover any\n"\
"     SAR parameters.\n"

#define ASF_OUTPUT_STRING \
"     If the output file is in ASF internal format with one more band than\n"\
"     the input image, with the additional band containing data of the\n"\
"     requested type.  The additional band is added at the end.\n"\
"\n"\
"     The tool could be used several times if more than one type of data\n"\
"     is desired.\n"

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
"       > "ASF_NAME_STRING" incidence_angle <input basename> <output basename>\n"\
"       > "ASF_NAME_STRING" doppler <input basename> <output basename>\n"\
"\n"\
"     Extracting the new information without the input image:\n"\
"       > "ASF_NAME_STRING" incidence_angle INPUT TMP\n"\
"       > adjust_bands --bands INCIDENCE_ANGLE TMP ANGLES\n"

#define ASF_SEE_ALSO_STRING \
"     adjust_bands    May be used in conjunction with this tool to extract the\n"\
"                     requested data instead of adding it.\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

void add_aux_band(const char *inFile, const char *outFile, const char *type);

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
      "Version:\n"  TOOL_SUITE_NAME TOOL_SUITE_VERSION_STRING "\n\n");
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
  char *inFile, *outFile, *type;
  int currArg = 1;
  int NUM_ARGS = 3;

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
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
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

  type = argv[currArg];
  inFile = argv[currArg+1];
  outFile = argv[currArg+2];
  
  char *in_base = get_basename(inFile);
  char *out_base = get_basename(outFile);

  asfPrintStatus("Processing: %s -> %s\n", in_base, out_base);

  add_aux_band(inFile, outFile, type);
  asfPrintStatus("Done.\n");

  free(in_base);
  free(out_base);

  return EXIT_SUCCESS;
}

void add_aux_band(const char *inFile, const char *outFile, const char *type)
{
#define ADD_INCIDENCE_ANGLE 1
#define ADD_LOOK_ANGLE 2
#define ADD_SLANT_RANGE 3
#define ADD_DOPPLER 4
#define ADD_TIME 5
#define ADD_LAT 6
#define ADD_LON 7
#define ADD_YAW 8
  const char *band_str=MAGIC_UNSET_STRING;

  int what=0;
  if (strcmp_case(type, "incidence_angle") == 0) {
    asfPrintStatus("Adding Incidence Angle (degrees)\n");
    what=ADD_INCIDENCE_ANGLE;
    band_str = "INCIDENCE_ANGLE_ELLIPSOID";
  }
  else if (strcmp_case(type, "look_angle") == 0) {
    asfPrintStatus("Adding Look Angle (degrees)\n");
    what=ADD_LOOK_ANGLE;
    band_str = "LOOK_ANGLE";
  }
  else if (strcmp_case(type, "slant_range") == 0) {
    asfPrintStatus("Adding Slant Range (m)\n");
    what=ADD_SLANT_RANGE;
    band_str = "SLANT_RANGE";
  }
  else if (strcmp_case(type, "doppler") == 0) {
    asfPrintStatus("Adding Doppler(Hz)\n");
    what=ADD_DOPPLER;
    band_str = "DOPPLER";
  }
  else if (strcmp_case(type, "yaw") == 0) {
    asfPrintStatus("Adding Yaw Angle (degrees)\n");
    what=ADD_YAW;
    band_str = "YAW_ANGLE";
  }
  else if (strcmp_case(type, "time") == 0) {
    asfPrintStatus("Adding Time since start of acquisition (s)\n");
    what=ADD_TIME;
    band_str = "TIME";
  }
  else if (strcmp_case(type, "time") == 0) {
    asfPrintStatus("Adding Latitude (degrees)\n");
    what=ADD_LAT;
    band_str = "LAT";
  }
  else if (strcmp_case(type, "time") == 0) {
    asfPrintStatus("Adding Longitude (degrees)\n");
    what=ADD_LON;
    band_str = "LON";
  }
  else {
    asfPrintError("Unknown band type: %s\n", type);
  }

  char *inImg = appendExt(inFile, ".img");
  char *inMeta = appendExt(inFile, ".meta");
  char *outImg = appendExt(outFile, ".img");
  char *outMeta = appendExt(outFile, ".meta");

  int ii, jj, kk;
  meta_parameters *meta = meta_read(inMeta);
  meta_parameters *meta_out = meta_copy(meta);

  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;
  int nb = meta->general->band_count;

  strcat(meta_out->general->bands, ",");
  strcat(meta_out->general->bands, band_str);
  meta_out->general->band_count += 1; 

  float *buf = MALLOC(sizeof(float)*ns);
  FILE *fpIn = FOPEN(inImg, "rb");
  FILE *fpOut = FOPEN(outImg, "wb");

  // first writing the bands that are already there 
  asfPrintStatus("Copying original bands...\n");
  for (ii=0; ii<nl; ++ii) {
    for (kk=0; kk<nb; ++kk) {
      get_band_float_line(fpIn, meta, kk, ii, buf);
      put_band_float_line(fpOut, meta_out, kk, ii, buf);
    }
    asfLineMeter(ii,nl);
  } 
  fclose(fpIn);

  // now the new band
  asfPrintStatus("Adding the new band...\n");
  for (ii=0; ii<nl; ++ii) {
    for (jj=0; jj<ns; ++jj) {
      buf[jj] = 0;
      switch (what) {
        case ADD_INCIDENCE_ANGLE:
          buf[jj] = R2D*meta_incid(meta, ii, jj);
          break;
        case ADD_LOOK_ANGLE:
          buf[jj] = R2D*meta_look(meta, ii, jj);
          break;
        case ADD_SLANT_RANGE:
        {
          double time, slant;
          if (meta->sar) {
            meta_get_timeSlantDop(meta, ii, jj, &time, &slant, NULL);
            buf[jj] = slant;
          }
          break;
        }
        case ADD_DOPPLER:
        {
          double time, slant, dop;
          if (meta->sar) {
            meta_get_timeSlantDop(meta, ii, jj, &time, &slant, &dop);
            buf[jj] = dop;
          }
          break;
        }
        case ADD_YAW:
          if (meta->sar) {
            buf[jj] = R2D*meta_yaw(meta, ii, jj);
          }
          break;
        case ADD_TIME:
        {
          double time, slant;
          if (meta->sar) {
            meta_get_timeSlantDop(meta, ii, jj, &time, &slant, NULL);
            buf[jj] = time;
          }
          break;
        }
        case ADD_LAT:
        {
          double lat, lon;
          meta_get_latLon(meta, ii, jj, 0, &lat, &lon);
          buf[jj] = lat;
          break;
        }
        case ADD_LON:
        {
          double lat, lon;
          meta_get_latLon(meta, ii, jj, 0, &lat, &lon);
          buf[jj] = lon;
          break;
        }
        default:
          asfPrintError("add_aux_band - Internal Error %d\n", what);
      }
    }
    put_band_float_line(fpOut, meta_out, nb, ii, buf);
    asfLineMeter(ii,nl);
  }
  fclose(fpOut);
  FREE(buf);

  meta_write(meta_out, outMeta);

  FREE(inImg);
  FREE(outImg);
  FREE(inMeta);
  FREE(outMeta);

  meta_free(meta);
  meta_free(meta_out);
}

