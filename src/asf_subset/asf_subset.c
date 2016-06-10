#define ASF_NAME_STRING "asf_subset"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-shape <file>]\n"\
"          [-latlon <minLat> <maxLat> <minLon> <maxLon>]\n"\
"          [-map <startX> <startY> <endX> <endY>]\n"\
"          [-xy <startX> <startY> <sizeX> <sizeY>] <inFile> <outFile>\n"

#define ASF_DESCRIPTION_STRING \
"     This program allows users to subset an image by line/sample ranges,\n"\
"     lat/lon ranges or shapefile polygon.\n"

#define ASF_INPUT_STRING \
"     The input file in ASF internal format can be map projected as well as\n"\
"     in its native projection.\n"

#define ASF_OUTPUT_STRING \
"     The output file will be an ASF internal file that will have the same\n"\
"     characteristics as the input file.\n"

#define ASF_OPTIONS_STRING \
"     -shape <file>\n"\
"          Defines the shapefile that contains the polygon.\n"\
"\n"\
"     -latlon\n"\
"          Takes the definition for the subset as geographic dimensions.\n"\
"\n"\
"     -map\n"\
"          Takes the definition for the subset in map coordinates.\n"\
"\n"\
"     -xy\n"\
"          Takes the definition for the subset in image coordinates.\n"\
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
"          The input file is: example.img (and example.meta)\n"\
"          The input file contains 8 bands (e.g., quad-pol palsar data)\n"\
"          of amplitude and phase data, like this:\n"\
"            AMP_HH,PHASE_HH,AMP_HV,PHASE_HV,AMP_VH,PHASE_VH,AMP_VV,PHASE_VV\n"\
"          and you wanted just the HH and VV amplitudes:\n"\
"            > adjust_bands -bands AMP_HH,AMP_VV example output\n"\
"\n"\
"          Suppose you have an ampitude image as amp.img (and amp.meta)\n"\
"          and an interferogram image as interf.img (together with\n"\
"          interf.meta) that you wanted to combine together.\n"\
"          In amp.meta the band_names string is AMP, and in interf.meta\n"\
"          the band_names string is INTERFEROGRAM.  Then:\n"\
"            > adjust_bands -bands AMP,INTERFEROGRAM amp interf output\n"\
"\n"\
"          To simple combine two separate files into a single multi-band\n"\
"          file, you can leave out the -bands option (or, specify all the\n"\
"          bands, if you would like to change the order):\n"\
"            > adjust_bands in1 in2 out\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_vector.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <asf_raster.h>
#include <smap.h>

// Print minimalistic usage info & exit
static void usage(char *name)
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
      "Version:\n   %s\n\n",
      version_string(ASF_NAME_STRING));
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
int main (int argc, char *argv[])
{
  int *start = NULL, nParts, nVertices, currArg = 1, NUM_ARGS = 2;
  int latlon = FALSE, map = FALSE, smap = FALSE, projected = FALSE;
  long long startX, startY, sizeX, sizeY;
  double minX, maxX, minY, maxY;
  char *shapeFile = NULL, tmpDir[1024];
  double *lat, *lon;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<=1)
    usage(argv[0]);
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
    else if (strmatches(key,"-quiet","--quiet","-q",NULL))
      quietflag = TRUE;
    else if (strmatches(key,"-latlon","--latlon",NULL)) {
      latlon = TRUE;
      CHECK_ARG(4);
      maxX = atof(GET_ARG(1));
      minX = atof(GET_ARG(2));
      maxY = atof(GET_ARG(3));
      minY = atof(GET_ARG(4));
    }
    else if (strmatches(key,"-map","--map",NULL)) {
      map = TRUE;
      CHECK_ARG(4);
      maxX = atof(GET_ARG(1));
      minX = atof(GET_ARG(2));
      maxY = atof(GET_ARG(3));
      minY = atof(GET_ARG(4));
    }
    else if (strmatches(key,"-xy","--xy",NULL)) {
      CHECK_ARG(4);
      sizeY = atof(GET_ARG(1));
      sizeX = atof(GET_ARG(2));
      startY = atof(GET_ARG(3));
      startX = atof(GET_ARG(4));
    }
    else if (strmatches(key,"-shape","--shape",NULL)) {
      latlon = TRUE;
      CHECK_ARG(1);       
      shapeFile = (char *) MALLOC(sizeof(char)*512);
      strcpy(shapeFile,GET_ARG(1));
    }
    else {
      --currArg;
      break;
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  char *inFile = argv[currArg];
  char *outFile = argv[currArg+1];

  // Check whether we deal with SMAP data, as that needs additional processing
  meta_parameters *meta = meta_read(inFile);
  if (strcmp_case(meta->general->sensor, "SMAP") == 0)
    smap = TRUE;
  if (meta->projection && meta->projection->type != SCANSAR_PROJECTION &&
    latlon == FALSE) {
    map = TRUE;
    projected = TRUE;
  }

  // Create a temporary directory, if we needed it
  if (shapeFile) {
    sprintf(tmpDir, "%s%s-", get_dirname(outFile), get_basename(outFile));
    strcat(tmpDir, time_stamp_dir());
    create_clean_dir(tmpDir);
  }

  // Put geographic information into the appropriate arrays
  if (shapeFile) {
    shape2latlon(shapeFile, &lat, &lon, &start, &nParts, &nVertices);
    if (map) {
      int ii;
      int nl = meta->general->line_count;
      int ns = meta->general->sample_count;
      double line, sample;
      double minSample = ns, maxSample = 0.0, minLine = nl, maxLine = 0.0;
      for (ii=0; ii<nVertices; ii++) {
        meta_get_lineSamp(meta, lat[ii], lon[ii], 0.0, &line, &sample);
        if (line < minLine)
          minLine = line;
        if (line > maxLine)
          maxLine = line;
        if (sample < minSample)
          minSample = sample;
        if (sample > maxSample)
          maxSample = sample;
      }
      startX = minSample;
      startY = minLine;
      sizeX = maxSample - minSample;
      sizeY = maxLine - minLine;
    }
  }
  else if (latlon) {
    nVertices = 4;
    lat = (double *) MALLOC(sizeof(double)*nVertices);
    lon = (double *) MALLOC(sizeof(double)*nVertices);
    lat[0] = maxY;
    lon[0] = minX;
    lat[1] = maxY;
    lon[1] = maxX;
    lat[2] = minY;
    lon[2] = maxX;
    lat[3] = minY;
    lon[3] = minX;
    int dateline = crosses_dateline(lon, 0, nVertices);
    if (dateline && maxX < 0) {
      lon[1] += 360.0;
      lon[2] += 360.0;
    }
  }
  meta_free(meta);

  // Get on with the subsetting
  if (latlon && !map) {
    if (smap) {
      if (shapeFile) {
        char *tmpFile = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+15));
        sprintf(tmpFile, "%s%csubset.img", tmpDir, DIR_SEPARATOR);
        char *tmp2File = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+15));
        sprintf(tmp2File, "%s%cupdate_meta.img", tmpDir, DIR_SEPARATOR);
        asfPrintStatus("Clipping to polygon extent ...\n");
        clip_to_polygon(inFile, tmpFile, lat, lon, start, nParts, nVertices);
        asfPrintStatus("\nShrinking image to polygon extent ...\n");
        trim_zeros_ext(tmpFile, tmp2File, TRUE, TRUE, TRUE);
        update_smap_geolocation(tmp2File, outFile);
        FREE(tmpFile);      
        remove_dir(tmpDir);
        
      }
      else {
        asfPrintStatus("Subsetting to geographic box extent ...\n");
        subset_by_latlon(inFile, outFile, lat, lon, nVertices);
      }
    }
    else {
      if (shapeFile) {
        char *tmpFile = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+15));
        sprintf(tmpFile, "%s%csubset.img", tmpDir, DIR_SEPARATOR);
        char *tmp2File = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+15));
        sprintf(tmp2File, "%s%cclip.img", tmpDir, DIR_SEPARATOR);
        asfPrintStatus("Subsetting to geographic box extent ...\n");
        subset_by_latlon(inFile, tmpFile, lat, lon, nVertices);
        asfPrintStatus("\nClipping to polygon extent ...\n");
        clip_to_polygon(tmpFile, tmp2File, lat, lon, start, nParts, nVertices);
        asfPrintStatus("\nShrinking image to polygon extent ...\n");
        trim_zeros_ext(tmp2File, outFile, TRUE, TRUE, TRUE);
        FREE(tmpFile);
        FREE(tmp2File);
        remove_dir(tmpDir);
      }
      else {
        asfPrintStatus("Subsetting to geographic box extent ...\n");
        subset_by_latlon(inFile, outFile, lat, lon, nVertices);
      }
    }
  }
  else if (map) {
    if (!projected)
      asfPrintError("Input file needs to be projected for this option!\n");
    if (shapeFile) {
      char *tmpFile = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+15));
      sprintf(tmpFile, "%s%csubset.img", tmpDir, DIR_SEPARATOR);
      asfPrintStatus("Subsetting to map projected box extent ...\n");
      trim(inFile, tmpFile, startX, startY, sizeX, sizeY);
      asfPrintStatus("\nClipping to polygon extent ...\n");
      clip_to_polygon(tmpFile, outFile, lat, lon, start, nParts, nVertices);
      FREE(tmpFile);      
      remove_dir(tmpDir);
    }
    else {
      asfPrintStatus("Subsetting to map projected box extent ...\n");
      subset_by_map(inFile, outFile, minX, maxX, minY, maxY);    
    }
  }
  else
    trim(inFile, outFile, startX, startY, sizeX, sizeY);

  asfPrintStatus("Done.\n");

  return EXIT_SUCCESS;
}
