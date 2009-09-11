/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"meta_check"

#define ASF_USAGE_STRING \
"[ -log <file> ] [ -list ] <reference metadata> <test metadata>\n"\
"\n"\
"Additional option: -help"

#define ASF_DESCRIPTION_STRING \
"Meta_check determines the differences for essential parameters between\n"\
"two metadata files."

#define ASF_INPUT_STRING \
"<reference metadata>\n"\
"Reference metadata file to be tested against. This is a single metadata\n"\
"file unless the -list option is chosen. Then this is a list of metadata\n"\
"to be tested against.\n"\
"<test metadata>\n"\
"Metadata file to be tested. This is a single metadata file unless the -list\n"\
"option is chosen. Then this is a list of metadata to be tested.\n"

#define ASF_OUTPUT_STRING \
""

#define ASF_OPTIONS_STRING \
"-log	Keeps a log file in case things are run in batch mode.\n"\
"-list  Reads a list for reference and target metadata files.\n"

#define ASF_EXAMPLES_STRING \
"meta_check -log test.log fn1_old.meta fn1_new.meta"

#define ASF_LIMITATIONS_STRING \
"None known."

#define ASF_SEE_ALSO_STRING \
""

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"None."

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

#include <stdio.h>
#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

#define NUM_ARGS 2

/* usage - enter here on command-line usage error*/
void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         ASF_NAME_STRING
         " "
         ASF_USAGE_STRING
         "\n\n");
  exit (EXIT_FAILURE);
}

/* help_page - go here when the -help option is specified */
void help_page()
{
  if(system("echo '"
            "\n\n\n"
            "Tool name:\n" ASF_NAME_STRING "\n\n\n"
            "Usage:\n" ASF_USAGE_STRING "\n\n\n"
            "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
            "Input:\n" ASF_INPUT_STRING "\n\n\n"
            "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
            "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
            "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
            "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
            "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
            "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
            "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
            "' | less") != -1)
    exit(EXIT_SUCCESS);
  
  else if(system("echo '"
                 "\n\n\n"
                 "Tool name:\n" ASF_NAME_STRING "\n\n\n"
                 "Usage:\n" ASF_USAGE_STRING "\n\n\n"
                 "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
                 "Input:\n" ASF_INPUT_STRING "\n\n\n"
                 "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
                 "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
                 "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
                 "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
                 "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
                 "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
                 "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
                 "' | more") != -1)
    exit(EXIT_SUCCESS);
  
  else
    printf("\n\n\n"
           "Tool name:\n" ASF_NAME_STRING "\n\n\n"
           "Usage:\n" ASF_USAGE_STRING "\n\n\n"
           "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
           "Input:\n" ASF_INPUT_STRING "\n\n\n"
           "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
           "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
           "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
           "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
           "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
           "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
           "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n");
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

void check_metadata(meta_parameters *metaRef, meta_parameters *metaTest)
{
  int ii, vectors, errors=FALSE;
  double delta;

  // Check sensor
  if (strcmp(metaRef->general->sensor, metaTest->general->sensor) != 0) {
    asfPrintStatus("   Sensor differs - Reference: %s, target: %s\n",
		   metaRef->general->sensor, metaTest->general->sensor);
    errors = TRUE;
  }
  // Check orbit number
  if (metaRef->general->orbit != metaTest->general->orbit) {
    asfPrintStatus("   Orbit number differs - Reference: %i, target: %i\n",
		   metaRef->general->orbit, metaTest->general->orbit);
    errors = TRUE;
  }
  // Check frame number
  if (metaRef->general->frame != metaTest->general->frame) {
    asfPrintStatus("   Frame number differs - Reference: %i, target: %i\n",
		   metaRef->general->frame, metaTest->general->frame);
    errors = TRUE;
  }
  // Check beam mode
  if (strcmp(metaRef->general->mode, metaTest->general->mode) != 0) {
    asfPrintStatus("   Beam mode differs - Reference: %s, target: %s\n",
		   metaRef->general->mode, metaTest->general->mode);
    errors = TRUE;
  }
  // Check earth radius
  if (!FLOAT_EQUIVALENT(metaRef->sar->earth_radius, metaTest->sar->earth_radius)) {
    asfPrintStatus("   Earth radius differs - Delta: %.4lf m\n",
		   fabs(metaRef->sar->earth_radius - metaTest->sar->earth_radius));
    errors = TRUE;
  }
  // Check satellite height
  if (!FLOAT_EQUIVALENT(metaRef->sar->satellite_height, 
			metaTest->sar->satellite_height)) {
    asfPrintStatus("   Satellite height differs - Delta: %.4lf m\n",
		   fabs(metaRef->sar->satellite_height -
			metaTest->sar->satellite_height));
    errors = TRUE;
  }
  // Check Doppler values
  if (!FLOAT_EQUIVALENT(metaRef->sar->range_doppler_coefficients[0],
			metaTest->sar->range_doppler_coefficients[0])) {
    asfPrintStatus("   Range Doppler centroid differs - Delta: %.4lf Hz\n",
		   fabs(metaRef->sar->range_doppler_coefficients[0] -
			metaTest->sar->range_doppler_coefficients[0]));
    errors = TRUE;
  }
  if (!FLOAT_EQUIVALENT(metaRef->sar->range_doppler_coefficients[1],
			metaTest->sar->range_doppler_coefficients[1])) {
    asfPrintStatus("   Range Doppler per pixel differs - Delta: %g Hz/pixel\n",
		   fabs(metaRef->sar->range_doppler_coefficients[1] -
			metaTest->sar->range_doppler_coefficients[1]));
    errors = TRUE;
  }
  if (!FLOAT_EQUIVALENT(metaRef->sar->range_doppler_coefficients[2],
			metaTest->sar->range_doppler_coefficients[2])) {
    asfPrintStatus("   Range Doppler per pixel squared differs - Delta: %g Hz/"
		   "pixel^2\n",
		   fabs(metaRef->sar->range_doppler_coefficients[2] -
			metaTest->sar->range_doppler_coefficients[2]));
    errors = TRUE;
  }
  if (!FLOAT_EQUIVALENT(metaRef->sar->azimuth_doppler_coefficients[0],
			metaTest->sar->azimuth_doppler_coefficients[0])) {
    asfPrintStatus("   Azimuth Doppler centroid differs - Delta: %.4lf Hz\n",
		   fabs(metaRef->sar->azimuth_doppler_coefficients[0] -
			metaTest->sar->azimuth_doppler_coefficients[0]));
    errors = TRUE;
  }
  if (!FLOAT_EQUIVALENT(metaRef->sar->azimuth_doppler_coefficients[1],
			metaTest->sar->azimuth_doppler_coefficients[1])) {
    asfPrintStatus("   Azimuth Doppler per pixel differs - Delta: %g Hz/pixel\n",
		   fabs(metaRef->sar->azimuth_doppler_coefficients[1] -
			metaTest->sar->azimuth_doppler_coefficients[1]));
    errors = TRUE;
  }
  if (!FLOAT_EQUIVALENT(metaRef->sar->azimuth_doppler_coefficients[2],
			metaTest->sar->azimuth_doppler_coefficients[2])) {
    asfPrintStatus("   Azimuth Doppler per pixel squared differs - Delta: %g"
		   "Hz/pixel^2\n",
		   fabs(metaRef->sar->azimuth_doppler_coefficients[2] -
			metaTest->sar->azimuth_doppler_coefficients[2]));
    errors = TRUE;
  }
  // Check center latitude/longitude
  if (!FLOAT_EQUIVALENT(metaRef->general->center_latitude, 
			metaTest->general->center_latitude)) {
    delta = metaRef->general->center_latitude - metaTest->general->center_latitude;
    asfPrintStatus("   Center latitude differs - Reference latitude: %.4lf, "
		   "delta: %.5lf degrees\n", metaRef->general->center_latitude, 
		   delta);

    errors = TRUE;
  }
  if (!FLOAT_EQUIVALENT(metaRef->general->center_longitude, 
			metaTest->general->center_longitude)) {
    delta = metaRef->general->center_longitude - 
      metaTest->general->center_longitude;
    asfPrintStatus("   Center longitude differs - Reference longitude: %.4lf, "
		   "delta: %.5lf degrees\n", metaRef->general->center_longitude, 
		   delta);
    errors = TRUE;
  }
  // Check slant range to first pixel
  if (!FLOAT_EQUIVALENT(metaRef->sar->slant_range_first_pixel,
			metaTest->sar->slant_range_first_pixel)) {
    asfPrintStatus("   Slant range to first pixel differs - Delta: %.4lf m\n",
		   fabs(metaRef->sar->slant_range_first_pixel -
			metaTest->sar->slant_range_first_pixel));
    errors = TRUE;
  }
  // Check state vectors
  if (metaRef->state_vectors->year != metaTest->state_vectors->year) {
    asfPrintStatus("   Year for first state vector differs - Reference: %i, "
		   "target: %i\n", metaRef->state_vectors->year,
		   metaTest->state_vectors->year);
    errors = TRUE;
  }
  if (metaRef->state_vectors->julDay != metaTest->state_vectors->julDay) {
    asfPrintStatus("   Julian day of year for first state vector differs - "
		   "Reference: %i, target: %i\n", metaRef->state_vectors->julDay,
		   metaTest->state_vectors->julDay);
    errors = TRUE;
  }
  if (!FLOAT_EQUIVALENT(metaRef->state_vectors->second, 
			metaTest->state_vectors->second)) {
    asfPrintStatus("   Seconds of day for first state vector differs - "
		   "Delta: %.6lf s\n", fabs(metaRef->state_vectors->second -
					    metaTest->state_vectors->second));
    errors = TRUE;
  }
  if (metaRef->state_vectors->vector_count != 
      metaTest->state_vectors->vector_count) {
    asfPrintStatus("   Number of state vectors differs - Reference: %i, target: "
		   "%i\n", metaRef->state_vectors->vector_count,
		   metaTest->state_vectors->vector_count);
    errors = TRUE;
  }
  vectors = 3;
  for (ii=0; ii<vectors; ii++) {
    if (!FLOAT_EQUIVALENT(metaRef->state_vectors->vecs[ii].time,
			  metaTest->state_vectors->vecs[ii].time)) {
      asfPrintStatus("   Time for state vector %i differs - Delta: %.4lf s\n", ii+1,
		     fabs(metaRef->state_vectors->vecs[ii].time -
			  metaTest->state_vectors->vecs[ii].time));
      errors = TRUE;
    }
    if (!FLOAT_EQUIVALENT(metaRef->state_vectors->vecs[ii].vec.pos.x,
			  metaTest->state_vectors->vecs[ii].vec.pos.x)) {
      asfPrintStatus("   Position in x for state vector %i differs - Delta: "
		     "%.4lf m\n", ii+1, 
		     fabs(metaRef->state_vectors->vecs[ii].vec.pos.x -
			  metaTest->state_vectors->vecs[ii].vec.pos.x));
      errors = TRUE;
    }
    if (!FLOAT_EQUIVALENT(metaRef->state_vectors->vecs[ii].vec.pos.y,
			  metaTest->state_vectors->vecs[ii].vec.pos.y)) {
      asfPrintStatus("   Position in y for state vector %i differs - Delta: "
		     "%.4lf m\n", ii+1, 
		     fabs(metaRef->state_vectors->vecs[ii].vec.pos.y -
			  metaTest->state_vectors->vecs[ii].vec.pos.y));
      errors = TRUE;
    }
    if (!FLOAT_EQUIVALENT(metaRef->state_vectors->vecs[ii].vec.pos.z,
			  metaTest->state_vectors->vecs[ii].vec.pos.z)) {
      asfPrintStatus("   Position in z for state vector %i differs - Delta: "
		     "%.4lf m\n", ii+1, 
		     fabs(metaRef->state_vectors->vecs[ii].vec.pos.z -
			  metaTest->state_vectors->vecs[ii].vec.pos.z));
      errors = TRUE;
    }
    if (!FLOAT_EQUIVALENT(metaRef->state_vectors->vecs[ii].vec.vel.x,
			  metaTest->state_vectors->vecs[ii].vec.vel.x)) {
      asfPrintStatus("   Velocity in x for state vector %i differs - Delta: "
		     "%.4lf m/s\n", ii+1, 
		     fabs(metaRef->state_vectors->vecs[ii].vec.vel.x -
			  metaTest->state_vectors->vecs[ii].vec.vel.x));
      errors = TRUE;
    }
    if (!FLOAT_EQUIVALENT(metaRef->state_vectors->vecs[ii].vec.vel.y,
			  metaTest->state_vectors->vecs[ii].vec.vel.y)) {
      asfPrintStatus("   Velocity in y for state vector %i differs - Delta: "
		     "%.4lf m/s\n", ii+1, 
		     fabs(metaRef->state_vectors->vecs[ii].vec.vel.y -
			  metaTest->state_vectors->vecs[ii].vec.vel.y));
      errors = TRUE;
    }
    if (!FLOAT_EQUIVALENT(metaRef->state_vectors->vecs[ii].vec.vel.z,
			  metaTest->state_vectors->vecs[ii].vec.vel.z)) {
      asfPrintStatus("   Velocity in z for state vector %i differs - Delta: "
		     "%.4lf m/s\n", ii+1, 
		     fabs(metaRef->state_vectors->vecs[ii].vec.vel.z -
			  metaTest->state_vectors->vecs[ii].vec.vel.z));
      errors = TRUE;
    }
  }

  // Report success
  if (!errors)
    asfPrintStatus("   No differences found!\n");
}

/* Start of main progam */
int main(int argc, char *argv[])
{
  FILE *fpReference, *fpTest, *fpMeta;
  char szMetaReference[255], szMetaTarget[255], szListMetaReference[255];
  char szListMetaTarget[255], line[1024], *metaFile[255];
  char **testFile, **testSensor, **testMode;
  meta_parameters *metaRef, *metaTest;
  int currArg=1;
  int logFlag=FALSE, listFlag=FALSE, found=FALSE;
  int ii, targets=0, *testOrbit, *testFrame;
  
  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-list","--list",NULL)) {
      listFlag = TRUE;
    }
    else if (strmatches(key,"-help","--help",NULL))
      help_page();
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }
  
  // Fetch required arguments
  strcpy(szMetaReference,argv[argc - 2]);
  strcpy(szMetaTarget,argv[argc - 1]);

  if (listFlag) {
    // Assign list file names
    strcpy(szListMetaReference, szMetaReference);
    strcpy(szListMetaTarget, szMetaTarget);

    // Determine number of target metadata files
    fpTest = FOPEN(szListMetaTarget, "r");
    while (fgets(line, 1024, fpTest))
      targets++;
    FCLOSE(fpTest);

    // Reading essential target information in arrays
    testFile = (char **) MALLOC(sizeof(char)*targets);
    testSensor = (char **) MALLOC(sizeof(char)*targets);
    testMode = (char **) MALLOC(sizeof(char)*targets);
    for (ii=0; ii<targets; ii++) {
      testFile[ii] = MALLOC(sizeof(char)*512);
      testSensor[ii] = MALLOC(sizeof(char)*10);
      testMode[ii] = MALLOC(sizeof(char)*10);
    }
    testOrbit = (int *) MALLOC(sizeof(int)*targets);
    testFrame = (int *) MALLOC(sizeof(int)*targets);

    fpTest = FOPEN(szListMetaTarget, "r");
    for (ii=0; ii<targets; ii++) {
      fgets(line, 1024, fpTest);
      metaTest = meta_read(line);
      strcpy(testFile[ii], line);
      strcpy(testSensor[ii], metaTest->general->sensor);
      strcpy(testMode[ii], metaTest->general->mode);
      testOrbit[ii] = metaTest->general->orbit;
      testFrame[ii] = metaTest->general->frame;
    }
    FCLOSE(fpTest);

    // Open reference list files and match them up with target files
    fpReference = FOPEN(szListMetaReference, "r");
    while (fgets(line, 1024, fpReference)) {
      strcpy(szMetaReference, line);
      metaRef = meta_read(szMetaReference);
      for (ii=0; ii<targets; ii++) {
	if (strcmp(metaRef->general->sensor, testSensor[ii]) == 0 &&
	    strcmp(metaRef->general->mode, testMode[ii]) == 0 &&
	    metaRef->general->orbit == testOrbit[ii] &&
	    metaRef->general->frame == testFrame[ii]) {
	  found = TRUE;
	  break;
	}
      }
      if (found) {
	// Report on which files we are actually working
	asfPrintStatus("\n   Reference: %s", szMetaReference);
	asfPrintStatus("   Target: %s\n", testFile[ii]);
    
	// Read the metadata from target file and check things out
	fpTest = FOPEN(szListMetaTarget, "r");
	while (fgets(line, 1024, fpTest))
	  if (strcmp(testFile[ii], line) == 0) {
	    metaTest = meta_read(line);
	    check_metadata(metaRef, metaTest);
	    meta_free(metaRef);
	    meta_free(metaTest);
	  }
      }
      else {
	asfPrintStatus("   Could not find a match for '%s'\n\n", line);
	meta_free(metaRef);
      }
      FCLOSE(fpTest);
    }
    


    // Clean up
    FCLOSE(fpReference);
    FCLOSE(fpTest);
    meta_free(metaRef);
    meta_free(metaTest);
  }
  else {
    // Report on which files we are actually working
    asfPrintStatus("\n   Reference: %s\n", szMetaReference);
    asfPrintStatus("   Target: %s\n\n", szMetaTarget);
    
    // Read metadata
    metaRef = meta_read(szMetaReference);
    metaTest = meta_read(szMetaTarget);

    // Check metadata
    check_metadata(metaRef, metaTest);
    
    // Clean up
    meta_free(metaRef);
    meta_free(metaTest);
  }

  return(0);
}
