#include "asf_baseline.h"
#include <sys/stat.h>
#include <sys/types.h>

#define VERSION 2.0

/*
#define S_IFMT   0170000
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
*/

static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <sensor> <beam mode> <input directory> <output directory>\n",
	 name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   sensor: satellite sensor\n"
	 "   beam mode: FN1 to FN5 (R1), ST1 to ST7 (R1), STD (E1/E2)\n"
	 "   input directory: location of the SRF files\n"
	 "   output directory: location of the filtered SRF files\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   Filters SRFs of given beam mode from one directory into another\n");
  printf("\n"
	 "Version: %.2f, ASF InSAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int get_basic_info(const char *inFile, char *sensor, char *mode)
{
  FILE *fp;
  char line[1024], sensorSRF[5], modeSRF[10];
  int i, nSegments = 0;
  int foundSRF = FALSE, foundSensor = FALSE, foundMode = FALSE;

  // Check whether the file is in fact a scan results file
  fp = FOPEN(inFile, "r");
  while (fgets(line, 1024, fp)) {

    if (strstr(line, "OBJECT = SCAN_RESULTS_FILE")) {
      foundSRF = TRUE;
      while (fgets(line, 1024, fp)) {

        if (strstr(line, "OBJECT = BODY") &&
            strncmp(line, " OBJECT = BODY", 14) == 0) {
          while (fgets(line, 1024, fp)) {

            // Look for platform (sensor)
            if (strstr(line, "PLATFORM") &&
                strncmp(line, "  PLATFORM", 10) == 0) {
              sscanf(line, "  PLATFORM = \"%s\"", sensorSRF);
              sensorSRF[2] = 0;
              if (strcmp_case(sensor, sensorSRF) == 0)
                foundSensor = TRUE;
	      break;
              //printf("sensor: %s\n", sensorSRF);
            }
          }
        }

        // Look for segments
        if (strstr(line, "SEGMENT_COUNT")) {
          sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
          for (i=0; i<nSegments; i++) {

            while (fgets(line, 1024, fp)) {

              if (strstr(line, "OBJECT = SEGMENT") &&
                  strncmp(line, "  OBJECT = SEGMENT", 18) == 0) {

                // Look for beam mode
                while (fgets(line, 1024, fp)) {
                  if (strstr(line, "MODE")) {
                    sscanf(line, "   MODE = \"%s\"", modeSRF);
                    modeSRF[3] = 0;
                    if (strcmp_case(mode, modeSRF) == 0)
                      foundMode = TRUE;
                    //printf("mode: %s\n", modeSRF);
                    break;
                  }
                }

              } // while inside OBJECT = SEGMENT

              if (strstr(line, "END_OBJECT = SEGMENT") &&
                  strncmp(line, "  END_OBJECT = SEGMENT", 22)==0)
                break;

            } // if within a SEGMENT object

          } // while looking for SEGMENTS objects

        } // looping through a SEGMENT object (for)

      } // if SEGMENT_COUNT was found

    }
  }
  FCLOSE(fp);
  if (foundSensor && foundMode)
    return TRUE;
  else
    return FALSE;
}

void filter_srfs(char *sensor, char *beam_mode, char *input_dir, 
		 char *output_dir)
{
  struct dirent *dp;
  struct stat statbuf;
  DIR *dir;
  int nFiles= 0;
  char file_name[25], source[1024], target[1024];

  // Some more error checking on beam modes
  if (strcmp_case(sensor, "R1") == 0) {
    if (strcmp_case(beam_mode, "FN1") != 0 &&
        strcmp_case(beam_mode, "FN2") != 0 &&
        strcmp_case(beam_mode, "FN3") != 0 &&
        strcmp_case(beam_mode, "FN4") != 0 &&
        strcmp_case(beam_mode, "FN5") != 0 &&
        strcmp_case(beam_mode, "ST1") != 0 &&
        strcmp_case(beam_mode, "ST2") != 0 &&
        strcmp_case(beam_mode, "ST3") != 0 &&
        strcmp_case(beam_mode, "ST4") != 0 &&
        strcmp_case(beam_mode, "ST5") != 0 &&
        strcmp_case(beam_mode, "ST6") != 0 &&
        strcmp_case(beam_mode, "ST7") != 0)
      asfPrintError("Unknown beam mode '%s' for sensor '%s'.\n",
                    beam_mode, sensor);
  }
  else if (strcmp_case(sensor, "E1") == 0 || strcmp_case(sensor, "E2") == 0 ||
           strcmp_case(sensor, "J1") == 0) {
    if (strcmp_case(beam_mode, "STD") != 0)
      asfPrintError("Unknown beam mode '%s' for sensor '%s'.\n",
                    beam_mode, sensor);
  }

  // Read directory for scan results files of sensor + beam mode
  asfPrintStatus("Filtering scan results file directory ...\n\n");
  chdir(input_dir);
  dir = opendir(input_dir);
  while ((dp = readdir(dir)) != NULL) {

    if (stat(dp->d_name, &statbuf) == -1)
      continue;
    sprintf(file_name, "%s", dp->d_name);
    if (get_basic_info(file_name, sensor, beam_mode) &&
	!S_ISDIR(statbuf.st_mode)) {
      sprintf(source, "%s/%s", input_dir, file_name);
      sprintf(target, "%s/%s", output_dir, file_name);
      fileCopy(source, target);
      nFiles++;
      asfPrintStatus("\rNumber of scan results files read: %6d", nFiles);
    }
  }
  closedir(dir);
  printf("\n");
}


int main(int argc, char *argv[])
{
  char *sensor, *beam_mode, *input_dir, *output_dir;
  currArg = 1;

  // Parse command line
  if (argc == 1)
    usage(argv[0]);

  if (currArg < (argc - 5)) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  
  sensor = (char *) MALLOC(sizeof(char)*10);
  sprintf(sensor, "%s", uc(argv[currArg]));
  beam_mode = (char *) MALLOC(sizeof(char)*10);
  sprintf(beam_mode, "%s", uc(argv[currArg+1]));
  input_dir = (char *) MALLOC(sizeof(char)*512);
  strcpy(input_dir, argv[currArg+2]);
  output_dir = (char *) MALLOC(sizeof(char)*512);
  strcpy(output_dir, argv[currArg+3]);

  asfSplashScreen (argc, argv);
  
  // Rock and roll
  filter_srfs(sensor, beam_mode, input_dir, output_dir);

  // Clean up
  FREE(beam_mode);
  FREE(input_dir);
  FREE(output_dir);
  
  exit(0);
}
