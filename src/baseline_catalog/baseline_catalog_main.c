#include "asf_baseline.h"

#define VERSION 2.0

static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <sensor> <beam mode> <input directory> <output directory>\n",
	 name);
  printf("\n"
	 "OPTIONAL ARGUMENTS:\n"
	 "   update: only baselines of pairs that involve orbits after this "
	 "orbit number are calculated.\n");
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   beam mode: FN1 to FN5 (R1), ST1 to ST7 (R1), STD (E1/E2)\n"
	 "   input directory: location of the SRF files\n"
	 "   output directory: location of all the output files\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   Calculates a baseline catalog for the given sensor and beam mode.\n");
  printf("\n"
	 "Version: %.2f, ASF InSAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  char *beam_mode, *sensor, *input_dir, *output_dir;
  currArg = 1;

  // Parse command line
  if (argc == 1)
    usage(argv[0]);

  if (currArg < (argc - 5)) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  
  sensor = (char *) MALLOC(sizeof(char)*25);
  sprintf(sensor, "%s", uc(argv[currArg]));
  beam_mode = (char *) MALLOC(sizeof(char)*10);
  sprintf(beam_mode, "%s", uc(argv[currArg+1]));
  input_dir = (char *) MALLOC(sizeof(char)*512);
  strcpy(input_dir, argv[currArg+2]);
  output_dir = (char *) MALLOC(sizeof(char)*512);
  strcpy(output_dir, argv[currArg+3]);

  asfSplashScreen (argc, argv);
  
  // Rock and roll
  baseline_catalog(sensor, beam_mode, input_dir, output_dir);

  // Clean up
  FREE(beam_mode);
  FREE(sensor);
  FREE(input_dir);
  FREE(output_dir);
  
  exit(0);
}
