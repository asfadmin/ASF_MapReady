#include "asf_baseline.h"

#define VERSION 2.0

static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <beam mode>\n",
	 name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   beam mode: FN1 to FN5, ST1 to ST7\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   Calculates the Radarsat baseline catalog for the given beam mode.\n");
  printf("\n"
	 "Version: %.2f, ASF InSAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  char *beam_mode;

  /* parse command line */
  if (argc != 2) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  
  beam_mode = (char *) MALLOC(sizeof(char)*10);
  sprintf(beam_mode, "%s", uc(argv[currArg]));  
  asfSplashScreen (argc, argv);
  
  baseline_catalog(beam_mode);

  exit(0);
}
