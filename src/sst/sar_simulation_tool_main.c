#include "asf_simulation.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <dem> <satellite> <beam mode> <tle> <orbit direction>\n", name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   dem:              DEM the simulated amplitude image is derived from\n"
	 "   satellite:        satellite name\n"
	 "   beam mode:        beam mode \n"
         "   tle:              name of the TLE file containing orbit information\n"
	 "   orbit direction:  'ascending' or 'descending'\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   Simulates a SAR image for the defined geometry.\n");
  printf("\n"
	 "Version: %.2f, ASF InSAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  // Allocate some memory
  char *demFile = (char *) MALLOC(sizeof(char)*255);
  char *satellite = (char *) MALLOC(sizeof(char)*15);
  char *beam_mode = (char *) MALLOC(sizeof(char)*15);
  char *tleFile = (char *) MALLOC(sizeof(char)*255);
  sat_t *tle = (sat_t *) MALLOC(sizeof(sat_t));
  satellite_t *sat = (satellite_t *) MALLOC(sizeof(satellite_t));

  // Parse command line
  if (argc < 6) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }  
  strcpy(demFile, argv[1]);
  strcpy(satellite, argv[2]);
  strcpy(beam_mode, argv[3]);
  strcpy(tleFile, argv[4]);
  strcpy(sat->orbit_direction, argv[5]);

  asfSplashScreen (argc, argv);
  
  // Read TLE file
  read_tle(tleFile, satellite, tle);

  // Read satellite configuration file
  sat = read_satellite_config("satellite.config", satellite, beam_mode);
  strcpy(sat->orbit_direction, argv[5]);
  strcpy(sat->satellite, satellite);

  // Simulate SAR image
  sar_simulation_tool(demFile, sat, tle);

  // Clean up
  FREE(demFile);
  FREE(satellite);
  FREE(beam_mode);
  FREE(tleFile);
  FREE(tle);
  FREE(sat);

  exit(0);
}
