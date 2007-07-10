#include "asf_simulation.h"

#define VERSION 1.0

static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <dem> <satellite> <beam mode> <tle> <orbit direction>\n", name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   dem:              DEM the simulated amplitude image is derived from\n"
	 "   satellite:        satellite height above the ground\n"
	 "   beam mode:        look angle at scene center\n"
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
  satellite_t *sat;
  tle_t *tle;
  char *demFile, *satellite, *beam_mode, *orbit_direction, *tleFile;

  // Allocate some memory
  demFile = (char *) MALLOC(sizeof(char)*255);
  satellite = (char *) MALLOC(sizeof(char)*15);
  beam_mode = (char *) MALLOC(sizeof(char)*15);
  orbit_direction = (char *) MALLOC(sizeof(char)*15);
  tleFile = (char *) MALLOC(sizeof(char)*255);
  tle = (tle_t *) MALLOC(sizeof(tle_t));
  sat = (satellite_t *) MALLOC(sizeof(satellite_t));

  // Parse command line
  if (argc < 6) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }  
  strcpy(demFile, argv[1]);
  strcpy(satellite, argv[2]);
  strcpy(beam_mode, argv[3]);
  strcpy(tleFile, argv[4]);
  strcpy(orbit_direction, argv[5]);
  
  asfSplashScreen (argc, argv);
  
  // Read TLE file
  read_tle(tleFile, satellite, tle);

  // Read satellite configuration file
  read_satellite_config("satellite.config", satellite, beam_mode, sat);

  // Simulate SAR image
  sar_simulation_tool(demFile, sat, tle);

  exit(0);

  // Clean up
  FREE(demFile);
  FREE(satellite);
  FREE(beam_mode);
  FREE(orbit_direction);
  FREE(tleFile);
  FREE(tle);
  FREE(sat);

  exit(0);
}
