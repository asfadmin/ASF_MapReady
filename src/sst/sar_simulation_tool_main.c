#include "asf_simulation.h"

#define VERSION 1.0

static double calc_look(stateVector st, double incid)
{
  double re = 6378137.000; //[m] - semimajor axis WGS84
  double rp = 6356752.314; //[m] - semiminor axis WGS84
  double ht = sqrt(st.pos.x*st.pos.x + st.pos.y*st.pos.y + st.pos.z*st.pos.z);
  double lat = asin(st.pos.z / ht);
  double er = (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  double look = look_from_incid(incid*D2R, er, ht)*R2D;
  return look;
}

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <dem> <satellite> <beam mode> <orbit direction>\n", name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   dem:              DEM the simulated amplitude image is derived from\n"
	 "   satellite:        satellite name\n"
	 "   beam mode:        beam mode \n"
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
  sat_t *tle = (sat_t *) MALLOC(sizeof(sat_t));
  satellite_t *sat = (satellite_t *) MALLOC(sizeof(satellite_t));

  // Parse command line
  if (argc < 5) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }  
  strcpy(demFile, argv[1]);
  strcpy(satellite, argv[2]);
  strcpy(beam_mode, argv[3]);
  strcpy(sat->orbit_direction, argv[4]);

  asfSplashScreen (argc, argv);
  
  // Read TLE file
  read_tle(satellite, tle);

  /*
  // Calculate look angles from incidence angles
  double tle_time =
    time_to_secs(tle->tle.epoch_year, tle->tle.epoch_day, tle->tle.epoch_fod);
  stateVector st = tle_propagate(tle, tle_time);
  char line[255];
  FILE *fp = FOPEN("incidence_angles.txt", "r");
  while (fgets(line, 255, fp)) {
    char *p = strchr(line, ':');
    double minIncid, maxIncid;
    sscanf(p+1, "%lf,%lf\n", &minIncid, &maxIncid);
    double minLook = calc_look(st, minIncid);
    double maxLook = calc_look(st, maxIncid);
    *p = '\0';
    char beam_mode[10];
    strcpy(beam_mode, line);
    printf("beam mode: %s, minIncid: %.2lf, maxIncid: %.2lf, minLook: %.2lf, "
	   "maxLook: %.2lf\n", 
	   beam_mode, minIncid, maxIncid, minLook, maxLook);
  }
  FCLOSE(fp);
  exit(0);
  */

  // Read satellite configuration file
  sat = read_satellite_config(satellite, beam_mode);
  strcpy(sat->orbit_direction, argv[4]);
  strcpy(sat->satellite, satellite);

  // Simulate SAR image
  sar_simulation_tool(demFile, sat, tle);

  // Clean up
  FREE(demFile);
  FREE(satellite);
  FREE(beam_mode);
  FREE(tle);
  FREE(sat);

  exit(0);
}
