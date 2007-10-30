#ifndef _ASF_SIMULATION_H_
#define _ASF_SIMULATION_H_

#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "asf_terrcorr.h"

#define GM 3.9860045e+14         // Gravitational constant [m^3 / s^2].
#define ECC 8.1827385e-02        // GEM6 model eccentricity of the Earth.
#define speedOfLight 299792458.0 // Speed of light in vacuum [m/s] 
typedef struct {
  double a;                     // Semi-major axis.
  double e;                     // Eccentricity.
  double i;                     // Inclination.
  double cap_omega;             // Longitude of ascending node.
  double omega;                 // Argument of perigee.
  double ea;                    // Eccentric anomaly.
} keplerian_orbit_t;

typedef struct {
  double x, y, z, vx, vy, vz;
} cartesian_orbit_t;

typedef struct {
  char satellite[25];           // Name of satellite
  char sensor[25];              // Name of sensor
  char beam_mode[10];           // Beam mode of sensor
  char orbit_direction[15];     // 'ascending' or 'descending'
  double look_angle;            // Look angle at scene center
  double pixel_size;            // Pixel size of the SAR image
} satellite_t;

typedef struct {
  int year;                     // Year
  int day;                      // Julian day
  double time;                  // Time of the day (seconds)
  double inclination;           // Orbital inclination
  double cap_omega;             // Right ascension of the ascending node
  double eccentricity;          // Eccentricity
  double omega;                 // Argument of perigee
  double mean_anomaly;          // Mean anomaly
  double mean_motion;           // Mean motion
  int rev_at_epoch;             // Revolution number at epoch
} tle_t;

// Prototypes from sar_simulation_tool.c
void read_tle(char *tleFile, char *satellite, tle_t *tle);
void read_satellite_config(char *satelliteFile, char *header, 
			   satellite_t *sat);
void sar_simulation_tool(char *demFile, satellite_t *sat, tle_t *tle);

#endif
