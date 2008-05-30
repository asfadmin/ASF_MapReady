#ifndef _ASF_SIMULATION_H_
#define _ASF_SIMULATION_H_

#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "asf_raster.h"
#include "asf_vector.h"
#include "asf_import.h"
#include "asf_terrcorr.h"
#include "asf_geocode.h"
#include "asf_export.h"
#include "dateUtil.h"
#include "sgpsdp.h"
#include "geolocate.h"
#include "frame_calc.h"

#define GM 3.9860045e+14         // Gravitational constant [m^3 / s^2].
#define ECC 8.1827385e-02        // GEM6 model eccentricity of the Earth.
#define speedOfLight 299792458.0 // Speed of light in vacuum [m/s] 
typedef struct {
  char satellite[25];           // Name of satellite
  char sensor[25];              // Name of sensor
  char beam_mode[10];           // Beam mode of sensor
  double wavelength;            // Sensor wavelength
  char orbit_direction[15];     // 'ascending' or 'descending'
  double look_angle;            // Look angle at scene center
  double pixel_size;            // Pixel size of the SAR image
  int pixel_count;              // Line and samples of SAR image
} satellite_t;

typedef struct {
  double clat, clon;            // Center lat/lon of frame
  stateVector st;               // State vector of center frame
  double time;                  // State vector time
} frame_t;

// Prototypes from sar_simulation_tool.c
void sar_simulation_tool(char *demFile, satellite_t *sat, sat_t *tle);

// Prototypes from tle.c
void read_tle(const char *satellite, sat_t *sat);
stateVector tle_propagate(sat_t *sat, double t);
double secs_to_jul(double t);
double jul_to_secs(double jul);
double time_to_secs(int year, int doy, double fod);

// Prototypes from config.c
satellite_t *read_satellite_config(char *sensor, char *mode);

#endif
