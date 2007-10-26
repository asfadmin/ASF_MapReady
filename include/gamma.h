#ifndef __GAMMA_H__
#define __GAMMA_H__

#include "asf_meta.h"

// Structure of GAMMA ISP parameter file
typedef struct {
  char title[100];                        // Title
  char sensor[100];                       // Name of sensor
  int acquisition[3];                     // Acquisition date - 2007 1 15
  double start_time;                      // Start time [s]
  double center_time;                     // Center time [s]
  double end_time;                        // End time [s]
  double azimuth_line_time;               // Azimuth time per pixel [s]
  int line_header_size;                   // Line header size
  int range_samples;                      // Sample count
  int azimuth_lines;                      // Line count
  int range_looks;                        // Range look count
  int azimuth_looks;                      // Azimuth look count
  char image_format[25];                  // Image format: FCOMPLEX
  char image_geometry[25];                // Image geometry: SLANT_RANGE
  double range_scale_factor;              // Range scale factor
  double azimuth_scale_factor;            // Azimuth scale factor
  double center_latitude;                 // Center latitude [degrees]
  double center_longitude;                // Center longitude [degrees]
  double heading;                         // Heading [degrees]
  double range_pixel_spacing;             // Range pixel spacing [m]
  double azimuth_pixel_spacing;           // Azimuth pixel spacing [m]
  double near_range_slc;                  // Slant range first pixel [m]
  double center_range_slc;                // Slant range middle pixel [m]
  double far_range_slc;                   // Slant range last pixel [m]
  double first_slant_range_polynomial[6]; // First slant range polynomial  
  double center_slant_range_polynomial[6];// Center slant range polynomial 
  double last_slant_range_polynomial[6];  // Last slant range polynomial 
  double incidence_angle;                 // Incidence angle [degrees]
  int azimuth_deskew;                     // Deskew flag
  double azimuth_angle;                   // Azimuth angle [degrees]
  double radar_frequency;                 // Radar frequency [Hz]
  double adc_sampling_rate;               // Sampling rate [Hz]
  double chirp_bandwidth;                 // Chirp bandwidth [Hz]
  double prf;                             // Pulse repetition frequency [Hz]
  double azimuth_proc_bandwidth;          // Processing bandwidth [Hz]
  double doppler_polynomial[4];           // Doppler polynomial [Hz]
  double doppler_poly_dot[4];             // Doppler rate polynomial [Hz]
  double doppler_poly_ddot[4];            // Doppler second order [Hz]
  double receiver_gain;                   // Receiver gain [dB]
  double calibration_gain;                // Calibration gain [dB]
  double sar_to_earth_center;             // Satellite height [m]
  double earth_radius_below_sensor;       // Earth radius [m]
  double earth_semi_major_axis;           // Semimajor axis [m]
  double earth_semi_minor_axis;           // Semiminor axis [m]
} gamma_isp;

// Structure of GAMMA MSP parameter file
typedef struct {
} gamma_msp;

// Function prototypes
meta_parameters* gamma_isp2meta(gamma_isp *gamma, meta_state_vectors *stVec);
meta_parameters* gamma_msp2meta(gamma_msp *gamma);
gamma_isp* meta2gamma_isp(meta_parameters *meta);
gamma_msp* meta2gamma_msp(meta_parameters *meta);
void write_gamma_isp_header(const char *inFile, gamma_isp *gamma, 
			    meta_state_vectors *stVec);
void write_gamma_msp_header(const char *inFile, gamma_msp *gamma, 
			    meta_state_vectors *stVec);

#endif
