#ifndef __GAMMA_H__
#define __GAMMA_H__

#include "asf_meta.h"

typedef struct {
  int line_count;
  int sample_count;
  int azimuth_look_count;
  int range_look_count;
  data_type_t data_type;
} gamma_meta;

typedef struct {int year, month, day, hour, minute; double seconds;} DATE;
typedef struct {int hour, minute; double seconds;} TIME;
typedef struct {double x,y,z;} POS;           // x,y,z position vector [m]
typedef struct {double vx,vy,vz;} VEL;        // vx,vy,vz velocity vector [m/s]
typedef struct {double ax,ay,az;} ACC;        // ax,ay,az acceleration vector [m/s^2]
typedef struct{double lat,lon,alt;} POSITION;	// position in lat/lon and altitude (WGS-84), [degrees] [degrees] [m]

// Structure of GAMMA ISP parameter file
typedef struct {
  char title[FIELD_STRING_MAX];            // Title, usually "Platform name nnnnn", nnnnn is orbit no.
  int orbit;                               // Extracted from title, 'nnnnn'
  char sensor[FIELD_STRING_MAX];           // Name of sensor (RADARSAT, SRL-1, SRL-2, ERS-1, ERS-2, JERS-1, ...)
  DATE acquisition;                        // Acquisition date - YYYY MM DD hh mm ss.ttt UTC
  double start_time;                       // Start time, UTC seconds since start of day [s]
  double center_time;                      // Center time, UTC seconds since start of day [s]
  double end_time;                         // End time, UTC seconds since start of day [s]
  double azimuth_line_time;                // Azimuth time per pixel [s]
  int line_header_size;                    // Line header size [bytes]
  int range_samples;                       // Sample count
  int azimuth_lines;                       // Line count
  int range_looks;                         // Range look count (SLC = 1)
  int azimuth_looks;                       // Azimuth look count (SLC = 1)
  char image_format[FIELD_STRING_MAX];     // Image format (data type): FCOMPLEX, SCOMPLEX, FLOAT, SHORT, BYTE
  char image_data_type[FIELD_STRING_MAX];  // Image data type: AMPLITUDE_IMAGE etc.
  char image_geometry[FIELD_STRING_MAX];   // Image geometry: SLANT_RANGE, GROUND_RANGE, GEOCODED
  double range_scale_factor;               // Range scale factor (1.0 unless resampled)
  double azimuth_scale_factor;             // Azimuth scale factor (1.0 unless resampled)
  double center_latitude;                  // Center latitude [degrees]
  double center_longitude;                 // Center longitude [degrees]
  double heading;                          // Heading [degrees]
  double range_pixel_spacing;              // Range pixel spacing [m]
  double azimuth_pixel_spacing;            // Azimuth pixel spacing [m]
  double near_range_slc;                   // Slant (or ground) range first pixel [m]
  double center_range_slc;                 // Slant (or ground) range middle pixel [m]
  double far_range_slc;                    // Slant (or ground) range last pixel [m]
  double first_slant_range_polynomial[6];  /* First slant range polynomial [m]:
  
                                              sr0[i] == first_slant_range_polynomial[i]
                                              r0     == near_range_slc
                                              dgr    == ground range - near_range_slc
                                              sr     == slant range (result of polynomial)
                                              gr     == ground range (independent factor)
                                              
                                              sr(gr) = sr0[0] + sr0[1]*dgr + sr0[2]*dgr^2
                                                              + sr0[3]*dgr^3 + sr0[4]*dgr^4  */                                              
  double center_slant_range_polynomial[6]; /* center slant range polynomial [m]:
  
                                              sr0[i] == center_slant_range_polynomial[i]
                                              r0     == near_range_slc
                                              dgr    == ground range - near_range_slc
                                              sr     == slant range (result of polynomial)
                                              gr     == ground range (independent factor)
                                              
                                              sr(gr) = sr0[0] + sr0[1]*dgr + sr0[2]*dgr^2
                                                              + sr0[3]*dgr^3 + sr0[4]*dgr^4  */                                              
  double last_slant_range_polynomial[6];  /* last slant range polynomial [m]
  
                                              sr0[i] == last_slant_range_polynomial[i]
                                              r0     == near_range_slc
                                              dgr    == ground range - near_range_slc
                                              sr     == slant range (result of polynomial)
                                              gr     == ground range (independent factor)
                                              
                                              sr(gr) = sr0[0] + sr0[1]*dgr + sr0[2]*dgr^2
                                                              + sr0[3]*dgr^3 + sr0[4]*dgr^4  */                                              
  double incidence_angle;                 // Incidence angle [degrees]
  int azimuth_deskew;                     // Deskew flag
  double azimuth_angle;                   // Azimuth angle [degrees]
  double radar_frequency;                 // Radar carrier center frequency [Hz]
  double adc_sampling_rate;               // Sampling rate (A/D) [Hz]
  double chirp_bandwidth;                 // Chirp bandwidth [Hz]
  double prf;                             // Pulse repetition frequency [Hz]
  double azimuth_proc_bandwidth;          // 3db Processing bandwidth [Hz]
  double doppler_polynomial[4];           /* Doppler centroid polynomial [Hz]:
  
                                             dp[i]  == doppler_polynomial[i]
                                             r1     == center_range_slc
                                             sr     == slant range (independent factor)
                                             dsr    == slant range - center_range_slc (middle of image)
                                             dp(sr) == result of doppler polynomial
  
                                             dp(sr) = dp[0] + dp[1]*dsr + dp[2]*dsr^2
                                                            + dp[3]*dsr^3 + dp[4]*dsr^4  */
  double doppler_poly_dot[4];             // Doppler rate polynomial [Hz], First derivative of doppler_polynomial
  double doppler_poly_ddot[4];            // Doppler second order [Hz], Second derivative of doppler_polynomial
  double receiver_gain;                   // Receiver gain [dB]
  double calibration_gain;                // Calibration gain [dB]
  double sar_to_earth_center;             // Satellite height [m] at scene center, from earth center
  double earth_radius_below_sensor;       // Geocentric earth radius [m]
  double earth_semi_major_axis;           // Semimajor axis [m]
  double earth_semi_minor_axis;           // Semiminor axis [m]
  int number_of_state_vectors;            // Number of satellite position/velocity state vectors
  meta_state_vectors *stVec;               // Array of state vectors (x,y,z,vx,vy,vz) [m] and [m/s]
} gamma_isp;

// Structure of GAMMA MSP parameter file
typedef struct {
  char title[FIELD_STRING_MAX];           // Title
  int orbit;                              // Orbit number
  DATE acquisition;                       // Acquisition date - 2007 1 15
  TIME raw_data_start_time;               // Hrs, mins, seconds - 7 26 38.548176
  char band[FIELD_STRING_MAX];            // Polarization or channel; HH, HV, VH, VV, CH1, CH2 etc
  char image_data_type[FIELD_STRING_MAX]; // Image data type: AMPLITUDE_IMAGE etc.
  double earth_semi_major_axis;           // Ellipsoid semi-major axis [m]
  double earth_semi_minor_axis;           // Ellipsoid semi-minor axis [m]
  double scene_center_latitude;           // Latitude at scene center [degrees]
  double scene_center_longitude;          // Longitude at scene center [degrees]
  double track_angle;                     // Track angle [degrees]
  double platform_altitude;               // Satellite height above earth geoid at scene center [m]
  double terrain_height;                  // Average terrain height above geoid [m]
  POS sensor_position_vector;             // Satellite position vector
  VEL sensor_velocity_vector;             // Satellite velocity vector
  ACC sensor_acceleration_vector;         // Satellite acceleration vector
  double prf;                             // Pulse repetition frequency (PRF)
  double I_bias;                          // DC bias of the raw data I channel
  double Q_bias;                          // DC bias of the raw data Q channel
  double I_sigma;                         // Standard deviation of the raw data I channel
  double Q_sigma;                         // Standard deviation of the raw data Q channel
  double IQ_corr;                         // Correlation of the raw data I and Q channels
  double SNR_range_spectrum;              // Average SNR determined from range
  double DAR_doppler;                     // Doppler ambiguity resolver (DAR) estimate of the doppler centroid at center of swath
  double DAR_snr;                         // Unambiguous doppler estimate of the signal to noise ratio
  double doppler_polynomial[4];           /* Doppler centroid polynomial coefficients as a function of range [Hz/m]:
                                             fd(t,r) = c0 + c1*r + c2*r^2 + c3*r^3                                   */
  double doppler_poly_dot[4];             // First derivative of doppler_polynomial w.r.t. along-track time
  double doppler_poly_ddot[4];            // Second derivative of doppler_polynomial w.r.t. along-track time
  int sec_range_migration;                // Secondary range migration flag, (on or off)
  int azimuth_deskew;                     // Azimuth SAR processing deskew flag (on or off)
  double autofocus_snr;                   // Autofocus SNR
  double echo_time_delay;                 // Time delay between trans. of pulse and first sample of echo [s]
  double receiver_gain;                   // Receiver gain (attenuation) used for relative calibration [dB]
  double calibration_gain;                // Calibration gain factor to determine relative or absolute backscatter intensity [dB]
  double near_range_raw;                  // Raw SAR data near slant range [m]
  double center_range_raw;                // Raw SAR data center slant range [m]
  double far_range_raw;                   // Raw SAR data far slant range [m]
  double near_range_slc;                  // Output image (SLC) near slant range [m]
  double center_range_slc;                // Output image (SLC) center slant range [m]
  double far_range_slc;                   // Output image (SLC) far slant range [m]
  double range_pixel_spacing;             // Slant range image pixel spacing [m]
  double range_resolution;                // Slant range image resolution [m]
  double azimuth_bandwidth_fraction;      // Fraction of doppler bandwidth [Hz] to process, 0.0 to 1.0 (0.8 nominal)
  int prefilter_azimuth_offset;           // Initial azimuth prefilter decimation factor, e.g. 4 for a 4:1 decimation
  int total_raw_echoes;                   // Total number of raw echoes (lines) in the SAR signal data file
  int offset_to_first_echo_to_process;    // Offset, in echoes (lines), from the start of SAR signal data file to first line to process
  int echoes_to_process;                  // Number of lines to process
  int range_offset;                       // Offset, in samples, to first range sample to process (each I/Q pair is a sample)
  int raw_range_samples;                  /* Number of raw range samples to process in each echo (line),
                                             range_offset + raw_range_samples < total raw data samples   */
  int near_range_extension;               // Near range swath extension [samples]
  int far_range_extension;                // Far range swath extension [samples]
  int range_looks;                        // Number of range looks per sub-aperture
  int azimuth_looks;                      // Number of range looks per sub-aperture
  double azimuth_offset;                  // Along-track azimuth offset to first image line from start of SAR raw data [s]
  double azimuth_pixel_spacing;           // Azimuth image pixel spacing [m]
  double azimuth_resolution;              // Azimuth image resolution [m]
  int range_pixels;                       // Image dimension in range direction
  int azimuth_pixels;                     // Image dimension in azimuth direction
  char image_format[FIELD_STRING_MAX];    // Image format (data type), FCOMPLEX, SCOMPLEX, FLOAT, SHORT, BYTE
  double sensor_latitude;                 // SAR Geodetic latitude at image center [degrees]
  double sensor_longitude;                // SAR Geodetic longitude at image center [degrees]
  double sensor_track_angle;              // Track angle of the sensor at image center [degrees]
                                          /* Location Block:
                                               Ascending:                   Descending:              Both:
                                                    1: bottom left               1: top right          - Geodetic coordinates
                                                    2: bottom right              2: top left             are on the WGS-84
                                                    3: top left                  3: bottom right         reference datum
                                                    4: top right                 4: bottom left        - Altitude is from
                                                    5: center                    5: center               the WGS-84 ellipsoid  */
  POSITION map_coordinate_1;              // Geodetic latitude, longitude, and altitude of first record near range (WGS-84)
  POSITION map_coordinate_2;              // Geodetic latitude, longitude, and altitude of first record far range (WGS-84)
  POSITION map_coordinate_3;              // Geodetic latitude, longitude, and altitude of last record near range (WGS-84)
  POSITION map_coordinate_4;              // Geodetic latitude, longitude, and altitude of last record far range (WGS-84)
  POSITION map_coordinate_5;              // Geodetic latitude, longitude, and altitude of center line/sample (WGS-84)
  int number_of_state_vectors;            // Number of satellite position/velocity state vectors
  meta_state_vectors *stVec;               // Array of state vectors (x,y,z,vx,vy,vz) [m] and [m/s]
} gamma_msp;

// Function prototypes
meta_parameters *meta_read_gamma_isp(const char *inName,
                                     const char *data_type,
                                     const char *image_data_type);
meta_parameters* gamma_isp2meta(gamma_isp *gamma);
meta_parameters* gamma_msp2meta(gamma_msp *gamma);
//gamma_isp* meta2gamma_isp(meta_parameters *meta);
//gamma_msp* meta2gamma_msp(meta_parameters *meta);
//void write_gamma_isp_header(const char *inFile, gamma_isp *gamma);
//void write_gamma_msp_header(const char *inFile, gamma_msp *gamma);

#endif
