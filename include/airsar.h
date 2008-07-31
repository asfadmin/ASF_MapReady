#ifndef __AIRSAR_H__
#define __AIRSAR_H__

#include "asf_meta.h"

// General AirSAR header structure
typedef struct {
  char scene_id[255];             // Scene ID
  char flight_line[255];          // Name of flight line
  char date_acquisition[25];      // Date of acquisition
  char date_processing[25];       // Date of processing
  char radar_projection[25];      // Radar projection
  double width;                   // Width [km]
  double length;                  // Length [km]
  double range_pixel_spacing;     // Range pixel spacing [m]
  double azimuth_pixel_spacing;   // Azimuth pixel spacing [m]
  double corner1_lat;             // Corner 1 latitude [degrees]
  double corner1_lon;             // Corner 1 longitude [degrees]
  double corner2_lat;             // Corner 2 latitude [degrees]
  double corner2_lon;             // Corner 2 longitude [degrees]
  double corner3_lat;             // Corner 3 latitude [degrees]
  double corner3_lon;             // Corner 3 longitude [degrees]
  double corner4_lat;             // Corner 4 latitude [degrees]
  double corner4_lon;             // Corner 4 longitude [degrees]
  double c_bandwidth;             // C-band Radar Bandwidth [MHz]
  double l_bandwidth;             // L-band Radar Bandwidth [MHz]
  double p_bandwidth;             // P-band Radar Bandwidth [MHz]
  char mode[10];                  // AIRSAR Mode
  int c_pol_data;                 // C-band Polarimetric data [1=yes, 0=none]
  int l_pol_data;                 // L-band Polarimetric data [1=yes, 0=none]
  int p_pol_data;                 // P-band Polarimetric data [1=yes, 0=none]
  int c_cross_data;               // C-band Cross Track Interferometric data 
                                  // [1=yes, 0=none]
  int l_cross_data;               // L-band Cross Track Interferometric data 
                                  // [1=yes, 0=none]
  int c_along_data;               // C-band Along Track Interferometric data 
                                  // [1=yes, 0=none]
  int l_along_data;               // L-band Along Track Interferometric data 
                                  // [1=yes, 0=none]
  char baseline[10];              // Interferometry Baseline Length: long
  char frequency_band1[10];       // Single Frequency/Channel band 1
  char frequency_band2[10];       // Single Frequency/Channel band 2
  char frequency_band3[10];       // Single Frequency/Channel band 3
  char format_band1[25];          // Data Format band 1
  char format_band2[25];          // Data Format band 2
  char format_band3[25];          // Data Format band 3
} airsar_general;

// Specific AirSAR parameter header structure
typedef struct {
  int sample_count;               // Number of samples per record
  int line_count;                 // Number of lines in image
  char processor[10];             // JPL aircraft SAR processor version
  data_type_t data_type;          // Data type
  char range_projection[25];      // Range projection - GROUND
  double x_pixel_size;            // Range pixel spacing [m]
  double y_pixel_size;            // Azimuth pixel spacing [m]
  long first_data_offset;         // Byte offset of first data record
  long calibration_header_offset; // Byte offset of calibration header
  long dem_header_offset;         // Byte offset of DEM header
  char site_name[50];             // Site name
  char cct_type[10];              // CCT type
  int cct_id;                     // CCT ID - basename for naming scheme
  double start_lat;               // Latitude at start of scene [degrees]
  double start_lon;               // Longitude at start of scene [degrees]
  double end_lat;                 // Latitude at end of scene [degrees]
  double end_lon;                 // Longitude at end of scene [degrees]
  char acquisition_date[50];      // Date of acquisition (GMT)
  double acquisition_seconds;     // Acquisition time [s]
  char frequencies[5];            // Frequencies collected - PLC
  double prf;                     // PRF at start of transfer
  double range_sampling_rate;     // Sampling rate [MHz]
  double chirp_bandwidth;         // Chirp bandwidth [MHz]
  double pulse_length;            // Pulse length [micro seconds]
  double wavelength;              // Processor wavelength [m]
  double near_slant_range;        // Near slant range [m]
  double far_slant_range;         // Far slant range [m]
  double near_look_angle;         // Near look angle [degrees]
  double far_look_angle;          // Far look angle [degrees]
  int azimuth_look_count;         // Number of looks processed in azimuth
  int range_look_count;           // Number of looks processed in range
  int deskewed;                   // Deskew flag (1=deskewed, 2=not deskewed)
  double sr_sample_spacing;       // Slant range sample spacing [m]
  double slant_range_resolution;  // Slant range resolution [m]
  double azimuth_sample_spacing;  // Azimuth sample spacing [m]
  double azimuth_resolution;      // Azimuth resolution [m]
  double center_lat;              // Latitude of image [degrees]
  double center_lon;              // Longitude of image [degrees]
  double scale_factor;            // General scale factor
  double gps_altitude;            // GPS altitude [m]
  double lat_peg_point;           // Latitude of peg point [degrees]
  double lon_peg_point;           // Longitude of peg point [degrees]
  double head_peg_point;          // Heading at peg point [degrees]
  double along_track_offset;      // Along-track offset S0 [m]
  double cross_track_offset;      // Cross-track offset C0 [m]
} airsar_parameters;

// Function prototypes
meta_parameters* airsar2meta(airsar_general *general, 
			     airsar_parameters *params);
airsar_parameters *read_airsar_params(const char *inBaseName);

#endif
