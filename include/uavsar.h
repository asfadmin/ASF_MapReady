#ifndef __UAVSAR_H__
#define __UAVSAR_H__

#include "asf_meta.h"

// UAVSAR data types
typedef enum {
  POLSAR_SLC=1,
  POLSAR_MLC,
  POLSAR_DAT,
  POLSAR_GRD,
  POLSAR_HGT,
  INSAR_AMP,
  INSAR_INT,
  INSAR_UNW,
  INSAR_COR,
  INSAR_AMP_GRD,
  INSAR_INT_GRD,
  INSAR_UNW_GRD,
  INSAR_COR_GRD,
  INSAR_HGT_GRD
} uavsar_type_t;

// Polarimetric UAVSAR header structure
typedef struct {
  char id[255];                   // File name
  uavsar_type_t type;             // UAVSAR data type
  char site[255];                 // Site description
  char acquisition_mode[25];      // Acquisition mode: PolSAR
  char polarization[25];          // Polarization
  int row_count;                  // Number of rows [pixels]
  int column_count;               // Number of columns [pixels]
  int header_bytes;               // Header size [bytes]
  int tail_bytes;                 // Tail size [bytes]
  char projection[50];            // SCX (slant_range), EQA (equi-rectangular)
  double lat_peg_point;           // Latitude of peg point [degrees]
  double lon_peg_point;           // Longitude of peg point [degrees]
  double head_peg_point;          // Heading at peg point [degrees]
  double along_track_offset;      // Along-track offset S0 [m]
  double cross_track_offset;      // Cross-track offset C0 [m]
  double azimuth_pixel_spacing;   // SAR geometry [m/pixel] or 
                                  // Projected [degrees/pixel]
  double range_pixel_spacing;     // SAR geometry [m/pixel] or 
                                  // Projected [degrees/pixel]
  int bytes_per_pixel;            // Data size [bytes]
  char value_format[32];          // Data type
  char endianess[32];             // Usually little endian data
  double data_scale;              // Scale for data
  double data_shift;              // Shift for data
  double min_value;               // Minimum data value
  double max_value;               // Maximum data value
  double wavelength;              // Center wavelength [cm]
  double semi_major;              // Ellipsoid semi-major axis [m]
  double eccentricity;            // Ellipsoid eccentricity squared
  char look_direction[10];        // Sensor look direction
  double range_spacing;           // Range spacing per bin [m]
  double azimuth_spacing;         // Azimuth spacing [m]
  double slant_range_first_pixel; // Image starting range [km]
  double yaw;                     // Global average yaw [degrees]
  double pitch;                   // Global average pitch [degrees]
  double roll;                    // Global average roll [degrees]
  double altitude;                // Global average altitude [m]
  double terrain_height;          // Global average terrain height [m]
  double squint_angle;            // Global average squint angle [degrees]
  double pulse_length;            // Pulse length [microseconds]
  double steering_angle;          // Steering angle (90 is boresite) [degrees]
  double bandwidth;               // Bandwidth [MHz]
  char acquisition_date[50];      // Date of acquisition
  int range_look_count;           // Number of range looks
  int azimuth_look_count;         // Number of azimuth looks
  char data_units[30];            // Data units
  char processor[10];             // Processor version
  double lat_upper_left;          // Upper left latitude
  double lon_upper_left;          // Upper left longitude
  double lat_upper_right;         // Upper right latitude
  double lon_upper_right;         // Upper right longitude
  double lat_lower_left;          // Lower left latitude
  double lon_lower_left;          // Lower left longitude
  double lat_lower_right;         // Lower right latitude
  double lon_lower_right;         // Lower right longitude
} uavsar_polsar;

// Interferometric UAVSAR header structure
typedef struct {
  char id[255];                   // File name
  uavsar_type_t type;             // UAVSAR data type
  char site[255];                 // Site description
  char processing_mode[25];       // Processing mode: InSAR
  char polarization[25];          // Polarization
  int row_count;                  // Number of rows [pixels]
  int column_count;               // Number of columns [pixels]
  int header_bytes;               // Header size [bytes]
  int tail_bytes;                 // Tail size [bytes]
  char projection[50];            // SCX (slant_range), EQA (equi-rectangular)
  double lat_peg_point;           // Latitude of peg point [degrees]
  double lon_peg_point;           // Longitude of peg point [degrees]
  double head_peg_point;          // Heading at peg point [degrees]
  double along_track_offset;      // Along-track offset S0 [m]
  double cross_track_offset;      // Cross-track offset C0 [m]
  double azimuth_pixel_spacing;   // SAR geometry [m/pixel] or 
                                  // Projected [degrees/pixel]
  double range_pixel_spacing;     // SAR geometry [m/pixel] or 
                                  // Projected [degrees/pixel]
  int bytes_per_pixel;            // Data size [bytes]
  char value_format[32];          // Data type
  char endianess[32];             // Usually little endian data
  double data_scale;              // Scale for data
  double data_shift;              // Shift for data
  double min_value;               // Minimum data value
  double max_value;               // Maximum data value
  double wavelength;              // Center wavelength [cm]
  double semi_major;              // Ellipsoid semi-major axis [m]
  double eccentricity;            // Ellipsoid eccentricity squared
  char look_direction[10];        // Sensor look direction
  double range_spacing;           // Range spacing per bin [m]
  double azimuth_spacing;         // Azimuth spacing [m]
  double slant_range_first_pixel; // Image starting range [km]
  double yaw;                     // Global average yaw [degrees]
  double pitch;                   // Global average pitch [degrees]
  double roll;                    // Global average roll [degrees]
  double altitude;                // Global average altitude [m]
  double terrain_height;          // Global average terrain height [m]
  double squint_angle;            // Global average squint angle [degrees]
  double pulse_length;            // Pulse length [microseconds]
  double steering_angle;          // Steering angle (90 is boresite) [degrees]
  double bandwidth;               // Bandwidth [MHz]
  char acquisition_date[50];      // Date of acquisition
  int range_look_count;           // Number of range looks
  int azimuth_look_count;         // Number of azimuth looks
  char data_units[30];            // Data units
  char processor[10];             // Processor version
  double lat_upper_left;          // Upper left latitude
  double lon_upper_left;          // Upper left longitude
  double lat_upper_right;         // Upper right latitude
  double lon_upper_right;         // Upper right longitude
  double lat_lower_left;          // Lower left latitude
  double lon_lower_left;          // Lower left longitude
  double lat_lower_right;         // Lower right latitude
  double lon_lower_right;         // Lower right longitude
} uavsar_insar;

// Function prototypes
meta_parameters* uavsar_polsar2meta(uavsar_polsar *params);
meta_parameters* uavsar_insar2meta(uavsar_insar *params);
uavsar_polsar *read_uavsar_polsar_params(const char *dataFile,
					 uavsar_type_t type);
uavsar_insar *read_insar_polsar_params(const char *dataFile,
				       uavsar_type_t type);
char *check_data_type(const char *inFileName);
char **get_uavsar_products(const char *data_type, char *type, int *num_product);

#endif
