#ifndef __ALOS_MOSAIC_H__
#define __ALOS_MOSAIC_H__

#include "asf_meta.h"

// ALOS mosaic header structure
typedef struct {
  char image_file_id[60];      // Mosaic image file ID
  char product_id[15];         // Mosaic product ID
  char area[15];               // Mosaic area
  char acquisition[25];        // Observation date ID
  char path_mode[25];          // Original path mode ID
  char separate[5];            // Separate ID
  int file_no;                 // Mosaic image file number
  char product_type[10];       // Product type
  char mission[10];            // Mission name
  char sensor[10];             // Sensor name
  char original_path_type[10]; // Original path type
  double pixel_spacing;        // Pixel spacing [m]
  char orbit_direction[10];    // Time direction indicator along line direction
  char polarization[5];        // Original path polarization
  int start_rsp;               // Mosaic start RSP
  int path;                    // Using path no.
  char resampling_method[10];  // Resampling method
  double corner1_lat;          // Corner 1 latitude [degrees]
  double corner1_lon;          // Corner 1 longitude [degrees]
  double corner2_lat;          // Corner 2 latitude [degrees]
  double corner2_lon;          // Corner 2 longitude [degrees]
  double corner3_lat;          // Corner 3 latitude [degrees]
  double corner3_lon;          // Corner 3 longitude [degrees]
  double corner4_lat;          // Corner 4 latitude [degrees]
  double corner4_lon;          // Corner 4 longitude [degrees]
  char continental_name[25];   // Continental name
  char continental_code[5];    // Continental code
  char national_code[8][5];    // National codes
  char map_projection[5];      // Map projection method ID
  double orig_lat;             // Original latitude [degrees]
  double orig_lon;             // Original longitude [degrees]
  double ps_ref_lat;           // PS reference latitude [degrees]
  double ps_ref_lon;           // PS reference longitude [degrees]
  char hemisphere[10];         // Hemisphere ID
  int utm_zone;                // Number of UTM zone
  double lcc_ref_lat1;         // LCC reference latitudinal line 1 [degrees]
  double lcc_ref_lat2;         // LCC reference latitudinal line 2 [degrees]
  double range_spacing;        // Pixel (range) spacing [m]
  double azimuth_spacing;      // Line (azimuth) spacing [m]
  double true_north;           // Angle of projection axis from true north [deg]
  char datum[10];              // Geodetic coordinate
  char ellipsoid[10];          // Ellipsoid designator
  double semimajor;            // Ellipsoid semi-major axis [km]
  double semiminor;            // Ellipsoid semi-minor axis [km]
  double ellipticity;          // Ellipsoid inverse of ellipticity
  double cal_factor;           // Calibration factor
  int sample_count;            // Data record length
  int line_count;              // Data record number
  int bits_per_pixel;          // Number of bits a pixel
  char processing_date[10];    // Processing date (JST)
  char processing_time[10];    // Processing time (JST)
  char creation_country[10];   // Processing creation country
  char creation_agency[10];    // Processing creation agency
  char creation_equipment[15]; // Product creation equipment
  char processor_name[20];     // Processor name
  char version_number[15];     // Processor version number
  char manual_revision[5];     // Manual revision
  char format_revision[5];     // Format revision
  int orig_path;               // Path number
  char sar_image_file_id[50];  // SAR image file ID
  int rsp_path_number;         // RSP path number
  char downlink_segment[20];   // Downlink segment number
  double off_nadir;            // Off-nadir angle
  char observation_date[10];   // Observation date (UTC)
  char obs_polarization[5];    // Polarization
  int cycle_number;            // Cycle number
} alos_mosaic_header;


// Function prototypes
meta_parameters* alos_mosaic2meta(alos_mosaic_header *header);
alos_mosaic_header *read_alos_mosaic_header(const char *dataFile);

#endif
