#ifndef __TERRASAR_H__
#define __TERRASAR_H__

#include "asf_meta.h"

// TerraSAR meta structure
typedef struct {
  // general block
  char filename[1025];                // basename
  char mission[20];                   // sensor
  char sensor[20];                    // sensor_name
  char imagingMode[5];                // mode: SM,SC,HS,SL
  char elevationBeamConfiguration[25];// mode
  // no processor information but individual processing steps
  char imageDataType[25];             // data_type: COMPLEX, DETECTED
  int imageDataDepth;                 // data_type: 2x16 bit, 16 bit
  char imageDataFormat[255];    // image_data_type: GEOTIFF, COSAR, UNDEFINED
  char azimuthTimeUTC[35];            // acquisition_date
  int absOrbit;                       // orbit
  char orbitDirection[25];            // orbit_direction
  // no frame number - DLR uses time information
  int numberOfLayers;                 // band_count
  char bands[512];                    // bands: construct from polarizations
  int numberOfRows;                   // line_count
  int numberOfColumns;                // sample_count
  double rangeResolution;             // x_pixel_size
  double azimuthResolution;           // y_pixel_size
  double sceneCenterCoordLat;         // center_latitude
  double sceneCenterCoordLon;         // center_longitude

  // SAR block
  char projection[25];          // image_type: SLANTRANGE, GROUNDRANGE, MAP
  char lookDirection[15];             // look_direction: LEFT, RIGHT
  int azimuthLooks;                   // look_count
  int rangeLooks;                     // look_count
  char imageCoordinateType[25];       // deskewed: RAW, ZERODOPPLER
  double rowSpacing;                  // azimuth_time_per_pixel
  double columnSpacing;               // range_time_per_pixel
  double rangeTimeFirst;              // slant_range_first_pixel [s]
  double rangeTimeLast;               // slant_range_last_pixel [s]
  double centerFrequency;             // wavelength
  double prf;                         // prf
  double totalProcessedAzimuthBandwidth; // azimuth_processing_bandwidth
  double pulseLength;                 // pulse_duration
  double rsf;                         // range_sampling_rate
  char polarisationMode[10];          // polarization: SINGLE, DUAL, TWIN, QUAD
  
  // Doppler block
  meta_doppler *doppler;              // Doppler estimates

  // state vectors
  char sceneStart[30];
  char sceneStop[30];                 
  meta_state_vectors *state_vectors;  // state vectors

  // location block
  double sceneCornerCoord1Lat;        // lat_start_near_range
  double sceneCornerCoord1Lon;        // lon_start_near_range
  double sceneCornerCoord2Lat;        // lat_start_far_range
  double sceneCornerCoord2Lon;        // lon_start_far_range
  double sceneCornerCoord3Lat;        // lat_end_near_range
  double sceneCornerCoord3Lon;        // lon_end_near_range
  double sceneCornerCoord4Lat;        // lat_end_far_range
  double sceneCornerCoord4Lon;        // lon_end_far_range

  // calibration
  double cal_factor;                  // calibration constant in beta naught

} terrasar_meta;


// Function prototypes
meta_parameters* terrasar2meta(terrasar_meta *terrasar);
terrasar_meta *read_terrasar_meta(const char *dataFile);

#endif
