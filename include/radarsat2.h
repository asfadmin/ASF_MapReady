#ifndef __RADARSAT2_H__
#define __RADARSAT2_H__

#include "asf_meta.h"

// Radarsat-2 meta structure
typedef struct {
  // general block
  char filename[1025];           // basename
  char satellite[20];            // satellite
  char sensor[20];               // sensor_name
  char beamModeMnemonic[10];     // beam mode
  char acquisitionType[25];      // type of data acquisition
  char productType[25];          // product_type: SLC, SGF, SGX, SGC, SSG, SPG
  char dataType[25];             // data_type: COMPLEX
  char zeroDopplerAzimuthTime[30]; // acquisition time
  char processingFacility[15];   // processing facility
  char softwareVersion[15];      // version of processor version
  int bitsPerSample;             // data_type: 2x16 bit, 16 bit
  // no orbit information in product.xml
  int absOrbit;                  // orbit
  char passDirection[25];        // orbit_direction
  int band_count;                // band_count
  char bands[512];               // bands: construct from polarizations
  int numberOfLines;             // line_count
  int numberOfSamplesPerLine;    // sample_count
  double sampledPixelSpacing;    // x_pixel_size
  double sampledLineSpacing;     // y_pixel_size
  double semiMajorAxis;          // semi-major axis
  double semiMinorAxis;          // semi-minor axis
  char lineTimeOrdering[20];     // flipping vertical when decreasing
  char pixelTimeOrdering[20];    // flipping horizontal when decreasing

  // SAR block
  char antennaPointing[15];           // look_direction: LEFT, RIGHT
  int numberOfAzimuthLooks;           // look_count in azimuth
  int numberOfRangeLooks;             // look_count in range
  //char imageCoordinateType[25];       // deskewed: RAW, ZERODOPPLER
  double slantRangeNearEdge;          // slant_range_first_pixel [s]
  double radarCenterFrequency;        // wavelength
  double pulseRepetitionFrequency;    // prf [Hz]
  double totalProcessedAzimuthBandwidth; // azimuth_processing_bandwidth
  double satelliteHeight;             // satellite height
  double pulseLength;                 // pulse_duration [s]
  double adcSamplingRate;             // range_sampling_rate [Hz]
  char polarizations[20];             // polarization sequence: HH VV HV VH

  // Doppler block
  meta_doppler *doppler;              // Doppler estimates  

  // state vectors
  char zeroDopplerTimeFirstLine[30];
  char zeroDopplerTimeLastLine[30];                 
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
} radarsat2_meta;


// Function prototypes
meta_parameters* radarsat2meta(radarsat2_meta *radarsat2);
radarsat2_meta *read_radarsat2_meta(const char *dataFile);

#endif
