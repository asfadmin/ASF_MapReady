#ifndef __SENTINEL_H__
#define __SENTINEL_H__

#include "asf_meta.h"

// Sentinel meta structure
typedef struct {
  char time[30];
  double posX, posY, posZ;
  double velX, velY, velZ;
} sentinel_vector;

typedef struct {
  char polarization[25];
  char annotation[512];
  char noise[512];
  char calibration[512];
} sentinel_files;

typedef struct {
  int line;
  int count;
  int *pixel;
  float *value;
} sentinel_lut_line;

typedef struct {
  char granule[128];
  char familyName[25];
  char number[3];
  char instrument[5];
  char mode[5];
  char processor[25];
  char version[15];
  int orbitNumber;
  int relativeOrbitNumber;
  int cycleNumber;
  char pass[15];
  char polarization[50];
  char productClass[3];
  char productType[5];
  char resolution;
  char startTime[30];
  char stopTime[30];
  char mission[5];
  char missionDataTakeID[10];
  char projection[25];
  double rangeSamplingRate;
  double radarFrequency;
  double roll;
  double pitch;
  double yaw;
  double slantRangeTime;
  char pixelValue[30];
  char outputPixels[30];
  double rangePixelSpacing;
  double azimuthPixelSpacing;
  double azimuthTimeInterval;
  int numberOfSamples;
  int numberOfLines;
  double incidenceAngleMidSwath;
  char ellipsoidName[15];
  double ellipsoidSemiMajorAxis;
  double ellipsoidSemiMinorAxis;
  int vector_count;
  sentinel_vector *stVec;
  int gcp_count;
  gcp_location *gcp;
  int band_count;
  int file_count;
  char **data;
  char polarization_count;
  sentinel_files *file;
} sentinel_meta;

// Function prototypes
meta_parameters* sentinel2meta(sentinel_meta *sentinel);
sentinel_meta *read_sentinel_meta(const char *manifest, int channel);
void check_sentinel_meta(const char *fileName, char *mission, char *beamMode, 
  char *productType);

#endif
