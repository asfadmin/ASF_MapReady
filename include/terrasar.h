#ifndef __TERRASAR_H__
#define __TERRASAR_H__

#include "asf_meta.h"

// TerraSAR meta structure
typedef struct {
  char imageData[1025];
  char imageDataType[25];
  char imageDataFormat[25];
  int numberOfLayers;
  int imageDataDepth;
  int numberOfRows;
  int numberOfColumns;
  double groundRangeResolution;
  double azimuthResolution;
} terrasar_meta;


// Function prototypes
meta_parameters* terrasar2meta(terrasar_meta *terrasar);
terrasar_meta *read_terrsar_meta(const char *dataFile);

#endif
