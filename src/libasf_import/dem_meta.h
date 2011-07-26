#ifndef __DEM_H__
#define __DEM_H__

#include "asf_meta.h"
#include "fgdc_meta.h"
#include "gdal.h"
#include "ogr_srs_api.h"
#include "cpl_conv.h"
#include "cpl_vsi.h"
#include "cpl_minizip_unzip.h"

typedef enum {
  DEM_ARCGRID=1,
  DEM_BIL_GRIDFLOAT,
  DEM_DEM,
  DEM_DTED,
  DEM_GEOTIFF,
  DEM_HGT
} dem_format_t;

// DEM meta structure
typedef struct 
{
  char dataset[255];           // Name of data set
  char tiles[255];             // Tiles covered
  char type[25];               // DEM type
  dem_format_t format;         // DEM format
  data_type_t data_type;       // data type
  int row_count;               // number of rows
  int column_count;            // number of columns
  int band_count;              // number of bands
  double min_value;            // minimum value
  double max_value;            // maximum value
  double mean_value;           // mean value
  double standard_deviation;   // standard deviation
  char unit_type[25];          // unit type
  double no_data;              // no data value
} dem_meta;

// Function prototypes
void import_dem(const char *inBaseName, int list, const char *outBaseName,
		const char *dem_type, const char *tmp_dir,
		char ***pImportFiles, int *nFiles);

#endif
