#ifndef __FGDC2URSA_H__
#define __FGDC2URSA_H__

#include "sqlca.h"
#include "asf_meta.h"
#include "dateUtil.h"

#define MAX_DATASET_ID 75
#define MAX_ORIGIN 50
#define MAX_TITLE 50
#define MAX_ONLINE_LINK 100
#define MAX_PROCESSING_LEVEL 25
#define MAX_PLACE 25
#define MAX_PLATFORM 25
#define MAX_INSTRUMENT 25
#define MAX_BROWSE_LOCATION 100
#define MAX_BROWSE_FORMAT 10
#define MAX_ACCESS 20
#define MAX_COPYRIGHT 50
#define MAX_START_TIME 30
#define MAX_CENTER_TIME 30
#define MAX_END_TIME 30
#define MAX_ORBIT_DIRECTION 15
#define MAX_MODE 15
#define MAX_SPATIAL_REFERENCE 10
#define MAX_CELL_VALUE 50
#define MAX_RASTER_OBJECT 15
#define MAX_FORMAT 15

typedef struct
{
  char dataset_id[MAX_DATASET_ID];    // Dataset ID
  char origin[MAX_ORIGIN];            // Origin
  char title[MAX_TITLE];              // Title
  char online_link[MAX_ONLINE_LINK];  // Online link
  float west_bounding;        // West bounding coordinate (minimum longitude)
  float east_bounding;        // East bounding coordinate (maximum longitude)
  float north_bounding;       // North bounding coordinate (maximum latitude)
  float south_bounding;       // South bounding coordinate (minimum latitude)
  float near_start_lat;       // Near start latitude
  float near_start_lon;       // Near start longitude
  float far_start_lat;        // Far start latitude
  float far_start_lon;        // Far start longitude
  float far_end_lat;          // Far end latitude
  float far_end_lon;          // Far end longitude
  float near_end_lat;         // Near end latitude
  float near_end_lon;         // Near end longitude
  float center_lat;           // Center latitude
  float center_lon;           // Center longitude
  char processing_level[MAX_PROCESSING_LEVEL];   // Processing level
  char *place;                // Geographic location
  char platform[MAX_PLATFORM];        // Platform
  char instrument[MAX_INSTRUMENT];    // Instrument
  int band_count;             // Number of bands
  char *browse_location;      // Browse image location
  char *browse_format;        // Browse image format
  char access[MAX_ACCESS];    // Data access level
  char copyright[MAX_COPYRIGHT];      // Copyright holder
  char *start_time;           // Image start time
  char *center_time;          // Image center time
  char *end_time;             // Image end time
  char orbit_direction[MAX_ORBIT_DIRECTION];     // Orbit direction
  char mode[MAX_MODE];        // Imaging mode
  char spatial_reference[MAX_SPATIAL_REFERENCE]; // Spatial reference
  char cell_value[MAX_CELL_VALUE];    // Cell value type
  char raster_object[MAX_RASTER_OBJECT];         // Raster object type
  int row_count;              // Number of rows
  int col_count;              // Number of columns
  char format[MAX_FORMAT];    // Format name
  float fees;                 // Fees
} dataset_t;

void addData(dataset_t *data);
void getCenterTime(char *start_time, char *end_time, char *center_time);


#endif
