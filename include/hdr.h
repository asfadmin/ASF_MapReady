#ifndef __HDR_H
#define __HDR_H   /* include only once */

#include "ddr.h"

typedef struct HDR
{
  int magnitude_bytes;      /* Magnitue bytes per pixel */
  int elevation_bytes;      /* Elevation bytes per pixel */
  char data_type[10];       /* Data file type: EQA */
  double elevation_scale;   /* Elevation scale */
  double elevation_shift;   /* Elevation shift */
  double line_increment;    /* Posting spacing in latitude [deg] */
  double sample_increment;  /* Posting spacing in longitude [deg] */
  double start_lat;         /* Starting corner position in latitude [deg] */
  double start_lon;         /* Starting corner position in longitude [deg] */
  int line_count;        /* Number of lines */
  int sample_count;      /* Number of samples */
} jpl_header;

/* Prototypes */
void hdr2ddr(jpl_header *hdr, struct DDR *ddr);

#endif
