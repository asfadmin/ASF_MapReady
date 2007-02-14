#ifndef __ENVI_H__
#define __ENVI_H__

#include "asf_meta.h"

/* General ENVI header structure */
typedef struct {
  char description[255];     /* Description header */
  int samples;               /* Number of samples */
  int lines;                 /* Number of lines */
  int bands;                 /* Number of bands */
  int header_offset;         /* Offset to be applied for header */
  char file_type[25];        /* ENVI standard (for the moment) */
  int data_type;             /* Data type - 
				1: 8-bit byte, 
				2: 16-bit signed integer,
				3: 32-bit signed long integer, 
				4: 32-bit floating point,
				5: 64-bit double, 
				6: 2x32-bit complex, 
				9: 2x64-bit complex, 
				12: 16-bit unsigned integer, 
				13: 32-bit unsigned long integer, 
				14: 64-bit signed long integer,
				15: 64-bit unsigned long integer */
  char interleave[10];       /* bil - band interleaved by line,
				bip - band interleaved by pixel,
				bsq - band sequential */
  char sensor_type[25];      /* RADARSAT, ERS-1/2, JERS-1 */
  int byte_order;            /* 0 - little endian,
				1 - big endian */
  char projection[50];       /* Projection name */
  int ref_pixel_x;           /* Reference pixel in x */
  int ref_pixel_y;           /* Reference pixel in y */
  double pixel_easting;      /* Pixel easting */
  double pixel_northing;     /* Pixel northing */
  double proj_dist_x;        /* Projection distance in x */
  double proj_dist_y;        /* Projection distance in y */
  int projection_zone;       /* Projection zone */
  double center_lat;         /* Latitude at center of projection */
  double center_lon;         /* Longitude at center of projection */
  double standard_parallel1; /* First standard parallel */
  double standard_parallel2; /* Second standard parallel */
  double semimajor_axis;     /* Semimajor axis */
  double semiminor_axis;     /* Semiminor axis */
  char hemisphere[10];       /* Hemisphere */
  char wavelength_units[25]; /* Units of wavelength */
  double wavelength;         /* Wavelength */
  double data_ignore;        /* No data value */
  double pixel_size_x;       /* Pixel size in x */
  double pixel_size_y;       /* Pixel size in y */
  char default_stretch[25];  /* % linear, linear range, gaussian, equalize, 
				square root */
} envi_header;

/* Prototype for functions */
envi_header* meta2envi(meta_parameters *meta);
meta_parameters* envi2meta(envi_header *envi);
void write_envi_header(const char *inFile, meta_parameters *meta,
		       envi_header *envi);

#endif
