#ifndef __ESRI_H__
#define __ESRI_H__

#include "asf_meta.h"

/* General ESRI header structure */
typedef struct {
  int nrows;         /* number of lines */
  int ncols;         /* number of samples */
  int nbands;        /* number of bands */
  int nbits;         /* number of bits: 1,4,8,16,32 */
  char byteorder;    /* byte order: I(ntel) - little endian (default), 
		        M(otorola) - big endian */
  char layout[5];    /* bil - band interleaved by line
		        bip - band interleaved by pixel
		        bsq - band sequential */
  int skipbytes;     /* header bytes to skip - default: 0 */
  double ulxmap;     /* center of upper left pixel in x */
  double ulymap;     /* center of upper left pixel in y */
  double xdim;       /* map units in x */
  double ydim;       /* map units in y */
  int bandrowbytes;  /* bytes per band in a row (only used with bil) */
  int totalrowbytes; /* total bytes per band in a row (only used with bil) */
  int bandgapbytes;  /* gap bytes per band (only used with bsq) */
  int nodata;        /* no data value */
} esri_header;

typedef enum {
  CLARKE_1866,
  CLARKE_1880,
  BESSEL_1841,
  INTERNATIONAL_1924,
  INTERNATIONAL_1967,
  WGS_1972,
  WGS_1984,
  GRS_1967,
  GRS_1980
} spheroid_type_t;

/* Prototypes for functions */
esri_header* meta2esri(meta_parameters *meta);
meta_parameters* esri2meta(esri_header *esri);

#endif
