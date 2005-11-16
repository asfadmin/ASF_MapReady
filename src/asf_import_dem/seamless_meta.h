
#ifndef _SEAMLESS_META_H_
#define _SEAMLESS_META_H_

#include <asf.h>
#include <asf_nan.h>
#include <asf_reporting.h>

#define SEAMLESS_META_MAGIC_UNSET_CHAR      '?'
#define SEAMLESS_META_MAGIC_UNSET_STRING    "???"
#define SEAMLESS_META_MAGIC_UNSET_INT       -999999999
#define SEAMLESS_META_MAGIC_UNSET_DOUBLE    NAN

#define SEAMLESS_META_FALSE    0
#define SEAMLESS_META_TRUE     !SEAMLESS_META_FALSE

#define SET_ERR_MSG(msg) strcpy(self->err_msg,(msg))

typedef struct {
  int ncols;            // width of image
  int ncols_valid;
  int nrows;            // height of image
  int nrows_valid;
  double xllcorner;     // lower-left corner lat
  int xllcorner_valid;
  double yllcorner;     // lower left corner lon
  int yllcorner_valid;
  double cellsize;      // pixel size (degrees)
  int cellsize_valid;
  int NODATA_value;     // value indicating pixel is invalid
  int NODATA_value_valid;
  char byteorder[64];   // little-endian vs. big-endian
  int byteorder_valid;
  char projection[64];
  int projection_valid;
  char datum[64];
  int datum_valid;
  char spheroid[64];
  int spheroid_valid;
  char units[64];       // units in the horizontal (xy) plane
  int units_valid;
  char zunits[64];      // units in the vertical (z) direction
  int zunits_valid;

  char err_msg[1024];
  int initialized;

} seamless_meta_t;


seamless_meta_t *seamless_meta_new(void);
void seamless_meta_destroy(seamless_meta_t *self);

void seamless_meta_set_ncols(seamless_meta_t *self, int value);
int seamless_meta_get_ncols(seamless_meta_t *self);
void seamless_meta_set_nrows(seamless_meta_t *self, int value);
int seamless_meta_get_nrows(seamless_meta_t *self);
void seamless_meta_set_xllcorner(seamless_meta_t *self, double value);
double seamless_meta_get_xllcorner(seamless_meta_t *self);
void seamless_meta_set_yllcorner(seamless_meta_t *self, double value);
double seamless_meta_get_yllcorner(seamless_meta_t *self);
void seamless_meta_set_cellsize(seamless_meta_t *self, double value);
double seamless_meta_get_cellsize(seamless_meta_t *self);
void seamless_meta_set_NODATA_value(seamless_meta_t *self, int value);
int seamless_meta_get_NODATA_value(seamless_meta_t *self);
void seamless_meta_set_byteorder(seamless_meta_t *self, char *value);
char *seamless_meta_get_byteorder(seamless_meta_t *self);
void seamless_meta_set_projection(seamless_meta_t *self, char *value);
char *seamless_meta_get_projection(seamless_meta_t *self);
void seamless_meta_set_datum(seamless_meta_t *self, char *value);
char *seamless_meta_get_datum(seamless_meta_t *self);
void seamless_meta_set_spheroid(seamless_meta_t *self, char *value);
char *seamless_meta_get_spheroid(seamless_meta_t *self);
void seamless_meta_set_units(seamless_meta_t *self, char *value);
char *seamless_meta_get_units(seamless_meta_t *self);
void seamless_meta_set_zunits(seamless_meta_t *self, char *value);
char *seamless_meta_get_zunits(seamless_meta_t *self);

#endif // _SEAMLESS_META_H_
