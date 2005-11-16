
#include "seamless_meta.h"

/*
static seamless_meta_t self = {
  SEAMLESS_META_MAGIC_UNSET_INT,     //ncols
  SEAMLESS_META_FALSE,               //ncols_valid
  SEAMLESS_META_MAGIC_UNSET_INT,     //nrows
  SEAMLESS_META_FALSE,               //nrows_valid
  SEAMLESS_META_MAGIC_UNSET_DOUBLE,  //xllcorner
  SEAMLESS_META_FALSE,               //xllcorner_valid
  SEAMLESS_META_MAGIC_UNSET_DOUBLE,  //yllcorner
  SEAMLESS_META_FALSE,               //yllcorner_valid
  SEAMLESS_META_MAGIC_UNSET_DOUBLE,  //cellsize
  SEAMLESS_META_FALSE,               //cellsize_valid
  SEAMLESS_META_MAGIC_UNSET_INT,     //NODATA_value
  SEAMLESS_META_FALSE,               //NODATA_value_valid
  SEAMLESS_META_MAGIC_UNSET_STRING,  //byteorder
  SEAMLESS_META_FALSE,               //byteorder_valid
  SEAMLESS_META_MAGIC_UNSET_STRING,  //projection
  SEAMLESS_META_FALSE,               //projection_valid
  SEAMLESS_META_MAGIC_UNSET_STRING,  //datum
  SEAMLESS_META_FALSE,               //datum_valid
  SEAMLESS_META_MAGIC_UNSET_STRING,  //spheroid
  SEAMLESS_META_FALSE,               //spheroid_valid
  SEAMLESS_META_MAGIC_UNSET_STRING,  //units
  SEAMLESS_META_FALSE,               //units_valid
  SEAMLESS_META_MAGIC_UNSET_STRING,  //zunits
  SEAMLESS_META_FALSE,               //zunits_valid

  "",                                //err_msg
  SEAMLESS_META_FALSE                //initialized
};
*/

static void seamless_meta_unset(seamless_meta_t *self) {
  self->ncols = SEAMLESS_META_MAGIC_UNSET_INT;
  self->ncols_valid = SEAMLESS_META_FALSE;
  self->nrows = SEAMLESS_META_MAGIC_UNSET_INT;
  self->nrows_valid = SEAMLESS_META_FALSE;
  self->xllcorner = SEAMLESS_META_MAGIC_UNSET_DOUBLE;
  self->xllcorner_valid = SEAMLESS_META_FALSE;
  self->yllcorner = SEAMLESS_META_MAGIC_UNSET_DOUBLE;
  self->yllcorner_valid = SEAMLESS_META_FALSE;
  self->cellsize = SEAMLESS_META_MAGIC_UNSET_DOUBLE;
  self->cellsize_valid = SEAMLESS_META_FALSE;
  self->NODATA_value = SEAMLESS_META_MAGIC_UNSET_INT;
  self->NODATA_value_valid = SEAMLESS_META_FALSE;
  strcpy( self->byteorder, SEAMLESS_META_MAGIC_UNSET_STRING);
  self->byteorder_valid = SEAMLESS_META_FALSE;
  strcpy( self->projection, SEAMLESS_META_MAGIC_UNSET_STRING);
  self->projection_valid = SEAMLESS_META_FALSE;
  strcpy( self->datum, SEAMLESS_META_MAGIC_UNSET_STRING);
  self->datum_valid = SEAMLESS_META_FALSE;
  strcpy( self->spheroid, SEAMLESS_META_MAGIC_UNSET_STRING);
  self->spheroid_valid = SEAMLESS_META_FALSE;
  strcpy( self->units, SEAMLESS_META_MAGIC_UNSET_STRING);
  self->units_valid = SEAMLESS_META_FALSE;
  strcpy( self->zunits, SEAMLESS_META_MAGIC_UNSET_STRING);
  self->zunits_valid = SEAMLESS_META_FALSE;
  strcpy( self->err_msg, "");
  self->initialized = SEAMLESS_META_FALSE;
}

seamless_meta_t *seamless_meta_new(void)
{
  seamless_meta_t *self = (seamless_meta_t*)MALLOC(sizeof(seamless_meta_t));
  seamless_meta_unset(self);
  self->initialized = SEAMLESS_META_TRUE;
  return self;
}

void seamless_meta_destroy(seamless_meta_t *self)
{
  seamless_meta_unset(self);
  self->initialized = SEAMLESS_META_FALSE;  // just to make sure
  FREE (self);
}


// ncols ----------------------------------------
void seamless_meta_set_ncols(seamless_meta_t *self, int value)
{
  self->ncols = value;
  self->ncols_valid = SEAMLESS_META_TRUE;
}
int seamless_meta_get_ncols(seamless_meta_t *self)
{
  if (self->ncols_valid == SEAMLESS_META_TRUE) {
    return self->ncols;
  }
  else {
    asfPrintError("Requested field 'ncols' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_INT;  // to silence whiney compiler
}

// nrows ----------------------------------------
void seamless_meta_set_nrows(seamless_meta_t *self, int value)
{
  self->nrows = value;
  self->nrows_valid = SEAMLESS_META_TRUE;
}
int seamless_meta_get_nrows(seamless_meta_t *self)
{
  if (self->nrows_valid == SEAMLESS_META_TRUE) {
    return self->nrows;
  }
  else {
    asfPrintError("Requested field 'nrows' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_INT;  // to silence whiney compiler
}

// xllcorner ------------------------------------
void seamless_meta_set_xllcorner(seamless_meta_t *self, double value)
{
  self->xllcorner = value;
  self->xllcorner_valid = SEAMLESS_META_TRUE;
}
double seamless_meta_get_xllcorner(seamless_meta_t *self)
{
  if (self->xllcorner_valid == SEAMLESS_META_TRUE) {
    return self->xllcorner;
  }
  else {
    asfPrintError("Requested field 'xllcorner' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_DOUBLE;  // to silence whiney compiler
}

// yllcorner ------------------------------------
void seamless_meta_set_yllcorner(seamless_meta_t *self, double value)
{
  self->yllcorner = value;
  self->yllcorner_valid = SEAMLESS_META_TRUE;
}
double seamless_meta_get_yllcorner(seamless_meta_t *self)
{
  if (self->yllcorner_valid == SEAMLESS_META_TRUE) {
    return self->yllcorner;
  }
  else {
    asfPrintError("Requested field 'yllcorner' in seamless meta structure is invalid.\n");

  }
  return SEAMLESS_META_MAGIC_UNSET_DOUBLE;  // to silence whiney compiler
}

// cellsize -------------------------------------
void seamless_meta_set_cellsize(seamless_meta_t *self, double value)
{
  self->cellsize = value;
  self->cellsize_valid = SEAMLESS_META_TRUE;
}
double seamless_meta_get_cellsize(seamless_meta_t *self)
{
  if (self->cellsize_valid == SEAMLESS_META_TRUE) {
    return self->cellsize;
  }
  else {
    asfPrintError("Requested field 'cellsize' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_DOUBLE;  // to silence whiney compiler
}

// NODATA_value ---------------------------------
void seamless_meta_set_NODATA_value(seamless_meta_t *self, int value)
{
  self->NODATA_value = value;
  self->NODATA_value_valid = SEAMLESS_META_TRUE;
}
int seamless_meta_get_NODATA_value(seamless_meta_t *self)
{
  if (self->NODATA_value_valid == SEAMLESS_META_TRUE) {
    return self->NODATA_value;
  }
  else {
    asfPrintError("Requested field 'NODATA_value' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_INT;  // to silence whiney compiler
}

// byteorder ------------------------------------
void seamless_meta_set_byteorder(seamless_meta_t *self, char *value)
{
  strcpy( self->byteorder, value);
  self->byteorder_valid = SEAMLESS_META_TRUE;
}
char *seamless_meta_get_byteorder(seamless_meta_t *self)
{
  if (self->byteorder_valid == SEAMLESS_META_TRUE) {
    return self->byteorder;
  }
  else {
    asfPrintError("Requested field 'byteorder' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_STRING;  // to silence whiney compiler
}

// projection ------------------------------------
void seamless_meta_set_projection(seamless_meta_t *self, char *value)
{
  strcpy( self->projection, value);
  self->projection_valid = SEAMLESS_META_TRUE;
}
char *seamless_meta_get_projection(seamless_meta_t *self)
{
  if (self->projection_valid == SEAMLESS_META_TRUE) {
    return self->projection;
  }
  else {
    asfPrintError("Requested field 'projection' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_STRING;  // to silence whiney compiler
}

// datum ------------------------------------
void seamless_meta_set_datum(seamless_meta_t *self, char *value)
{
  strcpy( self->datum, value);
  self->datum_valid = SEAMLESS_META_TRUE;
}
char *seamless_meta_get_datum(seamless_meta_t *self)
{
  if (self->datum_valid == SEAMLESS_META_TRUE) {
    return self->datum;
  }
  else {
    asfPrintError("Requested field 'datum' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_STRING;  // to silence whiney compiler
}

// spheroid ------------------------------------
void seamless_meta_set_spheroid(seamless_meta_t *self, char *value)
{
  strcpy( self->spheroid, value);
  self->spheroid_valid = SEAMLESS_META_TRUE;
}
char *seamless_meta_get_spheroid(seamless_meta_t *self)
{
  if (self->spheroid_valid == SEAMLESS_META_TRUE) {
    return self->spheroid;
  }
  else {
    asfPrintError("Requested field 'spheroid' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_STRING;  // to silence whiney compiler
}

// units ------------------------------------
void seamless_meta_set_units(seamless_meta_t *self, char *value)
{
  strcpy( self->units, value);
  self->units_valid = SEAMLESS_META_TRUE;
}
char *seamless_meta_get_units(seamless_meta_t *self)
{
  if (self->units_valid == SEAMLESS_META_TRUE) {
    return self->units;
  }
  else {
    asfPrintError("Requested field 'units' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_STRING;  // to silence whiney compiler
}

// zunits ------------------------------------
void seamless_meta_set_zunits(seamless_meta_t *self, char *value)
{
  strcpy( self->zunits, value);
  self->zunits_valid = SEAMLESS_META_TRUE;
}
char *seamless_meta_get_zunits(seamless_meta_t *self)
{
  if (self->zunits_valid == SEAMLESS_META_TRUE) {
    return self->zunits;
  }
  else {
    asfPrintError("Requested field 'zunits' in seamless meta structure is invalid.\n");
  }
  return SEAMLESS_META_MAGIC_UNSET_STRING;  // to silence whiney compiler
}
