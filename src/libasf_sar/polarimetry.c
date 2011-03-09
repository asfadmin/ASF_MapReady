#include <unistd.h>
#include "asf_sar.h"
#include "asf_raster.h"
#include "asf_nan.h"
#include "asf_complex.h"
#include <assert.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_eigen.h>

#define EPS 1.E-15

typedef struct {
   int current_row;
   int nrows;  // # in held in memory, not total image rows
   meta_parameters *meta;

   float *amp; // HH amplitude data

   quadPolS2Float *s2_data_buffer;
   quadPolS2Float **s2_lines;

  quadPolC3Float *c3_data_buffer;
  quadPolC3Float **c3_lines;

  quadPolT3Float *t3_data_buffer;
  quadPolT3Float **t3_lines;

   floatVector *pauli_buffer;
   floatVector **pauli_lines;

   complexMatrix **coh_buffer;
   complexMatrix ***coh_lines;

   int amp_band;
   int hh_amp_band, hh_phase_band;
   int hv_amp_band, hv_phase_band;
   int vh_amp_band, vh_phase_band;
   int vv_amp_band, vv_phase_band;
  int c11_band, c12_real_band, c12_imag_band;
  int c13_real_band, c13_imag_band, c22_band;
  int c23_real_band, c23_imag_band, c33_band;
  int t11_band, t12_real_band, t12_imag_band;
  int t13_real_band, t13_imag_band, t22_band;
  int t23_real_band, t23_imag_band, t33_band;
} PolarimetricImageRows;


//static complexFloat complex_new_gsl(gsl_complex c)
//{
//    complexFloat ret;
//    ret.real = GSL_REAL(c);
//    ret.imag = GSL_IMAG(c);
//    return ret;
//}

static quadPolS2Float qual_pol_s2_zero()
{
    quadPolS2Float ret;
    ret.hh = complex_zero();
    ret.vh = complex_zero();
    ret.hv = complex_zero();
    ret.vv = complex_zero();
    return ret;
}

static quadPolC3Float qual_pol_c3_zero()
{
  quadPolC3Float ret;
  ret.c11 = 0.0;
  ret.c12_real = 0.0;
  ret.c12_imag = 0.0;
  ret.c13_real = 0.0;
  ret.c13_imag = 0.0;
  ret.c22 = 0.0;
  ret.c23_real = 0.0;
  ret.c23_imag = 0.0;
  ret.c33 = 0.0;
  return ret;
}

static quadPolT3Float qual_pol_t3_zero()
{
  quadPolT3Float ret;
  ret.t11 = 0.0;
  ret.t12_real = 0.0;
  ret.t12_imag = 0.0;
  ret.t13_real = 0.0;
  ret.t13_imag = 0.0;
  ret.t22 = 0.0;
  ret.t23_real = 0.0;
  ret.t23_imag = 0.0;
  ret.t33 = 0.0;
  return ret;
}

static PolarimetricImageRows *
polarimetric_image_rows_new(meta_parameters *meta, int nrows, int multi)
{
    PolarimetricImageRows *self = MALLOC(sizeof(PolarimetricImageRows));

    self->nrows = nrows;
    self->meta = meta;

    // nrows must be odd
    if (multi) {
      self->current_row = 0;
    }
    else {
      assert((self->nrows-1)%2==0);
      self->current_row = -(nrows+1)/2;
    }

    int ns = meta->general->sample_count;

    self->amp = CALLOC(ns, sizeof(float));
    if (meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
      self->s2_data_buffer = CALLOC(nrows*ns, sizeof(quadPolS2Float));
      self->s2_lines = CALLOC(nrows, sizeof(quadPolS2Float*));
    }
    else if (meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
      self->c3_data_buffer = CALLOC(nrows*ns, sizeof(quadPolC3Float));
      self->c3_lines = CALLOC(nrows, sizeof(quadPolC3Float*));
    }

    // initially, the line pointers point at their natural locations in
    // the buffer
    int i;
    if (meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
      for (i=0; i<nrows; ++i)
	self->s2_lines[i] = &(self->s2_data_buffer[ns*i]);
    }
    if (meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
      for (i=0; i<nrows; ++i)
	self->c3_lines[i] = &(self->c3_data_buffer[ns*i]);
    }

    // these guys are the pauli basis elements we've calculated for the
    // loaded rows
    self->pauli_buffer = CALLOC(nrows*ns, sizeof(floatVector));
    self->pauli_lines = CALLOC(nrows, sizeof(floatVector*));
    for (i=0; i<nrows; ++i)
        self->pauli_lines[i] = &(self->pauli_buffer[ns*i]);

    // coherency matrix elements for the loaded rows
    self->coh_buffer = MALLOC(nrows*ns*sizeof(complexMatrix*));
    for (i=0; i<nrows*ns; ++i)
        self->coh_buffer[i] = complex_matrix_new(3,3);
    self->coh_lines = MALLOC(nrows*sizeof(complexMatrix**));
    for (i=0; i<nrows; ++i)
        self->coh_lines[i] = &(self->coh_buffer[ns*i]);

    // band numbers in the input file
    self->amp_band = -1;
    self->hh_amp_band = self->hh_phase_band = -1;
    self->hv_amp_band = self->hv_phase_band = -1;
    self->vh_amp_band = self->vh_phase_band = -1;
    self->vv_amp_band = self->vv_phase_band = -1;
    self->c11_band = -1;
    self->c12_real_band = -1;
    self->c12_imag_band = -1;
    self->c13_real_band = -1; 
    self->c13_imag_band = -1;
    self->c22_band = -1;
    self->c23_real_band = -1;
    self->c23_imag_band = -1;
    self->c33_band = -1;
    self->t11_band = -1;
    self->t12_real_band = -1;
    self->t12_imag_band = -1;
    self->t13_real_band = -1;
    self->t13_imag_band = -1;
    self->t22_band = -1;
    self->t23_real_band = -1;
    self->t23_imag_band = -1;
    self->t33_band = -1;

    return self;
}

static int polarimetric_image_rows_get_bands(PolarimetricImageRows *self)
{
    int ok=TRUE;
    // "extra" amplitude band needs to be the first band
    char *bands = self->meta->general->bands;
    self->amp_band = strncmp_case(bands, "AMP", 3) == 0 ? 0 : -1;

    // polarimetric bands
    if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
      self->hh_amp_band = find_band(self->meta, "AMP-HH", &ok);
      self->hh_phase_band = find_band(self->meta, "PHASE-HH", &ok);
      self->hv_amp_band = find_band(self->meta, "AMP-HV", &ok);
      self->hv_phase_band = find_band(self->meta, "PHASE-HV", &ok);
      self->vh_amp_band = find_band(self->meta, "AMP-VH", &ok);
      self->vh_phase_band = find_band(self->meta, "PHASE-VH", &ok);
      self->vv_amp_band = find_band(self->meta, "AMP-VV", &ok);
      self->vv_phase_band = find_band(self->meta, "PHASE-VV", &ok);
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
      self->c11_band = find_band(self->meta, "C11", &ok);
      self->c12_real_band = find_band(self->meta, "C12_real", &ok);
      self->c12_imag_band = find_band(self->meta, "C12_imag", &ok);
      self->c13_real_band = find_band(self->meta, "C13_real", &ok);
      self->c13_imag_band = find_band(self->meta, "C13_imag", &ok);
      self->c22_band = find_band(self->meta, "C22", &ok);
      self->c23_real_band = find_band(self->meta, "C23_real", &ok);
      self->c23_imag_band = find_band(self->meta, "C23_imag", &ok);
      self->c33_band = find_band(self->meta, "C33", &ok);
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
      self->t11_band = find_band(self->meta, "T11", &ok);
      self->t12_real_band = find_band(self->meta, "T12_real", &ok);
      self->t12_imag_band = find_band(self->meta, "T12_imag", &ok);
      self->t13_real_band = find_band(self->meta, "T13_real", &ok);
      self->t13_imag_band = find_band(self->meta, "T13_imag", &ok);
      self->t22_band = find_band(self->meta, "T22", &ok);
      self->t23_real_band = find_band(self->meta, "T23_real", &ok);
      self->t23_imag_band = find_band(self->meta, "T23_imag", &ok);
      self->t33_band = find_band(self->meta, "T33", &ok);
    }
    return ok;
}

static void calculate_pauli_for_row(PolarimetricImageRows *self, int n)
{
    int j, ns=self->meta->general->sample_count;
    complexFloat cpx_a, cpx_b, cpx_c;
    float value;

    // HH-VV, HV+VH, HH+VV

    if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
      for (j=0; j<ns; ++j) {
        quadPolS2Float q = self->s2_lines[n][j];
	cpx_a = complex_sub(q.hh, q.vv);
	cpx_b = complex_add(q.hv, q.vh);
	cpx_c = complex_add(q.hh, q.vv);
        self->pauli_lines[n][j].A = (float) hypot(cpx_a.real, cpx_a.imag);
        self->pauli_lines[n][j].B = (float) hypot(cpx_b.real, cpx_b.imag);
        self->pauli_lines[n][j].C = (float) hypot(cpx_c.real, cpx_c.imag);	
      }
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
      for (j=0; j<ns; ++j) {
	quadPolC3Float q = self->c3_lines[n][j];

	value = fabs(q.c11 - 2.0*q.c13_real + q.c33) / 2.0;
	if (ISNAN(value)) value = EPS;
	if (value > 0)
	  value = 10 *log10(value);	
	self->pauli_lines[n][j].A = value;

	value = fabs(q.c22);
	if (ISNAN(value)) value = EPS;
	if (value > 0)
	  value = 10 *log10(value);
	self->pauli_lines[n][j].B = value;

	value = fabs(q.c11 + 2.0*q.c13_real + q.c33) / 2.0;
	if (ISNAN(value)) value = EPS;
	if (value > 0)
	  value = 10 *log10(value);
	self->pauli_lines[n][j].C = value;
      }
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
      for (j=0; j<ns; ++j) {
	quadPolT3Float q = self->t3_lines[n][j];
	self->pauli_lines[n][j].A = fabs(q.t22);
	self->pauli_lines[n][j].B = fabs(q.t33);
	self->pauli_lines[n][j].C = fabs(q.t11);
      }
    }
}

static void calculate_coherence_for_row(PolarimetricImageRows *self, int n)
{
    // [ A*A  B*A  C*A ]    A = HH + VV
    // [ A*B  B*B  C*B ]    B = HH - VV
    // [ A*C  B*C  C*C ]    C = 2*HV
    int j, ns=self->meta->general->sample_count;
    if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
      for (j=0; j<ns; ++j) {
        quadPolS2Float q = self->s2_lines[n][j];
        complexVector v = //complex_vector_normalize(
          complex_vector_new(
            complex_add(q.hh, q.vv),
            complex_sub(q.hh, q.vv),
            complex_add(q.hv, q.vh))/*)*/;
        complexVector vc = complex_vector_conj(v);

        complexMatrix *m = self->coh_lines[n][j];

        complex_matrix_set(m,0,0,complex_mul(vc.A, v.A));
        complex_matrix_set(m,0,1,complex_mul(vc.A, v.B));
        complex_matrix_set(m,0,2,complex_mul(vc.A, v.C));

        complex_matrix_set(m,1,0,complex_mul(vc.B, v.A));
        complex_matrix_set(m,1,1,complex_mul(vc.B, v.B));
        complex_matrix_set(m,1,2,complex_mul(vc.B, v.C));

        complex_matrix_set(m,2,0,complex_mul(vc.C, v.A));
        complex_matrix_set(m,2,1,complex_mul(vc.C, v.B));
        complex_matrix_set(m,2,2,complex_mul(vc.C, v.C));

        //complex_matrix_scale(m,1./sqrt(2.0));
        complex_matrix_scale(m,0.5);
      }
    }
}

static void polarimetric_image_rows_load_next_row(PolarimetricImageRows *self,
                                                  FILE *fin)
{
  // we discard the top (0) row, slide all rows up one, then load
  // the new row into the top position

  // don't actually move any data -- update pointers into the
  // buffers

  // FIRST -- slide row pointers
  int k;
  for (k=0; k<self->nrows-1; ++k) {
    if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX)
      self->s2_lines[k] = self->s2_lines[k+1];
    else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX)
      self->c3_lines[k] = self->c3_lines[k+1];
    else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX)
      self->t3_lines[k] = self->t3_lines[k+1];
    self->pauli_lines[k] = self->pauli_lines[k+1];
  }
  
  // the next line to load will go into the spot we just dumped
  int last = self->nrows - 1;
  if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX)
    self->s2_lines[last] = self->s2_lines[0];
  else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX)
    self->c3_lines[last] = self->c3_lines[0];
  else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX)
    self->t3_lines[last] = self->t3_lines[0];
  self->pauli_lines[last] = self->pauli_lines[0];
  
  self->current_row++;
  
  // NEXT, load in new row into the final row
  // if we have moved off the top of the image, we will need to
  // fill with zeros, instead of loading a row
  int ns = self->meta->general->sample_count;

  float *amp_buf=NULL, *phase_buf=NULL;
  float *c11=NULL, *c12_real=NULL, *c12_imag=NULL, *c13_real=NULL;
  float *c13_imag=NULL, *c22=NULL, *c23_real=NULL, *c23_imag=NULL, *c33=NULL;
  float *t11=NULL, *t12_real=NULL, *t12_imag=NULL, *t13_real=NULL;
  float *t13_imag=NULL, *t22=NULL, *t23_real=NULL, *t23_imag=NULL, *t33=NULL;
  float C11, C12_real, C12_imag, C13_real, C13_imag;
  float C22, C23_real, C23_imag, C33;
  float phase, phase12, phase13, phase23;

  amp_buf = MALLOC(sizeof(float)*ns);

  if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
    phase_buf = MALLOC(sizeof(float)*ns);
  }
  else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
    c11 = MALLOC(sizeof(float)*ns);
    c12_real = MALLOC(sizeof(float)*ns);
    c12_imag = MALLOC(sizeof(float)*ns);
    c13_real = MALLOC(sizeof(float)*ns);
    c13_imag = MALLOC(sizeof(float)*ns);
    c22 = MALLOC(sizeof(float)*ns);
    c23_real = MALLOC(sizeof(float)*ns);
    c23_imag = MALLOC(sizeof(float)*ns);
    c33 = MALLOC(sizeof(float)*ns);
  }
  else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
    t11 = MALLOC(sizeof(float)*ns);
    t12_real = MALLOC(sizeof(float)*ns);
    t12_imag = MALLOC(sizeof(float)*ns);
    t13_real = MALLOC(sizeof(float)*ns);
    t13_imag = MALLOC(sizeof(float)*ns);
    t22 = MALLOC(sizeof(float)*ns);
    t23_real = MALLOC(sizeof(float)*ns);
    t23_imag = MALLOC(sizeof(float)*ns);
    t33 = MALLOC(sizeof(float)*ns);
  }


  int row = self->current_row + (self->nrows-1)/2;
  if (row < self->meta->general->line_count) {
    // amplitude, we only store the current row
    if (self->current_row >= 0 && self->amp_band >= 0)
      get_band_float_line(fin, self->meta, self->amp_band,
                          self->current_row, self->amp);

    // now the SLC rows
    if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {

      get_band_float_line(fin, self->meta, self->hh_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->hh_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[last][k].hh = complex_new_polar(sqrt(amp_buf[k]),
						       phase_buf[k]);
      
      get_band_float_line(fin, self->meta, self->hv_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->hv_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[last][k].hv = complex_new_polar(sqrt(amp_buf[k]),
						       phase_buf[k]);
      
      get_band_float_line(fin, self->meta, self->vh_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->vh_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[last][k].vh = complex_new_polar(sqrt(amp_buf[k]),
						       phase_buf[k]);
      
      get_band_float_line(fin, self->meta, self->vv_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->vv_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[last][k].vv = complex_new_polar(sqrt(amp_buf[k]),
						       phase_buf[k]);
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
 
      get_band_float_line(fin, self->meta, self->c11_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c11 = amp_buf[k];

      get_band_float_line(fin, self->meta, self->c12_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c12_real = amp_buf[k];

      get_band_float_line(fin, self->meta, self->c12_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c12_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c13_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c13_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c13_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c13_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c22_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c22 = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c23_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c23_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c23_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c23_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c33_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[last][k].c33 = amp_buf[k]; 
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
 
      get_band_float_line(fin, self->meta, self->t11_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t11 = amp_buf[k];

      get_band_float_line(fin, self->meta, self->t12_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t12_real = amp_buf[k];

      get_band_float_line(fin, self->meta, self->t12_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t12_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t13_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t13_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t13_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t13_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t22_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t22 = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t23_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t23_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t23_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t23_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t33_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[last][k].t33 = amp_buf[k]; 
    }
    
    calculate_pauli_for_row(self, last);
    calculate_coherence_for_row(self, last);
  }
  else {
    // window has scrolled off top of image -- fill with zeros
    for (k=0; k<self->meta->general->sample_count; ++k) {
      if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX)
	self->s2_lines[last][k] = qual_pol_s2_zero();
      else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX)
	self->c3_lines[last][k] = qual_pol_c3_zero();
      else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX)
	self->t3_lines[last][k] = qual_pol_t3_zero();
      self->pauli_lines[last][k].A = 0.0;
      self->pauli_lines[last][k].B = 0.0;
      self->pauli_lines[last][k].C = 0.0;
    }
  }
  
  free(amp_buf);
  if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX)
    free(phase_buf);
  if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
    free(c11);
    free(c12_real);
    free(c12_imag);
    free(c13_real);
    free(c13_imag);
    free(c22);
    free(c23_real);
    free(c23_imag);
    free(c33);
  }
  if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
    free(t11);
    free(t12_real);
    free(t12_imag);
    free(t13_real);
    free(t13_imag);
    free(t22);
    free(t23_real);
    free(t23_imag);
    free(t33);
  }
}

static void polarimetric_image_rows_load_new_rows(PolarimetricImageRows *self,
                                                  FILE *fin)
{
  int i,k,ns = self->meta->general->sample_count;

  float *amp_buf=NULL, *phase_buf=NULL;
  float *c11=NULL, *c12_real=NULL, *c12_imag=NULL, *c13_real=NULL;
  float *c13_imag=NULL, *c22=NULL, *c23_real=NULL, *c23_imag=NULL, *c33=NULL;
  float *t11=NULL, *t12_real=NULL, *t12_imag=NULL, *t13_real=NULL;
  float *t13_imag=NULL, *t22=NULL, *t23_real=NULL, *t23_imag=NULL, *t33=NULL;
  float phase, phase12, phase13, phase23;
  float C11, C12_real, C12_imag, C13_real, C13_imag;
  float C22, C23_real, C23_imag, C33;

  amp_buf = MALLOC(sizeof(float)*ns);

  if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
    phase_buf = MALLOC(sizeof(float)*ns);
  }
  else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
    c11 = MALLOC(sizeof(float)*ns);
    c12_real = MALLOC(sizeof(float)*ns);
    c12_imag = MALLOC(sizeof(float)*ns);
    c13_real = MALLOC(sizeof(float)*ns);
    c13_imag = MALLOC(sizeof(float)*ns);
    c22 = MALLOC(sizeof(float)*ns);
    c23_real = MALLOC(sizeof(float)*ns);
    c23_imag = MALLOC(sizeof(float)*ns);
    c33 = MALLOC(sizeof(float)*ns);
  }
  else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
    t11 = MALLOC(sizeof(float)*ns);
    t12_real = MALLOC(sizeof(float)*ns);
    t12_imag = MALLOC(sizeof(float)*ns);
    t13_real = MALLOC(sizeof(float)*ns);
    t13_imag = MALLOC(sizeof(float)*ns);
    t22 = MALLOC(sizeof(float)*ns);
    t23_real = MALLOC(sizeof(float)*ns);
    t23_imag = MALLOC(sizeof(float)*ns);
    t33 = MALLOC(sizeof(float)*ns);
  }

  int amp_band = self->amp_band;
  if (amp_band < 0)
    amp_band = self->hh_amp_band;

  // multilook the amplitude values as we go
  for (k=0; k<ns; ++k)
    self->amp[k] = 0.0;

  for (i=0; i<self->nrows; ++i) {
    int row = self->current_row + i;
    get_band_float_line(fin, self->meta, amp_band, row, amp_buf);
    for (k=0; k<ns; ++k)
      self->amp[k] += amp_buf[k];

    // now the SLC rows
    if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {

      get_band_float_line(fin, self->meta, self->hh_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->hh_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[i][k].hh = complex_new_polar(sqrt(amp_buf[k]),
						    phase_buf[k]);
      
      get_band_float_line(fin, self->meta, self->hv_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->hv_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[i][k].hv = complex_new_polar(sqrt(amp_buf[k]),
						    phase_buf[k]);
      
      get_band_float_line(fin, self->meta, self->vh_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->vh_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[i][k].vh = complex_new_polar(sqrt(amp_buf[k]),
						    phase_buf[k]);
      
      get_band_float_line(fin, self->meta, self->vv_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->vv_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
	self->s2_lines[i][k].vv = complex_new_polar(sqrt(amp_buf[k]),
						    phase_buf[k]);
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
 
      get_band_float_line(fin, self->meta, self->c11_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c11 = amp_buf[k];

      get_band_float_line(fin, self->meta, self->c12_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c12_real = amp_buf[k];

      get_band_float_line(fin, self->meta, self->c12_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c12_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c13_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c13_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c13_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c13_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c22_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c22 = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c23_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c23_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c23_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c23_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->c33_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->c3_lines[i][k].c33 = amp_buf[k]; 
    }
    else if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
 
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t11 = amp_buf[k];

      get_band_float_line(fin, self->meta, self->t12_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t12_real = amp_buf[k];

      get_band_float_line(fin, self->meta, self->t12_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t12_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t13_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t13_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t13_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t13_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t22_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t22 = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t23_real_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t23_real = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t23_imag_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t23_imag = amp_buf[k]; 

      get_band_float_line(fin, self->meta, self->t33_band, row, amp_buf);
      for (k=0; k<ns; ++k)
	self->t3_lines[i][k].t33 = amp_buf[k]; 

    }

    calculate_pauli_for_row(self, i);
    calculate_coherence_for_row(self, i);
  }

  // we multilook the amplitude data now, since we only keep one row
  // around-- the rest of the stuff is not multilooked since it isn't
  // the final result (all rows are kept in memory)
  for (k=0; k<ns; ++k)
    self->amp[k] /= self->nrows;

  self->current_row += self->nrows;

  free(amp_buf);
  if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX)
    free(phase_buf);
  if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
    free(c11);
    free(c12_real);
    free(c12_imag);
    free(c13_real);
    free(c13_imag);
    free(c22);
    free(c23_real);
    free(c23_imag);
    free(c33);
  }
  if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
    free(t11);
    free(t12_real);
    free(t12_imag);
    free(t13_real);
    free(t13_imag);
    free(t22);
    free(t23_real);
    free(t23_imag);
    free(t33);
  }
}

static void polarimetric_image_rows_free(PolarimetricImageRows* self)
{
    free(self->amp);
    if (self->meta->general->image_data_type == POLARIMETRIC_S2_MATRIX) {
      free(self->s2_data_buffer);
      free(self->s2_lines);
    }
    if (self->meta->general->image_data_type == POLARIMETRIC_C3_MATRIX) {
      free(self->c3_data_buffer);
      free(self->c3_lines);
    }
    if (self->meta->general->image_data_type == POLARIMETRIC_T3_MATRIX) {
      free(self->t3_data_buffer);
      free(self->t3_lines);
    }
    free(self->pauli_buffer);
    free(self->pauli_lines);

    int i;
    for (i=0; i<self->nrows*self->meta->general->sample_count; ++i)
        complex_matrix_free(self->coh_buffer[i]);

    free(self->coh_buffer);
    free(self->coh_lines);

    // do not free metadata pointer!
    free(self);
}

static double log3(double v)
{
    return log(v)/log(3.);
}

// Dump a 2D histogram image in entropy-alpha space
// the array is twice as wide as it is tall -- we use a square histogram,
// but generate two of them -- the "left" one is the 0<anisotropy<.5
// histogram, and the "right" is the .5<anisotropy<1 one.

// For Cloude-Pottier-8, anisotropy is ignored so when the histogram is
// dumped the two sides are added together, and we only dump the left
// half of the arrays when generating the histogram and classification map

// For Cloude-Potter-16, we dump the double-wide histograms and
// classification maps.

#define HIST_SIZE 512
int hist_vals[HIST_SIZE][HIST_SIZE][HIST_SIZE];
int class_map[HIST_SIZE][HIST_SIZE*2];

#define ENTROPY_ALPHA 0
#define ALPHA_ANISOTROPY 1
#define ANISOTROPY_ENTROPY 2

static int get_hist_val(int which, int ii, int jj, int kk)
{
  switch (which) {
    default:
    case ENTROPY_ALPHA:
      return hist_vals[jj][ii][kk];
    case ALPHA_ANISOTROPY:
      return hist_vals[kk][ii][jj];
    case ANISOTROPY_ENTROPY:
      return hist_vals[jj][kk][ii];
  }
}

static void dump_hist(const char *base_filename, int which)
{
  char *filename = NULL;
  switch (which) {
    default:
    case ENTROPY_ALPHA:
      filename = appendToBasename(base_filename, "_entropy_alpha_hist");
      break;
    case ALPHA_ANISOTROPY:
      filename = appendToBasename(base_filename, "_alpha_anisotropy_hist");
      break;
    case ANISOTROPY_ENTROPY:
      filename = appendToBasename(base_filename, "_anisotropy_entropy_hist");
      break;
  }

  int size = HIST_SIZE;
  meta_parameters *m = raw_init();
  m->general->line_count = size;
  m->general->sample_count = size;
  m->general->data_type = INTEGER16;
  m->general->no_data = 0;
  m->general->image_data_type = IMAGE;
  strcpy(m->general->basename, filename);
  m->general->band_count = 1;

  switch (which) {
    default:
      strcpy(m->general->bands, "???");
      break;
    case ENTROPY_ALPHA:
      strcpy(m->general->bands, "Entropy-Alpha");
      break;
    case ALPHA_ANISOTROPY:
      strcpy(m->general->bands, "Anisotropy-Alpha");
      break;
    case ANISOTROPY_ENTROPY:
      strcpy(m->general->bands, "Entropy-Anisotropy");
      break;
  }

  char *meta_file = appendExt(filename, ".meta");
  char *img_file = appendExt(filename, ".img");
  meta_write(m, meta_file);

  FILE *fp = fopenImage(img_file, "wb");

  int i,j,k;

    // which= ENTROPY_ALPHA:
    //   i= alpha index  [VERTICAL]
    //   j= entropy index [HORIZONTAL]
    // which= ALPHA_ANISOTROPY:
    //   i= alpha index  [VERTICAL]
    //   j= anisotropy index [HORIZONTAL]
    // which= ANISOTROPY_ENTROPY:
    //   i= anisotropy index [VERTICAL]
    //   j= entropy index [HORIZONTAL]

  float *buf = MALLOC(sizeof(float)*size);
  for (i=0; i<size; ++i) {
    for (j=0; j<size; ++j) {
      buf[j] = 0;
      for (k=0; k<size; ++k)
        buf[j] += get_hist_val(which,i,j,k);
      if (buf[j] > 32767) // avoid overflow
        buf[j] = 32767;
    }
    put_float_line(fp,m,i,buf);
  }

  free(buf);
  fclose(fp);
  meta_free(m);

  free(meta_file);
  free(img_file);
  free(filename);
}

static void dump_combined_hist(const char *base_filename)
{
  char *filename = appendToBasename(base_filename, "_combined_hist");
  int i,j,k,size=HIST_SIZE;

  meta_parameters *m = raw_init();
  m->general->line_count = size*2;
  m->general->sample_count = size*2;
  m->general->data_type = INTEGER16;
  m->general->image_data_type = IMAGE;
  m->general->no_data = 0;
  m->general->band_count = 1;
  strcpy(m->general->bands, "Histogram");
  strcpy(m->general->basename, filename);

  char *meta_file = appendExt(filename, ".meta");
  char *img_file = appendExt(filename, ".img");
  meta_write(m, meta_file);

  FILE *fp = fopenImage(img_file, "wb");
  float *buf = MALLOC(sizeof(float)*size*2);

  // for the top size lines:
  //  left half = anisotropy/alpha
  //  right half = entropy/alpha

  for (i=0; i<size; ++i) {
    for (j=0; j<size; ++j) {
      buf[j] = 0;
      for (k=0; k<size; ++k)
        buf[j] += get_hist_val(ALPHA_ANISOTROPY,i,j,k);
    }
    for (j=0; j<size; ++j) {
      int jj = j+size;
      buf[jj] = 0;
      for (k=0; k<size; ++k)
        buf[jj] += get_hist_val(ENTROPY_ALPHA,i,j,k);
    }
    // avoid overflow
    for (j=0; j<size*2; ++j) {
      if (buf[j] > 32767) 
        buf[j] = 32767;
    }
    put_float_line(fp,m,i,buf);
  }

  // for the bottom size lines:
  //  left half = zeros
  //  right half = anisotropy/entropy

  for (j=0; j<size; ++j)
    buf[j] = 0;
  for (i=0; i<size; ++i) {
    for (j=0; j<size; ++j) {
      int jj = j+size;
      buf[jj] = 0;
      for (k=0; k<size; ++k)
        buf[jj] += get_hist_val(ANISOTROPY_ENTROPY,i,j,k);
      if (buf[jj] > 32767) 
        buf[jj] = 32767;
    }
    put_float_line(fp,m,i+size,buf);
  }

  free(buf);
  fclose(fp);
  meta_free(m);

  free(meta_file);
  free(img_file);
  free(filename);
}

static void dump_class_map(const char *base_filename, int wide)
{
  char *filename = appendToBasename(base_filename, "_class_map");

  int height = HIST_SIZE;
  int width = wide ? HIST_SIZE*2 : HIST_SIZE;

  meta_parameters *m = raw_init();
  m->general->line_count = height;
  m->general->sample_count = width;
  m->general->data_type = BYTE;
  m->general->image_data_type = IMAGE;
  strcpy(m->general->basename, filename);

  char *meta_file = appendExt(filename, ".meta");
  char *img_file = appendExt(filename, ".img");
  meta_write(m, meta_file);

  FILE *fp = fopenImage(img_file, "wb");

  int i,j;
  float *buf = MALLOC(sizeof(float)*width);

  for (i=0; i<height; ++i) {
    for (j=0; j<width; ++j)
      buf[j] = class_map[i][j];
    put_float_line(fp,m,i,buf);
  }

  free(buf);
  fclose(fp);
  meta_free(m);

  free(meta_file);
  free(img_file);
  free(filename);
}

static double calc_alpha_real(double e)
{
  // still not entirely clear why we have to put fabs() here
  double alpha = acos(fabs(e));

  // alpha should be 0-90
  if (alpha < 0 || alpha > 1.571) {
    printf("Invalid alpha value: %f\n", alpha);
    alpha = 0;
  }

  return alpha;
}

static double calc_alpha(gsl_complex z)
{
  // alpha: acos(e[0]), e=eigenvector of coherence matrix
  return calc_alpha_real(GSL_REAL(z));
}

static void add_boundary(int wide)
{
  const char *boundary_file = "classifications/ea_boundary.txt";

  if (share_file_exists(boundary_file)) {
    FILE *fp = fopen_share_file(boundary_file,"r");

    // fopen_share_file() dies if it failed to open
    assert(fp); 

    int k;
    char line[256];

    // skip past the header line, size line, blank line, headings line
    for (k=0; k<4; ++k)
      fgets(line,256,fp); 

    // now read the actual points
    while (fgets(line,256,fp)) {
      double entropy, alpha;
      sscanf(line,"%lf,%lf", &entropy, &alpha);
      int i = (HIST_SIZE-1)*(1-alpha/90.);
      int j = entropy*HIST_SIZE;
      class_map[i][j] = 255;
      if (wide)
        class_map[i][j+HIST_SIZE] = 255;
    }
    FCLOSE(fp);
  }
  else {
    asfPrintStatus("Boundary map file (ea_boundary.txt) file not found."
        "\n*** To get the entropy/alpha feasibilty region boundary on the "
        "classification\n    map, see asf_calpol.\n");
  }
}

static void do_sinclair_bands(int band1, int band2, int band3,
                              PolarimetricImageRows *img_rows,
                              int line, int l, int multi, int chunk_size,
                              meta_parameters *outMeta, FILE *fout,
                              float *buf)
{
  int j, m;
  int ns = outMeta->general->sample_count;

  if (multi) {
    // multilook case -- average all buffered lines to produce a
    // single output line
    if (band1 >= 0) {
      for (j=0; j<ns; ++j) {
        buf[j] = 0.0;
        for (m=0; m<chunk_size; ++m)
          buf[j] += complex_amp(img_rows->s2_lines[m][j].hh);
        buf[j] /= (float)chunk_size;
      }
      put_band_float_line(fout, outMeta, band1, line, buf);
    }
    if (band2 >= 0) {
      for (j=0; j<ns; ++j) {
        buf[j] = 0.0;
        for (m=0; m<chunk_size; ++m) {
          complexFloat c = complex_add(img_rows->s2_lines[m][j].hv,
                                       img_rows->s2_lines[m][j].vh);
          buf[j] += complex_amp(complex_scale(c, 0.5));
        }
        buf[j] /= (float)chunk_size;
      }
      put_band_float_line(fout, outMeta, band2, line, buf);
    }
    if (band3 >= 0) {
      for (j=0; j<ns; ++j) {
        buf[j] = 0.0;
        for (m=0; m<chunk_size; ++m)
          buf[j] += complex_amp(img_rows->s2_lines[m][j].vv);
        buf[j] /= (float)chunk_size;
      }
      put_band_float_line(fout, outMeta, band3, line, buf);
    }
  }
  else {
    // not multilooking -- no averaging necessary
    if (band1 >= 0) {
      for (j=0; j<ns; ++j)
        buf[j] = complex_amp(img_rows->s2_lines[l][j].hh);
      put_band_float_line(fout, outMeta, band1, line, buf);
    }
    if (band2 >= 0) {
      for (j=0; j<ns; ++j) {
        complexFloat c = complex_add(img_rows->s2_lines[l][j].hv,
                                     img_rows->s2_lines[l][j].vh);
        buf[j] = complex_amp(complex_scale(c, 0.5));
      }
      put_band_float_line(fout, outMeta, band2, line, buf);
    }
    if (band3 >= 0) {
      for (j=0; j<ns; ++j)
        buf[j] = complex_amp(img_rows->s2_lines[l][j].vv);
      put_band_float_line(fout, outMeta, band3, line, buf);
    }
  }
}

static void do_pauli_bands(int band1, int band2, int band3,
                           PolarimetricImageRows *img_rows,
                           int line, int l, int multi, int chunk_size,
                           meta_parameters *outMeta, FILE *fout,
                           float *buf)
{
  int j, m;
  int ns = outMeta->general->sample_count;

  if (multi) {
    // multilook case -- average all buffered lines to produce a
    // single output line
    if (band1 >= 0) {
      for (j=0; j<ns; ++j) {
        buf[j] = 0.0;
        for (m=0; m<chunk_size; ++m)
          buf[j] += img_rows->pauli_lines[m][j].A;
	//buf[j] += complex_amp(img_rows->pauli_lines[m][j].A);
        buf[j] /= (float)chunk_size;
      }
      put_band_float_line(fout, outMeta, band1, line, buf);
    }
    if (band2 >= 0) {
      for (j=0; j<ns; ++j) {
        buf[j] = 0.0;
        for (m=0; m<chunk_size; ++m)
          buf[j] += img_rows->pauli_lines[m][j].B;
	//buf[j] += complex_amp(img_rows->pauli_lines[m][j].B);
        buf[j] /= (float)chunk_size;
      }
      put_band_float_line(fout, outMeta, band2, line, buf);
    }
    if (band3 >= 0) {
      for (j=0; j<ns; ++j) {
        buf[j] = 0.0;
        for (m=0; m<chunk_size; ++m)
          buf[j] += img_rows->pauli_lines[m][j].C;
	//buf[j] += complex_amp(img_rows->pauli_lines[m][j].C);
        buf[j] /= (float)chunk_size;
      }
      put_band_float_line(fout, outMeta, band3, line, buf);
    }
  }
  else {
    // not multilooking -- no averaging necessary
    if (band1 >= 0) {
      for (j=0; j<ns; ++j)
        buf[j] = img_rows->pauli_lines[l][j].A;
      //buf[j] = complex_amp(img_rows->pauli_lines[l][j].A);
      put_band_float_line(fout, outMeta, band1, line, buf);
    }
    if (band2 >= 0) {
      for (j=0; j<ns; ++j)
        buf[j] = img_rows->pauli_lines[l][j].B;
      //buf[j] = complex_amp(img_rows->pauli_lines[l][j].B);
      put_band_float_line(fout, outMeta, band2, line, buf);
    }
    if (band3 >= 0) {
      for (j=0; j<ns; ++j)
        buf[j] = img_rows->pauli_lines[l][j].C;
      //buf[j] = complex_amp(img_rows->pauli_lines[l][j].C);
      put_band_float_line(fout, outMeta, band3, line, buf);
    }
  }
}

static void
do_coherence_bands(int entropy_band, int anisotropy_band, int alpha_band,
                   int class_band,
                   PolarimetricImageRows *img_rows,
                   int line, int l, int multi, int chunk_size,
                   gsl_matrix_complex *T, gsl_vector *eval,
                   gsl_matrix_complex *evec, gsl_eigen_hermv_workspace *ws,
                   meta_parameters *outMeta, FILE *fout,
                   float *buf, classifier_t *classifier)
{
  if (entropy_band >= 0 || anisotropy_band >= 0 || alpha_band >= 0 || 
      class_band >= 0)
  {
    int ns = outMeta->general->sample_count;
    int onl = outMeta->general->line_count;

    float *entropy = MALLOC(sizeof(float)*ns);
    float *anisotropy = MALLOC(sizeof(float)*ns);
    float *alpha = MALLOC(sizeof(float)*ns);

    // size of the horizontal window, used for ensemble averaging
    // actual window size is hw*2+1
    int hw;
    if (multi)
      hw = 0; // no horizontal averaging
    else
      hw = 2; // 5 pixels averaging horizontally

    // coherence -- do ensemble averaging for each element
    int j;
    for (j=0; j<ns; ++j) {
      int ii,jj,m;
      for (ii=0; ii<3; ++ii) {
        for (jj=0; jj<3; ++jj) {
          gsl_complex c = gsl_complex_rect(0,0);
          int k,n=0;
          for (m=0; m<chunk_size; ++m) {
            for (k=j-hw;k<=j+hw;++k) {
              if (k>=0 && k<ns && m+line>l && m+line<onl-l) {
                ++n;
                complexFloat f =
                  img_rows->coh_lines[m][k]->coeff[ii][jj];
                c = gsl_complex_add_real(c, f.real);
                c = gsl_complex_add_imag(c, f.imag);
              }
            }
          }
          if (n>1) {
            gsl_complex_div_real(c, (float)n);
          }
          gsl_matrix_complex_set(T,ii,jj,c);
        }
      }
      
      gsl_eigen_hermv(T, eval, evec, ws);
      gsl_eigen_hermv_sort(eval, evec, GSL_EIGEN_SORT_ABS_DESC);
      
      double e1 = gsl_vector_get(eval, 0);
      double e2 = gsl_vector_get(eval, 1);
      double e3 = gsl_vector_get(eval, 2);
      
      double eT = e1+e2+e3;
      
      double P1 = e1/eT;
      double P2 = e2/eT;
      double P3 = e3/eT;
      
      double P1l3 = log3(P1);
      double P2l3 = log3(P2);
      double P3l3 = log3(P3);
      
      // If a Pn value is small enough, the log value will be NaN.
      // In this case, the value of -Pn*log3(Pn) is supposed to be
      // zero - we have to force it.
      entropy[j] =
        (meta_is_valid_double(P1l3) ? -P1*P1l3 : 0) +
        (meta_is_valid_double(P2l3) ? -P2*P2l3 : 0) +
        (meta_is_valid_double(P3l3) ? -P3*P3l3 : 0);
      
      // mathematically, entropy is limited to be between 0 and 1.
      // however it sometimes is just a bit out of that range due
      // to numerical anomalies
      if (!meta_is_valid_double(entropy[j]))
        entropy[j] = 0.0;
      else if (entropy[j] < 0)
        entropy[j] = 0.0;
      else if (entropy[j] > 1)
        entropy[j] = 1.0;
      
      if (e2+e3 != 0)
        anisotropy[j] = (e2-e3)/(e2+e3);
      else
        anisotropy[j] = 0;
      
      // as for entropy, anisotropy is limited to be between 0 and 1.
      // guard against numerical anomalies (usually this is due to
      // one really big eigenvalue)
      if (!meta_is_valid_double(anisotropy[j]))
        anisotropy[j] = 0.0;
      else if (anisotropy[j] < 0)
        anisotropy[j] = 0.0;
      else if (anisotropy[j] > 1)
        anisotropy[j] = 1.0;
      
      // calculate the "mean alpha" (mean scattering angle)
      // this is the polar angle when expressing each eigenvector
      // in spherical coordinates.  the mean alpha is weighted by
      // the eigenvector (so weight by P1-3)
      double alpha1 = calc_alpha(gsl_matrix_complex_get(evec, 0, 0));
      double alpha2 = calc_alpha(gsl_matrix_complex_get(evec, 0, 1));
      double alpha3 = calc_alpha(gsl_matrix_complex_get(evec, 0, 2));
      
      alpha[j] = R2D*(P1*alpha1 + P2*alpha2 + P3*alpha3);
      if (!meta_is_valid_double(alpha[j]))
        alpha[j] = 0.0;
    }
    
    if (entropy_band >= 0)
      put_band_float_line(fout, outMeta, entropy_band, line, entropy);
    if (anisotropy_band >= 0)
      put_band_float_line(fout, outMeta, anisotropy_band, line, anisotropy);
    if (alpha_band >= 0)
      put_band_float_line(fout, outMeta, alpha_band, line, alpha);
    
    if (class_band >= 0) {
      assert(classifier != NULL);
      for (j=0; j<ns; ++j) {
        buf[j] = (float)classify(classifier, entropy[j], anisotropy[j],
                                 alpha[j]);
      }
      put_band_float_line(fout, outMeta, class_band, line, buf);
    }

    for (j=0; j<ns; ++j) {
      int entropy_index = entropy[j]*(float)HIST_SIZE;
      if (entropy_index<0) entropy_index=0;
      if (entropy_index>HIST_SIZE-1) entropy_index=HIST_SIZE-1;
            
      int alpha_index = HIST_SIZE-1-alpha[j]/90.0*(float)HIST_SIZE;
      if (alpha_index<0) alpha_index=0;
      if (alpha_index>HIST_SIZE-1) alpha_index=HIST_SIZE-1;
      
      //printf("%10.1f %10.1f %5d %5d --> %4d\n",
      //       entropy[j], alpha[j],
      //       entropy_index, alpha_index,
      //      ea_hist[entropy_index][alpha_index]+1);
      int anisotropy_index = anisotropy[j]*(float)HIST_SIZE;
      hist_vals[entropy_index][alpha_index][anisotropy_index] += 1;
    }

    free(entropy);
    free(anisotropy);
    free(alpha);
  }
}

static int verify_equal_re(const char *id, float lhs, float rhs)
{
  if (fabs(lhs - rhs) > .001) {
    //printf("Not equal (%s): %f != %f\n", id, lhs, rhs);
    return FALSE;
  }
  return TRUE;
}

static int verify_equal_cpx(const char *id, complexFloat lhs, complexFloat rhs)
{
  if (fabs(lhs.real - rhs.real) > .001 ||
      fabs(lhs.imag - rhs.imag) > .001)
  {
    //printf("Not equal (%s): (%f,%f) != (%f,%f)\n", id,
    //       lhs.real, lhs.imag, rhs.real, rhs.imag);
    return FALSE;
  }
  return TRUE;
}

static int verify_fd(float hh2, float vv2, complexFloat hhvv, float fs,
                     float fd, complexFloat alpha, complexFloat beta)
{
  int ok1,ok2,ok3;

  // (1) hh2 = fs*beta*beta + fd*alpha*alpha
  ok1 = verify_equal_re("Eq. 1", hh2, fs*complex_amp_sqr(beta) +
                                      fd*complex_amp_sqr(alpha));
  // (2) vv2 = fs + fd
  ok2 = verify_equal_re("Eq. 2", vv2, fs+fd);

  // (3) hhvv = fs*beta + fd*alpha
  ok3 = verify_equal_cpx("Eq. 3", hhvv, complex_add(
                           complex_scale(beta, fs), complex_scale(alpha, fd)));

  return ok1 && ok2 && ok3;
}

static void solve_fd1(float hh2, float vv2, complexFloat hhvv,
                      float *fs, float *fd, complexFloat *beta)
{
  // 3 equations with 3 unknowns: (complex)
  //   |hh|^2 = fd + |b|^2 * fs
  //   |vv|^2 = fs + fd
  //   hh*conj(vv) = b*fs - fd
  
  // fs, fd are real.  beta (b) is complex.
  // splitting beta into br and bi (real and imaginary)
  // 4 equations and 4 unknown: (real)
  //   |hh|^2 = fd + |b|^2 * fs
  //   |vv|^2 = fs + fd
  //   Re(hh*conj(vv)) = br*fs - fd
  //   Im(hh*conj(vv)) = bi*fs

  // For brevity:
  float x = hhvv.real;
  float y = hhvv.imag;
  float h = hh2;
  float v = vv2;

  // Analytic solution of the 4 equations & 4 unknowns:
  //   fd = (-x^2 - y^2 + hv) / D1
  //   br = (hx + v^2 + hv + 3xv + 2x^2) / D2 - 1
  //   bi = D1 * y / D2
  //   fs = D2 / D1
  // where
  //   D1 = h + v + 2x
  //   D2 = v^2 + 2xv + x^2 + y^2
  
  float D1 = h + v + 2.*x;
  float D2 = v*v + 2.*x*v + x*x + y*y;
  
  *fd = (h*v - x*x - y*y) / D1;
  *fs = D2 / D1;
     
  float br = (h*x + v*v + h*v + 3.*x*v + 2.*x*x) / D2 - 1.;
  float bi = D1 * y / D2;
     
  *beta = complex_new(br,bi);
}

static void solve_fd2(float hh2, float vv2, complexFloat hhvv,
                      float *fs, float *fd, complexFloat *alpha)
{
  // 3 equations with 3 unknowns: (complex)
  //   |hh|^2 = fs + |a|^2 * fd
  //   |vv|^2 = fs + fd
  //   hh*conj(vv) = a*fd + fs
  
  // fs, fd are real.  alpha (a) is complex.
  // splitting alpha into ar and ai (real and imaginary)
  // 4 equations and 4 unknown: (real)
  //   |hh|^2 = fs + |a|^2 * fd
  //   |vv|^2 = fs + fd
  //   Re(hh*conj(vv)) = ar*fd + fs
  //   Im(hh*conj(vv)) = ai*fd

  // For brevity:
  float x = hhvv.real;
  float y = hhvv.imag;
  float h = hh2;
  float v = vv2;

  // Analytic solution of the 4 equations & 4 unknowns:
  //   fs = (-x^2 - y^2 + hv) / D1
  //   ar = (hx - v^2 - hv + 3xv - 2x^2) / D2 + 1
  //   ai = D1 * y / D2
  //   fd = D2 / D1
  // where
  //   D1 = h + v - 2x
  //   D2 = v^2 - 2xv + x^2 + y^2
  
  float D1 = h + v - 2.*x;
  float D2 = v*v - 2.*x*v + x*x + y*y;
  
  *fs = (h*v - x*x - y*y) / D1;
  *fd = D2 / D1;
     
  float ar = (h*x - v*v - h*v + 3.*x*v - 2.*x*x) / D2 + 1.;
  float ai = D1 * y / D2;
     
  *alpha = complex_new(ar,ai);
}

static void do_freeman(int band1, int band2, int band3,
                       PolarimetricImageRows *img_rows,
                       int line, int l, int multi, int chunk_size,
                       meta_parameters *outMeta, FILE *fout)
{
  if (band1 >= 0 || band2 >= 0 || band3 >= 0)
  {
    //if (outMeta->general->radiometry != r_SIGMA) {
    //  asfPrintError("The Freeman/Durden decomposition requires "
    //                "PowerScale (Sigma, non-db) data.\n");
    //}

    int j, m;
    int ns = outMeta->general->sample_count;

    float *hh2 = MALLOC(sizeof(float)*ns);
    float *vv2 = MALLOC(sizeof(float)*ns);
    float *hv2 = MALLOC(sizeof(float)*ns);
    complexFloat *hhvv = MALLOC(sizeof(complexFloat)*ns);
    float sf = 1.0 / (float)chunk_size;

    if (multi) {
      // multilook case -- average all buffered lines to produce a
      // single output line
      for (j=0; j<ns; ++j) {

        hh2[j] = 0.0;
        vv2[j] = 0.0;
        hhvv[j] = complex_zero();
        hv2[j] = 0.0;

        for (m=0; m<chunk_size; ++m) {
          complexFloat hh = img_rows->s2_lines[m][j].hh;
          hh2[j] += complex_amp_sqr(hh);

          complexFloat vv = img_rows->s2_lines[m][j].vv;
          vv2[j] += complex_amp_sqr(vv);

          hhvv[j] = complex_add(hhvv[j], complex_mul(hh, complex_conj(vv)));

          hv2[j] += complex_amp_sqr(img_rows->s2_lines[m][j].hv);
        }

        hh2[j] *= sf;
        vv2[j] *= sf;
        hv2[j] *= sf;

        hhvv[j] = complex_scale(hhvv[j], sf);
      }
    }
    else {
      // not multilooking -- no averaging necessary
      for (j=0; j<ns; ++j) {
        complexFloat hh = img_rows->s2_lines[l][j].hh;
        hh2[j] = complex_amp_sqr(hh);

        complexFloat vv = img_rows->s2_lines[l][j].vv;
        vv2[j] = complex_amp_sqr(vv);

        hhvv[j] = complex_mul(hh, complex_conj(vv));
        hv2[j] = complex_amp_sqr(img_rows->s2_lines[l][j].hv);
      }
    }

    float *Ps = MALLOC(sizeof(float)*ns);
    float *Pd = MALLOC(sizeof(float)*ns);
    float *Pv = MALLOC(sizeof(float)*ns);

    // now calculate fs, fd and alpha or beta for each sample, and
    // from those we can get the Ps, Pd, and Pv values
    for (j=0; j<ns; ++j) {
      float fs, fd;
      complexFloat alpha, beta;
      if (hhvv[j].real > 0) {
        // Re(Shh*conj(Svv))>0 ==> alpha=-1, solve for fs, fd, and beta
        solve_fd1(hh2[j], vv2[j], hhvv[j], &fs, &fd, &beta);
        alpha = complex_new(-1, 0);
      }
      else {
        // Re(Shh*conj(Svv))<0 ==> beta=1, solve for fs, fd, and alpha
        solve_fd2(hh2[j], vv2[j], hhvv[j], &fs, &fd, &alpha);
        beta = complex_new(1, 0);
      }

      // double-check the solution
      verify_fd(hh2[j], vv2[j], hhvv[j], fs, fd, alpha, beta);

      // now calculate the final contributions from each scattering mechanism
      Ps[j] = fs * (1. + complex_amp_sqr(beta));
      Pd[j] = fd * (1. + complex_amp_sqr(alpha));
      Pv[j] = 8. * hv2[j];

      // convert to dB
      Ps[j] = 10*log10(Ps[j]*Ps[j]);
      Pd[j] = 10*log10(Pd[j]*Pd[j]);
      Pv[j] = 10*log10(Pv[j]*Pv[j]);
    }

    free(hh2);
    free(vv2);
    free(hv2);
    free(hhvv);

    if (band1 >= 0)
      put_band_float_line(fout, outMeta, band1, line, Ps);
    if (band2 >= 0)
      put_band_float_line(fout, outMeta, band2, line, Pd);
    if (band3 >= 0)
      put_band_float_line(fout, outMeta, band3, line, Pv);

    free(Pd);
    free(Ps);
    free(Pv);
  }
}

static void do_class_map(classifier_t *classifier, int class_band, int wide,
                         const char *outFile)
{
  // build the class_map array from the hist_vals array.
  int i, j, k, size = HIST_SIZE;
  if (class_band >= 0) {
    if (!wide) {
      // non-wide: this is the Cloude-Pottier 8 classes case, where
      // we use only half of the class_map array
      for (i=0; i<size; ++i) {
        double alpha = (double)(size-1-i)/(double)size*90.;
        class_map[i][0] = 0;
        for (j=1; j<size; ++j) {
          int count = 0;
          for (k=0; k<size; ++k)
            count += hist_vals[j][i][k];
          if (count > 0) {
            double entropy = (double)j/(double)size;
            class_map[i][j] = classify(classifier, entropy, 0, alpha);
          }
          else
            class_map[i][j] = 0;
        }
      }
      // drawing the white border lines -- assumes look-up-tables use
      // "255" as white.  (cloude8.lut)
      int prev = -1;
      for (i=0; i<size; ++i) {
        double alpha = (double)(size-1-i)/(double)size*90.;
        for (j=0; j<size; ++j) {
          double entropy = (double)j/(double)size;
          int curr = classify(classifier, entropy, 0, alpha);
          if (j>0 && prev != curr)
            class_map[i][j] = 255;
          prev = curr;
        }
      }
      prev = -1;
      for (j=0; j<size; ++j) {
        double entropy = (double)j/(double)size;
        for (i=0; i<size; ++i) {
          double alpha = (double)(size-1-i)/(double)size*90.;
          int curr = classify(classifier, entropy, 0, alpha);
          if (i>0 && prev != curr)
            class_map[i][j] = 255;
          prev = curr;
        }
      }
      // if we have the file "ea_boundary.txt" in the share directory
      // (this file can be generated by calling make_entropy_alpha_boundary()
      // defined below, and exposed via asf_calpol)
      add_boundary(FALSE);

      dump_class_map(outFile, FALSE);
    }
    else {
      // wide: this is the Cloude-Pottier 16 classes case, where
      // we use the full (double-width) ea_hist array, the left half
      // being the anisotropy=0 plane, the right half being the
      // anisotropy=.5 plane (though we actually use .55 below to ensure
      // we're definitely in the >.5 blocks -- .5 could possibly be
      // classified in the (0,.5) range depending on use of <= vs < above,
      // roundoff, etc)
      for (i=0; i<size; ++i) {
        double alpha = (double)(size-1-i)/(double)size*90.;
        for (j=0; j<size*2; ++j) {
          if (j==0) {
            class_map[i][j] = 0;
          }
          else if (j == size) {
            class_map[i][j] = 255;
          }
          else {
            int jj = j;
            if (j>size) jj -= size;
            int count = 0;
            for (k=0; k<size; ++k)
              count += hist_vals[jj][i][k];
            if (count > 0) {
              double entropy = (double)jj/(double)size;
              double aniso = j > size ? 0.55 : 0.0;
              class_map[i][j] = classify(classifier, entropy, aniso, alpha);
            }
            else
              class_map[i][j] = 0;
          }
        }
      }
      // drawing the white border lines -- assumes look-up-tables use
      // "255" as white.  (cloude16.lut)
      int prev = -1;
      for (i=0; i<size; ++i) {
        double alpha = (double)(size-1-i)/(double)size*90.;
        for (j=0; j<size*2; ++j) {
          double entropy = j > size ?
            (double)(j-size)/(double)size :
            (double)j/(double)size;
          double aniso = j > size ? 0.55 : 0.0;
          int curr = classify(classifier, entropy, aniso, alpha);
          if (j>0 && prev != curr)
            class_map[i][j] = 255;
          prev = curr;
        }
      }
      prev = -1;
      for (j=0; j<size*2; ++j) {
        double entropy = j > size ?
          (double)(j-size)/(double)size :
          (double)j/(double)size;
        double aniso = j > size ? 0.55 : 0.0;
        for (i=0; i<size; ++i) {
          double alpha = (double)(size-1-i)/(double)size*90.;
          int curr = classify(classifier, entropy, aniso, alpha);
          if (i>0 && prev != curr)
            class_map[i][j] = 255;
          prev = curr;
        }
      }
      // if we have the file "ea_boundary.txt" in the share directory
      // (this file can be generated by calling make_entropy_alpha_boundary()
      // defined below, and exposed via asf_calpol)
      add_boundary(TRUE);
      
      dump_class_map(outFile, TRUE);
    }
  }
}

void polarimetric_decomp(const char *inFile, const char *outFile,
                         int amplitude_band,
                         int pauli_1_band,
                         int pauli_2_band,
                         int pauli_3_band,
                         int entropy_band,
                         int anisotropy_band,
                         int alpha_band,
                         int sinclair_1_band,
                         int sinclair_2_band,
                         int sinclair_3_band,
                         int freeman_1_band,
                         int freeman_2_band,
                         int freeman_3_band,
                         const char *classFile,
                         int class_band)
{
  int debug_mode = classFile && strcmp_case(classFile, "DEBUG")==0;
  int debug_band = -1;
  if (debug_mode) {
    debug_band = class_band;
    class_band = -1;
  }

  char *meta_name = appendExt(inFile, ".meta");
  meta_parameters *inMeta = meta_read(meta_name);
  meta_parameters *outMeta = meta_read(meta_name);

  char *in_img_name = appendExt(inFile, ".img");
  char *out_img_name = appendExt(outFile, ".img");

  int i, j, k;
  //my_randomize();

  // chunk_size represents the number of rows we keep in memory at one
  // time, centered on the row currently being processed.  This is to
  // handle the ensemble averaging that we do
  int chunk_size = 5;
  assert((chunk_size-1)%2==0); // chunk_size should be odd

  // If the image is not multilooked, we will multilook it here, and use
  // the multilooked values for the ensemble averaging.  This will result
  // in much less smoothing of the image.
  int multi = FALSE;
  if (inMeta->sar && inMeta->sar->multilook==0) {
    multi = TRUE;
    chunk_size = inMeta->sar->look_count;
  }

  // aliases
  int nl = inMeta->general->line_count;
  int ns = inMeta->general->sample_count;

  FILE *fin = fopenImage(in_img_name, "rb");
  FILE *fout = fopenImage(out_img_name, "wb");

  // this struct will hold the current row being processed, and
  // chunk_size/2 rows before & after
  PolarimetricImageRows *img_rows =
      polarimetric_image_rows_new(inMeta, chunk_size, multi);

  // make sure all bands we need are there, and find their numbers
  // and offsets
  int ok = polarimetric_image_rows_get_bands(img_rows);

  if (!ok)
      asfPrintError("Not all required bands found-- "
                    "is this SLC quad-pol data?\n");

  float *buf = MALLOC(sizeof(float)*ns);

  // at the start, we want to load the buffers as follows: (for chunk_size=5)
  //   *lines[0] = ALL ZEROS
  //   *lines[1] = ALL ZEROS
  //   *lines[2] = line 0 of the image
  //   *lines[3] = line 1 of the image
  //   *lines[4] = line 2 of the image
  // next time through the loop:
  //   *lines[0] = ALL ZEROS
  //   *lines[1] = line 0 of the image
  //   *lines[2] = line 1 of the image
  //   *lines[3] = line 2 of the image
  //   *lines[4] = line 3 of the image
  // we don't actually move the data from line n to line n-1, we just move
  // the pointers.  initially, the pointers will match the buffer (as set
  // in the loop directly above), but the second time through the pointers
  // slide down one row (line 3 is loaded into the beginning of the buffer,
  // but line pointer 4 points at the beginning).

  if (multi) {
    // multilook case:
    // preload rows --> load look_count rows, these will all be combined
    // to produce a single output line
    polarimetric_image_rows_load_new_rows(img_rows, fin);
  }
  else {
    // non-multilook case:
    // preload rows --> center of window will be row 0.
    // the next (chunk_size+1)/2 rows are also loaded, and ready to go.
    for (i=0; i<(chunk_size+1)/2; ++i)
      polarimetric_image_rows_load_next_row(img_rows, fin);    
    assert(img_rows->current_row == 0);
  }

  // output metadata differs from input only in the number
  // of bands, and the band names
  char *out_meta_name = appendExt(outFile, ".meta");
  int nBands =
      (amplitude_band>=0) +
      (pauli_1_band>=0) + (pauli_2_band>=0) + (pauli_3_band>=0) +
      (entropy_band>=0) + (anisotropy_band>=0) + (alpha_band>=0) +
      (sinclair_1_band>=0) + (sinclair_2_band>=0) + (sinclair_3_band>=0) +
      (class_band >= 0) +
      (freeman_1_band>=0) + (freeman_2_band>=0) + (freeman_3_band>=0);

  char bands[255];
  strcpy(bands, "");

  for (i=0; i<10; ++i) {
      if (amplitude_band == i)
          strcat(bands, "HH-AMP,");
      else if (sinclair_1_band == i)
          strcat(bands, "HH,");
      else if (sinclair_2_band == i)
          strcat(bands, "HV+VH_2,");
      else if (sinclair_3_band == i)
          strcat(bands, "VV,");
      else if (pauli_1_band == i)
          strcat(bands, "HH-VV,");
      else if (pauli_2_band == i)
          strcat(bands, "HV+VH,");
      else if (pauli_3_band == i)
          strcat(bands, "HH+VV,");
      else if (entropy_band == i)
          strcat(bands, "Entropy,");
      else if (anisotropy_band == i)
          strcat(bands, "Anisotro,"); // abbreviated version, agrees with GUI
      else if (alpha_band == i)
          strcat(bands, "Alpha,");
      else if (freeman_1_band == i)
          strcat(bands, "Ps,");
      else if (freeman_2_band == i)
          strcat(bands, "Pd,");
      else if (freeman_3_band == i)
          strcat(bands, "Pv,");
      else if (class_band == i) {
        if (!classFile)
          strcat(bands,"Classified,");
        else if (strncmp_case(classFile,"cloude8",7)==0)
          strcat(bands, "Cloude-Pottier-8,");
        else if (strncmp_case(classFile,"cloude16",8)==0)
          strcat(bands, "Cloude-Pottier-16,");
        else {
          // append the classification filename as the band name (minus .cla)
          char *s = appendExt(classFile,"");
          strcat(bands, s);
          strcat(bands, ",");
          free(s);
        }
      }
      else
          break;
  }

  if (strlen(bands) > 0) // chop last comma
      bands[strlen(bands)-1] = '\0';

  outMeta->general->band_count = nBands;
  strcpy(outMeta->general->bands, bands);

  // for multilooking, the number of output lines shrinks by look_count
  int onl = multi ? nl/chunk_size : nl;

  if (multi) {
    outMeta->sar->multilook = 1;
    outMeta->general->line_count = onl;
    outMeta->general->y_pixel_size *= outMeta->sar->look_count;
    outMeta->sar->azimuth_time_per_pixel *= outMeta->sar->look_count;
  }

  outMeta->general->image_data_type = POLARIMETRIC_IMAGE;
  meta_write(outMeta, out_meta_name);
  free(out_meta_name);

  // set up the classification, if needed
  classifier_t *classifier = NULL;
  if (classFile != NULL && class_band >= 0)
    classifier = read_classifier(classFile);

  // population histogram image, in entropy-alpha space
  for (i=0; i<HIST_SIZE; ++i)
    for (j=0; j<HIST_SIZE; ++j)
      for (k=0; k<HIST_SIZE; ++k)
        hist_vals[i][j][k] = 0;

  //-----------------------------------------------------------------------
  // done setting up metadata, now write the data

  // gsl infrastructure for calculating eigen- vals & vecs for the
  // coherence matrix
  gsl_matrix_complex *T = gsl_matrix_complex_alloc(3,3);
  gsl_vector *eval = gsl_vector_alloc(3);
  gsl_matrix_complex *evec = gsl_matrix_complex_alloc(3,3);
  gsl_eigen_hermv_workspace *ws = gsl_eigen_hermv_alloc(3);

  // now loop through the lines of the output image
  for (i=0; i<onl; ++i) {

      // Indicates which line in the various *lines arrays contains
      // what corresponds to line i in the output. since the line pointers
      // slide, this never changes.
      const int l = (chunk_size-1)/2;

      // normal amplitude band (usually, this is added to allow terrcorr)
      if (amplitude_band >= 0)
        put_band_float_line(fout, outMeta, amplitude_band, i, img_rows->amp);

      // if requested, generate sinlair output
      do_sinclair_bands(sinclair_1_band, sinclair_2_band, sinclair_3_band,
                        img_rows, i, l, multi, chunk_size, outMeta, fout, buf);

      // calculate the pauli output (magnitude of already-calculated
      // complex pauli basis elements), and save the requested pauli
      // bands in the output
      do_pauli_bands(pauli_1_band, pauli_2_band, pauli_3_band,
                     img_rows, i, l, multi, chunk_size, outMeta, fout, buf);


      // Freeman-Durden
      do_freeman(freeman_1_band, freeman_2_band, freeman_3_band,
                 img_rows, i, l, multi, chunk_size, outMeta, fout);

      // do any polarimetry that uses the coherence matrix
      do_coherence_bands(entropy_band, anisotropy_band, alpha_band, class_band,
                         img_rows, i, l, multi, chunk_size, T, eval, evec, ws,
                         outMeta, fout, buf, classifier);
                         

      // load the next row, if there are still more to go
      if (i<onl-1) {
          if (multi) {
              polarimetric_image_rows_load_new_rows(img_rows, fin);
          }
          else {
              polarimetric_image_rows_load_next_row(img_rows, fin);
              assert(img_rows->current_row == i+1);
          }
      }

      asfLineMeter(i,onl);
  }

  if (entropy_band >= 0 || anisotropy_band >= 0 || alpha_band >= 0 || 
      class_band >= 0)
  {

    if (entropy_band >= 0 || anisotropy_band >= 0 || alpha_band >= 0)
      asfPrintStatus("Generating population histogram...\n");
    else
      asfPrintStatus("Generating population histograms and "
                     "classification map...\n");

    // dump population graph & class map
    int wide = FALSE;
    if (class_band >= 0)
      wide = strncmp_case(classFile,"cloude16",8) == 0;

    // dump projections of the 3-d histogram
    dump_hist(outFile, ENTROPY_ALPHA);
    dump_hist(outFile, ALPHA_ANISOTROPY);
    dump_hist(outFile, ANISOTROPY_ENTROPY);
    dump_combined_hist(outFile);

    // dump classification map (entropy/alpha)
    do_class_map(classifier, class_band, wide, outFile);
  }

  gsl_vector_free(eval);
  gsl_eigen_hermv_free(ws);
  gsl_matrix_complex_free(evec);
  gsl_matrix_complex_free(T);

  polarimetric_image_rows_free(img_rows);

  fclose(fin);
  fclose(fout);

  free(buf);

  free(out_img_name);
  free(in_img_name);
  free(meta_name);
  free_classifier(classifier);

  meta_free(inMeta);
  meta_free(outMeta);
}

static int has_amp_band(const char *inFile)
{
    int ret = FALSE;
    char *mfile = appendExt(inFile, ".meta");
    if (fileExists(mfile)) {
      meta_parameters *meta = meta_read(mfile);
      if (strncmp_case(meta->general->bands, "AMP", 3) == 0)
        ret = TRUE;
      meta_free(meta);
    }
    free(mfile);
    return ret;
}

void cpx2classification(const char *inFile, const char *outFile,
                        int tc_flag, const char *classFile)
{
  if (tc_flag && has_amp_band(inFile))
    polarimetric_decomp(inFile, outFile,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        -1,-1,-1,classFile, 1);
  else
    polarimetric_decomp(inFile, outFile,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        -1,-1,-1,classFile, 0);
}

void cpx2sinclair(const char *inFile, const char *outFile, int tc_flag)
{
  asfPrintStatus("\n\nGenerating Sinclair decomposition channels\n");
  if (tc_flag && has_amp_band(inFile))
    polarimetric_decomp(inFile,outFile,0,-1,-1,-1,-1,-1,-1,1,2,3,-1,-1,-1,
                        NULL,-1);
  else
    polarimetric_decomp(inFile,outFile,-1,-1,-1,-1,-1,-1,-1,0,1,2,-1,-1,-1,
                        NULL,-1);
}

void cpx2pauli(const char *inFile, const char *outFile, int tc_flag)
{
  asfPrintStatus("\n\nGenerating Pauli decomposition channels\n");
  if (tc_flag && has_amp_band(inFile))
    polarimetric_decomp(inFile,outFile,0,1,2,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        NULL,-1);
  else
    polarimetric_decomp(inFile,outFile,-1,0,1,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        NULL,-1);
}

void cpx2cloude_pottier(const char *inFile, const char *outFile, int tc_flag)
{
  asfPrintStatus("\n\nCalculating entropy, anisotropy and alpha "
		 "for Cloude-Pottier classification\n");
  cpx2classification(inFile, outFile, tc_flag, "cloude8.cla");
}

void cpx2cloude_pottier8(const char *inFile, const char *outFile, int tc_flag)
{
  asfPrintStatus("\n\nCalculating entropy, anisotropy and alpha "
		 "for Cloude-Pottier classification (8 classes)\n");
  cpx2classification(inFile, outFile, tc_flag, "cloude8.cla");
}

void cpx2cloude_pottier16(const char *inFile, const char *outFile, int tc_flag)
{
  asfPrintStatus("\n\nCalculating entropy, anisotropy and alpha "
		 "for Cloude-Pottier classification (16 classes)\n");
  cpx2classification(inFile, outFile, tc_flag, "cloude16.cla");
}

void cpx2entropy_anisotropy_alpha(const char *inFile, const char *outFile,
                                  int tc_flag)
{
  asfPrintStatus("\n\nCalculating entropy, anisotropy and alpha.\n");
  if (tc_flag && has_amp_band(inFile))
    polarimetric_decomp(inFile,outFile,0,-1,-1,-1,1,2,3,-1,-1,-1,-1,-1,-1,
                        NULL,-1);
  else 
    polarimetric_decomp(inFile,outFile,-1,-1,-1,-1,0,1,2,-1,-1,-1,-1,-1,-1,
                        NULL,-1);
}

void cpx2freeman_durden(const char *inFile, const char *outFile, int tc_flag)
{
  asfPrintStatus("\n\nGenerating Freeman/Durden decomposition channels\n");
  if (tc_flag && has_amp_band(inFile))
    polarimetric_decomp(inFile,outFile,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,3,
                        NULL,-1);
  else
    polarimetric_decomp(inFile,outFile,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,2,
                        NULL,-1);
}

static gsl_matrix *make_diag3(double e00, double e11, double e22)
{
  gsl_matrix *mat = gsl_matrix_calloc(3, 3);
  gsl_matrix_set(mat, 0, 0, e00);
  gsl_matrix_set(mat, 1, 1, e11);
  gsl_matrix_set(mat, 2, 2, e22);
  return mat;
}

static void calc_entropy_alpha(gsl_matrix *mat, double *entropy, double *alpha)
{
  gsl_vector *eval = gsl_vector_alloc(3);
  gsl_matrix *evec = gsl_matrix_alloc(3,3);

  gsl_eigen_symmv_workspace *w = gsl_eigen_symmv_alloc(3);
  gsl_eigen_symmv(mat, eval, evec, w);
  gsl_eigen_symmv_free(w);

  gsl_eigen_symmv_sort(eval, evec, GSL_EIGEN_SORT_ABS_ASC);

  double e1 = gsl_vector_get(eval, 0);
  double e2 = gsl_vector_get(eval, 1);
  double e3 = gsl_vector_get(eval, 2);

  double eT = e1+e2+e3;

  double P1 = e1/eT;
  double P2 = e2/eT;
  double P3 = e3/eT;

  double P1l3 = log3(P1);
  double P2l3 = log3(P2);
  double P3l3 = log3(P3);

  // If a Pn value is small enough, the log value will be NaN.
  // In this case, the value of -Pn*log3(Pn) is supposed to be
  // zero - we have to force it.
  *entropy =
    (meta_is_valid_double(P1l3) ? -P1*P1l3 : 0) +
    (meta_is_valid_double(P2l3) ? -P2*P2l3 : 0) +
    (meta_is_valid_double(P3l3) ? -P3*P3l3 : 0);

  // calculate the "mean alpha" (mean scattering angle)
  // this is the polar angle when expressing each eigenvector
  // in spherical coordinates.  the mean alpha is weighted by
  // the eigenvector (so weight by P1-3)
  double alpha1 = calc_alpha_real(gsl_matrix_get(evec, 0, 0));
  double alpha2 = calc_alpha_real(gsl_matrix_get(evec, 0, 1));
  double alpha3 = calc_alpha_real(gsl_matrix_get(evec, 0, 2));

  *alpha = R2D*(P1*alpha1 + P2*alpha2 + P3*alpha3);
  if (!meta_is_valid_double(*alpha))
    *alpha = 0.0;

  gsl_vector_free(eval);
  gsl_matrix_free(evec);
}

void make_entropy_alpha_boundary(const char *fname, int size)
{
  FILE *fp = FOPEN(fname, "w");

  fprintf(fp, "Entropy/Alpha Feasible Region Boundary\n");
  fprintf(fp, "File Size: %d\n\n", size);
  fprintf(fp, "Entropy,Alpha\n");

  // number of points generated in each curve's section
  int numtop = size/2;
  int numbot1 = size/4;
  int numbot2 = size/4;

  // fix any roundoff problems, while ensuring that numbot1 & numbot2
  // are equal
  while (numtop+numbot1+numbot2 < size) {
    ++numbot2; ++numbot1;
  }
  while (numtop+numbot1+numbot2 > size) {
    --numtop;
  }

  assert(numtop+numbot1+numbot2 == size);
 
  int i;
  double entropy, alpha;

  // Generate the top curve by finding the eigenvalues of
  // the matrix:
  //   [ 1 0 0 ]
  //   [ 0 m 0 ]
  //   [ 0 0 m ]
  // for 0 <= m <= 1

  for (i=0; i<numtop; ++i) {
    double m = (double)i / (double)(numtop-1);
    gsl_matrix *mat = make_diag3(1, m, m);
    calc_entropy_alpha(mat, &entropy, &alpha);
    fprintf(fp,"%f,%f\n",entropy,alpha);
    gsl_matrix_free(mat);
    asfPercentMeter((double)i/size);
  }

  // Generate the first part of the bottom curve by finding the eigenvalues
  // of the matrix:
  //  [ 1 0 0  ]
  //  [ 0 1 0  ]
  //  [ 0 0 2m ]
  // for 0 <= m <= 0.5
  // ==> We just use m instead of 2m, and let m go from 0 to 1.

  for (i=0; i<numbot1; ++i) {
    double m = (double)i / (double)(numbot1-1);
    gsl_matrix *mat = make_diag3(1, 1, m);
    calc_entropy_alpha(mat, &entropy, &alpha);
    fprintf(fp,"%f,%f\n",entropy,alpha);
    gsl_matrix_free(mat);
    asfPercentMeter((double)(i+numtop)/size);
  }

  // Generate the second part of the bottom curve by finding the eigenvalues
  // of the matrix:
  //  [ 2m-1 0 0 ]
  //  [ 0    1 0 ]
  //  [ 0    0 1 ]
  // for 0.5 <= m <= 1
  // ==> We just use m instead of 2m-1, and let m go from 0 to 1.

  for (i=0; i<numbot2; ++i) {
    double m = (double)i / (double)(numbot2-1);
    gsl_matrix *mat = make_diag3(m, 1, 1);
    calc_entropy_alpha(mat, &entropy, &alpha);
    fprintf(fp,"%f,%f\n",entropy,alpha);    
    gsl_matrix_free(mat);
    asfPercentMeter((double)(i+numtop+numbot1)/size);
  }

  asfPercentMeter(1.0);
  FCLOSE(fp);
}
