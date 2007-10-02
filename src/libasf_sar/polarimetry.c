#include "asf_sar.h"
#include "asf_raster.h"
#include <assert.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_eigen.h>

static int find_band(meta_parameters *meta, char *name, int *ok)
{
    int band_num = get_band_number(meta->general->bands,
                                   meta->general->band_count, name);

    if (band_num < 0) {
        asfPrintStatus("Band '%s' not found.\n", name);
        *ok = FALSE;
    }

    return band_num;
}

typedef struct {
   complexFloat hh;
   complexFloat hv;
   complexFloat vh;
   complexFloat vv;
} quadPolFloat;

typedef struct {
    complexFloat A;
    complexFloat B;
    complexFloat C;
} complexVector;

typedef struct {
    int rows, columns;
    complexFloat **coeff;
} complexMatrix;

typedef struct {
   int current_row;
   int nrows; // # in held in memory, not total image rows
   meta_parameters *meta;

   quadPolFloat *data_buffer;
   quadPolFloat **lines;

   complexVector *pauli_buffer;
   complexVector **pauli_lines;

   complexMatrix **coh_buffer;
   complexMatrix ***coh_lines;

   int hh_amp_band, hh_phase_band;
   int hv_amp_band, hv_phase_band;
   int vh_amp_band, vh_phase_band;
   int vv_amp_band, vv_phase_band;

} PolarimetricImageRows;

static complexFloat complex_new(float re, float im)
{
    complexFloat ret;
    ret.real = re;
    ret.imag = im;
    return ret;
}

static complexFloat complex_new_polar(float amp, float phase)
{
    complexFloat ret;
    ret.real = amp * cos(phase);
    ret.imag = amp * sin(phase);
    return ret;
}

static complexFloat complex_zero()
{
    return complex_new(0,0);
}

static complexFloat complex_sub(complexFloat a, complexFloat b)
{
    return complex_new(a.real-b.real, a.imag-b.imag);
}

static complexFloat complex_add(complexFloat a, complexFloat b)
{
    return complex_new(a.real+b.real, a.imag+b.imag);
}

static float complex_amp(complexFloat c)
{
    return (float)hypot((float)(c.real), (float)(c.imag));
}

static complexFloat complex_scale(complexFloat c, float f)
{
    return complex_new(c.real*f, c.imag*f);
}

static complexFloat complex_conj(complexFloat c)
{
    return complex_new(c.real, -c.imag);
}

static complexFloat complex_mul(complexFloat a, complexFloat b)
{
    return complex_new(a.real*b.real - a.imag*b.imag,
                       a.real*b.imag + a.imag*b.real);
}

static complexVector complex_vector_new(complexFloat a,
                                        complexFloat b,
                                        complexFloat c)
{
    complexVector ret;
    ret.A = a;
    ret.B = b;
    ret.C = c;
    return ret;
}

static complexVector complex_vector_conj(complexVector v)
{
    return complex_vector_new(complex_conj(v.A),
        complex_conj(v.B), complex_conj(v.C));
}

static complexVector complex_vector_zero()
{
    return complex_vector_new(complex_zero(), complex_zero(), complex_zero());
}

static quadPolFloat qual_pol_zero()
{
    quadPolFloat ret;
    ret.hh = complex_zero();
    ret.vh = complex_zero();
    ret.hv = complex_zero();
    ret.vv = complex_zero();
    return ret;
}

static complexMatrix *complex_matrix_new(int rows, int columns)
{
    int i,j;
    complexMatrix *ret = MALLOC(sizeof(complexMatrix));
    ret->rows = rows;
    ret->columns = columns;
    ret->coeff = (complexFloat**)MALLOC(sizeof(complexFloat*)*rows);
    for (i=0; i<rows; ++i) {
        ret->coeff[i]=(complexFloat*)MALLOC(sizeof(complexFloat)*columns);
        for (j=0; j<columns; ++j)
            ret->coeff[i][j] = complex_zero();
    }
    return ret;
}

static void complex_matrix_free(complexMatrix *doomed)
{
    int i;
    for (i=0; i<doomed->rows; ++i)
        FREE(doomed->coeff[i]);
    FREE(doomed->coeff);
    FREE(doomed);
}

static void complex_matrix_set(complexMatrix *self, int row, int column,
                               complexFloat value)
{
    self->coeff[row][column] = value;
}

static PolarimetricImageRows *polarimetric_image_rows_new(meta_parameters *meta,
                                                          int nrows)
{
    PolarimetricImageRows *self = MALLOC(sizeof(PolarimetricImageRows));

    self->nrows = nrows;
    self->meta = meta;

    // nrows must be odd
    assert((self->nrows-1)%2==0);
    self->current_row = -(nrows-1)/2;

    int ns = meta->general->sample_count;

    self->data_buffer = CALLOC(nrows*ns, sizeof(quadPolFloat));
    self->lines = CALLOC(nrows, sizeof(quadPolFloat*));

    // initially, the line pointers point at their natural locations in
    // the buffer
    int i;
    for (i=0; i<nrows; ++i)
        self->lines[i] = &(self->data_buffer[ns*i]);

    // these guys are the pauli basis elements we've calculated for the
    // loaded rows
    self->pauli_buffer = CALLOC(nrows*ns, sizeof(complexVector));
    self->pauli_lines = CALLOC(nrows, sizeof(complexVector*));
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
    self->hh_amp_band = self->hh_phase_band = -1;
    self->hv_amp_band = self->hv_phase_band = -1;
    self->vh_amp_band = self->vh_phase_band = -1;
    self->vv_amp_band = self->vv_phase_band = -1;

    return self;
}

static int polarimetric_image_rows_get_bands(PolarimetricImageRows *self)
{
    int ok=TRUE;
    self->hh_amp_band = find_band(self->meta, "HH-AMP", &ok);
    self->hh_phase_band = find_band(self->meta, "HH-PHASE", &ok);
    self->hv_amp_band = find_band(self->meta, "HV-AMP", &ok);
    self->hv_phase_band = find_band(self->meta, "HV-PHASE", &ok);
    self->vh_amp_band = find_band(self->meta, "VH-AMP", &ok);
    self->vh_phase_band = find_band(self->meta, "VH-PHASE", &ok);
    self->vv_amp_band = find_band(self->meta, "VV-AMP", &ok);
    self->vv_phase_band = find_band(self->meta, "VV-PHASE", &ok);
    return ok;
}

static void calculate_pauli_for_row(PolarimetricImageRows *self, int n)
{
    int j, ns=self->meta->general->sample_count;
    for (j=0; j<ns; ++j) {
        quadPolFloat q = self->lines[n][j];

        // HH-VV, 2*HV, HH+VV
        self->pauli_lines[n][j] = complex_vector_new(
            complex_sub(q.hh, q.vv),
            complex_scale(q.hv, 2),
            complex_add(q.hh, q.vv));
    }
}

static void calculate_coherence_for_row(PolarimetricImageRows *self, int n)
{
    // [ A*A  B*A  C*A ]    A = HH + VV
    // [ A*B  B*B  C*B ]    B = HH - VV
    // [ A*C  B*C  C*C ]    C = 2*HV
    int j, ns=self->meta->general->sample_count;
    for (j=0; j<ns; ++j) {
        complexVector v = self->pauli_lines[n][j];
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
    }
}

static void polarimetric_image_rows_load_next_row(PolarimetricImageRows *self,
                                                  FILE *fin)
{
    // we discard the top row, slide all rows up one, then load
    // the new row into the top position

    // don't actually move any data -- update pointers into the
    // buffers

    // FIRST -- slide row pointers
    int k;
    for (k=0; k<self->nrows-1; ++k) {
      self->lines[k] = self->lines[k+1];
      self->pauli_lines[k] = self->pauli_lines[k+1];
    }

    // the next line to load will go into the spot we just dumped
    int last = self->nrows - 1;
    self->lines[last] = self->lines[0];
    self->pauli_lines[last] = self->pauli_lines[0];

    self->current_row++;

    // NEXT, load in new row into the final row
    // if we have moved off the top of the image, we will need to
    // fill with zeros, instead of loading a row
    int ns = self->meta->general->sample_count;
    float *amp_buf = MALLOC(sizeof(float)*ns);
    float *phase_buf = MALLOC(sizeof(float)*ns);
    int row = self->current_row + (self->nrows-1)/2;
    if (row < self->meta->general->line_count) {
      get_band_float_line(fin, self->meta, self->hh_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->hh_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
          self->lines[row][k].hh = complex_new_polar(amp_buf[k], phase_buf[k]);

      get_band_float_line(fin, self->meta, self->hv_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->hv_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
          self->lines[row][k].hv = complex_new_polar(amp_buf[k], phase_buf[k]);

      get_band_float_line(fin, self->meta, self->vh_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->vh_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
          self->lines[row][k].vh = complex_new_polar(amp_buf[k], phase_buf[k]);

      get_band_float_line(fin, self->meta, self->vv_amp_band, row, amp_buf);
      get_band_float_line(fin, self->meta, self->vv_phase_band, row, phase_buf);
      for (k=0; k<ns; ++k)
          self->lines[row][k].vv = complex_new_polar(amp_buf[k], phase_buf[k]);

      calculate_pauli_for_row(self, last);
      calculate_coherence_for_row(self, last);
    }
    else {
      // window has scrolled off top of image -- fill with zeros
      for (k=0; k<self->meta->general->sample_count; ++k) {
          self->lines[last][k] = qual_pol_zero();
          self->pauli_lines[last][k] = complex_vector_zero();
      }
    }

    free(amp_buf);
    free(phase_buf);
}

static void polarimetric_image_rows_free(PolarimetricImageRows* self)
{
    free(self->data_buffer);
    free(self->lines);
    free(self->pauli_buffer);
    free(self->pauli_lines);

    int i;
    for (i=0; i<self->nrows*self->meta->general->sample_count; ++i)
        complex_matrix_free(self->coh_buffer[i]);

    free(self->coh_buffer);
    free(self->coh_lines);

    free(self);
}

void polarimetric_decomp(const char *inFile, const char *outFile,
                         int pauli_1_band,
                         int pauli_2_band,
                         int pauli_3_band,
                         int entropy_band,
                         int anisotropy_band,
                         int alpha_band)
{
  char *meta_name = appendExt(inFile, ".meta");
  meta_parameters *meta = meta_read(meta_name);

  char *in_img_name = appendExt(inFile, ".img");
  char *out_img_name  = appendExt(outFile, ".img");

  int i, j;
  const int chunk_size = 5;
  assert((chunk_size-1)%2==0); // chunk_size should be odd

  // aliases
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  FILE *fin = fopenImage(in_img_name, "rb");
  FILE *fout = fopenImage(out_img_name, "wb");

  // this struct will hold the current row being processed, and
  // chunk_size/2 rows before & after
  PolarimetricImageRows *img_rows =
      polarimetric_image_rows_new(meta, chunk_size);

  // make sure all bands we need are there, and find their numbers
  // and offsets
  int ok = polarimetric_image_rows_get_bands(img_rows);

  if (!ok)
      asfPrintError("Not all required bands found -- is this quad-pole data?\n");

  float *pauli_1 = MALLOC(sizeof(float)*ns);
  float *pauli_2 = MALLOC(sizeof(float)*ns);
  float *pauli_3 = MALLOC(sizeof(float)*ns);
  float *entropy = MALLOC(sizeof(float)*ns);
  float *anisotropy = MALLOC(sizeof(float)*ns);
  float *alpha = MALLOC(sizeof(float)*ns);

  // at the start, we want to load the buffers as follows: (for chunk_size=5)
  //   XX_amp_lines[0] = ALL ZEROS
  //   XX_amp_lines[1] = ALL ZEROS
  //   XX_amp_lines[2] = line 0 of the image
  //   XX_amp_lines[3] = line 1 of the image
  //   XX_amp_lines[4] = line 2 of the image
  // next time through the loop:
  //   XX_amp_lines[0] = ALL ZEROS
  //   XX_amp_lines[1] = line 0 of the image
  //   XX_amp_lines[2] = line 1 of the image
  //   XX_amp_lines[3] = line 2 of the image
  //   XX_amp_lines[4] = line 3 of the image
  // we don't actually move the data from line n to line n-1, we just move
  // the pointers.  initially, the pointers will match the buffer (as set
  // in the loop directly above), but the second time through the pointers
  // slide down one row (line 3 is loaded into the beginning of the buffer,
  // but line pointer 4 points at the beginning).

  // preload rows --> center of window will be row 0.
  // the next (chunk_size-1)/2 rows are also loaded, and ready to go.
  for (i=0; i<(chunk_size+1)/2; ++i)
      polarimetric_image_rows_load_next_row(img_rows, fin);
  assert(img_rows->current_row == 0);

  // size of the horizontal window, used for ensemble averaging
  // actual window size is w*2+1
  const int w = 2;

  gsl_matrix_complex *T = gsl_matrix_complex_alloc(3,3);
  gsl_vector *eval = gsl_vector_alloc(3);
  gsl_matrix_complex *evec = gsl_matrix_complex_alloc(3,3);
  gsl_eigen_hermv_workspace *ws = gsl_eigen_hermv_alloc(3);

  // now loop through the lines of the output image
  for (i=0; i<nl; ++i) {

      // indicates which line in the various *lines arrays contains
      // what corresponds to line i in the output. since the line pointers
      // slide, this never changes
      const int l = (chunk_size-1)/2;

      // calculate the pauli output (magnitude of already-calculated
      // complex pauli basis elements)
      for (j=0; j<ns; ++j) {
          pauli_1[j] = complex_amp(img_rows->pauli_lines[l][j].A);
          pauli_2[j] = complex_amp(img_rows->pauli_lines[l][j].B);
          pauli_3[j] = complex_amp(img_rows->pauli_lines[l][j].C);
      }

      // save the pauli bands in the output
      if (pauli_1_band > 0)
          put_band_float_line(fout, meta, pauli_1_band, i, pauli_1);
      if (pauli_2_band > 0)
          put_band_float_line(fout, meta, pauli_2_band, i, pauli_2);
      if (pauli_3_band > 0)
          put_band_float_line(fout, meta, pauli_3_band, i, pauli_3);

      // now coherence -- do averaging for each element
      for (j=0; j<ns; ++j) {          
          int ii,jj,m;
          for (ii=0; ii<3; ++ii) {
              for (jj=0; jj<3; ++jj) {
                  gsl_complex c = gsl_complex_rect(0,0);
                  int k,n=0;
                  for (m=0; m<chunk_size; ++m) {
                      for (k=-w;k<=w;++k) {
                          if (k>0 && k<ns && i>l && i<ns-l) {
                              ++n;
                              complexFloat f =
                                  img_rows->coh_lines[m][k]->coeff[ii][jj];
                              // cheat for speed
                              c.dat[0] += f.real; c.dat[1] += f.imag;
                          }
                      }
                  }
                  c.dat[0] /= (float)n;
                  c.dat[1] /= (float)n;
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

          entropy[j] = -P1*log(P1) - P2*log(P2) - P3*log(P3);
          anisotropy[j] = (e2-e3)/(e2+e3);
      }

      // alpha
      for (j=0; j<ns; ++j) {
          alpha[j] = acos(pauli_1[j] / 
              sqrt(pauli_1[j]*pauli_1[j] + pauli_2[j]*pauli_2[j] +
                  pauli_3[j]*pauli_3[j]));
      }

      if (entropy_band > 0)
          put_band_float_line(fout, meta, entropy_band, i, entropy);
      if (anisotropy_band > 0)
          put_band_float_line(fout, meta, anisotropy_band, i, anisotropy);
      if (alpha_band > 0)
          put_band_float_line(fout, meta, alpha_band, i, alpha);

      // load the next row, if there are still more to go
      if (i<nl-1) {
          polarimetric_image_rows_load_next_row(img_rows, fin);
          assert(img_rows->current_row == i+1);
      }
  }

  gsl_vector_free(eval);
  gsl_eigen_hermv_free(ws);
  gsl_matrix_complex_free(evec);
  gsl_matrix_complex_free(T);

  polarimetric_image_rows_free(img_rows);

  fclose(fin);
  fclose(fout);

  free(pauli_1);
  free(pauli_2);
  free(pauli_3);
  free(entropy);
  free(anisotropy);
  free(alpha);

  free(out_img_name);
  free(in_img_name);
  free(meta_name);
  meta_free(meta);
}
