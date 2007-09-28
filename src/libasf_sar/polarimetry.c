#include "asf_sar.h"
#include "asf_raster.h"
#include <assert.h>

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
   int current_row;
   int nrows; // # in held in memory, not total image rows
   meta_parameters *meta;

   float *hh_amp, *hh_phase;
   float *hv_amp, *hv_phase;
   float *vh_amp, *vh_phase;
   float *vv_amp, *vv_phase;

   float **hh_amp_lines, **hh_phase_lines;
   float **hv_amp_lines, **hv_phase_lines;
   float **vh_amp_lines, **vh_phase_lines;
   float **vv_amp_lines, **vv_phase_lines;

   complexFloat *pauli_1, *pauli_2, *pauli_3;
   complexFloat **pauli_1_lines, **pauli_2_lines, **pauli_3_lines;

   int hh_amp_band, hh_phase_band;
   int hv_amp_band, hv_phase_band;
   int vh_amp_band, vh_phase_band;
   int vv_amp_band, vv_phase_band;

} PolarimetricImageRows;

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

    self->hh_amp = CALLOC(nrows*ns, sizeof(float));
    self->hh_phase = CALLOC(nrows*ns, sizeof(float));
    self->hh_amp_lines = CALLOC(nrows, sizeof(float*));
    self->hh_phase_lines = CALLOC(nrows, sizeof(float*));

    self->hv_amp = CALLOC(nrows*ns, sizeof(float));
    self->hv_phase = CALLOC(nrows*ns, sizeof(float));
    self->hv_amp_lines = CALLOC(nrows, sizeof(float*));
    self->hv_phase_lines = CALLOC(nrows, sizeof(float*));

    self->vh_amp = CALLOC(nrows*ns, sizeof(float));
    self->vh_phase = CALLOC(nrows*ns, sizeof(float));
    self->vh_amp_lines = CALLOC(nrows, sizeof(float*));
    self->vh_phase_lines = CALLOC(nrows, sizeof(float*));

    self->vv_amp = CALLOC(nrows*ns, sizeof(float));
    self->vv_phase = CALLOC(nrows*ns, sizeof(float));
    self->vv_amp_lines = CALLOC(nrows, sizeof(float*));
    self->vv_phase_lines = CALLOC(nrows, sizeof(float*));

    // initially, the line pointers point at their natural locations in
    // the buffer
    int i;
    for (i=0; i<nrows; ++i) {
        self->hh_amp_lines[i] = &(self->hh_amp[ns*i]);
        self->hh_phase_lines[i] = &(self->hh_phase[ns*i]);
        self->hv_amp_lines[i] = &(self->hv_amp[ns*i]);
        self->hv_phase_lines[i] = &(self->hv_phase[ns*i]);
        self->vh_amp_lines[i] = &(self->vh_amp[ns*i]);
        self->vh_phase_lines[i] = &(self->vh_phase[ns*i]);
        self->vv_amp_lines[i] = &(self->vv_amp[ns*i]);
        self->vv_phase_lines[i] = &(self->vv_phase[ns*i]);
    }

    // these guys are the pauli basis elements we've calculated for the
    // loaded rows
    self->pauli_1 = CALLOC(nrows*ns, sizeof(complexFloat));
    self->pauli_1_lines = CALLOC(nrows, sizeof(complexFloat*));
    self->pauli_2 = CALLOC(nrows*ns, sizeof(complexFloat));
    self->pauli_2_lines = CALLOC(nrows, sizeof(complexFloat*));
    self->pauli_3 = CALLOC(nrows*ns, sizeof(complexFloat));
    self->pauli_3_lines = CALLOC(nrows, sizeof(complexFloat*));

    // coherency matrix elements for the loaded rows
    //  ...

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

static void calculate_pauli_for_row(PolarimetricImageRows *self, int n)
{
    int j, ns=self->meta->general->sample_count;
    for (j=0; j<ns; ++j) {
        complexFloat hh = complex_new_polar(self->hh_amp_lines[n][j],
                                            self->hh_phase_lines[n][j]);
        complexFloat hv = complex_new_polar(self->hv_amp_lines[n][j],
                                            self->hv_phase_lines[n][j]);
        //complexFloat vh = complex_new_polar(self->vh_amp_lines[n][j],
        //                                    self->vh_phase_lines[n][j]);
        complexFloat vv = complex_new_polar(self->vv_amp_lines[n][j],
                                            self->vv_phase_lines[n][j]);

        // |HH-VV|
        self->pauli_1_lines[n][j] = complex_sub(hh, vv);
        self->pauli_2_lines[n][j] = complex_scale(hv, 2);
        self->pauli_3_lines[n][j] = complex_add(hh, vv);
    }
}

static void calculate_coherence_for_row(PolarimetricImageRows *self, int n)
{
    // [ A*A  B*A  C*A ]    A = HH + VV
    // [ A*B  B*B  C*B ]    B = HH - VV
    // [ A*C  B*C  C*C ]    C = 2*HV
    int j, ns=self->meta->general->sample_count;
    for (j=0; j<ns; ++j) {
        complexFloat A = self->pauli_3_lines[n][j];
        complexFloat B = self->pauli_1_lines[n][j];
        complexFloat C = self->pauli_2_lines[n][j];

        complexFloat Ac = complex_conj(A);
        complexFloat Bc = complex_conj(B);
        complexFloat Cc = complex_conj(C);

        //self->coh11[n][j] = complex_mul(Ac, A);
        //self->coh12[n][j] = complex_mul(Ac, B);
        //self->coh13[n][j] = complex_mul(Ac, C);

        //self->coh21[n][j] = complex_mul(Bc, A);
        //self->coh22[n][j] = complex_mul(Bc, B);
        //self->coh23[n][j] = complex_mul(Bc, C);

        //self->coh31[n][j] = complex_mul(Cc, A);
        //self->coh32[n][j] = complex_mul(Cc, B);
        //self->coh33[n][j] = complex_mul(Cc, C);
    }
}

static void polarimetric_image_rows_load_next_row(PolarimetricImageRows *self,
                                                  FILE *fin)
{
    // we discard the top row, slide all rows up one, then load
    // the new row into the top position

    // don't actually move any data -- update pointers into the
    // buffers

    // FIRST -- slide pointers
    int k;
    for (k=0; k<self->nrows-1; ++k) {
      self->hh_amp_lines[k] = self->hh_amp_lines[k+1];
      self->hh_phase_lines[k] = self->hh_phase_lines[k+1];
      self->hv_amp_lines[k] = self->hv_amp_lines[k+1];
      self->hv_phase_lines[k] = self->hv_phase_lines[k+1];
      self->vh_amp_lines[k] = self->vh_amp_lines[k+1];
      self->vh_phase_lines[k] = self->vh_phase_lines[k+1];
      self->vv_amp_lines[k] = self->vv_amp_lines[k+1];
      self->vv_phase_lines[k] = self->vv_phase_lines[k+1];

      self->pauli_1_lines[k] = self->pauli_1_lines[k+1];
      self->pauli_2_lines[k] = self->pauli_2_lines[k+1];
      self->pauli_3_lines[k] = self->pauli_3_lines[k+1];
    }

    // the next line to load will go into the spot we just dumped
    int last = self->nrows - 1;
    self->hh_amp_lines[last] = self->hh_amp_lines[0];
    self->hh_phase_lines[last] = self->hh_amp_lines[0];
    self->hv_amp_lines[last] = self->hv_amp_lines[0];
    self->hv_phase_lines[last] = self->hv_amp_lines[0];
    self->vh_amp_lines[last] = self->vh_amp_lines[0];
    self->vh_phase_lines[last] = self->vh_amp_lines[0];
    self->vv_amp_lines[last] = self->vv_amp_lines[0];
    self->vv_phase_lines[last] = self->vv_amp_lines[0];

    self->pauli_1_lines[last] = self->pauli_1_lines[0];
    self->pauli_2_lines[last] = self->pauli_2_lines[0];
    self->pauli_3_lines[last] = self->pauli_3_lines[0];

    self->current_row++;

    // NEXT, load in new row into the final row
    // if we have moved off the top of the image, we will need to
    // fill with zeros, instead of loading a row
    int row = self->current_row + (self->nrows-1)/2;
    if (row < self->meta->general->line_count) {
      get_band_float_line(fin, self->meta, self->hh_amp_band,
          row, self->hh_amp_lines[last]);
      get_band_float_line(fin, self->meta, self->hh_phase_band,
          row, self->hh_phase_lines[last]);
      get_band_float_line(fin, self->meta, self->hv_amp_band,
          row, self->hv_amp_lines[last]);
      get_band_float_line(fin, self->meta, self->hv_phase_band,
          row, self->hv_phase_lines[last]);
      get_band_float_line(fin, self->meta, self->vh_amp_band,
          row, self->vh_amp_lines[last]);
      get_band_float_line(fin, self->meta, self->vh_phase_band,
          row, self->vh_phase_lines[last]);
      get_band_float_line(fin, self->meta, self->vv_amp_band,
          row, self->vv_amp_lines[last]);
      get_band_float_line(fin, self->meta, self->vv_phase_band,
          row, self->vv_phase_lines[last]);

      calculate_pauli_for_row(self, last);
      calculate_coherence_for_row(self, last);
    }
    else {
      // window has scrolled off top of image -- fill with zeros
      for (k=0; k<self->meta->general->sample_count; ++k) {
          self->hh_amp_lines[last][k] = self->hh_phase_lines[last][k] = 0.0;
          self->hv_amp_lines[last][k] = self->hv_phase_lines[last][k] = 0.0;
          self->vh_amp_lines[last][k] = self->vh_phase_lines[last][k] = 0.0;
          self->vv_amp_lines[last][k] = self->vv_phase_lines[last][k] = 0.0;

          self->pauli_1_lines[last][k] = complex_new(0,0);
          self->pauli_2_lines[last][k] = complex_new(0,0);
          self->pauli_3_lines[last][k] = complex_new(0,0);
      }
    }
}

static void polarimetric_image_rows_free(PolarimetricImageRows* self)
{
    free(self->hh_amp);
    free(self->hh_phase);
    free(self->hh_amp_lines);
    free(self->hh_phase_lines);

    free(self->hv_amp);
    free(self->hv_phase);
    free(self->hv_amp_lines);
    free(self->hv_phase_lines);

    free(self->vh_amp);
    free(self->vh_phase);
    free(self->vh_amp_lines);
    free(self->vh_phase_lines);

    free(self->vv_amp);
    free(self->vv_phase);
    free(self->vv_amp_lines);
    free(self->vv_phase_lines);

    free(self);
}

void polarimetric_decomp(const char *inFile, const char *outFile,
                         int pauli_1_band,
                         int pauli_2_band,
                         int pauli_3_band,
                         int entrpy_band,
                         int anisotropy_band,
                         int alpha_band)
{
  char *meta_name = appendExt(inFile, ".meta");
  meta_parameters *meta = meta_read(meta_name);

  char *in_img_name = appendExt(inFile, ".img");
  char *out_img_name  = appendExt(outFile, ".img");

  int i, j;
  const int chunk_size = 5;
  assert(chunk_size-1%2==0); // chunk_size should be odd

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
  //float *entropy = MALLOC(sizeof(float)*ns);
  //float *anisotropy = MALLOC(sizeof(float)*ns);
  //float *alpha = MALLOC(sizeof(float)*ns);

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

  // now loop through the lines of the output image
  for (i=0; i<nl; ++i) {

      // calculate the pauli output (magnitude of already-calculated
      // complex pauli basis elements)
      for (j=0; j<ns; ++j) {
          pauli_1[j] = complex_amp(img_rows->pauli_1_lines[i][j]);
          pauli_2[j] = complex_amp(img_rows->pauli_2_lines[i][j]);
          pauli_3[j] = complex_amp(img_rows->pauli_3_lines[i][j]);
      }

      // save the pauli bands in the output
      if (pauli_1_band > 0)
          put_band_float_line(fout, meta, pauli_1_band, i, pauli_1);
      if (pauli_2_band > 0)
          put_band_float_line(fout, meta, pauli_2_band, i, pauli_2);
      if (pauli_3_band > 0)
          put_band_float_line(fout, meta, pauli_3_band, i, pauli_3);

      // now coherence

      // load the next row, if there are still more to go
      if (i<nl-1) {
          polarimetric_image_rows_load_next_row(img_rows, fin);
          assert(img_rows->current_row == i+1);
      }
  }

  polarimetric_image_rows_free(img_rows);

  fclose(fin);
  fclose(fout);

  free(out_img_name);
  free(in_img_name);
  free(meta_name);
  meta_free(meta);
}
