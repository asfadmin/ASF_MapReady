#include "asf_sar.h"

static void find_band(meta_parameters *meta, const char *name,
                      int *band_num, int *band_offset, int *ok)
{
    *band_num = get_band_number(meta->general->bands,
        meta->general->band_count, name);

    if (*band_num < 0) {
        asfPrintStatus("Band '%s' not found.\n", name);
        *ok = FALSE;
    } else {
        *band_offset = *band_num * meta->general->line_count;
    }
}

static void ensemble_avg(float **amp_lines, float **phase_lines, int k, int ns,
                         float *re, float *im, int kernel_size)
{
    float amp_avg = 0;
    float phase_avg = 0;

    int i, j;

    for (i=0; i<=kernel_size; ++i) {
        for (j=k-kernel_size; j<=k+kernel_size; ++j) {
            if (j>=0 && j<ns) {
                amp_avg += amp_lines[i][j];
                phase_avg += phase_lines[i][j];
            }
        }
    }

    amp_avg /= kernel_size*kernel_size;
    phase_avg /= kernel_size*kernel_size;

    *re = amp_avg * cos(phase_avg);
    *im = amp_avg * sin(phase_avg);
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

  int i, j, k;
  const int chunk_size = 5;
  assert(chunk_size-1%2==0); // chunk_size should be odd

  // aliases
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int nb = meta->general->band_count;
  const char *bands = meta->general->bands;

  FILE *fin = fopenImage(in_img_name, "rb");

  // make sure all bands we need are there, and find their numbers
  // and offsets
  int ok = TRUE;

  int hh_amp_band, hh_amp_line_offset;
  find_band(meta, "HH-AMP", &hh_amp_band, &hh_amp_line_offset, &ok);
  int hh_phase_band, hh_phase_line_offset;
  find_band(meta, "HH-PHASE", &hh_phase_band, &hh_phase_line_offset, &ok);
  float *hh_amp = MALLOC(sizeof(float)*chunk_size*ns);
  float *hh_phase = MALLOC(sizeof(float)*chunk_size*ns);
  float **hh_amp_lines = MALLOC(sizeof(float*)*chunk_size);
  float **hh_phase_lines = MALLOC(sizeof(float*)*chunk_size);

  int hv_amp_band, hv_amp_line_offset;
  find_band(meta, "HV-AMP", &hv_amp_band, &hv_amp_line_offset, &ok);
  int hv_phase_band, hv_phase_line_offset;
  find_band(meta, "HV-PHASE", &hv_phase_band, &hv_phase_line_offset, &ok);
  float *hv_amp = MALLOC(sizeof(float)*chunk_size*ns);
  float *hv_phase = MALLOC(sizeof(float)*chunk_size*ns);
  float **hv_lines = MALLOC(sizeof(float*)*chunk_size);

  int vh_amp_band, vh_amp_line_offset;
  find_band(meta, "VH-AMP", &vh_amp_band, &vh_amp_line_offset, &ok);
  int vh_phase_band, vh_phase_line_offset;
  find_band(meta, "VH-PHASE", &vh_phase_band, &vh_phase_line_offset, &ok);
  float *vh_amp = MALLOC(sizeof(float)*chunk_size*ns);
  float *vh_phase = MALLOC(sizeof(float)*chunk_size*ns);
  float **vh_lines = MALLOC(sizeof(float*)*chunk_size);

  int vv_amp_band, vv_amp_line_offset;
  find_band(meta, "VV-AMP", &vv_amp_band, &vv_amp_line_offset, &ok);
  int vv_phase_band, vv_phase_line_offset;
  find_band(meta, "VV-PHASE", &vv_phase_band, &vv_phase_line_offset, &ok);
  float *vv_amp = MALLOC(sizeof(float)*chunk_size*ns);
  float *vv_phase = MALLOC(sizeof(float)*chunk_size*ns);
  float **vv_lines = MALLOC(sizeof(float*)*chunk_size);

  if (!ok)
      asfPrintError("Not all required bands found -- is this quad-pole data?\n");

  // initially, the line pointers point at their natural locations in
  // the buffer
  for (i=0; i<chunk_size; ++i) {
      hh_amp_lines[i] = &(hh_amp[ns*i]);
      hh_phase_lines[i] = &(hh_phase[ns*i]);
      hv_amp_lines[i] = &(hv_amp[ns*i]);
      hv_phase_lines[i] = &(hv_phase[ns*i]);
      vh_amp_lines[i] = &(vh_amp[ns*i]);
      vh_phase_lines[i] = &(vh_phase[ns*i]);
      vv_amp_lines[i] = &(vv_amp[ns*i]);
      vv_phase_lines[i] = &(vv_phase[ns*i]);
  }

  float *pauli_1 = MALLOC(sizeof(float)*ns);
  float *pauli_2 = MALLOC(sizeof(float)*ns);
  float *pauli_3 = MALLOC(sizeof(float)*ns);
  float *entropy = MALLOC(sizeof(float)*ns);
  float *anisotropy = MALLOC(sizeof(float)*ns);
  float *alpha = MALLOC(sizeof(float)*ns);

  // at the start, we want to load the buffers as follows: (chunk_size=5)
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
  int cs2 = (chunk_size-1)/2;
  for (i=0; i<cs2; ++i) {
      for (j=0; j<ns; ++j) {
          hh_amp_lines[i][j] = hh_phase_lines[i][j] = 0.0;
          hv_amp_lines[i][j] = hv_phase_lines[i][j] = 0.0;
          vh_amp_lines[i][j] = vh_phase_lines[i][j] = 0.0;
          vv_amp_lines[i][j] = vv_phase_lines[i][j] = 0.0;
      }
  }
  for (i=0; i<(chunk_size+1)/2; ++i) {
      get_band_float_line(fin, meta, hh_amp_band, i, hh_amp_lines[i+cs2]);
      get_band_float_line(fin, meta, hh_phase_band, i, hh_phase_lines[i+cs2]);
      get_band_float_line(fin, meta, hv_amp_band, i, hv_amp_lines[i+cs2]);
      get_band_float_line(fin, meta, hv_phase_band, i, hv_phase_lines[i+cs2]);
      get_band_float_line(fin, meta, vh_amp_band, i, vh_amp_lines[i+cs2]);
      get_band_float_line(fin, meta, vh_phase_band, i, vh_phase_lines[i+cs2]);
      get_band_float_line(fin, meta, vv_amp_band, i, vv_amp_lines[i+cs2]);
      get_band_float_line(fin, meta, vv_phase_band, i, vv_phase_lines[i+cs2]);
  }

  // now loop through the lines of the output image
  for (i=0; i<nl; ++i) {
      for (j=0; j<ns; ++j) {
          float hh_re, hh_im;
          ensemble_avg(hh_amp_lines, hh_phase_lines, j, ns, 
              &hh_re, &hh_im, chunk_size);

          float vh_re, vh_im;
          ensemble_avg(vh_amp_lines, vh_phase_lines, j, ns,
              &vh_re, &vh_im, chunk_size);

          float hv_re, hv_im;
          ensemble_avg(hv_amp_lines, hv_phase_lines, j, ns,
              &hv_re, &hv_im, chunk_size);

          float vv_re, vv_im;
          ensemble_avg(vv_amp_lines, vv_phase_lines, j, ns,
              &vv_re, &vv_im, chunk_size);

          //|HH-VV|
          pauli_1[j] = hypot(hh_re-vv_re, hh_im-vv_im);
          //|HV+VH|/2
          pauli_2[j] = hypot(.5*(hv_re+vh_re),.5*(hv_im+hv_im));
          //|HH+VV|
          pauli_3[j] = hypot(hh_re+vv_re, hh_im+vv_im);

          // form coherence matrix
      }

      // get ready for the next line -- slide pointers
      for (k=0; k<chunk_size-1; ++k) {
          hh_amp_lines[k] = hh_amp_lines[k+1];
          hh_phase_lines[k] = hh_phase_lines[k+1];
          hv_amp_lines[k] = hv_amp_lines[k+1];
          hv_phase_lines[k] = hv_phase_lines[k+1];
          vh_amp_lines[k] = vh_amp_lines[k+1];
          vh_phase_lines[k] = vh_phase_lines[k+1];
          vv_amp_lines[k] = vv_amp_lines[k+1];
          vv_phase_lines[k] = vv_phase_lines[k+1];
      }

      // load new data into the last row
      int last = chunk_size;
      int l = i + (chunk_size+1)/2;
      if (l < nl) {
          get_band_float_line(fin, meta, hh_amp_band, l, hh_amp_lines[last]);
          get_band_float_line(fin, meta, hh_phase_band, l, hh_phase_lines[last]);
          get_band_float_line(fin, meta, hv_amp_band, l, hv_amp_lines[last]);
          get_band_float_line(fin, meta, hv_phase_band, l, hv_phase_lines[last]);
          get_band_float_line(fin, meta, vh_amp_band, l, vh_amp_lines[last]);
          get_band_float_line(fin, meta, vh_phase_band, l, vh_phase_lines[last]);
          get_band_float_line(fin, meta, vv_amp_band, l, vv_amp_lines[last]);
          get_band_float_line(fin, meta, vv_phase_band, l, vv_phase_lines[last]);
      } else {
          // window has scrolled off top of image -- fill with zeros
          for (j=0; j<ns; ++j) {
              hh_amp_lines[last][j] = hh_phase_lines[last][j] = 0.0;
              hv_amp_lines[last][j] = hv_phase_lines[last][j] = 0.0;
              vh_amp_lines[last][j] = vh_phase_lines[last][j] = 0.0;
              vv_amp_lines[last][j] = vv_phase_lines[last][j] = 0.0;
          }
      }
  }

  fclose(fin);
  free(out_img_name);
  free(in_img_name);
  free(meta_name);
  meta_free(meta);
}
