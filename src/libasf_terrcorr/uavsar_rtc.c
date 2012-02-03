#include <asf_terrcorr.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "uavsar.h"
#include <asf_endian.h>

int uavsar_rtc(const char *input_file, const char *correction_file,
               const char *annotation_file, const char *output_file)
{
  asfPrintStatus("Input file: %s\n", input_file);
  asfPrintStatus("Correction file: %s\n", correction_file);
  asfPrintStatus("Annotation file: %s\n", annotation_file);
  asfPrintStatus("Output file: %s\n", output_file);

  if (!fileExists(input_file))
    asfPrintError("Not found: %s\n", input_file);
  if (!fileExists(correction_file))
    asfPrintError("Not found: %s\n", correction_file);
  if (!fileExists(annotation_file))
    asfPrintError("Not found: %s\n", annotation_file);

  asfPrintStatus("Reading metadata...\n");
  uavsar_polsar *polsar_params = read_uavsar_polsar_params(annotation_file,
                                                           POLSAR_GRD);
  meta_parameters *meta_scale = uavsar_polsar2meta(polsar_params);
  meta_parameters *meta_input = uavsar_polsar2meta(polsar_params);

  int nl = meta_input->general->line_count;
  int ns = meta_input->general->sample_count;

  asfPrintStatus("Applying corrections...\n");

  int is_complex = !(
    strstr(input_file, "HHHH") || strstr(input_file, "HVHV") || strstr(input_file, "VVVV"));

  float *scale_buf = MALLOC(sizeof(float)*ns);

  int ii, jj;

  FILE *ifp = FOPEN(input_file, "rb");
  FILE *ofp = FOPEN(output_file, "wb");
  FILE *sfp = FOPEN(correction_file, "rb");

  if (is_complex) {
    asfPrintStatus("Correcting complex data...");
    meta_input->general->data_type = COMPLEX_REAL32;
    complexFloat *buf = MALLOC(sizeof(complexFloat)*ns);
    for (ii=0; ii<nl; ++ii) {
      get_float_line(sfp, meta_scale, ii, scale_buf);
      get_complexFloat_line(ifp, meta_input, ii, buf);
      for (jj=0; jj<ns; ++jj) {
        //ieee_big32(scale_buf[jj]);
        ieee_big32(buf[jj].real);
        ieee_big32(buf[jj].imag);
        buf[jj].real *= scale_buf[jj];
        buf[jj].imag *= scale_buf[jj];
      }
      put_complexFloat_line(ofp, meta_input, ii, buf);
      asfLineMeter(ii,nl);
    }
    FREE(buf);
  }
  else {
    asfPrintStatus("Correcting real data...");
    float *buf = MALLOC(sizeof(float)*ns);
    for (ii=0; ii<nl; ++ii) {
      get_float_line(sfp, meta_scale, ii, scale_buf);
      get_float_line(ifp, meta_input, ii, buf);
      for (jj=0; jj<ns; ++jj) {
        //ieee_big32(scale_buf[jj]);
        ieee_big32(buf[jj]);
        buf[jj] *= scale_buf[jj];
      }
      put_float_line(ofp, meta_input, ii, buf);
      asfLineMeter(ii,nl);
    }
    FREE(buf);
  }

  FCLOSE(ifp);
  FCLOSE(ofp);
  FCLOSE(sfp);
  FREE(scale_buf);
  meta_free(meta_scale);
  meta_free(meta_input);

  return 0;
}
