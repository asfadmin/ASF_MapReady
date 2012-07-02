#include "asf_sar.h"
#include "asf_raster.h"
#include "asf_meta.h"
#include "asf.h"
#include <assert.h>

void c2p(const char *infile, const char *outfile, 
         int multilook, int banded)
{
  char *meta_name = appendExt(infile, ".meta");
  char *data_name = appendExt(infile, ".img");
  c2p_ext(data_name, meta_name, outfile, multilook, banded);
  FREE(meta_name);
  FREE(data_name);
}

void c2p_ext(const char *inDataName, const char *inMetaName,
             const char *outfile, int multilook, int banded)
{
  meta_parameters *in_meta = meta_read(inMetaName);
  int data_type = in_meta->general->data_type;
  in_meta->general->data_type = meta_polar2complex(data_type);
  int band_count = in_meta->general->band_count;
  char **band_names = extract_band_names(in_meta->general->bands, band_count);
  
  // some sanity checks
  switch (data_type) {
  case COMPLEX_BYTE:
  case COMPLEX_INTEGER16:
  case COMPLEX_INTEGER32:
  case COMPLEX_REAL32:
  case COMPLEX_REAL64:
    break;
  default:
    asfPrintError("c2p: %s is not a complex image.\n", inDataName);
  }
  
  if (!in_meta->sar)
    asfPrintError("c2p: %s is missing a SAR block.\n", inDataName);
  
  asfPrintStatus("Converting complex image to amplitude/phase ...\n");
  
  int nl = in_meta->general->line_count;
  int ns = in_meta->general->sample_count;
  
  meta_parameters *out_meta = meta_read(inMetaName);
  out_meta->general->data_type = meta_complex2polar(data_type);
  out_meta->general->band_count = in_meta->general->band_count*2;
  
  FILE *fin = FOPEN(inDataName, "rb");
  char *outfile_img = appendExt(outfile, ".img");
  complexFloat *cpx = MALLOC(sizeof(complexFloat)*ns);
  float *amp = MALLOC(sizeof(float)*ns);
  float *phase = MALLOC(sizeof(float)*ns);
  
  int band, line, samp;
  
  FILE *fout = FOPEN(outfile_img, "wb");
  
  for (band=0; band<band_count; band++) {
    asfPrintStatus("\nConverting band: %s\n", band_names[band]);
    
    // get the metadata band_count correct, needed in the put_* calls
    char polarization[5];
    if (strncmp_case(band_names[band], "COMPLEX-", 8) == 0) {
      strcpy(polarization, band_names[band]+8);
      if (band == 0)
	sprintf(out_meta->general->bands, "AMP-%s,PHASE-%s", 
		polarization, polarization);
      else {
	char tmp[25];
	sprintf(tmp, ",AMP-%s,PHASE-%s", polarization, polarization);
	strcat(out_meta->general->bands, tmp);
      }
    }
    
    for (line=0; line<nl; line++) {
      get_complexFloat_line(fin, in_meta, line, cpx);
      for (samp=0; samp<ns; samp++) {
	float re = cpx[samp].real;
	float im = cpx[samp].imag;
	if (re != 0.0 || im != 0.0) {
	  amp[samp] = sqrt(re*re + im*im);
	  phase[samp] = atan2(im, re);
	} 
	else
	  amp[samp] = phase[samp] = 0.0;
      }
      put_band_float_line(fout, out_meta, band*2+0, line, amp);
      put_band_float_line(fout, out_meta, band*2+1, line, phase);
      asfPercentMeter((float)line/(float)(nl));
    }
    asfPercentMeter(1.0);
  }
  
  FCLOSE(fin);
  FCLOSE(fout);
  
  meta_write(out_meta, outfile);
  meta_free(in_meta);
  meta_free(out_meta);
  
  FREE(amp);
  FREE(phase);
  FREE(cpx);
  
  FREE(outfile_img);
}
