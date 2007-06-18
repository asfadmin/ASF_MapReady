#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"
#include "asf_export.h"

/* local constants */
#define VERSION 4.0

int multilook(char *inFile, char *outFile, char *metaFile, char *overlay)
{
  meta_parameters *metaIn, *metaOut;
  char inAmp[255], inPhase[255], outAmp[255], outPhase[255], outRGB[255];
  FILE *fpAmpIn, *fpPhaseIn, *fpAmpOut, *fpPhaseOut, *fpRGB, *fpOverlay;
  int look_line, look_sample, step_line, step_sample, i,line, sample, row, col;
  long long nitems, newitems, in_sample_count, in_line_count, out_sample_count;
  long long out_line_count, ds;
  register float *ampIn, *phaseIn, ampScale;
  float *ampOut, *phaseOut, *rgb, *scaleIn, Sin[256],Cos[256], scale;
  complexFloat z;
  register float tmp,zImag,zReal,ampI;
  register int index,offset;
  const float convers=256.0/(2*3.14159265358979);
  
  // Create filenames and open files for reading
  create_name(inAmp, inFile, "_amp.img");
  create_name(inPhase, inFile, "_phase.img");
  metaIn = meta_read(inPhase);
  metaOut = meta_read(inPhase);
  create_name(outAmp, outFile, "_amp.img");
  create_name(outPhase, outFile, "_phase.img");
  create_name(outRGB, outFile, "_phase_rgb.img");

  // Create new metadata file for the amplitude and phase.
  look_sample = step_sample = 1;
  look_line = step_line = metaOut->sar->look_count;  
  in_sample_count = metaIn->general->sample_count;
  in_line_count = metaIn->general->line_count;
  metaOut->general->sample_count /= step_sample;
  metaOut->general->line_count /= step_line;
  out_sample_count = metaOut->general->sample_count;
  out_line_count = metaOut->general->line_count;
  metaOut->sar->line_increment = 1;
  metaOut->sar->sample_increment = 1;
  metaOut->sar->azimuth_time_per_pixel *= step_line;
  metaOut->general->x_pixel_size *= step_sample;
  metaOut->general->y_pixel_size *= step_line;
  meta_write(metaOut, outAmp);
  meta_write(metaOut, outPhase);
  meta_write(metaOut, outRGB);
  
  // Open the files
  fpAmpIn = fopenImage(inAmp, "rb");
  fpPhaseIn = fopenImage(inPhase, "rb");
  fpAmpOut = fopenImage(outAmp, "wb");
  fpPhaseOut = fopenImage(outPhase, "wb");
  fpRGB = fopenImage(outRGB, "wb");
  if (overlay)
    fpOverlay = fopenImage(overlay, "rb");
  
  for (i=0;i<256;i++) {
    float phas=((float)i)/256.0*(2*3.14159265358979);
    Sin[i]=sin(phas);
    Cos[i]=cos(phas);
  }
  
  // Set data variables
  ampScale = 1.0/(look_line*look_sample);
  nitems   = (look_line-step_line)*in_sample_count;
  newitems = step_line*in_sample_count;
  
  ds       = sizeof(float);
  ampIn    = (float *)MALLOC(ds*(newitems+nitems+look_sample));
  phaseIn  = (float *)MALLOC(ds*(newitems+nitems+look_sample));
  if (overlay)
    scaleIn  = (float *)MALLOC(ds*(newitems+nitems+look_sample));
  ampOut   = (float *)MALLOC(ds*out_sample_count);
  rgb      = (float *)MALLOC(ds*out_sample_count);
  phaseOut = (float *)MALLOC(ds*out_sample_count);
  
  // Let the user know what's happening  
  asfPrintStatus("Input is %lld lines by %lld samples\n",
		 in_line_count, in_sample_count);
  asfPrintStatus("Ouput is %lld lines by %lld samples\n\n",
		 out_line_count,out_sample_count);
  
  // Get to work
  for(line=0; line<out_line_count; line++) {
    
    get_float_lines(fpAmpIn, metaIn, line*step_line, look_line, ampIn);
    get_float_lines(fpPhaseIn, metaIn, line*step_line, look_line, phaseIn);
    if (overlay)
      get_float_lines(fpOverlay, metaOut, line, 1, scaleIn);
    
    // Begin adding data
    for (sample=0; sample<out_sample_count; sample++) { 
      tmp = 0.0, zReal=0.0, zImag=0.0;
      // Add up looking area
      for (col=0; col<look_sample; col++) {
	offset=sample*step_sample+col;
	for (row=0; row<look_line; row++) {
	  ampI = ampIn[offset];
	  index = 0xFF&((int)(phaseIn[offset]*convers));
	  tmp += ampI * ampI;
	  zReal += ampI * Cos[index];
	  zImag += ampI * Sin[index];
	  offset += in_sample_count;
	}
      }
      
      // Get phase from complex values
      z.real = zReal;
      z.imag = zImag;

      ampOut[sample] = Cabs(z)*ampScale; 
      phaseOut[sample] = Cphase(z);
      if (overlay)
	scale = scaleIn[sample];
      else
	scale = 1.0;
      if (phaseOut[sample] < 0.0)
	rgb[sample] = (phaseOut[sample] + M_PI) * scale;
      else
	rgb[sample] = phaseOut[sample] * scale;
    }
    
    // Write out data to file
    put_float_line(fpAmpOut, metaOut, line, ampOut);
    put_float_line(fpPhaseOut, metaOut, line, phaseOut);
    put_float_line(fpRGB, metaOut, line, rgb);
    
    asfLineMeter(line, out_line_count);
        
    // Reposition data for next read
    for (i=0;i<nitems;i++) {
      ampIn[i] = ampIn[i + newitems];
      phaseIn[i] = phaseIn[i + newitems];
    }
  }
  
  // Clean up
  FREE(ampIn);
  FREE(phaseIn);
  FREE(ampOut);
  FREE(rgb);
  FREE(phaseOut);
  if (overlay) {
    FREE(scaleIn);
    FCLOSE(fpOverlay);
  }
  FCLOSE(fpAmpIn);
  FCLOSE(fpPhaseIn);
  FCLOSE(fpAmpOut);
  FCLOSE(fpPhaseOut);
  FCLOSE(fpRGB);
  
  meta_free(metaIn);
  meta_free(metaOut);
  
  // Export a color version of the interferogram to JPEG
  create_name(outPhase, outFile, "_phase_rgb");
  check_return(asf_export_with_lut(JPEG, SIGMA, "interferogram.lut", 
				   outPhase, outPhase),
	       "colorized interferogram (asf_export)");

  return 0;
}
