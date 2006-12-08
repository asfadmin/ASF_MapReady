#include "asf.h"
#include "ifm.h"

int complex2polar(char *cpxfile, char *ampfile, char *phsfile)
{
  FILE *fdCpx, *fdAmp, *fdPhase;          /* File Pointers                    */
  int line, sample;                       /* Line & sample indices for looping*/
  int percentComplete;                    /* Percent of data processed        */
  int blockSize;                          /* Number of samples gotten         */
  float *amp, *aP, *phase, *pP;           /* Output data buffers              */
  complexFloat *cpx, *cP;                 /* Input data buffers               */
  meta_parameters *inMeta, *ampMeta, *phsMeta; /* In/Out meta structs          */

  /* Read the input meta data. Assume data_type is COMPLEX_* & make it so. */
  inMeta = meta_read(cpxfile);
  inMeta->general->data_type = meta_polar2complex(inMeta->general->data_type);
  
  /* Create & write a meta files for the output images */
  ampMeta = meta_read(cpxfile);
  ampMeta->general->data_type = meta_complex2polar(inMeta->general->data_type);
  ampMeta->general->image_data_type = AMPLITUDE_IMAGE;
  meta_write(ampMeta,ampfile);
  phsMeta = meta_read(cpxfile);
  phsMeta->general->data_type = meta_complex2polar(inMeta->general->data_type);
  phsMeta->general->image_data_type = PHASE_IMAGE;
  meta_write(phsMeta,phsfile);
  
  /* Malloc buffers, check and open files */
  cpx = (complexFloat *) MALLOC (sizeof(complexFloat)
                         * inMeta->general->sample_count * CHUNK_OF_LINES);
  amp = (float *) MALLOC (sizeof(float) * ampMeta->general->sample_count
                  * CHUNK_OF_LINES);
  phase = (float *) MALLOC (sizeof(float) * phsMeta->general->sample_count
                    * CHUNK_OF_LINES);
  fdCpx = fopenImage(cpxfile, "rb");
  fdAmp = fopenImage(ampfile, "wb");  
  fdPhase = fopenImage(phsfile, "wb");

  /* Run thru the complex file, writing real data to amp and imag data to phase */
  printf("\n");
  percentComplete = 0;
  for (line=0; line<inMeta->general->line_count; line+=CHUNK_OF_LINES) {
    if ((line*100/inMeta->general->line_count == percentComplete)) {
      printf("\rConverting complex to amp and phase: %3d%% complete.",
             percentComplete++);
      fflush(NULL);
    }
    blockSize = get_complexFloat_lines(fdCpx,inMeta,line,CHUNK_OF_LINES,cpx);
    cP = cpx;
    aP = amp;
    pP = phase;
    for (sample=0; sample<blockSize; sample++) {
      if (cP->real!=0.0 || cP->imag!=0.0) {
        *aP = sqrt((cP->real)*(cP->real) + (cP->imag)*(cP->imag));
        *pP = atan2(cP->imag, cP->real);
      }
      else { *aP = 0.0; *pP = 0.0; }
      cP++;
      aP++;
      pP++;
    }
    put_float_lines(fdAmp,   ampMeta, line, CHUNK_OF_LINES, amp);
    put_float_lines(fdPhase, phsMeta, line, CHUNK_OF_LINES, phase);
  }
  printf("\rConverted complex to amp and phase:  100%% complete.\n\n");
  
  /* close, free, halt */
  FCLOSE(fdCpx);
  FCLOSE(fdAmp);
  FCLOSE(fdPhase);
  FREE(cpx);
  FREE(amp);
  FREE(phase);\
  meta_free(inMeta);
  meta_free(ampMeta);
  meta_free(phsMeta);
}
