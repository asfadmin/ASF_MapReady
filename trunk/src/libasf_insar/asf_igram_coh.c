#include "asf.h"
#include "asf_meta.h"
#include "asf_insar.h"
#include "asf_raster.h"

#define AMP(cpx) sqrt((cpx).real*(cpx).real + (cpx).imag*(cpx).imag)
#define MIN(a,b) (((a) < (b)) ? (a) : (b))

int asf_igram_coh(int lookLine, int lookSample, int stepLine, int stepSample,
		  char *masterFile, char *slaveFile, char *outBase,
		  float *average)
{
  char ampFile[255], phaseFile[255], igramFile[512];
  char cohFile[512], ml_ampFile[255], ml_phaseFile[255], ml_igramFile[512];
  FILE *fpMaster, *fpSlave, *fpAmp, *fpPhase, *fpCoh, *fpAmp_ml, *fpPhase_ml;
  int line, sample_count, line_count, count;
  float	bin_high, bin_low, max=0.0, sum_a, sum_b, ampScale;
  double hist_sum=0.0, percent, percent_sum;
  long long hist_val[HIST_SIZE], hist_cnt=0;
  meta_parameters *inMeta,*outMeta, *ml_outMeta;
  complexFloat *master, *slave, *sum_igram, *sum_ml_igram;
  float *amp, *phase, *sum_cpx_a, *sum_cpx_b, *coh, *pCoh;
  float *ml_amp, *ml_phase;

  // FIXME: Processing flow with two-banded interferogram needed - backed out
  //        for now
  create_name(ampFile, outBase,"_igram_amp.img");
  create_name(phaseFile, outBase,"_igram_phase.img");
  create_name(ml_ampFile, outBase,"_igram_ml_amp.img");
  create_name(ml_phaseFile, outBase,"_igram_ml_phase.img");
  //create_name(igramFile, outBase,"_igram.img");
  //create_name(ml_igramFile, outBase, "_igram_ml.img");
  //sprintf(cohFile, "coherence.img");
  create_name(cohFile, outBase, "_coh.img");

  // Read input meta file
  inMeta = meta_read(masterFile);
  line_count = inMeta->general->line_count; 
  sample_count = inMeta->general->sample_count;
  ampScale = 1.0/(stepLine*stepSample);

  // Generate metadata for single-look images 
  outMeta = meta_read(masterFile);
  outMeta->general->data_type = REAL32;

  // Write metadata for interferometric amplitude
  outMeta->general->image_data_type = AMPLITUDE_IMAGE;
  meta_write(outMeta, ampFile);

  // Write metadata for interferometric phase
  outMeta->general->image_data_type = PHASE_IMAGE;
  meta_write(outMeta, phaseFile);

  /*
  // Write metadata for interferogram
  outMeta->general->image_data_type = INTERFEROGRAM;
  outMeta->general->band_count = 2;
  strcpy(outMeta->general->bands, "IGRAM-AMP,IGRAM-PHASE");
  meta_write(outMeta, igramFile);
  */	
     
  // Generate metadata for multilooked images
  ml_outMeta = meta_read(masterFile);
  ml_outMeta->general->data_type = REAL32;
  ml_outMeta->general->line_count = line_count/stepLine;
  ml_outMeta->general->sample_count = sample_count/stepSample;
  ml_outMeta->general->x_pixel_size *= stepSample;
  ml_outMeta->general->y_pixel_size *= stepLine;
  ml_outMeta->sar->multilook = 1;
  ml_outMeta->sar->line_increment   *= stepLine;
  ml_outMeta->sar->sample_increment *= stepSample;
  // FIXME: This is the wrong increment but create_dem_grid does not know any
  //        better at the moment.
  //ml_outMeta->sar->line_increment = 1;
  //ml_outMeta->sar->sample_increment = 1;

  // Write metadata for multilooked interferometric amplitude
  ml_outMeta->general->image_data_type = AMPLITUDE_IMAGE;
  meta_write(ml_outMeta, ml_ampFile);

  // Write metadata for multilooked interferometric phase
  ml_outMeta->general->image_data_type = PHASE_IMAGE;
  meta_write(ml_outMeta, ml_phaseFile);

  // Write metadata for coherence image
  ml_outMeta->general->image_data_type = COHERENCE_IMAGE;
  meta_write(ml_outMeta, cohFile);

  /*
  // Write metadata for multilooked interferogram
  ml_outMeta->general->image_data_type = INTERFEROGRAM;
  strcpy(ml_outMeta->general->bands, "IGRAM-AMP,IGRAM-PHASE");
  ml_outMeta->general->band_count = 2;
  meta_write(ml_outMeta, ml_igramFile);
  */

  // Allocate memory
  master = (complexFloat *) MALLOC(sizeof(complexFloat)*sample_count*lookLine);
  slave = (complexFloat *) MALLOC(sizeof(complexFloat)*sample_count*lookLine);
  amp = (float *) MALLOC(sizeof(float)*sample_count*lookLine);
  phase = (float *) MALLOC(sizeof(float)*sample_count*lookLine);
  ml_amp = (float *) MALLOC(sizeof(float)*sample_count/stepSample);
  ml_phase = (float *) MALLOC(sizeof(float)*sample_count/stepSample);
  coh = (float *) MALLOC(sizeof(float)*sample_count/stepSample);
  sum_cpx_a = (float *) MALLOC(sizeof(float)*sample_count);
  sum_cpx_b = (float *) MALLOC(sizeof(float)*sample_count);
  sum_igram = (complexFloat *) MALLOC(sizeof(complexFloat)*sample_count);
  sum_ml_igram = (complexFloat *) MALLOC(sizeof(complexFloat)*sample_count);

  // Open files
  fpMaster = FOPEN(masterFile,"rb");
  fpSlave = FOPEN(slaveFile,"rb");
  fpAmp = FOPEN(ampFile,"wb");
  fpPhase = FOPEN(phaseFile,"wb");
  fpAmp_ml = FOPEN(ml_ampFile,"wb");
  fpPhase_ml = FOPEN(ml_phaseFile,"wb");
  //FILE *fpIgram = FOPEN(igramFile, "wb");
  //FILE *fpIgram_ml = FOPEN(ml_igramFile, "wb");
  fpCoh = FOPEN(cohFile,"wb");

  // Initialize histogram
  for (count=0; count<HIST_SIZE; count++) hist_val[count] = 0;

  asfPrintStatus("   Calculating interferogram and coherence ...\n\n");

  for (line=0; line<line_count; line+=stepLine)
  {
    register int offset, row, column, limitLine;
    double igram_real, igram_imag;
    int inCol;
    limitLine=MIN(lookLine, line_count-line);

    printf("Percent completed %3.0f\r",(float)line/line_count*100.0);

    pCoh = coh;

    // Read in the next lines of data
    get_complexFloat_lines(fpMaster, inMeta, line, limitLine, master);
    get_complexFloat_lines(fpSlave, inMeta, line, limitLine, slave);

    // Add the remaining rows into sum vectors
    offset = sample_count;
    for (column=0; column<sample_count; column++)
    {
      offset = column;
      sum_cpx_a[column] = 0.0;
      sum_cpx_b[column] = 0.0;
      sum_igram[column].real = 0.0;
      sum_igram[column].imag = 0.0;
      sum_ml_igram[column].real = 0.0;
      sum_ml_igram[column].imag = 0.0;
      igram_real = 0.0;
      igram_imag = 0.0;

      for (row=0; row<limitLine; row++)
      {
	// Complex multiplication for interferogram generation
	igram_real = master[offset].real*slave[offset].real + 
	  master[offset].imag*slave[offset].imag;
        igram_imag = master[offset].imag*slave[offset].real - 
	  master[offset].real*slave[offset].imag;
        amp[offset] = sqrt(igram_real*igram_real + igram_imag*igram_imag);
        if (FLOAT_EQUIVALENT(igram_real, 0.0) || 
	    FLOAT_EQUIVALENT(igram_imag, 0.0))
	  phase[offset]=0.0;
        else
	  phase[offset] = atan2(igram_imag, igram_real);

       	sum_cpx_a[column] += AMP(master[offset])*AMP(master[offset]);
      	sum_cpx_b[column] += AMP(slave[offset])*AMP(slave[offset]);
       	sum_igram[column].real += igram_real;
	sum_igram[column].imag += igram_imag;
	if (line % stepLine == 0 && row < stepLine) {
	  sum_ml_igram[column].real += igram_real;
	  sum_ml_igram[column].imag += igram_imag;
	}

	offset += sample_count;
      }

      ml_amp[column] = 
	sqrt(sum_ml_igram[column].real*sum_ml_igram[column].real + 
	     sum_ml_igram[column].imag*sum_ml_igram[column].imag)*ampScale;
      if (FLOAT_EQUIVALENT(sum_ml_igram[column].real, 0.0) || 
	  FLOAT_EQUIVALENT(sum_ml_igram[column].imag, 0.0))
	ml_phase[column] = 0.0;
      else
	ml_phase[column] = atan2(sum_ml_igram[column].imag, 
				 sum_ml_igram[column].real);
    }

    // Write single-look and multilooked amplitude and phase
    put_float_lines(fpAmp, outMeta, line, stepLine, amp);
    put_float_lines(fpPhase, outMeta, line, stepLine, phase);
    put_float_line(fpAmp_ml, ml_outMeta, line/stepLine, ml_amp);
    put_float_line(fpPhase_ml, ml_outMeta, line/stepLine, ml_phase);
    //put_band_float_lines(fpIgram, outMeta, 0, line, stepLine, amp);
    //put_band_float_lines(fpIgram, outMeta, 1, line, stepLine, phase);
    //put_band_float_line(fpIgram_ml, ml_outMeta, 0, line/stepLine, ml_amp);
    //put_band_float_line(fpIgram_ml, ml_outMeta, 1, line/stepLine, ml_phase);

    // Calculate the coherence by adding from sum vectors
    for (inCol=0; inCol<sample_count; inCol+=stepSample)
    {
      register int limitSample = MIN(lookSample,sample_count-inCol);
      sum_a = 0.0;
      sum_b = 0.0;
      igram_real = 0.0;
      igram_imag = 0.0;

      // Step over multilook area and sum output columns
      for (column=0; column<limitSample; column++)
      {
	igram_real += sum_igram[inCol+column].real;
	igram_imag += sum_igram[inCol+column].imag;				
	sum_a += sum_cpx_a[inCol+column];
	sum_b += sum_cpx_b[inCol+column];
      }

      if (FLOAT_EQUIVALENT((sum_a*sum_b), 0.0))
	*pCoh = 0.0;
      else 
      {
	*pCoh = (float) sqrt(igram_real*igram_real + igram_imag*igram_imag) /
	  sqrt(sum_a * sum_b);
	if (*pCoh>1.0001)
	{ 
	  printf("   coh = %f -- setting to 1.0\n",*pCoh);
	  printf("   You shouldn't have seen this!\n");
	  printf("   Exiting.\n");
          exit(EXIT_FAILURE);
	  *pCoh=1.0;
	}
      }
     pCoh++;
    } 

    // Write out values for coherence
    put_float_line(fpCoh, ml_outMeta, line/stepLine, coh);

    // Keep filling coherence histogram
    for (count=0; count<sample_count/stepSample; count++)
    {
      register int tmp;
      tmp = (int) (coh[count]*HIST_SIZE); /* Figure out which bin this value is in */
      /* This shouldn't happen */
      if(tmp >= HIST_SIZE)
	tmp = HIST_SIZE-1;
      if(tmp < 0)
	tmp = 0;
      
      hist_val[tmp]++;        // Increment that bin for the histogram
      hist_sum += coh[count];   // Add up the values for the sum
      hist_cnt++;             // Keep track of the total number of values
      if (coh[count]>max) 
	max = coh[count];  // Calculate maximum coherence
    }
  } // End for line

  printf("Percent completed %3.0f\n",(float)line/line_count*100.0);

  // Sum and print the statistics
  percent_sum = 0.0;
  printf("   Coherence  :  Occurrences  :  Percent\n");
  printf("   ---------------------------------------\n");
  for (count=0; count<HIST_SIZE; count++) {
    bin_low  = (float)(count)/(float)HIST_SIZE;
    bin_high = (float)(count+1)/(float)HIST_SIZE;
    percent  = (double)hist_val[count]/(double)hist_cnt;
    percent_sum += (float)100*percent;
    printf("   %.2f -> %.2f :   %.8lld       %2.3f \n",
		   bin_low,bin_high, (long long) hist_val[count],100*percent);
  }
  *average = (float)hist_sum/(float)hist_cnt;
  printf("   ---------------------------------------\n");
  printf("   Maximum Coherence: %.3f\n", max);
  printf("   Average Coherence: %.3f  (%.1f / %lld) %f\n", 
		 *average,hist_sum, hist_cnt, percent_sum);

  // Free and exit
  FREE(master); 
  FREE(slave);
  FREE(amp); 
  FREE(phase);
  FREE(ml_amp); 
  FREE(ml_phase); 
  FREE(coh); 
  FCLOSE(fpMaster); 
  FCLOSE(fpSlave);
  FCLOSE(fpAmp); 
  FCLOSE(fpPhase);
  FCLOSE(fpAmp_ml); 
  FCLOSE(fpPhase_ml);
  //FCLOSE(fpIgram);
  //FCLOSE(fpIgram_ml);
  FCLOSE(fpCoh); 
  meta_free(inMeta);
  meta_free(outMeta);
  meta_free(ml_outMeta);
  return(0);
}

