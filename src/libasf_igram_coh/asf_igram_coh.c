#include "asf_igram_coh.h"

#define   MIN(a,b)      (((a) < (b)) ? (a) : (b))                    /* minimum */

int asf_igram_coh(int lookLine, int lookSample, int stepLine, int stepSample,
		  char *masterFile, char *slaveFile, char *outBase)
{
  char ampFile[255], phaseFile[255];
  char cohFile[255], ml_ampFile[255], ml_phaseFile[255];
  FILE *fpMaster, *fpSlave, *fpAmp, *fpPhase, *fpCoh, *fpAmp_ml, *fpPhase_ml;
  int line, sample_count, line_count, count;
  float	bin_high, bin_low, average, max=0.0, sum_a, sum_b, ampScale;
  double hist_sum=0.0, percent, percent_sum;
  long long hist_val[HIST_SIZE], hist_cnt=0;
  meta_parameters *inMeta,*outMeta, *ml_outMeta;
  complexFloat *master, *slave, *sum_igram, *sum_ml_igram;
  float *amp, *phase, *sum_cpx_a, *sum_cpx_b, *coh, *pCoh;
  float *ml_amp, *ml_phase;

  create_name(ampFile, outBase,"_igram_amp.img");
  create_name(phaseFile, outBase,"_igram_phase.img");
  create_name(ml_ampFile, outBase,"_igram_ml_amp.img");
  create_name(ml_phaseFile, outBase,"_igram_ml_phase.img");
  create_name(cohFile, outBase,"_coh.img");

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

  // Generate metadata for multilooked images
  ml_outMeta = meta_read(masterFile);
  ml_outMeta->general->data_type = REAL32;
  ml_outMeta->general->line_count = line_count/stepLine;
  ml_outMeta->general->sample_count = sample_count/stepSample;
  ml_outMeta->general->x_pixel_size *= stepSample;
  ml_outMeta->general->y_pixel_size *= stepLine;
  ml_outMeta->sar->line_increment   *= stepLine;
  ml_outMeta->sar->sample_increment *= stepSample;

  // Write metadata for multilooked interferometric amplitude
  ml_outMeta->general->image_data_type = AMPLITUDE_IMAGE;
  meta_write(ml_outMeta, ml_ampFile);

  // Write metadata for multilooked interferometric phase
  ml_outMeta->general->image_data_type = PHASE_IMAGE;
  meta_write(ml_outMeta, ml_phaseFile);

  // Write metadata for coherence image
  ml_outMeta->general->image_data_type = COHERENCE_IMAGE;
  meta_write(ml_outMeta, cohFile);

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
  fpCoh = FOPEN(cohFile,"wb");

  // Initialize histogram
  for (count=0; count<HIST_SIZE; count++) hist_val[count] = 0;

  asfPrintStatus("   Calculating interferogram and coherence ...\n\n");

  for (line=0; line<line_count; line+=stepLine)
  {
    register int offset, row, column, limitLine;
    register float denXYS1, denXYS2;
    double igram_real, igram_imag;
    int inCol;
    limitLine=MIN(lookLine, line_count-line);

    printf("   Percent completed %3.0f\r",(float)line/line_count*100.0);

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

  printf("   Percent completed %3.0f\n",(float)line/line_count*100.0);

  // Sum and print the statistics
  percent_sum = 0.0;
  asfPrintStatus("   Coherence  :  Occurrences  :  Percent\n");
  asfPrintStatus("   ---------------------------------------\n");
  for (count=0; count<HIST_SIZE; count++) {
    bin_low  = (float)(count)/(float)HIST_SIZE;
    bin_high = (float)(count+1)/(float)HIST_SIZE;
    percent  = (double)hist_val[count]/(double)hist_cnt;
    percent_sum += (float)100*percent;
    asfPrintStatus("   %.2f -> %.2f :   %.8lld       %2.3f \n",
		   bin_low,bin_high, (long long) hist_val[count],100*percent);
  }
  average = (float)hist_sum/(float)hist_cnt;
  asfPrintStatus("   ---------------------------------------\n");
  asfPrintStatus("   Maximum Coherence: %.3f\n", max);
  asfPrintStatus("   Average Coherence: %.3f  (%.1f / %lld) %f\n", 
		 average,hist_sum, hist_cnt, percent_sum);

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
  FCLOSE(fpCoh); 
  meta_free(inMeta);
  meta_free(outMeta);
  meta_free(ml_outMeta);
  return(0);
}

