/*****************************************************************************
NAME:Patch.c

SYNOPSIS:
	Encapsulates global-independent routines for SAR processing
        a patch of input data.

DESCRIPTION:
	A "patch" is any contiguous rectangle of SAR data.  To
        conserve memory, SAR processing is done on a patch-by-patch basis.

EXTERNAL ASSOCIATES:

FILE REFERENCES:

PROGRAM HISTORY:

HARDWARE/SOFTWARE LIMITATIONS:

BUGS:

*****************************************************************************/
#include "asf.h"
#include <unistd.h>
#include "ardop_defs.h"
#include "../../include/asf_endian.h"

/* Functions in calibration.c (date: Jan 2003) */
void calculateRCS(int projectionFlag, meta_parameters *meta, float *DNsquared,
                  float *radarCrossSection, int curLine,int numSamples, 
		  const satellite *s);
void intensity(int n_range,float *pwrs,float *amps);

extern struct ARDOP_PARAMS g;/*ARDOP Globals, defined in ardop_params.h*/

/*
setPatchLoc: 
Update slantToFirst / doppler parameters for number of skipped samples.
Update resampling parameters for the number of skipped lines and samples.
Store patch location to patch variables.
leftFile is the starting sample, relative to the beginning of the file line.
leftSamp is the starting sample, relative to the beginning of the good data.
*/
void setPatchLoc(patch *p,satellite *s,meta_parameters *meta,int leftFile,int leftSamp,
		 int top)
{
  /*Update slant range.*/
  p->slantToFirst = s->orig_slantToFirst + leftFile * p->slantPer;
  
  /*Update doppler.*/
  p->fd=(s->orig_fd+s->orig_fdd*leftFile+s->orig_fddd*leftFile*leftFile)*s->prf;
  p->fdd=(s->orig_fdd+2*s->orig_fddd*leftFile)*s->prf;
  p->fddd=s->orig_fddd*s->prf;
  
  /*Figure out resampling coefficents at given line.*/
  p->xResampScale = s->sloper+top*s->dsloper;
  p->xResampOffset = s->interr+top*s->dinterr;
  p->yResampScale = s->slopea+top*s->dslopea;
  p->yResampOffset = s->intera+top*s->dintera;
  
  /*Compensate resampling coefficents for skipped samples.*/
  p->xResampOffset+=p->xResampScale*leftSamp;
  p->yResampOffset+=p->yResampScale*leftSamp;
  
  /*Store patch location:*/
  p->fromSample=leftFile;
  p->fromLine=top;
  
  /*Fetch state vector*/
  {
    double tCenter=meta_get_time(meta, p->fromLine+p->n_az/2 ,0);
    stateVector centerSt=meta_get_stVec(meta,tCenter);
    fixed2gei(&centerSt,0.0);/*Puts state vector in inertial frame*/	
    p->g=init_geolocate(&centerSt);
  }
}


meta_parameters *raw_init(void);
/*
  DebugWritePatch:
  Outputs the current patch trans array, and
  converts it to amplitude and phase.
*/
void debugWritePatch(const patch *p,char *basename)
{
  FILE *fp;
  char cmd[512],name[320],outname[256],multilookname[320],exportname[320];
  /*	meta_parameters *meta = raw_init();*/
  meta_parameters *meta;
  
  strcpy(outname,g.out);
  strcat(strcat(outname,"_"),basename);
  printf("   Outputting Debugging image '%s'...\n",outname);
  strcat(strcpy(name,outname),".img");
  
  meta = meta_read(g.in1);
  meta->general->line_count = p->n_range;
  meta->general->sample_count = p->n_az;
  meta->general->data_type = COMPLEX_REAL32;
  meta->general->image_data_type = COMPLEX_IMAGE;
  meta_write(meta, name);
  
  fp = fopenImage(name,"wb");
  
  put_complexFloat_lines(fp, meta, 0, p->n_range, p->trans);
  
  FCLOSE(fp);
  sprintf(cmd,"c2p %s %s\n", name, outname);
  system(cmd);
  sprintf(multilookname, "%s_ml.img", outname);
  sprintf(cmd,"multilook -look 2x2 -step 2x2 %s %s\n", 
	  outname, multilookname);
  system(cmd);
  sprintf(exportname, "%s_ml_rgb.img", outname);
  sprintf(cmd,"convert2jpeg %s %s\n", exportname, outname);
  system(cmd);
  sprintf(cmd, "rm %s_* %s.meta %s.img\n", outname, outname, outname);
  system(cmd);
  
  meta_free(meta);
}

/*
  processPatch:
  Performs all processing necessary on the given patch.
  Expects a fully prepared patch.
  
  - read in & range compress the data (rciq).
  - fft the data along azimuth.
  - range migrate the data (rmpatch).
  - azimuth compress the data (acpatch).
  the data is returned in the patch's trans array.
*/
void processPatch(patch *p,const getRec *signalGetRec,const rangeRef *r,
		  const satellite *s)
{
  int i;
  
  printf("   RANGE COMPRESSING CHANNELS...\n");
  elapse(0); 
  rciq(p,signalGetRec,r);
  if (!quietflag) elapse(1);
  if (s->debugFlag & AZ_RAW_T) debugWritePatch(p,"az_raw_t");
  
  printf("   TRANSFORMING LINES...\n");
  elapse(0);
  cfft1d(p->n_az,NULL,0);
  for (i=0; i<p->n_range; i++) cfft1d(p->n_az,&p->trans[i*p->n_az],-1);
  if (!quietflag) elapse(1);
  if (s->debugFlag & AZ_RAW_F) debugWritePatch(p,"az_raw_f");
  if (!(s->debugFlag & NO_RCM))
    {
      printf("   START RANGE MIGRATION CORRECTION...\n");
      elapse(0);
      rmpatch(p,s);
      if (!quietflag) elapse(1);
      if (s->debugFlag & AZ_MIG_F) debugWritePatch(p,"az_mig_f");
    }
  printf("   INVERSE TRANSFORMING LINES...\n");
  elapse(0);
  acpatch(p,s);
  if (!quietflag) elapse(1);
  
  /*	printf("  Range-Doppler done...\n");*/
}
/*
  writePatch:
  Outputs one full patch of data to the given file.
*/
void writePatch(const patch *p,const satellite *s,const file *f,int patchNo)
{
  int outLine;		 /* Counter for line base output */
  FILE *fp_amp,*fp_cpx;  /* File pointers for the amplitude and  complex outputs*/
  FILE *fp_pwr,*fp_sig;  /* File pointers for the power and Sigma_0 outputs */
  FILE *fp_gam,*fp_bet;  /* File pointers for the Gamma_0 and Beta_0 outputs */
  complexFloat *outputBuf; /* Buffer for one line of patch = n_range    */
  complexFloat *mlBuf;   /* Buffer for multilooking the amplitude image */
  float	*amps;           /* Output Amplitude  = n_az/nlooks X n_range */
  float	*pwrs;  	 /* Output power */
  char *openMode="ab";	 /* Normally append output.*/
  int mlCount=0;	 /* Counter for setting up the multilook buffer */
  int writeNoiseTable=0; /* Flag to determine whether to write the noise table and 
			    antenna pattern */
  meta_parameters *metaCpx, *metaAmp, *metaIn;
  int off_slc = f->n_az_valid * (patchNo-1);
  int off_ml = f->n_az_valid * (patchNo-1) / (f->nlooks);

  if (patchNo==1)
    openMode="wb";	 /* for first patch, truncate output. */
  
  if ((patchNo==1) && (s->vecLen!=0))
    writeNoiseTable=1;	/* If first patch AND antenna pattern correction, 
			   write the noise table */
  
  printf("   WRITING PATCH OUT...\n");
  elapse(0);
  
  /* Allocate buffer space  ------------------------*/
  amps = (float *) MALLOC(p->n_range*sizeof(float));
  pwrs = (float *) MALLOC(p->n_range*sizeof(float));
  
  outputBuf = (complexFloat *)MALLOC(p->n_range*sizeof(complexFloat));
  mlBuf = (complexFloat *)MALLOC(p->n_range*f->nlooks*sizeof(complexFloat));
  
  /* transpose and write out the complex image data and amplitude image*/
  fp_cpx=fopenImage(f->out_cpx,openMode);  
  fp_amp=fopenImage(f->out_amp,openMode);
  
  /* Now, if there were other command-line optional files to write, open the files */
    if (s->imageType.power) 
    fp_pwr=fopenImage(f->out_pwr,openMode);
  if (s->imageType.sigma) 
    fp_sig=fopenImage(f->out_sig,openMode);
  if (s->imageType.gamma) 
    fp_gam=fopenImage(f->out_gam,openMode);
  if (s->imageType.beta) 
    fp_bet=fopenImage(f->out_bet,openMode);

  /* Fill in metadata */  
  metaIn = meta_read(f->in);

  metaCpx = meta_read(f->in);
  metaCpx->general->data_type = COMPLEX_REAL32;
  metaCpx->general->image_data_type = COMPLEX_IMAGE;
  metaCpx->general->line_count = f->n_az_valid * patchNo;
  metaCpx->general->sample_count = p->n_range;
  metaCpx->general->start_line = f->firstLineToProcess + s->dop_precomp + 1;
  metaCpx->general->start_sample = f->skipFile + 1;
  metaCpx->general->x_pixel_size = f->rngpix;
  metaCpx->general->y_pixel_size = f->azpix;
  metaCpx->sar->line_increment = 1.0;
  metaCpx->sar->sample_increment = 1.0;
  meta_write(metaCpx, f->out_cpx);

  metaAmp = meta_read(f->in);
  metaAmp->general->data_type = REAL32;
  metaAmp->general->image_data_type = AMPLITUDE_IMAGE;
  metaAmp->general->line_count = f->n_az_valid / f->nlooks * patchNo;
  metaAmp->general->sample_count = p->n_range;
  metaAmp->general->start_line = f->firstLineToProcess + s->dop_precomp + 1;
  metaAmp->general->start_sample = f->skipFile + 1;
  metaAmp->general->x_pixel_size = f->rngpix;
  metaAmp->general->y_pixel_size = f->azpix;
  metaAmp->sar->line_increment = 1.0;
  metaAmp->sar->sample_increment = 1.0;
  metaAmp->sar->azimuth_time_per_pixel *= f->nlooks;
  meta_write(metaAmp, f->out_amp);

  if (s->imageType.power) {
    metaAmp->general->image_data_type = POWER_IMAGE;
    meta_write(metaAmp, f->out_pwr);
  }
  if (s->imageType.sigma) {
    metaAmp->general->image_data_type = SIGMA_IMAGE;
    meta_write(metaAmp, f->out_sig);
  }
  
  if (s->imageType.gamma) {
    metaAmp->general->image_data_type = GAMMA_IMAGE;
    meta_write(metaAmp, f->out_gam);
  }
  if (s->imageType.beta) {
    metaAmp->general->image_data_type = BETA_IMAGE;
    meta_write(metaAmp, f->out_bet);
  }

  /* This gets messy */
  for (outLine=0; outLine<f->n_az_valid; outLine++)
    {
      int j; /* loop counter */
      int base = f->firstOutputLine+outLine; /* loop counter for transposed data */
      
      if(writeNoiseTable==1)
	printf("   Writing .noise and .ant files\n");
            
      /* Print statement for the antenna pattern correction option */
      if(s->vecLen==0)
	if(!quietflag && (outLine % 1024 == 0)) 
	  printf("   ...Writing Line %i\n",outLine);
      if(s->vecLen!=0)
	if(!quietflag && (outLine % 1024 == 0)) 
	  printf("   ...Writing Line %i and applying Antenna Pattern Correction\n",
		 outLine);
      
      /* Fill up the buffers */
      for (j=0; j<p->n_range; j++,base+=p->n_az)
	{	
	  outputBuf[j] = p->trans[base];
	  
	  /* For speed, if we aren't correcting the antenna pattern, 
	     write the multi-look buffer now */
	  if(s->vecLen==0)
	    {
	      mlBuf[j+mlCount*p->n_range].real = outputBuf[j].real;
	      mlBuf[j+mlCount*p->n_range].imag = outputBuf[j].imag;
	    }	
	}			
      
      /* Apply the Antenna Pattern Correction if desired */
      if(s->vecLen!=0)
	{
	  
	  /* On the first time through, write out the noise table */
	  if(writeNoiseTable==1)
	    {
	      writeTable(metaIn,s,p->n_range);
	      writeNoiseTable=0;
	    }
	  
	  antptn_correct(metaIn,outputBuf,base,p->n_range,s);
	  if(!quietflag && (j==0)) printf("   Correcting Line %d\n",outLine);
	  
	  /* Otherwise, write the multi-look buffer now */
	  for(j=0;j<p->n_range;j++)
	    {
	      
	      mlBuf[j+mlCount*p->n_range].real = outputBuf[j].real;
	      mlBuf[j+mlCount*p->n_range].imag = outputBuf[j].imag;
	    }
	}
      
      put_complexFloat_line(fp_cpx, metaCpx, outLine+off_slc, outputBuf);
      mlCount+=1;
      /* Multilook takes f->nlooks lines of data and averages them together, 
	 and writes one line on return */
      if(mlCount==f->nlooks)
	{
	  /* multilook on the power image, then use the power image to generate 
	     an amplitude, and all other detected images from the command line. 
	     Be careful because I recycle amps after writing out the line of 
	     amplitude data, and it gets used as the sigma_0, beta_0, and gamma_0 
	     line. It my be confusing, but it uses less memory.*/
	  multilook(mlBuf,p->n_range,f->nlooks,pwrs);
	  intensity(p->n_range,pwrs,amps);
	  
	  metaAmp = meta_read(f->out_amp);
	  put_float_line(fp_amp, metaAmp, outLine/f->nlooks+off_ml, amps);
	  
	  if (s->imageType.power) {
	    metaAmp = meta_read(f->out_pwr);
	    put_float_line(fp_pwr, metaAmp, outLine/f->nlooks+off_ml, pwrs);
	  }
	  if (s->imageType.sigma)
	    {
	      metaAmp = meta_read(f->out_sig);
	      calculateRCS(SIGMA_0, metaIn, pwrs, amps,
			   base-mlCount/2, p->n_range,s);
	      put_float_line(fp_sig, metaAmp, outLine/f->nlooks+off_ml, amps);
	    }
      	  if (s->imageType.gamma)
	    {
	      metaAmp = meta_read(f->out_gam);
	      calculateRCS(GAMMA_0, metaIn, pwrs, amps,
			   base-mlCount/2, p->n_range,s);
	      put_float_line(fp_gam, metaIn, outLine/f->nlooks+off_ml, amps);
	    }	  
	  if (s->imageType.beta)
	    {
	      metaAmp = meta_read(f->out_bet);
	      calculateRCS(BETA_0, metaIn, pwrs, amps,
			   base-mlCount/2, p->n_range,s);
	      put_float_line(fp_bet, metaAmp, outLine/f->nlooks+off_ml, amps);
	    }
	  
	  mlCount=0;
	}
    }
  FCLOSE(fp_cpx);
  FCLOSE(fp_amp);
  if (s->imageType.power) 
    FCLOSE(fp_pwr);
  if (s->imageType.sigma) 
    FCLOSE(fp_sig);
  if (s->imageType.gamma) 
    FCLOSE(fp_gam);
  if (s->imageType.beta) 
    FCLOSE(fp_bet);
    
  printf("\n");
  if (logflag) printLog("\n");
  if (!quietflag) {
    printf("   AMPLITUDE IMAGE FINISHED: Wrote %i lines and %i samples\n",
	   f->n_az_valid/f->nlooks,p->n_range);	
    printf("   PATCH FINISHED: Wrote %i lines of %i samples (float)\n\n",
	   f->n_az_valid,p->n_range);
  }
  FREE((void *)amps);
  FREE((void *)pwrs);
  FREE((void *)outputBuf);
  if (!quietflag) elapse(1);
}


/*destroyPatch:
De-allocates a patch of data.
*/
void destroyPatch(patch *p)
{
  FREE(p->trans);
  FREE(p);
}
