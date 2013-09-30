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
#include "asf_meta.h"
#include "ardop_defs.h"
#include <assert.h>
#include <asf_export.h>
#include <asf_sar.h>


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
    p->g=init_geolocate_meta(&centerSt,meta);
  }
}

// if png_flag is true, outputs a PNG.  if false, outputs a jpeg
static void patchToRGBImage(char *outname, int png_flag)
{
  update_status("Generating %s", outname);
  if (!quietflag)
    printf("   Outputting Debugging image '%s'...\n",outname);

  // input name is just the output name with ".img"
  char *polar_name = MALLOC((strlen(outname)+20)*sizeof(char));
  sprintf(polar_name, "%s_polar.img", outname);

  // will multilook, as well as conver to polar, generates a 2-band
  // file (amplitude is band 1, phase is band 2)
  c2p(outname,polar_name,FALSE,TRUE);

  // prepare for RGB conversion
  int i,j;
  meta_parameters *meta = meta_read(polar_name);
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  float *amp = MALLOC(sizeof(float)*ns);
  float *phase = MALLOC(sizeof(float)*ns);
  unsigned char *red = MALLOC(sizeof(unsigned char)*ns);
  unsigned char *grn = MALLOC(sizeof(unsigned char)*ns);
  unsigned char *blu = MALLOC(sizeof(unsigned char)*ns);
  FILE *fp = fopenImage(polar_name, "rb");
  const double TWOPI=2.*PI;
  const double PIOVER3=PI/3.;
  int n=0;

  // first/second passes are stats gathering
  asfPrintStatus("Gathering stats...\n");
  double avg=0, stddev=0;
  for (i=0; i<nl; i+=3) {
    get_float_line(fp,meta,i,amp);
    for (j=0; j<ns; j+=3) {
      avg += amp[j];
      ++n;
    }
    asfPercentMeter((float)i/((float)nl*2.));
  }
  avg /= (double)n;
  for (i=0; i<nl; i+=3) {
    get_float_line(fp,meta,i,amp);
    for (j=0; j<ns; j+=3) {
      stddev += (amp[j]-avg)*(amp[j]-avg);
    }
    asfPercentMeter((float)(i+nl)/((float)nl*2.));
  }
  asfPercentMeter(1);
  stddev = sqrt(stddev/(double)n);
  double min = avg - 2*stddev;
  double max = avg + 2*stddev;

  // open up PNG/JPEG output file
  FILE *ofp=NULL;
  struct jpeg_compress_struct cinfo;
  png_structp png_ptr;
  png_infop png_info_ptr;
  char *jpgname = NULL;
  char *pngname = NULL;
  if (png_flag) {
    asfPrintStatus("Generating debug image (PNG)...\n");
    pngname = appendExt(outname, ".png");
    initialize_png_file(pngname,meta,&ofp,&png_ptr,&png_info_ptr,TRUE);
  }
  else {
    asfPrintStatus("Generating debug image (JPEG)...\n");
    jpgname = appendExt(outname, ".jpg");
    initialize_jpeg_file(jpgname,meta,&ofp,&cinfo,TRUE);
  }

  // now read in the polar image, calculate RGB values, write to jpeg/png
  for (i=0; i<nl; ++i) {
    get_band_float_line(fp,meta,0,i,amp);
    get_band_float_line(fp,meta,1,i,phase);

    for (j=0; j<ns; ++j) {
      // scale to 2-sigma, to get brightness of the pixel
      unsigned char intensity;
          //=(unsigned char)(amp[j] * 128./(float)avg * 41./255.);
      if (amp[j]<=min)
        intensity=0;
      else if (amp[j]>=max)
        intensity=255;
      else
        intensity=(unsigned char)((amp[j]-min)/(max-min)*255.);

      // color of the pixel is determined by the phase
      unsigned char r=0,g=0,b=0;
      if (phase[j]<-PI) phase[j]+=TWOPI;     // ensure [-PI, PI)
      if (phase[j]>=PI) phase[j]-=TWOPI;
      int range = (int)((phase[j]+PI)/PIOVER3); // will be 0-6 (and rarely 6)
      switch (range) {
        case 0: r=1;           break;
        case 1: r=1; g=1;      break;
        case 2:      g=1;      break;
        case 3:      g=1; b=1; break;
        case 4:           b=1; break;
        case 5: r=1;      b=1; break;
        case 6:                break; // left black
        default:
          printf("phase: %f, range: %d\n", phase[j], range);
          assert(FALSE); break;
      }

      red[j] = r*intensity;
      grn[j] = g*intensity;
      blu[j] = b*intensity;
    }

    // write the line
    if (png_flag)
      write_rgb_png_byte2byte(ofp,red,grn,blu,png_ptr,png_info_ptr,ns);
    else
      write_rgb_jpeg_byte2byte(ofp,red,grn,blu,&cinfo,ns);

    // keep the user interested!
    asfPercentMeter((float)i/((float)nl));
  }
  asfPercentMeter(1.0);

  // clean up
  FCLOSE(fp);
  FREE(amp);
  FREE(phase);
  FREE(red);
  FREE(grn);
  FREE(blu);
  meta_free(meta);
  if (png_flag)
    finalize_png_file(ofp,png_ptr,png_info_ptr);
  else
    finalize_jpeg_file(ofp,&cinfo);
  FREE(jpgname);
  FREE(pngname);
  removeImgAndMeta(polar_name);
  FREE(polar_name);
}

void debugWritePatch_Line(int lineNo, complexFloat *line, char *basename,
                          int n_range, int n_az)
{
  FILE *fp;
  meta_parameters *meta;
  char outname[320],name[320];
  char *mode;

  strcpy(outname,g.out);
  strcat(strcat(outname,"_"),basename);
  strcat(strcpy(name,outname),".img");

  if (lineNo == 0) {
      meta = meta_read(g.in1);
      meta->general->line_count = n_range;
      meta->general->sample_count = n_az;
      meta->general->data_type = COMPLEX_REAL32;
      meta->general->image_data_type = COMPLEX_IMAGE;
      meta_write(meta, name);
  } else {
      meta = meta_read(name);
  }

  mode = lineNo == 0 ? "wb" : "ab";
  fp = fopenImage(name,mode);
  put_complexFloat_line(fp, meta, lineNo, line);
  FCLOSE(fp);

  if (lineNo == meta->general->line_count - 1)
    patchToRGBImage(outname, TRUE);

  meta_free(meta);
}

/*
  DebugWritePatch:
  Outputs the current patch trans array, and
  converts it to amplitude and phase.
*/
void debugWritePatch(const patch *p,char *basename)
{
  FILE *fp;
  char name[1024],outname[1024];
  meta_parameters *meta;
  int i;

  strcpy(outname,g.out);
  strcat(strcat(outname,"_"),basename);
  strcat(strcpy(name,outname),".img");


  meta = meta_read(g.in1);
  meta->general->line_count = p->n_range;
  meta->general->sample_count = p->n_az;
  meta->general->data_type = COMPLEX_REAL32;
  meta->general->image_data_type = COMPLEX_IMAGE;
  meta_write(meta, name);

  fp = fopenImage(name,"wb");

  for (i=0; i<p->n_range; ++i)
    put_complexFloat_line(fp, meta, i, p->trans+i*p->n_az);

  FCLOSE(fp);
  meta_free(meta);

  patchToRGBImage(outname, TRUE);
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

  update_status("Range compressing");
  if (!quietflag) printf("   RANGE COMPRESSING CHANNELS...\n");
  elapse(0);
  rciq(p,signalGetRec,r);
  if (!quietflag) elapse(1);
  if (s->debugFlag & AZ_RAW_T) debugWritePatch(p,"az_raw_t");

  update_status("Starting azimuth compression");
  if (!quietflag) printf("   TRANSFORMING LINES...\n");
  elapse(0);
  cfft1d(p->n_az,NULL,0);
  for (i=0; i<p->n_range; i++) cfft1d(p->n_az,&p->trans[i*p->n_az],-1);
  if (!quietflag) elapse(1);
  if (s->debugFlag & AZ_RAW_F) debugWritePatch(p,"az_raw_f");
  if (!(s->debugFlag & NO_RCM))
    {
      update_status("Range cell migration");
      if (!quietflag) printf("   START RANGE MIGRATION CORRECTION...\n");
      elapse(0);
      rmpatch(p,s);
      if (!quietflag) elapse(1);
      if (s->debugFlag & AZ_MIG_F) debugWritePatch(p,"az_mig_f");
    }
  update_status("Finishing azimuth compression");
  if (!quietflag) printf("   INVERSE TRANSFORMING LINES...\n");
  elapse(0);
  acpatch(p,s);
  if (!quietflag) elapse(1);

  /*    if (!quietflag) printf("  Range-Doppler done...\n");*/
}
/*
  writePatch:
  Outputs one full patch of data to the given file.
*/
void writePatch(const patch *p,const satellite *s,meta_parameters *meta,
    const file *f,int patchNo)
{
  int outLine;       /* Counter for line base output */
  FILE *fp_amp,*fp_cpx;  /* File pointers for the amplitude and  complex outputs*/
  FILE *fp_pwr,*fp_sig;  /* File pointers for the power and Sigma_0 outputs */
  FILE *fp_gam,*fp_bet;  /* File pointers for the Gamma_0 and Beta_0 outputs */
  complexFloat *outputBuf; /* Buffer for one line of patch = n_range    */
  complexFloat *mlBuf;   /* Buffer for multilooking the amplitude image */
  float *amps;           /* Output Amplitude  = n_az/nlooks X n_range */
  float *pwrs;       /* Output power */
  char *openMode="ab";   /* Normally append output.*/
  int mlCount=0;     /* Counter for setting up the multilook buffer */
  int writeNoiseTable=0; /* Flag to determine whether to write the noise table and
                antenna pattern */
  int off_slc = f->n_az_valid * (patchNo-1);
  int off_ml = f->n_az_valid * (patchNo-1) / (f->nlooks);

  if (patchNo==1)
    openMode="wb";   /* for first patch, truncate output. */

  if ((patchNo==1) && (s->vecLen!=0))
    writeNoiseTable=1;  /* If first patch AND antenna pattern correction,
               write the noise table */

  update_status("Range-doppler done");
  if (!quietflag) printf("   WRITING PATCH OUT...\n");
  elapse(0);

  /* Allocate buffer space  ------------------------*/
  amps = (float *) MALLOC(p->n_range*sizeof(float));
  pwrs = (float *) MALLOC(p->n_range*sizeof(float));

  outputBuf = (complexFloat *)MALLOC(p->n_range*sizeof(complexFloat));
  mlBuf = (complexFloat *)MALLOC(p->n_range*f->nlooks*sizeof(complexFloat));

  meta->general->center_latitude = NAN;
  meta->general->center_longitude = NAN;

  /* Fill in metadata */
  meta_get_latLon(meta, meta->general->line_count/2,
          meta->general->sample_count/2, 0.0,
          &(meta->general->center_latitude),
          &(meta->general->center_longitude));

  meta_parameters *metaCpx=meta_copy(meta);
  metaCpx->general->data_type = COMPLEX_REAL32;
  metaCpx->general->image_data_type = COMPLEX_IMAGE;
  metaCpx->general->radiometry = r_AMP;
  metaCpx->general->line_count = f->n_az_valid * patchNo;
  metaCpx->general->sample_count = p->n_range;
  meta_get_latLon(metaCpx, metaCpx->general->line_count/2,
          metaCpx->general->sample_count/2, 0.0,
          &(metaCpx->general->center_latitude),
          &(metaCpx->general->center_longitude));
  metaCpx->general->start_line = f->firstLineToProcess + s->dop_precomp + 1;
  metaCpx->general->start_sample = f->skipFile + 1;
  metaCpx->general->x_pixel_size = f->rngpix;
  metaCpx->general->y_pixel_size = f->azpix;
  meta_write(metaCpx, f->out_cpx);
  fp_cpx=fopenImage(f->out_cpx,openMode);

  meta_parameters *metaAmp=meta_copy(metaCpx);
  metaAmp->general->data_type = REAL32;
  metaAmp->general->image_data_type = AMPLITUDE_IMAGE;
  metaAmp->general->radiometry = r_AMP;
  metaAmp->general->line_count = f->n_az_valid / f->nlooks * patchNo;
  metaAmp->general->y_pixel_size *= f->nlooks;
  metaAmp->sar->azimuth_time_per_pixel *= f->nlooks;
  meta_write(metaAmp, f->out_amp);
  fp_amp=fopenImage(f->out_amp,openMode);

  meta_parameters *metaPower=0, *metaSigma=0, *metaGamma=0, *metaBeta=0;
  if (s->imageType.power) {
    metaPower=meta_copy(metaAmp);
    metaPower->general->image_data_type = AMPLITUDE_IMAGE;
    metaPower->general->radiometry = r_POWER;
    meta_write(metaPower, f->out_pwr);
    fp_pwr=fopenImage(f->out_pwr,openMode);
  }
  if (s->imageType.sigma) {
    metaSigma=meta_copy(metaAmp);
    metaSigma->general->image_data_type = AMPLITUDE_IMAGE;
    metaSigma->general->radiometry = r_SIGMA;
    meta_write(metaSigma, f->out_sig);
    fp_sig=fopenImage(f->out_sig,openMode);
  }
  if (s->imageType.gamma) {
    metaGamma=meta_copy(metaAmp);
    metaGamma->general->image_data_type = AMPLITUDE_IMAGE;
    metaGamma->general->image_data_type = r_GAMMA;
    meta_write(metaGamma, f->out_gam);
    fp_gam=fopenImage(f->out_gam,openMode);
  }
  if (s->imageType.beta) {
    metaBeta=meta_copy(metaAmp);
    metaBeta->general->image_data_type = AMPLITUDE_IMAGE;
    metaBeta->general->radiometry = r_BETA;
    meta_write(metaBeta, f->out_bet);
    fp_bet=fopenImage(f->out_bet,openMode);
  }

  /* This gets messy */
  for (outLine = 0; outLine < f->n_az_valid; outLine++)
  {
      int j; /* loop counter */
      int base = f->firstOutputLine+outLine; /* loop counter for transposed data */

      if(writeNoiseTable==1) {
          if (!quietflag) printf("   Writing .noise and .ant files\n");
      }

      /* Print statement for the antenna pattern correction option */
      if(s->vecLen==0) {
          if(!quietflag && (outLine % 1024 == 0)) {
              printf("   ...Writing Line %i\n",outLine);
          }
      }
      if(s->vecLen!=0) {
          if(!quietflag && (outLine % 1024 == 0)) {
              printf("   ...Writing Line %i and applying Antenna Pattern Correction\n",
                     outLine);
          }
      }

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
              writeTable(meta,s,p->n_range);
              writeNoiseTable=0;
          }

          antptn_correct(meta,outputBuf,base,p->n_range,s);
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

          put_float_line(fp_amp, metaAmp, outLine/f->nlooks+off_ml, amps);

          if (s->imageType.power) {
              put_float_line(fp_pwr, metaPower, outLine/f->nlooks+off_ml, pwrs);
          }
          if (s->imageType.sigma)
          {
              calculateRCS(SIGMA_0, meta, pwrs, amps,
                           base-mlCount/2, p->n_range,s);
              put_float_line(fp_sig, metaSigma, outLine/f->nlooks+off_ml, amps);
          }
          if (s->imageType.gamma)
          {
              calculateRCS(GAMMA_0, meta, pwrs, amps,
                           base-mlCount/2, p->n_range,s);
              put_float_line(fp_gam, metaGamma, outLine/f->nlooks+off_ml, amps);
          }
          if (s->imageType.beta)
          {
              calculateRCS(BETA_0, meta, pwrs, amps,
                           base-mlCount/2, p->n_range,s);
              put_float_line(fp_bet, metaBeta, outLine/f->nlooks+off_ml, amps);
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

  if (!quietflag) printf("\n");
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
  FREE((void *)mlBuf);
  meta_free(metaAmp);
  meta_free(metaCpx);
  if (metaPower) meta_free(metaPower);
  if (metaSigma) meta_free(metaSigma);
  if (metaGamma) meta_free(metaGamma);
  if (metaBeta)  meta_free(metaBeta);
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
