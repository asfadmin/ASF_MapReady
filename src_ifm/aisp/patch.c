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
#include "aisp_defs.h"


/* Functions in calibration.c (date: Jan 2003) */
void calculateRCS(int projectionFlag, meta_parameters *meta, float *DNsquared,
                  float *radarCrossSection, int curLine,int numSamples, const satellite *s);
void intensity(int n_range,float *pwrs,float *amps);

extern struct AISP_PARAMS g;/*AISP Globals, defined in aisp_params.h*/

/*
setPatchLoc: 
Update slantToFirst / doppler parameters for number of skipped samples.
Update resampling parameters for the number of skipped lines and samples.
Store patch location to patch variables.
leftFile is the starting sample, relative to the beginning of the file line.
leftSamp is the starting sample, relative to the beginning of the good data.
*/
void setPatchLoc(patch *p,satellite *s,meta_parameters *meta,int leftFile,int leftSamp,int top)
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
	char name[255],outname[255];
	meta_parameters *meta = raw_init();

	strcpy(outname,g.out);
	strcat(strcat(outname,"_"),basename);
	printf("   Outputting Debugging image '%s'...\n",outname);
	strcat(strcpy(name,outname),".cpx");
	fp = fopenImage(name,"wb");
	FWRITE(p->trans,sizeof(FCMPLX),p->n_az*p->n_range,fp);
	FCLOSE(fp);
	meta->general->line_count = p->n_range;
	meta->general->sample_count   = p->n_az;
	meta->general->data_type    = REAL32;
	meta_write(meta, outname);
	meta_free(meta);
	printf("   Converting Debugging image '%s' to polar form...\n",outname);
	sprintf(name,"c2p %s %s\n",outname,outname);
	system(name);
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
void writePatch(const patch *p,const satellite *s,meta_parameters *meta,const file *f,int patchNo)
{
	int outLine;		/* Counter for line base output */
	FILE *fp_amp,*fp_cpx;   /* File pointers for the amplitude and  complex outputs*/
	FILE *fp_pwr,*fp_sig;   /* File pointers for the power and Sigma_0 outputs */
        FILE *fp_gam,*fp_bet;   /* File pointers for the Gamma_0 and Beta_0 outputs */
	FCMPLX *outputBuf;  	/* Buffer for one line of patch = n_range    */
	FCMPLX *mlBuf;	    	/* Buffer for multilooking the amplitude image */
	float	*amps;      	/* Output Amplitude  = n_az/nlooks X n_range */
	float	*pwrs;  	/* Output power */
	char *openMode="ab";	/* Normally append output.*/
	int mlCount=0;		/* Counter for setting up the multilook buffer */
	int writeNoiseTable=0;	/* Flag to determine whether to write the noise table and antenna pattern */
	if (patchNo==1)
		openMode="wb";		/* for first patch, truncate output. */

	if ((patchNo==1) && (s->vecLen!=0))
		writeNoiseTable=1;	/* If first patch AND antenna pattern correction, write the noise table */


	printf("   WRITING PATCH OUT...\n");
	elapse(0);
	
/* Allocate buffer space  ------------------------*/
	amps = (float *) MALLOC(p->n_range*sizeof(float));
	pwrs = (float *) MALLOC(p->n_range*sizeof(float));
	
	outputBuf = (FCMPLX *)MALLOC(p->n_range*sizeof(FCMPLX));
	mlBuf = (FCMPLX *)MALLOC(p->n_range*f->nlooks*sizeof(FCMPLX));

/* transpose and write out the complex image data and amplitude image*/

	fp_cpx=fopenImage(f->out_cpx,openMode);
	FSEEK64(fp_cpx,f->n_az_valid*(patchNo-1),SEEK_SET);


	fp_amp=fopenImage(f->out_amp,openMode);
	FSEEK64(fp_amp,f->n_az_valid*(patchNo-1)/(f->nlooks),SEEK_SET);

/* Now, if there were other command-line optional files to write, open the files */

	if (s->imageType.power) 
	{
	  fp_pwr=fopenImage(f->out_pwr,openMode);
	  FSEEK64(fp_pwr,f->n_az_valid*(patchNo-1)/(f->nlooks),SEEK_SET);
	}

	if (s->imageType.sigma) 
	{
	  fp_sig=fopenImage(f->out_sig,openMode);
	  FSEEK64(fp_sig,f->n_az_valid*(patchNo-1)/(f->nlooks),SEEK_SET);
	}

	if (s->imageType.gamma) 
	{
	  fp_gam=fopenImage(f->out_gam,openMode);
	  FSEEK64(fp_gam,f->n_az_valid*(patchNo-1)/(f->nlooks),SEEK_SET);
	}
	
	if (s->imageType.beta) 
	{
	  fp_bet=fopenImage(f->out_bet,openMode);
	  FSEEK64(fp_bet,f->n_az_valid*(patchNo-1)/(f->nlooks),SEEK_SET);
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
                        if(!quietflag && (outLine % 1024 == 0)) printf("   ...Writing Line %i\n",outLine);
                if(s->vecLen!=0)
                        if(!quietflag && (outLine % 1024 == 0)) printf("   ...Writing Line %i and applying Antenna Pattern Correction\n",outLine);
		
		/* Fill up the buffers */
		for (j=0; j<p->n_range; j++,base+=p->n_az)
		{	
			outputBuf[j] = p->trans[base];

/* For speed, if we aren't correcting the antenna pattern, write the multi-look buffer now */

			if(s->vecLen==0)
			{
				mlBuf[j+mlCount*p->n_range].r = outputBuf[j].r;
				mlBuf[j+mlCount*p->n_range].i = outputBuf[j].i;
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
				mlBuf[j+mlCount*p->n_range].r = outputBuf[j].r;
                                mlBuf[j+mlCount*p->n_range].i = outputBuf[j].i;
			}
		}

		FWRITE(outputBuf,sizeof(FCMPLX),p->n_range,fp_cpx);
		
		mlCount+=1;
		/* Multilook takes f->nlooks lines of data and averages them together, and writes one line on return */
		if(mlCount==f->nlooks)
                {
/* multilook on the power image, then use the power image to generate an amplitude, and all other detected images from the command line. Be careful because I recycle amps after writing out the line of amplitude data, and it gets used as the sigma_0, beta_0, and gamma_0 line. It my be confusing, but it uses less memory.*/
			multilook(mlBuf,p->n_range,f->nlooks,pwrs);
			intensity(p->n_range,pwrs,amps);
			FWRITE(amps,sizeof(float),p->n_range,fp_amp);
			if (s->imageType.power)
			  FWRITE(pwrs,sizeof(float),p->n_range,fp_pwr);
			if (s->imageType.sigma)
			{
			  calculateRCS(SIGMA_0,meta,pwrs,amps,base-mlCount/2,p->n_range,s);
			  FWRITE(amps,sizeof(float),p->n_range,fp_sig);
			}

			if (s->imageType.gamma)
			{
			  calculateRCS(GAMMA_0,meta,pwrs,amps,base-mlCount/2,p->n_range,s);
			  FWRITE(amps,sizeof(float),p->n_range,fp_gam);
			}

			if (s->imageType.beta)
			{
			  calculateRCS(BETA_0,meta,pwrs,amps,base-mlCount/2,p->n_range,s);
			  FWRITE(amps,sizeof(float),p->n_range,fp_bet);
			}
			
                        mlCount=0;
                }
	}
	FCLOSE(fp_cpx);
	save_meta(meta,f->out_cpx,f->n_az_valid*patchNo,p->n_range,
		 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
		 f->rngpix,f->azpix,1);
	FCLOSE(fp_amp);
	save_meta(meta,f->out_amp,(f->n_az_valid/f->nlooks)*patchNo,p->n_range,
                 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 f->rngpix,f->azpix,f->nlooks);
	if (s->imageType.power) 
	{
	  FCLOSE(fp_pwr);
	  save_meta(meta,f->out_pwr,(f->n_az_valid/f->nlooks)*patchNo,p->n_range,
                 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 f->rngpix,f->azpix,f->nlooks);
	}

	if (s->imageType.sigma) 
	{
	  FCLOSE(fp_sig);
	  save_meta(meta,f->out_sig,(f->n_az_valid/f->nlooks)*patchNo,p->n_range,
                 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 f->rngpix,f->azpix,f->nlooks);
	}

	if (s->imageType.gamma) 
	{
	  FCLOSE(fp_gam);
	  save_meta(meta,f->out_gam,(f->n_az_valid/f->nlooks)*patchNo,p->n_range,
                 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 f->rngpix,f->azpix,f->nlooks);
	}if (s->imageType.beta) 
	{
	  FCLOSE(fp_bet);
	  save_meta(meta,f->out_bet,(f->n_az_valid/f->nlooks)*patchNo,p->n_range,
                 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 f->rngpix,f->azpix,f->nlooks);
	}
	
	

	printf("\n");
	if (logflag) printLog("\n");
	if (!quietflag) {
	  printf("   AMPLITUDE IMAGE FINISHED: Wrote %i lines and %i samples\n",f->n_az_valid/f->nlooks,p->n_range);	
	  printf("   PATCH FINISHED: Wrote %i lines of %i samples (float)\n\n",f->n_az_valid,p->n_range);
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





