/*****************************************************************************
NAME:Patch.c   ---- PAISP VERSION!!!!!

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
#include "paisp_defs.h"
#include <mpi.h>

#define SIGMA_0 2
#define GAMMA_0 3
#define BETA_0 4

/* Prototypes (intensity, and calculateRCS are from calibrate.c [may 2002])*/
void intensity(int n_range,float *pwrs,float *amps);
void calculateRCS(int projectionFlag, meta_parameters *meta,
                  float *DNsquared, float *radarCrossSection,
                  int curLine,int numSamples, const satellite *s);


extern int my_pe;


/*
setPatchLoc:
Update slantToFirst / doppler parameters for number of skipped samples.
Update resampling parameters for the number of skipped lines and samples.
Store patch location to patch variables.
leftFile is the starting sample, relative to the beginning of the file line.
leftSamp is the starting sample, relative to the beginning of the good data.
*/

void setPatchLoc(patch *p,satellite *s,meta_parameters *meta,int leftFile,int leftSamp,int top,
	int firstLine)
{
/*Update slant range.*/
	p->slantToFirst = s->orig_slantToFirst + leftFile * p->slantPer;
	
/*Update doppler.*/
	p->fd=(s->orig_fd+s->orig_fdd*leftFile+s->orig_fddd*leftFile*leftFile)*s->prf;
	p->fdd=(s->orig_fdd+2*s->orig_fddd*leftFile)*s->prf;
	p->fddd=s->orig_fddd*s->prf;
	
/*Figure out resampling coefficents at given line.*/
        p->xResampScale = s->sloper+(top-firstLine)*s->dsloper;
        p->xResampOffset = s->interr+(top-firstLine)*s->dinterr;
        p->yResampScale = s->slopea+(top-firstLine)*s->dslopea;
        p->yResampOffset = s->intera+(top-firstLine)*s->dintera;

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


/*
DebugWritePatch:
Outputs the current patch trans array, and
converts it to amplitude and phase.
*/
void debugWritePatch(const patch *p,char *basename)
{
	FILE *fp;
	char name[255];
	printf("Outputting Debugging image '%s'...\n",basename);
	strcat(strcpy(name,basename),".cpx");
	fp = fopenImage(name,"wb");
	FWRITE(p->trans,sizeof(FCMPLX),p->n_az*p->n_range,fp);
	FCLOSE(fp);
	sprintf(name,"makeddr %s %i %i float\n",basename,p->n_range,p->n_az);
	system(name);
	printf("Converting Debugging image '%s' to polar form...\n",basename);
	sprintf(name,"c2p %s %s\n",basename,basename);
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
void processPatch(patch *p,const getRec *signalGetRec,const rangeRef *r,const satellite *s)
{
	int i;
/*	char msg[256];*/

	if (IM_DSP) printf("\n   RANGE COMPRESSION...\n");
	elapse(0,NULL); 
	rciq(p,signalGetRec,r);
/*	sprintf(msg,"PE %i: RC ",my_pe);
	elapse(1,msg);*/
	if (s->debugFlag & 8 && IM_DSP) debugWritePatch(p,"rangecomp");

	if (IM_DSP) printf("   TRANSFORMING LINES...\n");
	elapse(0,NULL);
	cfft1d(p->n_az,NULL,0);
	for (i=0; i<p->n_range; i++) cfft1d(p->n_az,&p->trans[i*p->n_az],-1);
/*	sprintf(msg,"PE %i: TR ",my_pe);
	elapse(1,msg);*/
	if (s->debugFlag & 8 && IM_DSP) debugWritePatch(p,"rangefft");

	if (IM_DSP) printf("   RANGE MIGRATION CORRECTION...\n");
	elapse(0,NULL);
	rmpatch(p,s);
/*	sprintf(msg,"PE %i: RM ",my_pe);
	elapse(1,msg);*/
	if (s->debugFlag & 4 && IM_DSP) debugWritePatch(p,"migfft");

	if (IM_DSP) printf("   AZIMUTH COMPRESSION...\n");
	elapse(0,NULL);
	acpatch(p,s);
/*	sprintf(msg,"PE %i: AC ",my_pe);
	elapse(1,msg);*/
	
/*	if (IM_DSP) printf("  Range-Doppler done...\n");*/
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
	FCMPLX *mlBuf;		/* Buffer for multilooking the amplitude image */
	float *amps;		/* Output Amplitude  = n_az/nlooks X n_range */
	float *pwrs;		/* power image */
	char *openMode="r+b";	/* Normally append output.*/
/*	char msg[256];*/		/* Message display char buffer */
        int mlCount=0;          /* Counter for setting up the multilook buffer */
        int writeNoiseTable=0;  /* Flag to determine whether to write the noise table */

	int final_line_cpx;	/* MPI Reduction variables */
	int final_line_amp;
	int final_line_pwr;
	int final_line_sig;
	int final_line_bet;
	int final_line_gam;
	int max_line_cpx;
	int max_line_amp;
	int max_line_pwr;
	int max_line_sig;
	int max_line_bet;
	int max_line_gam;
	long long offset;	/* File offset variables, must be long long for > 2GB access */
	long long amp_offset;
	long long pwr_offset;
	long long sig_offset;
	long long bet_offset;
	long long gam_offset;

	elapse(0,NULL);

        if ((patchNo==1) && (s->vecLen!=0))
                writeNoiseTable=1;      /* If first patch AND antenna pattern correction, write the noise table */

	if (IM_DSP) printf("   WRITING PATCH OUT...\n");

/* Allocate buffer space  ------------------------*/
	amps = (float *) MALLOC (p->n_range*sizeof(float));
	pwrs = (float *) MALLOC (p->n_range*sizeof(float));
	outputBuf = (FCMPLX *) MALLOC (p->n_range*sizeof(FCMPLX));
	mlBuf = (FCMPLX *)MALLOC(p->n_range*f->nlooks*sizeof(FCMPLX));

/* transpose and write out the complex image data.*/
	fp_cpx=fopenImage(f->out_cpx,openMode);
	fp_amp=fopenImage(f->out_amp,openMode);
	if (s->imageType.power) fp_pwr=fopenImage(f->out_pwr,openMode);
	if (s->imageType.sigma) fp_sig=fopenImage(f->out_sig,openMode);
	if (s->imageType.gamma) fp_gam=fopenImage(f->out_gam,openMode);
	if (s->imageType.beta)  fp_bet=fopenImage(f->out_bet,openMode);

	offset = (long long)f->n_az_valid * (long long)(patchNo-1)*p->n_range*sizeof(FCMPLX);
	gam_offset=bet_offset=sig_offset=pwr_offset=amp_offset = (long long)f->n_az_valid/f->nlooks * (long long)(patchNo-1)*p->n_range*sizeof(float);
	

	FSEEK64(fp_cpx,offset,SEEK_SET);
	FSEEK64(fp_amp,amp_offset,SEEK_SET);
	
/* Now, if there were other command-line optional files to write, open the files */
	if (s->imageType.power) FSEEK64(fp_pwr,pwr_offset,SEEK_SET);
	if (s->imageType.sigma) FSEEK64(fp_sig,sig_offset,SEEK_SET);
	if (s->imageType.gamma) FSEEK64(fp_gam,gam_offset,SEEK_SET);
	if (s->imageType.beta)  FSEEK64(fp_bet,bet_offset,SEEK_SET);



	/* This gets messy */
	for (outLine=0; outLine<f->n_az_valid; outLine++)
	{
		int j;
		int base = f->firstOutputLine+outLine;
		
		if(writeNoiseTable==1)
			if(IM_DSP)
	                        printf("   Writing .noise and .ant files\n");

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
                        antptn_correct(meta, outputBuf,f->firstOutputLine+outLine,p->n_range,s);
                        
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
	FCLOSE(fp_amp);
	if (s->imageType.power) FCLOSE(fp_pwr);
	if (s->imageType.sigma) FCLOSE(fp_sig);
	if (s->imageType.gamma) FCLOSE(fp_gam);
	if (s->imageType.beta)  FCLOSE(fp_bet);

	final_line_cpx = f->n_az_valid*patchNo;
        MPI_Reduce(&final_line_cpx, &max_line_cpx, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	final_line_amp = f->n_az_valid/f->nlooks*patchNo;
	MPI_Reduce(&final_line_amp, &max_line_amp, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	if (s->imageType.power) {
	  final_line_pwr = f->n_az_valid/f->nlooks*patchNo;
	  MPI_Reduce(&final_line_pwr, &max_line_pwr, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	}
	if (s->imageType.sigma) {
	  final_line_sig = f->n_az_valid/f->nlooks*patchNo;
	  MPI_Reduce(&final_line_sig, &max_line_sig, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	}
	if (s->imageType.gamma) {
	  final_line_gam = f->n_az_valid/f->nlooks*patchNo;
	  MPI_Reduce(&final_line_gam, &max_line_gam, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	}
	if (s->imageType.beta) {
	  final_line_bet = f->n_az_valid/f->nlooks*patchNo;
	  MPI_Reduce(&final_line_bet, &max_line_bet, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	}

	/*Update DDR, so the user can view the image as it's created.*/ 
        if (IM_OUT)
	{
		save_ddr(f->out_cpx,max_line_cpx,p->n_range,
               	 	 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 	 f->rngpix,f->azpix,1);
        	save_ddr(f->out_amp,max_line_amp,p->n_range,
                 	 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 	 f->rngpix,f->azpix,f->nlooks);
		if (s->imageType.power) save_ddr(f->out_pwr,max_line_pwr,p->n_range,
                 	 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 	 f->rngpix,f->azpix,f->nlooks);
		if (s->imageType.sigma) save_ddr(f->out_sig,max_line_sig, p->n_range,
                 	 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 	 f->rngpix,f->azpix,f->nlooks);
		if (s->imageType.gamma) save_ddr(f->out_gam,max_line_gam, p->n_range,
                 	 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                 	 f->rngpix,f->azpix,f->nlooks);
		if (s->imageType.beta)  save_ddr(f->out_bet,max_line_bet, p->n_range,
                 	 f->firstLineToProcess+s->dop_precomp+1,f->skipFile+1,
                   	 f->rngpix,f->azpix,f->nlooks);
	}

	if (IM_DSP && !quietflag)
	{
		printf("\n   AMPLITUDE IMAGE FINISHED: Wrote %i lines and %i samples\n",f->n_az_valid/f->nlooks,p->n_range);
        	printf("   PATCH FINISHED: Wrote %i lines of %i samples (float)\n",f->n_az_valid,p->n_range);
        }

	FREE((void *)amps);
	FREE((void *)pwrs);
        FREE((void *)outputBuf);
	FREE((void *)mlBuf);
/*	sprintf(msg,"PE %i : OUT ",my_pe);
        elapse(1,msg);*/

}


/*destroyPatch:
De-allocates a patch of data.
*/
void destroyPatch(patch *p)
{
	FREE(p->trans);
	FREE(p);
}

