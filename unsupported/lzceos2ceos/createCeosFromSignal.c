/****************************************************************
FUNCTION NAME:

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "sarout.h"
#include "aisp_params.h"

void getSignalFormat(char *,int *,int *,float *,float *,char *,float *);
void createRSR(char *file, struct AISP_PARAMS *asp, struct rng_spec_rec *rsr);
double getSNR(struct rng_spec_rec *);
void createCeosFromSignal(meta_parameters *meta,char *inImgFile, ceosLeader *leader);

void createCeosFromSignal(meta_parameters *meta,char *inImgFile, ceosLeader *leader)
 {
   struct AISP_PARAMS a;
   int nbytes; 		/* Number of bytes per line */
   int nhead;		/* Number of header bytes per line */
   float xmi, xmq; 	/* I,Q bias values */
   char  iqflip;	/* IQ flip flag    */
   char  pfile[256];	/* AISP parameter file name */
   float dwp;

   /* Read .in file 
    --------------*/
   strcat(strcpy(pfile,inImgFile),".in");
   read_params(pfile, &a);

   /* Read .fmt file 
    ---------------*/
   printf("Reading signal format\n");
   fflush(NULL);
   getSignalFormat(inImgFile, &nbytes, &nhead, &xmi, &xmq, &iqflip,&dwp);

   /* Store Data Window Position 
    ---------------------------*/
   leader->dssr.rng_gate = 1000000.0 * dwp * meta->geo->rngPixTime;
   leader->facdr.datawin = 1000000.0 * dwp * meta->geo->rngPixTime;

   /* Create a range spectra record 
    ------------------------------*/
   createRSR(inImgFile,&a,&(leader->rsr));

   /* Calculate the SNR 
    ------------------*/
   leader->dqsr.snr = leader->facdr.sigtonoi = getSNR(&(leader->rsr));
 }

double getSNR(struct rng_spec_rec *rsr)
 {
   int   i;
   int   minSpot;
   double searchVal;
   double sum = 0.0;
   double aveSignal, aveNoise; 

   /* Estimate the signal with noise */
   for (i = 0; i< 32; i++) sum += rsr->data_values_spec[i]; 
   for (i = 96; i< 128; i++) sum += rsr->data_values_spec[i]; 
   aveSignal = fabs(sum / 64.0);

   /* Estimate the noise floor */
   searchVal = rsr->min_power;
   for (i = 0; i< 128; i++) if (rsr->data_values_spec[i]==searchVal) minSpot=i;
   sum = 0.0;
   for (i=minSpot-2; i<=minSpot+2; i++) sum += rsr->data_values_spec[i];
   aveNoise = fabs(sum / 5.0);

   return((aveSignal-aveNoise)/aveNoise);
 }

void createRSR(char *file, struct AISP_PARAMS *asp, struct rng_spec_rec *rsr)
 {
    char cmd[256];
    FILE *fp;
    int   samp;
    int   pixPerBin;
    float value[2048]; 
    int   i, cnt;
    double min = GILLION, max = -1.0*GILLION;
    double sum_pow;

    rsr->seq_num      = 1;              /* range spectra sequence no. */
    rsr->sar_chan     = 1;              /* SAR channel */
    rsr->n_dset       = 1;              /* number of data sets */
    rsr->dset_size    = 4032;           /* data set size */
    rsr->req_recs     = 1;              /* number of records required */
    rsr->table_no     = 1;              /* table sequence number */
    rsr->pixel_offset = asp->ifirst;    /* offset from first pixel */
    rsr->first_freq   = 0;              /* center freq of first spectra bin */
    rsr->last_freq    = asp->fs;        /* center freq of last spectra bin */
    rsr->n_bins       = 128;            /* number of freq bins in table */

    sprintf(cmd,"spectra %s.raw",file); system(cmd);
    sprintf(cmd,"rm spectra_az"); system(cmd);
    fp = FOPEN("spectra_range","r");
    i = 0; while(fscanf(fp,"%i %f",&samp,&value[i])==2) i++;
    FCLOSE(fp);
    sprintf(cmd,"rm spectra_range"); system(cmd);

    rsr->n_pixels = i;
    rsr->n_lines  = i;
    cnt = i;
    pixPerBin = cnt / rsr->n_bins;

    /* Average sum vector down to 128 bins, track Min & Max */
    for (samp=0; samp<rsr->n_bins; samp++) {
       sum_pow = 0.0;
       for (i=0; i<pixPerBin; i++) sum_pow += value[samp*pixPerBin+i];
       sum_pow /= (pixPerBin*cnt);
       rsr->data_values_spec[samp] = sum_pow;
       min = DMIN(min,sum_pow);
       max = DMAX(max,sum_pow);
    }

    for (samp=0; samp<rsr->n_bins; samp++) { rsr->data_values_spec[samp]/=max; }

    rsr->min_power = min/max;           /* minimum spectral power */
    rsr->max_power = 1.0;               /* maximum spectral power */
    strcpy(rsr->spare_rsr_1,"                ");
    strcpy(rsr->spare_rsr_2,"                ");
    rsr->spare_rsr_3[0]='\0';           /* spare */

 }

/***************************************************************************
getSignalFormat:
        Reads the number of good bytes, number of bytes per line,
I and Q DC offset, and i/q flip parameters from
".fmt" input file.
****************************************************************************/
#define FILL(A,B,C)     if (fgets((A),(B),(C))!=NULL)

void getSignalFormat(char *baseName,int *nbytes,int *nhead,
		     float *xmi,float *xmq,char *iqFlip,float *dwp)
{
        char name[255];
        char buf[80];
        FILE *fp;
        int   line;
        float AGC;

        strcpy(name,baseName);
        strtok(name,".");
        strcat(name,".fmt");

	fp=FOPEN(name,"r");

	/****replaced by FOPEN*************
        if (NULL==(fp=open(name,"r")))
         {
 	  printf("Couldn't open raw data formatting file named '%s'!\n",name);
	  exit(1);
         }*********************************/
        FILL(buf,80,fp) sscanf(buf,"%i%i", nbytes, nhead);
        FILL(buf,80,fp) sscanf(buf,"%f%f", xmi, xmq);
        FILL(buf,80,fp) sscanf(buf,"%c", iqFlip);
        fgets(buf,80,fp);/* Skip comment line */

        if (NULL==fgets(buf,80,fp)) return; /*If no extra data, bail!*/

        /*Read first line's info.*/
        sscanf(buf,"%d%g%g",&line,dwp,&AGC);

}

