/*****************************************************************************
NAME:calibration.c

SYNOPSIS:
	Applies calibration to processed image data.

DESCRIPTION:
	Combines subroutines from aisp_setup.c with new algorithms, all to 
	calibrate the output of the aisp

EXTERNAL ASSOCIATES:
	Called mainly from patch.c

FILE REFERENCES:

PROGRAM HISTORY:

HARDWARE/SOFTWARE LIMITATIONS:

BUGS:

*****************************************************************************/
#include "asf.h"
#include "ceos.h"
#include "paisp_defs.h"
#include "asf_meta.h"
#include "odl.h"
#include <math.h>

extern int my_pe;

#define RANGEREF 500000   /* Arbitrarily set by Mark Ayers in aisp_setup.c */
#define PROC_GAIN 64
#define SIGMA_0 2
#define GAMMA_0 3
#define BETA_0 4

/*-------------------------------------------------------------------------*/
/*    The following is the list of all parameters needed to run aisp.c     */
/*              These are originally declared in aisp_setup.c              */
/*-------------------------------------------------------------------------*/
extern struct AISP_PARAMS g;/*AISP Globals, defined in aisp_params.h*/

/*-------------------------------------------------------------------------
FUNCTION NAME: multilook - performs amplitude multilook of t1 array

SYNTAX: multilook(FCMPLX *, int, int, int, float *)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    patch	FCMPLX *	Complex Image Patch (n_range X n_looks)
    n_range	int		lines in patch
    nlooks	int		number of looks to perform
    amps	float *		return multiooked amplitude patch

DESCRIPTION:
    Given a complex input patch of size n_range * n_looks, returns a 
    multilooked amplitude lines using nlooks in the sample direction
    (output size is n_range X nlooks). 

RETURN VALUE: none

SPECIAL CONSIDERATIONS:
 assumes range resampling was done at time of range migration

PROGRAM HISTORY:
 Converted from H. Zebker's multilook.f - T. Logan Fall '96
 Extensively modified to allow simultaneous writing of AMP and CPX images -
 Mark Ayers 8/00
****************************************************************/
#include "asf.h"
#include "paisp_defs.h"

void multilook(FCMPLX *patch,int n_range,int nlooks,float *pwrs)
{
  float   temp_pwr;
  int     samp, line;	

  for (samp=0; samp<n_range; samp++)
  {
    temp_pwr=0;
    for (line=0; line<nlooks; line++)
    {
      temp_pwr+=patch[samp+n_range*line].r*patch[samp+n_range*line].r+patch[samp+n_range*line].i*patch[samp+n_range*line].i;

    }
    pwrs[samp] = temp_pwr/nlooks;
    
  }
}

void intensity(int n_range,float *pwrs,float *amps)
{
  int     samp;	

  for (samp=0; samp<n_range; samp++)
  {
    amps[samp] = sqrt(pwrs[samp]);
  } 
}
/*-------------------------------------------------------------------------*/
/* The five functions (noiseFromAntennaPattern, sigma_naught, gamma_naught,
   beta_naught, and calculateGain are used internally by the subroutines
   calculateRCS, writeTable, and antptn_correct                            */
/*-------------------------------------------------------------------------*/

/*_________________________________________________________________________
  calibrationGain calculates the correction to apply to the data. Note that
  it is the correction to the digital number squared, so you must take the
  square root. gainFromFile is two-way correction gain from the input file. 
  _________________________________________________________________________*/ 

double calibrationGain(double gainFromFile, double slantRange, double rangeRef,double incidAngle)
{
	return pow(slantRange/rangeRef,3)*sin(incidAngle)*gainFromFile;
}



double interpolate(double x, double x_1, double x_2, double y_1, double y_2)
{
	return (y_2-y_1)*(x-x_1)/(x_2-x_1)+y_1;
}

/* Three different definitions of RCS, each a diffent projected area */

double sigma_naught(double noise,double noise_factor,double linear_conversion_factor, double DNsquared)
{
     return 10*log10(linear_conversion_factor*(DNsquared - noise_factor*noise));
}

double gamma_naught(double noise,double noise_factor,double linear_conversion_factor, double DNsquared, double incidAngle)
{
     return 10*log10((linear_conversion_factor*(DNsquared - noise_factor*noise))/cos(incidAngle));
}

double beta_naught(double noise,double noise_factor,double linear_conversion_factor, double DNsquared, double incidAngle)
{
     return 10*log10((linear_conversion_factor*(DNsquared - noise_factor*noise))/sin(incidAngle));
}

double lookUpGain(meta_parameters *meta, int line, int sample, const satellite *s)
{
 
    static int ind;       /* The WHILE loop index for finding the right table entry */
    double lookAngle;     /* Calculated look angle for each pixel */  	
    double gainThisPixel; /* Antenna gain for this pixel */
    int old_ind;          /* Just to make sure we are not caught in an infinite loop*/
  
    /* Calculate the look angle for this pixel and check to see if it is valid */
  
    lookAngle=meta_look(meta,line,sample);
 
    if(lookAngle>s->ang_vec[s->vecLen-1] || lookAngle<s->ang_vec[0])
    {
	fprintf(stderr,"Function lookUpGain: Look angle outside of valid range\n");
	return NULL;
    }

    if (ind>s->vecLen-1) ind=0;
    old_ind=ind;

    /* Loop through the Elevation Angle table and find the gain for this pixel */	
    while (ind < s->vecLen-1)
    {
	if((lookAngle >= s->ang_vec[ind]) && (lookAngle < s->ang_vec[ind+1]))
	{  /* Find the difference between the steps in Elevation Angle in the table */
	    gainThisPixel=interpolate(lookAngle, s->ang_vec[ind], s->ang_vec[ind+1], s->gain_vec[ind], s->gain_vec[ind+1]);
	   /* We found the right bin, so we exit */
	    return gainThisPixel;
	}
	/* Keeping searching for the right bin */
	ind++;
	if (ind>s->vecLen-2) ind=0;
	if (ind==old_ind) break;     /*You almost got caught in an infinite loop*/
    }
    /* Function failed if we make it here */
    printf("Function lookUpGain: failed to find pixel gain.\n");
    fflush(NULL);
    return 0;  /* Not possible to get here; just to silence compiler */
}


void calculateRCS(int projectionFlag, meta_parameters *meta, float *DNsquared, float *radarCrossSection, int curLine,int numSamples, const satellite *s)
{
  double incidAngle=0;		/* Calculated incidence angle for each pixel */
  double slantRange=0;		/* Calculated slant range for each pixel */
  int ii;                       /* Loops through all samples in current line */
  double myGain,noiseVector;    /* Gain and noise element for the current line & sample */
  if (projectionFlag==BETA_0)     /*Beta_Naught*/
  {
    for (ii=0;ii<numSamples;ii++)    
    {
      incidAngle=meta_incid(meta,curLine,ii);

      slantRange=meta_get_slant(meta,curLine,ii);
      myGain=lookUpGain(meta,curLine, ii,s);
      noiseVector=calibrationGain(myGain,slantRange,RANGEREF,incidAngle);
      radarCrossSection[ii]=beta_naught(noiseVector,s->noise,s->gain, DNsquared[ii],incidAngle);

    }
  }
 else if (projectionFlag==GAMMA_0) /*Gamma_Naught*/
  {
    for (ii=0;ii<numSamples;ii++)    
    {
      incidAngle=meta_incid(meta,curLine,ii);
      slantRange=meta_get_slant(meta,curLine,ii);
      myGain=lookUpGain(meta,curLine, ii,s);
      noiseVector=calibrationGain(myGain,slantRange,RANGEREF,incidAngle);
      radarCrossSection[ii]=gamma_naught(noiseVector,s->noise,s->gain, DNsquared[ii],incidAngle);
    }
  }
  else                       /*Sigma_Naught*/
  {
    for (ii=0;ii<numSamples;ii++)    
    {
      incidAngle=meta_incid(meta,curLine,ii);
      slantRange=meta_get_slant(meta,curLine,ii);
      myGain=lookUpGain(meta,curLine, ii,s);
      noiseVector=calibrationGain(myGain,slantRange,RANGEREF,incidAngle);
      radarCrossSection[ii]=sigma_naught(noiseVector,s->noise,s->gain, DNsquared[ii]);

    } 
 
  } 
}


/* The following is the antenna pattern correction code, with the elevang_vec
and the gain_vec taken from an odl cal_params file.  This function takes one
output line and corrects it by muliplying its values by a gain indexed from
the look angle, added by Mark Ayers 7/00 */

void antptn_correct(meta_parameters *meta,FCMPLX *outputBuf,int curLine,int numSamples,const satellite *s)
{
   int samp;			/* Current Sample index for FOR loop */
   double incidAngle=0;		/* Calculated incidence angle for each pixel */
   double slantRange=0;		/* Calculated slant range for each pixel */	
   double gainThisPixel;		/* Antenna gain for this pixel */	 
	
   /* Process line "curLine" of data */
   for(samp=0;samp<numSamples;samp++)
   {	
	incidAngle=meta_incid(meta,curLine,samp);
	slantRange=meta_get_slant(meta,curLine,samp);
 	gainThisPixel=lookUpGain(meta,curLine,samp,s)*PROC_GAIN;

	/* Now that we have the right gain, multiply it by the I and Q values in the output Buffer */
	outputBuf[samp].r=sqrt(calibrationGain(gainThisPixel,slantRange,RANGEREF,incidAngle))*outputBuf[samp].r;
	outputBuf[samp].i=sqrt(calibrationGain(gainThisPixel,slantRange,RANGEREF,incidAngle))*outputBuf[samp].i;		

   }
}

/* Write the noise table to a file in the form noise vs. slant range
   this table is written out to a .noise file */
void writeTable(meta_parameters *meta,const satellite *s,int numSamples)
{

	int line=0;			/* Line to calculate the noise vector from */
	int index;			/* Table indexing counter */
	int tableEntries=256;		/* Number of table entries */
        char cmd[300];                  /* arg for system command*/
	double slantRange,incidAngle;	/* Slant range and Incidence angle variables */
	double lookAngle,antennaGain;	/* Look angle and Antennta Gain variables */
	double *table;			/* Noise table */
	double indexDelta;		/* Index delta between elements across the image */
	FILE *noisePtr;			/* Noise vector output file pointer */

	table=(double *)MALLOC(sizeof(double)*tableEntries);
	indexDelta=numSamples/tableEntries;

	/* Generate the Noise Table and Write it out on the execution of aisp_setup */
	for(index=0;index<tableEntries;index++)
	{
		table[index]=0;
		slantRange=meta_get_slant(meta,line,index*indexDelta);
		incidAngle=meta_incid(meta,line,index*indexDelta);
		lookAngle=meta_look(meta,line,index*indexDelta);
	        antennaGain=lookUpGain(meta,line,index*indexDelta,s);
		table[index]=calibrationGain(antennaGain,slantRange, RANGEREF, incidAngle);
	}
	/* Write out the noise vector and the antenna pattern */
        sprintf(cmd,"cp %s %s.ant",g.CALPRMS,g.out);     system(cmd);
        noisePtr=FOPEN(strcat(g.out,".noise"),"w");
        fprintf(noisePtr,"%d\n",tableEntries);
	for(index=0;index<tableEntries;index++)
		fprintf(noisePtr,"%f\n",table[index]);

	FCLOSE(noisePtr);
}
