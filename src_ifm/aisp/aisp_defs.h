/****************************************************************************
*								            *
*   aisp_def.h - Global type, function, and constants definitions            *
*   Copyright (C) 1997  ASF STEP LAB 			   	    	    *
*									    *
*   ASF STEP LAB Contacts:						    *
*	Lab Coordinator   - Rick Guritz		rguritz@images.alaska.edu   *
*	Software Engineer - Tom Logan		tlogan@images.alaska.edu    *
* 									    *
*	Alaska SAR Facility			STEP Lab Web Site:	    *	
*	Geophysical Institute			www.images.alaska.edu	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
/* Global Definitions for ASP software - T. Logan 8/96*/

#ifndef __ASPMATH_H     /* include only once */
#define __ASPMATH_H

/*-----------------------------------------------------*/
/* define complex variable type if not already defined */
/*-----------------------------------------------------*/
#include "asf.h"

#ifndef __complex_var
#define __complex_var
typedef struct {
   float r;
   float i;
} FCMPLX;
#endif

#include "read_signal.h"
#include "geolocate.h"
#include "asf_meta.h"

/*-----------------------------*/
/* Simple function definitions */
/*-----------------------------*/
#define   NINT(a)       ((a)>=0.0 ? (int)((a)+0.5):(int)((a)-0.5)) /* nearest int */
#define   MIN(a,b)      (((a) < (b)) ? (a) : (b))                    /* minimum     */
#define   MAX(a,b)      (((a) > (b)) ? (a) : (b))                    /* maximum     */



float  Cabs(FCMPLX); 
FCMPLX Cadd (FCMPLX,FCMPLX);
FCMPLX Cconj(FCMPLX);
FCMPLX Cmplx(float,float);
FCMPLX Czero();
FCMPLX Csmul(float,FCMPLX);
FCMPLX Cmul (FCMPLX,FCMPLX);

/*cfft1d: Perform FFT, 1 dimentional:
	dir=0 -> init; 
	dir<0 -> forward; 
	dir>0 -> backward*/
void cfft1d(int n, FCMPLX *c, int dir);

/*-----------------------*/
/* Constants Definitions */
/*-----------------------*/
#define   VERSION       2.9
#define   default_n_az 4096

#define   speedOfLight 299792458.0 /*Speed of light in vacuum, m/s */
#define   pi      	3.14159265358979323
#define   pi2     	(2*pi)


/*--------------*/
/*  Prototypes: */
/*--------------*/
double fftEstDop(getRec *inFile,int startLine,int xStride,int nLines);
void estdop(char file[], int nDopLines, float *a, float *b,float *c);
void calc_range_ref(FCMPLX *range_ref, int rangeFFT, int refLen);
void elapse(int fnc);
void multilook(FCMPLX *patch,int n_range,int nlooks, float *pwrs);
void save_ddr(const char *fname,int nl,int ns,int sl,int ss,
	double pdx,double pdy, int li);

/*-------------Structures:---------------
patch: a chunk of SAR data, throughout the processor.
rangeRef: the range reference function, for rciq.
satellite: sundry imaging-related parameters, for rmpatch and acpatch.
file: parameters describing output file.
*/
typedef struct {
	int n_az,n_range;/*Number of samples in azimuth and range directions.*/
	FCMPLX *trans;/*Complex buffer-- indexed as trans[x*n_az+y].*/
	float slantToFirst,slantPer;/*Slant range (m) to first pixel; per pixel.*/
	float fd,fdd,fddd;/*Doppler coefficients (Hz; Hz/pixel; Hz/pixel/pixel)*/
	GEOLOCATE_REC *g;/*Initialized with appropriate inertial state vector*/
	float xResampScale,xResampOffset;/*Resampling range coefficients.*/
	float yResampScale,yResampOffset;/*Resampling azimuth coefficients.*/
	int fromSample,fromLine;/*Patch's location in original file.*/
} patch;

typedef struct {
	int refLen;/*Length of reference function, in samples.*/
	int rangeFFT;/*Length of FFTs for range reference function.*/
	FCMPLX *ref;/*FFT'd range reference function.*/
} rangeRef;

typedef struct {
  int intensity;/* Intensity image flag */
  int power;/*Intensity squared image flag*/
  int sigma;/*RCS image, Sigma_0 projection*/
  int gamma;/*RCS image, Gamma_0 projection*/
  int beta;/*RCS image, Beta_0 projection*/
  int complex;/*Complex image flag*/
} imageFlag;

typedef struct {
	float wavl,vel;/*Radar wavelength (m); fixed-earth velocity (m/s)*/
	float prf;/*Pulse repetition frequency (Hz)*/
	float a2;/*Azimuth doppler deskew coefficient.*/
	int dop_precomp;/*Amount of doppler deskew already removed.*/
	float refPerRange;/*Number of pixels of azimuth reference function per meter of slant range..*/
	int az_reflen;/*Maximum length of azimuth reference function, in pixels.*/
	int ideskew;/*Deskew image? 0-- no; 1-- yes.*/
	int debugFlag;/*Debugging flag, with various bitwise meanings.*/
/*These parameters apply to the "original" SAR image-- before windowing.*/
	float orig_slantToFirst,orig_fd,orig_fdd,orig_fddd;/*Original image slant, doppler.*/
	float sloper,interr,slopea,intera;/*Resampling coefficents (original).*/
	double dsloper,dinterr,dslopea,dintera;/*Resampling deltas.*/
	int hamming;/*hamming window flag*/
	int kaiser;/*kaiser window flag*/
        imageFlag imageType;/* Image type flag -- 0= intensity, 1=power, 2=sigma_0, 3=gamma_0, 4=beta_0 */
	double *ang_vec;/* Look angle vector from the CAL_PARAMS file */
	double *gain_vec;/* Gain vector from the CAL_PARAMS file */
	int vecLen;/* Cal Params vectors length */
        double noise;/* Noise factor applied to noise vector*/
        double gain;/* Linear scale factor to convert DNs to RCS */
	float pctbw; /* Fraction of range bandwith to remove */
	float pctbwaz; /* Fraction of azimuth bandwidth to remove */
} satellite;

typedef struct {
	char in[255]; /*Input file.*/
	char out_cpx[255],out_amp[255]; /*Complex and Amplitude output names.*/
        char out_pwr[255], out_sig[255], out_gam[255], out_bet[255];  /*Power or RCS (dB) output names.*/
	float azpix,rngpix; /*Azimuth and range pixel size.*/
	int firstLineToProcess,firstOutputLine,skipFile,skipSamp;
	int n_az_valid,nlooks;
	int nPatches;
} file;


/*-------------Initialization:---------------*/
#include "aisp_params.h"
int parse_cla(int argc,char *argv[],struct AISP_PARAMS *g,meta_parameters **meta_out);
void aisp_setup(struct AISP_PARAMS *g,meta_parameters *meta,int *N_az,int *N_range,
	satellite **s,rangeRef **r,file **f,getRec **signalGetRec);
patch *newPatch(int n_az,int n_range);
double getDopplerRate(double r,double f0,GEOLOCATE_REC *g);

/*---------Global-free Patch Routines:--------*/
void setPatchLoc(patch *p,satellite *s,meta_parameters *meta,int leftFile,int leftSamp,int top);
void debugWritePatch(const patch *p,char *basename);
void processPatch(patch *p,const getRec *signalGetRec,
	const rangeRef *r,const satellite *s);
void writePatch(const patch *p,const satellite *s,meta_parameters *meta,const file *f,int patchNo);
void destroyPatch(patch *p);

/*-------Routines to manipulate patches.----------*/
void rciq(patch *p,const getRec *signalGetRec,const rangeRef *r);
void rmpatch(patch *p,const satellite *s);
void acpatch(patch *p,const satellite *s);
void antptn_correct(meta_parameters *meta,FCMPLX *outputBuf,int curLine,int numSamples,const satellite *s);
void writeTable(meta_parameters *meta, const satellite *s, int numSamples);
#endif
