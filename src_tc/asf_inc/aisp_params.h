/****************************************************************************
*								            *
*   aisp_params.h   Parameters needed to run AISP                           *
*   Copyright (C) 1997  Alaska SAR Facility		   	    	    *
*									    *
*   ASF APD LAB Contacts:						    *
*	Special Projects Managet - Rick Guritz  rguritz@asf.alaska.edu      *
*	Lead Software Engineer - Tom Logan	tlogan@asf.alaska.edu       *
* 									    *
*	Alaska SAR Facility			APD Lab Web Site:	    *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

/*-------------------------------------------------------------------------*/
/*   This structure contains all of the parameters needed to run asp.c     */
/*-------------------------------------------------------------------------*/
struct AISP_PARAMS {
  char	in1[256];	     /* First input file basename		   */
  char	out[256];	     /* Output File basename                       */
  char  CALPRMS[255];	     /* Calibration Parameters file (if desired)   */
  int	iflag;		     /* Debugging Flag 	   			   */
  int   nbytes;		     /* Number of input bytes per line (w/ header) */
  int   ngood;		     /* Number of input bytes per line (no header) */
  int	hamFlag;	     /* Use a hamming window on the az ref func.   */
  int   kaiFlag;	     /* Use a kaiser window on the az ref func.    */
  int	pwrFlag;	     /* Create a power image			   */
  int   ifirstline;	     /* First line to read (from 0)  		   */
  int   npatches;	     /* Number of range input patches 		   */
  int   ifirst;		     /* First sample pair to use (start 0)	   */
  int   na_valid;	     /* Number of valid points in the Azimuth	   */
  int   deskew; 	     /* Deskew flag				   */
  float caltone;	     /* Caltone % of sample rate		   */
  int   isave;		     /* Start range bin				   */
  int   nla;		     /* Number of range bins to process		   */
  float fd;		     /* Doppler centroid quadratic coefs (Hz/prf)  */
  float	fdd;		     /* Doppler centroid quadratic coefs (Hz/prf)  */
  float fddd;	             /* Doppler centroid quadratic coefs (Hz/prf)  */
  float re;		     /* Earth Radius (m)			   */
  float vel;		     /* Body fixed S/C velocity (m/s)		   */
  float ht;		     /* Spacecraft height (m)			   */
  float r00;		     /* Range of first sample in raw data file (m) */
  float	prf;		     /* Pulse Repitition Frequency (pps)	   */
  float xmi;		     /* bias value for i values			   */
  float xmq;		     /* bias value for q values			   */
  char  iqflip[4];	     /* Flip i/q or offset video (y/n flip;o off)  */ 
  float azres;		     /* Desired azimuth resolution (m)		   */
  int   nlooks;		     /* Number of looks in the azimuth		   */
  float	fs;		     /* Range sampling rate (Hz)	  	   */
  float	slope;		     /* Chirp Slope (Hz/s)			   */
  float	pulsedur;	     /* Pulse Duration (s)			   */
  float	nextend;	     /* Chirp Extension Points			   */
  char  srm[4];		     /* Secondary range migration correction (y/n) */
  float wavl;		     /* Radar Wavelength			   */
  float	rhww;		     /* Range spectrum wt. (1.0=none;0.54=hamming) */
  float	pctbw;		     /* Fraction of range bandwidth to remove  	   */
  float	pctbwaz;	     /* Fraction of azimuth bandwidth to remove	   */
  float sloper, interr,	     /* 1st Patch Slope and Inter. for range       */
	slopea, intera;      /* 1st Patch Slope and Inter. for azimuth     */
  float dsloper, dinterr,    /* Delta slope,inter per patch in range       */
	dslopea, dintera;    /* Delta slope,inter per patch in azimith     */
  float timeOff,slantOff; /*Characteristic Timing Offset (manually entered.*/
};
void print_params(const char *in,struct AISP_PARAMS *a,const char *sourceProgram);
void read_params(const char *in,struct AISP_PARAMS *);

