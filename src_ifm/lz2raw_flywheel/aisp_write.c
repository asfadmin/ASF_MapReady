#include "asf.h"
#include "lzFetch.h"
#include "decoder.h"
#include "aisp_params.h"

/********************************
AISP Parameter file generator routines.
These write AISP input files *.in and *.fmt.
*/
void writeAISPparams(bin_state *s,char *outN,double fd, double fdd, double fddd)
{
	struct AISP_PARAMS g;
	int az_reflen;
	float slantToLast;
	float azpix, rngpix;
	float refPerRange;
	
	g.iflag=1;     /* Debugging Flag 	   			   */
	g.ifirstline=0;    /* First line to read (from 0)  		   */
	g.npatches=1000;   /* Number of range input patches 		   */
	g.ifirst=0;  /* First sample pair to use (start 0)	   */
	g.na_valid=-99;    /* Number of valid points in the Azimuth	   */
	g.deskew=0;     /* Deskew flag				   */
	g.isave=0;    /* Start range bin				   */
	g.nla=s->nValid; /* Number of range bins to process		   */

	if (fd != 0.0)
	  {
	    g.fd=fd/s->prf;
	    g.fdd=fdd/s->prf;
	    g.fddd=fddd/s->prf;
	  }
	else
          { 
	     printf("   WARNING:: No Doppler Parameters Passed to .in file creation\n");
	     g.fd=0.0; g.fdd=-99.0; g.fddd=-99.0;
	     if (!s->zeroDopSteered)
		g.fd=s->estDop;/*Set estimated doppler for non-zero doppler steered satellites*/
	  }

	g.re=s->re; /*approximate earth radius at scene center.*/
	g.vel=s->vel; /*satellite velocity, m/s.*/
	g.ht=s->ht; /*satellite height above earth, m.*/
	g.r00=s->range_gate*speedOfLight/2.0;/*range to target.*/
	g.prf=s->prf;/*Pulse Repetition Freqency*/
	g.azres=s->azres;    /* Desired azimuth resolution (m)		   */
	g.nlooks=s->nLooks;     /* Number of looks to square up data */
	g.fs=s->fs; /*Range sampling frequency, Hz*/
	g.slope=s->slope; /*chirp slope, Hz/sec.*/
	g.pulsedur=s->pulsedur; /*chirp length, in sec.*/
	g.nextend=0;    /* Chirp Extension Points			   */
	g.wavl=speedOfLight/s->frequency; /*radar wavelength, in m.*/
	g.rhww=0.8;   /* Range spectrum wt. (1.0=none;0.54=hamming) */
	g.pctbw=g.pctbwaz=0.0;
	g.sloper=g.interr=g.slopea=g.intera=0.0;
	g.dsloper=g.dinterr=g.dslopea=g.dintera=0.0;
	
	/* Calculate the number of valid patches per line. */
	/* This calculation comes from aisp_setup() in aisp_setup.c. */
		azpix=(g.vel*g.re/(g.re+g.ht))/g.prf;
		rngpix=speedOfLight/(g.fs*2.0);

		/* for acpatch's np- the length of the azimuth 
			reference function */
		refPerRange = g.wavl/(2.0*g.azres*azpix);
		slantToLast=g.r00+(g.isave+g.nla)*rngpix;
		az_reflen=refPerRange*slantToLast;

		g.na_valid = 4096 - az_reflen;
		g.na_valid/=g.nlooks;
		g.na_valid*=g.nlooks;
	/* end calculate valid patches per line */

	print_params(outN,&g,"lz2raw/ceos2raw");
}

void writeAISPformat(bin_state *s,char *outN)
{
	char *aispN;
/*Open & write out start of AISP input format file.*/
	s->dotFMT=FOPEN(aispN=appendExt(outN,".fmt"),"w");
	fprintf(s->dotFMT,
		"%d 0                 ! Bytes per line, bytes per header.\n"
		"%f %f             ! i,q bias\n"
		"n                       ! Flip i/q?\n"
		"! Starting line #, Window Shift (pixels), AGC Scaling\n",
		2*s->nSamp,s->I_BIAS,s->Q_BIAS);
/*s->dotFMT is written to by updateAGC_window, so don't close it yet!*/
	free(aispN);
}
