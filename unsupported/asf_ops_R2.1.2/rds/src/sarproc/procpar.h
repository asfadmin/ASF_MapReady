#ifndef _PROCPAR_H
#define _PROCPAR_H

static char sccsid_procpar_h[] = 
    "@(#)procpar.h	1.2 96/04/09 20:29:11";

/* -- parameter data -- */
/* varables derived from the parameter files */

/* system data */
extern int sys_ckspeed;		/* system clock frequency */

/* input data */
extern int in_bypass;		/* 1 = bypass input interface */
extern char in_source[80];	/* input source filename / DCRSI */
extern int in_format;		/* sensor: 0=ERS(OGRC)
					   1=JERS
					   2=RADARSAT
					   3=ERS(OBRC) */
extern int in_startblock;	/* starting block if input from DCRS */
extern int in_startline;	/* starting line if input from file */
extern int in_startpoint;	/* starting point in line if input is
					from a file */
extern int in_blkoff;		/* sync code offset in first block */
extern int in_shift;		/* byte alignment shift */
extern int in_norminv;		/* 1=normal, 0=inverted bit values */
extern char in_trantbl[80];	/* input translate table filename */
extern int in_linelen;		/* input line length */
extern int in_outlen;		/* output line length (to range cor) */
extern int in_chirp;		/* chirp length */
extern int in_vlines;		/* # of valid lines per frame */
extern int in_ilines;		/* # of invalid lines per frame */
extern int in_scenelen;		/* # of frames in the scene */
extern int in_offch;		/* line # where offset changes */
extern int in_offset1;		/* offset used before change */
extern int in_offset2;		/* offset used after change */
extern int in_swstart;		/* start of swath within range line */
extern int in_lenoff;		/* length offset */
extern char in_jdfm_tbl[80];	/* JDFM table filename */
extern char in_stc_wgt[80];	/* STC weight filename */

/* range correlator data */
     /* from parameter file */
extern int rc_bypass;		/* 1 = bypass range correlator */
extern int rc_fscale;		/* forward fft scaling flags */
extern int rc_iscale;		/* inverse fft scaling flags */
extern char rc_reffun[80];	/* reference function filename */
extern int rc_refrot;		/* ref. function rotation */
extern char rc_rcfun[80];	/* radiom.comp. function filename */
extern int rc_rcrot;		/* radiom.comp. function rotation */
     /* special modes */
extern int rc_refform;		/* 1=divide ref.func. output by 2 */
extern int rc_refconj;		/* 1=conjugate an input to ref.funct */
extern int rc_refrdat;		/* 1=conjugate ref.funct, 0=data */
extern int rc_rcform;		/* 1=divide r.c.func. output by 2 */
extern int rc_rcconj;		/* 1=conjugate an input to r.c.funct */
extern int rc_rcrdat;		/* 1=conjugate r.c.funct., 0=data */

/* corner turn data */
     /* from parameter file */
extern int ct_bypass;		/* 1 = bypass corner turn */
extern int ct_outlen;		/* output line length */
extern int ct_vlpf;		/* valid lines per frame */
extern int ct_rdlen1;		/* frame 1 read length */
extern int ct_rdlen2;		/* frame 2 read length */
extern int ct_aspect;		/* aspect ratio */
     /* special modes */
extern int ct_unsrot;		/* unscrambler rotation */

/* azimuth forward fft data */
extern int af_bypass;		/* 1 = bypass azimuth forward fft */
extern int af_scale;		/* fft scaling flags */

/* azimuth processing data */
     /* from parameter file */
extern int ap_bypass;		/* 1 = bypass azimuth processing */
extern int ap_looks;		/* 1 or 4 look data */
extern int ap_rmwt;		/* range migration weighting funct */
extern int ap_rmlines;		/* # of range migration output lines */
extern char ap_rmpath[80];	/* range migration funct. filename */
extern int ap_looklen;		/* valid points per look */
extern int ap_lookpos;		/* first non-zero point in look */
extern char ap_xfun[80];	/* xfer function filename */
extern int ap_scale;		/* fft scaling flags */
extern int ap_azwt;		/* azimuth interp weighting function */
extern int ap_detshift;		/* detector bit shift 
					(-1 = bypass detector) */
extern int ap_ptspace;		/* az. interp. point spacing */
extern char ap_azstart[80];	/* azint line start array filename */
     /* special modes */
extern int ap_rmform;		/* 1 = divide r.m. output by 2 */
extern int ap_xform;		/* 1 = divide xfer output by 2 */
extern int ap_xconj;		/* 1 = conjugate an input to xfer */
extern int ap_xtran;		/* 1=conj. xfer funct., 0=data */
extern int ap_azform;		/* 1=divide azint output by 2 */

/* multi-look data */
extern int ml_bypass;		/* 1 = bypass multi-look processing */
extern char ml_output[80];	/* output disk filename or "DCRSI" */
extern int ml_startblock;	/* start block if output is to DCRSi */
extern int ml_outbits;		/* output 16 or 32 bits */
extern int ml_frdelay;		/* number of frames to delay */
extern int ml_froff;		/* azimuth offset between frames */
extern int ml_azlen;		/* azimuth line length */
extern int ml_linelen;		/* multi-look output line length */
extern int ml_azoff;		/* azimuth offset */
extern char ml_rotfun[80];	/* rotation function filename */
extern int ml_vppl;		/* valid points per look */
extern int ml_lkoff;		/* look offset */
extern int ml_aspect;		/* aspect ratio */
extern int ml_segsize;		/* # of points to use in avg buffers */
extern int ml_divisor;		/* divisor for averaging */


/* variables used by multiple modules that are not
   directly derived from parameter file.
   all of these variables have prefix pr_ .
*/

extern int pr_nlines;		/* number of valid lines in scene */
extern int pr_tlines;		/* total number of lines in scene 
					(valid + invalid) */
extern int pr_segments;         /* number of segments in staged FFT */
extern int pr_valpoints;        /* number of fully correlated points 
					in a line */
extern int pr_rglen;		/* length of range line to be 
					processed */
extern int pr_actline;		/* actual length of the range line */
extern int pr_look;             /* 1 for 4 look; 0 for 1 look */
extern int pr_aifftlen;         /* azimuth inverse FFT length */
extern int pr_avglines;		/* number of averaged lines */

/* register override data */
typedef struct regov {
	struct regov *nxt_regov; /* pointer to next override */
	int addr;		/* register address */
	int value;		/* register value */
} REGOV, *ROPTR;

extern ROPTR regov_list;	/* register overflow data list */

#endif /* ! _PROCPAR_H */
