#ifndef _PROCFIL_H
#define _PROCFIL_H

static char sccsid_procfil_h[] = 
    "@(#)procfil.h	1.2 96/04/09 20:29:11";

/* procfil.h -- Alaska SAR processing file & data definitions */

typedef struct {	/* default processing parameter file */
	char sat[3];		/* sensor name */
	int ns_ra;		/* number of samples per range line */
	int nfft_ra;		/* range fft length */
	int fftsc_raf;		/* range forward fft scaling */
	int fftsc_rai;		/* range inverse fft scaling */
	int nchirp;		/* # of points in range chirp */
	int nfft_az4;		/* azimuth forward fft length, 4 look */
	int nfft_az1;		/* azimuth forward fft length, 1 look */
	float bw_az4;		/* azimuth processor bandwidth/look */
	int fftsc_az4f;		/* azimuth fwd fft scaling: 4 look */
	int fftsc_az1f;		/* azimuth fwd fft scaling: 1 look */
	int fftsc_az4i;		/* azimuth inv fft scaling: 4 look */
	int fftsc_az1i;		/* azimuth inv fft scaling: 1 look */
	int det_sc;		/* detector scaling */
	float h_az;		/* azimuth weighting pedestal */
	float a_prf;		/* prf conversion constant term */
	float b_prf;		/* prf conversion linear term */
	float a_dwp;		/* data window conversion const. term */
	float b_dwp;		/* data window conversion linear term */
	float fcnoise;		/* noise center frequency */
	float bwnoise;		/* noise bandwidth */
	float fcsig;		/* signal center frequency */
	float bwsig;		/* signal bandwidth */
	int pro_gain;		/* total gain through processor */
      /* clutter-lock routine parameters */
	int clock_iter;		/* maximum clutter-lock iterations */
	float fderror_max;	/* maximum total error to pass test */
	float fdaerror_max;	/* maximum fda chng to try quadratic */
	float fdberror_max;	/* maximum fdb chng to try quadratic */
	float fdc_max;		/* maximum fdc allowed */
      /* processor gain and fft overflow parameters */
	float gain_ref;		/* expected energy at standard gain */
	int scale_threshold;	/* fft overflow register threshold */
      /* lookup table generation routine parameters */
	float lmean_tol;	/* I and Q mean value error tolerance */
	float lstdv_diff_max;	/* max relative diff, I to Q std. dev.*/
	int blkskip;		/* blocks to skip to find data */
	int sbm;  		/* single byte match */
	int fsm;		/* frame sync byte match */
	int maxfsdo;		/* max # of consecutive frames with */
				/*   missing sync codes.            */
      /* the next three factors are for the radiometric data record */
	float noise_fctr;	/* noise scale factor constant */
	float linear_fctr;	/* linear conversion factor constant */
	float offset_fctr;	/* offset conversion factor constant */
	float peak_ref;		/* peak reference */
} DPP_FILE, *DPP_FILE_PTR;

typedef struct ryp {	/* roll, yaw, pitch data */
	float roll;
	float yaw;
	float pitch;
} RYP;

typedef struct {	/* sensor parameters file */
	char sat[3];		/* sensor name */
	double xlambda;		/* radar wavelength */
	float beam_el;		/* elevation beam width */
	float beam_az;		/* azimuth beam width */
	char side;		/* L (left) or R (right) side looking */
	float gamma_nom;	/* look angle */
	double csr;		/* complex sampling rate */
	float incl;		/* nominal orbit inclination */
	float ecc;		/* nominal orbit eccentricity */
	float bw_ra;		/* bandwidth of transmitted chirp */
	float tau;		/* transmitted pulse length (time) */
	int nbits;		/* # of bits per I or Q sample */
	RYP eatt;		/* extreme attitude roll, yaw, pitch */
	RYP eattrt;		/* extreme attitude rates for each */
				/* replica minimum check values: */
	float xlocation;	    /* peak location */
	float x3db;		    /* 3db signal width */
	float xpslr;		    /* peak side lobe ratio */
	float xislr;		    /* integrated side lobe ratio */
	float pbw;		/* total processor bandwidth */
	float dbw;		/* Antenna azimuth pattern b/w */
	double srspace;		/* slant range pixel spacing */
	double grspace;		/* ground range pixel spacing */
	float ant_len;		/* azimuth antenna length */
	float minsnr;		/* minimum acceptable snr (dB) */
	float est_noise_floor;	/* estimated noise floor */
	float noise;		/* noise when no pre or post */
} SP_FILE, *SP_FILE_PTR;

typedef struct {	/* range transfer function file */
	int npts;		/* # of points in transfer function */
	float tau;		/* pulse duration */
	double csr;		/* complex sampling rate */
	float a;		/* secondary range compression coeff. */
	float b;		/* linear chirp phase coefficient */
	float c;		/* quadratic chirp phase coefficient */
	float powmax;		/* maximum power t.f. element */
	float avgpow;		/* average power in transfer function */
	float h;		/* range weighting pedestal */
	float powloss;		/* power loss in weighting */
} RTF_FILE, *RTF_FILE_PTR;

typedef struct {	/* Earth model file */
	char earth_mod[25];	/* name of earth model */
	char emod_src[25];	/* source of the model */
	double r_e;		/* radius of earth at equator */
	double f;		/* flattening factor */
	double r_pole;		/* radius of earth at pole */
} EM_FILE, *EM_FILE_PTR;

typedef struct {	/* Antenna elevation pattern file */
	char name[25];		/* name of elevation pattern */
	char src[25];		/* source of pattern */
	int entries;		/* # of points in pattern */
	float step;		/* measurement step size */
	float beg;		/* elevation of first measurement */
	float end;		/* elevation of last measurement */
} AEP_FILE, *AEP_FILE_PTR;

typedef struct {	/* Greenwich Mean Time (GMT) record */
	int yr;			/* GMT year */
	int day;		/* GMT day */
	int hr;			/* GMT hour */
	int min;		/* GMT minute */
	float second;		/* GMT second and millisecond */
} GMT, *GMT_PTR;

typedef struct xyz {	/* x, y, z data record */
	double x;
	double y;
	double z;
} XYZ;

typedef struct {	/* job request */
	char    id[30];		/* job id */
	char    type[4];	/* job type code (STD, CPX, RPR, CSD) */
	char    site[33];	/* image site name */
	char    take_id[14];	/* data take id: SS/X/rrrrr.nn */
	char    tape_id[7];	/* Cassette ID: TYnnnn */
	int	start_blk;	/* first block recorded */
	int	end_blk;	/* last block recorded */
	GMT	start;		/* time of first good SAR line */
	GMT	end;		/* time of last good SAR line */
	float	lat;		/* latitude of center of desired area */
	float	lon;		/* longitude of cntr of desired area */
	GMT	targ;		/* time of target area */
	int	targ_rg;	/* target ground range in Km */
	float	ave_hght;	/* average terrain height */
	int	proc_gain;	/* desired processor gain */
	char    deskew[4];	/* "deskew applied" flag: YES or NOT */
	char    gnd_slnt_rg[7];	/* range flag: GROUND or SLANT */
	char	jobname[15];	/* derived job name */
	int	status;		/* job processing status flag */
} RQST_RCD, *RQST_PTR;

typedef struct sv {	/* state vector */
	struct sv *nxt_sv;	/* pointer to next statevector */
	int rev;		/* orbit number */
	XYZ pos;		/* x,y,z position of spacecraft */
	XYZ vel;		/* x,y,z velocity of spacecraft */
	GMT gmt;		/* current time */
	int time;		/* satellite binary time */
	int precision;		/* precision of statevectors */
				/* 1 = PREDICTED             */
				/* 2 = RESTITUTED            */
				/* 3 = PRELIMINARY           */
				/* 4 = PRECISION             */
} SV, *SV_PTR;

typedef struct {	/* two minute state vector file */
	char sat[3];		/* sensor name */
	int rev;		/* current orbit number */
	int nsv;		/* number of statevectors */
	SV_PTR sv_list;		/* pointer to first state vector */
} TMSV_FILE, *TMSV_FILE_PTR;

typedef struct {	/* state vector file */
	SV sv;			/* state vector data */
	RYP att;		/* roll, yaw, pitch s/c attitude */
} SV_FILE, *SV_FILE_PTR;

typedef struct {	/* time correlation element */
	int rev;		/* orbit number */
	GMT gmt;		/* Greenwich mean time */
	unsigned int bt;	/* satellite binary time */
	int delta;		/* clock period of satellite clock */
				/*  for J-ERS-1, time error in mlsc */
} TC_FILE, *TC_FILE_PTR;

typedef struct {	/* initial doppler parameters (slant range) */
	float fda;		/* Doppler frequency constant term */
	float fdb;		/* Doppler frequency linear term */
	float fdc;		/* Doppler frequency quadratic term */
	float fdota;		/* Doppler rate constant term */
	float fdotb;		/* Doppler rate linear term */
	float fdotc;		/* Doppler rate quadratic term */
} IDP_FILE, *IDP_FILE_PTR;

typedef struct pream {	/* data take preamble file */
	struct pream *nxt_pre;	/* pointer to next preamble */
	char sat[3];		/* sensor name */
	char dcrs_id[15];	/* dcrs casette id */
	char takeid[15];	/* data take ID */
	int prenum;		/* preamble number */
	char sen_mode[9];	/* sensor mode */
	double prf;		/* pulse repetition frequency */
	int rec_gain;		/* receive channel gain */
	int cal_att;		/* calibration attenuation */
	float noise_dton;	/* noise power at data take start */
	float calp_dton;	/* calibration pulse power at start */
	float repp_dton;	/* first 8 replicas average power */
	int fmt_start;		/* format # of start of pp region */
	int fmt_id;		/* recorded id # of the format */
	int blk_start;		/* casette block # of preamble start */
	int bit_off;		/* bit offset within block */
	int polarity;		/* 0 = normal, 1 = inverted polarity */
	double time_ref_fmt;	/* time reference format number */
	GMT time_ref_gmt;	/* time reference GMT */
} PREAM_FILE, *PREAM_FILE_PTR;

typedef struct postam {	/* data take postamble file */
	struct postam *nxt_post; /* pointer to next postamble */
	char takeid[15];	/* data take ID */
	int postnum;		/* postamble number */
	int rec_gain;		/* receive channel gain */
	int cal_att;		/* calibration attenuation */
	float noise_dtoff;	/* noise power at data take end */
	float calp_dtoff;	/* calibration pulse power at end */
	float repp_dtoff;	/* last 8 replicas average power */
	int fmt_start;		/* format # of start of pp region */
	int fmt_id;		/* recorded id # of the format */
	int blk_start;		/* casette block # of postamble start */
	int bit_off;		/* bit offset within block */
	int polarity;		/* 0 = normal, 1 = inverted polarity */
} POSTAM_FILE, *POSTAM_FILE_PTR;

typedef struct tape_seg { /* valid tape data segment block */
	struct tape_seg *nxt_seg; /* pointer to next tape segment */
	struct ppb *ppb_list;	/* pointer to preproc. block list */
	PREAM_FILE_PTR pre;	/* pointer to segment's preamble */
	POSTAM_FILE_PTR post;	/* pointer to segment's postamble */
	int prenum;		/* preamble number */
	int postnum;		/* postamble number */
	int fmt_start;		/* starting format # */
	int fmt_id;		/* recorded id # of start format */
				/* for ERS: AMI format # */
	int fmt_end;		/* ending format # */
	int end_id;		/* recorded id # of end format */
	int blk_start;		/* cassette block # of segment start */
	int bit_off;		/* bit offset within block */
	int polarity;		/* 1 = normal, 0 = inverted polarity */
	int gap_type;		/* 0 = end of take   */
				/* 1 = recording gap */
				/* 2 = loss of sync  */
	int blk_end;		/* cassette block # of segment end */
	GMT start;		/* time of first good data format */
	GMT end;		/* time of last good data format */
	int win_count;		/* number of window changes in table */
	int wfmt[20];		/* format numbers at window changes */
	int wdwp[20];		/* window position at window changes */
	int agc_count;		/* number of AGC changes in table */
	int afmt[80];		/* format numbers at AGC changes */
	int agc[80];		/* agc value at AGC changes */
	int pptotal;		/* total number of pp region in a seg */
	int ppfmt[40];		/* starting format of pp region */
	int ppblk[40];		/* block addr of ppfmt */
	int ppoff[40];		/* offset in block of ppfmt */
} TAPE_SEG, *TAPE_SEG_PTR;

typedef struct ppb {	/* preprocessing region data block */
    /* sensor status data */
	struct ppb *nxt_ppb;	/* pointer to next preproc. block */
	int fmt_start;		/* format # of start of pp region */
	int fmt_id;		/* recorded id # of the format */
	int blk_start;		/* casette block # of start of reg. */
	int bit_off;		/* bit offset within block */
	int polarity;		/* 0 = normal, 1 = inverted polarity */
	int dly1;		/* first data window delay */
	int dly2;		/* second data window delay */
	int dly_chg;		/* data window delay change line# */
	double r_close;		/* slant range to first range cell */
	double r_mid;		/* slant range to middle range cell */
	double r_far;		/* slant range to last range cell */
	int loc_mid;		/* middle range cell number */
	int loc_far;		/* far range cell number */
	SV sv;			/* original statevector */
	RYP att;		/* roll, yaw, pitch attitude of SAR */
	float agc_pos;		/* gain adjustment curve position */
				/*  (not applicable to ERS-1) */

    /* signal parameters data */
	float snr;		/* signal-to-noise ratio in pp region */
	float ber;		/* bit error rate in pp region */
	float pslr;		/* peak side lobe ratio */
	float islr;		/* integrated side lobe ratio */
	float imean;		/* I channel mean */
	float istdv;		/* I channel standard deviation */
	float iprob;		/* I ch. prob. of gaussian likelihood */
	float qmean;		/* Q channel mean */
	float qstdv;		/* Q channel standard deviation */
	float qprob;		/* Q ch. prob. of gaussian likelihood */
			    /* the next 6 values are after correction */
	float imean_cor;	/* I channel mean */
	float istdv_cor;	/* I channel standard deviation */
	float iprob_cor;	/* I ch. prob. of gaussian likelihood */
	float qmean_cor;	/* Q channel mean */
	float qstdv_cor;	/* Q channel standard deviation */
	float qprob_cor;	/* Q ch. prob. of gaussian likelihood */
	int ihist_cor[64];	/* I histogram */
	int qhist_cor[64];	/* Q histogram */
	float iqrephase;	/* phase angle between I and Q */
	int reperror;		/* error flag for replica measure */

    /* processing parameter data */
	int fftsc_raf;		/* range forward fft scaling */
	int fftsc_rai;		/* range inverse fft scaling */
	float bw_az4_act;	/* actual azimuth processor bw/look */
	int fftsc_az4f;		/* azimuth fwd fft scaling: 4 look */
	int fftsc_az1f;		/* azimuth fwd fft scaling: 1 look */
	int fftsc_az4i;		/* azimuth inv fft scaling: 4 look */
	int fftsc_az1i;		/* azimuth inv fft scaling: 1 look */
	int det_sc;		/* detector scaling */
	float h_az;		/* azimuth weighting pedestal */
	    /* default parameters above, derived parameters below */
	float r_e;		/* Earth model equatorial radius */
	float f;		/* Earth model flattening factor */
	char mcv_file[80];	/* rnge radiom. corr. vector filename */
	float mcv_max;		/* unnormalized maximum entry in mcv */
	int pro_gain;		/* overall gain through processor */
	float sat_speed;	/* speed of satellite in space */
	float swth_speed;	/* speed of swath on Earth surface */
	float targ_speed;	/* speed of target on Earth surface */
	float vrel;		/* relative vel between targ and SAR */
	float lat_rough;	/* rough region cntr. latitude loc. */
	float lon_rough;	/* rough region cntr. longitude loc. */
	float fda;		/* Doppler frequency constant term */
	float fdb;		/* Doppler frequency linear term */
	float fdc;		/* Doppler frequency quadratic term */
	float fdota;		/* Doppler rate constant term */
	float fdotb;		/* Doppler rate linear term */
	float fdotc;		/* Doppler rate quadratic term */
	int quality;		/* region quality flag */
				/*   -1 = moved */
	char img_name[16];	/* image product id */
	int bypass;		/* 1 = bypass this region */
	float peak;		/* peak from replica measure */
	float noise;		/* noise for each region */

	double near_start[3];	/* latitude, longitude, radius at 'A' */
	double far_start[3];	/* latitude, longitude, radius at 'B' */
	double center[3];	/* latitude, longitude, radius at 'C' */
	double near_end[3];	/* latitude, longitude, radius at 'D' */
	double far_end[3];	/* latitude, longitude, radius at 'E' */
} PP_BLOCK, *PP_BLOCK_PTR;

typedef struct {	/* auxiliary data block (ERS-1) */
	int ifmt;		/* IDHT format number */
	int obrc;		/* 1 = OBRC, 0 = OGRC */
	int time;		/* on-board clock value */
	int task;		/* activity task */
	int sample;		/* sample flags */
	int format;		/* AMI format number */
	int window_start;	/* sampling window start time */
	int pri;		/* pulse repitition interval */
	int cal_att;		/* calibration attenuation */
	int rec_gain;		/* receiver gain select */
} AUX_DATA, *AUX_PTR;

typedef struct {	/* house keeping data block for JERS-1 */
	float prf;		/* prf */
	int sar;		/* sar activity index */
	int stc;		/* STC, sampling window start time */
	int agc;		/* AGC, auto gain control data */
	int stp;		/* STP, step attenuation data */
	int format;		/* format number */
} HK_DATA, *HK_PTR;

typedef struct {	/* auxilary info for CEOS leader */
	int	nbins;		/* number of bins in histogram */
	int  	hstgrm[256];	/* histogram table */
	float	meansamp;	/* mean sample */
	float	stdvsamp;	/* standard deviation of sample */
	float	meanfreq;	/* mean frequency */
	float	stdvfreq;	/* standard deviation of frequency */
	float	minfreq;	/* minimum frequency */
	float	maxfreq;	/* maximum frequency */
	int	q_nbins;	/* ditto for q (only CSD) */
	int  	q_hstgrm[256];	/*			   */
	float	q_meansamp;	/*			   */
	float	q_stdvsamp;	/*			   */
	float	q_meanfreq;	/*			   */
	float	q_stdvfreq;	/*			   */
	float	q_minfreq;	/*			   */
	float	q_maxfreq;	/*			   */
} CEOS_INFO, *CEOS_INFO_PTR;

#endif /* ! _PROCFIL_H */
