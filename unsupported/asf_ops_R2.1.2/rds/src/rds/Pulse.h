/*=============================================================================
 |  @(#)Pulse.h	1.54 98/04/06 10:12:48
 |
 |  Generic Pulse Definition.
 |  Alaska SAR Facility (ASF) Project.
 |  Copyright (C) Jet Propulsion Laboratory.
 |
 *============================================================================*/
#ifndef _RDS_PULSE_H_
#define _RDS_PULSE_H_

#ifdef  NOSTDARG
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#include <math.h>
#include "Time.h"

static const char sccsid_Pulse_h[] =
	"@(#)Pulse.h	1.54 98/04/06 10:12:48";

#define	U8		unsigned char
#define	U16		unsigned short
#define	U32		unsigned int

#define	PI		((double) 3.14159265358979323846)
#define MC		((double) 2.99792458E8)
#define RE		((double) 6378144.0)	/* Meters */
#define XLAMBDA         ((double) 0.0565646)
#define EMF_RE          ((double) 6378144.0)    /* Meters */
#define EMF_RP          ((double) 6356755.0)    /* Meters */
#define EMF_F           ((double) 298.257)
#define	RAD_PER_DEG	(PI / (double) 180)
#define	DEG_PER_RAD	((double) 180 / PI)

/*--------------------*
 |  Instrument Modes
 *--------------------*/
typedef enum {
    ST1, ST2, ST3, ST4, ST5, ST6, ST7,	/* R1 Standard */
    SWA, SWB, SNA, SNB,			/* R1 ScanSAR */
    WD1, WD2, WD3,			/* R1 Wide */
    FN1, FN2, FN3, FN4, FN5,		/* R1 Fine Resolution */
    EL1,				/* R1 Low Incidence */
    EH1, EH2, EH3, EH4, EH5, EH6,	/* R1 High Incidence */
    ES1,				/* E1 Standard */
    ES2,				/* E2 Standard */
    JS1,				/* J1 Standard */
    MODE_INVALID
} Mode_t;

#define	MODE_MAX	MODE_INVALID
#define	isScanSAR(_m)	((_m) >= SWA && (_m) <= SNB)
extern  const char*	ModeImage(Mode_t x);


/*--------------------*
 |  Instrument Modes
 *--------------------*/
typedef enum {
    /* R1 Standard */
    BEAM_S1 = 1, BEAM_S2, BEAM_S3, BEAM_S4, BEAM_S5, BEAM_S6, BEAM_S7,

    /* R1 Wide */
    BEAM_W1, BEAM_W2, BEAM_W3, BEAM_W2R,

    /* R1 Low/High Incidence */
    BEAM_EL1, BEAM_EH2, BEAM_EH4, BEAM_EH6,

    /* R1 Fine Resolution */
    BEAM_F1, BEAM_F2, BEAM_F3, BEAM_F4, BEAM_F5,

    /* E1 Standard */
    BEAM_E1,

    /* E2 Standard */
    BEAM_E2,

    /* J1 Standard */
    BEAM_J1,
    BEAM_INVALID
} Beam_t;

#define	BEAM_MAX	BEAM_INVALID

/*---------------------*
 |  State Vector Type
 *---------------------*/
typedef enum {
    PREDICTED = 1,		/* This definition should agree with SV def */
    RESTITUTED,
    PRELIMINARY,
    PRECISION
    
} SV_Type_t;

extern  const char*	SV_TypeImage(SV_Type_t x);

/*-------------------------------------------*
 |  Type of State Vector Coordinate Systems
 *-------------------------------------------*/
typedef enum {
    TRUE_EQUATORIAL = 1,
    MEAN_OF_DATE
    
} SV_Coord_t;

extern  const char*	SV_CoordImage(SV_Coord_t x);

/*--------------------*
 |  Tape Position
 *--------------------*/
typedef struct {
    int		blk;		/* Block number */
    int		bit;		/* Bit offset within block */
} TapePos_t;

/*--------------------------*
 |  Telemetry Frame Header
 *--------------------------*/
typedef struct { 
    TapePos_t	start;
    void*	tapebuf;	/* Tape Buffer */
    U16         bit_error;	/* Number of SYNC bit errors */
    U16		frame_cnt;	/* Frame counter */

} Frame_t;

/*------------------------------*
 |  Pulse == Series of Frames
 *------------------------------*/
typedef	Frame_t	Pulse_t;	/* Pulse == Sequence of telemetry frames */

/*------------------*
 |  Complex Number 
 *------------------*/
typedef struct {
    double	re;
    double	im;

} Complex_t;

/*-------------------------*
 |  For range calculation
 *-------------------------*/
typedef struct {	
    double      near;		/* slant range to first range cell */
    double      mid;            /* slant range to middle range cell */
    double      far;            /* slant range to last range cell */

} Range_t;

/*---------------------------------------------------------------*
 |  Greenwich Mean Time (GMT) Record  - modified version of ASP
 *---------------------------------------------------------------*/
typedef struct {		/* Greenwich Mean Time (GMT) record */
    int		yr;		/* GMT year */
    int		day;		/* GMT day */
    int		hr;		/* GMT hour */
    int		min;		/* GMT minute */
    double	second;		/* GMT second and millisecond */
} GMT;

extern	double	gmt_diff(const GMT *t1, const GMT *t2);
extern  void	gmt_add(GMT *gmt, double sec);

/*-------------------------*
 |  Pre-processing Region
 *-------------------------*/
typedef struct {	
    TapePos_t	start;		/* Address of first format */
    TapePos_t	end;		/* Address of last format */
    int		pass;		/* 'A' => ascending, 'D' => descending */
    void*	segmt;
    int 	frame_id;	/* recorded id # of the format */
    int		fmt_start;	/* Start format/cycle */
    int 	fmt_cnt;	/* # of formats/cycles occupied by image */
    GMT		gmt_start;	/* GMT at frame start */
    GMT		gmt_end;	/* GMT at frame end */
    GMT		gmt_center;	/* GMT at frame center */
    double 	ll_center[3];	/* lat/lon/radius at image center */
    double 	ll_1_near[3];	/* lat/lon/radius at near start corner */
    double 	ll_1_far[3];	/* lat/lon/radius at far start corner */
    double 	ll_N_near[3];	/* lat/lon/radius at near end corner */
    double 	ll_N_far[3];	/* lat/lon/radius at far end corner */
    double	pv_1_near[6];	/* pos/vel at near start corner */
    double 	pv_1_far[6];	/* pos/vel at far start corner */
    double 	pv_N_near[6];	/* pos/vel at near end corner */
    double 	pv_N_far[6];	/* pos/vel at far end corner */
    double 	pv_center[6];	/* pos/vel at image center */
    double	swth_speed;
    Range_t	range;		/* Slant range */

} PPR_t;

/*------------------------------*
 |  Segment == Series of Pulses
 *------------------------------*/
#define PULSE_PER_SEGMT_MIN     10000
#define	SEGMT_PER_READ_MAX	8


typedef struct {
    TapePos_t	start;		/* Address of first pulse in segment */
    TapePos_t	end;		/* Address of first bit beyond last frame */
    Mode_t	mode;		/* Instrument mode */
    short	reverse;
    short	pulse_err;	/* TRUE if has a pulse that's unusually long */
    int		type;		/* -1 => image data; Positive => cal. data */
    int		bit_error;	/* Bit error count of SYNC code */
    int		dwp_cnt;
    int		agc_cnt;
    int		ppr_cnt;
    int		burst_len;	/* Number of pulses in a burst */
    int		burst_cnt;
    int		frame_cnt;
    int		pulse_cnt;
    int		pulse_pcm;	/* Reference PCM GMT pulse */
    int		burst_0;	/* 1st processable burst - index into 'burst' */
    int		pulse_0;	/* 1st processable pulse - index into 'pulse' */
    Pulse_t**	pulse;
    int*	burst;
    int*	dwp;
    int*	agc;
    PPR_t*	ppr;
    double	cal;		/* Average caltone power */
    double	prf;
    double 	lookangle;
    double	hidelta;	/* Time spacing between high latitude frames */
    GMT		gmt_start;	/* Segment start time */
    GMT		gmt_end;	/* Segment start time */
    GMT		gmt_hilat;	/* GMT at low extremum */
    GMT		gmt_pcm;	/* Reference PCM GMT */

} Segmt_t;

/*-----------------------------------*
 |  Needed to support segment merge
 *-----------------------------------*/
typedef struct {
    int		blk_start;
    int		blk_cnt;
    void*	tapebuf;
    Segmt_t	segmt_free;
    int		merge_max;
    int*	merge_cnt;
    Segmt_t*	merge;
    int		segmt_cnt;
    Segmt_t	segmt[SEGMT_PER_READ_MAX];
    int		pulse_exist;

} Merge_t;

/*---------------------*
 |  Per-Scan Context
 *---------------------*/
#ifndef	SCAN_STATE_SIZE
#define	SCAN_STATE_SIZE		16
#endif

typedef struct {                /* Frame Scan Context Per Scan Thread */
    Frame_t     frame;
    int         i;
    int         n;
    int		len;
    U32*        buf;
    int         pcm_cnt;	/* # of times PCM clk risen hi in this pulse */
    int         blk_start;      /* Starting tape block number of tapebuf */

    /*  Reserved for and may be mapped to platform-specific data structure */
    double	sat_align;	/* So that 'sat' field is double-word aligned */
    int		sat[SCAN_STATE_SIZE];

} Scan_t;

/*-------------------*
 |  GHA Correction
 *-------------------*/
typedef struct {
    GMT		time;
    double	angle;
} GHA_t;

extern GHA_t	gha;		/* Used by utc2gha.f & Set by get_ref_gha_ */

#define LAT_MAX		900	/* Maximum number of center lat's per mode */

typedef struct {		/* Fixed Frame Center */
    Mode_t      mode;
    int         rmin;           /* Minimum revolution number */
    int         rmax;           /* Maximum revolution number */
    int         left;           /* True -> left looking, False -> right */
    int 	cnt;		/* Total number of latitudes in array */
    float	azim;		/* Image size azimuth in meters */
    int		pass[10];
    double      lat[LAT_MAX];	/* Fixed frame latitude array */

} FFC_t;

#define FFC_MAX         100	/* Max number of fixed frame tables */

extern	FFC_t	fix_ctr[FFC_MAX];

/*-------------------------------------*
 |  Statevector Propagation Structure
 *-------------------------------------*/

#define	STV_MAX		145	/* No. of predicted statevectors in minutes */
#define	STV_SPACING	((double) 60.0)	/* Propagation interval in seconds */

typedef struct {
    SV_Type_t	precision;
    SV_Coord_t	coord_sys;
    int		rev;
    GMT		t0;		/*  GMT of first statevector, sv[0] */
    double	sv[STV_MAX+5][6]; /* Predicted 1-min statevectors (pos, vel) */

} STV_t;
    
extern STV_t*  stv_init(STV_t*, GMT* t0, double sv0[6]);
extern double* stv_prop(const STV_t*, GMT* t1, double sv1[6]);

/*---------------------*
 |  Look Angle Tables
 *---------------------*/
#define	LOOKTABLE_MAX	4

typedef	struct {		/* Look angle table */
    int		rmin;		/* Minimum revolution number */
    int		rmax;		/* Maximum revolution number */
    double	beam[BEAM_MAX];	/* Look angles of all beams */
} LookTable_t;

extern	LookTable_t	looktable[LOOKTABLE_MAX];

/*------------------------------*
 |  Nominal Swath Width in Km.
 *------------------------------*/
extern	double	nominal_swath[BEAM_MAX];

/*----------------------------------------------------------------------------*
 | Effective window duration = Decoded window duration - window_duration_error
 *----------------------------------------------------------------------------*/
extern	double	window_duration_error;

/*--------------------*
 |  Roll, Yaw, Pitch 
 *--------------------*/
typedef struct {
    float roll;
    float yaw;
    float pitch;
} RYP;

extern	RYP*	get_RYP(const double sv[6], RYP* att);

/*-------------------------------------*
 |  Default processing parameter file
 *-------------------------------------*/
typedef struct {
    char	sat[3];		/* sensor name */
    int		ns_ra;		/* number of samples per range line */
    int		nfft_ra;	/* range fft length */
    int		fftsc_raf;	/* range forward fft scaling */
    int		fftsc_rai;	/* range inverse fft scaling */
    int		nchirp;		/* # of points in range chirp */
    int		nfft_az4;	/* azimuth forward fft length, 4 look */
    int		nfft_az1;	/* azimuth forward fft length, 1 look */
    float	bw_az4;		/* azimuth processor bandwidth/look */
    int		fftsc_az4f;	/* azimuth fwd fft scaling: 4 look */
    int		fftsc_az1f;	/* azimuth fwd fft scaling: 1 look */
    int		fftsc_az4i;	/* azimuth inv fft scaling: 4 look */
    int		fftsc_az1i;	/* azimuth inv fft scaling: 1 look */
    int		det_sc;		/* detector scaling */
    float	h_az;		/* azimuth weighting pedestal */
    float	a_prf;		/* prf conversion constant term */
    float	b_prf;		/* prf conversion linear term */
    float	a_dwp;		/* data window conversion const. term */
    float	b_dwp;		/* data window conversion linear term */
    float	fcnoise;	/* noise center frequency */
    float	bwnoise;	/* noise bandwidth */
    float	fcsig;		/* signal center frequency */
    float	bwsig;		/* signal bandwidth */
    int		pro_gain;	/* total gain through processor */

    /* clutter-lock routine parameters */
    int		clock_iter;	/* maximum clutter-lock iterations */
    float	fderror_max;	/* maximum total error to pass test */
    float	fdaerror_max;	/* maximum fda chng to try quadratic */
    float	fdberror_max;	/* maximum fdb chng to try quadratic */
    float	fdc_max;	/* maximum fdc allowed */

    /* processor gain and fft overflow parameters */
    float	gain_ref;	/* expected energy at standard gain */
    int		scale_threshold;/* fft overflow register threshold */

    /* lookup table generation routine parameters */
    float	lmean_tol;	/* I and Q mean value error tolerance */
    float	lstdv_diff_max;	/* max relative diff, I to Q std. dev.*/
    int		blkskip;	/* blocks to skip to find data */
    int		sbm;		/* single byte match */
    int		fsm;		/* frame sync byte match */
    int		maxfsdo;	/* max # of consecutive frames with */
				/*   missing sync codes.            */
    /* the next three factors are for the radiometric data record */
    float	noise_fctr;	/* noise scale factor constant */
    float	linear_fctr;	/* linear conversion factor constant */
    float	offset_fctr;	/* offset conversion factor constant */
    float	peak_ref;	/* peak reference */

} DPP_FILE;

/*--------------------------*
 |  Sensor parameters file
 *--------------------------*/
typedef struct {
    char	sat[3];		/* sensor name */
    double	xlambda;	/* radar wavelength */
    float	beam_el;	/* elevation beam width */
    float	beam_az;	/* azimuth beam width */
    char	side;		/* L (left) or R (right) side looking */
    float	gamma_nom;	/* look angle */
    double	csr;		/* complex sampling rate */
    float	incl;		/* nominal orbit inclination */
    float	ecc;		/* nominal orbit eccentricity */
    float	bw_ra;		/* bandwidth of transmitted chirp */
    float	tau;		/* transmitted pulse length (time) */
    int		nbits;		/* # of bits per I or Q sample */
    RYP		eatt;		/* extreme attitude roll, yaw, pitch */
    RYP		eattrt;		/* extreme attitude rates for each */
				/* replica minimum check values: */
    float	xlocation;	/* peak location */
    float	x3db;		/* 3db signal width */
    float	xpslr;		/* peak side lobe ratio */
    float	xislr;		/* integrated side lobe ratio */
    float	pbw;		/* total processor bandwidth */
    float	dbw;		/* Antenna azimuth pattern b/w */
    double	srspace;	/* slant range pixel spacing */
    double	grspace;	/* ground range pixel spacing */
    float	ant_len;	/* azimuth antenna length */
    float	minsnr;		/* minimum acceptable snr (dB) */
    float	est_noise_floor;/* estimated noise floor */
    float	noise;		/* noise when no pre or post */

} SP_FILE;

/*--------------------------------*
 |  Range transfer function file
 *--------------------------------*/
typedef struct {
    int		npts;		/* # of points in transfer function */
    float	tau;		/* pulse duration */
    double	csr;		/* complex sampling rate */
    float	a;		/* secondary range compression coeff. */
    float	b;		/* linear chirp phase coefficient */
    float	c;		/* quadratic chirp phase coefficient */
    float	powmax;		/* maximum power t.f. element */
    float	avgpow;		/* average power in transfer function */
    float	h;		/* range weighting pedestal */
    float	powloss;	/* power loss in weighting */

} RTF_FILE;

/*--------------------*
 |  Earth model file
 *--------------------*/
typedef struct {
    char	earth_mod[25];	/* name of earth model */
    char	emod_src[25];	/* source of the model */
    double	r_e;		/* radius of earth at equator */
    double	f;		/* flattening factor */
    double	r_pole;		/* radius of earth at pole */

} EM_FILE;

/*----------------------------*
 |  Time Correlation Element
 *----------------------------*/
typedef struct {		/* time correlation element */
    int		rev;		/* orbit number */
    GMT		gmt;		/* Greenwich mean time */
    unsigned 	bt;		/* satellite binary time */
    int		delta;		/* clock period of satellite clock */
				/*  for J-ERS-1, time error in mlsc */
} TC_FILE;

/*-----------------------------*
 |  Job Processing Parameters
 *-----------------------------*/
typedef struct Job_t {
    STV_t	stv;
    TC_FILE	tcf;		/* Time correlation */
    double	e_win_duration;
    double	e_dwp_a;
    double	e_dwp_b;
    double	j_win_duration;
    double	j_dwp_a;
    double	j_dwp_b;

    void*	sat;		/* Satellite to process - Set by RDS_Init() */
    void*	sat_data;	/* Set & used by satellite specific code */
    U32*	pcm;		/* PCM buffer */
    U8*		(*Buf)(void** tapebuf, int blk_start, int bit_start);
    int		(*Log)();	/* Error log routine */
    void*	err;		/* Error handle */
    void*	msg;		/* Original processing request */

    /* Input required for PPR corner calculation */
    
    GMT		start_time;     /* Start time of datatake or frame*/
    GMT		end_time;       /* End time of datatake or frame*/
    GHA_t	gha;		/* GHA correction */
    Mode_t	mode;           /* Instrument mode */
    int		scan_job;	/* TRUE => scan job, FALSE => decode job */
    double	lat_error;	/* Max latitude error (in seconds) */
    int		min_burst_swa;
    int		min_burst_swb;
    int		min_burst_sna;
    int		min_burst_snb;

    /* Required input source */

    Mode_t	exp_beam[4];    /* experimental beam mapping */
    char	platform[4];	/* Type of satellite */
    int		mode_check;	/* TRUE => Enable mode check, FALSE => no */
    int		tape_check;	/* TRUE => Enable media check, FALSE => no */
    int		media_check;	/* TRUE => Check tape label, FALSE => ignore */
    char	media_type[8];	/* DCRSI, ID-1, or DISK */
    char	media_id[16];	/* Tape id if media_type != DISK */
    char	media_loc[256];	/* Disk file location if media_type == DISK */
    int		dsk_start;	/* Tape block address of start of disk file */
    int		blk_size;	/* Block size in bytes */
    int		reverse;
    TapePos_t	start;		/* First tape address to begin process */
    TapePos_t	end; 		/* Last tape address to process >= blk_start */

    /* Input required for a SCAN or DECODE request */
    int		max_burst_per_image;
    int		min_pulse_per_read;

    int		e_sync_diff;	/* Max corrupted frame SYNC bits for ERS */
    int		j_sync_diff;	/* Max corrupted frame SYNC bits for JERS */
    int		r_sync_diff;	/* Max corrupted frame SYNC bits for Radarsat */
    int         sync_diff;	/* Max no. of corrupted frame SYNC bits */
    int         tail_diff;	/* Max no. of corrupted tail SYNC bits */
    int		eco_pad;	/* Max no. of bytes padded per short pulse */
    int         pcm_diff;	/* Max no. of corrupted PCM SYNC bits */
    int         aux_diff;	/* Max no. of corrupted AUX SYNC bits */
    int         aux_recover;	/* Will attempt to recover AUX sync if <= */
    int		gmt_diff_max;	/* Max aberation from job start/end times (s) */
    int         dcrsi_csize;    /* 7 or 8 bits on CMD serial channel */
    int         seek_timeout;	/* each retry time out in secs. */
    int         seek_retry;	/* how many retries */
    int		tape_errlim;	/* OK if read failed within last N blocks */
    double      aux_tdelta;     /* max spacecraft time delta (secs.) */
    char	sca_file[256];	/* Scan results file name */

    /* Input required for a DECODE request */

    int		frame_id;	/* Frame number to decode */
    char	output_fmt[6];	/* Output format */
    char	aux_file[256];	/* Auxiliary data file name */
    char	eco_file[256];	/* Echo data file name */
    char	rep_file[256];	/* Replica data file name */
    char	eph_file[256];	/* Ephemeris data file name */
    char	bof_file[256];	/* Burst offset data file name */

    /* Not required for PPR corner calculations but part of proc. request */
    
    char	src[16];	/* Real CP name */
    char	dst[16];	/* Our real name */
    int		job_id;		/* Job ID */
    int		sensor;         /* Sensor type */
    int		rev;		/* Revolution */
    int		seq;		/* Sequence */
    char	station[6];	/* Station ID */
    char	activity[4];	/* Activity ID */
    char	recorder[18];	/* Recorder ID */
    char	frame_mode[10];	/* Frame mode */
    char	site_name[32];	/* Site name */

} Job_t;

/*-------------------*
 |  Satellite Class
 *-------------------*/
typedef	const Pulse_t* const*	PulseList_t;

typedef struct {
    int		frame_size_min;	/* Shortest frame length in bytes */
    int		frame_size_max;	/* Longest frame length in bytes */
    int		frame_size;	/* Nominal frame length in bytes */
    int		data_size;	/* Nominal data length */
    int		header_size;	/* Nominal header length */
    int		frame_per_pulse_min;
    int		frame_per_pulse_max;
    const U32*	PRN[4];
    float	fd3p[3];	/* Default zero doppler parameter value */
    float	fr3p[3];	/* Default zero doppler rate value */
    void*	(*Init)(Job_t*);
    void	(*Destroy)(Job_t*);
    int		(*Decode)(Segmt_t*, int fr_burst, int to_burst, int eos,
			  const Job_t*);
    Frame_t*	(*Scan)(Scan_t*, const Job_t*);
    Frame_t*	(*NxtFrame)(Scan_t*, const Job_t*, const Pulse_t* prev);
    int		(*NewPulse)(Pulse_t*, Scan_t*, Segmt_t*, const Job_t*,
			    const Pulse_t* prev);
    int		(*ExtraPulse)(Segmt_t*, Pulse_t** free_pulse, size_t free_cnt,
			      const Job_t*, const int has_tapebuf);
    int		(*SameDWP)(const Pulse_t*, const Pulse_t*);
    int		(*SameAGC)(const Pulse_t*, const Pulse_t*);
    int		(*GMT)(GMT*, PulseList_t, Segmt_t*, const Job_t*);
    int		(*DWP)(double* dwp_in_secs, PulseList_t, const Segmt_t*,
		       const Job_t*);
    int		(*AGC)(PulseList_t, const Segmt_t* sp, const Job_t*);
    RYP*	(*RYP)(const double sv[6], RYP* att);
    double	(*PRF)(PulseList_t, const Segmt_t* sp, const Job_t*);
    double	(*LookAngle)(PulseList_t, const Segmt_t*, const Job_t*);
    double	(*WDuration)(PulseList_t, const Segmt_t*, const Job_t*);
    int		(*CAL)(double*, Segmt_t*, const Job_t*);
    Mode_t	(*Mode)(const Segmt_t*, const Job_t*);
    int 	(*Beam)(const Pulse_t*);
    int		(*SameBurst)(const Pulse_t*, const Pulse_t*);
    int		(*PulseCnt)(const Pulse_t*);

} Satellite_t;
    
/*----------------------------------------------------------*
 |  Count the number of 1-bits given an 8-bit index value.
 |  This table was generated by InitBitDiffer().
 *----------------------------------------------------------*/
extern	int	diff[256];

#define DIF(_x,_sync,_zp) \
	(*(_zp) = (_x) ^ (_sync),\
	 *(_zp) = diff[((U8*)_zp)[0]] + diff[((U8*)_zp)[1]]+\
		  diff[((U8*)_zp)[2]] + diff[((U8*)_zp)[3]] )
#ifndef RoundUp
#define RoundUp(_x,_n)	(((_x + ((_n)-1)) / (_n)) * (_n))
#endif

#ifdef  NOSTDARG
extern	int		strcmpv();
#else
extern	int		strcmpv(char *s0, char* s1, ...);
#endif
extern	U8* 		Unscramble(const U32** prn, U32* buf, U32 bit,
				   U32 start, size_t len);
extern	void		Scan_Position(TapePos_t* pos, const Scan_t* fs,
				   const Job_t*);
extern	Merge_t*	Scan(Merge_t* mp, const Job_t*);
extern	Merge_t*	Merge(Merge_t* mp, const Job_t*);
extern	int		Segmt_cmp(const Merge_t* m0, const Merge_t* m1);
extern	int		Merge_cmp(const Segmt_t* ss, const Segmt_t* ds);
extern	int		Segmt_Save(Segmt_t* ds, size_t ds_cnt,
			    char* buf, const Job_t*);
extern	void		get_ref_gha_(int *yr, int *day, int *hr, int *min,
			    double *sec, double *ang);
extern	void		fit_dops(double xi[3][3], float *y);
extern	int		get_speed(int left_look, double sv[6], RYP*, Range_t*,
			    double* v_swath, float fd[3], float fdot[3]);
extern	Range_t*	get_range(double sv[6], double prf, double angle,
			    double delay, double wdlen, Range_t* r);
extern	int		get_pass(double* pos_near_start, double* pos_center,
			    double* pos_far_end);
extern	PPR_t*		PPR_first(PPR_t*, Segmt_t*, const Job_t*);
extern	PPR_t*		PPR_next(PPR_t* new, PPR_t* prev, const Job_t*);
extern	FFC_t*		GetFrame(Mode_t, const Job_t*);

#define BLKNUM(_blk,_sp,_job) \
	((_sp)->reverse ? ((_job)->end.blk+(_job)->start.blk-(_blk)) : (_blk))
#define BITNUM(_bit,_sp,_job) \
	((_sp)->reverse ? ((_job)->blk_size*8-1-(_bit)) : (_bit))

#endif /*!_RDS_PULSE_H_ */
