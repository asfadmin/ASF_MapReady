/*============================================================================
 |  @(#)R1.c	1.121 98/04/06 10:11:57
 |
 |  RadarSAT (R1) Pulse Object Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include <syslog.h>
#include <stdlib.h>
#include <errno.h>
#include "R1.h"
#include "version.h"

static const char sccsid_R1_c[] =
        "@(#)R1.c	1.121 98/04/06 10:11:57";

/*------------------------------------------------*
 |  Constants Used by R1_Scan() & R1_NextFrame()
 *------------------------------------------------*/
#define R32(_x)			RoundUp(_x, sizeof(int))
#define FIRST_BIT               ((R32(R1_FRAME_SIZE)-R1_FRAME_SIZE)*8 + 1) 
#define M_DATA  		0x07FE0000U
#define V_DATA  		0x07FE0000U
#define M_SCID  		0x0FF00000U
#define V_SCID  		0x0C900000U
#define M_AUX   		0x60000

static	U8 map[64*1024];        /* Frame Sync Look-up Table */

/*-----------------------------------*
 |  PRN Table Used for Unscrambling
 *-----------------------------------*/
static const U8 prn_0[] = {
#include "R1_prn.h"
,0};
static const U8 prn_1[] = {0,
#include "R1_prn.h"
};
static const U8 prn_2[] = {0,0,
#include "R1_prn.h"
,0,0,0};
static const U8 prn_3[] = {0,0,0,
#include "R1_prn.h"
,0,0};

/*----------------------------------------*
 |  The following are used for decoding
/*----------------------------------------*/

#define	BURST_PER_IMAGE_MAX	1600
#define REP_PER_BURST_SIZE      (8 * 1440 + 32)
#define EPH_PER_BURST_SIZE      (2 * 512)
#define AUX_PER_BURST_SIZE      2048
#define ECO_PER_BURST_SIZE      \
(R1_PULSE_PER_BURST_MAX * R1_FRAME_PER_PULSE_MAX * (R1_FRAME_SIZE-12) + 32)

#define	REPLICA_NBIN		10
#define	REPLICA_NFFT		1024
#define	REPLICA_LINE_MAX	2

typedef struct RefPulse_t {
    int	num;
    int val;

} RefPulse_t;

typedef struct {
    int		(*type)();	/* Message type */
    int  	offset;		/* Byte offset to BOGUS_COUNT */
    int         fd;             /* File descriptor */
    int		bad;
    RefPulse_t*	ref;		/* Point to 'ref_pulse' in Decode_t */
    char*       file;           /* Output filename */
    char*       buf;		/* Write cache */
    size_t      bufLen;		/* Current cache usage */
    size_t      bufSize;	/* Maximum cache size */
    U8*		eco_valid;	/* Point to 'eco_valid' in Decode_t */
    U8*		rep_valid;	/* Point to 'rep_valid' in Decode_t */
    U32*	burst_offset;	/* Point to 'burst_offset' in Decode_t */
    Complex_t*	workrep;	/* Point to 'workrep' in Decode_t */
    Complex_t*	rep;		/* Point to 'rep' in Decode_t */

} Writer_t;

typedef struct {
    Writer_t	ecoWrt;
    Writer_t	repWrt;
    Writer_t	auxWrt;
    Writer_t	ephWrt;
    char*       sca_cache;
    char*       eco_cache;
    char*       rep_cache;
    char*       aux_cache;
    char*       eph_cache;
    RefPulse_t	ref_pulse;
    U8  	eco_valid[BURST_PER_IMAGE_MAX];
    U8  	rep_valid[BURST_PER_IMAGE_MAX];
    U32 	burst_offset[BURST_PER_IMAGE_MAX+1];
    Complex_t	workrep[REPLICA_NFFT+15];
    Complex_t	rep[REPLICA_NBIN];

} Decode_t;

#define	CALTONE_NBIN		10
#define	CALTONE_NFFT		8192
#define	CALTONE_LINE_MAX	10
#define	CALTONE_FREQ		(SMO/40)	/* 1/4 of sampling rate */

static Complex_t zero = {0.0, 0.0};
static Complex_t voltage[] = {
#include "R1_cal.h"
};

/* Calibration Attenuator Setting in dB */
static const double cal_gain[16] = {
    /* 0000 */  0.0,
    /* 0001 */  0.0, 		/* invalid entry */
    /* 0010 */ 24.4,
    /* 0011 */ 48.8,
    /* 0100 */ 12.2,
    /* 0101 */  0.0,		/* invalid entry */
    /* 0110 */ 36.6,
    /* 0111 */ 61.0,
    /* 1000 */  6.1,
    /* 1001 */  0.0,		/* invalid entry */
    /* 1010 */ 30.5,
    /* 1011 */ 54.9,
    /* 1100 */ 18.3,
    /* 1101 */  0.0,		/* invalid entry */
    /* 1110 */ 42.7,
    /* 1111 */ 67.1
};
/* LPT Power Setting in dB */
static const double lpt_gain[16] = {
    /* 0000 */  0.0,		/* invalid entry */
    /* 0001 */  0.0, 		/* invalid entry */
    /* 0010 */ 25.0,
    /* 0011 */ 25.5,
    /* 0100 */ 26.0,
    /* 0101 */ 26.5,		/* invalid entry */
    /* 0110 */ 27.0,
    /* 0111 */ 27.5,
    /* 1000 */ 28.0,
    /* 1001 */ 28.5,		/* invalid entry */
    /* 1010 */ 29.0,
    /* 1011 */ 29.5,
    /* 1100 */ 30.0,
    /* 1101 */  0.0,		/* invalid entry */
    /* 1110 */  0.0,		/* invalid entry */
    /* 1111 */  0.0		/* invalid entry */
};

#define SMO                     ((double) 129.2683E6)
#define SAMPLING_FREQ(p)        ((p)->aux.sampling_rate == 0 ? SMO/4 :  \
                                 (p)->aux.sampling_rate == 1 ? SMO/7 : SMO/10)
#define TIME_UNIT(p)            (6 / SAMPLING_FREQ(p))
#define	EU_Error(_c)		((1.0681e-5*(_c) - 1.75e-1) * DEG_PER_RAD)
#define EU_Rate(_c)		((1.0681e-6*(_c) - 1.75e-2) * DEG_PER_RAD)
#define CAL_GAIN(p)             cal_gain[(p)->aux.CAL_attenuation & 0xF]
#define	Beam(p)			(p)->aux.beam_select
#define AGC(p)			(p)->aux.AGC_setting
#define	DWP(p)	(((p)->aux.window_start_msb<<4) | (p)->aux.window_start_lsb)

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static	const int repAGC[] =	{ 0, 4, 2, 6, 1, 5, 3, 7 };
#define ReplicaAGC(p) 		\
(repAGC[(p)->aux.replica_AGC >> 3] | (repAGC[(p)->aux.replica_AGC & 7] << 3))

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static const U32 beamIDMap[16] = { 0,0,0,0,0, 5,6,7,1,2,3, 2,0,0,0,0 };
#define BeamID(_p) \
beamIDMap[((_p)->aux.beam_sequence >> ((3-(_p)->aux.beam_select)*4)) & 0xF]

/*----------------------------------------------------------------------------*
 |  NAME
 |	R1_PRN_Init - generate a sequence of PRN bits
 |
 |  SYPNOSIS
 |	void R1_PRN_Init(U8* prn, int length);
 |
 |  DESCRIPTION
 |	Generate a sequence of '8*length' PRN bits.  The first 80 bits
 |	are 0 as they correspond to the unencoded frame header.
 *----------------------------------------------------------------------------*/
static
void R1_PRN_Init(U8* prn, int length)
{
    register int i,j, seed = 0xFF, k;
    static int xor[] =
    {
	/* 0000 */  0x000,
	/* 0001 */  0x100,
	/* 0010 */  0x100,
	/* 0011 */  0x000,
	/* 0100 */  0x100,
	/* 0101 */  0x000,
	/* 0110 */  0x000,
	/* 0111 */  0x100,
	/* 1000 */  0x100,
	/* 1001 */  0x000,
	/* 1010 */  0x000,
	/* 1011 */  0x100,
	/* 1100 */  0x000,
	/* 1101 */  0x100,
	/* 1110 */  0x100,
	/* 1111 */  0x000,
    };
    /*  First 10 bytes of frame are unencoded. */
    for (i = 0; i < 10; ++i)
	prn[i] = 0;

    /* For each encoded byte in frame, do: */
    for (i = 10; i < length; ++i) {
	prn[i] = 0;
	for (j = 0; j < 8; ++j) {
	    seed = (k = seed) >> 1;
	    seed |= xor[((seed>>3)&0xE)+(seed&1)];
	    prn[i] = (k >> 8) | (prn[i] << 1);
	}
    } /* End of each byte */
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Frame_t* R1_Scan(Scan_t* p, const Job_t* job)
{
    register int n = p->n, i = p->i;
    register U32 y, *w = p->buf;
    register U32 x = !n ? w[i] : (w[i] & (0xFFFFFFFFU >> n));

#ifdef	DEBUG
    printf("R1_Scan %d: i %d, n %d, len %d\n", p->blk_start, p->i, p->n,p->len);
#endif
    while (i <= p->len) {
        if (n = map[x >> 16]) {
	    if (n == 16) {
		if (x == R1_SYNC) {
		    n = 0;
		    break;
		}
	    }
	    else if ((y = (x>>(16-n))|(w[i-1]<<(16+n))) == R1_SYNC) {
		--i;
		n += 16;
		x = y;
		break;
	    }
	}
        if ((n = map[x & 0xFFFFU]) &&
	    (y = (x<<n)|(w[i+1]>>(32-n))) == R1_SYNC) {
            x = y;
            break;
        };
	n = 0;
        x = w[++i];
    }
    if (i > p->len || (i == p->len && n >= FIRST_BIT)) {
	if (p->i < p->len || (p->i == p->len && p->n < FIRST_BIT)) {
	    p->n = n;
	    p->i = i;
	    Scan_Position(&p->frame.start, p, job);
	}
	return NULL;
    }
    y = 32-n;
    p->n = n;
    p->i = i;
    p->frame.bit_error = 0;
    p->frame.frame_cnt = !((!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>y))) & M_AUX);
    Scan_Position(&p->frame.start, p, job);

#ifdef	DEBUG
printf("New Frame: <%d, %d>, i %d, n %d%s\n",
	p->frame.start.blk, p->frame.start.bit, p->i, p->n,
	p->frame.frame_cnt ? " ++++++++ PULSE +++++++++++" : "");
#endif
    return (&p->frame);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
Frame_t* R1_NextFrame(Scan_t* p, const Job_t* job, const Pulse_t* pr)
{
    register int i, n;
    register U32 *w = p->buf;
    U32 z, bit_error = 0;

    i = p->i + ((n = p->n + R1_FRAME_SIZE*8) >> 5);
    n &= 0x1F;
    if (n) {
	register int y = 32 - n;

	if (i > p->len || (i == p->len && n >= FIRST_BIT)
	||  ((z = (w[i]<<n)|(w[i+1]>>y)) != R1_SYNC &&
	     DIF(z, R1_SYNC, &bit_error) > job->sync_diff)
	){
	    if (p->i < p->len || (p->i == p->len && p->n < FIRST_BIT)) {
		p->n = n;
		p->i = i;
		Scan_Position(&p->frame.start, p, job);
	    }
	    return NULL;
	}
	p->frame.frame_cnt = !(((w[i+1] << n) | (w[i+2] >> y)) & M_AUX);
    }
    else {
	if (i > p->len || (i == p->len && n >= FIRST_BIT)
	||  ((z = w[i]) != R1_SYNC &&
	     DIF(z, R1_SYNC, &bit_error) > job->sync_diff)
	){
	    if (p->i < p->len || (p->i == p->len && p->n < FIRST_BIT)) {
		p->n = n;
		p->i = i;
		Scan_Position(&p->frame.start, p, job);
	    }
	    return NULL;
	}
	p->frame.frame_cnt = !(w[i+1] & M_AUX);
    }
    p->i = i;
    p->n = n;
    Scan_Position(&p->frame.start, p, job);
    p->frame.bit_error = bit_error;

#ifdef DEBUG
printf("NextFrame: <%d, %d>, i %d, n %d%s\n",
	p->frame.start.blk, p->frame.start.bit, p->i, p->n,
	p->frame.frame_cnt ? " ++++++++ PULSE +++++++++++" : "");
#endif
    return (&p->frame);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_NewPulse(Pulse_t* pp, Scan_t* fp, Segmt_t* sp, const Job_t* job,
		const Pulse_t* pr)
{
#ifdef DEBUG
    static U32 seq = 0, mode = 0, stat = 0, numb = 0, pul1 = 0, pul2 = 0;
#endif
    if (! pp) {
	register int  nl = fp->n + (R1_HDR_SIZE << 3);
	register U32* bp = fp->buf + fp->i + (nl >> 5);
	const    U32* rp = R1.PRN[fp->n >> 3] + (nl >> 5);
	register int  nr = 32 - (nl &= 0x1F);
	register int  sl = nl & ~7, sr = 32-sl;
	register U32  calstat, imgstat, pulse_1, n_beams, d1, d0;
	const U32 sync = (nl ? ((bp[0]<<nl) | (bp[1]>>nr)) : bp[0])
		       ^ (sl ? ((rp[0]<<sl) | (rp[1]>>sr)) : rp[0]);
	U32 z;
	if (!pr ? (sync != R1_AUX) :
	    (pr->frame_cnt < R1_FRAME_PER_PULSE_MIN ||
		(sync != R1_AUX && (DIF(sync,R1_AUX,&z) > job->aux_diff &&
		   (!fp->frame.frame_cnt ||
		    (d1 = (((nl ? ((bp[10]<<nl)|(bp[11]>>nr)) : bp[10]) ^
			    (sl ? ((rp[10]<<sl)|(rp[11]>>sr)) : rp[10]))&0xFF),
		     d0 = (((nl ? ((bp[11]<<nl)|(bp[12]>>nr)) : bp[11]) ^
			    (sl ? ((rp[11]<<sl)|(rp[12]>>sr)) : rp[11]))>> 29),
		     d1 != ((R1_Pulse_t*)pr)->aux.sc_days_msb ||
		     d0 != ((R1_Pulse_t*)pr)->aux.sc_days_lsb))
		))
	    ))
	    return 0;
	/* Want only Payload Mode == Image, Image Status == Executing Image */

	imgstat = (nl ? ((bp[2] << nl) | (bp[3] >> nr)) : bp[2])
		^ (sl ? ((rp[2] << sl) | (rp[3] >> sr)) : rp[2]);
	calstat = (imgstat >> 23) & 0xF;
	imgstat = (imgstat >> 27);
	pulse_1 = (nl ? ((bp[5] << nl) | (bp[6] >> nr)) : bp[5])
		^ (sl ? ((rp[5] << sl) | (rp[6] >> sr)) : rp[5]);
	n_beams = (pulse_1 >> 14) & 0x3;
	pulse_1 = (pulse_1 & 0xFF);
	return ((imgstat & 0x13) != 0x11 
		&& pr && imgstat == (((U32*) &((R1_Pulse_t*)pr)->aux)[2] >> 27) 
		? -2
		: (pulse_1 ? -1 :
		  (n_beams ? -2 :
		  (calstat == 1 || calstat == 2 ? calstat : -1))));
    }
    memcpy( &((R1_Pulse_t*)pp)->aux,
	Unscramble(R1.PRN, fp->buf + fp->i, fp->n, R1_HDR_SIZE,
		   job->scan_job ? R1_AUX_SIZE : R1_DATA_SIZE),
	R1_AUX_SIZE);

    pp->frame_cnt = 1;
    pp->start = fp->frame.start;
    pp->tapebuf = fp->frame.tapebuf;
    pp->bit_error = fp->frame.bit_error;

#ifdef DEBUG
    if (seq != ((R1_Pulse_t*)pp)->aux.beam_sequence ||
	mode != ((R1_Pulse_t*)pp)->aux.payload_mode ||
	stat != ((R1_Pulse_t*)pp)->aux.image_status ||
	numb != ((R1_Pulse_t*)pp)->aux.number_of_beams ||
	pul1 != ((R1_Pulse_t*)pp)->aux.pulse_cnt1) {

	seq = ((R1_Pulse_t*)pp)->aux.beam_sequence;
	mode = ((R1_Pulse_t*)pp)->aux.payload_mode;
	stat = ((R1_Pulse_t*)pp)->aux.image_status;
	numb = ((R1_Pulse_t*)pp)->aux.number_of_beams;
	pul1 = ((R1_Pulse_t*)pp)->aux.pulse_cnt1;
	
    printf("%d/%.5d: seq %x, mode %d, stat %d, nbeam %d, cnt1 %d, cnt2 %d\n",
	pp->start.blk, pp->start.bit,
	((R1_Pulse_t*)pp)->aux.beam_sequence,
	((R1_Pulse_t*)pp)->aux.payload_mode,
	((R1_Pulse_t*)pp)->aux.image_status,
	((R1_Pulse_t*)pp)->aux.number_of_beams,
	((R1_Pulse_t*)pp)->aux.pulse_cnt1,
	((R1_Pulse_t*)pp)->aux.pulse_cnt2);
    }
#endif
    return -1;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
void BreakPulse(Pulse_t* pp_new, Pulse_t* pp, int frame_cnt,
		const Job_t* job, const int has_tapebuf)
{
    register int n;
    register U8* buf;

    *pp_new = *pp;
    pp->frame_cnt = frame_cnt;
    pp_new->frame_cnt -= frame_cnt;
    n = pp->start.bit + frame_cnt * R1_FRAME_SIZE*8;
    pp_new->start.blk += n / (job->blk_size*8);
    pp_new->start.bit  = n % (job->blk_size*8);
    pp_new->bit_error  = 0;

    buf = has_tapebuf
	? (*job->Buf)(&pp_new->tapebuf, pp_new->start.blk, pp_new->start.bit)
	: NULL;
    memcpy(
	&((R1_Pulse_t*)pp_new)->aux,
	job->scan_job
	    ? (buf ? (void*) Unscramble(R1.PRN, (U32*)((U32)buf & (~3)),
				((U32)buf & 3)*8 + (pp_new->start.bit & 7),
				R1_HDR_SIZE, R1_AUX_SIZE)
		   : &((R1_Pulse_t*)pp)->aux)
	    : (buf + R1_HDR_SIZE),
	R1_AUX_SIZE
    );
    if (! buf && job->scan_job) {
	/*  Zero out the S/C time so we can later interpolate its GMT */
	memset(((U8*) &((R1_Pulse_t*)pp_new)->aux) + 43, 0, 6);
    }
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int R1_ExtraPulse(Segmt_t* sp, Pulse_t** free_pulse, size_t free_cnt,
		  const Job_t* job, const int has_tapebuf)
{
    register int i;
    register Pulse_t* pp = sp->pulse[sp->pulse_cnt-1];
    if (free_cnt == 0)
	return -1;

    if (pp->frame_cnt <= R1_FRAME_PER_PULSE_MAX)
	return 0;

#ifdef	DEBUG
    printf("---------- Pulse %d/%d/%.5d has %d frames <0> %dB/%dP -------\n",
	pp->start.blk-job->dsk_start, pp->start.blk, pp->start.bit, 
	pp->frame_cnt, sp->burst_cnt, sp->pulse_cnt);
#endif
    for (i = sp->pulse_cnt-4; i >= 0; --i)
	if (sp->pulse[i]->frame_cnt + sp->pulse[i+1]->frame_cnt +
	    sp->pulse[i+2]->frame_cnt == pp->frame_cnt)
	    break;

    if (i >= 0) {
	if (free_cnt <= 1)
	    return -1;

	BreakPulse(free_pulse[0], pp, sp->pulse[i]->frame_cnt,
		   job, has_tapebuf);
	pp = free_pulse[0];
	BreakPulse(free_pulse[1], pp, sp->pulse[i+1]->frame_cnt,
		   job, has_tapebuf);
	return 2;
    }

    for (i = sp->pulse_cnt-3; i >= 0; --i)
	if (sp->pulse[i]->frame_cnt + sp->pulse[i+1]->frame_cnt ==
	    pp->frame_cnt)
	    break;
    if (i >= 0) {
	BreakPulse(free_pulse[0], pp, sp->pulse[i]->frame_cnt,
		   job, has_tapebuf);
	return 1;
    }
    if (!(sp->burst_len > 1 &&
	  pp->frame_cnt >  R1_FRAME_PER_PULSE_MAX &&
	  pp->frame_cnt <= R1_FRAME_PER_PULSE_MAX*2))
	return 0;

    if (sp->pulse_cnt - sp->burst[sp->burst_cnt-1] == sp->burst_len) {
        register Pulse_t* pp2 = sp->pulse[sp->pulse_cnt-3];
        register Pulse_t* pp1 = sp->pulse[sp->pulse_cnt-2];

        for (i = sp->burst[sp->burst_cnt-1]; i < sp->pulse_cnt-3; ++i) {
            if (sp->pulse[i  ]->frame_cnt == pp2->frame_cnt &&
                sp->pulse[i+1]->frame_cnt == pp1->frame_cnt) {

                BreakPulse(free_pulse[0], pp, sp->pulse[i+2]->frame_cnt,
			   job, has_tapebuf);
                return 1;
            }
        }
    }
    for (i = sp->pulse_cnt-2; i >= sp->burst[sp->burst_cnt-1]; --i)
	if (abs(sp->pulse[i]->frame_cnt*2 - pp->frame_cnt) == 1) {
	    BreakPulse(free_pulse[0], pp, sp->pulse[i]->frame_cnt,
		       job, has_tapebuf);
	    return 1;
	}
#ifdef DEBUG
    printf("---------- Pulse %d/%d/%.5d has %d frames %dP/%dB/%d/%d/%d -----\n",
pp->start.blk-job->dsk_start, pp->start.blk, pp->start.bit, pp->frame_cnt,
sp->pulse_cnt, sp->burst_cnt,
sp->pulse_cnt-sp->burst[sp->burst_cnt-1], sp->burst[sp->burst_cnt-1],
((R1_Pulse_t*) pp)->aux.pulse_cnt1);
#endif
    return 0;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_PulseCnt(const Pulse_t* obj)
{
    register int n = ((R1_Pulse_t*) obj)->aux.pulse_cnt1;
    return (n && !((R1_Pulse_t*) obj)->aux.payload_stripmap ? (n+1) : 1);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_Beam(const Pulse_t* obj)
{
    return (! ((R1_Pulse_t*)obj)->aux.number_of_beams
	    ? (((R1_Pulse_t*)obj)->aux.beam_sequence & 0x1F)
	    : BeamID((R1_Pulse_t*)obj));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |      R1_Mode - determine spacecraft mode from data
 |
 |  SYPNOSIS
 |      Mode_t R1_Mode(Segmt_t* sp, const Job_t* job)
 |
 |          sp  - segment pointer
 |          job - ptr to job parameter structure
 |
 |          returns - spacecraft mode
 |                    else MODE_INVALID if error
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
Mode_t R1_Mode(const Segmt_t* sp, const Job_t* job)
{
    register int i;
    const R1_Pulse_t** pp = (const R1_Pulse_t**) sp->pulse;

    if (! pp) return MODE_INVALID;

    for (i = 2; i < sp->pulse_cnt; ++i)
	if (pp[i-2]->aux.beam_sequence == pp[i-1]->aux.beam_sequence &&
	    pp[i-2]->aux.number_of_beams == pp[i-1]->aux.number_of_beams &&
	    pp[i-2]->aux.beam_select == pp[i-1]->aux.beam_select &&
	    pp[i-1]->aux.beam_sequence == pp[i]->aux.beam_sequence &&
	    pp[i-1]->aux.number_of_beams == pp[i]->aux.number_of_beams &&
	    pp[i-1]->aux.beam_select == pp[i]->aux.beam_select)
	    break;
    if (i == sp->pulse_cnt)
	return MODE_INVALID;

    switch (pp[i]->aux.number_of_beams) {
    case 0:
	i = pp[i]->aux.beam_sequence & 0x1F;
	if (i >= 1 && i <= 7)
	    return (ST1 + i-1);
	if (i >= 8 && i <= 10)
	    return (WD1 + i-8);
	if (i == 11)
	    return (WD2);
        /*---------------------------------------------------*
         |  For the experimental beam slots use the mapping
         |  table to determine the beam.
         *---------------------------------------------------*/
	if (i >= 12 && i <= 15)
	    return (job->exp_beam[i-12]);
	if (i >= 16 && i <= 20)
	    return (FN1 + i-16);

	return MODE_INVALID;	
    case 1:
	return (sp->burst_cnt >= 2 ? SNA : MODE_INVALID);
    case 2:
	return (sp->burst_cnt >= 3 ? SNB : MODE_INVALID);
    case 3:
        for (i = 0; i < sp->burst_cnt; ++i) {
	    int beam = BeamID(pp[sp->burst[i]]);
	    if (beam == 3 || beam == 7)
		return (sp->burst_cnt >= 4 ? SWA : MODE_INVALID);
	    else if (beam == 5 || beam == 6)
		return (sp->burst_cnt >= 4 ? SWB : MODE_INVALID);
	}
    }
    return MODE_INVALID;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |	Return the number of replica samples contained in pulse 'p'
 *---------------------------------------------------------------------------*/
int ReplicaSamples(const R1_Pulse_t** pp, const Segmt_t* sp)
{
    register int k, i =
	(Pulse_t**) pp == sp->pulse ? 1 :
       ((Pulse_t**) pp == sp->pulse + sp->pulse_cnt - 1 ? -1 : 0);

    if (! pp[0]->aux.replica_present)
	return 0;

    k = pp[i]->aux.sampling_rate == pp[i+1]->aux.sampling_rate ||
	pp[i]->aux.sampling_rate == pp[i-1]->aux.sampling_rate ? i :
       (pp[i+1]->aux.sampling_rate == pp[i-1]->aux.sampling_rate ||
       (sp->burst_cnt > 1 && sp->pulse+sp->pulse_cnt != (Pulse_t**) pp+i+2 &&
	pp[i+1]->aux.window_duration == pp[i+2]->aux.window_duration) ? i+1 :i);

    return (pp[k]->aux.sampling_rate == 0 ? 1440: \
	   (pp[k]->aux.sampling_rate == 1 ? 822 : 576));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
double R1_WindowDuration(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    const R1_Pulse_t** pp = (const R1_Pulse_t**) obj;
    register int k, i;

    if (sp->mode >= ST1 && sp->mode <= ST7)
	return nominal_swath[BEAM_S1+(sp->mode-ST1)] * ((double)1E-6/0.15);

    i = (Pulse_t**) pp == sp->pulse ? 1 :
       ((Pulse_t**) pp == sp->pulse + sp->pulse_cnt - 1 ? -1 : 0);

    k = pp[i]->aux.window_duration == pp[i+1]->aux.window_duration ||
	pp[i]->aux.window_duration == pp[i-1]->aux.window_duration  ? i:
       (pp[i+1]->aux.window_duration == pp[i-1]->aux.window_duration ||
       (sp->burst_cnt > 1 && sp->pulse+sp->pulse_cnt != (Pulse_t**) pp+i+2 &&
	pp[i+1]->aux.window_duration == pp[i+2]->aux.window_duration) ? i+1 :i);

    i = pp[i]->aux.sampling_rate == pp[i+1]->aux.sampling_rate ||
	pp[i]->aux.sampling_rate == pp[i-1]->aux.sampling_rate ? i :
       (pp[i+1]->aux.sampling_rate == pp[i-1]->aux.sampling_rate ||
       (sp->burst_cnt > 1 && sp->pulse+sp->pulse_cnt != (Pulse_t**) pp+i+2 &&
	pp[i+1]->aux.window_duration == pp[i+2]->aux.window_duration) ? i+1 :i);

    return ((pp[k]->aux.window_duration+1)*6/SAMPLING_FREQ(pp[i]) - 
	    (sp->mode >= EH1 && sp->mode <= EH6 ? window_duration_error : 0));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_AGC(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    return AGC(((const R1_Pulse_t**) obj)[0]);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
#define RX_GAIN(pp, sp) \
    (AGC((pp)[0]) < 32 ? AGC((pp)[0]) :(AGC((pp)[0]) - 0x38 + 32))

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_DWP(double *delay, PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    register int dwp;
    const R1_Pulse_t** pp = (const R1_Pulse_t**) obj;
    *delay = ((dwp = DWP(pp[0])) + 5) * 6 / SAMPLING_FREQ(pp[0]);
    return dwp;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |      get_RYP -
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |      This routine calculates spacecraft attitude from input statevector.
 |
 *---------------------------------------------------------------------------*/
RYP* R1_RYP(const double sv[6], RYP* att)
{
    double ryp[3];
    if (!sv || !att) return 0;
    att->yaw   = 0;
    att->pitch = 0;
    att->roll  = 0;
#ifdef  DEBUG
    printf("get_RYP(): yaw %g, pitch %g, roll %g\n",
	    att->yaw, att->pitch, att->roll);
#endif
    return att;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
double R1_PRF(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    register int i, i0 = obj-(PulseList_t)sp->pulse, iN = sp->pulse_cnt-3;
    for (i = i0; i <= iN; ++i)
	if ((sp->burst_len <= 1 || (R1_SameBurst(sp->pulse[i],sp->pulse[i+1]) &&
				    R1_SameBurst(sp->pulse[i],sp->pulse[i+2]))) 
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+1])->aux.prf_period
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+2])->aux.prf_period)
	return 
	    SAMPLING_FREQ((R1_Pulse_t*)sp->pulse[i])
		/ ((((R1_Pulse_t*)sp->pulse[i])->aux.prf_period + 2)*6);
    i = i0;
    if (i > iN)
	i = iN;
    while (--i >= 0)
	if ((sp->burst_len <= 1 || (R1_SameBurst(sp->pulse[i],sp->pulse[i+1]) &&
				    R1_SameBurst(sp->pulse[i],sp->pulse[i+2]))) 
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+1])->aux.prf_period
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+2])->aux.prf_period)
	return 
	    SAMPLING_FREQ((R1_Pulse_t*)sp->pulse[i])
		/ ((((R1_Pulse_t*)sp->pulse[i])->aux.prf_period + 2)*6);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_SameBurst(const Pulse_t* p1, const Pulse_t* p2)
{
    register int i = 0;
    if (((R1_Pulse_t*)p1)->aux.prf_period ==
	((R1_Pulse_t*)p2)->aux.prf_period)
	++i;
    if (((R1_Pulse_t*)p1)->aux.window_duration ==
	((R1_Pulse_t*)p2)->aux.window_duration)
	++i;
    return (i == 2 ? -1 : i);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_SameAGC(const Pulse_t* obj, const Pulse_t* src)
{
    const int x = AGC((R1_Pulse_t*)obj);
    const int y = AGC((R1_Pulse_t*)src);
    return (x == 0 || y == 0 ? 1 : (x == y ? -1 : 0));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_SameDWP(const Pulse_t* obj, const Pulse_t* src)
{
    const int x = DWP((R1_Pulse_t*)obj);
    const int y = DWP((R1_Pulse_t*)src);
    return (x == 0 || y == 0 ? 1 : (x == y ? -1 : 0)); 
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int R1_Decode_GMT(const R1_Pulse_t* pp, GMT* tp)
{
    static int yr[]  = { 0, 1995, 1996, 1997, 1998, 1999, 2000};
    static int doy[] = { 0,  365,  731, 1096, 1461, 1826, 2192};

    register int k, usec = (int) (1000 * (pp->aux.sc_msec + .0005 +
	     ((pp->aux.sc_usec_msb << 8) | pp->aux.sc_usec_lsb) / 1024.0));
    tp->second = (pp->aux.sc_secs % 60) + (usec / 1.0E6);
    tp->min = (pp->aux.sc_secs % 3600) / 60;
    tp->hr = (pp->aux.sc_secs / 3600);
    tp->yr = 0;
    tp->day = ((pp->aux.sc_days_msb << 3) | pp->aux.sc_days_lsb) + 263;

    for (k = 0; k < sizeof(doy)/sizeof(int); ++k)
	if (tp->day <= doy[k]) {
	    tp->day -= doy[k-1];
	    tp->yr = yr[k];
	    break;
	}
}
/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int R1_GMT(GMT* tp, PulseList_t obj, Segmt_t* sp, const Job_t* job)
{
    register int i, i0 = obj - (PulseList_t)sp->pulse,
		    iN = sp->pulse_cnt-3;
    register double td;
    GMT t1, t2;

    for (i = i0; i <= iN; ++i)
	if ((sp->burst_len <= 1 || (R1_SameBurst(sp->pulse[i],sp->pulse[i+1]) &&
				    R1_SameBurst(sp->pulse[i],sp->pulse[i+2]))) 
/*
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+1])->aux.prf_period
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+2])->aux.prf_period
*/
	&&  (R1_Decode_GMT((R1_Pulse_t*) sp->pulse[i], tp),
	     R1_Decode_GMT((R1_Pulse_t*) sp->pulse[i+1], &t1),
	     td = gmt_diff(&t1, tp), td > 0 && td < 0.001)
	&&  (R1_Decode_GMT((R1_Pulse_t*) sp->pulse[i+2], &t2),
	     td = gmt_diff(&t2, &t1), td > 0 && td < 0.001)) {

	    gmt_add(tp, (i0-i) / R1_PRF((PulseList_t)&sp->pulse[i],sp,job));
	    return ((int) tp);
	}
    i = i0;
    if (i > iN)
	i = iN;
    while (--i >= 0) {
	if ((sp->burst_len <= 1 || (R1_SameBurst(sp->pulse[i],sp->pulse[i+1]) &&
				    R1_SameBurst(sp->pulse[i],sp->pulse[i+2]))) 
/*
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+1])->aux.prf_period
	&&  ((R1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((R1_Pulse_t*) sp->pulse[i+2])->aux.prf_period
*/
	&&  (R1_Decode_GMT((R1_Pulse_t*) sp->pulse[i], tp),
	     R1_Decode_GMT((R1_Pulse_t*) sp->pulse[i+1], &t1),
	     td = gmt_diff(&t1, tp), td > 0 && td < 0.001)
	&&  (R1_Decode_GMT((R1_Pulse_t*) sp->pulse[i+2], &t2),
	     td = gmt_diff(&t2, &t1), td > 0 && td < 0.001)) {

	    gmt_add(tp, (i0-i) / R1_PRF((PulseList_t)&sp->pulse[i],sp,job));
	    return ((int) tp);
	}
    }
    printfLLog(LOG_ERR, "Pulse %d/%.5d: bad GMT",
	       obj[0]->start.blk, obj[0]->start.bit);
    return ((int) tp);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
double R1_LookAngle(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    register int k,i;
    register R1_Pulse_t* pp;

    for (i = (obj-(PulseList_t)sp->pulse)+1; i < sp->pulse_cnt; ++i)
	if ((((R1_Pulse_t*)sp->pulse[i-1])->aux.beam_select ==
	     ((R1_Pulse_t*)sp->pulse[i  ])->aux.beam_select) &&
	    (((R1_Pulse_t*)sp->pulse[i-1])->aux.beam_sequence ==
	     ((R1_Pulse_t*)sp->pulse[i  ])->aux.beam_sequence) &&
	    (sp->burst_cnt > 1 ? 1 :
	     ((R1_Pulse_t*)sp->pulse[i  ])->aux.beam_sequence <= BEAM_F5))
	    break;
    pp = (R1_Pulse_t*) (i == sp->pulse_cnt ? obj[0] : sp->pulse[i]);

    i = sp->burst_cnt > 1 /* pp->aux.number_of_beams */ ?
	((pp->aux.beam_sequence >> ((3 - pp->aux.beam_select) * 4)) & 0xF) :
	( pp->aux.beam_sequence <= BEAM_F5 ? pp->aux.beam_sequence : 0 );

    for (k = 0; k < LOOKTABLE_MAX; ++k)
	if (looktable[k].rmin <= job->rev && job->rev <= looktable[k].rmax)
	    break;

    if (k == LOOKTABLE_MAX || i == 0) {
	if (k == LOOKTABLE_MAX) k = 0;

	printfLLog(LOG_ERR,
"can't find look angle for rev %d, beam_sel %d, seq %X, num %d @%d/%.5d; %dB",
job->rev, pp->aux.beam_select, pp->aux.beam_sequence, pp->aux.number_of_beams,
	    pp->hdr.start.blk, pp->hdr.start.bit, sp->burst_cnt);
    }
    return looktable[k].beam[i];
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
double Temperature(U32 tempCode)
{
    double log_Rth = log((2940.0 * tempCode) / (294.0 - tempCode));
    return (1.0/(.001473+.0002372*log_Rth+(1.074E-7)*pow(log_Rth,3.0))) - 273.1;
}
/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
const static char hdr[] = "\
/* This file was automatically generated by %s */\n\
/* Copyright (C) 1996, California Institute of Technology. */\n\
/* ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged. */\n\n\
OBJECT = %s\n\
 OBJECT = COMMON_HEADER\n\
  MSG_TYPE = \"%s\"\n\
  TIME = %.4d-%.3dT%.2d:%.2d:%.2d\n\
  DESTINATION = \"%s\"\n\
  SOURCE = \"%s\"\n\
  NUMBER_OF_RECORDS = 1\n\
 END_OBJECT = COMMON_HEADER\n\
\n\
 OBJECT = BODY\n\
  JOB_ID = %d\n\
  FRAME_ID = %d\n\
  FRAME_MODE = \"%s\"\n\
  MODE = \"%s\"\n\
  PLATFORM = \"%s\"\n\
  SENSOR = \"%c\"\n\
  REVOLUTION = %d\n\
  SEQUENCE = %d\n\
  START_BURST = 1\n\
  END_BURST = %-8d\n\
 END_OBJECT = BODY\n\
END_OBJECT = %s\n\
END\n\
";

const static char auxhdr[] = "\
/* This file was automatically generated by %s */\n\
/* Copyright (C) 1996, California Institute of Technology. */\n\
/* ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged. */\n\n\
OBJECT = %s\n\
 OBJECT = COMMON_HEADER\n\
  MSG_TYPE = \"%s\"\n\
  TIME = %.4d-%.3dT%.2d:%.2d:%.2d\n\
  DESTINATION = \"%s\"\n\
  SOURCE = \"%s\"\n\
  NUMBER_OF_RECORDS = 1\n\
 END_OBJECT = COMMON_HEADER\n\
\n\
 OBJECT = BODY\n\
  JOB_ID = %d\n\
  FRAME_ID = %d\n\
  FRAME_MODE = \"%s\"\n\
  MODE = \"%s\"\n\
  PLATFORM = \"%s\"\n\
  SENSOR = \"%c\"\n\
  REVOLUTION = %d\n\
  SEQUENCE = %d\n\
  START_BURST = 1\n\
  END_BURST = %-8d\n\
";

const static char trailer[] =
"\n\
 END_OBJECT = BODY\n\
END\n\
";

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int ReplicaPulse(const R1_Pulse_t** pa, size_t np, 
		 const Segmt_t* sp, R1_Pulse_t** rp)
{
    register int k;
    R1_Pulse_t* pp;
    size_t nR = 0;

    for (k = np/2; k < np && nR == 0; ++k) {
	pp = (R1_Pulse_t*)pa[k];
	nR = ReplicaSamples(&pa[k], sp);
    }
    for (k = np/2; --k >= 0 && nR == 0;) {
	pp = (R1_Pulse_t*)pa[k];
	nR = ReplicaSamples(&pa[k], sp);
    }
    *rp = (nR ? pp : NULL);
    return nR;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int get_reference_pulse(int num, const R1_Pulse_t** pa, size_t np, 
			RefPulse_t* ref)
{
    register int i;
    if (ref->num == num)
	return ref->val;

    for (i = 1; i < np; ++i) {
	register const R1_Pulse_t* p0 = pa[i-1];
	register const R1_Pulse_t* p1 = pa[i];

	if (p0->aux.pulse_cnt1 == p1->aux.pulse_cnt1 &&
	    p0->aux.pulse_cnt2 == p1->aux.pulse_cnt2 &&
	    p1->aux.pulse_cnt1 >= p1->aux.pulse_cnt2 &&
	    p0->aux.beam_sequence == p1->aux.beam_sequence &&
	    p0->aux.number_of_beams == p1->aux.number_of_beams &&
	    p0->aux.beam_select == p1->aux.beam_select &&
	    p0->aux.window_duration == p1->aux.window_duration)
	    break;
    }
    if (i == np) {
	printfLLog(LOG_ERR, "Burst #%d @%d/%.5d: cannot find a reference pulse",
	    num, pa[0]->hdr.start.blk, pa[0]->hdr.start.bit);
	i = 0;
    }
    ref->num = num;
    ref->val = i;

    return i;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int Save_DWP_AGC_Changes(const Segmt_t* sp, const R1_Pulse_t** pa, size_t np,
			 int ref, char *buf, const Job_t* job)
{
    const double time_unit = TIME_UNIT(pa[ref]);
    const int pulse_cnt = isScanSAR(job->mode)
	? (pa[ref]->aux.pulse_cnt1 - pa[ref]->aux.pulse_cnt2) : 0;

    register int n = 0, fmt_0 = (pa-(const R1_Pulse_t**)sp->pulse)+pulse_cnt;
    register int i, k, lo, hi;
    double dwp_delay;

    lo = 0; hi = sp->dwp_cnt;
    while (lo < hi) {
        const int i = (lo+hi)/2;
        const int k = sp->dwp[i];
        if (k == fmt_0) {
            lo = i+1;
            break;
        }
        if (k > fmt_0)
            hi = i;
        else
            lo = i+1;
    }
    fmt_0 -= pulse_cnt;
    k = 0;
    n += sprintf(buf+n, "\n   WINDOW_POS_CHANGES = (%d", pulse_cnt+1);
    for (i = lo; i < sp->dwp_cnt && sp->dwp[i]-fmt_0 < np; ++i)
	n += sprintf(buf+n, "%s%d", ((++k)%8) ? ", " : ",\n   ",
		     sp->dwp[i]-fmt_0+1);
    k = 0;
    n += sprintf(buf+n, ")\n   WINDOW_POS_VALUES = (%e",
	(R1_DWP(&dwp_delay, (PulseList_t)&sp->pulse[sp->dwp[lo-1]], sp, job),
		 dwp_delay));
    for (i = lo; i < sp->dwp_cnt && sp->dwp[i]-fmt_0 < np; ++i)
	n += sprintf(buf+n, "%s%e", ((++k)%8) ? ", " : ",\n   ",
		    (DWP((R1_Pulse_t*)sp->pulse[sp->dwp[i]])+5)*time_unit);

    fmt_0 += pulse_cnt;
    lo = 0; hi = sp->agc_cnt;
    while (lo < hi) {
        const int i = (lo+hi)/2;
        const int k = sp->agc[i];
        if (k == fmt_0) {
            lo = i+1;
            break;
        }
        if (k > fmt_0)
            hi = i;
        else
            lo = i+1;
    }
    fmt_0 -= pulse_cnt;
    k = 0;
    n += sprintf(buf+n, ")\n   AGC_CHANGES = (%d", pulse_cnt+1);
    for (i = lo; i < sp->agc_cnt && sp->agc[i]-fmt_0 < np; ++i)
	n += sprintf(buf+n, "%s%d", ((++k)%8) ? ", " : ",\n   ",
		     sp->agc[i]-fmt_0+1);
    k = 0;
    n += sprintf(buf+n, ")\n   AGC_VALUES = (%d.0",
		 RX_GAIN((R1_Pulse_t**)&sp->pulse[sp->agc[lo-1]], sp));
    for (i = lo; i < sp->agc_cnt && sp->agc[i]-fmt_0 < np; ++i)
	n += sprintf(buf+n, "%s%d.0", ((++k)%8) ? ", " : ",\n   ",
		     RX_GAIN((R1_Pulse_t**)&sp->pulse[sp->agc[i]], sp));
    return n;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int R1_SaveAux (Segmt_t* sp, const R1_Pulse_t** pa, int np, int num,
		char* buf, int eof, Writer_t* wp, const Job_t* job)
{
    register int i, k, ref, frame_cnt, n = 0;
    R1_Pulse_t *rp;
    double time_unit, err;
    GMT gmt;

    static double roll_error = 0.0;
    static size_t frm_total = 0, bit_error = 0;

const static char aux_bad[] =
"\n\
  OBJECT = BURST_INFO\n\
   BURST = %d\n\
   BURST_ADDRESS = (0, 0)\n\
   PULSE_FRAME_CNT = 0\n\
   ECHO_VALID = \"NO\" /* Missing */\n\
   ECHO_SAMPLES = 0\n\
   ECHO_PULSES = 0\n\
   REPLICA_SAMPLES = 0\n\
   REPLICA_VALID = \"NO\"\n\
   NUMBER_OF_BEAMS = 0\n\
   BEAM_SELECT = %d\n\
   REPLICA_AGC = 0.0\n\
   CAL1_ATTENUATOR = 0.0\n\
   CAL2_ATTENUATOR = 0.0\n\
   CHIRP_TYPE = 0\n\
   LNA_TEMP = 0.0\n\
   SUBSYSTEM_TEMP = 0.0\n\
   PROTECTOR_TEMP = 0.0\n\
   CALIBRATION_TEMP = 0.0\n\
   SAMPLING_RATE = 0.0\n\
   PULSE_COUNT_1 = 0\n\
   PULSE_COUNT_2 = 0\n\
   PULSE_PERIOD = 0.0\n\
   WINDOW_START_TIME = 0.0\n\
   WINDOW_DURATION = 0.0\n\
   ROLL_VALID = \"NO\"\n\
   ROLL_ERROR = 0.0\n\
   ROLL_RATE = 0.0\n\
   PITCH_VALID = \"NO\"\n\
   PITCH_ERROR = 0.0\n\
   PITCH_RATE = 0.0\n\
   YAW_VALID = \"NO\"\n\
   YAW_ERROR = 0.0\n\
   YAW_RATE = 0.0\n\
   SC_TIME = 1970-001T00:00:00\n\
   REPLICA_PRESENT = \"NO\"\n\
   RX_AGC = 0.0\n\
   WINDOW_POS_CHANGES = (0)\n\
   WINDOW_POS_VALUES = (0)\n\
   AGC_CHANGES = (0)\n\
   AGC_VALUES = (0)\n\
  END_OBJECT = BURST_INFO\n\
";
    if (! pa) return AUX_PER_BURST_SIZE;

    if (eof) {
	register double pwr = 0.0;
	register int nB = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4));

	if (! isScanSAR(job->mode)) {
	    ref = get_reference_pulse(1, pa, np, wp->ref);
	    n += Save_DWP_AGC_Changes(sp, pa, np, ref, buf+n, job);
	    n += sprintf(buf+n, ")\n  END_OBJECT = BURST_INFO\n");
	}
	else if (num % nB) {
	    /*  Last cycle is incomplete - pad it with dummy bad bursts */

	    static int B[4][4] = {{1,2,3,7}, {1,2,5,6}, {1,2,0,0}, {2,5,6,0}};
	    k = nB - (num % nB);

	    for (i = 1; i <= k; ++i)
		n += sprintf(buf+n, aux_bad, num+i,
		     B[job->mode-SWA][(num+i-1)%nB]);
	}
	/*  Output average statistics */
	for (i = 0; i < REPLICA_NBIN; ++i)
	    pwr += (wp->rep[i].re*wp->rep[i].re + wp->rep[i].im*wp->rep[i].im)
		 / (num*num);

	n += sprintf(buf+n,
"\n  AVG_REPLICA_POW = %e\n  AVG_ROLL_ERROR = %e\n  BIT_ERROR_RATE = %e\n\n\
 END_OBJECT = BODY\nEND_OBJECT = AUXILIARY_FILE\nEND\n",
	    10*log10(pwr/REPLICA_NBIN),
	    roll_error / num, 
	    bit_error / ((double)frm_total*32));

	return n;
    }
    if (num == 1) {
        time_t t;
        struct tm tm;
        localtime_r((time(&t), &t), &tm);

        n = sprintf (buf, auxhdr, version_id, "AUXILIARY_FILE","AUXILIARY_FILE",
            tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour, tm.tm_min, tm.tm_sec,
            job->src,
	    job->dst,
            job->job_id,
            job->frame_id,
            job->frame_mode,
            ModeImage(job->mode),
            job->platform,
            job->sensor,
            job->rev,
            job->seq, 1
            );
	if (isScanSAR(job->mode)) {
	    static const char tag[] = "END_BURST = ";
	    wp->offset = (strstr(buf, tag) - buf) + (sizeof(tag)-1);
	}
	roll_error = 0.0;
	bit_error = frm_total = 0;
    }
    for (i = 0; i < np; ++i) {
	frm_total += pa[i]->hdr.frame_cnt;
	bit_error += pa[i]->hdr.bit_error;
    }
    if (! isScanSAR(job->mode)) {
	register const int blk = (job->end.blk + job->start.blk) / 2;

	if (pa[0]->hdr.start.blk <= blk && blk <= pa[np-1]->hdr.start.blk)
	    for (i = 1; i < np; ++i)
		if (pa[i-1]->aux.roll_error_msb == pa[i]->aux.roll_error_msb &&
		    pa[i-1]->aux.roll_error_lsb == pa[i]->aux.roll_error_lsb)
		    roll_error = EU_Error((pa[i]->aux.roll_error_msb<<7) |
					   pa[i]->aux.roll_error_lsb);
	if (num > 1)
	    return 0;
    }
    ref = get_reference_pulse(num, pa, np, wp->ref);
    frame_cnt = pa[ref]->hdr.frame_cnt;
    time_unit = TIME_UNIT(pa[ref]);

    if (! isScanSAR(job->mode)) {
	i = np-1;
	R1_GMT(&gmt, (PulseList_t)&pa[k = 0], sp, job);
    }
    else {
	register int ctr;
	k = pa[ref]->aux.pulse_cnt1 - pa[ref]->aux.pulse_cnt2;
	for (i = k+1; i < np-1; ++i)
	    if (DWP(pa[i]) != DWP(pa[k]) && DWP(pa[i+1]) != DWP(pa[k]))
		break;
	if ((ctr = np - (np-k)/2) < 0)
	    ctr = 0;
	R1_GMT(&gmt, (PulseList_t)&pa[ctr], sp, job);
    }
    n += sprintf(buf+n, "\n  OBJECT = BURST_INFO\n");
    n += sprintf(buf+n, "   BURST = %d\n", num);
    n += sprintf(buf+n, "   BURST_ADDRESS = (%d, %.5d) /* REF %d/%.5d */\n",
	 BLKNUM(pa[0]->hdr.start.blk,sp,job),
	 BITNUM(pa[0]->hdr.start.bit,sp,job),
	 BLKNUM(pa[ref]->hdr.start.blk,sp,job),
	 BITNUM(pa[ref]->hdr.start.bit,sp,job));
    n += sprintf(buf+n, "   PULSE_FRAME_CNT = %d\n", frame_cnt);

    if (isScanSAR(job->mode) && pa[ref]->aux.pulse_cnt1+1 != np) {
	n += sprintf(buf+n, "   ECHO_VALID = \"NO\" /* want %d pulses */\n",
	    pa[ref]->aux.pulse_cnt1+1);

	printfLLog(LOG_ERR, "Burst #%d @%d/%.5d: has %d pulses, want %d", num,
	    BLKNUM(pa[0]->hdr.start.blk,sp,job),
	    BITNUM(pa[0]->hdr.start.bit,sp,job), np, pa[ref]->aux.pulse_cnt1+1);
    }
    else if (gmt_diff(&gmt, &job->start_time) < -job->gmt_diff_max ||
             gmt_diff(&gmt, &job->end_time) > job->gmt_diff_max) {
	n += sprintf(buf+n, "   ECHO_VALID = \"NO\" /* bad GMT */\n");

	printfLLog(LOG_ERR,
	    "Burst #%d @%d/%.5d: has center pulse with bad GMT", num,
	    BLKNUM(pa[0]->hdr.start.blk,sp,job), 
	    BITNUM(pa[0]->hdr.start.bit,sp,job));
    }
    else if (i != np-1) {
	n += sprintf(buf+n, "   ECHO_VALID = \"NO\" /* DWP changed */\n");

	printfLLog(LOG_ERR, "Burst #%d @%d/%.5d: DWP changed", num,
	    BLKNUM(pa[0]->hdr.start.blk,sp,job),
	    BITNUM(pa[0]->hdr.start.bit,sp,job));
    }
    else if (isScanSAR(job->mode) && !wp->eco_valid[num-1]) {
	n += sprintf(buf+n, "   ECHO_VALID = \"NO\" /* missing data */\n");
    }
    else {
	n += sprintf(buf+n, "   ECHO_VALID = \"YES\"\n");
    }
    n += sprintf(buf+n, "   ECHO_SAMPLES = %d\n", 
	(pa[ref]->aux.window_duration+1)*6);

    if (isScanSAR(job->mode))
	n += sprintf(buf+n, "   ECHO_PULSES = %d\n", np);
    else {
	static const char tag[] = "ECHO_PULSES = ";
	n += sprintf(buf+n, "   ECHO_PULSES = %-8d\n",1);
	wp->offset = (strstr(buf,tag) - buf) +(sizeof(tag) -1);
    }
    n += sprintf(buf+n, "   REPLICA_SAMPLES = %d\n",
	 ReplicaPulse(pa, np, sp, &rp));
    n += sprintf(buf+n, "   REPLICA_VALID = \"%s\"\n",
	 rp && (!isScanSAR(job->mode) || wp->rep_valid[num-1]) ? "YES" : "NO");

    n += sprintf(buf+n, "   NUMBER_OF_BEAMS = %d\n",
	 pa[ref]->aux.number_of_beams+1);
    n += sprintf(buf+n, "   BEAM_SELECT = %d\n", BeamID(pa[ref]));
    n += sprintf(buf+n, "   REPLICA_AGC = %d.0\n", ReplicaAGC(pa[ref]));

    n += sprintf(buf+n, "   CAL1_ATTENUATOR = %e\n", (double)
	 (pa[ref]->aux.CAL_status == 1 ? CAL_GAIN(pa[ref]) : 0.0));
    n += sprintf(buf+n, "   CAL2_ATTENUATOR = %e\n", (double)
	 (pa[ref]->aux.CAL_status == 2 ? CAL_GAIN(pa[ref]) : 0.0));
    n += sprintf(buf+n, "   CHIRP_TYPE = %d\n", pa[ref]->aux.chirp_type);

    n += sprintf(buf+n, "   LNA_TEMP = %f\n",
	 Temperature(pa[ref]->aux.LNA_temp));
    n += sprintf(buf+n, "   SUBSYSTEM_TEMP = %f\n",
         Temperature(pa[ref]->aux.subsystem_temp));
    n += sprintf(buf+n, "   PROTECTOR_TEMP = %f\n",
         Temperature(pa[ref]->aux.protector_temp));
    n += sprintf(buf+n, "   CALIBRATION_TEMP = %f\n",
         Temperature(pa[ref]->aux.CAL_temp));

    n += sprintf(buf+n, "   SAMPLING_RATE = %e\n", SAMPLING_FREQ(pa[ref]));
    n += sprintf(buf+n, "   PULSE_COUNT_1 = %d\n", pa[ref]->aux.pulse_cnt1+1);
    n += sprintf(buf+n, "   PULSE_COUNT_2 = %d\n", pa[ref]->aux.pulse_cnt2+1);
    n += sprintf(buf+n, "   PULSE_PERIOD = %e\n",
	 1.0 / R1_PRF((PulseList_t)&pa[ref], sp, job));

    n += sprintf(buf+n, "   WINDOW_START_TIME = %e\n", (double)
	 (((pa[ref]->aux.window_start_msb<<4) |
	    pa[ref]->aux.window_start_lsb) + 5) * time_unit);
    n += sprintf(buf+n, "   WINDOW_DURATION = %e\n", (double)
         (pa[ref]->aux.window_duration + 1) * time_unit);

    n += sprintf(buf+n, "   ROLL_VALID = \"%s\"\n",
         pa[ref]->aux.roll_valid ? "YES" : "NO");
    n += sprintf(buf+n, "   ROLL_ERROR = %e\n", (err =
	 EU_Error((pa[ref]->aux.roll_error_msb<<7)|pa[ref]->aux.roll_error_lsb),
	 roll_error += err, err));
    n += sprintf(buf+n, "   ROLL_RATE = %e\n", EU_Rate(pa[ref]->aux.roll_rate));
    n += sprintf(buf+n, "   PITCH_VALID = \"%s\"\n",
         pa[ref]->aux.pitch_valid ? "YES" : "NO");
    n += sprintf(buf+n, "   PITCH_ERROR = %e\n",
	 EU_Error(pa[ref]->aux.pitch_error));
    n += sprintf(buf+n, "   PITCH_RATE = %e\n",
         EU_Rate((pa[ref]->aux.pitch_rate_msb<<8)|pa[ref]->aux.pitch_rate_lsb));
    n += sprintf(buf+n, "   YAW_VALID = \"%s\"\n",
         pa[ref]->aux.yaw_valid ? "YES" : "NO");
    n += sprintf(buf+n, "   YAW_ERROR = %e\n", 
         EU_Error((pa[ref]->aux.yaw_error_msb<<7)|pa[ref]->aux.yaw_error_lsb));
    n += sprintf(buf+n, "   YAW_RATE = %e\n", EU_Rate(pa[ref]->aux.yaw_rate));

    n += sprintf(buf+n, "   SC_TIME = %.4d-%.3dT%.2d:%.2d:%f\n",
		 gmt.yr, gmt.day, gmt.hr, gmt.min, gmt.second);
    n += sprintf(buf+n, "   REPLICA_PRESENT = \"%s\"\n", rp ? "YES" : "NO");
    n += sprintf(buf+n, "   RX_AGC = %d.0", RX_GAIN(&pa[ref], sp));

    if (isScanSAR(job->mode)) {
	n += Save_DWP_AGC_Changes(sp, pa, np, ref, buf+n, job);
	n += sprintf(buf+n, ")\n  END_OBJECT = BURST_INFO\n");
    }
    return n;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
#define	eph_double(px, pp,_i)	*(double*) (\
	((unsigned short*) px)[0] = ((pp)[_i+3])->aux.ephemeris,	\
	((unsigned short*) px)[1] = ((pp)[_i+2])->aux.ephemeris,	\
	((unsigned short*) px)[2] = ((pp)[_i+1])->aux.ephemeris,	\
	((unsigned short*) px)[3] = ((pp)[_i])->aux.ephemeris, px)

#define	eph_float(px, pp,_i)	*(float*) (\
	((unsigned short*) px)[0] = ((pp)[_i+1])->aux.ephemeris,	\
	((unsigned short*) px)[1] = ((pp)[_i])->aux.ephemeris, px)
static
int R1_SaveEph (const Segmt_t* sp, const R1_Pulse_t** pa, int np, int num,
		char* buf, int eof, Writer_t* wp, const Job_t* job)
{
    register int i, n = 0;
    double x;
    float  y;
    time_t t;
    struct tm tm;

const static char eph_bad[] =
"\n\
  OBJECT = BURST_INFO\n\
   BURST = %d\n\
   DATA_VALID = \"NO\" /* %s */\n\
   BEAM_SELECT = %d\n\
   REVOLUTION = 0\n\
   TIME = 1995-001T00:00:00.000\n\
   GREENWICH_ANGLE = 0.0\n\
   A = 0.0\n\
   H = 0.0\n\
   K = 0.0\n\
   P = 0.0\n\
   Q = 0.0\n\
   L = 0.0\n\
   DRAG = 0.0\n\
   ROLL_BIAS = 0.0\n\
   PITCH_BIAS = 0.0\n\
   YAW_BIAS = 0.0\n\
  END_OBJECT = BURST_INFO\n\
";
    static int B[4][4] = {{1,2,3,7},{1,2,5,6},{1,2,0,0},{2,5,6,0}};

    if (! pa) return EPH_PER_BURST_SIZE;
    if (eof) {
	/*  Last cycle is incomplete - pad it with dummy bad bursts */
	register int nB = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4));

	if (isScanSAR(job->mode) && (num % nB)) {
	    register int k = nB - (num % nB);
	    for (i = 1; i <= k; ++i)
		n += sprintf(buf+n, eph_bad, num+i, "missing",
		     B[job->mode-SWA][(num+i-1)%nB]); 
	}
	n += sprintf(buf+n,
	"\n END_OBJECT = BODY\nEND_OBJECT = EPHEMERIS_FILE\nEND\n");
	return n;
    }
    if (! isScanSAR(job->mode) && num > 1)
	return 0;

    if (num == 1) {
	static const char tag[] = "END_BURST = ";
        time_t t;
        struct tm tm;
        localtime_r((time(&t), &t), &tm);

        n = sprintf (buf, auxhdr, version_id, "EPHEMERIS_FILE","EPHEMERIS_FILE",
            tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour, tm.tm_min, tm.tm_sec,
            job->src,
            job->dst,
            job->job_id,
            job->frame_id,
            job->frame_mode,
            ModeImage(job->mode),
            job->platform,
            job->sensor,
            job->rev,
            job->seq, 1
            );
	wp->offset = (strstr(buf, tag) - buf) + (sizeof(tag)-1);
    }
    i = get_reference_pulse(num, pa, np, wp->ref);

    if (isScanSAR(job->mode) && pa[i]->aux.pulse_cnt1+1 != np) {
	register int nB = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4));
	n += sprintf(buf+n, eph_bad, num, "bad pulse count",
	     B[job->mode-SWA][(num-1)%nB]);
	return n;
    }
    for (i = 0; i < np && (pa[i  ]->aux.ephemeris != (R1_EPH >> 16) ||
			   pa[i+1]->aux.ephemeris != (R1_EPH & 0xFFFF));
	++i);
    if (i == np) {
	register int nB = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4));
	n += sprintf(buf+n, eph_bad, num, "can't find SYNC",
	     B[job->mode-SWA][(num-1)%nB]);
	return n;
    }
    n += sprintf(buf+n, "\n  OBJECT = BURST_INFO\n");
    n += sprintf(buf+n, "   BURST = %d\n", num);
    n += sprintf(buf+n, "   DATA_VALID = \"YES\"\n");
    n += sprintf(buf+n, "   BEAM_SELECT = %d\n", BeamID(pa[i]));

    n += sprintf(buf+n, "   REVOLUTION = %d\n",
	 pa[i+3]->aux.ephemeris << 16 | pa[i+2]->aux.ephemeris);

    n += sprintf(buf+n, "   TIME = %.4d-%.3dT%.2d:%.2d:%.2d.%.3d\n",
	 pa[i+4]->aux.ephemeris, pa[i+5]->aux.ephemeris,
	 pa[i+6]->aux.ephemeris, pa[i+7]->aux.ephemeris,
	 pa[i+8]->aux.ephemeris, pa[i+9]->aux.ephemeris);

    n += sprintf(buf+n, "   GREENWICH_ANGLE = %e\n", eph_double(&x, pa, i+10));
    n += sprintf(buf+n, "   A = %e\n", eph_double(&x, pa, i+14));
    n += sprintf(buf+n, "   H = %e\n", eph_double(&x, pa, i+18));
    n += sprintf(buf+n, "   K = %e\n", eph_double(&x, pa, i+22));
    n += sprintf(buf+n, "   P = %e\n", eph_double(&x, pa, i+26));
    n += sprintf(buf+n, "   Q = %e\n", eph_double(&x, pa, i+30));
    n += sprintf(buf+n, "   L = %e\n", eph_double(&x, pa, i+34));
    n += sprintf(buf+n, "   DRAG = %e\n", eph_double(&x, pa, i+38));

    n += sprintf(buf+n, "   ROLL_BIAS = %f\n", eph_float(&y, pa, i+42));
    n += sprintf(buf+n, "   PITCH_BIAS = %f\n", eph_float(&y, pa, i+44));
    n += sprintf(buf+n, "   YAW_BIAS = %f\n", eph_float(&y, pa, i+46));

    n += sprintf(buf+n, "   EPHEMERIS_ADDRESS = (%d, %d)\n",
	 BLKNUM(pa[i]->hdr.start.blk,sp,job),
	 BITNUM(pa[i]->hdr.start.bit,sp,job));
    n += sprintf(buf+n, "  END_OBJECT = BURST_INFO\n");

    return n;
}


/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int R1_SaveEco (const Segmt_t* sp, const R1_Pulse_t** pa, int np, int num,
		char* buf, int eof, Writer_t* wp, const Job_t* job)
{
    struct EcoHdr {
        char sync[12];
        int  burst;
        int  samples;
        int  pulses;
        int  filler[2];
    } ecoHdr;
    time_t t;
    struct tm tm;
    register int i, n = 0, hdrlen = sizeof(struct EcoHdr), totalNr = 0;

    if (!pa) return ECO_PER_BURST_SIZE;

    if (eof) {
        register int nB = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4));

        if (isScanSAR(job->mode) && (num % nB)) {
	    /*  Last cycle is incomplete - pad it with dummy bad bursts */
	    ecoHdr.samples = ecoHdr.pulses = 0;
	    nB -= (num % nB);

	    for (i = 1; i <= nB; ++i) {
		ecoHdr.burst = num+i;
		memcpy(buf+n, &ecoHdr, sizeof(struct EcoHdr));
		n += sizeof(struct EcoHdr);
		if (num+i <= BURST_PER_IMAGE_MAX)
		    wp->burst_offset[num+i] = wp->burst_offset[num] + n;
	    }
	}
	return n;
    }
    i = get_reference_pulse(num, pa, np, wp->ref);

    memcpy(ecoHdr.sync, "ECHORAW00001", sizeof(ecoHdr.sync));
    ecoHdr.burst = num;
    ecoHdr.samples = (pa[i]->aux.window_duration+1)*6;
    ecoHdr.pulses  = np;
    ecoHdr.filler[0] = ecoHdr.filler[1] = 0;

    if (num == 1) {
	static const char tag[] = "END_BURST = ";
        time_t t;
        struct tm tm;
        localtime_r((time(&t), &t), &tm);

        n = sprintf (buf, hdr, version_id, "ECHO_FILE", "ECHO_FILE",
            tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour, tm.tm_min, tm.tm_sec,
            job->src,
	    job->dst,
            job->job_id,
            job->frame_id,
            job->frame_mode,
            ModeImage(job->mode),
            job->platform,
            job->sensor,
            job->rev,
            job->seq, 1, "ECHO_FILE");
	hdrlen += (wp->burst_offset[0] = n);
	wp->offset = (strstr(buf, tag) - buf) + (sizeof(tag)-1);
    }
    if (isScanSAR(job->mode) || num == 1) {
	memcpy(buf+n, &ecoHdr, sizeof(struct EcoHdr));
	n += sizeof(struct EcoHdr);
    }
    for (i = 0; i < np; ++i) {
	const R1_Pulse_t* cp = pa[i];
	register int k, nSamples = ecoHdr.samples;
	register nF = cp->hdr.frame_cnt;
	register nR = ReplicaSamples(&pa[i], sp);
	void* tapebuf = cp->hdr.tapebuf;

	for (k = 0; k < nF && nSamples; ++k) {
	    register U8* fp;
	    register int nS = R1_DATA_SIZE - (k == 0 ? R1_AUX_SIZE : 0);
	    if (nR >= nS) {
		nR -= nS;
		continue;
	    }
	    nS -= nR;
	    if (nS > nSamples)
		nS = nSamples;

	    fp = (*job->Buf)(&tapebuf,
		cp->hdr.start.blk, cp->hdr.start.bit + k*R1_FRAME_SIZE*8);
	    memcpy(buf+n, fp + R1_HDR_SIZE + (k ? 0 : R1_AUX_SIZE) + nR, nS);
	    nR = 0;
	    n += nS;
	    nSamples -= nS;
	}
	if (nSamples) {
	    if (nSamples <= job->eco_pad) {
		/* No warning */
	    }
	    else if (isScanSAR(job->mode))
		printfLLog(LOG_INFO, 
		    "Burst #%d, Pulse %d @%d/%.5d: pad %d bytes", num, i,
		    BLKNUM(cp->hdr.start.blk,sp,job),
		    BITNUM(cp->hdr.start.bit,sp,job), nSamples);
	    else
		printfLLog(LOG_INFO, "Pulse %d @%d/%.5d: pad %d bytes",
		    &pa[i] - (const R1_Pulse_t**)sp->pulse, 
		    BLKNUM(cp->hdr.start.blk,sp,job),
		    BITNUM(cp->hdr.start.bit,sp,job), nSamples);
	    memset(buf+n, 0, nSamples);
	    n += nSamples;
	}
    }
    if (isScanSAR(job->mode) && num <= BURST_PER_IMAGE_MAX) {
	wp->burst_offset[num] = (num == 1 ? n : (wp->burst_offset[num-1]+n));
	wp->eco_valid[num-1] = (n == ecoHdr.samples*ecoHdr.pulses+hdrlen);
    }
    return n;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int R1_CAL(double* cal, Segmt_t* sp, const Job_t* job)
{
    Complex_t samples[CALTONE_NFFT], work[CALTONE_NFFT+15], sum[CALTONE_NBIN];
    const R1_Pulse_t** pa = (const R1_Pulse_t**) sp->pulse;
    register double pwr = 0.0;
    register int k, ref, np = sp->pulse_cnt;

    for (ref = 1; ref < np; ++ref)
	if (pa[ref]->aux.window_duration == pa[ref-1]->aux.window_duration)
	    break;
    if (ref == np) ref = 0;

    zfft1di(CALTONE_NFFT, work);
    if (np > CALTONE_LINE_MAX)
	np = CALTONE_LINE_MAX;

    for (k = 0; k < CALTONE_NBIN; ++k)
	sum[k] = zero;

    for (k = 0; k < np; ++k) {
	const R1_Pulse_t* pp = pa[k];
	void* tapebuf = pp->hdr.tapebuf;
	register int nR, nS, nC, nSamples, i, n = 0;
	register double K;

	nSamples = (pa[ref]->aux.window_duration+1)*6;
	if (nSamples > CALTONE_NFFT)
	    nSamples = CALTONE_NFFT;

	nR = ReplicaSamples(&pa[k], sp);
	nS = R1_DATA_SIZE - R1_AUX_SIZE;
	nC = R1_HDR_SIZE  + R1_AUX_SIZE;
	
	for (i = 0; i < pp->hdr.frame_cnt && nSamples; ++i) {
	    register int s, x;
	    if (nR >= nS) {
		nR -= nS;
		nS = R1_DATA_SIZE;
		nC = R1_HDR_SIZE;
		continue;
	    }
	    nS -= nR;
	    if (nS > nSamples)
		nS = nSamples;
	    {
	    const U8* bp = (*job->Buf)(&tapebuf,
		 pp->hdr.start.blk, pp->hdr.start.bit+i*R1_FRAME_SIZE*8) +nC+nR;
	    const U8* pn = prn_0 + nC + nR;

	    if ((x = (pp->hdr.start.bit & 0x7)) == 0)
		for (s = 0; s < nS; ++s) 
		    samples[n++] = voltage[bp[s] ^ pn[s]];
	    else {
		const int y = 8-x;
		for (s = 0; s < nS; ++s)
		    samples[n++] = voltage[(((bp[s]<<x)|(bp[s+1]>>y)) ^ pn[s])
					   & 0xFF];
	    }}
	    nSamples -= nS;
	    nS = R1_DATA_SIZE;
	    nC = R1_HDR_SIZE;
	    nR = 0;
	}
	while (n < CALTONE_NFFT) samples[n++] = zero;

	zfft1d(-1, CALTONE_NFFT, samples, 1, work);

	K = sqrt(pow(10.0, CAL_GAIN(pp)/10.0));
#ifdef	REAL
	n = CALTONE_NFFT * CALTONE_FREQ / SAMPLING_FREQ(pp);
#else
	n = CALTONE_NFFT / 2;
#endif
	if (n < CALTONE_NBIN)
	    return 0;

	n -= CALTONE_NBIN / 2;
	for (i = 0; i < CALTONE_NBIN; ++i) {
	    sum[i].re += K * samples[n].re;
	    sum[i].im += K * samples[n].im;
	    n++;
	}
    }
    for (k = 0; k < CALTONE_NBIN; ++k)
	pwr += (sum[k].re*sum[k].re + sum[k].im*sum[k].im) / (np*np);

    *cal = (pwr == 0.0) ? 0.0 : (10.0 * log10(pwr / CALTONE_NBIN));
    return (int) cal;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
void ReplicaPower(U8* bp, size_t len, double K, Complex_t* wrk, Complex_t* sum)
{
    register int i, n;
    Complex_t samples[REPLICA_NFFT], zero = {0.0, 0.0};

    for (i = 0; i < len; ++i) samples[i] = voltage[bp[i]];
    while (i < REPLICA_NFFT) samples[i++] = zero;
    zfft1d(-1, REPLICA_NFFT, samples, 1, wrk);

    n = (len - REPLICA_NBIN) / 2;
    for (i = 0; i < REPLICA_NBIN; ++i) {
	sum[i].re += K * samples[n].re;
	sum[i].im += K * samples[n].im;
	n++;
    }
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int R1_SaveRep (const Segmt_t* sp, const R1_Pulse_t** pa, int np, int num,
		char* buf, int eof, Writer_t* wp, const Job_t* job)
{
    struct RepHdr {
        char sync[12];
        int  burst;
        int  samples;
        int  filler[3];
    } repHdr;
    R1_Pulse_t* pp;
    register int n = 0, hdrlen = sizeof(struct RepHdr);

    if (! pa) return REP_PER_BURST_SIZE;
    if (eof) {
	/*  Last cycle is incomplete - pad it with dummy bad bursts */
	register int k = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4));

	if (isScanSAR(job->mode) && (num % k)) {
	    register int i;
	    repHdr.samples = 0;
	    k -= (num % k);
	    for (i = 1; i <= k; ++i) {
		repHdr.burst = num+i;
		memcpy(buf+n, &repHdr, sizeof(struct RepHdr));
		n += sizeof(struct RepHdr);
	    }
	}
	return n;
    }
    if (! isScanSAR(job->mode) && num > 1)
	return 0;

    memcpy(repHdr.sync, "RREPLICA0001", sizeof(repHdr.sync));
    repHdr.burst = num;
    repHdr.samples = ReplicaPulse(pa, np, sp, &pp);
    repHdr.filler[0] = repHdr.filler[1] = repHdr.filler[2] = 0;

    if (num == 1) {
	static const char tag[] = "END_BURST = ";
	register int k;
        time_t t;
        struct tm tm;
        localtime_r((time(&t), &t), &tm);

        n = sprintf (buf, hdr, version_id, "REPLICA_FILE", "REPLICA_FILE",
            tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour, tm.tm_min, tm.tm_sec,
            job->src,
	    job->dst,
            job->job_id,
            job->frame_id,
            job->frame_mode,
            ModeImage(job->mode),
            job->platform,
            job->sensor,
            job->rev,
            job->seq, 1, "REPLICA_FILE");
	hdrlen += n;
	wp->offset = (strstr(buf, tag) - buf) + (sizeof(tag)-1);

	for (k = 0; k < REPLICA_NBIN; ++k)
	    wp->rep[k] = zero;
	zfft1di(REPLICA_NFFT, wp->workrep);
    }
    memcpy(buf+n, &repHdr, sizeof(struct RepHdr));
    n += sizeof(struct RepHdr);

    if (repHdr.samples > 0) {
	register int k, nP = n, nR = repHdr.samples, nF = pp->hdr.frame_cnt;
	void* tapebuf = pp->hdr.tapebuf;

	for (k = 0; k < nF && nR; ++k) {
	    register U8* fp = (*job->Buf)(&tapebuf,
		pp->hdr.start.blk, pp->hdr.start.bit + k*R1_FRAME_SIZE*8);
	    register int nS = R1_DATA_SIZE - (k ? 0 : R1_AUX_SIZE);
	    if (nS > nR) nS = nR;
	    memcpy(buf + n, fp + R1_HDR_SIZE +(k ? 0 : R1_AUX_SIZE), nS);
	    n  += nS;
	   nR -= nS;
	}
	ReplicaPower(buf+nP, repHdr.samples,
	    sqrt(pow(10.0, ReplicaAGC(pp)/10.0)), wp->workrep, wp->rep);
    }
    if (isScanSAR(job->mode) && num <= BURST_PER_IMAGE_MAX)
	wp->rep_valid[num-1] = (n == repHdr.samples+hdrlen);
    return n;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int R1_SaveBof (const Segmt_t* sp, const R1_Pulse_t** pa, int npulse,
		int* burst, int nburst, char* buf, Writer_t* wp,
		const Job_t* job)
{
static const char bofhdr[] = "\
/* This file was automatically generated by %s */\n\
/* Copyright (C) 1996, California Institute of Technology. */\n\
/* ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged. */\n\n\
OBJECT = BURST_OFFSET_FILE\n\
 OBJECT = COMMON_HEADER\n\
  MSG_TYPE = \"BURST_OFFSET_FILE\"\n\
  TIME = %.4d-%.3dT%.2d:%.2d:%.2d\n\
  DESTINATION = \"%s\"\n\
  SOURCE = \"%s\"\n\
  NUMBER_OF_RECORDS = 1\n\
 END_OBJECT = COMMON_HEADER\n\
\n\
 OBJECT = BODY\n\
  JOB_ID = %d\n\
  FRAME_ID = %d\n\
  FRAME_MODE = \"%s\"\n\
  MODE = \"%s\"\n\
  PLATFORM = \"%s\"\n\
  SENSOR = \"%c\"\n\
  REVOLUTION = %d\n\
  SEQUENCE = %d\n\
  N_BURSTS = %d\n\
  N_BEAMS = %d\n\
  BYTE_OFFSETS =\n (%.9d\
";
    register int i, k, n;
    register int nb = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4));
    time_t t;
    struct tm tm;
    localtime_r((time(&t), &t), &tm);

#ifdef DEBUG
    k = ((nburst+nb-1) / nb) * nb;
#else
    k = (nburst / nb) * nb;
#endif
    n = sprintf (buf, bofhdr, version_id,
	tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour, tm.tm_min, tm.tm_sec,
	job->src,
	job->dst,
	job->job_id,
	job->frame_id,
	job->frame_mode,
	ModeImage(job->mode),
	job->platform,
	job->sensor,
	job->rev,
	job->seq,
	k,
	nb,
	wp->burst_offset[0]
	);
    for (i = 1; i <= k; ++i)
	n += sprintf(buf+n, "%s%.9d",(i%5 ?", " :",\n  "), wp->burst_offset[i]);
    n += sprintf(buf+n,
	")\n END_OBJECT = BODY\nEND_OBJECT = BURST_OFFSET_FILE\nEND\n");
    return n;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Writer_t* R1_flush (Writer_t* obj, const Job_t* job, int eof)
{
    register char *p;
    register size_t len;

    if (obj->bufLen == 0)
	return obj;

    if (obj->fd == -1 &&
	(obj->fd = open(
	    (p = strchr(obj->file, ':')) ? p+1 : obj->file, O_WRONLY|O_CREAT|
#ifndef	DIRECT_IO
	    O_TRUNC,
#else
	    (obj->type == R1_SaveEco ? (O_TRUNC|O_DIRECT) : O_TRUNC),
#endif
	    S_IRUSR|S_IWUSR|S_IRGRP)) == -1) {
        (*job->Log)(job->err, "%s: create failed, %s\n",
		    obj->file, strerror(errno));
        return NULL;
    }
#ifndef	DIRECT_IO
    if (write(obj->fd, obj->buf, obj->bufLen) != obj->bufLen) {
	(*job->Log)(job->err, "%s: write failed, %s\n",
		    obj->file, strerror(errno));
	close(obj->fd);
	obj->fd = -1;
	obj->bufLen = 0;
	return NULL;
    }
    len = obj->bufLen;
    obj->bufLen = 0;
#else
    if (obj->type != R1_SaveEco) {
	if (write(obj->fd, obj->buf, obj->bufLen) != obj->bufLen) {
	    (*job->Log)(job->err, "%s: write failed, %s\n",
			obj->file, strerror(errno));
	    close(obj->fd);
	    obj->fd = -1;
	    obj->bufLen = 0;
	    return NULL;
	}
	len = obj->bufLen;
	obj->bufLen = 0;
    }
    else {
	if (!eof || !(obj->bufLen & 0x1FF))
	    len = obj->bufLen & (~0x1FF);
	else {
	    len = (obj->bufLen+511) & (~0x1FF);
	    memset(obj->buf+obj->bufLen, 0, len-obj->bufLen);
	}
	if (write(obj->fd, obj->buf, len) != len) {
	    (*job->Log)(job->err, "%s: write failed, %s\n",
			obj->file, strerror(errno));
	    close(obj->fd);
	    obj->fd = -1;
	    obj->bufLen = 0;
	    return NULL;
	}
	if (obj->bufLen <= len)
	    obj->bufLen  = 0;
	else {
	    memcpy(obj->buf, obj->buf+len, obj->bufLen-len);
	    obj->bufLen -= len;
	}
	if (eof) {
	    close(obj->fd);
	    if ((obj->fd = open(
		    (p = strchr(obj->file, ':')) ? p+1 : obj->file,
		    O_WRONLY, S_IRUSR|S_IWUSR|S_IRGRP)) == -1) {
		(*job->Log)(job->err, "%s: can't re-open, %s\n",
			    obj->file, strerror(errno));
		obj->bufLen = 0;
		return NULL;
	    }
	}
    }
#endif /* DIRECT_IO */

#ifdef DEBUG
    printf("%s: %d bytes written\n", 
	   obj->type == R1_SaveEco ? "ECO" : 
	   obj->type == R1_SaveAux ? "AUX" :
	   obj->type == R1_SaveRep ? "REP" : "EPH", len);
#endif
    return obj;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Writer_t* decode(Writer_t* obj, Segmt_t* sp, 
	  int k,    /* First burst/pulse to decode */
	  int n,    /* Last burst/pulse to decode */
	  int eos,  /* Last decode call for this segment - true if nonzero */
	  const Job_t* job)
{
    register int np, nb, maxLen = (*obj->type)(sp, NULL);

    if (! isScanSAR(job->mode)) {
	char s[12];
	++n;
	while (k < n) {
	    if (obj->bufSize < obj->bufLen+maxLen && ! R1_flush(obj,job,0))
		return NULL;

	    np = n-k;
	    if (np > R1_PULSE_PER_BURST_MAX)
		np = R1_PULSE_PER_BURST_MAX;

	    obj->bufLen += (*obj->type)(sp, sp->pulse+k, np,
			   k+1, obj->buf+obj->bufLen, 0, obj, job);
	    k += np;
	}
	if (!eos) return obj;

	obj->bufLen += (*obj->type)(sp, sp->pulse, sp->pulse_cnt,
			sp->pulse_cnt, obj->buf+obj->bufLen, 1, obj, job);
	R1_flush(obj, job, 1);

	if (obj->type == R1_SaveAux &&
	   (sprintf(s, "%-8d", sp->pulse_cnt),
	    lseek(obj->fd, (off_t) obj->offset, SEEK_SET) != obj->offset ||
	    write(obj->fd, s, 8) != 8)) {

	    (*job->Log)(job->err, "%s: can't update END_BURST\n", obj->file);
            close(obj->fd);
            obj->fd = -1;
            obj->bufLen = 0;

            return NULL;
	}
	close(obj->fd);
	obj->fd = -1;
	obj->bufLen = 0;

	return obj;
    }
    while (k <= n) {

	np = ((k == n && eos) ? sp->pulse_cnt : sp->burst[k+1]) - sp->burst[k];
	if (k == 0) obj->bad = -1;

	if (obj->bad < 0) {
	    register int  beam = R1_Beam(sp->pulse[sp->burst[k]]);
	    if ((job->mode != SNB && beam != 1) ||
		(job->mode == SNB && beam != 2) || np != sp->burst_len) {
		k++;
		continue;
	    }	
	    obj->bad = k;
	}
	else if (np != sp->burst_len) {
	    if (k == n && eos)
		break;

#ifndef	PULSE_CNT1_QUIT
	    printfLLog(LOG_ERR,
	"Burst #%d has %d pulses, want %d :: %dP/%dB/%d, k %d, n %d, %d\n",
		k-obj->bad+1, np, sp->burst_len,
		sp->pulse_cnt, sp->burst_cnt, sp->burst[k], k,n,eos);
#endif
	}
	if (obj->bufSize < obj->bufLen+maxLen && ! R1_flush(obj, job, 0))
	    return NULL;

	obj->bufLen += (*obj->type)(sp, sp->pulse+sp->burst[k], np,
			k-obj->bad+1, obj->buf+obj->bufLen, 0, obj, job);
	k++;
    }
    if (!eos) return obj;

    if (obj->bad >= 0 && 
       (nb = (job->mode == SNA ? 2 : (job->mode == SNB ? 3 : 4)),
	nb < (k - obj->bad))) {

	register int fd, bufLen;
	char s[12], *p;

	obj->bufLen += (*obj->type)(sp, sp->pulse+sp->burst[obj->bad], np,
			k-obj->bad, obj->buf+obj->bufLen, 1, obj, job);
	R1_flush(obj, job, 1);
#ifdef DEBUG
	sprintf(s, "%-8d",((k - obj->bad + nb-1)/nb)*nb);
#else
	sprintf(s, "%-8d",((k - obj->bad)/nb)*nb);
#endif
	if (lseek(obj->fd, (off_t) obj->offset, SEEK_SET) != obj->offset ||
	    write(obj->fd, s, 8) != 8) {

	    (*job->Log)(job->err, "%s: can't update END_BURST\n", obj->file);
            close(obj->fd);
            obj->fd = -1;
            obj->bufLen = 0;

            return NULL;
	}
	close(obj->fd);
	obj->fd = -1;
	obj->bufLen = 0;

	if (obj->type != R1_SaveEco)
	    return obj;

	if ((fd = open((p = strchr(job->bof_file, ':')) ? p+1 : job->bof_file,
		O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR|S_IRGRP))
	    == -1) {
	    (*job->Log)(job->err, "%s: create failed, %s\n",
			job->bof_file, strerror(errno));
	    return obj;
	}
	bufLen = R1_SaveBof(sp, (const R1_Pulse_t**)sp->pulse,
		 (k > n ? sp->pulse_cnt : sp->burst[n]), sp->burst+obj->bad,
		 k-obj->bad, obj->buf, obj, job);

	if (write(fd, obj->buf, bufLen) != bufLen) {
	    (*job->Log)(job->err, "%s: write failed, %s\n",
			job->bof_file, strerror(errno));
	    close(fd);
	    return NULL;
	}
	close(fd);
    }
    obj->bufLen = 0;
    return obj;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int R1_Decode(Segmt_t* sp, int fr_burst, int to_burst, int last_call,
	      const Job_t* job)
{
    register Writer_t* obj;
    register Decode_t* dp = (Decode_t*)job->sat_data;

    if (decode(&dp->ecoWrt, sp, fr_burst, to_burst, last_call, job) &&
	decode(&dp->repWrt, sp, fr_burst, to_burst, last_call, job) &&
	decode(&dp->auxWrt, sp, fr_burst, to_burst, last_call, job) &&
	decode(&dp->ephWrt, sp, fr_burst, to_burst, last_call, job))
	return 1;

    if ((obj = &dp->ecoWrt)->fd != -1) {
	R1_flush(obj, job, 1);
	close(obj->fd);
	obj->fd = -1;
    }
    if ((obj = &dp->repWrt)->fd != -1) {
	R1_flush(obj, job, 1);
	close(obj->fd, 1);
	obj->fd = -1;
    }
    if ((obj = &dp->auxWrt)->fd != -1) {
	R1_flush(obj, job, 1);
	close(obj->fd);
	obj->fd = -1;
    }
    if ((obj = &dp->ephWrt)->fd != -1) {
	R1_flush(obj, job, 1);
	close(obj->fd);
	obj->fd = -1;
    }
    return 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
void* R1_Init(Job_t* job)
{
    register int i;
    register U32 x;
    Decode_t* p;
    size_t aux_cache_size, eco_cache_size, eph_cache_size, rep_cache_size;

    memset(map, 0, sizeof(map));
    x = R1_SYNC;
    for (i = 1; i <= 16; ++i) {
        x >>= 1;
        map[x & 0xFFFFU] = i;
    }
    if (job->sat_data) {
	free(job->sat_data);
	job->sat_data = NULL;
    }
    if (job->scan_job)
	return (job);

    eco_cache_size = RoundUp(3*1024*1024, 512);
    rep_cache_size = RoundUp(REP_PER_BURST_SIZE*job->max_burst_per_image, 512);
    aux_cache_size = RoundUp(AUX_PER_BURST_SIZE*job->max_burst_per_image, 512);
    eph_cache_size = RoundUp(EPH_PER_BURST_SIZE*job->max_burst_per_image, 512);

    i = RoundUp(sizeof(Decode_t), 512)
      + aux_cache_size + eph_cache_size + eco_cache_size + rep_cache_size;

    if (! (p = (Decode_t*) memalign(512, i))) {
        (*job->Log)(job->err, "No memory for decode cache");
        return NULL;
    }
    p->eco_cache = (char*)p + RoundUp(sizeof(Decode_t), 512);
    p->rep_cache = p->eco_cache + eco_cache_size;
    p->aux_cache = p->rep_cache + rep_cache_size;
    p->eph_cache = p->aux_cache + aux_cache_size;
    p->ref_pulse.num = -1;
    p->ref_pulse.val = 0;
    memset(p->burst_offset, 0, sizeof(p->burst_offset));

    p->ecoWrt.type = R1_SaveEco;
    p->ecoWrt.file = job->eco_file;
    p->ecoWrt.fd = -1;
    p->ecoWrt.ref = &p->ref_pulse;
    p->ecoWrt.bad = -1;
    p->ecoWrt.buf = p->eco_cache;
    p->ecoWrt.bufLen = 0;
    p->ecoWrt.bufSize = eco_cache_size;
    p->ecoWrt.eco_valid = p->eco_valid;
    p->ecoWrt.rep_valid = p->rep_valid;
    p->ecoWrt.burst_offset = p->burst_offset;
    p->ecoWrt.workrep = p->workrep;
    p->ecoWrt.rep = p->rep;

    p->repWrt.type = R1_SaveRep;
    p->repWrt.file = job->rep_file;
    p->repWrt.fd = -1;
    p->repWrt.ref = &p->ref_pulse;
    p->repWrt.bad = -1;
    p->repWrt.buf = p->rep_cache;
    p->repWrt.bufLen = 0;
    p->repWrt.bufSize = rep_cache_size;
    p->repWrt.eco_valid = p->eco_valid;
    p->repWrt.rep_valid = p->rep_valid;
    p->repWrt.burst_offset = p->burst_offset;
    p->repWrt.workrep = p->workrep;
    p->repWrt.rep = p->rep;

    p->auxWrt.type = R1_SaveAux;
    p->auxWrt.file = job->aux_file;
    p->auxWrt.fd = -1;
    p->auxWrt.ref = &p->ref_pulse;
    p->auxWrt.bad = -1;
    p->auxWrt.buf = p->aux_cache;
    p->auxWrt.bufLen = 0;
    p->auxWrt.bufSize = aux_cache_size;
    p->auxWrt.eco_valid = p->eco_valid;
    p->auxWrt.rep_valid = p->rep_valid;
    p->auxWrt.burst_offset = p->burst_offset;
    p->auxWrt.workrep = p->workrep;
    p->auxWrt.rep = p->rep;

    p->ephWrt.type = R1_SaveEph;
    p->ephWrt.file = job->eph_file;
    p->ephWrt.fd = -1;
    p->ephWrt.ref = &p->ref_pulse;
    p->ephWrt.bad = -1;
    p->ephWrt.buf = p->eph_cache;
    p->ephWrt.bufLen = 0;
    p->ephWrt.bufSize = eph_cache_size;
    p->ephWrt.eco_valid = p->eco_valid;
    p->ephWrt.rep_valid = p->rep_valid;
    p->ephWrt.burst_offset = p->burst_offset;
    p->ephWrt.workrep = p->workrep;
    p->ephWrt.rep = p->rep;

    job->sat_data = (void*) p;
    return ((void*) job);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
void R1_Destroy(Job_t* job)
{
    if (job->sat_data) {
	free(job->sat_data);
	job->sat_data = NULL;
    }
}

const Satellite_t R1 = {
    R1_FRAME_SIZE,
    R1_FRAME_SIZE,
    R1_FRAME_SIZE,
    R1_DATA_SIZE,
    R1_HDR_SIZE,
    R1_FRAME_PER_PULSE_MIN,
    R1_FRAME_PER_PULSE_MAX,
    {(U32*)prn_0, (U32*)prn_1, (U32*)prn_2, (U32*)prn_3},
    {-1.0, 0.0, 0.0},	/* No default doppler value - to be calculated */
    { 0.0, 0.0, 0.0},
    R1_Init,
    R1_Destroy,
    R1_Decode,
    R1_Scan,
    R1_NextFrame,
    R1_NewPulse,
    R1_ExtraPulse,
    R1_SameDWP,
    R1_SameAGC,
    R1_GMT,
    R1_DWP,
    R1_AGC,
    R1_RYP,
    R1_PRF,
    R1_LookAngle,
    R1_WindowDuration,
    R1_CAL,
    R1_Mode,
    R1_Beam,
    R1_SameBurst,
    R1_PulseCnt,
};
