/*============================================================================
 |  @(#)E1.c	1.53 98/04/06 10:12:12
 |
 |  ERS-1/2 High Rate Pulse Format Definition.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#include <math.h>
#include <syslog.h>
#include "Pulse.h"
#include "E1.h"

static const char sccsid_E1_c[] =
        "@(#)E1.c	1.53 98/04/06 10:12:12";

/*------------------------------------------------*
 |  Constants Used by E1_Scan() & E1_NextFrame()
 *------------------------------------------------*/
#define R32(_x)			RoundUp(_x, sizeof(int))
#define	FIRST_BIT		((R32(E1_FRAME_SIZE)-E1_FRAME_SIZE)*8 + 1)
#define M_SYNC			0xFFFFFF00U
#define M_FIRST			0x3F	/* First frame in a format */
#define V_FIRST			0x00 
#define M_FDATA			0xFF	/* First frame in a SAR format */
#define V_FDATA			0x80

static	U8 map[64*1024];	/* Frame Sync Look-up Table */

/*-----------------------------------*
 |  PRN Table Used for Unscrambling
 *-----------------------------------*/
static const U8 prn_0[] = {
#include "E1_prn.h"
};
static const U8 prn_1[] = {0,
#include "E1_prn.h"
,0,0,0};
static const U8 prn_2[] = {0,0,
#include "E1_prn.h"
,0,0};
static const U8 prn_3[] = {0,0,0,
#include "E1_prn.h"
,0};

#define IsValidPRF(_p)	((_p) >= 1640 && (_p) <= 1720)
#define PRF(_p)		(1/((((E1_Pulse_t*)(_p))->aux.prf_period+2)*210.94E-9))
#define DWP(_p)		((E1_Pulse_t*)(_p))->aux.window_start

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Frame_t* E1_Scan(Scan_t* p, const Job_t* job)
{
    const U32 *w = p->buf;
    register int n = p->n, i = p->i;
    register U32 y, x = !n ? w[i] : (w[i] & (0xFFFFFFFFU >> n));

#ifdef	DEBUG
    printf("E1_Scan %d: i %d, n %d, len %d\n", p->blk_start, p->i, p->n,p->len);
#endif

    while (i <= p->len) {
        if (n = map[x >> 16]) {
	    if (n == 16) {
		if ((x & M_SYNC) == E1_SYNC) {
		    n = 0;
		    break;
		}
	    }
            else if (((y = (x>>(16-n))|(w[i-1]<<(16+n))) & M_SYNC) == E1_SYNC) {
		--i;
		n += 16;
		x = y;
		break;
	    }
	}
        if ((n = map[x & 0xFFFFU]) &&
            ((y = (x<<n)|(w[i+1]>>(32-n))) & M_SYNC) == E1_SYNC) {
	    x = y;
            break;
        }
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
    p->n = n;
    p->i = i;
    p->frame.bit_error = 0;
    p->frame.frame_cnt = ((x & M_FDATA) == V_FDATA);
    Scan_Position(&p->frame.start, p, job);

#ifdef	DEBUG
printf("New Frame: <%d, %d>, i %d, n %d, X %.8X, %s\n",
        p->frame.start.blk, p->frame.start.bit, p->i, p->n, x,
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
Frame_t* E1_NextFrame(Scan_t* p, const Job_t* job, const Pulse_t* pr)
{
    register int n, i;
    register U32 *w = p->buf;
    U32 z, bit_error = 0;

    i = p->i + ((n = p->n + E1_FRAME_SIZE*8) >> 5);
    n &= 0x1F;

    if (i > p->len || (i == p->len && n >= FIRST_BIT) ||
	(((z = !n ? w[i] : ((w[i]<<n) | (w[i+1]>>(32-n)))) & M_SYNC) != E1_SYNC
	 && DIF(z & M_SYNC, E1_SYNC, &bit_error) > (job->sync_diff * 3)/4) ||
	(pr && (p->frame.frame_cnt = ((z & M_FIRST) == V_FIRST)) &&
	 pr->frame_cnt >= job->aux_recover * E1_FRAME_PER_PULSE_MAX))
    {
	if (p->i < p->len || (p->i == p->len && p->n < FIRST_BIT)) {
	    p->n = n;
	    p->i = i;
	    Scan_Position(&p->frame.start, p, job);
	}
#ifdef DEBUG
printf("NextFrame %d/%.5d: i %d, n %d, %d, err %d, z %X, %d/%d len %d, %d\n",
        p->frame.start.blk, p->frame.start.bit, p->i, p->n,
        p->frame.frame_cnt, bit_error, z, i, n, p->len, pr->frame_cnt);
#endif
	return NULL;
    }
    p->i = i;
    p->n = n;
    Scan_Position(&p->frame.start, p, job);
    p->frame.frame_cnt = ((z & M_FIRST) == V_FIRST);
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
int E1_NewPulse(Pulse_t* pp, Scan_t* fp, Segmt_t* sp, const Job_t* job,
		const Pulse_t* pr)
{
    if (! pp) {
	const U8* bp = (U8*)(fp->buf + fp->i) + (fp->n>>3);
	const int nl = fp->n & 7;

	return (pr
	? ( (pr->frame_cnt % E1_FRAME_PER_PULSE_MIN)? 0 : -1 )
	: ( (fp->frame.frame_cnt
	    && 0xAA == (prn_0[16] ^
		(nl ? (((bp[16]<<nl)|(bp[17]>>(8-nl)))&0xFF) : bp[16]))
	    && 0x88 != (prn_0[22] ^
		(nl ? (((bp[22]<<nl)|(bp[23]>>(8-nl)))&0xFF) : bp[22])) )
	    ? -1 : 0 ));
    }
    memcpy( &((E1_Pulse_t*)pp)->aux,
	Unscramble(E1.PRN, fp->buf + fp->i, fp->n, E1_HDR_SIZE,
	    job->scan_job ? E1_AUX_SIZE : E1_DATA_SIZE),
	E1_AUX_SIZE);

    pp->frame_cnt = 1;
    pp->bit_error = fp->frame.bit_error;
    pp->start = fp->frame.start;
    pp->tapebuf = fp->frame.tapebuf;

    return -1;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
#ifdef	DEBUG
static
void BreakPulse(Pulse_t* pp_new, Pulse_t* pp, const Job_t* job, int frame_cnt,
	 	int has_tapebuf)
{
    register int n;
    register U8* buf;

    *pp_new = *pp;
    pp->frame_cnt = frame_cnt;
    n = pp->start.bit + pp->frame_cnt*E1_FRAME_SIZE*8;

    pp_new->bit_error  = 0;
    pp_new->frame_cnt -= frame_cnt;
    pp_new->start.blk += n / (job->blk_size*8);
    pp_new->start.bit  = n % (job->blk_size*8);
    buf = has_tapebuf
	? (*job->Buf)(&pp_new->tapebuf, pp_new->start.blk, pp_new->start.bit)
	: NULL;
    memcpy(
        &((E1_Pulse_t*)pp_new)->aux,
        job->scan_job
	    ? (buf ? (void*) Unscramble(E1.PRN, (U32*)((U32) buf&(~3)),
			((U32) buf&3)*8 + (pp_new->start.bit&7),
			E1_HDR_SIZE, E1_AUX_SIZE)
		   : &((E1_Pulse_t*)pp)->aux)
	    : (buf + E1_HDR_SIZE),
        E1_AUX_SIZE
    );
    if (! buf && job->scan_job) {
        /*  Zero out the S/C time so we can later interpolate its GMT */
	((U32*) &((E1_Pulse_t*)pp_new)->aux)[3] = 0;
    }
}
#endif

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_ExtraPulse(Segmt_t* sp, Pulse_t** free_pulse, size_t free_cnt,
		  const Job_t* job, const int tapebuf_valid)
{
    register Pulse_t* pp = sp->pulse[sp->pulse_cnt-1];
    register int i, np = pp->frame_cnt/E1_FRAME_PER_PULSE_MIN - 1;

    for (i = 0; i < np && free_cnt; ++i) {
	register U8* buf;
	register int n = pp->start.bit+E1_FRAME_PER_PULSE_MIN*E1_FRAME_SIZE*8;

	*free_pulse[i] = *pp;
	pp->frame_cnt = E1_FRAME_PER_PULSE_MIN;
	pp = free_pulse[i];
	pp->frame_cnt -= E1_FRAME_PER_PULSE_MIN;
	pp->start.blk += n / (job->blk_size*8);
	pp->start.bit  = n % (job->blk_size*8);
	pp->bit_error  = 0;

	buf = tapebuf_valid
	    ? (*job->Buf)(&pp->tapebuf, pp->start.blk, pp->start.bit)
	    : NULL;
	memcpy( &((E1_Pulse_t*) pp)->aux,
	    job->scan_job
		? (buf ? (void*)Unscramble(E1.PRN, (U32*)((U32) buf&(~3)),
				((U32) buf&3)*8 + (pp->start.bit&7),
				E1_HDR_SIZE, E1_AUX_SIZE)
		       : &((E1_Pulse_t*) sp->pulse[sp->pulse_cnt-1])->aux)
		: (buf + E1_HDR_SIZE),
            E1_AUX_SIZE
	);
	/*  Zero out S/C time so we can later interpolate its GMT */
	if (! buf && job->scan_job)
	    ((U32*) &((E1_Pulse_t*)pp)->aux)[3] = 0;
	--free_cnt;
    }
    return i;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_PulseCnt(const Pulse_t* obj)
{
    return 1;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_Beam(const Pulse_t* obj)
{
    return 0;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
Mode_t E1_Mode(const Segmt_t* sp, const Job_t* job)
{
    return (job->platform[1] == '1' ? ES1 :
	   (job->platform[1] == '2' ? ES2 : MODE_INVALID));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_AGC(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    return 0;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_DWP(double *delay, PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    register int dwp,
	i   = obj - (PulseList_t)sp->pulse,
	max = sp->pulse_cnt-1+sp->pulse_0;
    if (i == 0) ++i;
    while (i < max &&
	(((E1_Pulse_t*) sp->pulse[i])->aux.sc_time == 0 ||
	(dwp = DWP(sp->pulse[i])) != DWP(sp->pulse[i+1]) || i < 2 ||
	 dwp != DWP(sp->pulse[i-2])))
	++i;
    if (i == max) dwp = DWP(obj[0]);
    *delay = dwp * job->e_dwp_b - job->e_dwp_a;
    return dwp;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_SameBurst(const Pulse_t* obj, const Pulse_t* src)
{
    return -1;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_SameAGC(const Pulse_t* obj, const Pulse_t* src)
{
    return -1;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int E1_SameDWP(const Pulse_t* obj, const Pulse_t* src)
{
    const int x = DWP(obj);
    const int y = DWP(src);
    return (x == 0 || y == 0 ? 1 : (x == y ? -1 : 0));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
double E1_PRF(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    GMT gmt;
    register int i = (obj - (PulseList_t)sp->pulse)+1;
    register double prf = 1;

    if (i > sp->pulse_cnt+sp->pulse_0-3)
	i = sp->pulse_cnt+sp->pulse_0-3;

    while (--i >= sp->pulse_0)
	if (((E1_Pulse_t*) sp->pulse[i])->aux.sc_time != 0 &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i+1])->aux.prf_period &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i+2])->aux.prf_period &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i+3])->aux.prf_period &&
	    (prf = PRF(sp->pulse[i]), IsValidPRF(prf)))
	    return prf;

    i = obj - (PulseList_t)sp->pulse;
    if (i < sp->pulse_0+3)
	i = sp->pulse_0+3;
    while (++i < sp->pulse_cnt+sp->pulse_0)
	if (((E1_Pulse_t*) sp->pulse[i])->aux.sc_time != 0 &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i-1])->aux.prf_period &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i-2])->aux.prf_period &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i-3])->aux.prf_period &&
	    (prf = PRF(sp->pulse[i]), IsValidPRF(prf)))
	    return prf;

    printfLLog(LOG_ERR, "Pulse %d/%.5d: Bad PRF %f, S/C Time %u\n",
	BLKNUM(((E1_Pulse_t*) obj[0])->hdr.start.blk,sp,job),
	BITNUM(((E1_Pulse_t*) obj[0])->hdr.start.bit,sp,job),
	prf = PRF(obj[0]), ((E1_Pulse_t*) obj[0])->aux.sc_time);
    return prf;
}
    
/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |	Extract the format number from the auxiliary header of given pulse.
 |	Assuming no corruption, the returned format number should be one 
 |	greater than that of the preceding pulse.
 |
 |	Search backward then forward for 2 consecutive pulses with identical
 |	PRF but different by 1 in format number.
 |
 *---------------------------------------------------------------------------*/
#define	FormatNum(sp, ith_pulse) \
	(((E1_Pulse_t*) (sp)->pulse[ith_pulse])->aux.fmt_cnt_ms * 0x10000 +\
	 ((E1_Pulse_t*) (sp)->pulse[ith_pulse])->aux.fmt_cnt_ls)

int E1_GMT(GMT* gmt, PulseList_t pp, Segmt_t* sp, const Job_t* job)
{
    register double prf;
    register int n = 0, i = (pp - (PulseList_t)sp->pulse)+1;

    if (sp->gmt_start.yr != 0 && sp->prf > 0) {
	register int k, k0 = FormatNum(sp, sp->pulse_0),
			kN = k0+sp->pulse_cnt;

	if (i == sp->pulse_0+sp->pulse_cnt)
	    if (((E1_Pulse_t*) sp->pulse[--i])->aux.sc_time != 0)
		++n;

	while (--i >= sp->pulse_0) {
	    if (((E1_Pulse_t*) sp->pulse[i])->aux.sc_time == 0)
		continue;

	    k = FormatNum(sp,i);
	    if (k >= k0 && k < kN && k == FormatNum(sp,i+1)-1)
		break;
	    ++n;
	}
	if (i < sp->pulse_0)
	    k = k0;

	*gmt = sp->gmt_start;
	gmt_add(gmt, (k-k0+n)/sp->prf);

	return (int) gmt;
    }
    if (i == sp->pulse_0+sp->pulse_cnt)
	if (((E1_Pulse_t*) sp->pulse[--i])->aux.sc_time != 0)
	    ++n;

    while (--i >= sp->pulse_0) {
	if (((E1_Pulse_t*) sp->pulse[i])->aux.sc_time == 0)
	    continue;

	if (i < sp->pulse_0+sp->pulse_cnt-3 &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i+1])->aux.prf_period &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i+2])->aux.prf_period &&
	    ((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
	    ((E1_Pulse_t*) sp->pulse[i+3])->aux.prf_period &&
	    FormatNum(sp,i) == FormatNum(sp,i+1)-1 &&
	    FormatNum(sp,i) == FormatNum(sp,i+2)-2 &&
	    FormatNum(sp,i) == FormatNum(sp,i+3)-3 &&
	    (prf = PRF(sp->pulse[i]), IsValidPRF(prf)))
	    break;
	++n;
    }
    if (i < sp->pulse_0) {
	n = -1;
	i = pp - (PulseList_t)sp->pulse;
	while (++i < sp->pulse_0+sp->pulse_cnt) {
	    if (((E1_Pulse_t*) sp->pulse[i])->aux.sc_time == 0)
		continue;

	    if (i >= sp->pulse_0+3 &&
		((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
		((E1_Pulse_t*) sp->pulse[i-1])->aux.prf_period &&
		((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
		((E1_Pulse_t*) sp->pulse[i-2])->aux.prf_period &&
		((E1_Pulse_t*) sp->pulse[i])->aux.prf_period ==
		((E1_Pulse_t*) sp->pulse[i-3])->aux.prf_period &&
		FormatNum(sp,i) == FormatNum(sp,i-1)+1 &&
		FormatNum(sp,i) == FormatNum(sp,i-2)+2 &&
		FormatNum(sp,i) == FormatNum(sp,i-3)+3 &&
		(prf = PRF(sp->pulse[i]), IsValidPRF(prf)))
		break;
	    --n;
	}
	if (i == sp->pulse_cnt+sp->pulse_0) {
	    i = pp - (PulseList_t)sp->pulse;
	    n = 0;
	    prf = 1;
            printfLLog(LOG_ERR,"Pulse %d/%.5d: bad GMT, S/C time %u, format %u",
		sp->pulse[i]->start.blk, sp->pulse[i]->start.bit,
		((E1_Pulse_t*)sp->pulse[i])->aux.sc_time, FormatNum(sp,i));
	}
    }
    *gmt = job->tcf.gmt;
    gmt_add(gmt, ((double) ((E1_Pulse_t*) sp->pulse[i])->aux.sc_time -
	          (double) job->tcf.bt)*(job->tcf.delta+0.47)*1.0E-9 +
		 n/PRF(sp->pulse[i]));
    return (int) gmt;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
double E1_WindowDuration(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    return job->e_win_duration;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
double E1_LookAngle(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    register int k;
    for (k = 0; k < LOOKTABLE_MAX; ++k)
        if (looktable[k].rmin >= job->rev  &&
            looktable[k].rmax <= job->rev)
            break;
    if (k == LOOKTABLE_MAX) 
        k = 0;
    return looktable[k].beam[job->platform[1] == '1' ? BEAM_E1 : BEAM_E2];
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |	Confirmed no collision, i.e., there exists no x, y such that
 |	x != y && map[x] == map[y].
 *---------------------------------------------------------------------------*/
static
void* E1_Init (Job_t* job)
{
    register U32 n;
    memset(map, 0, sizeof(map));
    for (n = 0; n < 256; ++n) {
	register U32 i, x = E1_SYNC | n;
	for (i = 1; i <= 16; ++i) {
	    x >>= 1;
	    map[x & 0xFFFFU] = i;
	}
    }
    return ((void*) job);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
void E1_Destroy(Job_t* job)
{
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int E1_CAL(double* cal, Segmt_t* sp, const Job_t* job)
{
    return ((int) cal);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int E1_Decode(Segmt_t* sp, int k, int n, int f, const Job_t* job)
{
    return 1;
}

const Satellite_t E1 = {
    E1_FRAME_SIZE,
    E1_FRAME_SIZE,
    E1_FRAME_SIZE,
    E1_DATA_SIZE,
    E1_HDR_SIZE,
    E1_FRAME_PER_PULSE_MIN,
    E1_FRAME_PER_PULSE_MAX,
    {(U32*)prn_0, (U32*)prn_1, (U32*)prn_2, (U32*)prn_3},
    {    0.0,     0.0,     0.0},
    {-2200.0, -2200.0, -2200.0},
    E1_Init,
    E1_Destroy,
    E1_Decode,
    E1_Scan,
    E1_NextFrame,
    E1_NewPulse,
    E1_ExtraPulse,
    E1_SameDWP,
    E1_SameAGC,
    E1_GMT,
    E1_DWP,
    E1_AGC,
    get_RYP,
    E1_PRF,
    E1_LookAngle,
    E1_WindowDuration,
    E1_CAL,
    E1_Mode,
    E1_Beam,
    E1_SameBurst,
    E1_PulseCnt
};
