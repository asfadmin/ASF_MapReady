/*=============================================================================
 |  @(#)RDS.h	1.31 98/03/15 12:07:00
 |
 |  Multithreaded Raw Data Scanner (RDS).
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef _RDS_H_
#define _RDS_H_

#include <sys/types.h>
#include <string.h>
#include "pthread_wrapper.h"
#include "Q.h"
#include "Status.h"
#include "Tape.h"
#include "Pulse.h"
#include "E1.h"
#include "R1.h"
#include "J1.h"
#include "odl.h"

static const char sccsid_RDS_h[] =
	"@(#)RDS.h	1.31 98/03/15 12:07:00";

/*------------------------------------------------------------------------*
 |  Tape Buffer - Not a public type, intended for internal use by RDS_t.
 *------------------------------------------------------------------------*/

#define	DCRSI_READ_SIZE	(512 * DCRSI_BLOCK_SIZE)
#define	SONY_READ_SIZE	(16  * SONY_BLOCK_SIZE)
#define	TAPEBUF_SIZE	(SONY_READ_SIZE > DCRSI_READ_SIZE ?\
			 SONY_READ_SIZE : DCRSI_READ_SIZE)
#define	OVERLAP_SIZE	RoundUp(J1_FRAME_SIZE_MAX,4) /* longest of all */
#define	ALIGNMT_SIZE	(((OVERLAP_SIZE + 6*4 + 511)/512)*512 - 6*4)
#define	PADDING_SIZE	(512 - (SONY_READ_SIZE%512) /* + 512 */)
			/* +512 to allow access beyond OVERLAP_SIZE */

typedef struct TapeBuf_t {	/* Must be 512-aligned for direct I/O */
    struct TapeBuf_t* next;	/* Succeeding buffer */
    struct TapeBuf_t* prev;	/* Preceding buffer */
    const  Job_t*     job;
    int	   blk_start;      	/* Start address of this buffer */
    int	   blk_count;      	/* Buffer size of tape blocks */
    U8*	   buf;			/* Point to either 'data' or 'data'+256 */
    U8	   pad0[ALIGNMT_SIZE];	/* Padding to 512-align 'buf' */
    U8	   data[TAPEBUF_SIZE];	/* Must be 512-aligned for direct I/O */
    U8	   pad1[PADDING_SIZE];

} TapeBuf_t;

#define	E1_PULSE_PER_READ_MAX	\
    ((((TAPEBUF_SIZE+E1_FRAME_SIZE-1)/E1_FRAME_SIZE) +\
           E1_FRAME_PER_PULSE_MIN-1) /E1_FRAME_PER_PULSE_MIN + 1)

#define	R1_PULSE_PER_READ_MAX	\
    ((((TAPEBUF_SIZE+R1_FRAME_SIZE-1)/R1_FRAME_SIZE) +\
           R1_FRAME_PER_PULSE_MIN-1) /R1_FRAME_PER_PULSE_MIN + 1)

#define	J1_PULSE_PER_READ_MAX	\
    ((((TAPEBUF_SIZE+J1_FRAME_SIZE_MIN-1)/J1_FRAME_SIZE_MIN) +\
           J1_FRAME_PER_PULSE_MIN-1) /J1_FRAME_PER_PULSE_MIN + 1)

#ifndef	PULSE_PER_READ_MAX
#define	PULSE_PER_READ_MAX	\
	(R1_PULSE_PER_READ_MAX > E1_PULSE_PER_READ_MAX && \
	 R1_PULSE_PER_READ_MAX > J1_PULSE_PER_READ_MAX \
		? R1_PULSE_PER_READ_MAX : \
	(E1_PULSE_PER_READ_MAX > R1_PULSE_PER_READ_MAX && \
	 E1_PULSE_PER_READ_MAX > J1_PULSE_PER_READ_MAX \
		? E1_PULSE_PER_READ_MAX : J1_PULSE_PER_READ_MAX))
#endif

#ifndef	BURST_PER_READ_MAX
#define BURST_PER_READ_MAX	\
	((R1_PULSE_PER_BURST_MIN-1 + R1_PULSE_PER_READ_MAX) /\
	  R1_PULSE_PER_BURST_MIN + 1)
#endif
 
#ifndef	PCM_BIT_PER_READ_MAX
#define PCM_BIT_PER_READ_MAX	(2*PULSE_PER_READ_MAX)
#endif

/*--------------*
 |  RDS Class
 *--------------*/

typedef struct RDS_t {
    U8		sony_buf[256];		/* Needed for with direct I/O */
    U8          overlap[OVERLAP_SIZE];  /* Last frame read most of the time */
    U8*		sony_256;		/* Point to sony_buf for JERS */
    char*	pulsebuf;		/* [PULSE_MAX][PULSE_SIZE_MAX] */
    char*	sca_cache;

    Job_t 	job;
    Status_t	status;
    int		scanner_max;
    pthread_t*	scanner;
    pthread_t	merger;
    pthread_t	reader;

    TapeBuf_t**	tapebuf_Q_;
    Merge_t**	scan_Q_;
    Merge_t**	merge_Q_;
    Q_t		tapebuf_Q;
    Q_t		scan_Q;
    Q_t		merge_Q;

    int 	merge_max;
    int		merge_cnt;
    Merge_t*	merge;

    int 	segmt_max;
    int		segmt_cnt;
    Segmt_t*	segmt;

    int		pulse_to_check;
    int		pulse_max;
    int		pulse_cnt;
    Pulse_t**	pulse;

    int		burst_per_image_max;
    int		burst_max;
    int		burst_cnt;
    int*	burst;

    int		dwp_per_burst_max;
    int		dwp_max;
    int		dwp_cnt;
    int*	dwp;

    int		agc_per_burst_max;
    int		agc_max;
    int		agc_cnt;
    int*	agc;

    int 	pcm_max;		/* Size of PCM bit buffer in bytes */
    U32*	pcm;			/* PCM bit buffer */

    int		ppr_max;
    int		ppr_cnt;
    PPR_t*	ppr;

    Tape_t	tape;
    int		tapeerr;		/* tape_errlim is on */
    int		tapepos;
    int		tapeblk_max;
    int		tapebuf_max;
    TapeBuf_t*	tapebuf_prev;		/* Used only by writer thread */
    TapeBuf_t*	tapebuf;
    TapeBuf_t*	lastbuf;		/* Tapebuf last read in */

    /* For debugging purpose */
    char	tapedump[256];		/* Tape dump file base name */
    int		tapedump_err;		/* So we don't flood user with error */
    int		tapedump_fd;		/* Tape dump file descriptor */
    int		tapedump_nf;		/* Number of files created so far */
    int		tapedump_nb;		/* Current tape dump file size */

} RDS_t;

extern  RDS_t*  	RDS_malloc(const char* configfile, char* err);
extern	RDS_t*		RDS_Init(RDS_t*, ODL job);
extern	RDS_t*		RDS_Scan(RDS_t*);
extern	void		RDS_free(RDS_t*);
extern	void 		RDS_Destroy(RDS_t*);
extern	void		RDS_Reset(RDS_t* rds);
extern	int		RDS_Cleanup(const RDS_t*, const char* savedir,
				    char* errorbuf);
extern	int		RDS_Position(const RDS_t*);
extern	int		RDS_Errno(const RDS_t*);
extern	const char*	RDS_Error(const RDS_t*);

#define PPR_MAX		100     /* Theoretical max for entire orbit */
#define	PULSE_SIZE_MAX	(R1_PULSE_SIZE_MAX > E1_PULSE_SIZE_MAX ? \
			 R1_PULSE_SIZE_MAX : E1_PULSE_SIZE_MAX) 
#define	PULSE_PER_READ_MIN 	56

#endif /*!_RDS_H_ */
