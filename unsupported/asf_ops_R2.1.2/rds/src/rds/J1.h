/*============================================================================
 |  @(#)J1.h	1.7 98/03/06 17:40:26
 |
 |  JERS-1 Pulse Format Definition.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#ifndef _RDS_J1_H_
#define _RDS_J1_H_

#include <malloc.h>
#include <string.h>
#include <sys/types.h>
#include "Pulse.h"
#include "Tape.h"

static const char sccsid_J1_h[] =
	"@(#)J1.h	1.7 98/03/06 17:40:26";

extern	const Satellite_t	J1;

#define J1_SYNC			0xAA99AA5AU	/* MS Part of Format SYNC */
#define J1_SYNC2		0x5A655558U	/* LS Part of Format SYNC */
#define J1_SYNC2_MASK           0xFFFFFFF8U
#define	J1_SYNC_SIZE		60		/* Bits */
#define J1_HDR_SIZE		0
#define	J1_AUX_SIZE		32		/* bytes, SYNC+HK+Frame # */
#define	J1_DATA_SIZE		J1_FRAME_SIZE_MAX
#define J1_FRAME_SIZE 		J1_FRAME_SIZE_MAX
#define J1_FRAME_SIZE_MIN 	((37358-6)/8)
#define J1_FRAME_SIZE_MAX 	(98304/8)	/* ((39844+6)/8) */

#define J1_FRAME_PER_PULSE_MIN	1
#define J1_FRAME_PER_PULSE_MAX	1

/*-------------------*
 |  Telemetry Frame
 *-------------------*/
typedef struct {
    size_t	size;

} FrameJ1_t;

typedef struct {
    Frame_t	hdr;		/* Generic Frame Object Header */
    FrameJ1_t	frame;		/* Frame in its entirety with byte aligned */
    
} J1_Frame_t;

/*--------------------*
 |  Auxiliary Header
 *--------------------*/

typedef struct {		/* House Keeping (HK) Data Block for JERS-1 */
    U32	prf_mode	: 2;	/* PRF ON/OFF */
    U32 prf		: 6;	/* PRF - most significant */
    U32	cal_mode	: 2;	/* Calibration mode ON/OFF */
    U32 obs_mode	: 2;	/* Observation mode ON/OFF */
    U32 stc_pattern	: 10;	/* STC start time change pattern */
    U32 stc_init	: 10;	/* Initial STC start time */

    U32	stc		: 10;	/* STC start time */
    U32	stc_offset	: 6;	/* STC offset time */
    U32	agc_mode	: 2;	/* AGC/MGC */
    U32 agc_const	: 2;	/* AGC time constant */
    U32 agc		: 10;	/* AGC data */
    U32 gc_status_ms	: 2;	/* Gain control status - most significant */

    U32 gc_status_ls	: 8;	/* Gain control status - least significant */
    U32 stp		: 6;	/* STP ATT */
    U32 auto_manual	: 2;	/* Auto or manual */
    U32 obs_start	: 2;	/* Observation start or end */
    U32	combiner	: 6;	/* Combiner switch - most significant */
    U32 rcv_sw		: 2;	/* Receiver SW[1-3] - A or B or unfixed */
    U32	stc_sw		: 2;	/* STC CAL - CAL or OBS mode */
    U32	cal_sw		: 2;	/* CAL SW - CALL or OBS mode */
    U32	standby_mode_ms	: 2;	/* Standby mode selection - most significant */

    U32	standby_mode_ls	: 2;	/* Standby mode selection - least significant*/
    U32 status_change	: 2;	/* Change of status: MGC or AGC */
    U32	freqsyn_mode	: 2;	/* Frequency synthesizer ON/OFF */
    U32	control_mode	: 2;	/* Control unit ON/OFF */
    U32	xmitter_mode	: 2;	/* Transmitter unit ON/OFF */
    U32	receiver_mode	: 2;	/* Receiver unit ON/OFF */
    U32 sigproc_mode	: 2;	/* Signal processor unit ON/OFF */
    U32	power_a_mode	: 2;	/* Power amplifier power source A ON/OFF */
    U32 power_b_mode	: 2;	/* Power amplifier power source B ON/OFF */
    U32 power_r_mode	: 2;	/* Power amplifier power source R ON/OFF */
    U32 stc_init_mode	: 2;	/* STC initial start time: OBS or CAL mode */
    U32 str_mode	: 2;	/* STR ON/OFF */
    U32	str_ms		: 8;	/* STR status - most significant */

    U32	str_ls		: 8;	/* STR status - least significant */
    U32	spare_2		: 2;	/* Unused field */
    U32 framenum_ms	: 22;	/* Most significant 22 bits */

    U32	framenum_ls	: 26;	/* Least significant 26 bits */
    U32 pcm_bit		: 4;	/* PCM bit #0 */
    U32 pcm_clk		: 2;	/* PCM clk #0 */

} AuxJ1_t;

/*-------------------------*
 |  Auxiliary Data Object
 *-------------------------*/
typedef struct {
    Pulse_t	hdr;		/* Generic Pulse Header */
    AuxJ1_t	aux;		/* Extracted Auxiliary data */
    U32		pcm_bit;	/* Holds 13 PCM data bits */
    U32		pcm_clk;	/* Holds 13 PCM clock bits */
    U32		pcm_pos;	/* Holds 13 PCM clock bits */

} J1_Pulse_t;

#define	J1_PULSE_SIZE_MAX	sizeof(J1_Pulse_t)

#endif /*!_RDS_J1_H_ */
