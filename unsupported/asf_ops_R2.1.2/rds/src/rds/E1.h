/*============================================================================
 |  @(#)E1.h	1.9 98/02/18 13:49:46
 |
 |  ERS-1/2 High Rate Pulse Format Definition.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#ifndef _RDS_E1_H_
#define _RDS_E1_H_

#include <malloc.h>
#include <string.h>
#include <sys/types.h>
#include "Pulse.h"
#include "Tape.h"

static const char sccsid_E1_h[] =
	"@(#)E1.h	1.9 98/02/18 13:49:46";

extern	const Satellite_t	E1;

#define E1_SYNC		((unsigned int)(0xFAF320U<<8))	/* Pad with zero */
#define E1_HDR_SIZE	6
#define	E1_DATA_SIZE	250
#define	E1_AUX_SIZE	28
#define E1_FRAME_SIZE 	(E1_HDR_SIZE + E1_DATA_SIZE)

#define E1_FRAME_PER_PULSE_MIN	29
#define E1_FRAME_PER_PULSE_MAX	(2*E1_FRAME_PER_PULSE_MIN)

/*-------------------*
 |  Telemetry Frame
 *-------------------*/
typedef struct {
    U32 sync		: 24;	/* Synchronization code */
    U32 data_valid	: 2;	/* 1 => format contains zero data */
#define E1_DATA		2	/* 2 => format contains SAR data */

    U32 frame_cnt	: 6;	/* Range 0 .. 28; 0 => 1st frame in format */
    U32	pulse_cnt	: 16;	/* Count number of formats; 0 at power-on */
    U8  data[E1_DATA_SIZE];	/* Data are scrambled from hereon */

} FrameE1_t;

typedef struct {
    Frame_t	hdr;		/* Generic Frame Object Header */
    FrameE1_t	frame;		/* Frame in its entirety with byte aligned */
    
} E1_Frame_t;


/*--------------------*
 |  Auxiliary Header
 *--------------------*/
typedef struct {		/* Scrambled Auxiliary Data */
    U8  pkt_cnt;		/* Packet counter; 0 at power-on */
    U8	fmt_cnt;		/* Range 1 .. 48 per packet; 0 => new packet */
    U8  pkt_data[8];		/* Total data = 48 formats = 48*8 bytes */

    U8	fmt_id;			/* Fixed code = 0xAA */

    U32 image_mode	: 1;	/* Mode of operation */
#define E1_OBRC		1
#define E1_OGRC		0	/* Nominal mode */
#define E1_NOMINAL	M_OGRC

    U32 orbit_id	: 4;	/* Range 0 .. 15 => orbit number 1 to 16 */
    U32 spare_1		: 3;

    U32	sc_time;		/* ICU on-board time - 32-bit aligned */

    U8  data_type;		/* Activity task */
#define	E1_NOISE	0x88	/* Noise; no calibration */
#define	E1_CAL_EM	0x99	/* No echo; cal. drift (used for EM only) */
#define	E1_CAL_EC	0xA9	/* Echo; cal. drift */
#define	E1_REPLICA	0xAA	/* Echo; replica */
#define E1_ECHO		0xA0	/* Echo; no replica (because of OBRC) */

    U32 echo_valid	: 1;	/* 1 => Echo data valid, 0 => invalid */
    U32 repcal_valid	: 1;	/* 1 => Cal/Replica data valid, 0 => invalid */
    U32 noise_first	: 1;	/* 1 => first 4 formats of noise data */
    U32 repcal_first	: 1;	/* 1 => first 4 formats of cal/replica data */
    U32 echo_first	: 1;	/* 1 => first 4 formats of echo data */
    U32 spare_2		: 3;

    U32 fmt_cnt_ms	: 16;	/* Update every format, 0 => first format */
    U32	fmt_cnt_ls	: 16;

    U32 window_start	: 16;	/* Update every 15s with 4th PRI interrupt */
    U32 prf_period	: 16;	/* Pulse repetition interval */
    U32 CAL_setting	: 8;	/* Calibration attenuation setting */
    U32 AGC_setting	: 8;	/* Receive gain setting */

} AuxE1_t;


/*-------------------------*
 |  Auxiliary Data Object
 *-------------------------*/
typedef struct {
    Pulse_t	hdr;		/* Generic Pulse Header */
    AuxE1_t	aux;		/* Extracted Auxiliary data */

} E1_Pulse_t;

#define	E1_PULSE_SIZE_MAX	sizeof(E1_Pulse_t)

/*------------------*
 |  ERS-2 == ERS-1
 *------------------*/
#define E2              E1

#define E2_SYNC         E1_SYNC
#define E2_SYNC_SIZE	E1_SYNC_SIZE
#define E2_HDR_SIZE     E1_HDR_SIZE
#define E2_DATA_SIZE    E1_DATA_SIZE
#define E2_FRAME_SIZE   E1_FRAME_SIZE
#define E2_AUX_SIZE     E1_AUX_SIZE

#define E2_FRAME_PER_PULSE_MIN	E1_FRAME_PER_PULSE_MIN 
#define E2_FRAME_PER_PULSE_MAX	E1_FRAME_PER_PULSE_MAX
#define E2_PULSE_PER_READ_MIN	E1_PULSE_PER_READ_MIN
#define	E2_PULSE_PER_READ_MAX	E1_PULSE_PER_READ_MAX
#define	E2_PULSE_SIZE_MAX	E1_PULSE_SIZE_MAX

#define E2_DATA         E1_DATA
#define E2_OBRC         E1_OBRC
#define E2_OGRC         E1_OGRC  
#define E2_NOMINAL      E1_NOMINAL

#define E2_NOISE        E1_NOISE
#define E2_CAL_EM       E1_CAL_EM
#define E2_CAL          E1_CAL
#define E2_REPLICA      E1_REPLICA
#define E2_ECHO         E1_ECHO

typedef FrameE1_t       FrameE2_t;
typedef AuxE1_t         AuxE2_t;
typedef E1_Frame_t      E2_Frame_t;
typedef E1_Pulse_t      E2_Pulse_t;

#endif /*!_RDS_E1_H_ */
