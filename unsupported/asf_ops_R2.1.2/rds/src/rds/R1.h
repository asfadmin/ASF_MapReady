/*=============================================================================
 |  @(#)R1.h	1.19 98/02/25 15:17:00
 |
 |  RadarSAT (R1) Pulse Defition.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef _RDS_R1_H_
#define _RDS_R1_H_

#include <sys/types.h>
#include <string.h>
#include <math.h>
#include "Pulse.h"
#include "Tape.h"

static const char sccsid_R1_h[] =
	"@(#)R1.h	1.19 98/02/25 15:17:00";

extern	const Satellite_t	R1;

#define R1_SYNC		0x1ACFFC1DU     /* Frame SYNC Network ordering */
#define R1_AUX		0x352ef853U	/* Auxiliary SYNC code */
#define R1_EPH		0xf0f0ccaaU	/* Ephemeris SYNC code */

#define	R1_SYNC_SIZE	32		/* Unit of bits */
#define R1_HDR_SIZE	10		/* Unit of bytes */
#define R1_DATA_SIZE	311
#define R1_CRC_SIZE	2
#define R1_AUX_SIZE	50
#define	R1_FRAME_SIZE 	(R1_HDR_SIZE + R1_DATA_SIZE + R1_CRC_SIZE)

#define R1_FRAME_PER_PULSE_MIN	18
#define R1_FRAME_PER_PULSE_MAX	42
#define R1_PULSE_PER_BURST_MIN	8
#define R1_PULSE_PER_BURST_MAX	128

/*-------------------*
 |  Telemetry Frame
 *-------------------*/
typedef struct {		/* RadarSat Frame Header */
    U32 sync;    		/* Synchronization marker */
    U32 version         : 2;    /* Version number */
    U32 sc_id           : 10;   /* Spacecraft ID */
    U32 vc_id           : 3;    /* Virtual channel ID */
    U32 ctrl_flag       : 1;    /* Operational control field flag */
    U32 mc_count        : 8;    /* Master channel frame count */
    U32 vc_count        : 8;    /* Virtual channel frame count */
    U32 secondary_flag  : 1;    /* Secondary header flag */
    U32 sync_flag       : 1;    /* Synchronization flag */
    U32 order_flag      : 1;    /* Packet order flag */
    U32 segment_id      : 2;    /* Segment length identifier */
    U32 data_valid      : 11;   /* First header pointer, -1 <=> data valid */
    U8  data[R1_DATA_SIZE];
    U8  crc[R1_CRC_SIZE];

} FrameR1_t;

typedef	struct {		/* Frame Object */
    Frame_t	hdr;
    FrameR1_t   frame;		/* The entire frame in its entirety */

} R1_Frame_t;

/*-------------------*
 |  Auxiliary Header
 *-------------------*/
typedef struct {
    U32 sync;                   /* Auxiliary sync marker */

    U32 image_id;
    U32 payload_mode            : 3;	/* Offset 8 */
    U32 image_status		: 2;
    U32 CAL_status		: 4;
    U32 payload_fault           : 1;
    U32 payload_warning         : 1;
    U32 payload_image_type      : 2;
    U32 payload_stripmap        : 1;
    U32 payload_HPMC_ready      : 1;
    U32 payload_auto_mode       : 1;
    U32 replica_AGC             : 6;
    U32 			: 2;
    U32 CAL_attenuation		: 8;
    U32 chirp_type              : 4;	/* Offset 12 */
    U32 			: 12;
    U32 LNA_temp                : 8;
    U32 subsystem_temp          : 8;
    U32 protector_temp          : 8;	/* Offset 16 */
    U32 CAL_temp		: 8;
    U32 beam_sequence           : 16;

#define EPHEMERIS		20	/* Offset 20 */
    U32 ephemeris	        : 16;

    U32 number_of_beams         : 2;
    U32 sampling_rate           : 2;
    U32 			: 4;
    U32 pulse_cnt1              : 8;
    U32 pulse_cnt2              : 8;	/* Offset 24 */

#define PRF_PERIOD		25	/* Offset to prf_period */
    U32 prf_period              : 13;

    U32 beam_select             : 2;
    U32 			: 1;
    U32 window_start_msb        : 8;
    U32 window_start_lsb        : 4;	/* Offset 28 */
    U32 			: 4;
    U32 window_duration         : 12;
    U32 			: 4;
    U32 roll_error_msb          : 8;
    U32 roll_error_lsb          : 7;	/* Offset 32 */
    U32 roll_valid              : 1;
    U32 pitch_error             : 15;
    U32 pitch_valid             : 1;
    U32 yaw_error_msb           : 8;
    U32 yaw_error_lsb           : 7;	/* Offset 36 */
    U32 yaw_valid               : 1;
    U32 roll_rate               : 16;
    U32 pitch_rate_msb          : 8;
    U32 pitch_rate_lsb          : 8;	/* Offset 40 */
    U32 yaw_rate                : 16;
    U32 sc_days_msb             : 8;
    U32 sc_days_lsb             : 3;	/* Offset 44 */
    U32 sc_secs                 : 17;
    U32 sc_msec                 : 10;
    U32 sc_usec_msb             : 2;
    U32 sc_usec_lsb             : 8;	/* Offset 48 */
    U32 SC_TO2                  : 1;
    U32 replica_present         : 1;
    U32 AGC_setting             : 6;

} AuxR1_t;

#define	R1_PULSE_SIZE_MAX	72 /* seems to work better if double-aligned */

typedef struct {
    Pulse_t	hdr;		/* Generic Pulse Header */
    AuxR1_t	aux;		/* Extracted Auxiliary Data */

} R1_Pulse_t;

extern int ReplicaSamples(const R1_Pulse_t** obj, const Segmt_t* sp);

#endif /*!_RDS_R1_H_ */
