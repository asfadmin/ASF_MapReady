/*============================================================================
 |  @(#)J1.c	1.24 98/04/06 10:11:27
 |
 |  JERS-1 Pulse Format Definition.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#include <math.h>
#include <syslog.h>
#include <stdlib.h>
#include "Pulse.h"
#include "J1.h"

static const char sccsid_J1_c[] =
        "@(#)J1.c	1.24 98/04/06 10:11:27";

/*------------------------------------------------*
 |  Constants Used by J1_Scan() & J1_NextFrame()
 *------------------------------------------------*/
#define R32(_x)			RoundUp(_x, sizeof(int))
#define	PCM_SYNC_MASK		0xFFFFFF00U
#define PCM_SYNC_SIZE        	24
#define PCM_SYNC             	0xFAF32000U     /* Same as ERS frame sync */
#define	PCM_FREQ		2048.0
#define OBS_DATA_SIZE		(2*1536)
#define	CAL_DATA_SIZE		(2*1536)
#define FRN_DATA_SIZE		(2*24)
#define	PCM_DATA_SIZE		(2*3)
#define	HK_DATA_SIZE		(2*69)
#define	TIME_RATE		((double)1-0.097/86400) /*adjust for clk drift*/
#define isON(_mode)    		((_mode) != 0)
#define	FrameNum(_ap)		\
       ((IQmap[ (_ap)->framenum_ls & 0x3FF]) | \
	(IQmap[((_ap)->framenum_ls >> 10) & 0x3FF] << 5) | \
	(IQmap[ (_ap)->framenum_ls >> 20] << 10) | \
	(IQmap[ (_ap)->framenum_ms & 0x3FF] << 13) | \
	(IQmap[((_ap)->framenum_ms >> 10) & 0x3FF] << 18) | \
	(IQmap[ (_ap)->framenum_ms >> 20 ] << 23))

static U8 map[64*1024];		/* Format Sync Look-up Table */
static U8 PCmap[64*1024];	/* PCM Sync Lookup Table */

/*  Buried within Scan_t Data Structure
 */
typedef struct {
    AuxJ1_t	aux;
    int		frame_size;	/* Last detected size */
    int		nominal_size;	/* Nominal size == 38580 */
    int		bit_error;	/* Last detected bit error count */

} J1_Scan_t;

static const struct {
    double	prf;

} prf_table[] =  		/* PRF Look-up Table */
{
    /* 00  00 00 00 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 01  00 00 01 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 02  00 00 10 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 03  00 00 11 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 04  00 01 00 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 05  00 01 01 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 06  00 01 10 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 07  00 01 11 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 08  00 10 00 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 09  00 10 01 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 10  00 10 10 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 11  00 10 11 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 12  00 11 00 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 13  00 11 01 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 14  00 11 10 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 15  00 11 11 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 16  01 00 00 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 17  01 00 01 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 18  01 00 10 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 19  01 00 11 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 20  01 01 00 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 21  01 01 01 */	{1505.8 /* (30+69+24+18471+1328)*2 39846, 39840 */},
    /* 22  01 01 10 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 23  01 01 11 */	{1530.1 /* (30+69+24+18471+1012)*2 38580, 38586 */},
    /* 24  01 10 00 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 25  01 10 01 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 26  01 10 10 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 27  01 10 11 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 28  01 11 00 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 29  01 11 01 */	{1555.2 /* (30+69+24+18471+ 696)*2 38580, 38586 */},
    /* 30  01 11 10 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 31  01 11 11 */	{1581.1 /* (30+69+24+18471+ 380)*2 38580, 38586 */},
    /* 32  10 00 00 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 33  10 00 01 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 34  10 00 10 */	{1646.75537 /* 38580 */},
    /* 35  10 00 11 */	{1646.75537 /* 38580 */},
    /* 36  10 01 00 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 37  10 01 01 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 38  10 01 10 */	{1646.75537 /* 38580, 38586 */},
    /* 39  10 01 11 */	{1646.75537 /* 38580, 38586 */},
    /* 40  10 10 00 */	{   0.0 /* 0 */},
    /* 41  10 10 01 */	{   0.0 /* 0 */},
    /* 42  10 10 10 */	{   0.0 /* 0 */},
    /* 43  10 10 11 */	{   0.0 /* 0 */},
    /* 44  10 11 00 */	{   0.0 /* 0 */},
    /* 45  10 11 01 */	{   0.0 /* 0 */},
    /* 46  10 11 10 */	{   0.0 /* 0 */},
    /* 47  10 11 11 */	{   0.0 /* 0 */},
    /* 48  11 00 00 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 49  11 00 01 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 50  11 00 10 */	{1646.75537 /* , 38580, 38586 */},
    /* 51  11 00 11 */	{1646.75537 /* , 38580, 38586 */},
    /* 52  11 01 00 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 53  11 01 01 */	{1606.0 /* (30+69+24+18471+  85)*2 38580, 38586 */},
    /* 54  11 01 10 */	{1646.75537 /* 38580, 38586 */},
    /* 55  11 01 11 */	{1646.75537 /* 38580, 38586 */},
    /* 56  11 10 00 */	{   0.0 /* 0 */},
    /* 57  11 10 01 */	{   0.0 /* 0 */},
    /* 58  11 10 10 */	{   0.0 /* 0 */},
    /* 59  11 10 11 */	{   0.0 /* 0 */},
    /* 60  11 11 00 */	{   0.0 /* 0 */},
    /* 61  11 11 01 */	{   0.0 /* 0 */},
    /* 62  11 11 10 */	{   0.0 /* 0 */},
    /* 63  11 11 11 */	{   0.0 /* 0 */}
};
#define	PRF(_ap)	prf_table[(_ap)->prf].prf

static
const U8 IQmap[] =	/* To deinterleave duplicate I/Q bit values */
{			/* Assume value 0 if I and Q are different */
    0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01, 0x02, 0x02,
    0x03, 0x03, 0x02, 0x02, 0x03, 0x03, 0x00, 0x00, 0x01, 0x01,
    0x00, 0x00, 0x01, 0x01, 0x02, 0x02, 0x03, 0x03, 0x02, 0x02,
    0x03, 0x03, 0x04, 0x04, 0x05, 0x05, 0x04, 0x04, 0x05, 0x05,
    0x06, 0x06, 0x07, 0x07, 0x06, 0x06, 0x07, 0x07, 0x04, 0x04,
    0x05, 0x05, 0x04, 0x04, 0x05, 0x05, 0x06, 0x06, 0x07, 0x07,
    0x06, 0x06, 0x07, 0x07, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00,
    0x01, 0x01, 0x02, 0x02, 0x03, 0x03, 0x02, 0x02, 0x03, 0x03,
    0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01, 0x02, 0x02,
    0x03, 0x03, 0x02, 0x02, 0x03, 0x03, 0x04, 0x04, 0x05, 0x05,
    0x04, 0x04, 0x05, 0x05, 0x06, 0x06, 0x07, 0x07, 0x06, 0x06,
    0x07, 0x07, 0x04, 0x04, 0x05, 0x05, 0x04, 0x04, 0x05, 0x05,
    0x06, 0x06, 0x07, 0x07, 0x06, 0x06, 0x07, 0x07, 0x08, 0x08,
    0x09, 0x09, 0x08, 0x08, 0x09, 0x09, 0x0A, 0x0A, 0x0B, 0x0B,
    0x0A, 0x0A, 0x0B, 0x0B, 0x08, 0x08, 0x09, 0x09, 0x08, 0x08,
    0x09, 0x09, 0x0A, 0x0A, 0x0B, 0x0B, 0x0A, 0x0A, 0x0B, 0x0B,
    0x0C, 0x0C, 0x0D, 0x0D, 0x0C, 0x0C, 0x0D, 0x0D, 0x0E, 0x0E,
    0x0F, 0x0F, 0x0E, 0x0E, 0x0F, 0x0F, 0x0C, 0x0C, 0x0D, 0x0D,
    0x0C, 0x0C, 0x0D, 0x0D, 0x0E, 0x0E, 0x0F, 0x0F, 0x0E, 0x0E,
    0x0F, 0x0F, 0x08, 0x08, 0x09, 0x09, 0x08, 0x08, 0x09, 0x09,
    0x0A, 0x0A, 0x0B, 0x0B, 0x0A, 0x0A, 0x0B, 0x0B, 0x08, 0x08,
    0x09, 0x09, 0x08, 0x08, 0x09, 0x09, 0x0A, 0x0A, 0x0B, 0x0B,
    0x0A, 0x0A, 0x0B, 0x0B, 0x0C, 0x0C, 0x0D, 0x0D, 0x0C, 0x0C,
    0x0D, 0x0D, 0x0E, 0x0E, 0x0F, 0x0F, 0x0E, 0x0E, 0x0F, 0x0F,
    0x0C, 0x0C, 0x0D, 0x0D, 0x0C, 0x0C, 0x0D, 0x0D, 0x0E, 0x0E,
    0x0F, 0x0F, 0x0E, 0x0E, 0x0F, 0x0F, 0x00, 0x00, 0x01, 0x01,
    0x00, 0x00, 0x01, 0x01, 0x02, 0x02, 0x03, 0x03, 0x02, 0x02,
    0x03, 0x03, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01,
    0x02, 0x02, 0x03, 0x03, 0x02, 0x02, 0x03, 0x03, 0x04, 0x04,
    0x05, 0x05, 0x04, 0x04, 0x05, 0x05, 0x06, 0x06, 0x07, 0x07,
    0x06, 0x06, 0x07, 0x07, 0x04, 0x04, 0x05, 0x05, 0x04, 0x04,
    0x05, 0x05, 0x06, 0x06, 0x07, 0x07, 0x06, 0x06, 0x07, 0x07,
    0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01, 0x02, 0x02,
    0x03, 0x03, 0x02, 0x02, 0x03, 0x03, 0x00, 0x00, 0x01, 0x01,
    0x00, 0x00, 0x01, 0x01, 0x02, 0x02, 0x03, 0x03, 0x02, 0x02,
    0x03, 0x03, 0x04, 0x04, 0x05, 0x05, 0x04, 0x04, 0x05, 0x05,
    0x06, 0x06, 0x07, 0x07, 0x06, 0x06, 0x07, 0x07, 0x04, 0x04,
    0x05, 0x05, 0x04, 0x04, 0x05, 0x05, 0x06, 0x06, 0x07, 0x07,
    0x06, 0x06, 0x07, 0x07, 0x08, 0x08, 0x09, 0x09, 0x08, 0x08,
    0x09, 0x09, 0x0A, 0x0A, 0x0B, 0x0B, 0x0A, 0x0A, 0x0B, 0x0B,
    0x08, 0x08, 0x09, 0x09, 0x08, 0x08, 0x09, 0x09, 0x0A, 0x0A,
    0x0B, 0x0B, 0x0A, 0x0A, 0x0B, 0x0B, 0x0C, 0x0C, 0x0D, 0x0D,
    0x0C, 0x0C, 0x0D, 0x0D, 0x0E, 0x0E, 0x0F, 0x0F, 0x0E, 0x0E,
    0x0F, 0x0F, 0x0C, 0x0C, 0x0D, 0x0D, 0x0C, 0x0C, 0x0D, 0x0D,
    0x0E, 0x0E, 0x0F, 0x0F, 0x0E, 0x0E, 0x0F, 0x0F, 0x08, 0x08,
    0x09, 0x09, 0x08, 0x08, 0x09, 0x09, 0x0A, 0x0A, 0x0B, 0x0B,
    0x0A, 0x0A, 0x0B, 0x0B, 0x08, 0x08, 0x09, 0x09, 0x08, 0x08,
    0x09, 0x09, 0x0A, 0x0A, 0x0B, 0x0B, 0x0A, 0x0A, 0x0B, 0x0B,
    0x0C, 0x0C, 0x0D, 0x0D, 0x0C, 0x0C, 0x0D, 0x0D, 0x0E, 0x0E,
    0x0F, 0x0F, 0x0E, 0x0E, 0x0F, 0x0F, 0x0C, 0x0C, 0x0D, 0x0D,
    0x0C, 0x0C, 0x0D, 0x0D, 0x0E, 0x0E, 0x0F, 0x0F, 0x0E, 0x0E,
    0x0F, 0x0F, 0x10, 0x10, 0x11, 0x11, 0x10, 0x10, 0x11, 0x11,
    0x12, 0x12, 0x13, 0x13, 0x12, 0x12, 0x13, 0x13, 0x10, 0x10,
    0x11, 0x11, 0x10, 0x10, 0x11, 0x11, 0x12, 0x12, 0x13, 0x13,
    0x12, 0x12, 0x13, 0x13, 0x14, 0x14, 0x15, 0x15, 0x14, 0x14,
    0x15, 0x15, 0x16, 0x16, 0x17, 0x17, 0x16, 0x16, 0x17, 0x17,
    0x14, 0x14, 0x15, 0x15, 0x14, 0x14, 0x15, 0x15, 0x16, 0x16,
    0x17, 0x17, 0x16, 0x16, 0x17, 0x17, 0x10, 0x10, 0x11, 0x11,
    0x10, 0x10, 0x11, 0x11, 0x12, 0x12, 0x13, 0x13, 0x12, 0x12,
    0x13, 0x13, 0x10, 0x10, 0x11, 0x11, 0x10, 0x10, 0x11, 0x11,
    0x12, 0x12, 0x13, 0x13, 0x12, 0x12, 0x13, 0x13, 0x14, 0x14,
    0x15, 0x15, 0x14, 0x14, 0x15, 0x15, 0x16, 0x16, 0x17, 0x17,
    0x16, 0x16, 0x17, 0x17, 0x14, 0x14, 0x15, 0x15, 0x14, 0x14,
    0x15, 0x15, 0x16, 0x16, 0x17, 0x17, 0x16, 0x16, 0x17, 0x17,
    0x18, 0x18, 0x19, 0x19, 0x18, 0x18, 0x19, 0x19, 0x1A, 0x1A,
    0x1B, 0x1B, 0x1A, 0x1A, 0x1B, 0x1B, 0x18, 0x18, 0x19, 0x19,
    0x18, 0x18, 0x19, 0x19, 0x1A, 0x1A, 0x1B, 0x1B, 0x1A, 0x1A,
    0x1B, 0x1B, 0x1C, 0x1C, 0x1D, 0x1D, 0x1C, 0x1C, 0x1D, 0x1D,
    0x1E, 0x1E, 0x1F, 0x1F, 0x1E, 0x1E, 0x1F, 0x1F, 0x1C, 0x1C,
    0x1D, 0x1D, 0x1C, 0x1C, 0x1D, 0x1D, 0x1E, 0x1E, 0x1F, 0x1F,
    0x1E, 0x1E, 0x1F, 0x1F, 0x18, 0x18, 0x19, 0x19, 0x18, 0x18,
    0x19, 0x19, 0x1A, 0x1A, 0x1B, 0x1B, 0x1A, 0x1A, 0x1B, 0x1B,
    0x18, 0x18, 0x19, 0x19, 0x18, 0x18, 0x19, 0x19, 0x1A, 0x1A,
    0x1B, 0x1B, 0x1A, 0x1A, 0x1B, 0x1B, 0x1C, 0x1C, 0x1D, 0x1D,
    0x1C, 0x1C, 0x1D, 0x1D, 0x1E, 0x1E, 0x1F, 0x1F, 0x1E, 0x1E,
    0x1F, 0x1F, 0x1C, 0x1C, 0x1D, 0x1D, 0x1C, 0x1C, 0x1D, 0x1D,
    0x1E, 0x1E, 0x1F, 0x1F, 0x1E, 0x1E, 0x1F, 0x1F, 0x10, 0x10,
    0x11, 0x11, 0x10, 0x10, 0x11, 0x11, 0x12, 0x12, 0x13, 0x13,
    0x12, 0x12, 0x13, 0x13, 0x10, 0x10, 0x11, 0x11, 0x10, 0x10,
    0x11, 0x11, 0x12, 0x12, 0x13, 0x13, 0x12, 0x12, 0x13, 0x13,
    0x14, 0x14, 0x15, 0x15, 0x14, 0x14, 0x15, 0x15, 0x16, 0x16,
    0x17, 0x17, 0x16, 0x16, 0x17, 0x17, 0x14, 0x14, 0x15, 0x15,
    0x14, 0x14, 0x15, 0x15, 0x16, 0x16, 0x17, 0x17, 0x16, 0x16,
    0x17, 0x17, 0x10, 0x10, 0x11, 0x11, 0x10, 0x10, 0x11, 0x11,
    0x12, 0x12, 0x13, 0x13, 0x12, 0x12, 0x13, 0x13, 0x10, 0x10,
    0x11, 0x11, 0x10, 0x10, 0x11, 0x11, 0x12, 0x12, 0x13, 0x13,
    0x12, 0x12, 0x13, 0x13, 0x14, 0x14, 0x15, 0x15, 0x14, 0x14,
    0x15, 0x15, 0x16, 0x16, 0x17, 0x17, 0x16, 0x16, 0x17, 0x17,
    0x14, 0x14, 0x15, 0x15, 0x14, 0x14, 0x15, 0x15, 0x16, 0x16,
    0x17, 0x17, 0x16, 0x16, 0x17, 0x17, 0x18, 0x18, 0x19, 0x19,
    0x18, 0x18, 0x19, 0x19, 0x1A, 0x1A, 0x1B, 0x1B, 0x1A, 0x1A,
    0x1B, 0x1B, 0x18, 0x18, 0x19, 0x19, 0x18, 0x18, 0x19, 0x19,
    0x1A, 0x1A, 0x1B, 0x1B, 0x1A, 0x1A, 0x1B, 0x1B, 0x1C, 0x1C,
    0x1D, 0x1D, 0x1C, 0x1C, 0x1D, 0x1D, 0x1E, 0x1E, 0x1F, 0x1F,
    0x1E, 0x1E, 0x1F, 0x1F, 0x1C, 0x1C, 0x1D, 0x1D, 0x1C, 0x1C,
    0x1D, 0x1D, 0x1E, 0x1E, 0x1F, 0x1F, 0x1E, 0x1E, 0x1F, 0x1F,
    0x18, 0x18, 0x19, 0x19, 0x18, 0x18, 0x19, 0x19, 0x1A, 0x1A,
    0x1B, 0x1B, 0x1A, 0x1A, 0x1B, 0x1B, 0x18, 0x18, 0x19, 0x19,
    0x18, 0x18, 0x19, 0x19, 0x1A, 0x1A, 0x1B, 0x1B, 0x1A, 0x1A,
    0x1B, 0x1B, 0x1C, 0x1C, 0x1D, 0x1D, 0x1C, 0x1C, 0x1D, 0x1D,
    0x1E, 0x1E, 0x1F, 0x1F, 0x1E, 0x1E, 0x1F, 0x1F, 0x1C, 0x1C,
    0x1D, 0x1D, 0x1C, 0x1C, 0x1D, 0x1D, 0x1E, 0x1E, 0x1F, 0x1F,
    0x1E, 0x1E, 0x1F, 0x1F
};

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Frame_t* RecoverPulse(Scan_t* p, const Job_t* job)
{
    const U32 *w = p->buf;
    register J1_Scan_t* sat = (J1_Scan_t*) p->sat;
    register int i,n,k;
    U32 e,x, x0, x1, e1 = 0, e2 = 0;

    if (
       ((k = i = sat->nominal_size,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
       ||
	(k = i = sat->nominal_size+6,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
       ||
	(k = i = sat->nominal_size-6,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
       ||
	(k = i = sat->nominal_size+12,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
       ||
	(k = i = sat->nominal_size-12,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
#ifndef	DEBUG
       ||
	(k = i = sat->nominal_size-2,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
       ||
	(k = i = sat->nominal_size+2,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
#endif
#ifndef	DEBUG
       || 
	(k = sat->nominal_size,
	 i = k+18,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &e)
		<= job->tail_diff)
       || 
	(k = sat->nominal_size+6,
	 i = k+18,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &e)
		<= job->tail_diff)
       ||  
	(k = sat->nominal_size-6,
	 i = k+18,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &e)
		<= job->tail_diff)
       || 
	(k = sat->nominal_size+12,
	 i = k+18,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &e)
		<= job->tail_diff)
       || 
	(k = sat->nominal_size-12,
	 i = k+18,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &e)
		<= job->tail_diff)
#endif
       ||
        (k = sat->nominal_size,
	 i = k*2,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
       || 
	(k = sat->nominal_size,
	 i = k*2+18,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
	 DIF((!n ? w[i] :((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &e)
		<= job->tail_diff)
#ifdef	DEBUG
       ||
	(k = i = sat->nominal_size-4,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
       ||
	(k = i = sat->nominal_size+4,
	 n = p->n + ((i+31)&(~0x1F))-i,
	 i = p->i - ((i+31)>>5)+(n>>5), n &= 0x1F,
         x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
         x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
         DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
         DIF(x1,J1_SYNC2,&e2) <= job->sync_diff)
#endif
    )){
	sat->frame_size = k;
	p->n += (((k+31)&(~0x1F)) - k);
	p->i += -((k+31)>>5) + (p->n>>5);
	p->n &= 0x1F;
	Scan_Position(&p->frame.start, p, job);
	p->frame.bit_error = sat->bit_error = e1+e2;

#ifdef	DEBUG
printf("Split_2 : %d/%d, i %d, n %d, size %d, err %d/%d/%d\n",
	p->frame.start.blk, p->frame.start.bit, p->i, p->n, 
	sat->frame_size, e1, e2, e);
#endif
	return (&p->frame);
    }

#ifdef	DEBUG
printf("No Split: %d/%d, i %d, n %d, size %d, err %d/%d/%d, %d/%d, %x/%x\n",
	p->frame.start.blk, p->frame.start.bit, p->i, p->n,
	sat->frame_size, e1, e2, e, i,n, x0,x1);
#endif

    return NULL;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
void FindSync(const U32 *w, const int n1, const int sync_diff,
	      int *word_offset, int *bit_offset, int *bit_error)
{
    register int i = *word_offset, n = *bit_offset;
    register U32 x = !n ? w[i] : (w[i] & (0xFFFFFFFFU >> n));
    int e1, e2;

    while (i < n1) {
	register U32 z1, z2;

        if (n = map[x >> 16]) {
	    if (n == 16) {
#ifndef	DEBUG
		if (x == J1_SYNC && (w[i+1] & J1_SYNC2_MASK) == J1_SYNC2) {
#else
		if ((x == J1_SYNC || DIF(x,J1_SYNC, &e1) <= sync_diff) &&
		    ((z2 = w[i+1] & J1_SYNC2_MASK) == J1_SYNC2 ||
		     DIF(z2,J1_SYNC2,&e2) <= sync_diff)) {
#endif
		    n = 0;
		    break;
		}
	    }
#ifndef	DEBUG
	    else if (((x>>(16-n))|(w[i-1]<<(16+n))) == J1_SYNC &&
		(((w[i+1]>>(16-n))|(x<<(16+n))) & J1_SYNC2_MASK) == J1_SYNC2) {
#else
	    else if (z1 = (x>>(16-n))|(w[i-1]<<(16+n)),
		     z2 = ((w[i+1]>>(16-n))|(x<<(16+n))) & J1_SYNC2_MASK,
		(z1 == J1_SYNC  || DIF(z1,J1_SYNC, &e1) <= sync_diff) &&
		(z2 == J1_SYNC2 || DIF(z2,J1_SYNC2,&e2) <= sync_diff)) {
#endif
		--i;
		n += 16;
		break;
	    }
	}
#ifndef	DEBUG
        if ((n = map[x & 0xFFFFU]) &&
	    ((x<<n)|(w[i+1]>>(32-n))) == J1_SYNC &&
	    (((w[i+1]<<n)|(w[i+2]>>(32-n))) & J1_SYNC2_MASK) == J1_SYNC2) {
#else
        if ((n = map[x & 0xFFFFU])
	&&  (z1 = (x<<n)|(w[i+1]>>(32-n)),
	     z2 = ((w[i+1]<<n)|(w[i+2]>>(32-n))) & J1_SYNC2_MASK,
	    (z1 == J1_SYNC  || DIF(z1,J1_SYNC, &e1) <= sync_diff) &&
	    (z2 == J1_SYNC2 || DIF(z2,J1_SYNC2,&e2) <= sync_diff))) {
#endif
            break;
        };
	n = 0;
        x = w[++i];
    }
    *word_offset = i;
    *bit_offset = n;
    *bit_error = 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Frame_t* J1_Scan(Scan_t* p, const Job_t* job)
{
    const U32 *w = p->buf;
    const int n1 = p->len+R32(J1_FRAME_SIZE_MAX)/4;
    register J1_Scan_t* sat = (J1_Scan_t*) p->sat;
    int n, i, i_org, n_org, bit_error;
    AuxJ1_t aux1, aux2;

#ifdef	DEBUG
    printf("J1_Scan %d: i %d, n %d, len %d, n1 %d, %d/%d\n",
	    p->blk_start, p->i, p->n, p->len, n1,
	    p->frame.start.blk, p->frame.start.bit);
#endif
    sat->nominal_size = 0;
    n = n_org = p->n;
    i = i_org = p->i;

for (;;) {

    FindSync(w, n1, job->sync_diff, &i, &n, &sat->bit_error);
    if (i >= n1) {
        const int len = (i-p->i)*32+(n-p->n);
	if (sat->nominal_size ? (len > sat->nominal_size+6)
			      : (len > J1_FRAME_SIZE_MAX*8)) {
            p->i = i;
            p->n = n;
            Scan_Position(&p->frame.start, p, job);
        }
	return NULL;
    }
#ifdef	DEBUG
printf("SYNC %d/%d, i %d, n %d\n", p->frame.start.blk, p->frame.start.bit, i,n);
#endif
    /*  Remember the position of this first SYNC */

    p->i = i;
    p->n = n;
    Scan_Position(&p->frame.start, p, job);
    p->frame.bit_error = sat->bit_error;
    p->frame.frame_cnt = 1;

    /*  Retrieve the AUX header */

    n += J1_SYNC_SIZE;
    i += (n>>5);
    n &= 0x1F;
    if (! n)
	sat->aux = *(AuxJ1_t*)&w[i];
    else {
	const int  nr = 32-n;
	const U32 *bp = w+i;
	register int k;
	for (k = 0; k < sizeof(AuxJ1_t)/4; ++k)
	    ((U32*)&sat->aux)[k] = (bp[k]<<n)|(bp[k+1]>>nr);
    }
    /*  Find the next SYNC so we can determine the frame size */

    n = p->n + J1_FRAME_SIZE_MIN*8;
    i = p->i + (n >> 5);
    n &= 0x1F;

    FindSync(w, n1, job->sync_diff, &i, &n, &sat->bit_error);
    if (i >= n1) {
	const int len = (i-p->i)*32+(n-p->n);
	if (i > n1) {
	    i = n1;
	    n = 0;
	}
	if (sat->nominal_size ? (len > sat->nominal_size+6)
			      : (len > J1_FRAME_SIZE_MAX*8)) {
	    p->i = i;
	    p->n = n;
	    Scan_Position(&p->frame.start, p, job);
	}
	return NULL;
    }
    if (i < R32(J1_FRAME_SIZE_MAX)/4) {
	/*  Might want to note this position and 
	 |  loop back to search for 2nd SYNC rather than 1st SYNC.
	 */
	sat->nominal_size = 0;
	continue;
    }
    sat->nominal_size = sat->frame_size = (i-p->i)*32+(n-p->n);

#ifdef DEBUG
printf("SYNC2 %d/%d, i %d, n %d\n", 
	p->frame.start.blk, p->frame.start.bit, i, n);
#endif
    /*  Retrieve this AUX header */

    n += J1_SYNC_SIZE;
    i += (n>>5);
    n &= 0x1F;
    if (! n)
	aux1 = *(AuxJ1_t*)&w[i];
    else {
	const int  nr = 32-n;
	const U32 *bp = w+i;
	register int k;
	for (k = 0; k < sizeof(AuxJ1_t)/4; ++k)
	    ((U32*)&aux1)[k] = (bp[k]<<n)|(bp[k+1]>>nr);
    }
    /*  Make sure the next format has consistent PRF and frame number */ 

    if (sat->aux.prf != aux1.prf
    || (sat->aux.prf_mode & 2) == 0
    ||  FrameNum(&sat->aux) != FrameNum(&aux1)-1) {

	n = p->n + sat->nominal_size;
	i = p->i + (n>>5);
	n &= 0x1F;
	sat->nominal_size = 0;
	continue;
    }
    /*  Find the next SYNC so we can determine the nominal frame size */

    n += sat->nominal_size-12 - J1_SYNC_SIZE;
    i += (n >> 5);
    n &= 0x1F;
    FindSync(w, n1, job->sync_diff, &i, &n, &sat->bit_error);

    if ((i-p->i)*32+(n-p->n) != sat->nominal_size*2) {
	sat->nominal_size = 0;
	continue;
    }
#ifdef DEBUG
printf("SYNC3 %d/%d, i %d, n %d\n", 
	p->frame.start.blk, p->frame.start.bit, i, n);
#endif

    if (sat->nominal_size > J1_FRAME_SIZE_MAX*8) {
	if (sat->nominal_size < J1_FRAME_SIZE_MIN*8)
	    printfLLog(LOG_ERR, "%d/%d: %d, unexpected frame size\n",
		       p->frame.start.blk, p->frame.start.bit, sat->frame_size);
	n = p->n + sat->nominal_size;
	i = p->i + (n>>5);
	n &= 0x1F;
	sat->nominal_size = 0;
	continue;
    }
    /*  Retrieve this AUX header */

    n += J1_SYNC_SIZE;
    i += (n>>5);
    n &= 0x1F;
    if (! n)
	aux2 = *(AuxJ1_t*)&w[i];
    else {
	const int  nr = 32-n;
	const U32 *bp = w+i;
	register int k;
	for (k = 0; k < sizeof(AuxJ1_t)/4; ++k)
	    ((U32*)&aux2)[k] = (bp[k]<<n)|(bp[k+1]>>nr);
    }
    /*  Make sure the format has consistent frame number */ 

    if (FrameNum(&sat->aux) != FrameNum(&aux2)-2) {
	n = p->n + sat->nominal_size;
	i = p->i + (n>>5);
	n &= 0x1F;
	sat->nominal_size = 0;
	continue;
    }
#ifdef DEBUG
printf("Splitting: %d/%d, i %d, n %d, size %d, PRF %f\n",
	p->frame.start.blk, p->frame.start.bit, p->i, p->n, sat->frame_size,
	PRF(&sat->aux));
    if (i_org == 0)
#endif
	while (((p->i >  R32(J1_FRAME_SIZE_MAX)/4 && p->i > i_org) ||
	        (p->i == R32(J1_FRAME_SIZE_MAX)/4 && p->n > 0))
	    && RecoverPulse(p, job));

#ifdef	DEBUG
	while ((p->i-R32(J1_FRAME_SIZE_MAX)/4)*32+p->n > -J1_SYNC_SIZE &&
	       RecoverPulse(p, job));

printf("New Frame: %d/%d, i %d, n %d, size %d, %d, %d, %d\n",
	p->frame.start.blk, p->frame.start.bit, p->i, p->n, 
	sat->nominal_size, sat->frame_size, p->frame.bit_error,
	R32(J1_FRAME_SIZE_MAX)/4);
#endif
    return (&p->frame);

} /* Again until we find 2 consecutive SYNCs of PRF-length */
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
Frame_t* J1_NextFrame(Scan_t* p, const Job_t* job, const Pulse_t* pr)
{
    const U32 *w = p->buf;
    const int n1 = p->len+R32(J1_FRAME_SIZE_MAX)/4;
    register int i,n,k;
    register J1_Scan_t* sat = (J1_Scan_t*) p->sat;
    U32 x0, x1, e1 = 0, e2 = 0;

#ifdef	DEBUG
    printf("J1_Next %d: i %d, n %d, len %d, n1 %d, %d/%d, size %d\n",
	    p->blk_start,p->i,p->n, p->len, n1,
	    p->frame.start.blk, p->frame.start.bit, sat->frame_size);
#endif
    p->n += sat->frame_size;
    p->i += (p->n>>5);
    p->n &= 0x1F;
    Scan_Position(&p->frame.start, p, job);
    p->frame.frame_cnt = 1;
    p->frame.bit_error = sat->bit_error;

    if (
       ((k = sat->nominal_size, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
       ||  
	(k = sat->nominal_size+6, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
       || 
	(k = sat->nominal_size-6, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
       ||  
	(k = sat->nominal_size+12, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
       ||  
	(k = sat->nominal_size-12, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
#ifndef	DEBUG
       || 
	(k = sat->nominal_size-2, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
       || 
	(k = sat->nominal_size+2, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
#endif
#ifndef	DEBUG
       ||
        (k = sat->nominal_size, n = p->n+k-18, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n-18 >= 0 &&
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &x0)
		<= job->tail_diff)
       ||
	(k = sat->nominal_size+6, n = p->n+k-18, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n-18 >= 0 &&
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &x0)
		<= job->tail_diff)
       || 
	(k = sat->nominal_size-6, n = p->n+k-18, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n-18 >= 0 &&
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &x0)
		<= job->tail_diff)
       || 
	(k = sat->nominal_size+12, n = p->n+k-18, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n-18 >= 0 &&
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &x0)
		<= job->tail_diff)
       || 
	(k = sat->nominal_size-12, n = p->n+k-18, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n-18 >= 0 &&
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &x0)
		<= job->tail_diff)
#endif
#ifdef	DEBUG
       ||  
        (k = sat->nominal_size, n = p->n+k*2, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= J1_SYNC_SIZE &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
       ||  
        (k = sat->nominal_size, n = p->n+k*2-18, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n-18 >= 0 &&
	 DIF((!n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))))>>14, 0x1556A, &x0)
		<= job->tail_diff)
       || 
	(k = sat->nominal_size-4, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= 0 &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
       || 
	(k = sat->nominal_size+4, n = p->n+k, i = p->i+(n>>5), n &= 0x1F,
	 (n1-i)*32-n >= 0 &&
	(x0 = !n ? w[i] : ((w[i]<<n)|(w[i+1]>>(32-n))),
	 x1 =(!n ? w[i+1] : ((w[i+1]<<n)|(w[i+2]>>(32-n)))) & J1_SYNC2_MASK,
	 DIF(x0,J1_SYNC, &e1) <= job->sync_diff &&
	 DIF(x1,J1_SYNC2,&e2) <= job->sync_diff))
#endif
    )){
	n = p->n + k;
	i = p->i + (n>>5);
	n &= 0x1F;

	if (i < n1 || (i == n1 && n == 0)) {
	    sat->frame_size = k;
	    sat->bit_error = e1+e2;
#ifdef	DEBUG
printf("Next: %d/%d, size %d/%d, %d/%d, %d/%d, %x/%x\n",
	p->frame.start.blk, p->frame.start.bit, k, sat->frame_size, e1, e2,
	p->i-p->len, p->n, x0,x1);
#endif
	    return (&p->frame);
	}
    }
    return NULL;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int J1_NewPulse(Pulse_t* pp, Scan_t* fp, Segmt_t* sp, const Job_t* job,
		const Pulse_t* pr)
{
    register U32* w;
    register int i,n;

    if (! pp) return -1;

    pp->frame_cnt = 1;
    pp->start = fp->frame.start;
    pp->tapebuf = fp->frame.tapebuf;
    pp->bit_error = fp->frame.bit_error;

    n = (fp->n+J1_SYNC_SIZE);
    w = &fp->buf[fp->i+(n>>5)];
    n &= 0x1F;
    if (n == 0)
	((J1_Pulse_t*)pp)->aux = *(AuxJ1_t*)w;
    else {
	const int nr = 32-n;
	for (i = 0; i < sizeof(AuxJ1_t)/4; ++i)
	    ((U32*)&((J1_Pulse_t*)pp)->aux)[i] = (w[i]<<n)|(w[i+1]>>nr);
    }
    /*  Since the two telemetry data "bits" are duplicate,
     |  we look at and save only the first (MS) data bit.
     |  Also, since the I and Q of each PCM bit, data or clk, 
     |  have the same value, we only look at the I channel value.
     */
    n += HK_DATA_SIZE+FRN_DATA_SIZE;	/* Bring us to 1st PCM Data bit */
    w += (n >> 5);
    n &= 0x1F;

    ((J1_Pulse_t*)pp)->pcm_bit = ((J1_Pulse_t*)pp)->pcm_clk = 0;

    for (i = 0; i < 13; ++i) {
	const U32 x = n ? ((w[0]<<n)|(w[1]>>(32-n))) : w[0];
	((J1_Pulse_t*)pp)->pcm_bit = (((J1_Pulse_t*)pp)->pcm_bit << 1)
				   | (x >> 31);
	((J1_Pulse_t*)pp)->pcm_clk = (((J1_Pulse_t*)pp)->pcm_clk << 1)
				   | ((x >> 27) & 1);
	n += PCM_DATA_SIZE+OBS_DATA_SIZE;
	w += n >> 5;
	n &= 0x1F;
    }
    if (sp->prf == 0)
	sp->prf = PRF(&((J1_Scan_t*)fp->sat)->aux);

    return (isON(((J1_Pulse_t*)pp)->aux.obs_mode) ? -1 : 1);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
int J1_ExtraPulse(Segmt_t* sp, Pulse_t** free_pulse, size_t free_cnt,
                  const Job_t* job, const int tapebuf_valid)
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
static
int J1_PulseCnt(const Pulse_t* obj)
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
static
int J1_Beam(const Pulse_t* obj)
{
    return BEAM_J1;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
Mode_t J1_Mode(const Segmt_t* sp, const Job_t* job)
{
    return JS1;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int J1_AGC(PulseList_t pp, const Segmt_t* sp, const Job_t* job)
{
    return (IQmap[((const J1_Pulse_t*) pp[0])->aux.agc]);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int J1_DWP(double *delay, PulseList_t pp, const Segmt_t* sp, const Job_t* job)
{
    register int dwp = (IQmap[((const J1_Pulse_t*) pp[0])->aux.stc] + 1)*10;

    /* Possible values: 60 .. 300 usec */
    if (dwp < 60 || dwp > 300) {
	register int i = pp - (PulseList_t)sp->pulse;

	while (--i >= sp->pulse_0 &&
	((dwp = (IQmap[((const J1_Pulse_t*)sp->pulse[i])->aux.stc]+1)*10)<60 ||
	  dwp > 300));
	if (i >= sp->pulse_0) {
	    *delay = dwp*1E-6;
	    return dwp;
	}
	i = pp - (PulseList_t)sp->pulse;

	while (++i < sp->pulse_0+sp->pulse_cnt &&
	((dwp = (IQmap[((const J1_Pulse_t*)sp->pulse[i])->aux.stc]+1)*10)<60 ||
	  dwp > 300));
	if (i < sp->pulse_0+sp->pulse_cnt) {
	    *delay = dwp*1E-6;
	    return dwp;
	}
	dwp = (IQmap[((const J1_Pulse_t*) pp[0])->aux.stc] + 1)*10;
    }
    *delay = dwp*1E-6;
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
RYP* J1_RYP(const double sv[6], RYP* att)
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
static
double J1_PRF(PulseList_t pp, const Segmt_t* sp, const Job_t* job)
{
    return sp->prf; /* PRF(&((const J1_Pulse_t*) pp)->aux); */
}
    
/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int J1_SameBurst(const Pulse_t* obj, const Pulse_t* src)
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
static
int J1_SameAGC(const Pulse_t* obj, const Pulse_t* src)
{
    return (((J1_Pulse_t*) obj)->aux.agc == ((J1_Pulse_t*) src)->aux.agc
	    ? -1 : 0);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int J1_SameDWP(const Pulse_t* obj, const Pulse_t* src)
{
    return (((J1_Pulse_t*) obj)->aux.stc == ((J1_Pulse_t*) src)->aux.stc
	    ? -1 : 0);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int NextPCM(const U32* w, int *w_index, int *w_bit, const int w_len,
	    const Job_t* job)
{
    register int n = 0, i = *w_index-1;

    while (++i <= w_len) {
	U32 e;
	register U32 x = w[i];

        if (n = PCmap[x >> 16]) {
            if (n == 16) {
#ifdef	DEBUG
                if ((x & PCM_SYNC_MASK) == PCM_SYNC) {
#else
		if (DIF(x & PCM_SYNC_MASK, PCM_SYNC, &e) <= job->pcm_diff) {
#endif
                    n = 0;
                    break;
                }
            }
#ifdef	DEBUG
            else if ((((x>>(16-n))|(w[i-1]<<(16+n))) & PCM_SYNC_MASK)
			== PCM_SYNC) {
#else
            else if (DIF(((x>>(16-n))|(w[i-1]<<(16+n))) & PCM_SYNC_MASK,
			PCM_SYNC, &e) <= job->pcm_diff) {
#endif
		--i;
		n += 16;
		break;
	    }
        }
#ifdef	DEBUG
        if ((n = PCmap[x & 0xFFFFU]) &&
            (((x<<n)|(w[i+1]>>(32-n))) & PCM_SYNC_MASK) == PCM_SYNC) {
#else
        if ((n = PCmap[x & 0xFFFFU]) &&
	    DIF(((x<<n)|(w[i+1]>>(32-n))) & PCM_SYNC_MASK, 
			PCM_SYNC, &e) <= job->pcm_diff) {
#endif
	    break;
        }
        n = 0;
    }
    *w_index = i;
    *w_bit = n;
    return (i <= w_len);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
void SaveGMT(U32 sct, int n, Segmt_t* sp, const Job_t* job, GMT *best_gmt)
{
    GMT gmt;
    double d1, d2;
    register int lo = sp->pulse_pcm+1, hi = sp->pulse_cnt;

    /* Look for a pulse with smallest pcm_pos > n */

    while (lo < hi) {
	register int i = (lo+hi)/2;
	if (((J1_Pulse_t*)sp->pulse[i])->pcm_pos > n)
	    hi = i;
	else
	    lo = i+1;
    }
    --lo;
    sp->gmt_pcm = job->tcf.gmt;
    gmt_add(&sp->gmt_pcm, -lo/sp->prf + /* job->tcf.delta*1.0E-3 + */
#ifdef	DEBUG
  	TIME_RATE*((double)sct - job->tcf.bt - 2048/2048 -
		   (n-((J1_Pulse_t*)sp->pulse[lo])->pcm_pos)/2048));
#else
	((double)sct - job->tcf.bt - 2048/2048 -
		   (n-((J1_Pulse_t*)sp->pulse[lo])->pcm_pos)/2048));
#endif
    d1 = gmt_diff(&sp->gmt_pcm, &job->start_time);
    d2 = gmt_diff(best_gmt, &job->start_time);

#ifdef DEBUG
    printf("#%d %d/%d: %.4d-%.3dT%.2d:%.2d:%09.6f, SC %u, n %d, %d\n",
	lo, sp->pulse[lo]->start.blk, sp->pulse[lo]->start.bit,
	sp->gmt_pcm.yr, sp->gmt_pcm.day, sp->gmt_pcm.hr, sp->gmt_pcm.min,
	sp->gmt_pcm.second, sct, n, ((J1_Pulse_t*)sp->pulse[lo])->pcm_pos);
#endif

    if ((d1 > 0 ? d1 : -d1) < (d2 > 0 ? d2 : -d2)) {
	*best_gmt = sp->gmt_pcm;
#ifdef DEBUG
    printf("%.4d-%.3dT%.2d:%.2d:%09.6f: #%d, %u ----\n",
best_gmt->yr, best_gmt->day, best_gmt->hr, best_gmt->min, best_gmt->second,
	lo, sct);
#endif
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
U32 GetSCT(const U32* w, int i, int n)
{
    return (n == 0 ?   w[i+16/4] :
	    n < 25 ? ( w[i+16/4]<<n)
	           : ((w[i+16/4]<<n)|(w[i+16/4+1]>>(32-n))));
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
static
int J1_GMT(GMT* gmt, PulseList_t pp, Segmt_t* sp, const Job_t* job)
{
    register int i,n;
    register U32 clk, *w = job->pcm;
    const GMT bogus_gmt = {1970,01,0,0,0.0};
    GMT best_gmt = *(GMT*) &bogus_gmt;

    if (sp->pulse_pcm >= 0) {
	*gmt = sp->gmt_pcm;
	gmt_add(gmt, (pp-(PulseList_t)sp->pulse)/sp->prf);
	return ((int) gmt);
    }
    n = 0;
    clk = 0x55555555;
    memset(w, 0, (sp->pulse_cnt*2+31)/8);

#ifdef	DEBUG
printf("SEGMT %d/%d: frame size %d\n", sp->start.blk, sp->start.bit,
    (sp->pulse[1]->start.blk-sp->pulse[0]->start.blk)*job->blk_size*8+
    (sp->pulse[1]->start.bit-sp->pulse[0]->start.bit));
#endif

    for (i = 0; i < sp->pulse_cnt; ++i) {
	U32 e;
	register int k;
	const int b = ((J1_Pulse_t*)sp->pulse[i])->pcm_bit;
	const int c = ((J1_Pulse_t*)sp->pulse[i])->pcm_clk;

#ifdef	DEBUG
printf("%d: %x/%x, %d/%d\n", i, b,c, sp->pulse[i]->start.blk, 
				     sp->pulse[i]->start.bit);
#endif
	((J1_Pulse_t*)sp->pulse[i])->pcm_pos = n;

	for (k = 12; k > 0; --k) {
	    clk = (clk << 1) | ((c>>k)&1);
	    if ((clk & 0x1F) == 0x1) {
		w[n>>5] |= ((b>>k)&1) << (31-(n&0x1F));
		if (n == 0) sp->pulse_pcm = i;
		++n;
	    }
	}
	clk = (clk << 1) | (c&1);
	if ((clk & 0x1F) == 0x1) {
	    w[n>>5] |= (b&1) << (31-(n&0x1F));
	    if (n == 0) sp->pulse_pcm = i;
	    ++n;
	}
    }
    /*  Search the PCM bit stream for three consecutive minor frames.
     |  Each minor frame is identified by a PCM_SYNC code at the start,
     |  with the first frame marked by the frame ID field value of 0.
     |  A minor frame is made up of 128 8-bit words.  The frame ID is 
     |  contained in word offset #5 of each minor frame.
     |
     |  The S/C binary time is interleaved over 3 minor frames in word
     |  offsets 16 and 17, and is 27 bits in length as follows:
     |
     |		Frame/Word/Bits		S/C Bits
     |
     |		  #0 /#16 /2..0		26..24
     |		  #1 /#16 /7..0		23..16
     |		  #2 /#16 /7..0		15..8
     |		  #2 /#17 /7..0		 8..0
     |
     |  This S/C time corresponds to the time of the 1st bit of the PCM
     |  frame sync code of frame #0.  This S/C time plus (i/2048) seconds
     |  gives the S/C time of any subsequent ith bit, since a minor frame
     |  is transmitted every 1/2 second.
     */
    clk = (n-7*128*8) >> 5;

#ifdef	DEBUG
{   register int i,v = 0;
    for (i = 0; i <= clk; ++i)
	printf("%.8X%s", w[i], (++v)%8 == 0 ? "\n" : " ");
}
#endif
    i = 0;
    while (i <= clk) {
	int i0, i1, i2, i3, n0, n1, n2, n3;
	int i4, i5, i6, i7, n4, n5, n6, n7;
	register U32 t0, t1;

	if ((i0 = i1 = i2 = i,  NextPCM(w,&i0,&n0,clk,job)) &&
	    !((n0 ? ((w[i0+1]<<n0)|(w[i0+2]>>(32-n0))) : w[i0+1]) & 0x30000) &&
	    (i1 = i0+(128/4)-4, NextPCM(w,&i1,&n1,clk,job) && i1<i0+128/4+1) &&
	    (i2 = i1+(128/4)-4, NextPCM(w,&i2,&n2,clk,job) && i2<i1+128/4+1) &&
	    (i4 = i2+(256/4)-4, NextPCM(w,&i4,&n4,clk,job) && i4<i2+256/4+1) &&
	    (i5 = i4+(128/4)-4, NextPCM(w,&i5,&n5,clk,job) && i5<i4+128/4+1) &&
	    (i6 = i5+(128/4)-4, NextPCM(w,&i6,&n6,clk,job) && i6<i5+128/4+1) &&
	    (t0 =  (0x07000000U & GetSCT(w,i0,n0)) |
                  ((0xFF000000U & GetSCT(w,i1,n1)) >> 8) |
                  ((0xFFFF0000U & GetSCT(w,i2,n2)) >> 16),
	     t1 =  (0x07000000U & GetSCT(w,i4,n4)) |
                  ((0xFF000000U & GetSCT(w,i5,n5)) >> 8) |
                  ((0xFFFF0000U & GetSCT(w,i6,n6)) >> 16),
	     t1 = t0+2)) {

	    SaveGMT(t0, i0*32+n0, sp, job, &best_gmt);
	    break;

#ifdef	DEBUG
printf("SYNC %d: %.8X, %.8X, %.8X, %d, %d\n", i0,
	(n0 ? (w[i0]<<n0)|(w[i0+1]>>(32-n0)) : w[i0]) & PCM_SYNC_MASK,
	(n1 ? (w[i1]<<n1)|(w[i1+1]>>(32-n1)) : w[i1]) & PCM_SYNC_MASK,
	(n2 ? (w[i2]<<n2)|(w[i2+1]>>(32-n2)) : w[i2]) & PCM_SYNC_MASK,
	i1*32+n1-i0*32-n0, i2*32+n2-i1*32-n1);

	    i = i2+(128/4)/2;
#endif
	}	
	else {
	    i = (i2 > i1 ? i2 : (i1 > i0 ? i1 : i0)) + (128/4)/2;
	}
    }
    if (best_gmt.yr == 1970) {
#ifdef	DEBUG
	printfLLog(LOG_ERR,
	    "%d/%.5d..%d/%.5d: no PCM, can't determine time%s",
	    sp->start.blk, sp->start.bit, sp->end.blk, sp->end.bit,
	    sp->prf == 0 ? ", likewise with PRF" : "");
#endif
	if (sp->prf == 0)
	    sp->prf = 1;

	sp->pulse_pcm = 0;
    }
    *gmt = sp->gmt_pcm = best_gmt;
    gmt_add(gmt, (pp-(PulseList_t)sp->pulse)/sp->prf);

    return (best_gmt.yr == 1970 ? NULL : (int) gmt);
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION 
 *---------------------------------------------------------------------------*/
double J1_WindowDuration(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    return job->j_win_duration;
}

/*---------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *---------------------------------------------------------------------------*/
double J1_LookAngle(PulseList_t obj, const Segmt_t* sp, const Job_t* job)
{
    register int k;
    for (k = 0; k < LOOKTABLE_MAX; ++k)
        if (looktable[k].rmin >= job->rev  &&
            looktable[k].rmax <= job->rev)
            break;
    if (k == LOOKTABLE_MAX)
        k = 0;
    return looktable[k].beam[BEAM_J1];
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/static
void* J1_Init (Job_t* job)
{
    register U32 n, x;

    /*  Initialize format sync code lookup table */

    memset(map, 0, sizeof(map));
    x = J1_SYNC;
    for (n = 1; n <= 16; ++n) {
        x >>= 1;
        map[x & 0xFFFFU] = n;
    }
    /*  Initialize PCM sync code lookup table */

    memset(PCmap, 0, sizeof(PCmap));
    for (n = 0; n < 256; ++n) {
        register U32 i, x = PCM_SYNC | n;
        for (i = 1; i <= 16; ++i) {
            x >>= 1;
            PCmap[x & 0xFFFFU] = i;
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
 *----------------------------------------------------------------------------*/static
void J1_Destroy(Job_t* job)
{
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int J1_CAL(double* cal, Segmt_t* sp, const Job_t* job)
{
    return ((int) cal);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/static
int J1_Decode(Segmt_t* sp, int k, int n, int f, const Job_t* job)
{
    return 1;
}

const Satellite_t J1 = {
    J1_FRAME_SIZE_MIN,
    J1_FRAME_SIZE_MAX,
    J1_FRAME_SIZE,
    J1_DATA_SIZE,
    J1_HDR_SIZE,
    J1_FRAME_PER_PULSE_MIN,
    J1_FRAME_PER_PULSE_MAX,
    {0, 0, 0, 0},
    {    0.0,     0.0,     0.0},
    { -660.0,  -660.0,  -660.0},
    J1_Init,
    J1_Destroy,
    J1_Decode,
    J1_Scan,
    J1_NextFrame,
    J1_NewPulse,
    J1_ExtraPulse,
    J1_SameDWP,
    J1_SameAGC,
    J1_GMT,
    J1_DWP,
    J1_AGC,
    J1_RYP,
    J1_PRF,
    J1_LookAngle,
    J1_WindowDuration,
    J1_CAL,
    J1_Mode,
    J1_Beam,
    J1_SameBurst,
    J1_PulseCnt
};
