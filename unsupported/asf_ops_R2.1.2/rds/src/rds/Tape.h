/*============================================================================*
 |  @(#)Tape.h	1.19 98/02/10 10:30:53
 |
 |  Tape Drive Interface Object.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef _RDS_TAPE_H_
#define _RDS_TAPE_H_

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


static const char sccsid_Tape_h[] =
	"@(#)Tape.h	1.19 98/02/10 10:30:53";

/*----------------------------------------------------------* 
 |  Supported Devices:	DCRSI, SONY, or disk file.
 *----------------------------------------------------------*/

#define	DCRSI_TAPE		0
#define DCRSI_BLOCK_SIZE        4356	/* in bytes,  n*sizeof(int) */
#define SONY_TAPE		1
#define SONY_BLOCK_SIZE		144304	/* in bytes,  n*sizeof(int) */
#define	DataSource(tape)	((tape)->source&1) /* DCRSI_TAPE or SONY_TAPE */
#define	DataOnDisk(tape)	((tape)->source&2) /* TRUE or FALSE */

#define IO_TYPE			O_DIRECT	/* Direct I/O */
#ifndef U8
#define U8			unsigned char
#endif

/*--------------------------*
 |  Tape I/O Control Block
 *--------------------------*/

typedef struct {
    int		blk_found;		/* For handling forward/backward seek */
    int		blk_start;		/* Start reading from this block */
    int		blk_end;		/* Last block to read + 1 */
    size_t	blk_size;		/* Block size in bytes */
    int		(*Log)();		/* Same format as fprintf(...) */
    void*	err;			/* Error handle */
    int		file_blk;		/* Tape block # of first disk byte */
    int 	sony_err;		/* Needed for SONY reverse playback */
    short	sony_check;		/* Sony label check */
    short	reverse;		/* Reverse playback or not */
    U8		quit;			/* TRUE => emergency exit */
    U8		file_num;		/* Disk file number */
    U8		dcrsi_csize;		/* 7 or 8 bits on serial CMD channel */
    U8		source;			/* DCRsi-107 => 'A', else => 'L' */
    int		seek_timer;		/* Each time-out in seconds */
    int		seek_retry;		/* Maximum retry */
    char*	label;			/* Check tape label, if non-NULL */
    char*       data_port;              /* VDI device name or disk file name */
    char*       ctrl_port;              /* Serial port name or NULL for disk */
    int 	data_fd;		/* VDI file descriptor */
    int		ctrl_fd;		/* Serial port descriptor */

#ifdef	DIRECT_IO
    struct dioattr fa;			/* Direct I/O info */
#endif

} Tape_t;
 
extern	Tape_t* Tape_Init(Tape_t* tape,
			  int blk_start, int blk_end, int reverse,
			  int (*error_log)(), void* error_handle,
			  int source, int dataOnDisk,
			  char* tape_label, int dsk_start,
			  int seek_timer, int seek_retry);
extern	void	Tape_Destroy(Tape_t*);
extern  int	Tape_Read(Tape_t*, char* buf, unsigned nbytes, int ignerr);
extern	int	Tape_Shutdown(Tape_t*);

#endif /*!_RDS_TAPE_H_ */
