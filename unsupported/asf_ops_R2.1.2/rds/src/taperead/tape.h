/*=============================================================================
 |  @(#)tape.h	1.4 96/12/02 19:39:39
 |
 |  DCRSI & SONY-ID1 Tape Reader Module Include File.
 |  Alaska SAR Facility (ASF) Project.
 |
 |  Copyright (C) Jet Propulsion Laboratory.
 |
 *============================================================================*/

#ifndef _TAPE_H
#define _TAPE_H

#include <sys/types.h>
#include <unistd.h>

static char sccsid_tape_h[] =
	"@(#)tape.h	1.4 96/12/02 19:39:39";

/*  Tape block size in bytes - must be multiple of 4 bytes.
*/
#define	SONY_BLOCK_SIZE		144304
#define DCRSI_BLOCK_SIZE	4356

int	tape_open (char *dev_name, int oflag, int csize);
int	tape_close(int tape_fd);
int	tape_read (int tape_fd, char *buffer, int nbytes);
off_t	tape_seek (int tape_fd, off_t block_num, int whence);

#endif /* !_TAPE_H */
