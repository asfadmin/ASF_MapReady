/*============================================================================*
 |  @(#)Tape.c	1.86 98/03/14 18:28:37
 |
 |  RDS Interface to AMPEX and SONY Tape Drive, and Disk Files.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <termios.h>
#include <string.h>
#include <sys/time.h>
#include <syslog.h>
#include <aio.h>
#include "vdi_io.h"
#include "ml930drv.h"
#include "Tape.h"


static const char sccsid_Tape_c[] =
        "@(#)Tape.c	1.86 98/03/14 18:28:37";

static char ctrl_port_last[256]	= {0};
static Tape_t* Open(Tape_t* tape);

#define DCRSI_RETRY_SIZE	512
#define	RESET_ACKLEN		255
#define TAPE_LABELEN		127

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int InProgress(char* buf, int len)
{
    register int i = len - 6;
    while (--i >= 0)
	if (buf[i] == 'D' && buf[i+1] == 'S' && buf[i+2] == ' ' &&
	    isdigit(buf[i+3]) && isdigit(buf[i+4]) &&
	    isdigit(buf[i+5]) && isdigit(buf[i+6]))
	    return (buf[i+5] != buf[i+6]);
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
#ifdef	NOSTDARG
int SendCmd (va_alist)
va_dcl
#else

int SendCmd (Tape_t* tape, char *cmd, ...)
#endif
{
    register int n, i = 0, buf_len = 0, ack_cnt = 0, timeout = 0;
    char buf[256];
    int (*cmp)();
    va_list  ap, ap_new;

#ifdef	NOSTDARG
    char* cmd;
    Tape_t* tape;
    va_start(ap);
    tape= va_arg(ap, Tape_t*);
    cmd = va_arg(ap, char*);
#else
    va_start(ap, cmd);
#endif

#ifndef DEBUG
    printfLLog(LOG_DEBUG, "%s\n", cmd);
#endif
    n = strlen(cmd);
    if (write(tape->ctrl_fd, cmd, n) != n) {
	(*tape->Log)(tape->err, "Write error on DCRSi CMD channel: %s\n",
		     strerror(errno));
	return -1;
    }
    buf[0] = 0;
    cmp = (int (*)()) va_arg(ap, void*);

    while (cmp && buf_len < sizeof(buf)-1) {
	fd_set read_fds;

	FD_ZERO(&read_fds);
	FD_SET(tape->ctrl_fd, &read_fds);

	do {struct timeval timer;
	    timer.tv_usec = 0;
	    timer.tv_sec  = tape->seek_timer - timeout;
	    if (timer.tv_sec > 5) timer.tv_sec = 5;
	    n = select(FD_SETSIZE, &read_fds, NULL, NULL, &timer);
	}
	while (n == -1 && errno == EINTR && !tape->quit);
	
	if (tape->quit) {
	    printfLLog(LOG_DEBUG, "%s", buf);
	    return 0;
	}
	if (n == -1) {
	    printfLLog(LOG_DEBUG, "%s", buf);
	    (*tape->Log)(tape->err, "Select error on DCRSi CMD channel: %s.\n",
			 strerror(errno));
	    return -1;
	}
	if (n == 0) {
	    if (InProgress(buf, buf_len) || (timeout+=5) < tape->seek_timer)
		continue;
	    printfLLog(LOG_DEBUG, "%s", buf);
	    return 0;
	}
	do n = read(tape->ctrl_fd, buf+buf_len, (sizeof(buf)-1)-buf_len);
	while (n == -1 && errno == EINTR && !tape->quit);

	if (tape->quit) {
	    printfLLog(LOG_DEBUG, "%s", buf);
	    return 0;
	}
	if (n == -1) {
	    (*tape->Log)(tape->err,
		"Read error on DCRSi CMD channel: %s.\n", strerror(errno));
	    return -1;
	}
	buf[buf_len+=n] = 0;
#ifdef DEBUG
	printf("%s", &buf[buf_len-n]); fflush(stdout);
#endif
	for (;;) {
	    ap_new = ap;
	    if ((n = (*cmp)(tape, &buf[i], buf_len-i, &ap_new)) == 0)
		break;

	    if (n < 0) {
		printfLLog(LOG_DEBUG, "%s", buf);
		return n;
	    }
	    i += n;
	    ack_cnt++;
	    ap = ap_new;
	    if (!(cmp = (int (*)()) va_arg(ap, void*)))
		break;
	}
    }
    va_end(ap);
    printfLLog(LOG_DEBUG, "%s", buf);
    return (ack_cnt);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int IsInList(Tape_t* tape, char* buf, int len, va_list* ap)
{
    int n;
    char *p, *pat;
    if (len <= 0) return 0;

    while (pat = va_arg(*ap, char*))
	if ((n = strlen(pat)) <= len && (p = strstr(buf, pat))) {
	    n += p-buf;
	    while (pat = va_arg(*ap, char*));
	    return n;
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
int GetAddress(Tape_t* tape, char* buf, int len, va_list* ap)
{
    register int n, *start = va_arg(*ap, int*);
    register char *p;

    *start = 0;
    if (! (p = strstr(buf, "DS C022;")) && ! (p = strstr(buf, "DS 4022;")) &&
  	! (p = strstr(buf, "DS 4000;")) && ! (p = strstr(buf, "DS 4011;")) &&
	! (p = strstr(buf, "DS 4100;")) && ! (p = strstr(buf, "DS 8022;")))
	return 0;

    n = (p - buf) + 8;
    while (--p >= buf && (*p != 'D' || memcmp(p, "DL B-", 5)));
    if (p < buf || sscanf(p+5, "%d", start) != 1)
	*start = strstr(buf, "DE 261") ? -1 : 0;
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
int ResetCheck(Tape_t* tape, char* buf, int len, va_list* ap)
{
    register char *p, *response = va_arg(*ap, char*);
    response[RESET_ACKLEN] = 0;

    if (!strstr(strncpy(response, buf, RESET_ACKLEN), "Revision"))
	return 0;
    if (! (p = strstr(buf, "DS 4000;")) && ! (p = strstr(buf, "DS 4011;")) &&
	! (p = strstr(buf, "DS 4100;")) && ! (p = strstr(buf, "DS 4022;")) &&
	! (p = strstr(buf, "DS C022;")) && ! (p = strstr(buf, "DS 8022;")))
	return 0;
/*  tape->play_cmd = strstr(buf, "-107") ? 'A' : 'L'; */
    return ((p - buf) + 8);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int GetLabel(Tape_t* tape, char* buf, int len, va_list* ap)
{
    register char *p, *q, *label = va_arg(*ap, char*);
    register int n;    
    if ((! (p = strstr(buf, "DU R-")) && ! (p = strstr(buf, "DU F-"))) ||
	! (q = strstr(p += 5, ";*")))
	return 0;
    n = q-p;
    if (n > TAPE_LABELEN)
	n = TAPE_LABELEN;
    memcpy(label, p, n);
    label[n] = 0;
    return ((q-buf) + 2);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Tape_t* Tape_Seek(Tape_t* tape, int blk_seek)
{
    register int n = 0, blk_offset, byt_offset, blk_start, byt_start;
    char name[256], *p;

    if (DataOnDisk(tape)) {
	blk_offset = tape->file_blk;
	byt_offset = 0;

	for (;;) {
	    struct stat fs;
	    sprintf(name, "%s.%d", tape->data_port, ++n);

            if (stat((p=strchr(name, ':')) ? p+1 : name, &fs) == -1 && (n > 1 ||
		stat((p=strchr(strcpy(name,tape->data_port), ':')) ? p+1 : name,
		     &fs) == -1)) {
		(*tape->Log)(tape->err, "%s: stat error, %s\n",
			     name, strerror(errno));
		return NULL;
	    }
	    /*  Start Address */
	    blk_start = blk_offset;
	    byt_start = byt_offset;

	    /*  End Address */
	    byt_offset += (fs.st_size%tape->blk_size);
	    blk_offset += (fs.st_size/tape->blk_size) + 
			  (byt_offset/tape->blk_size);
	    byt_offset %= tape->blk_size;
	
	    if ((blk_seek < blk_offset) ||
		(blk_seek == blk_offset && byt_offset))
		break;
	}
	if (n != tape->file_num) {
	    if (tape->data_fd != -1)
		close(tape->data_fd);
	
	    if ((tape->data_fd = open(
		    (p = strchr(name, ':')) ? p+1 : name, O_RDONLY |
		    ((tape->file_blk - tape->blk_start) % 512 ? 0 : IO_TYPE) ))
		== -1) {
		(*tape->Log)(tape->err, "%s: open error, %s",
			     name, strerror(errno));
		return NULL;
	    }
	    tape->file_num = n;
	}
	n = (blk_seek - blk_start) * tape->blk_size + byt_start;
	if (lseek(tape->data_fd, n, SEEK_SET) != n) {
	    (*tape->Log)(tape->err, "Can't lseek %s, %s", name,strerror(errno));
	    return NULL;
	}
	return tape;	
    }
    if (DataSource(tape) == DCRSI_TAPE) {
	register int i = -1, blk_min = 0, error, blk_retry = blk_seek;
	int blk_found, op = VDI_REPLAY_P_DATA;

	while (++i <= tape->seek_retry) {
	    register int k;
	    char cmd[32];

	    if (i > 1) {
		register int blk_failed = blk_retry;

		if (blk_failed <= DCRSI_RETRY_SIZE ||
		   (blk_failed <= blk_seek && i == tape->seek_retry)) {
		    i = 1;
		    blk_retry = blk_seek + DCRSI_RETRY_SIZE;
		}
		else if (blk_failed <= blk_seek)
		    blk_retry -= DCRSI_RETRY_SIZE;

		else if (i < tape->seek_retry || blk_min == 0)
		    blk_retry += DCRSI_RETRY_SIZE;
		else
		    blk_retry = blk_min;

		if (blk_retry >= tape->blk_end) {
		    (*tape->Log)(tape->err,"Can't seek to block #%d", blk_seek);
		    return NULL;
		} 
		if (blk_found <= 0)
		    printfLLog(LOG_DEBUG,
		    "DCRSi can't seek to #%d - will try #%d",
			blk_failed, blk_retry);
		else 
		    printfLLog(LOG_DEBUG,
		    "DCRSi went to #%d instead of #%d - will try #%d",
			blk_found, blk_failed, blk_retry);
	    }
	    sprintf(cmd, ";PL A%d;", blk_retry);
	    error = SendCmd(tape, cmd, GetAddress, &blk_found, NULL);

	    if (error == -1 || tape->quit) /* I/O Error */
		return NULL;

	    if (error == 0) /* Timeout or other error */
		continue;

	    if (blk_found < 0)
		break;

	    if (blk_found == 0)
		continue;

	    if (abs(blk_min-blk_seek) > abs(blk_found-blk_seek))
		blk_min = blk_found;

	    if (i != tape->seek_retry &&
		blk_retry > DCRSI_RETRY_SIZE && blk_found > blk_seek)
		continue;

	    /*  Found the retry address */
	    if (ioctl(tape->data_fd, VDI_D_SET_OPERATION, (char*)&op) == -1) {
		(*tape->Log)(tape->err,
		    "Can't seek to block #%d - VDI_REPLAY error %d",
		     blk_seek, errno);
		return NULL;
	    }
	    if (ioctl(tape->data_fd, VDI_D_START_OPERATION, 0) == -1) {
		(*tape->Log)(tape->err,
		    "Can't seek to block #%d - VDI_START error %d",
		    blk_seek, errno);
		return NULL;
	    }
	    if ((tape->blk_found = blk_found) > blk_seek)
		break;

	    for (k = (blk_seek - blk_found) / DCRSI_RETRY_SIZE; k > 0; --k) {
		char buf[DCRSI_RETRY_SIZE * DCRSI_BLOCK_SIZE];

		if (read(tape->data_fd, buf, sizeof(buf)) != sizeof(buf)) {
		    (*tape->Log)(tape->err,
			"Can't seek to block #%d - Data read error, %s",
			blk_seek, strerror(errno));
		    return NULL;
		}
	    }
	    if ((k = (blk_seek - blk_found) % DCRSI_RETRY_SIZE) > 0) {
		char buf[DCRSI_RETRY_SIZE * DCRSI_BLOCK_SIZE];

		k *= tape->blk_size;
		if (read(tape->data_fd, buf, k) != k) {
		    (*tape->Log)(tape->err,
			"Can't seek to block #%d - Data read error, %s",
			blk_seek, strerror(errno));
		    return NULL;
		}
	    }
	    break;
	}
	if (blk_found <= 0) {
	    (*tape->Log)(tape->err,
		blk_found == -1 ? "Cassette may not be in place"
				: "Can't seek to block #%d",
		blk_seek);
	    return NULL;
	}
	if (tape->label) {
	    char label[TAPE_LABELEN+1], id[8];

	    /*  Check tape label */
	    if (SendCmd(tape, ";DU;", GetLabel, label, NULL) <= 0) {
		(*tape->Log)(tape->err, "can't find tape label");
		return NULL;
	    }
	    if (! *label) {
		char cmd_1[32], cmd_0[32];

		/*  Play forward 30000 blocks and see if we can get the label */
		sprintf(cmd_0, ";PL A%d;", blk_seek);
		sprintf(cmd_1, ";PL A%d;", blk_seek + 30000);

		if (SendCmd(tape, cmd_1, GetAddress, &blk_found, NULL) <= 0 ||
		    SendCmd(tape, ";DU;", GetLabel, label, NULL) <= 0 ||
		    SendCmd(tape, cmd_0, GetAddress, &blk_found, NULL) <= 0 ||
		    blk_found != blk_seek) {

		    (*tape->Log)(tape->err, "can't find tape label");
		    return NULL;
		}
	    }
	    if (!strstr(label, tape->label) &&
		(strlen(tape->label) < 12 || 
		(id[0] = tape->label[2], id[1] = tape->label[3],
		 id[2] = tape->label[8], id[3] = tape->label[9],
		 id[4] = tape->label[10],id[5] = tape->label[11], id[6] = 0,
		 !strstr(label, id))))  {

		(*tape->Log)(tape->err, "tape label mismatch, %s", label);
		return NULL;
	    }
	}
	else if (blk_found > blk_seek) 
	    printfLLog(LOG_INFO,
		"Could not seek to block #%d - Started from #%d instead.",
		blk_seek, blk_found);
	return tape;
    }
    if (DataSource(tape) == SONY_TAPE) {

        CMD_PLAYBACK_MACRO UserDefinedPlayParams;
	CMD_PASS_THRU UserDefinedPassThruParams;
	register char *label = UserDefinedPassThruParams.data;
	register int i, mode, blk_start;

	if (!tape->reverse || !tape->sony_check) {

            memset(&UserDefinedPlayParams, 0, sizeof(CMD_PLAYBACK_MACRO));
            UserDefinedPlayParams.searchType = tape->reverse ? 5 : 1;
            UserDefinedPlayParams.tracksetID = blk_seek;
            UserDefinedPlayParams.startFlag  = 1;

	    if (ioctl(tape->data_fd, PLAYBACK_MACRO, &UserDefinedPlayParams)
		== -1) {
		(*tape->Log)(tape->err,
		    "Can't seek to track #%d - PLAYBACK_MACRO fails, %s\n",
		     blk_seek, strerror(errno));
		return NULL;
	    }
	    sleep(1);
	    return tape;
	}
	/*  We need to restart the SONY in forward mode to get the label */

	blk_start = tape->blk_start;
	mode = tape->reverse;
	tape->reverse = 0;

	for (i = 0; i < tape->seek_retry; ++i) {
	    char id[8], buf[16*SONY_BLOCK_SIZE];

	    Tape_Destroy(tape);
	    sleep(2);
	    if (! Open(tape))
		return NULL;

	    if (read(tape->data_fd, buf, sizeof(buf)) != sizeof(buf)) {
		tape->blk_start += 512;
		continue;
	    }
	    memset(&UserDefinedPassThruParams, 0, sizeof(CMD_PASS_THRU));
	    UserDefinedPassThruParams.keyword1 = 0x50;
	    UserDefinedPassThruParams.keyword2 = 0x06;

	    if (ioctl(tape->data_fd, PASSTHRU_COMMAND, 
		     &UserDefinedPassThruParams) == -1) {
		tape->blk_start += 512;
		continue;
	    }
	    if (!strstr(label, tape->label) &&
		(strlen(tape->label) < 12 || 
		(id[0] = tape->label[2], id[1] = tape->label[3],
		 id[2] = tape->label[8], id[3] = tape->label[9],
		 id[4] = tape->label[10],id[5] = tape->label[11], id[6] = 0,
		 !strstr(label, id))))  {

		(*tape->Log)(tape->err, "tape label mismatch, %s", label);
		return NULL;
	    }
	    break;
	}
	if (i == tape->seek_retry) {
	    (*tape->Log)(tape->err, "can't find tape label");
	    return NULL;
	}
	tape->reverse = mode;
	tape->blk_start = blk_start;
	tape->sony_check = 0;

	/* Now is OK to restore the SONY in desired mode */
	Tape_Destroy(tape);
	sleep(2);
	tape->reverse = mode;

	return Open(tape);
    }
    return NULL;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int Tape_Read (Tape_t* tape, char* buf, unsigned nbyte, int ignerr)
{
    register int nread;

    if (DataOnDisk(tape)) {
	char name[256], *p;
	struct stat fs;
	off_t pos;
	/* Re-open file using buffer I/O to read in the rest */

	if ((p = "fstat", fstat(tape->data_fd, &fs)) == -1 ||
	    (p = "lseek", (pos = lseek(tape->data_fd, 0, SEEK_CUR))) == -1) {

	    sprintf(name, "%s.%d", tape->data_port, tape->file_num);
	    (*tape->Log)(tape->err, "%s: %s error, %s\n",
			 name, p, strerror(errno));
	    return 0;
	}
	if (fs.st_size == pos) {
	    close(tape->data_fd);

	    sprintf(name, "%s.%d", tape->data_port, ++tape->file_num);
	    if ((tape->data_fd = open(
		    (p = strchr(name, ':')) ? p+1 : name, O_RDONLY | IO_TYPE ))
		== -1) {
		(*tape->Log)(tape->err, "%s: open error, %s\n",
			     name, strerror(errno));
		return 0;
	    }
	    if (fstat(tape->data_fd, &fs) == -1) {
		(*tape->Log)(tape->err, "%s: fstat error, %s\n",
			     name, strerror(errno));
		return 0;
	    }
	    pos = 0;
	}
	else if (fs.st_size - pos < nbyte) {
	    register int n = fs.st_size - pos;	

	    nread = read(tape->data_fd, buf, n);
	    if (nread != n) {
		sprintf(name, "%s.%d", tape->data_port, tape->file_num);
		(*tape->Log)(tape->err,
		    "%s: read error, got %d of %d bytes, %s\n",
		    name, nread, n, strerror(errno));
		return 0;
	    }
	    close(tape->data_fd);

	    sprintf(name, "%s.%d", tape->data_port, ++tape->file_num);
	    if ((tape->data_fd = open(
		    (p = strchr(name, ':')) ? p+1 : name, O_RDONLY))
		== -1) {
		(*tape->Log)(tape->err, "%s: open error, %s\n",
			     name, strerror(errno));
		return 0;
	    }
	    nread = read(tape->data_fd, buf+n, nbyte-n);
	    if (nread != nbyte-n) {
		(*tape->Log)(tape->err,
		    "%s: read error, got %d of %d bytes, %s\n",
		    name, nread, nbyte-n, strerror(errno));
		return 0;
	    }
	    return nbyte;
	}
    }
    if (DataSource(tape) == DCRSI_TAPE && !DataOnDisk(tape)) {

        register int nb = tape->blk_found - tape->blk_start;
	register int blk_count = nbyte/tape->blk_size;

	struct timeval timeout = {0, 0};
	fd_set read_fds;

	FD_ZERO(&read_fds);
	FD_SET(tape->ctrl_fd, &read_fds);

	if (select(FD_SETSIZE, &read_fds, NULL, NULL, &timeout) > 0) {
	    char buf[256];
	    register int n = read(tape->ctrl_fd, buf, sizeof(buf)-1);
	    buf[n] = 0;
	    printfLLog(LOG_DEBUG, "%s\n", buf);
	}
	if (tape->blk_found > tape->blk_start) {
	    tape->blk_found -= blk_count;
	    if (nb >= blk_count)
		memset(buf, 0, nread = nbyte);
	    else {
		memset(buf, 0, nb *= tape->blk_size);
		nread = read(tape->data_fd, buf + nb, nbyte - nb);
		if (nread != -1) nread += nb;
	    }
	    return nread;
        }
    }
    if (tape->sony_err % 512) {
	tape->sony_err += nbyte/SONY_BLOCK_SIZE;
	memset(buf, 0, nread = nbyte);
    }
    else {
	nread = read(tape->data_fd, buf, nbyte);

	if (nread == nbyte)
	    tape->sony_err = tape->seek_retry * 512;

	else if (! tape->reverse ||
		DataOnDisk(tape) || DataSource(tape) != SONY_TAPE ||
		tape->sony_err >= tape->seek_retry * 512) {

	    int errnum = errno;
	    char name[256];
	    name[0] = 0;

	    if (DataOnDisk(tape))
		sprintf(name, "%s.%d: ", tape->data_port, tape->file_num);

	    if (ignerr)
		printfLLog(LOG_ERR, "%sread error, got %d of %d bytes, %s\n",
			 name, nread, nbyte, strerror(errno));
	    else
		(*tape->Log)(tape->err,"%sread error, got %d of %d bytes, %s\n",
			 name, nread, nbyte, strerror(errno));
	    errno = errnum;
	}
	else if (tape->reverse) {
	    register int blk_end = tape->blk_end;
	    Tape_Destroy(tape);
	    sleep(2);
	    tape->blk_end -= tape->sony_err + 512;
	    printfLLog(LOG_ERR, "Backing off to track #%d...", tape->blk_end);
	    if (! Open(tape))
		return NULL;
	    tape->blk_end = blk_end;
	    memset(buf, 0, nread = nbyte);
	    tape->sony_err += nbyte/SONY_BLOCK_SIZE;
	}
	else {
	    register int blk_start = tape->blk_start;
	    Tape_Destroy(tape);
	    sleep(2);
	    tape->blk_start += tape->sony_err + 512;
	    printfLLog(LOG_ERR, "Forwarding to track #%d...", tape->blk_start);
	    if (! Open(tape))
		return NULL;
	    tape->blk_start = blk_start;
	    memset(buf, 0, nread = nbyte);
	    tape->sony_err += nbyte/SONY_BLOCK_SIZE;
	}
    }
    if (tape->sony_check && tape->sony_err >= tape->seek_retry * 512) {
	/*  Need to check tape label */

        CMD_PASS_THRU UserDefinedPassThruParams;
	register char id[8], *label = UserDefinedPassThruParams.data;

        memset(&UserDefinedPassThruParams, 0, sizeof(CMD_PASS_THRU));
        UserDefinedPassThruParams.keyword1 = 0x50;
        UserDefinedPassThruParams.keyword2 = 0x06;

        if (ioctl(tape->data_fd, PASSTHRU_COMMAND, &UserDefinedPassThruParams)
            == -1) {
            (*tape->Log)(tape->err,
		"Can't sense tape label - ML930_CMD_PASS_THRU failed, %s\n",
		strerror(errno));
            return 0;
        }
	if (!strstr(label, tape->label) &&
	    (strlen(tape->label) < 12 || 
	    (id[0] = tape->label[2], id[1] = tape->label[3],
	     id[2] = tape->label[8], id[3] = tape->label[9],
	     id[4] = tape->label[10],id[5] = tape->label[11], id[6] = 0,
	    !strstr(label, id))))  {

	    (*tape->Log)(tape->err, "tape label mismatch, %s", label);
	    return 0;
	}
	tape->sony_check = 0;
    }
    return nread;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
Tape_t* Open(Tape_t* tape)
{
    if (DataOnDisk(tape))
	return Tape_Seek(tape, tape->blk_start);

    if (DataSource(tape) == DCRSI_TAPE) {
	struct termios tio;
	char ack[RESET_ACKLEN+1];
	int vdi_mode  = VDI_MASTER_MODE;

	tape->data_port = "/dev/vdi_0";		/* Default */
	tape->ctrl_port = "/dev/vdi_sp0";

        if ((tape->data_fd = open(tape->data_port, O_RDONLY)) == -1) {
	    (*tape->Log)(tape->err, "can't open %s: %s\n",
			 tape->data_port, strerror(errno));
	    return NULL;
        }
        if ((tape->ctrl_fd = open(tape->ctrl_port, O_RDWR)) == -1) {
	    (*tape->Log)(tape->err, "can't open %s: %s\n",
			 tape->ctrl_port, strerror(errno));
	    close(tape->data_fd);
	    tape->data_fd = -1;
            return NULL;
        }
	strcpy(ctrl_port_last, tape->ctrl_port);

        ioctl(tape->ctrl_fd, TCGETS, &tio);
        tio.c_cflag = B9600 | PARENB | HUPCL | CLOCAL | CREAD | 
		      (tape->dcrsi_csize == 7 ? CS7 : CS8);
        tio.c_iflag|= IGNCR | IXOFF;/* ignore CR */
        tio.c_lflag = 0;            /* no local character processing */
        tio.c_oflag = 0;            /* no output character processing */
        tio.c_cc[4] = 1;            /* MIN receive chars = 0 */
        tio.c_cc[5] = 1;            /* timeout on read is 1/10sec */
    
        ioctl(tape->ctrl_fd, TCSETS, &tio);
        ack[0] = 0;
    
        if (SendCmd(tape, ";RS;", ResetCheck, ack, NULL) <= 0) {
	    (*tape->Log)(tape->err, "%s", ack[0]
    	    		 ? "Invalid response to Reset Command"
			 : "No response from tape drive");
	    close(tape->data_fd);
	    close(tape->ctrl_fd);
	    tape->data_fd = tape->ctrl_fd = -1;
	    return NULL;
        }
        SendCmd(tape, ";EE;PD;", IsInList, "**",0, NULL);
    
        if (ioctl(tape->data_fd, VDI_D_INITIALISE, (char*) &vdi_mode) == -1) {
            (*tape->Log)(tape->err, "VDI_INITIALISE fails, errno %d\n", errno);
	    close(tape->data_fd);
	    close(tape->ctrl_fd);
	    tape->data_fd = tape->ctrl_fd = -1;
	    return NULL;
        }
        if (! Tape_Seek(tape, tape->blk_start)) {
	    SendCmd(tape, ";SL;", IsInList, "DS 4011;","DS 4000;",0, NULL);
	    SendCmd(tape, ";MD;UL;", IsInList, "*",0, NULL);
	    close(tape->data_fd);
	    close(tape->ctrl_fd);
	    tape->data_fd = tape->ctrl_fd = -1;
	    return NULL;
        }	
        return tape;
    }

    if (DataSource(tape) == SONY_TAPE) {
	register int i;
        DRIVER_SETUP UserDefinedSetupParams;
        CMD_INIT_MACRO UserDefinedInitParams;

	/*  New option for setting bus request level  */
        CMD_PASS_THRU UserDefinedPassThruParams;

	tape->data_port = "/dev/ml930a";	/* Default */

      /*  Due to Sony ID-1 pecularity, we need to retry before giving up */
      for (i = 1; i <= tape->seek_retry; ++i) {

        if ((tape->data_fd = open(tape->data_port, O_RDONLY)) == -1) {
	    (*tape->Log)(tape->err, "can't open %s: %s\n",
			 tape->data_port, strerror(errno));
	    return NULL;
        }
	/*  Reset DIR-930  */

	if (ioctl(tape->data_fd, RESET_DIR930, 0) == -1) {
	    if (i == tape->seek_retry)
		(*tape->Log)(tape->err, "%s: RESET_DIR930 failed, %s\n",
		  tape->data_port, strerror(errno));
            close(tape->data_fd);
	    tape->data_fd = -1;
	    if (i < tape->seek_retry) {
		sleep(2);
		continue;
	    }
	    return NULL;
	}
	sleep(1);

	/* The driver setup MUST be done before the InitParams!!! */

        memset(&UserDefinedSetupParams, 0, sizeof(DRIVER_SETUP));
        UserDefinedSetupParams.buffered_size = 0;       /* unbuffered */
        UserDefinedSetupParams.vme_device_addr= 0;      /* user space */

        if (ioctl(tape->data_fd, ML930_SETUP, &UserDefinedSetupParams) == -1) {
	    if (i == tape->seek_retry)
		(*tape->Log)(tape->err, "%s: ML930_SETUP failed, %s\n",
		  tape->data_port, strerror(errno));
            close(tape->data_fd);
	    tape->data_fd = -1;
	    if (i < tape->seek_retry) {
		sleep(2);
		continue;
	    }
	    return NULL;
        }
	/* Initialize board */

        memset(&UserDefinedInitParams, 0, sizeof(CMD_INIT_MACRO));
        UserDefinedInitParams.dataRate  = tape->reverse ? 2 : 1;
					        /* 1=128 Mbits/s, 2=64Mb/s */
        UserDefinedInitParams.xferMode  = 1;    /* VME64 DMA Transfers */
        UserDefinedInitParams.byteOrder = tape->reverse ? 5 : 1;
						/* bits(0:7) first */
        UserDefinedInitParams.rcdrType  = 1;    /* DFC-1800 */

        if (ioctl(tape->data_fd, INIT_MACRO, &UserDefinedInitParams) == -1) {
	    if (i == tape->seek_retry)
		(*tape->Log)(tape->err, "%s: INIT_MACRO failed, %s\n",
		  tape->data_port, strerror(errno));
            close(tape->data_fd);
	    tape->data_fd = -1;
	    if (i < tape->seek_retry) {
		sleep(2);
		continue;
	    }
	    return NULL;
        }
        memset(&UserDefinedPassThruParams, 0, sizeof(CMD_PASS_THRU));

	/* VMEPARAM:  keyword=0xf809 */
        UserDefinedPassThruParams.keyword1 = 0xf8;
        UserDefinedPassThruParams.keyword2 = 0x09;

	/* Subcommand ID - 0 = Set Bus Request Level */
        UserDefinedPassThruParams.data[1] = 0;

	/* Bus Request Level = 0 */
        UserDefinedPassThruParams.data[2] = 0;

	/* Reserved - set to 0 */
        UserDefinedPassThruParams.data[3] = 0;
        UserDefinedPassThruParams.data[4] = 0;

        if (ioctl(tape->data_fd, PASSTHRU_COMMAND, &UserDefinedPassThruParams)
	    == -1) {
	    if (i == tape->seek_retry)
		(*tape->Log)(tape->err, "%s: PASSTHRU_COMMAND failed, %s\n",
		  tape->data_port, strerror(errno));

            close(tape->data_fd);
	    tape->data_fd = -1;
	    if (i < tape->seek_retry) {
		sleep(2);
		continue;
	    }
	    return NULL;
        }
        /*  New option for forcing the DFC to supply its own playback clock */
        memset(&UserDefinedPassThruParams, 0, sizeof(CMD_PASS_THRU));

        /* DFC Passthrough command :  keyword=0x4813 */
        UserDefinedPassThruParams.keyword1 = 0x48;
        UserDefinedPassThruParams.keyword2 = 0x13;

        /* Subcommand ID - 4 = DFC Passthrough command sequence */
        UserDefinedPassThruParams.data[0] = 4;
        UserDefinedPassThruParams.data[1] = 0x8E;
        UserDefinedPassThruParams.data[2] = 0;
        UserDefinedPassThruParams.data[3] = 0x82;
        UserDefinedPassThruParams.data[4] = 0;

	if (ioctl(tape->data_fd, PASSTHRU_COMMAND, &UserDefinedPassThruParams)
	    == -1) {
	    if (i == tape->seek_retry)
		(*tape->Log)(tape->err,
		    "%s: ML930_CMD_PASS_THRU for DFC PB clock failed, %s\n",
		    tape->data_port, strerror(errno));

	    close(tape->data_fd);
	    tape->data_fd = -1;
	    if (i < tape->seek_retry) {
		sleep(2);
		continue;
	    }
	    return NULL;
        }
	/*  VRB Set Remote On */
	memset(&UserDefinedPassThruParams, 0, sizeof(CMD_PASS_THRU));
	UserDefinedPassThruParams.keyword1 = 0x40;
	UserDefinedPassThruParams.keyword2 = 0x10;

	if (ioctl(tape->data_fd, PASSTHRU_COMMAND, &UserDefinedPassThruParams)
	    == -1) {
	    if (i == tape->seek_retry)
		(*tape->Log)(tape->err,
		"%s: can't VRB set remote on - PASSTHRU_COMMAND failed, %s\n",
		     tape->data_port, strerror(errno));

            close(tape->data_fd);
	    tape->data_fd = -1;
	    if (i < tape->seek_retry) {
		sleep(2);
		continue;
	    }
	    return NULL;
	}
	/* Sony initialized OK - break out of retry loop */
	break;

      } /* end of retries */

	if (! Tape_Seek(tape,
		tape->reverse ? (tape->blk_end-1) : tape->blk_start)) {
	    close(tape->data_fd);
	    tape->data_fd = -1;
	    return NULL;
	}
	return tape;
    }
    return NULL;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
Tape_t* Tape_Init( Tape_t* tape, 
	int blk_start, int blk_end, int reverse,
	int (*error_log)(), void* error_handle,
	int source, int dataOnDisk,
	char* tape_label, int dsk_start,
	int seek_timer, int seek_retry)
{
    tape->blk_start = tape->blk_found = blk_start;
    tape->blk_end = blk_end + 1;
    tape->reverse = reverse;
    tape->Log = error_log;
    tape->err = error_handle;
    tape->seek_timer = seek_timer;
    tape->seek_retry = seek_retry;
    tape->dcrsi_csize = dsk_start;
    tape->source = dataOnDisk ? (source|2) : (source & ~2);
    tape->label = tape->data_port = tape_label;
    tape->file_blk = dsk_start;
    tape->file_num = tape->quit = tape->sony_err = 0;
    tape->data_fd  = tape->ctrl_fd = -1;
    tape->sony_check = 
	(source == SONY_TAPE && !dataOnDisk && tape_label) ? 1 : 0;
    tape->blk_size = 
	(source == SONY_TAPE ? SONY_BLOCK_SIZE : DCRSI_BLOCK_SIZE);

    sleep(1);
    return Open(tape);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void Tape_Destroy(Tape_t* tape)
{
    if (DataOnDisk(tape)) {
	/*  Do notthing */
    }
    else if (DataSource(tape) == DCRSI_TAPE) {
	if (tape->ctrl_fd != -1) {
	    SendCmd(tape, ";SL;", IsInList,"DS 4011;","DS 4000;","DS 4100",0,0);
	    SendCmd(tape, ";MD;UL;", IsInList, "*",0, NULL);
	    close(tape->ctrl_fd);
	    tape->ctrl_fd = -1;
	}
	if (tape->data_fd != -1) {
	    int operation = VDI_NO_OPERATION;
	    ioctl(tape->data_fd, VDI_D_STOP_OPERATION, 0);
	    ioctl(tape->data_fd, VDI_D_SET_OPERATION, &operation);
	}
    }
    else if (DataSource(tape) == SONY_TAPE) {
        CMD_STOP_MACRO UserDefinedStopParams;
        memset(&UserDefinedStopParams, 0, sizeof(CMD_STOP_MACRO));
        ioctl(tape->data_fd, STOP_MACRO, &UserDefinedStopParams);
    }
    close(tape->data_fd);
    tape->data_fd = -1;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int Tape_Shutdown(Tape_t* tape)
{
    int ctrl_fd;
    struct termios tio;

    if (DataOnDisk(tape) || DataSource(tape) != DCRSI_TAPE)
	return 0;

    if ((ctrl_fd = open(ctrl_port_last, O_RDWR)) == -1)
        return -1;

    ioctl(ctrl_fd, TCGETS, &tio);
    tio.c_cflag = B9600 | PARENB | HUPCL | CLOCAL | CREAD | 
		  (tape->dcrsi_csize == 7 ? CS7 : CS8);
    tio.c_iflag|= IGNCR | IXOFF;/* ignore CR */
    tio.c_lflag = 0;            /* no local character processing */
    tio.c_oflag = 0;            /* no output character processing */
    tio.c_cc[4] = 1;            /* MIN receive chars = 0 */
    tio.c_cc[5] = 1;            /* timeout on read is 1/10sec */

    ioctl(ctrl_fd, TCSETS, &tio);
    tape->ctrl_fd = ctrl_fd;
    SendCmd(tape, ";SL;", IsInList, "DS 4011;","DS 4000;","DS 4100",0, NULL);
    SendCmd(tape, ";MD;UL;", IsInList, "*",0, NULL);
    tape->ctrl_fd = -1;

    return 0;
}
