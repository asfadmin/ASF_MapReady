/*=============================================================================
 |  @(#)tape.c	1.9 97/03/20 17:27:58
 |
 |  DCRSI & SONY-ID1 Tape Reader Module.
 |  Alaska SAR Facility (ASF) Project.
 |
 |  Copyright (C) Jet Propulsion Laboratory.
 |
 *============================================================================*/

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <termios.h>
#include "tape.h"
#include "vdi_io.h"
#include "ml930drv.h"

static char sccsid_tape_c[] =
        "@(#)tape.c	1.9 97/03/20 17:27:58";

static char  vdi_ctrl_port[]	= "/dev/vdi_sp0";
static char  vdi_data_port[]	= "/dev/vdi_0";
static char  dir_data_port[]	= "/dev/ml930a";
static char *data_port_name 	= NULL;
static char *ctrl_port_name 	= vdi_ctrl_port;
static int   ctrl_fd[FD_SETSIZE];

int byte_order;

static
int tape_ctrl(int port_fd, char *cmd, ...)
{
    register int n, i = 0, buf_len = 0, ack_cnt = 0;
    static struct timeval timeout = {90, 0};
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
    printf("\n%s\n", cmd); fflush(stdout);
#endif
    n = strlen(cmd);

    if (write(port_fd, cmd, n) != n) {
	printf("Write error on DCRSi CMD channel: %s\n", strerror(errno));
	return -1;
    }
    cmp = (int (*)()) va_arg(ap, void*);

    while (cmp && buf_len < sizeof(buf)-1) {
	fd_set read_fds;

	FD_ZERO(&read_fds);
	FD_SET(port_fd, &read_fds);

	do n = select(FD_SETSIZE, &read_fds, NULL, NULL, &timeout);
	while (n == -1 && errno == EINTR);
	
	if (n == -1) {
	    printf("Select error on DCRsi CMD channel: %s.\n", strerror(errno));
	    return -1;
	}
	if (n == 0) {
/*	    printf("No response from DCRsi CMD channel for %d secs.\n",
		    timeout.tv_sec);
*/	    return -1;
	}
	do n = read(port_fd, buf+buf_len, (sizeof(buf)-1)-buf_len);
	while (n == -1 && errno == EINTR);

	if (n == -1) {
	    printf("Read error on DCRsi CMD channel: %s.\n", strerror(errno));
	    return -1;
	}
	buf[buf_len += n] = 0;
#ifndef DEBUG
	printf("%s", &buf[buf_len-n]); fflush(stdout);
#endif
	for (;;) {
	    ap_new = ap;
	    if ((n = (*cmp)(&buf[i], buf_len-i, &ap_new)) == 0)
		break;
	    if (n < 0)
		return -1;
	    i += n;
	    ack_cnt++;
	    ap = ap_new;
	    if (!(cmp = (int (*)()) va_arg(ap, void*)))
		break;
	}
    }
    va_end(ap);
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
int AND_cmp(char* buf, int len, va_list* ap)
{
    char *p, *pat = va_arg(*ap, char*);
    int  n = strlen(pat);
    return (n <= len && (p = strstr(buf, pat))) ? (n += p-buf) : 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int OR_cmp(char* buf, int len, va_list* ap)
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
int DF_cmp(char* buf, int len, va_list* ap)
{
    int n;
    register char* p;
    if ((p = strstr(buf, "DF ")) == NULL || strlen(p) <= 7)
	return 0;

    return (sscanf(p+3, "%d", &n) != 1 || n == 0) ? -1 : (p-buf+8);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
off_t tape_seek(int data_fd, off_t block_num, int whence)
{
    int  operation = VDI_REPLAY_P_DATA;
    char cmd[32], ack[32];

    if (data_port_name == dir_data_port) {
	CMD_PLAYBACK_MACRO UserDefinedPlayParams;

	memset(&UserDefinedPlayParams, 0, sizeof(CMD_PLAYBACK_MACRO));
	UserDefinedPlayParams.searchType = whence; /* From specified trackset */
	UserDefinedPlayParams.tracksetID = block_num;
	UserDefinedPlayParams.startFlag  = 1;

	if (ioctl(data_fd, PLAYBACK_MACRO, &UserDefinedPlayParams) == -1) {
	    printf("PLAYBACK_MACRO failed, %d\n", errno);
	    return -1;
	}
	return (block_num);
    }
    tape_ctrl(ctrl_fd[data_fd], ";EE;PD;", AND_cmp,"**", NULL);

    sprintf(cmd, ";PL A%d;", block_num);
    sprintf(ack, "DL B-%.8d", block_num);

    if (tape_ctrl(ctrl_fd[data_fd], cmd,
	OR_cmp,"DS C022;","DS 4011;",/*"DS 4000;",*/ 0, NULL) != 1)
	return -1;

    if (ioctl(data_fd, VDI_D_SET_OPERATION, (char *)&operation) == -1) {
        printf("VDI_D_SET_OPERATION failed, errno %d\n", errno);
	return -1;
    }
    if (ioctl(data_fd, VDI_D_START_OPERATION, 0) == -1) {
        printf("VDI_START_OPERATION failed, errno %d\n", errno);
	return -1;
    }
    printf("\n");
    return (block_num);
}


/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int tape_open(char *vdi_name, int oflag, int csize)
{
    int   data_fd;
    int   vdi_mode = VDI_MASTER_MODE;
    int   operation = VDI_REPLAY_P_DATA;

    struct termios tio;
    struct stat fs;

    if (strcasecmp(vdi_name, "dcrsi") == 0)
	data_port_name = vdi_data_port;

    else if (strcasecmp(vdi_name, "sony") == 0)
	data_port_name = dir_data_port;

    else {
	printf("%s: not a valid tape device name\n", vdi_name);
	return -1;
    }
    if ((data_fd = open(data_port_name, oflag)) == -1) {
	printf("%s: %s\n", data_port_name, strerror(errno));
	return -1;
    }
    if (data_port_name == dir_data_port) {
	DRIVER_SETUP UserDefinedSetupParams;
	CMD_INIT_MACRO UserDefinedInitParams;
	CMD_PASS_THRU UserDefinedPassThruParams;

	/* The driver setup MUST be done before the InitParams!!! */
	memset(&UserDefinedSetupParams, 0, sizeof(DRIVER_SETUP));

	UserDefinedSetupParams.buffered_size = 0;	/* unbuffered */
	UserDefinedSetupParams.vme_device_addr= 0;	/* user space */

	if (ioctl(data_fd, ML930_SETUP, &UserDefinedSetupParams) == -1) {
	    printf("%s: ML930_SETUP failed, %s\n", data_port_name,
		   strerror(errno));
	    close(data_fd);
	    return -1;
	}
	/* Initialize board */
	memset(&UserDefinedInitParams, 0, sizeof(CMD_INIT_MACRO));

	UserDefinedInitParams.dataRate  = 1;	/* 128 Mbits/sec */
	UserDefinedInitParams.xferMode  = 1;	/* VME64 DMA Transfers */
/*	UserDefinedInitParams.byteOrder = 0;	bits(24:31) first */
	UserDefinedInitParams.byteOrder = byte_order;	/* bits(0:7) first */
	UserDefinedInitParams.rcdrType  = 1; 	/* DFC-1800 */

	if (ioctl(data_fd, INIT_MACRO, &UserDefinedInitParams) == -1) {
	    printf("%s: INIT_MACRO failed, %s\n", data_port_name,
		   strerror(errno));
	    close(data_fd);
	    return -1;
	}
	/*  New option for setting bus request level  */
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

	if( ioctl(data_fd, PASSTHRU_COMMAND, &UserDefinedPassThruParams) == -1){
	    printf("%s: ML930_CMD_PASS_THRU failed, %s\n",
		data_port_name, strerror( errno ) );
	    close(data_fd);
	    return -1;
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

        if (ioctl(data_fd, PASSTHRU_COMMAND, &UserDefinedPassThruParams)
            == -1) {
            printf("%s: ML930_CMD_PASS_THRU for DFC PB clock failed, %s\n",
                   data_port_name, strerror(errno));
            close(data_fd);
            return -1;
        }
	return (data_fd);

    } /*  End openning DIR  */

    if ((ctrl_fd[data_fd] = open(ctrl_port_name, O_RDWR)) == -1) {
	close(data_fd);
        return -1;
    }
    ioctl(ctrl_fd[data_fd], TCGETS, &tio);

    tio.c_cflag = B9600 | PARENB | HUPCL | CLOCAL | CREAD |
		  (csize == 7 ? CS7 : CS8);
    tio.c_iflag|= IGNCR | IXOFF; /* ignore CR */
    tio.c_lflag = 0;            /* no local character processing */
    tio.c_oflag = 0;            /* no output character processing */
    tio.c_cc[4] = 1;            /* MIN receive chars = 0 */
    tio.c_cc[5] = 1;            /* timeout on read is 1/10sec */

    ioctl(ctrl_fd[data_fd], TCSETS, &tio);

    printf("Connected to %s at 9600 baud, %d bits/char.\n",
	ctrl_port_name, csize == 7 ? 7 : 8);

    if (tape_ctrl(ctrl_fd[data_fd], ";RS;",
	AND_cmp, "Revision",
	OR_cmp,"DS 4000;","DS 4011;","DS C022;",0, NULL) != 2) {
        printf("can't reset drive, %s\n", strerror(errno));
	close(data_fd);
	close(ctrl_fd[data_fd]);
	return -1;
    }
    if (ioctl(data_fd, VDI_D_INITIALISE, (char *)&vdi_mode) == -1) {
        printf("VDI_INITIALISE failed, errno %d\n", errno);
	close(data_fd);
	close(ctrl_fd[data_fd]);
	return -1;
    }
    printf("\n");
    return (data_fd);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int tape_close(int data_fd)
{
    int operation = VDI_NO_OPERATION;

    if (data_port_name == dir_data_port) {
	CMD_STOP_MACRO UserDefinedStopParams;
	memset(&UserDefinedStopParams, 0, sizeof(CMD_STOP_MACRO));

	ioctl(data_fd, STOP_MACRO, &UserDefinedStopParams);
	return (close(data_fd));
    }
    ioctl(data_fd, VDI_D_STOP_OPERATION, 0);
    ioctl(data_fd, VDI_D_SET_OPERATION, &operation);

    tape_ctrl(ctrl_fd[data_fd], ";SL;MD;UL;", AND_cmp, "DS 4011;", NULL);
    close(ctrl_fd[data_fd]);
    printf("\n");
    return (close(data_fd));
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int tape_read(int data_fd, char *buffer, int nbytes)
{
    register int n;
#ifndef DEBUG
    if (data_port_name == vdi_data_port) {
	static struct timeval timeout = {0, 0};
	register int n;
	fd_set read_fds;

	FD_ZERO(&read_fds);
	FD_SET(ctrl_fd[data_fd], &read_fds);

	n = select(FD_SETSIZE, &read_fds, NULL, NULL, &timeout);
	if (n > 0 && (n = read(ctrl_fd[data_fd], buffer, nbytes)) > 0) {
	    buffer[n] = 0;
	    printf("%s\n", buffer);
	}
    }
#endif
    n = read(data_fd, buffer, nbytes);
    return n;
}
