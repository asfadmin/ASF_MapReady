/*=============================================================================
 |  @(#)rewind.c	1.1 96/07/17 11:56:39
 |
 |  Stopit Utility
 |  Alaska SAR Facility (ASF) Project.
 |  Copyright (C) Jet Propulsion Laboratory.
 |
 *============================================================================*/
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <termios.h>
#include <sys/time.h>
#include <sys/types.h>

static char sccsid_stopit_c[] =
        "@(#)rewind.c	1.1 96/07/17 11:56:39";

static char *tape_name	= "/dev/vdi_0";
static char *ctrl_port	= "/dev/vdi_sp0";
static char *usage = "usage: %s [-c 7|8]\n";

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
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
int main (int argc, char *argv[])
{
    register int i, fd;
    int csize = 8;
    struct termios tio;
    register char *id;

    /*----------------------------------*
     |  Extract command-line arguments
     *----------------------------------*/
    id = ((id = strrchr(argv[0], '/')) ? id+1 : argv[0]);
    if (argc > 3 || argc == 2) {
	printf(usage, id);
	exit(-1);
    }
    for (i = 1; i < argc; ++i) {
	if (strcmp(argv[i], "-c") == 0) {
	    if (sscanf(argv[++i], "%d", &csize) != 1 || csize<7 || csize>8) {
		printf("%s not a valid value\n", argv[i]);
		exit(-1);
	    }
	}
	else {
	    printf("%s not a valid option\n", argv[i]);
	    printf(usage, id);
	    exit(-1);
	}
    }
    if ((fd = open(ctrl_port, O_RDWR)) == -1) {
	printf("%s: can't open %s, %s\n", id, ctrl_port, strerror(errno));
        close(fd);
        return -1;
    }
    ioctl(fd, TCGETS, &tio);
    tio.c_cflag = B9600|PARENB|HUPCL|CLOCAL|CREAD| (csize == 7 ? CS7 : CS8);
    tio.c_iflag|= IGNCR | IXOFF; /* ignore CR */
    tio.c_lflag = 0;            /* no local character processing */
    tio.c_oflag = 0;            /* no output character processing */
    tio.c_cc[4] = 1;            /* MIN receive chars = 0 */
    tio.c_cc[5] = 1;            /* timeout on read is 1/10sec */
    ioctl(fd, TCSETS, &tio);

    printf("Connected to %s at 9600 baud, %d bits/char.\n",
        ctrl_port, csize == 7 ? 7 : 8);

    tape_ctrl(fd, ";RW;", OR_cmp, "DS 4055;","DS 4000;",0,  NULL);
    tape_ctrl(fd, ";SL;", AND_cmp, "DS 4000", NULL);
    tape_ctrl(fd, ";MD;UL;", AND_cmp, "*", NULL);

    close(fd);
    printf("\n");
}
