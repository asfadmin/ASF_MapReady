/*
 *	EOSDIS Version 0 IMS Client telnet server.  Adapted from
 *	the BSD telnetd.
 */
/*
 * Copyright (c) 1983, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)telnetd.c	5.31 (Berkeley) 2/23/89";
#endif /* not lint */

/*
 * Telnet server.
 */
#ifdef sgi
#define SYSV
/* Access Version 2. Fixes compile time errors on SGI. */
#include <unistd.h>
#endif

#ifdef _AIX 
#include <sys/types.h>
#include <sys/select.h>
#endif

#ifdef hpux
#include <sys/ptyio.h>
#endif

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <netinet/in.h>

#include <arpa/telnet.h>

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#ifdef SYSV
#include <termio.h>
#include <sys/ioctl.h>
#else	/* not SYSV */
#include <sgtty.h>
#endif	/* not SYSV */

#ifdef SOLARIS

#include <unistd.h>
#include <sys/types.h>
#include <sys/conf.h>
#include <sys/filio.h>
#include <sys/tty.h>
#include <sys/ptyvar.h>
#include <stropts.h>
#include <fcntl.h>
#endif

#include <netdb.h>
#include <syslog.h>
#include <ctype.h>
#include <utmp.h>
#ifndef UTMP_FILE
#define UTMP_FILE "/etc/utmp"
#endif

#ifdef DGUX
#define TELOPT_TTYPE	TELOPT_TTY_TYPE
#define TELQUAL_IS	0	/* option is... */
#define TELQUAL_SEND	1	/* send option */
#endif /* DGUX */

#ifndef TELOPT_XDISPLOC
#define TELOPT_XDISPLOC	35
#endif /* TELOPT_XDISPLOC */

#define	OPT_NO			0		/* won't do this option */
#define	OPT_YES			1		/* will do this option */
#define	OPT_YES_BUT_ALWAYS_LOOK	2
#define	OPT_NO_BUT_ALWAYS_LOOK	3
char	hisopts[256];
char	myopts[256];

#ifndef L_INCR 
#define L_INCR SEEK_CUR
#endif

#ifdef SOLARIS
unsigned char	doopt[] = { IAC, DO, '%', 'c', 0 };
#else
char	doopt[] = { IAC, DO, '%', 'c', 0 };
#endif

#ifdef SOLARIS
unsigned char	dont[] = { IAC, DONT, '%', 'c', 0 };
#else
char	dont[] = { IAC, DONT, '%', 'c', 0 };
#endif

#ifdef SOLARIS
unsigned char	will[] = { IAC, WILL, '%', 'c', 0 };
#else
char	will[] = { IAC, WILL, '%', 'c', 0 };
#endif

#ifdef SOLARIS
unsigned char	wont[] = { IAC, WONT, '%', 'c', 0 };
#else
char	wont[] = { IAC, WONT, '%', 'c', 0 };
#endif

/*
 * I/O data buffers, pointers, and counters.
 */
char	ptyibuf[BUFSIZ], *ptyip = ptyibuf;

char	ptyobuf[BUFSIZ], *pfrontp = ptyobuf, *pbackp = ptyobuf;

char	netibuf[BUFSIZ], *netip = netibuf;
#define	NIACCUM(c)	{   *netip++ = c; \
			    ncc++; \
			}

char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;
char	*neturg = 0;		/* one past last bye of urgent data */
	/* the remote system seems to NOT be an old 4.2 */
int	not42 = 1;

#define	BANNER	"\r\n\r\nEOSDIS Version 0 IMS\r\n\r\r\n\r"

		/* buffer for sub-options */
char	subbuffer[100], *subpointer= subbuffer, *subend= subbuffer;
#define	SB_CLEAR()	subpointer = subbuffer;
#define	SB_TERM()	{ subend = subpointer; SB_CLEAR(); }
#define	SB_ACCUM(c)	if (subpointer < (subbuffer+sizeof subbuffer)) { \
				*subpointer++ = (c); \
			}
#define	SB_GET()	((*subpointer++)&0xff)
#define	SB_EOF()	(subpointer >= subend)

int	pcc, ncc;

int	pty, net;
int	inter;
extern	char **environ;
extern	int errno;
char	*line;
int	SYNCHing = 0;		/* we are in TELNET SYNCH mode */
/*
 * The following are some clocks used to decide how to interpret
 * the relationship between various variables.
 */

struct {
    int
	system,			/* what the current time is */
	echotoggle,		/* last time user entered echo character */
	modenegotiated,		/* last time operating mode negotiated */
	didnetreceive,		/* last time we read data from network */
	ttypeopt,		/* ttype will/won't received */
	ttypesubopt,		/* ttype subopt is received */
	getterminal,		/* time started to get terminal information */
	dispopt,		/* display will/won't received */
	dispsubopt,		/* display subopt is received */
	getdisplay,		/* time started to get display information */
	gotDM;			/* when did we last see a data mark */
} clocks;

#define	settimer(x)	(clocks.x = ++clocks.system)
#define	sequenceIs(x,y)	(clocks.x < clocks.y)

char	*Program_name;		/* program name from argv[0] */
char	*Config_name;		  /* config file specification from argv[1] */
int 	glbl_child;

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in from;
	int on = 1, fromlen;

	Program_name = argv[0];
	Config_name = argv[1];

#if	defined(DEBUG)
	{
	    int s, ns, foo;
	    struct servent *sp;
	    static struct sockaddr_in sin = { AF_INET };
		int optval;

	    sp = getservbyname("telnet", "tcp");
	    if (sp == 0) {
		    fprintf(stderr, "telnetd: tcp/telnet: unknown service\n");
		    exit(1);
	    }
	    sin.sin_port = sp->s_port;
	    argc--, argv++;
	    if (argc > 0) {
		    sin.sin_port = atoi(*argv);
		    sin.sin_port = htons((u_short)sin.sin_port);
	    }

	    s = socket(AF_INET, SOCK_STREAM, 0);
	    if (s < 0) {
		    perror("telnetd: socket");;
		    exit(1);
	    }

		optval = 1;

		(void) setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
							(char *) &optval, sizeof(optval));

	    if (bind(s, (struct sockaddr *) &sin, sizeof sin) < 0) {
		perror("bind");
		exit(1);
	    }
	    if (listen(s, 1) < 0) {
		perror("listen");
		exit(1);
	    }
	    foo = sizeof sin;
	    ns = accept(s, (struct sockaddr *) &sin, &foo);
	    if (ns < 0) {
		perror("accept");
		exit(1);
	    }
	    dup2(ns, 0);
	    close(s);
	}
#endif	/* defined(DEBUG) */
	openlog("telnetd", LOG_PID | LOG_ODELAY, LOG_DAEMON);
	fromlen = sizeof (from);
	if (getpeername(0, (struct sockaddr *) &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}
	if (setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, (char *) &on, sizeof (on)) < 0) {
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	}
	doit(0, &from);
}

char	*terminaltype = 0;
char	*displayloc = 0;
char	*envinit[3];

void	cleanup();

/*
 * ttloop
 *
 *	A small subroutine to flush the network output buffer, get some data
 * from the network, and pass it through the telnet state machine.  We
 * also flush the pty input buffer (by dropping its data) if it becomes
 * too full.
 */

void
ttloop()
{
    if (nfrontp-nbackp) {
	netflush();
    }
    ncc = read(net, netibuf, sizeof netibuf);
    if (ncc < 0) {
	syslog(LOG_INFO, "ttloop:  read: %m\n");
	exit(1);
    } else if (ncc == 0) {
	syslog(LOG_INFO, "ttloop:  peer died: %m\n");
	exit(1);
    }
    netip = netibuf;
    telrcv();			/* state machine */
    if (ncc > 0) {
	pfrontp = pbackp = ptyobuf;
	telrcv();
    }
}

/*
 * getterminaltype
 *
 *	Ask the other end to send along its terminal type.
 * Output is the variable terminaltype filled in.
 */

void
getterminaltype()
{
#ifdef SOLARIS
    static unsigned char sbuf[] = { IAC, DO, TELOPT_TTYPE };
#else
    static char sbuf[] = { IAC, DO, TELOPT_TTYPE };
#endif

    settimer(getterminal);
    if (hisopts[TELOPT_TTYPE] == OPT_YES_BUT_ALWAYS_LOOK) {

	bcopy(sbuf, nfrontp, sizeof sbuf);

	nfrontp += sizeof sbuf;
	while (sequenceIs(ttypeopt, getterminal)) {
	    ttloop();
	}
    }
    if (hisopts[TELOPT_TTYPE] == OPT_YES) {
#ifdef SOLARIS	
	static unsigned char sbbuf[] = { IAC, SB, TELOPT_TTYPE, TELQUAL_SEND, IAC, SE };
#else	
	static char sbbuf[] = { IAC, SB, TELOPT_TTYPE, TELQUAL_SEND, IAC, SE };
#endif

	bcopy(sbbuf, nfrontp, sizeof sbbuf);
	nfrontp += sizeof sbbuf;
	while (sequenceIs(ttypesubopt, getterminal)) {
	    ttloop();
	}
    }
}

/*
 * getdisplayloc
 *
 *	Ask the other end to send along its display location.
 * Output is the variable displayloc filled in.
 */

void
getdisplayloc()
{
#ifdef SOLARIS
    static unsigned char sbuf[] = { IAC, DO, TELOPT_XDISPLOC };
#else
    static char sbuf[] = { IAC, DO, TELOPT_XDISPLOC };
#endif

    settimer(getdisplay);
    if (hisopts[TELOPT_XDISPLOC] == OPT_YES_BUT_ALWAYS_LOOK) {
	bcopy(sbuf, nfrontp, sizeof sbuf);
	nfrontp += sizeof sbuf;
	while (sequenceIs(dispopt, getdisplay)) {
	    ttloop();
	}
    }
    if (hisopts[TELOPT_XDISPLOC] == OPT_YES) {

#ifdef SOLARIS
	static unsigned char sbbuf[] = { IAC, SB, TELOPT_XDISPLOC, TELQUAL_SEND, IAC, SE };
#else
	static char sbbuf[] = { IAC, SB, TELOPT_XDISPLOC, TELQUAL_SEND, IAC, SE };
#endif

	bcopy(sbbuf, nfrontp, sizeof sbbuf);
	nfrontp += sizeof sbbuf;
	while (sequenceIs(dispsubopt, getdisplay)) {
	    ttloop();
	}
    }
}

/*
 * Get a pty, scan input lines.
 */
doit(f, who)
	int f;
	struct sockaddr_in *who;
{
	char *host, *inet_ntoa();
	int i, p, t;
#ifdef SYSV
	struct termio b;
#else /* not SYSV */
	struct sgttyb b;
#endif /* not SYSV */
	struct hostent *hp;
	int c;

#if 1
	setpgrp();
#ifdef TIOCNOTTY
	t = open("/dev/tty", O_RDWR);
	if (t >= 0) {
		ioctl(t, TIOCNOTTY, 0);
		close(t);
	}
#endif /* TIOCNOTTY */
#endif
#ifdef sgi
	if (line = _getpty(&p, O_RDWR, 0600, 0))
		goto gotpty;
	fatalperror("_getpty");
#else /* not sgi */
#ifdef SVR4
	if ((p = open ("/dev/ptmx", O_RDWR)) >= 0)
		{
		grantpt(p);
		unlockpt(p);
		line = (char *) ptsname(p);
		goto gotpty;
		}
#else /* not SVR4 */
#ifdef HPUX  
/* HPUX fix: HPUX allows many more ptys. Allow loop to search through */
/*           character 'v'                                            */
	for (c = 'p'; c <= 'v'; c++) {
#else
	for (c = 'p'; c <= 's'; c++) {
#endif
		struct stat stb;

		line = "/dev/ptyXX";
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[sizeof("/dev/ptyp") - 1] = "0123456789abcdef"[i];
			p = open(line, O_RDWR);
			if (p > 0)
				goto gotpty;
		}
	}
#endif /* not SVR4 */
#endif /* not sgi */
	fatal(f, "All network ports in use");
	/*NOTREACHED*/
gotpty:
	dup2(f, 0);
#ifndef SVR4
	line[strlen("/dev/")] = 't';
#endif /* not SVR4 */
#if 0
#ifdef TIOCNOTTY
	t = open("/dev/tty", O_RDWR);
	if (t >= 0) {
		ioctl(t, TIOCNOTTY, 0);
		close(t);
	}
#endif /* TIOCNOTTY */
#endif
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
#ifdef SVR4
	if (ioctl(t, I_PUSH, "ptem") < 0)
		fatalperror(f, line);
	if (ioctl(t, I_PUSH, "ldterm") < 0)
		fatalperror(f, line);
	if (ioctl(t, I_PUSH, "ttcompat") < 0)
		fatalperror(f, line);
#endif /* SVR4 */
	if (fchmod(t, 0))
		fatalperror(f, line);
#ifndef SOLARIS
#ifndef _AIX
	(void)signal(SIGHUP, SIG_IGN);
	vhangup();
	(void)signal(SIGHUP, SIG_DFL);
#endif
#endif
#if 1
	close(open("/dev/tty",O_RDWR));
#endif
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
#ifdef SYSV
	if (ioctl(t, TCGETA, &b) < 0)
		fatalperror(f, line);
	b.c_iflag = ICRNL | IGNPAR | BRKINT;
	b.c_oflag = ONLCR | OPOST;
	b.c_lflag &= ~ECHO;
	if (ioctl(t, TCSETA, &b) < 0)
		fatalperror(f, line);
#else /* not SYSV */
	if (ioctl(t, TIOCGETP, &b) < 0)
		fatalperror(f, line);
	b.sg_flags = CRMOD|XTABS|ANYP;
	if (ioctl(t, TIOCSETP, &b) < 0)
		fatalperror(f, line);
	if (ioctl(p, TIOCGETP, &b) < 0)
		fatalperror(f, line);
	b.sg_flags &= ~ECHO;
	if (ioctl(p, TIOCSETP, &b) < 0)
		fatalperror(f, line);
#endif /* not SYSV */
#ifdef SOLARIS
	hp = gethostbyaddr((char *) &(who->sin_addr.s_addr), 
		sizeof (struct in_addr),
		(int) who->sin_family);
#else
	hp = gethostbyaddr(&who->sin_addr, sizeof (struct in_addr),
		who->sin_family);
#endif
	if (hp)
		host = hp->h_name;
	else
		host = inet_ntoa(who->sin_addr);

	net = f;
	pty = p;

	/*
	 * get terminal type and X window display name.
	 */
	hisopts[TELOPT_TTYPE] = OPT_YES_BUT_ALWAYS_LOOK;
	hisopts[TELOPT_XDISPLOC] = OPT_YES_BUT_ALWAYS_LOOK;
	getterminaltype();
	getdisplayloc();

	if ((i = fork()) < 0)
		fatalperror(f, "fork");
	if (i)
	{
		glbl_child = i;
		telnet(f, p);
	}
	close(f);
	close(p);
	dup2(t, 0);
	dup2(t, 1);
	dup2(t, 2);
	close(t);
	i = 0;
	if (terminaltype)
		envinit[i++] = terminaltype;
	if (displayloc)
		envinit[i++] = displayloc;
	else if (host)	
		{
		/*	Best guess of display...  */
		envinit[i] = (char *)malloc(strlen(host) + 11);
		strcpy(envinit[i], "DISPLAY=");
		strcat(envinit[i], host);
		strcat(envinit[i], ":0");
		i++;
		}
	envinit[i] = 0;
	environ = envinit;
	/*
	 *	Exec the application -- this should never return.
	 */
	do_exec(Program_name, Config_name, environ, host);
	syslog(LOG_ERR, "execl: %m\n");
	fatalperror(2, "execl");
	/*NOTREACHED*/
}

fatal(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];

	(void) sprintf(buf, "telnetd: %s.\r\n", msg);
	(void) write(f, buf, strlen(buf));
	exit(1);
}

fatalperror(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];
	extern char *sys_errlist[];

	(void) sprintf(buf, "%s: %s\r\n", msg, sys_errlist[errno]);
	fatal(f, buf);
}


/*
 * Check a descriptor to see if out of band data exists on it.
 */


stilloob(s)
int	s;		/* socket number */
{
    static struct timeval timeout = { 0 };
    fd_set	excepts;
    int value;

    do {
	FD_ZERO(&excepts);
	FD_SET(s, &excepts);
	value = select(s+1, (fd_set *)0, (fd_set *)0, &excepts, &timeout);
    } while ((value == -1) && (errno == EINTR));

    if (value < 0) {
	fatalperror(pty, "select");
    }
    if (FD_ISSET(s, &excepts)) {
	return 1;
    } else {
	return 0;
    }
}

/*
 * Main loop.  Select from pty and network, and
 * hand data to telnet receiver finite state machine.
 */
telnet(f, p)
{
	int on = 1;
	char hostname[MAXHOSTNAMELEN];
#define	TABBUFSIZ	512
	char	defent[TABBUFSIZ];
	char	defstrs[TABBUFSIZ];
#undef	TABBUFSIZ
	char *HE;
	char *HN;
	char *IM;

	ioctl(f, FIONBIO, &on);
	ioctl(p, FIONBIO, &on);
#ifndef SVR4
	ioctl(p, TIOCPKT, &on);
#endif
#if	defined(SO_OOBINLINE)
	setsockopt(net, SOL_SOCKET, SO_OOBINLINE, (char *) &on, sizeof on);
#endif	/* defined(SO_OOBINLINE) */

	signal(SIGTSTP, SIG_IGN);
	/*
	 * Ignoring SIGTTOU keeps the kernel from blocking us
	 * in ttioctl() in /sys/tty.c.
	 */

#ifdef SOLARIS
	sigignore(SIGTTOU);
#else
	signal(SIGTTOU, SIG_IGN);
#endif

	(void) signal(SIGCHLD, cleanup);
#if 0
	setpgrp();
#endif

	/*
	 * Request to do remote echo and to suppress go ahead.
	 */
	if (!myopts[TELOPT_ECHO]) {
	    dooption(TELOPT_ECHO);
	}
	if (!myopts[TELOPT_SGA]) {
	    dooption(TELOPT_SGA);
	}
	/*
	 * Is the client side a 4.2 (NOT 4.3) system?  We need to know this
	 * because 4.2 clients are unable to deal with TCP urgent data.
	 *
	 * To find out, we send out a "DO ECHO".  If the remote system
	 * answers "WILL ECHO" it is probably a 4.2 client, and we note
	 * that fact ("WILL ECHO" ==> that the client will echo what
	 * WE, the server, sends it; it does NOT mean that the client will
	 * echo the terminal input).
	 */
	(void) sprintf(nfrontp, (char *) doopt, TELOPT_ECHO);
	nfrontp += sizeof doopt-2;
	hisopts[TELOPT_ECHO] = OPT_YES_BUT_ALWAYS_LOOK;

	/*
	 * Show banner that getty never gave.
	 *
	 * We put the banner in the pty input buffer.  This way, it
	 * gets carriage return null processing, etc., just like all
	 * other pty --> client data.
	 */

	gethostname(hostname, sizeof (hostname));
/*
	if (getent(defent, "default") == 1) {
		char *getstr();
		char *p=defstrs;

		HE = getstr("he", &p);
		HN = getstr("hn", &p);
		IM = getstr("im", &p);
		if (HN && *HN)
			strcpy(hostname, HN);
		edithost(HE, hostname);
		if (IM && *IM)
			putf(IM, ptyibuf+1);
	} else {
*/
		sprintf(ptyibuf+1, BANNER, hostname);
/*
	}
*/

#ifdef SVR4
	ptyip = ptyibuf;		/* Prime the pump */
#else
	ptyip = ptyibuf+1;		/* Prime the pump */
#endif


	pcc = strlen(ptyip);		/* ditto */

#ifndef SVR4
	/* Clear ptybuf[0] - where the packet information is received */
	ptyibuf[0] = 0;
#endif

	/*
	 * Call telrcv() once to pick up anything received during
	 * terminal type negotiation.
	 */
	telrcv();

	for (;;) {
		fd_set ibits, obits, xbits;
		register int c;

		if (ncc < 0 && pcc < 0)
			break;

		FD_ZERO(&ibits);
		FD_ZERO(&obits);
		FD_ZERO(&xbits);
		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (nfrontp - nbackp || pcc > 0) {
			FD_SET(f, &obits);
			FD_SET(p, &xbits);
		} else {
			FD_SET(p, &ibits);
		}
		if (pfrontp - pbackp || ncc > 0) {
			FD_SET(p, &obits);
		} else {
			FD_SET(f, &ibits);
		}
		if (!SYNCHing) {
			FD_SET(f, &xbits);
		}
		if ((c = select(16, &ibits, &obits, &xbits,
						(struct timeval *)0)) < 1) {
			if (c == -1) {
				if (errno == EINTR) {
					continue;
				}
			}
			sleep(5);
			continue;
		}

		/*
		 * Any urgent data?
		 */
		if (FD_ISSET(net, &xbits)) {
		    SYNCHing = 1;
		}

		/*
		 * Something to read from the network...
		 */
		if (FD_ISSET(net, &ibits)) {
#if	!defined(SO_OOBINLINE)
			/*
			 * In 4.2 (and 4.3 beta) systems, the
			 * OOB indication and data handling in the kernel
			 * is such that if two separate TCP Urgent requests
			 * come in, one byte of TCP data will be overlaid.
			 * This is fatal for Telnet, but we try to live
			 * with it.
			 *
			 * In addition, in 4.2 (and...), a special protocol
			 * is needed to pick up the TCP Urgent data in
			 * the correct sequence.
			 *
			 * What we do is:  if we think we are in urgent
			 * mode, we look to see if we are "at the mark".
			 * If we are, we do an OOB receive.  If we run
			 * this twice, we will do the OOB receive twice,
			 * but the second will fail, since the second
			 * time we were "at the mark", but there wasn't
			 * any data there (the kernel doesn't reset
			 * "at the mark" until we do a normal read).
			 * Once we've read the OOB data, we go ahead
			 * and do normal reads.
			 *
			 * There is also another problem, which is that
			 * since the OOB byte we read doesn't put us
			 * out of OOB state, and since that byte is most
			 * likely the TELNET DM (data mark), we would
			 * stay in the TELNET SYNCH (SYNCHing) state.
			 * So, clocks to the rescue.  If we've "just"
			 * received a DM, then we test for the
			 * presence of OOB data when the receive OOB
			 * fails (and AFTER we did the normal mode read
			 * to clear "at the mark").
			 */
		    if (SYNCHing) {
			int atmark;

			ioctl(net, SIOCATMARK, (char *)&atmark);
			if (atmark) {
			    ncc = recv(net, netibuf, sizeof (netibuf), MSG_OOB);
			    if ((ncc == -1) && (errno == EINVAL)) {
				ncc = read(net, netibuf, sizeof (netibuf));
				if (sequenceIs(didnetreceive, gotDM)) {
				    SYNCHing = stilloob(net);
				}
			    }
			} else {
			    ncc = read(net, netibuf, sizeof (netibuf));
			}
		    } else {
			ncc = read(net, netibuf, sizeof (netibuf));
		    }
		    settimer(didnetreceive);
#else	/* !defined(SO_OOBINLINE)) */
		    ncc = read(net, netibuf, sizeof (netibuf));
#endif	/* !defined(SO_OOBINLINE)) */
		    if (ncc < 0 && errno == EWOULDBLOCK)
			ncc = 0;
		    else {
			if (ncc <= 0) {
			    break;
			}
			netip = netibuf;
		    }
		}

		/*
		 * Something to read from the pty...
		 */
		if (FD_ISSET(p, &xbits)) {
			if (read(p, ptyibuf, 1) != 1) {
				break;
			}
		}
		if (FD_ISSET(p, &ibits)) {
			pcc = read(p, ptyibuf, BUFSIZ);
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else {
				if (pcc <= 0)
					break;
#ifdef SVR4
				ptyip = ptyibuf;
#else
				/* Skip past "packet" */
				pcc--;
				ptyip = ptyibuf+1;
#endif
			}
		}

#ifndef SVR4
		if (ptyibuf[0] & TIOCPKT_FLUSHWRITE) {
			netclear();	/* clear buffer back */
			*nfrontp++ = IAC;
			*nfrontp++ = DM;
			neturg = nfrontp-1;  /* off by one XXX */
			ptyibuf[0] = 0;
		}
#endif

		while (pcc > 0) {
			if ((&netobuf[BUFSIZ] - nfrontp) < 2)
				break;
			c = *ptyip++ & 0377, pcc--;
			if (c == IAC)
				*nfrontp++ = c;
			*nfrontp++ = c;
			/* Don't do CR-NUL if we are in binary mode */
			if ((c == '\r') && (myopts[TELOPT_BINARY] == OPT_NO)) {
				if (pcc > 0 && ((*ptyip & 0377) == '\n')) {
					*nfrontp++ = *ptyip++ & 0377;
					pcc--;
				} else
					*nfrontp++ = '\0';
			}
		}
		if (FD_ISSET(f, &obits) && (nfrontp - nbackp) > 0)
			netflush();
		if (ncc > 0)
			telrcv();
		if (FD_ISSET(p, &obits) && (pfrontp - pbackp) > 0)
			ptyflush();
	}
	cleanup();
}
	
/*
 * State for recv fsm
 */
#define	TS_DATA		0	/* base state */
#define	TS_IAC		1	/* look for double IAC's */
#define	TS_CR		2	/* CR-LF ->'s CR */
#define	TS_SB		3	/* throw away begin's... */
#define	TS_SE		4	/* ...end's (suboption negotiation) */
#define	TS_WILL		5	/* will option negotiation */
#define	TS_WONT		6	/* wont " */
#define	TS_DO		7	/* do " */
#define	TS_DONT		8	/* dont " */

telrcv()
{
	register int c;
	static int state = TS_DATA;
	char buf[64];

	while (ncc > 0) {
		if ((&ptyobuf[BUFSIZ] - pfrontp) < 2)
			return(0);
		c = *netip++ & 0377, ncc--;

		/*
		** Check for CTRL-C
		*/

		if ((c == 0x003) && (state == 0))
		{
		   /*
		   **  Here we merely send SIGHUP to the access script
		   **  since CTRL-C was pressed.
		   */

#if 1
		   sprintf(buf, "kill -1 %d",glbl_child);
		   system(buf);
#endif
		   
        }

		switch (state) {

		case TS_CR:
			state = TS_DATA;
			/* Strip off \n or \0 after a \r */
			if ((c == 0) || (c == '\n')) {
				break;
			}
			/* FALL THROUGH */

		case TS_DATA:
			if (c == IAC) {
				state = TS_IAC;
				break;
			}
			if (inter > 0)
				break;
			/*
			 * We now map \r\n ==> \r for pragmatic reasons.
			 * Many client implementations send \r\n when
			 * the user hits the CarriageReturn key.
			 *
			 * We USED to map \r\n ==> \n, since \r\n says
			 * that we want to be in column 1 of the next
			 * printable line, and \n is the standard
			 * unix way of saying that (\r is only good
			 * if CRMOD is set, which it normally is).
			 */
			if ((c == '\r') && (hisopts[TELOPT_BINARY] == OPT_NO)) {
				state = TS_CR;
			}
			*pfrontp++ = c;
			break;

		case TS_IAC:

			switch (c) {

			/*
			 * Send the process on the pty side an
			 * interrupt.  Do this with a NULL or
			 * interrupt char; depending on the tty mode.
			 */
			case IP:
				interrupt();
				break;

			case BREAK:
				sendbrk();
				break;

			/*
			 * Are You There?
			 */
			case AYT:
				strcpy(nfrontp, "\r\n[Yes]\r\n");
				nfrontp += 9;
				break;

			/*
			 * Abort Output
			 */
			case AO: {
#ifdef SYSV
					struct termio b;
#else /* not SYSV */
					struct ltchars tmpltc;
#endif /* not SYSV */

					ptyflush();	/* half-hearted */
#ifdef SYSV
		/* seems that everyone has their own way of doing this... */
#if defined(VDISCRD)
					ioctl(pty, TCGETA, &b);
					if (b.c_cc[VDISCRD] != '\377') {
						*pfrontp++ = b.c_cc[VDISCRD];
					}
#endif /* VDISCRD */
#if defined(VFLUSHO)
					ioctl(pty, TCGETA, &b);
					if (b.c_cc[VFLUSHO] != '\377') {
						*pfrontp++ = b.c_cc[VFLUSHO];
					}
#endif /* VFLUSHO */
#else /* not SYSV */
					ioctl(pty, TIOCGLTC, &tmpltc);
					if (tmpltc.t_flushc != '\377') {
						*pfrontp++ = tmpltc.t_flushc;
					}
#endif /* not SYSV */
					netclear();	/* clear buffer back */
					*nfrontp++ = IAC;
					*nfrontp++ = DM;
					neturg = nfrontp-1; /* off by one XXX */
					break;
				}

			/*
			 * Erase Character and
			 * Erase Line
			 */
			case EC:
			case EL: {
#ifdef SYSV
					struct termio b;
#else /* not SYSV */
					struct sgttyb b;
#endif /* not SYSV */
					char ch;

					ptyflush();	/* half-hearted */
#ifdef SYSV
					ioctl(pty, TCGETA, &b);
					ch = (c == EC) ?
						b.c_cc[VERASE] : b.c_cc[VKILL];
#else /* not SYSV */
					ioctl(pty, TIOCGETP, &b);
					ch = (c == EC) ?
						b.sg_erase : b.sg_kill;
#endif /* not SYSV */
					if (ch != '\377') {
						*pfrontp++ = ch;
					}
					break;
				}

			/*
			 * Check for urgent data...
			 */
			case DM:
				SYNCHing = stilloob(net);
				settimer(gotDM);
				break;


			/*
			 * Begin option subnegotiation...
			 */
			case SB:
				state = TS_SB;
				continue;

			case WILL:
				state = TS_WILL;
				continue;

			case WONT:
				state = TS_WONT;
				continue;

			case DO:
				state = TS_DO;
				continue;

			case DONT:
				state = TS_DONT;
				continue;

			case IAC:
				*pfrontp++ = c;
				break;
			}
			state = TS_DATA;
			break;

		case TS_SB:
			if (c == IAC) {
				state = TS_SE;
			} else {
				SB_ACCUM(c);
			}
			break;

		case TS_SE:
			if (c != SE) {
				if (c != IAC) {
					SB_ACCUM(IAC);
				}
				SB_ACCUM(c);
				state = TS_SB;
			} else {
				SB_TERM();
				suboption();	/* handle sub-option */
				state = TS_DATA;
				SB_CLEAR();
			}
			break;

		case TS_WILL:
			if (hisopts[c] != OPT_YES)
				willoption(c);
			state = TS_DATA;
			continue;

		case TS_WONT:
			if (hisopts[c] != OPT_NO)
				wontoption(c);
			state = TS_DATA;
			continue;

		case TS_DO:
			if (myopts[c] != OPT_YES)
				dooption(c);
			state = TS_DATA;
			continue;

		case TS_DONT:
			if (myopts[c] != OPT_NO) {
				dontoption(c);
			}
			state = TS_DATA;
			continue;

		default:
			syslog(LOG_ERR, "telnetd: panic state=%d\n", state);
			printf("telnetd: panic state=%d\n", state);
			exit(1);
		}
	}
}

willoption(option)
	int option;
{
#ifdef SOLARIS
	unsigned char *fmt;
#else
	char *fmt;
#endif

	switch (option) {

	case TELOPT_BINARY:
#ifdef SYSV
		rawmode(1);
#else /* not SYSV */
		mode(RAW, 0);
#endif /* not SYSV */
		fmt = doopt;
		break;

	case TELOPT_ECHO:
		not42 = 0;		/* looks like a 4.2 system */
		/*
		 * Now, in a 4.2 system, to break them out of ECHOing
		 * (to the terminal) mode, we need to send a "WILL ECHO".
		 * Kludge upon kludge!
		 */
		if (myopts[TELOPT_ECHO] == OPT_YES) {
		    dooption(TELOPT_ECHO);
		}
		fmt = dont;
		break;

	case TELOPT_TTYPE:
		settimer(ttypeopt);
		if (hisopts[TELOPT_TTYPE] == OPT_YES_BUT_ALWAYS_LOOK) {
		    hisopts[TELOPT_TTYPE] = OPT_YES;
		    return(0);
		}
		fmt = doopt;
		break;

	case TELOPT_XDISPLOC:
		settimer(dispopt);
		if (hisopts[TELOPT_XDISPLOC] == OPT_YES_BUT_ALWAYS_LOOK) {
		    hisopts[TELOPT_XDISPLOC] = OPT_YES;
		    return(0);
		}
		fmt = doopt;
		break;

	case TELOPT_SGA:
		fmt = doopt;
		break;

	case TELOPT_TM:
		fmt = dont;
		break;

	default:
		fmt = dont;
		break;
	}
	if (fmt == doopt) {
		hisopts[option] = OPT_YES;
	} else {
		hisopts[option] = OPT_NO;
	}
	(void) sprintf(nfrontp, (char *) fmt, option);
	nfrontp += sizeof (dont) - 2;
}

wontoption(option)
	int option;
{
#ifdef SOLARIS
	unsigned char *fmt;
#else
	char *fmt;
#endif

	switch (option) {
	case TELOPT_ECHO:
		not42 = 1;		/* doesn't seem to be a 4.2 system */
		break;

	case TELOPT_BINARY:
#ifdef SYSV
		rawmode(0);
#else /* not SYSV */
		mode(0, RAW);
#endif /* not SYSV */
		break;

	case TELOPT_TTYPE:
	    settimer(ttypeopt);
	    break;

	case TELOPT_XDISPLOC:
	    settimer(dispopt);
	    break;
	}

	fmt = dont;

	hisopts[option] = OPT_NO;
	(void) sprintf(nfrontp, (char *) fmt, option);
	nfrontp += sizeof (doopt) - 2;
}

dooption(option)
	int option;
{
#ifdef SOLARIS
	unsigned char *fmt;
#else
	char *fmt;
#endif

	switch (option) {

	case TELOPT_TM:
		fmt = wont;
		break;

	case TELOPT_ECHO:
#ifdef SYSV
		echomode(1);
#else /* not SYSV */
		mode(ECHO|CRMOD, 0);
#endif /* not SYSV */
		fmt = will;
		break;

	case TELOPT_BINARY:
#ifdef SYSV
		rawmode(1);
#else /* not SYSV */
		mode(RAW, 0);
#endif /* not SYSV */
		fmt = will;
		break;

	case TELOPT_SGA:
		fmt = will;
		break;

	default:
		fmt = wont;
		break;
	}
	if (fmt == will) {
	    myopts[option] = OPT_YES;
	} else {
	    myopts[option] = OPT_NO;
	}
	(void) sprintf(nfrontp, (char *) fmt, option);
	nfrontp += sizeof (doopt) - 2;
}


dontoption(option)
int option;
{
#ifdef SOLARIS
    unsigned char *fmt;
#else
    char *fmt;
#endif
    switch (option) {
    case TELOPT_ECHO:		/* we should stop echoing */
#ifdef SYSV
	echomode(0);
#else /* not SYSV */
	mode(0, ECHO);
#endif /* not SYSV */
	fmt = wont;
	break;

    default:
	fmt = wont;
	break;
    }

    if (fmt = wont) {
	myopts[option] = OPT_NO;
    } else {
	myopts[option] = OPT_YES;
    }
    (void) sprintf(nfrontp, (char *) fmt, option);
    nfrontp += sizeof (wont) - 2;
}

/*
 * suboption()
 *
 *	Look at the sub-option buffer, and try to be helpful to the other
 * side.
 *
 *	Currently we recognize:
 *
 *	Terminal type is
 */

suboption()
{
    switch (SB_GET()) {
    case TELOPT_TTYPE: {		/* Yaaaay! */
	static char terminalname[5+41] = "TERM=";

	settimer(ttypesubopt);

	if (SB_GET() != TELQUAL_IS) {
	    return(0);		/* ??? XXX but, this is the most robust */
	}

	terminaltype = terminalname+strlen(terminalname);

	while ((terminaltype < (terminalname + sizeof terminalname-1)) &&
								    !SB_EOF()) {
	    register int c;

	    c = SB_GET();
	    if (isupper(c)) {
		c = tolower(c);
	    }
	    *terminaltype++ = c;    /* accumulate name */
	}
	*terminaltype = 0;
	terminaltype = terminalname;
	break;
    }

    case TELOPT_XDISPLOC: {
	static char displayname[8+41] = "DISPLAY=";

	settimer(dispsubopt);

	if (SB_GET() != TELQUAL_IS) {
	    return(0);		/* ??? XXX but, this is the most robust */
	}

	displayloc = displayname+strlen(displayname);

	while ((displayloc < (displayname + sizeof displayname-1)) &&
								    !SB_EOF()) {
	    register int c;

	    c = SB_GET();
	    if (isupper(c)) {
		c = tolower(c);
	    }
	    *displayloc++ = c;    /* accumulate name */
	}
	*displayloc = 0;
	displayloc = displayname;
	break;
    }

    default:
	;
    }
}

#ifdef SYSV
rawmode(onoff)
	int onoff;
{
	struct termio b;

	ptyflush();
	ioctl(pty, TCGETA, &b);
	if (onoff)
		b.c_lflag &= ~ICANON;
	else
		b.c_lflag |= ICANON;
	ioctl(pty, TCSETA, &b);
}
echomode(onoff)
	int onoff;
{
	struct termio b;

	ptyflush();
	ioctl(pty, TCGETA, &b);
	if (onoff)
		{
		b.c_lflag |= ECHO;
		b.c_iflag |= ICRNL;
		}
	else
		{
		b.c_lflag &= ~ECHO;
		b.c_iflag &= ~ICRNL;
		}
	ioctl(pty, TCSETA, &b);
}
#else /* not SYSV */
mode(on, off)
	int on, off;
{
	struct sgttyb b;

	ptyflush();
	ioctl(pty, TIOCGETP, &b);
	b.sg_flags |= on;
	b.sg_flags &= ~off;
	ioctl(pty, TIOCSETP, &b);
}
#endif /* not SYSV */

/*
 * Send interrupt to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write intr char.
 */
interrupt()
{
#ifdef SYSV
	struct termio b;
#else /* not SYSV */
	struct sgttyb b;
	struct tchars tchars;
#endif /* not SYSV */

	ptyflush();	/* half-hearted */
#ifdef SYSV

	ioctl(pty, TCGETA, &b);
	if (!(b.c_lflag & ICANON)) {
		*pfrontp++ = '\0';
		return(0);
	}
	*pfrontp++ = b.c_cc[VINTR];
#else /* not SYSV */
	ioctl(pty, TIOCGETP, &b);
	if (b.sg_flags & RAW) {
		*pfrontp++ = '\0';
		return(0);
	}
	*pfrontp++ = ioctl(pty, TIOCGETC, &tchars) < 0 ?
		'\177' : tchars.t_intrc;
#endif /* not SYSV */
}

/*
 * Send quit to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write quit char.
 */
sendbrk()
{
#ifdef SYSV
	struct termio b;
#else /* not SYSV */
	struct sgttyb b;
	struct tchars tchars;
#endif /* not SYSV */

	ptyflush();	/* half-hearted */
#ifdef SYSV
	ioctl(pty, TCGETA, &b);
	if (!(b.c_lflag & ICANON)) {
		*pfrontp++ = '\0';
		return(0);
	}
	*pfrontp++ = b.c_cc[VQUIT];
#else /* not SYSV */
	ioctl(pty, TIOCGETP, &b);
	if (b.sg_flags & RAW) {
		*pfrontp++ = '\0';
		return(0);
	}
	*pfrontp++ = ioctl(pty, TIOCGETC, &tchars) < 0 ?
		'\034' : tchars.t_quitc;
#endif /* not SYSV */
}

ptyflush()
{
	int n;

	if ((n = pfrontp - pbackp) > 0)
		n = write(pty, pbackp, n);
	if (n < 0)
		return(0);
	pbackp += n;
	if (pbackp == pfrontp)
		pbackp = pfrontp = ptyobuf;
}

/*
 * nextitem()
 *
 *	Return the address of the next "item" in the TELNET data
 * stream.  This will be the address of the next character if
 * the current address is a user data character, or it will
 * be the address of the character following the TELNET command
 * if the current address is a TELNET IAC ("I Am a Command")
 * character.
 */

char *
nextitem(current)
char	*current;
{
    if ((*current&0xff) != IAC) {
	return current+1;
    }
    switch (*(current+1)&0xff) {
    case DO:
    case DONT:
    case WILL:
    case WONT:
	return current+3;
    case SB:		/* loop forever looking for the SE */
	{
	    register char *look = current+2;

	    for (;;) {
		if ((*look++&0xff) == IAC) {
		    if ((*look++&0xff) == SE) {
			return look;
		    }
		}
	    }
	}
    default:
	return current+2;
    }
}


/*
 * netclear()
 *
 *	We are about to do a TELNET SYNCH operation.  Clear
 * the path to the network.
 *
 *	Things are a bit tricky since we may have sent the first
 * byte or so of a previous TELNET command into the network.
 * So, we have to scan the network buffer from the beginning
 * until we are up to where we want to be.
 *
 *	A side effect of what we do, just to keep things
 * simple, is to clear the urgent data pointer.  The principal
 * caller should be setting the urgent data pointer AFTER calling
 * us in any case.
 */

netclear()
{
    register char *thisitem, *next;
    char *good;
#define	wewant(p)	((nfrontp > p) && ((*p&0xff) == IAC) && \
				((*(p+1)&0xff) != EC) && ((*(p+1)&0xff) != EL))

    thisitem = netobuf;

    while ((next = nextitem(thisitem)) <= nbackp) {
	thisitem = next;
    }

    /* Now, thisitem is first before/at boundary. */

    good = netobuf;	/* where the good bytes go */

    while (nfrontp > thisitem) {
	if (wewant(thisitem)) {
	    int length;

	    next = thisitem;
	    do {
		next = nextitem(next);
	    } while (wewant(next) && (nfrontp > next));
	    length = next-thisitem;
	    bcopy(thisitem, good, length);
	    good += length;
	    thisitem = next;
	} else {
	    thisitem = nextitem(thisitem);
	}
    }

    nbackp = netobuf;
    nfrontp = good;		/* next byte to be sent */
    neturg = 0;
}

/*
 *  netflush
 *		Send as much data as possible to the network,
 *	handling requests for urgent data.
 */


netflush()
{
    int n;

    if ((n = nfrontp - nbackp) > 0) {
	/*
	 * if no urgent data, or if the other side appears to be an
	 * old 4.2 client (and thus unable to survive TCP urgent data),
	 * write the entire buffer in non-OOB mode.
	 */
	if ((neturg == 0) || (not42 == 0)) {
	    n = write(net, nbackp, n);	/* normal write */
	} else {
	    n = neturg - nbackp;
	    /*
	     * In 4.2 (and 4.3) systems, there is some question about
	     * what byte in a sendOOB operation is the "OOB" data.
	     * To make ourselves compatible, we only send ONE byte
	     * out of band, the one WE THINK should be OOB (though
	     * we really have more the TCP philosophy of urgent data
	     * rather than the Unix philosophy of OOB data).
	     */
	    if (n > 1) {
		n = send(net, nbackp, n-1, 0);	/* send URGENT all by itself */
	    } else {
		n = send(net, nbackp, n, MSG_OOB);	/* URGENT data */
	    }
	}
    }
    if (n < 0) {
	if (errno == EWOULDBLOCK)
	    return(0);
	/* should blow this guy away... */
	return(0);
    }
    nbackp += n;
    if (nbackp >= neturg) {
	neturg = 0;
    }
    if (nbackp == nfrontp) {
	nbackp = nfrontp = netobuf;
    }
}


void cleanup()
{
	char *p;

	p = line + sizeof("/dev/") - 1;
	logout(p);
/*
	if (logout(p))
		logwtmp(p, "", "");
*/
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
	*p = 'p';
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
	shutdown(net, 2);
	exit(1);

}
/*
 *	logout removes the utmp entry.
 */
logout(devname)
	char	*devname;		/* device name */
	{
	struct utmp	utmp;		/* buffer for utmp file */
	int		utmp_fd;	/* file desc for utmp file */

	utmp_fd = open(UTMP_FILE, O_RDWR);
	if (utmp_fd < 0)
		return(0);
	while (read(utmp_fd, &utmp, sizeof(utmp)) > 0)
		if (strncmp(utmp.ut_line, devname, sizeof(utmp.ut_line)) == 0)
			{
			strcpy(utmp.ut_name, "");
#ifndef sgi 
#ifndef SOLARIS
			strcpy(utmp.ut_host, "");
#endif
#endif
			time(&utmp.ut_time);
			lseek(utmp_fd, -sizeof(utmp), L_INCR);
			write(utmp_fd, &utmp, sizeof(utmp));
			break;
			}
	}

char	editedhost[32];

edithost(pat, host)
	register char *pat;
	register char *host;
{
	register char *res = editedhost;

	if (!pat)
		pat = "";
	while (*pat) {
		switch (*pat) {

		case '#':
			if (*host)
				host++;
			break;

		case '@':
			if (*host)
				*res++ = *host++;
			break;

		default:
			*res++ = *pat;
			break;

		}
		if (res == &editedhost[sizeof editedhost - 1]) {
			*res = '\0';
			return(0);
		}
		pat++;
	}
	if (*host)
		strncpy(res, host, sizeof editedhost - (res - editedhost) - 1);
	else
		*res = '\0';
	editedhost[sizeof editedhost - 1] = '\0';
}

static char *putlocation;

#ifdef SOLARIS
void put(char *s)
#else
put(s)
register char *s;
#endif
{

	while (*s)
		putchr(*s++);
}

putchr(cc)
{
	*putlocation++ = cc;
}

putf(cp, where)
register char *cp;
char *where;
{
	char *slash;
	char datebuffer[60];
#ifndef SOLARIS
	extern char *rindex();
#endif

	putlocation = where;

	while (*cp) {
		if (*cp != '%') {
			putchr(*cp++);
			continue;
		}
		switch (*++cp) {

		case 't':
#ifdef SOLARIS
			slash = (char *) strchr((char *) line, (int) '/');
#else
			slash = rindex(line, '/');
#endif

			if (slash == (char *) 0)
				puts(line);
			else
				puts(&slash[1]);
			break;

		case 'h':
			puts(editedhost);
			break;

/*
		case 'd':
			get_date(datebuffer);
			puts(datebuffer);
			break;
*/

		case '%':
			putchr('%');
			break;
		}
		cp++;
	}
}
