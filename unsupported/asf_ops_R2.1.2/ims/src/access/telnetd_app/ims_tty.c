/*
 *	ims_tty.c -- getty replacement for the IMS project.  Sets the
 *		     terminal up and exec's the real IMS program.
 *
 *	usage:
 *		ims_tty [psname] [-w whoname] [-u username] [-s speed] [-d dev]
 *			[-h host] [-p program] [-t type]
 *
 *	where
 *		psname		is the name to show up in a ps listing
 *		whoname		is the name to show up in a who listing
 *		username	is the name of a user in /etc/passwd.
 *				imstty will set it's UID and chdir
 *				to the home directory.
 *		speed		for a serial line, the line speed (e.g. 9600)
 *		dev		name of the device in /dev (e.g. ttya)
 *				if not given, uses file desc. 0
 *		host		name of remote host if remote login.
 *		program		is the name of the IMS program to be exec'ed.
 *				The default is "/usr/local/ims/ims".
 *		type		is the type of port, "dialin" for a modem line
 *				and "local" for a local (HYPERbus) terminal.
 *				The default is "local".
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <time.h>
#include <pwd.h>
#include <signal.h>
#include <utmp.h>
#ifndef UTMP_FILE
#define UTMP_FILE "/etc/utmp"	/* name of the utmp file */
#endif

#ifdef gould
#include <sgtty.h>
#else
#ifdef _AIX
#include <termios.h>
#include <sys/ioctl.h>
#else	/* not _AIX */
#include <sys/termio.h>
#endif	/* _AIX */
#endif	/* gould */

#include <termio.h>
#include <termios.h>

#if defined(_AIX) || defined(sgi) || defined(hpux)
#define SYSV
#endif

/*
** Local Functions.
*/
static void	setenv (char *, char *);
static int ioc_spd (int);
static void stuffenv (char **, char *, char *);

char	*Dev = "/dev/";		/* the special file directory */
int	Child;			/* process ID of child process */
char	*Program_name;		/* pointer to argv[0] */
#ifdef _AIX
extern void     perror(const char *s);
#endif	/* _AIX */

/*
** main ()
*/
main(argc,argv)
	int argc;
	char **argv;
	{
	int	stdin_fd, stdout_fd;	/* file descriptors */
	struct passwd *pwd;	/* password file entry */
	extern char **environ;	/* environment */
	char	**env;		/* environment to be built */
	register int i;		/* loop index */
	char *argv0;
	char buf[255];
	struct passwd *getpwnam();
#ifdef gould
	struct	sgttyb Line_attrib;	/* tty attributes */
#else
#ifdef _AIX
	struct	termios Line_attrib;	/* tty attributes */
#else	/* not _AIX */
	struct	termio Line_attrib;	/* tty attributes */
#endif	/* _AIX */
#endif
#ifdef TIOCGWINSZ
	struct winsize winsize;		/* to set window size */
#endif /* TIOCGWINSZ */
	int	speed = B9600;		/* default line speed for async tty */
	char	*whoname = "V0_IMS";	/* default name of user for who lists */
	char	*username = "imsuser";	/* default name of user to setuid to */
	char	*remhost = "";		/* remote host name */
	char	*devname = (char *)0;	/* name of device to open */
	char	*imspgm =		/* default IMS program to exec */
		"/usr/local/ims/ims";
	char	*port_type = (char *)0;	/* default type of async port */

	void	killchild();

	/*
	 *	Parse arguments.  Note that we ignore anything we
	 *	don't understand.
	 */

	signal(SIGINT, killchild);
	Program_name = argv[0];
	for (argc--,argv++; argc > 0; argc--,argv++)
		{
		if (**argv == '-')
			{
			switch(*++*argv)
				{
				case 'w': /*	Name for "who" listing. */
					whoname = argv[1];
					break;
				case 'u': /*	User name.  */
					username = argv[1];
					break;
				case 's': /*	Line speed.  */
					speed = ioc_spd(atoi(argv[1]));
					break;
				case 'd': /*	Device name */
					devname = argv[1];
					break;
				case 'h': /*	Remote host name */
					remhost = argv[1];
					break;
				case 'p': /*	IMS program to exec */
					imspgm = argv[1];
					break;
				case 't': /*	type of login port */
					port_type = argv[1];
					break;
				}
			/*
			 *	Skip the value after the flag.
			 */
			argc--;
			argv++;
			}
		}
	/*
	 *	If a device name was given, handle it first.
	 */
	if (devname)
		{
		if (devname[0] != '/')
			{
			/*
			 *	The name is not fully qualified -- build it.
			 */
			char	*newdevname;

			newdevname = (char *)malloc(strlen(Dev) +
				strlen(devname) + 1);
			strcpy(newdevname, Dev);
			strcat(newdevname, devname);
			devname = newdevname;
			}
		/*
		 *	Open standard input, output, error.
		 */
		close(0);
		close(1);
		close(2);
		stdin_fd = open(devname, O_RDONLY);
		stdout_fd = open(devname, O_WRONLY);
		dup(stdout_fd);
		/*
		 *	Wait for the modem negotiation to complete.
		 *	(Can you say "kludge"?)
		 */
		sleep(5);
		}
	else
		stdin_fd = 0;

	/*
	 *	Put the tty in cbreak mode, etc.
	 */
#ifdef gould
	if (gtty(stdin_fd,&Line_attrib) != 0)
		perror("Bad gtty call",errno);
	Line_attrib.sg_flags &= ~RAW;
	Line_attrib.sg_flags |= ECHO;
	Line_attrib.sg_flags |= CRMOD;
	if (argc > 2)
	Line_attrib.sg_ispeed =
	Line_attrib.sg_ospeed = speed;
	Line_attrib.sg_erase = '\010';
	if (stty(stdin_fd,&Line_attrib) != 0)
		perror("Bad stty call");
#else	/* not gould */
#ifdef _AIX
	if (tcgetattr(stdin_fd, &Line_attrib) != 0)
		perror("Bad ioctl TCGETA");
	/*
	 *	Set everything to a known state
	 */
	Line_attrib.c_iflag = ICRNL | IGNPAR | BRKINT;
	Line_attrib.c_oflag = ONLCR | OPOST;
	if (argc <= 2)	/* if speed wasn't given, save the old one */
		speed = Line_attrib.c_cflag & CBAUD;
	Line_attrib.c_cflag = CS8 | CREAD | HUPCL | speed | (speed << IBSHIFT);
	Line_attrib.c_lflag = ISIG | ICANON | ECHO | ECHOE | ECHOK |
		ECHOKE | IEXTEN;
	Line_attrib.c_cc[VINTR] = 0x03;
	Line_attrib.c_cc[VQUIT] = 0x1c;
	Line_attrib.c_cc[VERASE] = 0x08;
	Line_attrib.c_cc[VKILL] = 0x15;
	Line_attrib.c_cc[VEOF] = 0x04;
	Line_attrib.c_cc[VEOL] = 0x00;
	Line_attrib.c_cc[VSTART] = 0x11;
	Line_attrib.c_cc[VSTOP] = 0x13;
	Line_attrib.c_cc[VSUSP] = 0x1a;
	Line_attrib.c_cc[VEOL2] = 0x00;
	Line_attrib.c_cc[VDSUSP] = 0xff;
	Line_attrib.c_cc[VREPRINT] = 0x12;
	Line_attrib.c_cc[VDISCRD] = 0x0f;
	Line_attrib.c_cc[VWERSE] = 0x17;
	Line_attrib.c_cc[VLNEXT] = 0x16;
	if (tcsetattr(stdin_fd, TCSANOW, &Line_attrib) != 0)
		perror("Bad ioctl TCSETA");
	/*
	 *	Make sure that hardware flow control is set.
	 *	The system tends to "forget" this after a reboot.
	 *	Only do this for serial ports -- it will cause a
	 *	pseudo tty to hang.
	 */
	{
	union txname txname;
	int i;

	for (i=0; ; i++)
		{
		txname.tx_which = i;
		if ((ioctl(stdin_fd, TXGETCD, &txname) < 0) ||
				(txname.tx_name[0] == 0))
			break;
		if (strcmp(txname.tx_name, "rs") == 0)
			{
			/*
			 *	This is a serial port.
			 */
			if (ioctl(stdin_fd, TXADDCD, "rts") < 0)
				; /* ignore error if it's already there */
			break;
			}
		}
	}
#else	/* not _AIX */
	if (ioctl(stdin_fd,TCGETA,&Line_attrib) != 0)
		perror("Bad ioctl TCGETA");
	Line_attrib.c_iflag |= ICRNL ; 
	Line_attrib.c_iflag = ICRNL | IGNPAR | BRKINT;
	Line_attrib.c_oflag = ONLCR | OPOST;
	Line_attrib.c_lflag |= ECHO;
	/*
	Line_attrib.c_lflag |= ECHOE;
	*/
	Line_attrib.c_lflag |= ICANON;
	Line_attrib.c_lflag |= ISIG;

#ifdef ECHOCTL
	Line_attrib.c_lflag &= ~ECHOCTL;
#endif /* ECHOCTL */
	Line_attrib.c_cc[VINTR] = 0x03;
	Line_attrib.c_cflag |= HUPCL;
	Line_attrib.c_cc[VERASE] = '\010';
	if (argc > 2)
		{
		Line_attrib.c_cflag &= ~CBAUD;
		Line_attrib.c_cflag |= speed;
		}
	if (ioctl(stdin_fd,TCSETA,&Line_attrib) != 0)
		perror("Bad ioctl TCSETA");

#endif  /* _AIX  */
#endif	/* gould */
#ifdef TIOCGWINSZ
	/*
	 *	Make sure the window size is set to something sane.
	 */
#ifndef SOLARIS
	if (ioctl(stdin_fd,TIOCGWINSZ,&winsize) != 0)
		perror("Bad ioctl TIOCGWINSZ");
#endif

	winsize.ws_row = 24;
	winsize.ws_col = 80;
	if (ioctl(stdin_fd,TIOCSWINSZ,&winsize) != 0)
		perror("Bad ioctl TIOCSWINSZ");
	
#endif /* TIOCGWINSZ */
	/*
	 *	Check to see if logins are disabled.
	 */
	checklogins();

	/*
	 *	Build a utmp entry so that a user will show up.
	 */
	addutmp(stdin_fd, remhost, whoname);

	/*
	 *	Fork a child process to do the rest.
	 */
	Child = fork();
	if (Child < 0)
		{
		perror("imstty fork");
		exit(1);
		}
	if (Child > 0)
		{
		/*
		 *	Parent process -- wait for the child to exit and
		 *	log the user off.
		 */
		signal(SIGHUP,killchild);
		wait(0);
		delutmp(stdin_fd);
		exit(0);
		}

	/*
	 *	If a usename was given, get the password file entry
	 *	and change our user ID and directory.
	 */
	if (username)
		{
		pwd = getpwnam(username);
		if (pwd == (struct passwd *)0)
			{
			fprintf(stderr,"user %s doesn't exist\n", username);
			exit(2);
			}
		chdir(pwd->pw_dir);
		setgid(pwd->pw_gid);
		initgroups(username, pwd->pw_uid);
		setuid(pwd->pw_uid);
		}
	/*
	 *	Build a small environment.
	 */
	{
	int lastenv;

	lastenv = 0;
	env = (char **)malloc(6 * sizeof(char *));
	env[lastenv] = (char *)malloc(strlen(pwd->pw_dir) + 6);
	strcpy(env[lastenv], "HOME=");
	strcat(env[lastenv], pwd->pw_dir);
	lastenv++;
	env[lastenv] = (char *)malloc(strlen(pwd->pw_name) + 8);
	strcpy(env[lastenv], "LOGNAME=");
	strcat(env[lastenv], pwd->pw_name);
	lastenv++;
	env[lastenv] = "PATH=/usr/local/bin:/bin:/usr/bin";
	lastenv++;
	/*
	 *	If TERM was already set in the environment, use it.
	 */
	for (i=0; environ[i]; i++)
		if (strncmp(environ[i],"TERM=",5) == 0)
			{
			env[lastenv] = environ[i];
			lastenv++;
			break;
			}
	/*
	 *	If DISPLAY was already set in the environment, use it.
	 */
	for (i=0; environ[i]; i++)
		if (strncmp(environ[i],"DISPLAY=",8) == 0)
			{
			env[lastenv] = environ[i];
			lastenv++;
			break;
			}
	env[lastenv] = (char *)0;
	}
	/*
	 *	Set up the transaction log.
	 */
	environ = env;
#ifdef TRANLOG
	setuptranlog(pwd->pw_dir, devname, remhost, port_type);
#endif /* TRANLOG */

	/*
	 *	Init sets us up to ignore SIGHUP, so we need to
	 *	re-enable it here.  The others are for good measure.
	 */
	signal(SIGHUP, SIG_DFL);
	signal(SIGINT, SIG_DFL);
	signal(SIGTERM, SIG_DFL);

	/*
	 *	Exec the real command.
	 */
	argv0 = strrchr(imspgm, '/');
	if (!argv0)
		argv0 = imspgm;
	else
		argv0++;
	
	execle(imspgm, argv0, 0, environ);
	perror(imspgm);
	exit(1);
	}

/*
 *	ioc_spd takes a line speed (e.g. 1200 or 9600) and returns the
 *	appropriate value for an ioctl call to set the speed (e.g. B1200
 *	or B9600).
 */
static int ioc_spd(speed)
	int	speed;		/* line speed, e.g. 1200 for 1200 baud */
	{
	register int i;		/* loop counter */
	static int speeds[] = {	/* table of supported speeds */
		300,	B300,
		1200,	B1200,
		2400,	B2400,
		4800,	B4800,
		9600,	B9600,
		19200,	EXTA,
		0};
	
	for (i=0; speeds[i] > 0; i += 2)
		if (speeds[i] == speed)
			return(speeds[i+1]);
	return(0);
	}

/*
 *	addutmp writes a "dummy" utmp entry so that we can tell the line
 *	is in use.
 */
addutmp (stdin_fd, remhost, whoname)
	int	stdin_fd;	/* file desc. of standard input (for tty dev) */
	char	*remhost;	/* remote host name */
	char	*whoname;	/* name to be used in "who" listings */
	{
	struct utmp	utmp;		/* buffer for writing to utmp file */
	int		utmp_fd;	/* file desc for utmp file */
	int		utmp_slot;	/* slot in utmp file */
	char		*ttydev;	/* name of tty device */
	char		*ttyname();

	ttydev = ttyname(stdin_fd);
#ifdef SYSV
	/*
	 *	Find our entry in the utmp file.  If the ut_type field
	 *	is DEAD_PROCESS, getutid will search for a matching
	 *	ut_ut_id instead.
	 */
	setutent();
	strncpy(utmp.ut_id, ttydev+strlen(Dev), sizeof(utmp.ut_id));
	utmp.ut_type = DEAD_PROCESS;
	getutid(&utmp);

	/*
	 *	Build the new entry.
	 */
	utmp.ut_exit.e_exit = 2;
	strncpy(utmp.ut_name, whoname, sizeof(utmp.ut_name));
#if !defined(sgi)
#ifndef SOLARIS
	strncpy(utmp.ut_host, remhost, sizeof(utmp.ut_host));
#endif
#endif
	strncpy(utmp.ut_id, ttydev+strlen(Dev), sizeof(utmp.ut_id));
	strncpy(utmp.ut_line, ttydev+strlen(Dev), sizeof(utmp.ut_line));
	utmp.ut_pid = getpid();
	utmp.ut_time = time(0);
	utmp.ut_type = USER_PROCESS;

	/*
	 *	Write it out and close the file.
	 */
	pututline(&utmp);
	endutent();

#else	/* ! SYSV */
	/*
	 *	ttyslot is a library routine that figures out where
	 *	we belong in the utmp file.
	 */
	utmp_slot = ttyslot();

	/*
	 *	Open the utmp file.  If the open fails, just forget it.
	 */
	if (utmp_slot <= 0)
		utmp_fd = open(UTMP_FILE, O_WRONLY | O_APPEND);
	else
		utmp_fd = open(UTMP_FILE, O_WRONLY);
	if (utmp_fd < 0)
		return;
	/*
	 *	Fill in the utmp entry and write it to the file.
	 *	The user name (whoname) is just a dummy to indicate an IMS user.
	 */
	ttydev += 5;	/* strip off /dev/ */
	strncpy(utmp.ut_line, ttydev, sizeof(utmp.ut_line));
	strncpy(utmp.ut_name, whoname, sizeof(utmp.ut_name));
	strncpy(utmp.ut_host, remhost, sizeof(utmp.ut_host));
	time(&utmp.ut_time);
	if (utmp_slot > 0)
		lseek(utmp_fd, utmp_slot * sizeof(utmp), 0);
	write(utmp_fd, &utmp, sizeof(utmp));
	close(utmp_fd);
#endif	/* SYSV */
	}
/*
 *	delutmp delete the utmp entry written above.  Init would normally
 *	do this for async connections, but we must do it for telnet
 *	connections.
 */
delutmp(stdin_fd)
	int	stdin_fd;	/* file desc. of standard input (for tty dev) */
	{
#ifdef SYSV
	struct utmp	utmp;
	struct utmp	*p;
	char		*ttydev;	/* name of tty device */
	char		*ttyname();

	utmp.ut_type = USER_PROCESS;
	ttydev = ttyname(stdin_fd);
	strncpy(utmp.ut_id, ttydev+strlen(Dev), sizeof(utmp.ut_id));
	(void) setutent();
	if (p = getutid(&utmp))
		{
		p->ut_type = DEAD_PROCESS;
		p->ut_time = time(0);
		pututline(p);
		}
#else	/* not SYSV */
#endif	/* not SYSV */
	}

/*
 *	setuptranlog sets an environment variable, TRANDIR, to be
 *	the name of a file with connect information in it.
 */
setuptranlog(home, devname, remhost, port_type)
	char	*home;			/* name of home directory */
	char	*devname;		/* name of tty device */
	char	*remhost;		/* remote host name for net login */
	char	*port_type;		/* type of async port */
	{
	char	*fname;			/* file name (to be built) */
	FILE	*tranlog;		/* transaction log file */
	char	*dirname;		/* name of transaction log directory */
	char	*trandir = "TRANDIR";	/* name of environment variable */
	long	now;			/* current time in internal format */
	struct tm	*currtime;	/* current time and date */
	struct tm	*localtime();	/* function to convert time */
	char	*getenv();

	/*
	 *	The environment variable "TRANDIR" should contain the
	 *	name of the transaction log directory (if it doesn't,
	 *	use $HOME/trans).  We take the directory name and append
	 *	pid.log to build a unique name.  This name then
	 *	replaces the TRANDIR environment variable.
	 */
	dirname = getenv(trandir);
	if (dirname == (char *)0)
		{
		dirname = malloc(strlen(home) + 20);
		strcpy(dirname, home);
		strcat(dirname,"/trans");
		}
	fname = malloc(strlen(dirname) + 20);
	sprintf(fname, "%s/%d.log", dirname, getpid());
	setenv(trandir, fname);

	/*
	 *	Open the file and write the connect information in it.
	 */
	tranlog = fopen(fname, "w");
	if (tranlog == NULL)
		return;
	now = time(0);
	currtime = localtime(&now);
	fprintf(tranlog, "LOGON|%04d/%02d/%02d|%02d:%02d|",
		currtime->tm_year + 1900, currtime->tm_mon + 1,
		currtime->tm_mday, currtime->tm_hour, currtime->tm_min);
	if (port_type == (char *)0)
		port_type = "local";
	if (strcmp(remhost,"") == 0)
		fprintf(tranlog, "%s|%s", port_type, devname);
	else
		fprintf(tranlog, "internet|%s", remhost);
	if (strcmp(Program_name, "mailorder") == 0)
		fprintf(tranlog, "|mailbox");
	fprintf(tranlog, "\n");
	fclose(tranlog);
	}

/*
 *	killchild is the signal handler for the hangup signal.
 *	It merely propagates the signal to the child process.
 *	It would be easier to simply ingore SIGHUP, but then
 *	the child never gets it and the IMS process hangs around.
 */

void killchild(signo)

	int signo;		/* signal */
	{
	kill(Child, signo);
	signal(signo, killchild);
	exit(0);
	}
/*
 *	Check to see if logins are disabled.  If the file "/etc/nologin"
 *	exists, send it's contents to the terminal and exit.
 */
checklogins()
	{
	FILE	*nologin;	/* /etc/nologin file */
	char	buf[1024];	/* line buffer for file */

	nologin = fopen("/etc/nologin","r");
	if (nologin == NULL)
		return;
	while (fgets(buf, 1024, nologin) != NULL)
		fputs(buf, stdout);
	sleep(5);	/* give them time to read it */
	exit(1);
	}
/*
 *	setenv sets an environment variable
 */
extern char	**environ;

static void
setenv(name, value)
	char	*name;
	char	*value;
	{
	register char	**newenv, **newenviron;
	register char	**oldenv;
	register int	indx=0;
	/*
	 * Search for the desired environment variable in the current
	 * environment
	 */
	for (oldenv = environ; *oldenv != NULL; oldenv++, indx++)
		if (strncmp(*oldenv, name, strlen(name)) == 0)
			{
			stuffenv(oldenv, name, value);
			return;
			}
	/*
	 * The environment variable was not found, so allocate
	 * storage for a new environment
	 */
	if ((newenviron = (char **)malloc((indx + 2) * sizeof(char *))) == NULL)
		{
		printf("out of memory\n");
		exit(1);
		}

	/*
	 * Copy the environment
	 */
	for (oldenv = environ, newenv = newenviron; ; oldenv++, newenv++)
		{
		*newenv = *oldenv;
		/*
		 * Add the desired environment variable at the
		 * end of the environment
		 */
		if (*newenv == NULL)
			{
			stuffenv(newenv, name, value);
			*++newenv = NULL;
			environ = newenviron;
			return;
			}
		}
	}
static void
stuffenv(where, name, value)
	char	**where;	/* the address of where the evironment
				variable is stored */
	char	*name;		/* the name of the environment variable */
	char	*value;		/* the value of the environment variable */
	{

	if ((*where = malloc(strlen(name) + strlen(value) + 2)) ==
		NULL)
                {
		printf("out of memory\n");
		exit(1);
		}
	strcpy(*where, name);
	strcat(*where, "=");
	strcat(*where, value);
	return;
	}
