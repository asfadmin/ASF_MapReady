#ifndef lint
static char sccsid[] = "@(#)msgtest.c	1.1  12/19/96";
#endif

#include <stdio.h>
#include <fcntl.h>
#include <syslog.h>
#include "asf_syslog.h"

main(argc,argv)
	int argc;
	char *argv[];
{
	if(argv[1] == NULL) {
		printf("Usage: msgtest <message>\n");
		printf("Including a 'WARN' message will generate a warning beep\n");
		printf("Including a 'CRIT' message will generate a warning beep + ACK window\n");
		exit(1);
	}
	/*
	** This opens a logfile session for 'LOG_ASFSYSTEM' facility.
	** with the 'ASF_SYSTEM' message and process id.  Messages will be sent
	** to the console device (in addition to wherever else they may be sent
	** by syslog.conf).
	** 
	** This type of message will currently end up going to /var/adm/messages,
	** according to the current /etc/syslog.conf on my machine (skank).
	**
	** See syslog MAN pages for more info.
	*/

 	openlog("ASF_SYSTEM:",LOG_PID|LOG_CONS|LOG_NDELAY,LOG_ASFSYSTEM);

	/*
	** This controls logging of messages up to (and including) priority,
	** LOG_DEBUG.  Any messages with a priority lower than LOG_DEBUG
	** will not be logged.
	*/

 	setlogmask(LOG_UPTO(LOG_DEBUG));

	/*
	** This takes the message given on the command line and logs it as a
	** LOG_ALERT.  Output will depend on the entries for user.alert
	** (facility.level) in the syslog.conf file.
	*/

	syslog(LOG_ERR,argv[1]);

	/* This closes the logfile session. */

 	closelog();

	exit(0);
}
