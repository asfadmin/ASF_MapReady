/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* wait_io.c -- read a string or integer value in so many seconds */

#include <sys/time.h>
#include <stdio.h>
#include <signal.h>
#include <ctype.h>

int alarm = 0;

/* wait_for_int(value, timeout) ----------------------------------------
	This routine reads an integer value from the keyboard, and
	returns that value in 'value'.  If the value has not been
	entered in 'timeout' seconds, the routine returns -1, else
	the routine returns 0.
*/
wait_for_int(value, timeout)
int *value, timeout;
{
	int i;
	char t[17];

	i = wait_for_str(t,15,timeout);
	*value = atoi(t);
	return (i);
}

/* wait_for_str(t, len, timeout) ---------------------------------------
	This routine reads a string of characters from the keyboard,
	terminated by a newline character, and places the string in
	t.  A maximum of 'len' characters are read.  If the input has
	not been entered within 'timeout' seconds, the routine returns
	a -1, otherwise the routine returns 0.
*/
wait_for_str(t, len, timeout)
char t[];
int len, timeout;
{
	char s[256];
	int r = 0;

	set_alarm(timeout);
	if (gets(s) == NULL) {
	    t[0] = '\0';
	    r = -1;
	    s[0] = 'x'; 
	    open_keyboard();
	    while (s[0] != '\n' && s[0] != '\0')
		get_byte(s);
	    close_keyboard();
	} else strncpy(t,s,len);
	set_alarm(0);
	return (r);
}

/* trap_alarm() --------------------------------------------------------
	This routine traps the SIGALRM signal if the real time interval
	timer (set by set_alarm) times out.  The routine flags the
	event by setting 'alarm' to 1.
*/
void trap_alarm()
{
	alarm = 1;
}

/* set_alarm(timeout) --------------------------------------------------
	This routine sets up a real time interval timer with the value
	given by timeout (in seconds).
*/
set_alarm(timeout)
int timeout;
{
	struct itimerval time;
	void trap_alarm();

    /* clear any previous timer value */
	time.it_value.tv_sec = 0;
	time.it_value.tv_usec = 0;
	time.it_interval.tv_sec = 0;
	time.it_interval.tv_usec = 0;
	setitimer(ITIMER_REAL,&time,NULL);
	alarm = 0;
    /* if timeout is non-zero, set new timer */
	if (timeout) {
	    signal(SIGALRM, trap_alarm);
	    time.it_value.tv_sec = timeout;
	    setitimer(ITIMER_REAL,&time,NULL);
	}
}

/* wait_seconds(n) -----------------------------------------------------
	This routine waits n seconds, then returns.
*/
wait_seconds(n)
int n;
{
	char s[80];

	set_alarm(n);
	gets(s);
}
