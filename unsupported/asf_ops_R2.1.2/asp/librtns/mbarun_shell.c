/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbarun_shell.c -- fork a c shell from another program */

#include <stdio.h>

/* run_shell() -------------------------------------------------------
	This routine forks a new process which runs a c-shell, then
	waits for it to complete.
*/
run_shell()
{
	int status,i;

	i = fork();
	if (i < 0)			/* fork failed */
	    return (0);
	if (i == 0) {			/* child process comes here */
	    execl("/bin/csh","csh",0);
	    fprintf(stderr,"can't execute shell");
	    exit (1);
	}
	if (i > 0)			/* parent process comes here */
	    wait(&status);
	return (1);
}
