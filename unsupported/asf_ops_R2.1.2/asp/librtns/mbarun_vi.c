/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbarun_vi.c -- fork the vi editor from another program */

#include <stdio.h>

/* run_vi(filename) ----------------------------------------------------
	This routine forks a new process which runs the vi editor on the
	given file, then waits for it to complete.
*/
run_vi(filename)
char *filename;
{
	int status,i;

	i = fork();
	if (i < 0)			/* fork failed */
	    return (0);
	if (i == 0) {			/* child process comes here */
	    execl("/usr/bin/vi","vi",filename,0);
	    fprintf(stderr,"can't execute editor\n");
	    exit (1);
	}
	if (i > 0)			/* parent process comes here */
	    wait(&status);
	return (1);
}
