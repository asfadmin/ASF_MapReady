/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbawd.c -- write mb array data to a file */

#include <aspdecl.h>
#include <fcntl.h>
#include <stdio.h>

#define MEND_EXEC 0xfffff

/* wd (start, end, filename) -------------------------------------------
	This routine retrieves data from the mb array and writes
	it to a disk file.  If a problem is encountered, the routine
	returns FAIL, otherwise PASS.
*/
wd (start, end, filename)
int start, end;
char *filename;
{
	int out, len;
	unsigned short int *m;
	char s[100];

    /* open disk file */
	if ((out = open(filename,O_WRONLY + O_CREAT,0777)) == -1) {
	    sprintf("cannot open %s file",filename);
	    perror(s);
	    return (FAIL);
	}
    /* write data to the file */
	m = &mb.w[start >> 1];
	len = end - start;
	if (write(out,m,len) < len) {
	    sprintf("error writing %s file:",filename);
	    perror(s);
	    close(out);
	    return (FAIL);
	}
    /* close the file */
	close(out);
	return (PASS);
}
