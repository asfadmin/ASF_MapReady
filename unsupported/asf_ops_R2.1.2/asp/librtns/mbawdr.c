/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbawdr.c -- write response data to a file */

#include <aspdecl.h>
#include <fcntl.h>
#include <stdio.h>

#define RMODE_MEM_READ 0
#define MEND_EXEC 0xfffff

/* wdr (start, end, filename) ------------------------------------------
	This routine retrieves data from the response buffer and writes
	it to a disk file.  If a problem is encountered, the routine
	returns FAIL, otherwise PASS.
*/
wdr (start, end, filename)
int start, end;
char *filename;
{
	int pgaddr, pgend, i, iend, j, out, len;
	unsigned short int *r;
	char s[100];

    /* open disk file */
	if ((out = open(filename,O_WRONLY + O_CREAT,0777)) == -1) {
	    sprintf(s,"cannot open %s file",filename);
	    perror(s);
	    return (FAIL);
	}
	mb.w[RLOC_REP] = 0;
	ex_set_resp_mode (RMODE_MEM_READ);
	ex_get_resp_addr (start, &pgaddr, &i);
	ex_get_resp_addr (end, &pgend, &j);
	if (pgend < pgaddr || (pgend == pgaddr && j < i)) pgend += 64;
	while (pgaddr <= pgend) {
	    r = &mb.w[i >> 1];
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    len = iend - i;
	    ex_set_resp_page (pgaddr++);
	    asp_read( i, r, PAGE_SIZE - ( i-MLOC_EXEC ) );
	    if (write(out,r,len) < len) {
		sprintf("error writing %s file:",filename);
		perror(s);
		close(out);
		return (FAIL);
	    }
	    i = MLOC_EXEC;
	}
	close(out);
	return (PASS);
}
