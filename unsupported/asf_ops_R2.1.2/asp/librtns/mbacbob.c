/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbacbob.c -- invoke bob in batch mode */

int CALL = 0;
extern char name[80];

/* cbob (fname) --------------------------------------------------------
	This routine sets up the global variables CALL and name to
	specify running bob in batch mode, and then invokes bob.
	The batch file to execute is specified by "fname".
*/
cbob(fname)
char *fname;
{
	int end;

	strcpy(name,fname);
	CALL=1;
	end=abob();
	printf("\n");
	return(end);
}
