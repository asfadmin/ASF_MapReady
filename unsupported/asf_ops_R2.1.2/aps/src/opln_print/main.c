#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		main.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/opln_print/SCCS/s.main.c"

#include <stdio.h>

main(int argc, char *argv[])
{
	void 	usage_exit(char*, int);
	char	filename[80] ;
	int		rcode;
	FILE	*fp_in;
	char	buf[10];	/* small buf for file checking. 	*/
	int		nbytes;		/* for checking for read error.  	*/

	/* set stdout to unbuffered I/O */
	setbuf( stdout, (char *) NULL ) ;

	if(argc != 2 )
		usage_exit(argv[0], 1);

	if(strcmp(argv[1], "-MP") == 0 )
		usage_exit(argv[0], 2);

	/* now pick up the positional parameter file name.		*/
	strcpy(filename, argv[1]);

	/* open the file.  	*/
	fp_in = fopen(filename, "r");
		if(fp_in == NULL)
		{
			fprintf(stderr, "%s: cannot open %s...\n", 
			argv[0], filename);
			exit(1);
		}

	/* check the file */
	nbytes = fread(buf, 1, 5, fp_in);
	if(nbytes < 5)
	{
		fprintf(stderr,
			"%s:  the file %s is not an OPLN file\n", argv[0], argv[1]);
		exit(1);
	}

	if(strncmp(buf, "OPLN", 4) != 0)
	{
		fprintf(stderr,
			"%s:  the file %s is not an OPLN file\n", argv[0], argv[1]);
		exit(1);
	}

	rcode = fseek(fp_in, 0L, SEEK_SET);
	if(rcode != 0)
	{
		fprintf(stderr,
			"%s:  error rewinding input file %s\n", argv[0], argv[1]);
		exit(1);
	}

	/* insert header into output:  	*/
	fprintf(stdout, "J-ERS-1 OPLN file\n");
	fprintf(stdout," Input file name: %s\n\n", filename);

	rcode = print_opln(fp_in, stdout);
	switch (rcode)
	{
		case -1:
			fprintf(stderr, "%s:  unexpected EOF\n", argv[0]);
			break;
		case -2:
			fprintf(stderr, "%s:  error during read\n", argv[0]);
			break;
		case -3:
			fprintf(stderr, "%s:  the file %s is not an OPLN file\n", 
				argv[0], argv[1]);
			break;
		default:
			break;
	}

	fclose (fp_in);

	exit(rcode);

}

void usage_exit(char *progname, int j)
{
	fprintf(stderr,"usage:  %s [OPLN_filename]\n\n", 
			progname);
	fprintf(stderr,
		"\tThis command will write the NASDA OPLN file to standard output.\n");
	fprintf(stderr,
		"\tThis command has proved useful in viewing this file; \n");
	fprintf(stderr, "\tvi wasn't useful for this purpose.  \n");
	exit(j);
}
