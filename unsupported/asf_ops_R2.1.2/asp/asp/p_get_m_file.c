/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* p_get_m_file.c -- routine to read data function files */

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <procfil.h>
#include <procdec.h>


/* p_get_mem_file (Data, len, filename, ext) ---------------------------
	This routine reads a disk file containing data values, and
	loads those values into the array Data.  The file has the
	format:
	    <ascii data of any length>
	    $$nnnnn\n
	    <16-bit binary words>
	where nnnnn is an ascii-coded decimal string specifying how
	many binary words are to follow, and \n is a newline character.

	If there is a problem reading the file, a message is printed and
	the routine returns FAIL.  Otherwise, the routine returns PASS.
*/

p_get_mem_file (Data, len, filename, ext)
	short int Data[];
	int len;
	char *filename, *ext;
{
	int i, fd, dc;
	FILE *fp,*fdopen();
	char c, fname[100];
	char *strcat(), *strcpy();
	long int seekpos, ftell();

    /* open the file */
	strcat (strcpy (fname, filename), ext);
	fd = open (fname,O_RDONLY);
	if (fd < 0) {
	    printf ("Cannot open %s\n",fname);
	    return (FAIL);
	}
	fp = fdopen (fd,"r");

    /* scan for $$ */
	dc = 0;
	while ((c = fgetc(fp)) != EOF) {
	    if (c == '$')
		dc++;
	    else
		dc = 0;
	    if (dc == 2)
		break;
	}
	if (c == EOF) {
	    printf("Input file error on %s\n",fname);
	    printf("     - could not find start of data\n");
	    fclose (fp);
	    close (fd);
	    return (FAIL);
	}

    /* read the word count */
	fscanf (fp,"%d%c",&dc,&c);

    /* read the binary data */
	seekpos = ftell(fp);
	lseek (fd,seekpos,SEEK_SET);
	if (len < dc)
	    dc = len;
	i = read (fd, Data, dc << 1);

    /* close the file */
	fclose (fp);
	close (fd);

    /* if fewer words read than asked for, zero the rest */
	if (len > dc)
	    for (i = dc; i < len; i++)
		Data[i] = 0;

	return (PASS);
}


/* p_write_mem_file (Data, len, filename, ext, str) --------------------
	This routine writes a memory file of the same format used by
	the p_get_mem_file routine.  "str" provides the leading string
	of ascii characters (if any).  The file is written into the
	current directory.
*/

p_write_mem_file (Data, len, filename, ext, str)
	short int *Data;
	int len;
	char *filename, *ext, *str;
{
	FILE *fp;
	char fname[100];
	char *strcat(), *strcpy();

    /* open the file */
	strcat (strcpy (fname, filename), ext);
	if ((fp = fopen (fname,"w")) == NULL) {
	    printf ("Cannot open %s\n",fname);
	    return (FAIL);
	}

    /* write the data to the file */
	fputs(str,fp);
	fprintf(fp,"\n$$%d\n",len);
	fwrite(Data,2,len,fp);
	fclose(fp);
}
