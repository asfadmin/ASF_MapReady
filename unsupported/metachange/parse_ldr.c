/********************************************************************
FUNCTION NAME:   parse_ldr.c

SYNTAX: parse_ldr(char *filename,void *passThisAlong,
			void (*chunk_processor)(HEADER *bufHeader,char *buf,int era,void *passThisAlong))

DESCRIPTION:
 reads ASF groundstation file & calls the passed-in procedure with
 each of its chunks.  The procedure should have the form:
 
 void my_chunk_processor(struct HEADER *bufHeader,char *dataBuf,int era,void *passThisAlong);
 
 PassThisAlong can be used for anything you want to send to your chunk
 processor-- parse_ldr just passes it along.
 

RETURN VALUE:	0 indicates failure, 1 indicates success

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           2/97   O. Lawlor (ASF)  converted from T. Logan's get_facdr

*********************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ceos.h"
typedef (*chunk_processor_type)(struct HEADER *bufHeader,char *buf,int era,void *passThisAlong);
parse_ldr(char *filename,void *passThisAlong,
	chunk_processor_type chunk_processor)
{
	FILE 	*fp;
	char  name[256];
	char 	buff[11000];
	int	itype, length, era;
	struct  HEADER  bufhdr;
	era = set_era(filename,name,-1);
	if ((fp = fopen(name, "r")) == NULL)
		{fprintf(stderr,"PARSE_LDR:  Unable to open file %s\n", name);return(-1);}
	while (1)
	{
		if (fread (&bufhdr, 12, 1, fp) == NULL)
		{
			fclose(fp);
			return(-1);
		}
		length = bufhdr.recsiz - 12;
		if((fread(buff, length, 1, fp)) == NULL) 
			{ fprintf(stderr,"PARSE_LDR: Error reading data portion of record.\n"); return(-1);}
		(*chunk_processor)(&bufhdr,buff,era,passThisAlong);
	}
}
