/*CEOS Input Library implementation.

*/
#include "asf.h"
#include "asf_meta.h"
#include "ddr.h"
#include "ceos.h"
#include "ceos_io.h"

CEOS_FILE *fopenCeos(char *fName)
{
	CEOS_FILE *c=(CEOS_FILE *)MALLOC(sizeof(CEOS_FILE));
	
/* Determine format and size of input file */
	ceos2ddr(fName,&c->ddr,&c->headerBytes,&c->lineBytes);

/* Determine name of, then open input file.*/
	set_era(fName,c->name,0);
	c->f_in=FOPEN(c->name,"rb");
	return c;
}

void readCeosLine(int *dest,int y,CEOS_FILE *c)
{
	int ns=c->ddr.ns;
	int lineLen=ns*dtype2dsize(c->ddr.dtype,NULL);
	unsigned char *buf=(unsigned char *)MALLOC(lineLen);
	int i;
	FSEEK(c->f_in,c->headerBytes+y*c->lineBytes,0);
	FREAD(buf,1,lineLen,c->f_in);
	if (c->ddr.dtype==DTYPE_BYTE)
		for (i=0;i<ns;i++)
			dest[i]=buf[i];
	else 
		if (c->ddr.dtype==DTYPE_SHORT)
		for (i=0;i<ns;i++)
			dest[i]=(buf[2*i]<<8)+buf[2*i+1];
	else 
	{
		fprintf(stderr,"Attempted to read unsupported CEOS data type %d\n"
			"from image file %s!\n",c->ddr.dtype,c->name);
		exit(1);
	}
		
}

void closeCeos(CEOS_FILE *in)
{
	FCLOSE(in->f_in);
	in->f_in=NULL;
	FREE((void *)in);
}
