/*
  LAS 6.0 Image I/O routines: these routines
  allow you to input and output LAS image lines of
  any type as though they were floating point arrays--
  these procedures take care of all neccessary conversions.
  
  Note they only work with byte, short, long, float, and double images.
  Complex images are unsupported.
*/
#include "asf.h"
#include "las.h"
#include "asf_meta.h"
#include "asf_endian.h"

int dtype2dsize(int dtype,char **description)
{
	char *messTemp;
	if (description==NULL)
		description=&messTemp;
	switch(dtype)
	{
		case DTYPE_BYTE: *description="Unsigned Char pixels (1-byte unsigned).\n"; return 1;
		case DTYPE_SHORT: *description="Short int pixels (2-byte signed).\n";  return 2;
		case DTYPE_LONG: *description="Long int pixels (4-byte signed).\n"; return 4;
		case DTYPE_FLOAT: *description="Floating point pixels (32-bit IEEE).\n"; return 4;
		case DTYPE_DOUBLE: *description="Double pixels (64-bit IEEE).\n"; return 8;
		case DTYPE_COMPLEX: *description="Complex pixels (2x 4-byte IEEE).\n"; return 8;
		case DTYPE_COMPLEXREAL: *description="Complex pixels-- real component (2x 4-byte IEEE)\n";return 8;
		case DTYPE_COMPLEXIMAG: *description="Complex pixels-- imaginary component (2x 4-byte IEEE)\n";return 8;
		default:
			printf("Fatal Error: Unrecognized pixel format code '%i' in DDR.\n",dtype);
			exit(1);
	}   return (0); /***To make compiler HAPPY***/
}
/*Return the file offset to the given line and band number.
Lines and bands are indexed from zero.
*/
long long seekLoc(const struct DDR *ddr,int yLine,int bandNo)
{
	int dsize=dtype2dsize(ddr->dtype,NULL);
	return (long long) dsize*ddr->ns*(bandNo*ddr->nl+yLine);
}

/*Read a floating-point line from a LAS image file, 
converting if necessary. Lines and bands are indexed from zero.*/
void getFloatLine_mb(FILE *f,const struct DDR *ddr,int yLine,int bandNo,float *dest)
{
	int x,maxX=ddr->ns;
	int dsize=dtype2dsize(ddr->dtype,NULL);	
	void *inputBuf = MALLOC(dsize*maxX);

	FSEEK64(f,seekLoc(ddr,yLine,bandNo),0);
	FREAD(inputBuf,dsize,maxX,f);		

	if (ddr->dtype==DTYPE_FLOAT) {
		for (x=0;x<maxX;x++) {
			ieee_big32(((float*)inputBuf)[x]);
			dest[x]=((float*)inputBuf)[x];
		}
	}
	else if (ddr->dtype==DTYPE_BYTE) {
		for (x=0;x<maxX;x++) {
			dest[x] = ((unsigned char *)inputBuf)[x];
		}
	}
	else if (ddr->dtype==DTYPE_SHORT) {
		for (x=0;x<maxX;x++) {
			big16(((short int *)inputBuf)[x]);
			dest[x]=((short int *)inputBuf)[x];
		}
	}
	else if (ddr->dtype==DTYPE_LONG) {
		for (x=0;x<maxX;x++) {
			big32(((int *)inputBuf)[x]);
			dest[x]=((int *)inputBuf)[x];
		}
	}
	else if (ddr->dtype==DTYPE_DOUBLE) {
		for (x=0;x<maxX;x++) {
			ieee_big64(((double *)inputBuf)[x]);
			dest[x]=((double *)inputBuf)[x];
		}
	}
	FREE(inputBuf);
}
void getFloatLine(FILE *f,const struct DDR *ddr,int yLine,float *dest)
{/*As above, but assume band #0*/
	getFloatLine_mb(f,ddr,yLine,0,dest);
}

/*Write a floating-point line to a LAS image file, 
converting if necessary. Lines and bands are indexed from zero.*/
void putFloatLine_mb(FILE *f,const struct DDR *ddr,int yLine,int bandNo,const float *source)
{
	int x,maxX=ddr->ns;
	int dsize=dtype2dsize(ddr->dtype,NULL);
	void *outputBuf = MALLOC(dsize*maxX);
	
	FSEEK64(f,seekLoc(ddr,yLine,bandNo),0);

	if (ddr->dtype==DTYPE_FLOAT) {
		for (x=0;x<maxX;x++) {
			((float *)outputBuf)[x] = source[x];
			ieee_big32( ((float *)outputBuf)[x] );
		}
	}
	else if (ddr->dtype==DTYPE_BYTE) {
		for (x=0;x<maxX;x++) {
			((unsigned char *)outputBuf)[x] = source[x];
		}
	}
	else if (ddr->dtype==DTYPE_SHORT) {
		for (x=0;x<maxX;x++) {
			((short *)outputBuf)[x] = source[x];
			big16( ((short *)outputBuf)[x] );
		}
	}
	else if (ddr->dtype==DTYPE_LONG) {
		for (x=0;x<maxX;x++) {
			((int *)outputBuf)[x]=source[x];
			big32( ((int *)outputBuf)[x] );
		}
	}
	else if (ddr->dtype==DTYPE_DOUBLE) {
		for (x=0;x<maxX;x++) {
			((double *)outputBuf)[x] = source[x];
			ieee_big32( ((double*)outputBuf)[x] );
		}
	}
	FWRITE(outputBuf,dsize,maxX,f);
	FREE(outputBuf);
}
void putFloatLine(FILE *f,const struct DDR *ddr,int yLine,const float *source)
{/*As above, but assume band #0*/
	putFloatLine_mb(f,ddr,yLine,0,source);
}


