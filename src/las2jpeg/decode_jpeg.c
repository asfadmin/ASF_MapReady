/****************************************************************
Decode_jpeg:
	Reads given FILE * as a JFIF/JPEG file; writes
it to the given LAS image.
****************************************************************/
#include "asf.h"
#include "ifm.h"
#include "ifm2ppm.h"
#include "ddr.h"

#include "jpeglib.h"
/* global & local function declaration */

void decode_jpeg(FILE *fin,char *foutN)
{
	int x,y,i,wid,ht;
	struct DDR ddr;
	struct jpeg_error_mgr mgr;
	struct jpeg_decompress_struct cinfo;
	JSAMPLE *rgbBuf;
	char *outBuf;
	FILE *fout;	

	cinfo.err=jpeg_std_error(&mgr);
	jpeg_create_decompress(&cinfo);
	jpeg_stdio_src(&cinfo,fin);

	(void)jpeg_read_header(&cinfo,TRUE);
	(void)jpeg_start_decompress(&cinfo);

	wid=cinfo.output_width;
	ht=cinfo.output_height;
	rgbBuf=(JSAMPLE *)MALLOC(sizeof(JSAMPLE)*wid*cinfo.output_components);
	outBuf=(char *)MALLOC(wid*ht*cinfo.output_components);
	c_intddr(&ddr);
	ddr.nl=ht;ddr.ns=wid;ddr.dtype=1;
	ddr.nbands=cinfo.output_components;
	c_putddr(foutN,&ddr);
	
	for (y=0;y<ht;y++)
	{
		(void)jpeg_read_scanlines(&cinfo,&rgbBuf,1);
		for (x=0;x<wid;x++)
			for (i=0;i<ddr.nbands;i++)
				outBuf[(i*ht+y)*wid+x]=rgbBuf[cinfo.output_components*x+i];
	}

	(void)jpeg_finish_decompress(&cinfo);
	(void)jpeg_destroy_decompress(&cinfo);
	
	fout=fopenImage(foutN,"wb");
	FWRITE(outBuf,ddr.nbands,wid*ht,fout);
	FCLOSE(fout);

	FREE(outBuf);
	FREE(rgbBuf);
}

