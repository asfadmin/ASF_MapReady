/*Writer.c:
	The way remap actually writes pixels to its
output image.
*/

#include "asf.h"
#include "Matrix2D.h"
#include "las.h"
#include "remap.h"


/*Write a line of pixels*/
void writePixelLine(FILE *out, struct DDR *outDDR,int y,int bandNo, float *thisLine, char *outBuf)
{
	int x;
	int maxOutX=outDDR->ns;
/*Check for complex pixels:*/
	if (outDDR->dtype>=DTYPE_COMPLEX)
	{
		int outPixelSize=dtype2dsize(outDDR->dtype,NULL);
	/*For imaginary complex, we need to read in the old pixels, sift
		in the new ones, and write them out.  For reals, we just write.*/
		if (outDDR->dtype==DTYPE_COMPLEXIMAG)
		{/*Fetch the  existing pixels...*/
			FSEEK64(out,(long long) maxOutX*outPixelSize*y,0);
			FREAD(outBuf,(unsigned int)outPixelSize,(unsigned int)maxOutX,out);
		}
		switch(outDDR->dtype)
		{
		case DTYPE_COMPLEXREAL: /*Real-Complex*/
			for (x=0;x<maxOutX;x++)
				*(float *)(outBuf+(x*8))=thisLine[x];
			break;
		case DTYPE_COMPLEXIMAG: /*Imag-Complex*/
			for (x=0;x<maxOutX;x++)
				*(float *)(outBuf+(x*8)+4)=thisLine[x];
			break;
		}
		FSEEK64(out,(long long) maxOutX*outPixelSize*y,0);
		FWRITE(outBuf,(unsigned int)outPixelSize,(unsigned int)maxOutX,out);
	} 
	else 
	{
/*Otherwise, they're ordinary pixels, and can be handled easily.*/
		if (maxFlt!=-1.0)
		{/*Must remap amplitude of line to byte.*/
			register float scale=255.0/(maxFlt-minFlt);
			for (x=0;x<maxOutX;x++)
			{
				register float out;
				out=(thisLine[x]-minFlt)*scale;
				if (out<0) out=0;
				if (out>255) out=255;
				thisLine[x]=out;
			}
		}
		putFloatLine_mb(out,outDDR,y,bandNo,thisLine);
	}
	
/*	printf("\tWriting output line %i.\r",y+1);*/
}


