/*
	Mapping.c: the guts of remap.
	
	The main entry point is perform_mapping (about halfway down).  It is responsible for all the
actual image processing, spec'd by the input and output DDRs, and its parameters "map" and "samp".

5/97, ASF ISAR Tools.  By Orion Lawlor.
*/

#include "asf.h"
#include "las.h"
#include "Matrix2D.h"
#include "remap.h"

/****************************Perform_mapping-- the most important call********************
  This is the function which does the file I/O and image manipulation.
  There are a few caveats:
    If complex-valued input is specified, the output DDR must also have a dtype of
      either DTYPE_COMPLEXREAL or DTYPE_COMPLEXIMAG.
      If either of these options are specified, the output file will be
      created in a bizarre way-- only the correct fields of the input image
      will be read in, and only those same fields will be written out.
    Hence, to generate a correct Complex image, you have to call perform_mapping twice--
      once to remap the real fields, and again to remap the imaginary fields.
*/
void perform_mapping(FILE *in, struct DDR *inDDR, FILE *out, struct DDR *outDDR,
	mappingFunction map, sampleFunction samp,int bandNo)
{
	int x,y,maxOutX=outDDR->ns,maxOutY=outDDR->nl;
	float *thisLine=(float *)MALLOC(maxOutX*sizeof(float));
	char *outBuf;
	
	char *pixelDescription;
	pixelFetcher *getRec=createFetchRec(in,inDDR,outDDR->dtype,bandNo);
	int outPixelSize;
	
	outPixelSize=dtype2dsize(outDDR->dtype,&pixelDescription);
	outBuf=(char *)MALLOC((unsigned int)(maxOutX*outPixelSize));
	printf("   Output Pixel type/size: %s",pixelDescription);	
	printf("   Sampling Type: %s\n",samp->description);
	if (logflag) {
	  sprintf(logbuf,"   Output Pixel type/size: %s",pixelDescription);
	  printLog(logbuf);	
	  sprintf(logbuf,"   Sampling Type: %s\n",samp->description);
	  printLog(logbuf);
	}
	
/*Iterate over each pixel of the output image.*/
	for (y=0;y<maxOutY;y++)
	{
		fPoint outPt,inPt;
		outPt.y=y;
		for (x=0;x<maxOutX;x++)
		{
			outPt.x=x;
		   /*Compute the pixel's position in input space...*/
		   	map->doMap((void *)map,outPt,&inPt);
			
		   /*Now read that pixel's value and put it in the "thisLine" array as a float...*/
		   	thisLine[x]=samp->doSamp((void *)samp,getRec,inPt);
		}
		
		writePixelLine(out,outDDR,y,bandNo,thisLine,outBuf);
	}
	
/*	printf("\n\tperform_mapping complete!\n");*/
	killFetchRec(getRec);
	
	FREE(thisLine);
	FREE(outBuf);
}


