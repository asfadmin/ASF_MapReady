#include "asf.h"



#include "aisp_defs.h"
#include "read_signal.h"

main(int argc,char **argv)
{
#define mlX 8
#define mlY 16
	int x,outX,y,outY,mlLines,mlSamples;
	getRec *r;
	complexFloat *inBuf;
	FILE *outF;
	float *outBuf;
	if (argc!=3)
		{printf("Usage:signal2amp <ccsd> <output>\n");exit(1);}
	r=fillOutGetRec(argv[1]);
	mlLines=r->nLines/mlY;
	mlSamples=r->nSamples/mlX;
	printf("Creating a %d line by %d sample output file.\n",mlLines,mlSamples);
	inBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*r->nSamples);
	outBuf=(float *)MALLOC(sizeof(float)*(mlSamples+10));
	outF=fopenImage(argv[2],"wb");
	for (outY=0;outY<mlLines;outY++)
	{
		for (y=0;y<mlY;y++)
		{
			getSignalLine(r,outY*mlY+y,inBuf,0,r->nSamples);
			for (outX=0;outX<mlSamples;outX++)
			{
				outBuf[outX]=0;
				for (x=0;x<mlY;x++)
					outBuf[outX]+=Cabs(inBuf[outX*mlX+x]);
			}
		}
		if (!quietflag && (outY%1024 == 0)) printf("Writing line %d\n",outY);
		fwrite(outBuf,sizeof(float),mlSamples,outF);
	}
	return 0;
}

