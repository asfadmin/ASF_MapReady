/*
Specan Processor implementation file.

These routines run on both master and slave,
and do SAR processing on a single patch of 
data in azimuth.  It does so by calling
specan_process (in specan.c) in both range
and azimuth, and averaging the resulting 
patchlets using specan_ml.c.

Orion Lawlor, ASF 12/98.
*/
#include "asf.h"
#include "aisp_defs.h"
#include "specan.h"
#include "specan_ml.h"

/*Performs final initializations on a specan_patch structure*/
void init_patch(specan_patch *s)
{
	int i;
	for (i=0;i<s->oHt;i++)
	{
		double x=pi*(i-s->az.oCenter)/s->az.fftLen;
		double sinc;
		if (x!=0) sinc=sin(x)/x; else sinc=1;
		s->scallop[i]=1.0/pow(sinc,2.0);/*Sinc^2 amplitude scale
			because same antenna is used to transmit *and* recieve*/
	}
}
void read_patch(specan_patch *s,getRec *inFile,FCMPLX *in,int patchNo)
{
	int y,inLine;
	
	inLine=patchNo*s->az.iNum+(-s->az.iFirst);
	printf("\nReading patchlet %d, which starts at line %d\n",patchNo,inLine);
	for (y=0;y<s->iHt;y++)
		getSignalLine(inFile,inLine+y,&(in[y*s->iWid]),0,s->iWid);
}

/*Process a single, already read-in patch
of data using specan.
in[] is iWid x iHt
amp_out[] is oWid x oHt*/
void specan_process_patch(specan_patch *s,FCMPLX *in,float *amp_out)
{
	FCMPLX *out,*rng_out,*az_out;
	int patchlet,y,x;
	int azFFT=s->az.fftLen;
	int rngO=s->rng.oNum;
	int inputOffset=(-s->rng.iFirst)+s->rng.fftLen;
	
	specan_ml *ml=specan_ml_init(s->oWid,s->oHt);

	out=(FCMPLX *)malloc(sizeof(FCMPLX)*rngO*azFFT);
	rng_out=(FCMPLX *)malloc(sizeof(FCMPLX)*s->rng.fftLen);
	az_out=(FCMPLX *)malloc(sizeof(FCMPLX)*s->az.fftLen);
	
	/*Process one patchlet at a time, adding each to the
	  Multilooking buffer*/
	patchlet=(int)ceil(s->rng.iFirst/s->inSeq);/*Start a ways back from the image beginning*/
	while (patchlet*s->inSeq+inputOffset<s->iWid)
	{
		int destX=patchlet*s->outSeq;
		
		/*Read and compress patchlet in range, one line at a time*/
		for (y=0;y<azFFT;y++)
		{
			int inSamp=(int)(patchlet*s->inSeq+(-s->rng.iFirst));
			/*int outSamp=s->oWid*y;*/
			
			if ((inSamp<0)||(inSamp+s->rng.fftLen>=s->iWid))
				printf("**** Out of bounds: %d\n",inSamp);
			
			specan_process(&s->rng,&(in[y*s->iWid+inSamp]),rng_out);
			for (x=0;x<rngO;x++)
				out[azFFT*x+y]=rng_out[x];
		}
		
		/*Compress patchlet in azimuth, adding it to multilooking buffer*/
		for (x=0;x<rngO;x++)
		{
			specan_process(&s->az,&(out[azFFT*x]),az_out);
			specan_ml_look(ml,destX+x,az_out);
			/*if ((destX+x)<s->oWid)
			for (y=0;y<s->az.oNum;y++)
				amp_out[y*s->oWid+destX+x]=Cabs(az_out[y]);*/
		}
		patchlet+=1;/*s->nLooks/2;*/
	}
	specan_ml_out(ml,s->scallop,amp_out);
	specan_ml_free(ml);
	
	free(out);
	free(rng_out);
	free(az_out);
#if 0
/*Old, pre-multilooking code here for reference.*/
	for (y=0;y<s->az.fftLen;y++)
	{
		/*Compress in range.*/
		for (xPatch=0;xPatch<s->nxPatch;xPatch++)
		{
			int inSamp=xPatch*s->rng.iNum+(-s->rng.iFirst);
			int outSamp=s->oWid*y+xPatch*s->rng.oNum;
			specan_process(&s->rng,&(in[y*iWid+inSamp]),&(out[outSamp]));
		}
	}
		
	/*Azimuth compress down the lines.*/
	for (x=0;x<oWid;x++)
	{
		register double len=s->az.fftLen,num=s->oHt;
		for (y=0;y<len;y++)
			az_in[y]=out[oWid*y+x];
		specan_process(&s->az,az_in,az_out);
		for (y=0;y<num;y++)
			amp_out[oWid*y+x]=s->scallop[y]*Cabs(az_out[y]);
	}
#endif
}

void write_patch(specan_patch *s,float *amp_out,FILE *outFile)
{
	int y;
	printf("\tWriting out patch...\n");
	for (y=0;y<s->oHt;y++)
		fwrite(&(amp_out[y*s->oWid]),sizeof(float),s->oWid,outFile);
}

