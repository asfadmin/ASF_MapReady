/*Sig_sum:
	Sums signal data in range and azimuth,
writing sums to sum_az and sum_rng

*/

#include "asf.h"

#include "ardop_defs.h"
#include "read_signal.h"

main(int argc,char **argv)
{
#define fftLen 2000

	int startX=0,startY=0;
	int x,y,i;
	getRec *r;
	complexFloat *inBuf;
	complexFloat fftBuf[fftLen];
	float outBuf[fftLen];
	FILE *outF;
	if (argc!=2 && argc!=4)
		{printf("Usage:sig_sum <ccsd> [start X start Y]\n"
		"\n"
		"  Sig_sum computes the range and azimuth\n"
		"power summations of a complex signal data file.\n"
		"These power lists are written to files 'sum_range'\n"
		"and 'sum_az' and are both %d points long.\n"
		"ASF ISAR Tools, 1999\n",fftLen);exit(1);}
	
	r=fillOutGetRec(argv[1]);
	if (argc>3)
	{
		startX=atoi(argv[2]);
		startY=atoi(argv[3]);
	}
	
	inBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*fftLen*fftLen);
	
/*Read a fftLen x fftLen block of input complex data.*/
	for (y=0;y<fftLen;y++)
		getSignalLine(r,startY+y,&inBuf[y*fftLen],startX,fftLen);
	
/*Compute range sums.*/
	for (i=0;i<fftLen;i++) outBuf[i]=0;
	for (y=0;y<fftLen;y++)
	{/*Sum /across/ azimuth*/
		for (x=0;x<fftLen;x++)
			fftBuf[x]=inBuf[y*fftLen+x];
		for (i=0;i<fftLen;i++) 
			outBuf[i]+=fftBuf[i].r*fftBuf[i].r+fftBuf[i].i*fftBuf[i].i;
	}
	outF=FOPEN("sum_range","w");
	for (i=0;i<fftLen;i++) 
		fprintf(outF,"%d %f\n",i,outBuf[i]);
	FCLOSE(outF);

/*Compute azimuth sums.*/
	for (i=0;i<fftLen;i++) outBuf[i]=0;
	for (x=0;x<fftLen;x++)
	{/*Sum /across/ range*/
		for (y=0;y<fftLen;y++)
			fftBuf[y]=inBuf[y*fftLen+x];
		for (i=0;i<fftLen;i++) 
			outBuf[i]+=fftBuf[i].r*fftBuf[i].r+fftBuf[i].i*fftBuf[i].i;
	}
	outF=FOPEN("sum_az","w");
	for (i=0;i<fftLen;i++) 
		fprintf(outF,"%d %f\n",i,outBuf[i]);
	FCLOSE(outF);
	
	return 0;
}

