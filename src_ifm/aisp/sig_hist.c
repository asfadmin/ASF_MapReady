/*Sig_hist:
	Creates a signal-data histogram 
and calculates the I/Q gain imbalance and
non-orthogonality.
*/

#include "asf.h"
#include "ddr.h"
#include "aisp_defs.h"
#include "read_signal.h"

main(int argc,char **argv)
{
#define sigLen 1000 /*Program reads a sigLen x sigLen block of data*/
#define outSize 200

	int startX=0,startY=0;
	int x,y,ind;
	getRec *r;
	FCMPLX *inBuf;
	FCMPLX fftBuf[sigLen];
	double sumR,sumI,sumRI,sumRR,sumII;
	double stdR,stdI,cov,corr;
	int n;
	float outBuf[outSize*outSize];
	struct DDR ddr;/*Output DDR*/
	FILE *outF;
	if (argc!=2 && argc!=4)
		{printf("Usage:sig_hist <ccsd> [start X start Y]\n"
		"\n"
		"  Sig_hist computes a signal data histogram.\n"
		"ASF ISAR Tools, 1999\n",sigLen);exit(1);}
	
	r=fillOutGetRec(argv[1]);
	if (argc>3)
	{
		startX=atoi(argv[2]);
		startY=atoi(argv[3]);
	}
	
	inBuf=(FCMPLX *)MALLOC(sizeof(FCMPLX)*sigLen*sigLen);
	
/*Read a sigLen x sigLen block of input complex data.*/
	for (y=0;y<sigLen;y++)
		getSignalLine(r,startY+y,&inBuf[y*sigLen],startX,sigLen);
	
/*Compute histogram image.*/
	for (ind=0;ind<outSize*outSize;ind++)
		outBuf[ind]=0.0;
	sumR=sumI=sumRI=sumRR=sumII=0.0;
	for (ind=0;ind<sigLen*sigLen;ind++)
	{
		double r=inBuf[ind].r;
		double i=inBuf[ind].i;
		int x=(int)(r+outSize/2);
		int y=(int)(-i+outSize/2);
		if ((0<=x)&&(x<outSize)&&(0<=y)&&(y<outSize))
			outBuf[y*outSize+x]+=1.0;
		sumR+=r;
		sumI+=i;
		sumRR+=r*r;
		sumII+=i*i;
		sumRI+=r*i;
	}
	n=sigLen*sigLen;/*n== number of pixels added into sums*/
	/*Compute standard deviations*/
	stdR=sqrt((sumRR-sumR*sumR/n)/(n-1));
	stdI=sqrt((sumII-sumI*sumI/n)/(n-1));
	/*Compute covariance*/
	cov=(sumRI-sumR*sumI/n)/(n-1);
	/*Correlation is covariance over standard deviations*/
	corr=cov/(stdR*stdI);
	printf( "   Mean_R= %f Mean_I= %f \n"
		"   Mean R*R=%f; Mean I*I=%f\n"
		"   Std. Dev. R=%f; Std. Dev. I=%f\n"
		"   Mean R*I=%f; non-orthogonality correction=%f\n"
		"   Correlation coefficient=%f\n",
		sumR/n,sumI/n,sumRR/n,sumII/n,
		stdR,stdI,
		sumRI/n,sumRI/sumRR,corr);
	if (logflag) {
	  sprintf(logbuf, "   Mean_R= %f Mean_I= %f \n"
			  "   Mean R*R=%f; Mean I*I=%f\n"
			  "   Std. Dev. R=%f; Std. Dev. I=%f\n"
			  "   Mean R*I=%f; non-orthogonality correction=%f\n"
			  "   Correlation coefficient=%f\n",
			  sumR/n,sumI/n,sumRR/n,sumII/n,
		 	  stdR,stdI,
			  sumRI/n,sumRI/sumRR,corr);
	  printLog(logbuf);
	}

/*Write out histogram image.*/
	c_intddr(&ddr);
	ddr.nl=ddr.ns=outSize;
	ddr.dtype=4;
	ddr.nbands=1;
	c_putddr("sig_hist_img",&ddr);
	outF=fopenImage("sig_hist_img","wb");
	for (y=0;y<outSize;y++) 
		putFloatLine(outF,&ddr,y,&outBuf[y*outSize]);
	FCLOSE(outF);
	
	return 0;
}

