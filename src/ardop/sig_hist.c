/**********************************************************
Sig_hist:
	Creates a signal-data histogram and calculates the
	I/Q gain imbalance and non-orthogonality.

 Vers    Programmer   Date    Reason
 ----------------------------------------------------------
  1.0    ?. ???       1999    Original Development
  1.1    P. Denny     1/03    Use new metadata, kill DDR
                               Commandline Parsing
                               Somewhat informative Usage
***********************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "ardop_defs.h"
#include "read_signal.h"

#define SIGNAL_LENGTH 1000 /*Program reads a SIGNAL_LENGTH x SIGNAL_LENGTH block of data*/
#define OUTPUT_SIZE 200
#define SIG_HIST_VERSION 1.1

int main(int argc,char **argv)
{
	int startX=0,startY=0;
	int y,ind;
	getRec *r;
	complexFloat *inBuf;
	double sumR,sumI,sumRI,sumRR,sumII;
	double stdR,stdI,cov,corr;
	int n;
	float outBuf[OUTPUT_SIZE*OUTPUT_SIZE];
	meta_parameters *meta = raw_init();
	FILE *outF;

	while (currArg < (argc-6))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1); /*one string argument: log file */
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile,"a");
			logflag=1;
		}
		else if (strmatch(key,"-offset")) {
			CHECK_ARG(2);
			startY = atoi(GET_ARG(2));
			startX = atoi(GET_ARG(1));
		}
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 6) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	r=fillOutGetRec(argv[currArg]);

	inBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*SIGNAL_LENGTH*SIGNAL_LENGTH);
	
/*Read a SIGNAL_LENGTH x SIGNAL_LENGTH block of input complex data.*/
	for (y=0;y<SIGNAL_LENGTH;y++)
		getSignalLine(r,startY+y,&inBuf[y*SIGNAL_LENGTH],startX,SIGNAL_LENGTH);
	
/*Compute histogram image.*/
	for (ind=0;ind<OUTPUT_SIZE*OUTPUT_SIZE;ind++)
		outBuf[ind]=0.0;
	sumR=sumI=sumRI=sumRR=sumII=0.0;
	for (ind=0;ind<SIGNAL_LENGTH*SIGNAL_LENGTH;ind++)
	{
		double r=inBuf[ind].r;
		double i=inBuf[ind].i;
		int x=(int)(r+OUTPUT_SIZE/2);
		int y=(int)(-i+OUTPUT_SIZE/2);
		if ((0<=x)&&(x<OUTPUT_SIZE)&&(0<=y)&&(y<OUTPUT_SIZE))
			outBuf[y*OUTPUT_SIZE+x]+=1.0;
		sumR+=r;
		sumI+=i;
		sumRR+=r*r;
		sumII+=i*i;
		sumRI+=r*i;
	}
	n=SIGNAL_LENGTH*SIGNAL_LENGTH;/*n== number of pixels added into sums*/
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
	meta->general->line_count = OUTPUT_SIZE;
	meta->general->sample_count = OUTPUT_SIZE;
	meta->general->data_type = REAL32;
	meta_write(meta, "sig_hist_img");
	meta_free(meta);
	outF=fopenImage("sig_hist_img","wb");
	for (y=0;y<OUTPUT_SIZE;y++) 
		put_float_line(outF,meta,y,&outBuf[y*OUTPUT_SIZE]);
	FCLOSE(outF);
	
	return 0;
}

void usage (char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <logfile>] [-offset <Line> <Sample>] <ccsd>\n",name);
 printf("\n"
	"REQUIRED ARGUMENT:\n"
	"   ccsd   CCSD image to create histogram for.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log     Copy standard output to <logfile>.\n"
	"   -offset  <line> and <sample> to start collecting\n"
	"              the histogram data at. (default 0x0)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Computes a signal data histogram. Writes a histogram\n"
	"   image based on a %dx%d block of signal data with its\n"
	"   upper left corner at the line and sample specified by\n"
	"   the -offset option.\n", SIGNAL_LENGTH, SIGNAL_LENGTH);
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n", SIG_HIST_VERSION);
 exit(EXIT_FAILURE);
}
