/****************************************************************
FUNCTION NAME:  estdop - estimates the doppler centroid in range

SYNTAX: estdop(file, nlines, doppler)

PARAMETERS:
    NAME:     TYPE:     PURPOSE:
    --------------------------------------------------------
    file      char[]    input CCSD file name
    nlines    int       number of lines to use in average estimate
    doppler   float *   return constant average doppler value (in % of PRF)

DESCRIPTION:
    Calculates the doppler centroid in % of PRF by estimating from
    the raw signal data.

RETURN VALUE:

SPECIAL CONSIDERATIONS:
    This program actually calculates the doppler as a constant,
    a line, or a quadratic.  It returns the quadratic doppler.

PROGRAM HISTORY:
  1.1  Orion Lawlor-- 1/97  read from signal-data reading routines.
  2.0  Orion Lawlor-- 5/97  unwrap doppler phase

****************************************************************/
#include "asf.h"
#include "aisp_defs.h"
#include "read_signal.h"
#include "estdop.h"

void estdop(char file[],int nDopLines, float *a, float *b,float *c)
{
#define MULTILOOK 100
	FCMPLX        *signal, *signalNext, *dop;
	float         *x_vec, *y_vec,*phase;
	int           x,line,firstLine;
	getRec *r;
	float         t1, t2, t3;
	long double sum;
	float lastPhase;

	r=fillOutGetRec(file);
	firstLine=(r->nLines-nDopLines)/2;
	
	signal=(FCMPLX *)MALLOC(sizeof(FCMPLX)*r->nSamples);
	signalNext=(FCMPLX *)MALLOC(sizeof(FCMPLX)*r->nSamples);
	dop=(FCMPLX *)MALLOC(sizeof(FCMPLX)*r->nSamples);
	x_vec=(float *)MALLOC(sizeof(float)*r->nSamples);
	y_vec=(float *)MALLOC(sizeof(float)*r->nSamples);
	phase=(float *)MALLOC(sizeof(float)*r->nSamples);

	for (x = 0; x<r->nSamples; x++) 
		dop[x] = Czero();
	
	getSignalLine(r,firstLine,signalNext,0,r->nSamples);
	for (line = firstLine+1; line < firstLine+nDopLines; line++)
	{
		FCMPLX *ptr = signal; signal = signalNext; signalNext = ptr;
		getSignalLine(r,line,signalNext,0,r->nSamples);
		for (x = 0; x<r->nSamples; x++) 
			dop[x] = Cadd(dop[x],Cmul(signalNext[x],Cconj(signal[x])));
	}
	
	/*Multilook the phase data along range.*/
	for (x=0;x<r->nSamples/MULTILOOK;x++)
	{
		int i;
		double out_r=0.0,out_i=0.0;
		for (i=0;i<MULTILOOK;i++)
		{
			out_r+=dop[x*MULTILOOK+i].r;
			out_i+=dop[x*MULTILOOK+i].i;
		}
		dop[x].r=out_r;
		dop[x].i=out_i;
	}
	
	/*Phase-unwrap the doppler values into the "phase" array.*/
	lastPhase=0;
	for (x=0;x<r->nSamples/MULTILOOK;x++)
	{
		float nextPhase=atan2(dop[x].i, dop[x].r)*(1.0/(2.0*pi));
		while ((nextPhase-lastPhase)<-0.5) nextPhase+=1.0;
		while ((nextPhase-lastPhase)>0.5) nextPhase-=1.0;
		phase[x]=nextPhase;
		lastPhase=nextPhase;
	}
	
	sum = 0.0;

	for (x = 0; x<r->nSamples/MULTILOOK; x++) 
	{
	  	sum += phase[x]; 
	  	x_vec[x] = x*MULTILOOK; 
	  	y_vec[x] = phase[x];
	}

	if (1) /*Output doppler vs. range*/
	{
		FILE *f=FOPEN("dop_vs_rng","w");
		for (x=0;x<r->nSamples/MULTILOOK;x++)
			fprintf(f,"%.0f\t%f\n",x_vec[x],y_vec[x]);
		fclose(f);
	}

	sum = sum / (r->nSamples/MULTILOOK);
	printf("   Constant Average    : y = %f\n",(float)sum);
	if (logflag) {
	  sprintf(logbuf,"   Constant Average    : y = %f\n",(float)sum);
	  printLog(logbuf);
	}
	yaxb(x_vec, y_vec, r->nSamples/MULTILOOK, &t1, &t2);
	printf("   Linear Regression   : y = %f x + %f\n",t1,t2);
	if (logflag) {
	  sprintf(logbuf,"   Linear Regression   : y = %f x + %f\n",t1,t2);
	  printLog(logbuf);
	}
	yax2bxc(x_vec, y_vec, r->nSamples/MULTILOOK, &t1, &t2, &t3);
	printf("   Quadratic Regression: y = %f x^2 + %f x + %f\n",t1,t2,t3);
	if (logflag) {
	  sprintf(logbuf,"   Quadratic Regression: y = %f x^2 + %f x + %f\n",t1,t2,t3);
	  printLog(logbuf);
	}
	*a = t3; *b = t2; *c = t1;
	
	free((void *)signal);
	free((void *)signalNext);
	free((void *)dop);
	free((void *)x_vec);
	free((void *)y_vec);
	free((void *)phase);
	freeGetRec(r);

	return;
 }
