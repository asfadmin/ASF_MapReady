/* testprogram generates a rep file named sinus.rep with an sinus
* with 700 samples and f = 5Mhz (f sample = 18.96 Mhz);
*/
#include <stdio.h>
#include <math.h>
#include "drdapi.h"

#define SAMPLES		1020
#define FREQUENCY	5.3e9
#define MIXFREQ		5.2232e9
#define SAMPLEFREQ	18.96e6
#define CHIRP		15.5e6
#define PI		3.14159
#define NUMBER		3

main()
{
DRDFILE *rep,*aux;
FILE *file;
SCcomplex mySin[SAMPLES];
register int i,n;
double arg,arg1,fn,fI,fQ;

drdInit("sinus1",1,0,"HJWAGNER");
/* generating sin */
file = fopen("sinus","w");
for (i = 0; i < SAMPLES; i++) {
	arg = 2.0 * PI * (FREQUENCY - MIXFREQ) / SAMPLEFREQ * (double)i;
	mySin[i].I = (char)(127.5 * sin(arg) - 0.5);
	fprintf(file,"%d\t%d\n",i,(int)mySin[i].I);
	mySin[i].Q = (char)(127.5 * (cos(arg)) - 0.5);
	}
fclose(file);
rep = repCreate("sinus","hihihi","NoSat",NULL);
repWriteScan(rep,100,mySin,SAMPLES * 2);
repClose(rep);

aux = auxCreate("sinus","hihihi","NoSat",NULL);
auxWriteSamplingRate(aux,NULL,100,(ulong)(SAMPLEFREQ / 1000) );
auxWriteRadarFrequency(aux,100L,(ulong)(FREQUENCY / 1000) );
auxWriteMixingFrequencyShift(aux,NULL,100L,(ulong)(MIXFREQ / 1000.0) );
auxClose(aux);

file = fopen("2sinus","w");
for (i = 0; i < SAMPLES; i++) {
    fI = fQ = 0.0;
    for(n = 0; n < NUMBER; n++) {
	fn = (FREQUENCY - MIXFREQ) + ((double)(n-NUMBER/2)) * CHIRP/NUMBER;
	if (0 == i)
	    printf("f(%d) = %f Magnitude %f\n",n,fn,((double)(n+1)) * 20.0);
        arg = 2.0*PI* fn / SAMPLEFREQ *(double)i;
	fI += ((double)(n+1)) * 20.0 * sin(arg);
	fQ += ((double)(n+1)) * 20.0 * cos(arg);
	}
    mySin[i].I = fI;
    mySin[i].Q = fQ;
    fprintf(file,"%d\t%d\t%d\n",i,(int)mySin[i].I,(int)mySin[i].Q);
    }
fclose(file);
rep = repCreate("2sinus","hihihi","NoSat",NULL);
repWriteScan(rep,100,mySin,SAMPLES * 2);
repClose(rep);
 
aux = auxCreate("2sinus","hihihi","NoSat",NULL);
auxWriteSamplingRate(aux,NULL,100,(ulong)(SAMPLEFREQ / 1000) );
auxWriteRadarFrequency(aux,100L,(ulong)(FREQUENCY / 1000) );
auxWriteMixingFrequencyShift(aux,NULL,100L,(ulong)(MIXFREQ / 1000.0) );
auxClose(aux);

/* and now make a fm */
file = fopen("fm","w");
for (i = 0; i < SAMPLES; i++) {
	fn = FREQUENCY - MIXFREQ - CHIRP/2.0 + CHIRP/2 * i/SAMPLES; 
	arg = 2.0 * PI * fn / SAMPLEFREQ * ((double)i - fn/SAMPLEFREQ);
	mySin[i].I = (char)(127.5 * sin(arg) - 0.5);
	fprintf(file,"%d\t%d\n",i,(int)mySin[i].I);
	mySin[i].Q = (char)(127.5 * cos(arg) - 0.5);
	}
fclose(file);
rep = repCreate("fm","hihihi","NoSat",NULL);
repWriteScan(rep,100,mySin,SAMPLES * 2);
repClose(rep);
 
aux = auxCreate("fm","hihihi","NoSat",NULL);
auxWriteSamplingRate(aux,NULL,100,(ulong)(SAMPLEFREQ / 1000) );
auxWriteRadarFrequency(aux,100L,(ulong)(FREQUENCY / 1000) );
auxWriteMixingFrequencyShift(aux,NULL,100L,(ulong)(MIXFREQ / 1000.0) );
auxClose(aux);

printf("byebye\n");

}

