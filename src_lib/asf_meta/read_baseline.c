#include "asf.h"
#include "asf_meta.h"

extern int logflag;
extern char logbuf[255];
extern char errbuf[255];


baseline read_baseline(char *fName)
{
	baseline b;
	FILE *fbase;
	fbase = FOPEN(fName,"r");
/*	printf("Reading in baseline values from '%s'...\n",fName);*/
	if (4!=fscanf(fbase, "%lf %lf %lf %lf", &b.Bn, &b.dBn,
									 &b.Bp, &b.dBp))
	{
		sprintf(errbuf,"   ERROR: Couldn't read 4 baseline components\n"
			"from baseline file named '%s'!\n",fName);
		printErr(errbuf);
	}
	if (1!=fscanf(fbase,"%lf",&b.temporal))
	{
		printf("   Assuming 1-day temporal baseline...\n");
		b.temporal=1.0;
	}
	fclose(fbase);
	printf("   Baseline:   Normal: %f, delta: %f\n",b.Bn,b.dBn);
	printf("               Parallel: %f, delta: %f\n",b.Bp,b.dBp);
	printf("               Temporal: %f days\n\n",b.temporal);
	if (logflag) {
	  sprintf(logbuf,"   Baseline:   Normal: %f, delta: %f\n",b.Bn,b.dBn);
	  printLog(logbuf);
	  sprintf(logbuf,"               Parallel: %f, delta: %f\n",b.Bp,b.dBp);
	  printLog(logbuf);
	  sprintf(logbuf,"               Temporal: %f days\n\n",b.temporal);
	  printLog(logbuf);
	}
	return b;
}
