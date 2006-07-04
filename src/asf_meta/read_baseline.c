#include "asf.h"
#include "asf_meta.h"


baseline read_baseline(char *fName)
{
	baseline b;
	FILE *fbase;
	fbase = FOPEN(fName,"r");

	if (5!=fscanf(fbase, "%lf %lf %lf %lf %lf", &b.Bn, &b.dBn, &b.Bp, &b.dBp,
	              &b.temporal))
	{
		sprintf(errbuf,"   ERROR: Couldn't read five baseline components\n"
			"         from baseline file named '%s'!\n",fName);
		printErr(errbuf);
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
