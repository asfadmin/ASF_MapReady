/****************************************************************
FUNCTION NAME: StartWatch()  & StopWatch()

SYNTAX: void StartWatch();
	void StopWatch();
	void StartWatch(FILE *fLog);
	void StopWatch(FILE *fLog);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
    Keeps track of when a program starts and stops. Stopwatch will print out
    the elapsed time.

RETURN VALUE:
    None.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
     1.0 - Mike Shindle    4/96  Original Creation
****************************************************************/
#include "asf.h"

/*#ifdef __MWERKS__ */
#include <time.h>
	clock_t startTime;

	void StartWatch(void)
	{
	  startTime=clock();
	}

	void StopWatch(void)
	{
	  float elapsed;
	  clock_t stopTime=clock();
	  elapsed = stopTime-startTime;
	  elapsed/=CLOCKS_PER_SEC;
	  printf("Total wall clock time = %f seconds.\n\n",elapsed);
	}

	void StartWatchLog(FILE *fLog)
	{
	  time_t t;
	  char *c;

	  t = time(NULL);
	  c = asctime(localtime(&t));
	  fprintf(fLog, "Date: %s", c);
	}

	void StopWatchLog(FILE *fLog)
	{
	  float elapsed;
	  clock_t stopTime=clock();
	  elapsed = stopTime-startTime;
	  elapsed/=CLOCKS_PER_SEC;
	  fprintf(fLog, "Total wall clock time = %f seconds.\n\n",elapsed);
	}

/*
#else
#include <sys/time.h>

	/ this half for running under old SunOS /
	struct timeval tp1;

	void StartWatch()
	{
	  struct timezone tzp1;

	  gettimeofday(&tp1,&tzp1);
	}

	void StopWatch()
	{
	  long elapsed;
	  struct timeval tp2;
	  struct timezone tzp2;

	  gettimeofday(&tp2,&tzp2);
	  elapsed = tp2.tv_sec - tp1.tv_sec;
	  printf("Total wall clock time = %li seconds.\n\n",elapsed);
	}

#endif
*/


