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
    the elapsed CPU time.

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
	  if ( startTime == (clock_t) -1 ) {
	    bail ("clock() returned (clock_t) -1 in file " __FILE__ 
		  ", line number %d\n", __LINE__ - 2);
	  }
	}

	void StopWatch(void)
	{
	  float elapsed;
	  clock_t stopTime=clock();
	  if ( stopTime == (clock_t) -1 ) {
	    bail ("clock() returned (clock_t) -1 in file " __FILE__ 
		  ", line number %d\n", __LINE__ - 2);
	  }
	  elapsed = stopTime-startTime;
	  elapsed /= CLOCKS_PER_SEC;
	  printf("Total system clock time = %f seconds.\n\n",elapsed);
	}

	void StartWatchLog(FILE *fLog)
	{
	  time_t t;
	  char *c;

	  startTime=clock();
	  if ( startTime == (clock_t) -1 ) {
	    bail ("clock() returned (clock_t) -1 in file " __FILE__ 
		  ", line number %d\n", __LINE__ - 2);
	  }

	  t = time(NULL);
	  c = asctime(localtime(&t));
	  fprintf(fLog, "Stopwatch started on date: %s", c);
	}

	void StopWatchLog(FILE *fLog)
	{
	  float elapsed;
	  clock_t stopTime=clock();
	  if ( stopTime == (clock_t) -1 ) {
	    bail ("clock() returned (clock_t) -1 in file " __FILE__ 
		  ", line number %d\n", __LINE__ - 2);
	  }
	  elapsed = stopTime-startTime;
	  elapsed /= CLOCKS_PER_SEC;
	  fprintf(fLog, "Total CPU clock time = %f seconds.\n\n",elapsed);
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


