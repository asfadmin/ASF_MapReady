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
#include <assert.h>
#include <time.h>

#include "asf.h"

/*#ifdef __MWERKS__ */

/* Start time to be set and remembered by the Start* routines.  We
   initialize this to a sentinel value meaning which is to be
   interpreted as meaning "Start* hasn't been run yet".  */
static const clock_t start_time_unset_sentinel = (clock_t) -1;
static clock_t startTime = start_time_unset_sentinel;

void StartWatch(void) 
{
  startTime = clock ();
  assert (startTime != (clock_t) -1);
}

void StopWatch(void)
{
  /* The stopwatch must already have been started.  */
  assert (startTime != start_time_unset_sentinel);
  clock_t stopTime=clock();
  assert (stopTime != (clock_t) -1);
  float elapsed = stopTime - startTime;
  elapsed /= CLOCKS_PER_SEC;
  printf ("Total CPU time = %f seconds.\n\n",elapsed);
}

void StartWatchLog(FILE *fLog)
{
  startTime = clock ();
  assert (startTime != (clock_t) -1);
  time_t t = time (NULL);
  char *c = asctime (localtime (&t));
  fprintf (fLog, "Stopwatch started on date: %s", c);
}

void StopWatchLog(FILE *fLog)
{
  assert (startTime != start_time_unset_sentinel);
  clock_t stopTime = clock ();
  assert (stopTime != (clock_t) -1);
  float elapsed = stopTime - startTime;
  elapsed /= CLOCKS_PER_SEC;
  fprintf (fLog, "Total CPU time = %f seconds.\n\n", elapsed);
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


