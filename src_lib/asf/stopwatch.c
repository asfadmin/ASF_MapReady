/* Functions for timing programs.  */

#include <assert.h>
#include <time.h>

#include "asf.h"

/* Start time to be set and remembered by the Start* routines.  We
   initialize this to a sentinel value which is to be interpreted as
   meaning "Start* hasn't been run yet".  */
#define START_TIME_UNSET_SENTINEL -1
static clock_t startTime = (clock_t) START_TIME_UNSET_SENTINEL;

void StartWatch(void) 
{
  startTime = clock ();
  assert (startTime != (clock_t) -1);
}

void StopWatch(void)
{
  /* The stopwatch must already have been started.  */
  assert (startTime != START_TIME_UNSET_SENTINEL);
  clock_t stopTime=clock();
  assert (stopTime != (clock_t) -1);
  float elapsed = stopTime - startTime;
  elapsed /= CLOCKS_PER_SEC;
  printf ("Total CPU time = %f seconds.\n\n", elapsed);
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
  assert (startTime != START_TIME_UNSET_SENTINEL);
  clock_t stopTime = clock ();
  assert (stopTime != (clock_t) -1);
  float elapsed = stopTime - startTime;
  elapsed /= CLOCKS_PER_SEC;
  fprintf (fLog, "Total CPU time = %f seconds.\n\n", elapsed);
}


