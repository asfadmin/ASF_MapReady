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
  clock_t stopTime;
  float elapsed;

  /* The stopwatch must already have been started.  */
  assert (startTime != START_TIME_UNSET_SENTINEL);
  stopTime=clock();
  assert (stopTime != (clock_t) -1);
  elapsed = stopTime - startTime;
  elapsed /= CLOCKS_PER_SEC;
  printf ("Total CPU time = %f seconds.\n\n", elapsed);
}

void StartWatchLog(FILE *fLog)
{
  char *c;
  time_t t;
  startTime = clock ();
  assert (startTime != (clock_t) -1);
  t = time (NULL);
  c = asctime (localtime (&t));
  fprintf (fLog, "Stopwatch started on date: %s", c);
}

void StopWatchLog(FILE *fLog)
{
  clock_t stopTime;
  float elapsed;
  assert (startTime != START_TIME_UNSET_SENTINEL);
  stopTime = clock ();
  assert (stopTime != (clock_t) -1);
  elapsed = stopTime - startTime;
  elapsed /= CLOCKS_PER_SEC;
  fprintf (fLog, "Total CPU time = %f seconds.\n\n", elapsed);
}

char* date_time_stamp(void)
{
  time_t t;
  char *t_stamp;

  t_stamp = (char*) MALLOC(25*sizeof(char));
  t = time(NULL);
  strftime(t_stamp, 22, "%d-%b-%Y, %H:%M:%S", localtime(&t));

  return t_stamp;
}

char* date_stamp(void)
{
  time_t t;
  char *t_stamp;

  t_stamp = (char*) MALLOC(25*sizeof(char));
  t = time(NULL);
  strftime(t_stamp, 12, "%d-%b-%Y", localtime(&t));

  return t_stamp;
}

