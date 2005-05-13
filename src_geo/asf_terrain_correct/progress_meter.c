// Implementation of the interface described in progress_meter.h.

#include <assert.h>
#include <math.h>

#include "progress_meter.h"

ProgressMeter *
progress_meter_new (FILE *destination, size_t size)
{
  ProgressMeter *self = calloc (sizeof (ProgressMeter), 1);
  assert (self != NULL);

  self->destination = destination;
  self->callback = NULL;
  self->total_work = size;

  self->work_done = 0;
  self->fiftieths_printed = 0;

  fprintf (self->destination, "0%%");
  fflush (self->destination);

  return self;
}

ProgressMeter *
progress_meter_new_with_callback (void (*callback) (const char *format, ...), 
				  size_t size)
{
  ProgressMeter *self = calloc (sizeof (ProgressMeter), 1);
  assert (self != NULL);

  self->destination = NULL;
  self->callback = callback;
  self->total_work = size;

  self->work_done = 0;
  self->fiftieths_printed = 0;

  self->callback ("0%%");

  return self;
}

void
progress_meter_advance (ProgressMeter *self, size_t units_of_work)
{
  // We must not already be done.
  assert (self->work_done < self->total_work);

  // Do some work.
  self->work_done += units_of_work;

  // How many fiftieths of the total work to be done are finished now?
  size_t fiftieths_done_after;
  if ( self->work_done >= self->total_work ) {
    fiftieths_done_after = 50;
  }
  else {
    double fraction_done_after = (double) self->work_done / self->total_work;
    
    fiftieths_done_after = floor (fraction_done_after * 50);
  }

  // Print progress characters of percent done labels.
  size_t ii;
  for ( ii = self->fiftieths_printed ; ii < fiftieths_done_after ; ii++ ) {
    
    if ( (ii + 1) % 5 == 0 ) {
      if ( self->destination != NULL ) {
	fprintf (self->destination, "%d%%", 10 * (ii + 1) / 5);
	fflush (self->destination);
      }
      else {
	self->callback ("%d%%", 10 * (ii + 1) / 5);
      }
    }
    else {
      if ( self->destination != NULL ) {
	fprintf (self->destination, ".");
	fflush (self->destination);
      }
      else {
	self->callback (".");
      }
    }
  }

  // Update our notion of how many fiftieths of the work are finished.
  self->fiftieths_printed += fiftieths_done_after - self->fiftieths_printed;
  assert (self->fiftieths_printed == fiftieths_done_after);

  // If we are done, add a newline.
  if ( self->work_done >= self->total_work ) {
    if ( self->destination != NULL ) {
      fprintf (self->destination, "\n");
      fflush (self->destination);
    }
    else {
      self->callback ("\n");
    }
  }
}
