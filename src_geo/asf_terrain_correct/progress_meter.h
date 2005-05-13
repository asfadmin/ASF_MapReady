// This class shows a textual progress meter with completion
// percentages.  The output never backtracks and doesn't use anything
// that might be incompatible with any terminal type.

#ifndef PROGRESS_METER_H
#define PROGRESS_METER_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// Instance structure.  This structure should be considered opaque,
// use the methods only.
typedef struct {
  FILE *destination;
  void (*callback) (const char *format, ...);
  size_t total_work;
  size_t work_done;
  size_t fiftieths_printed;
} ProgressMeter;

// Create a new progress meter for a job of size units of work, which
// renders to already open and ready file pointer destination using
// standard fprintf() calls.  It is assumed that no work has been done
// at creation time.
ProgressMeter *
progress_meter_new (FILE *destination, size_t size);

// Create a new progremm meter for a job of size units of work, which
// renders by calling the suppliec callback.  The callback function
// must work like printf, except it must return void instead of a
// counf of the number of characters printed.
ProgressMeter *
progress_meter_new_with_callback (void (*callback) (const char *format, ...),
				  size_t size);

// Advance meter by units_of_work.  Note that this method may not
// result in any new text being displayed (or any other visible change
// on screen) if the amount of work done is too small, however, he
// completed work will be tracked and the visible representation of
// the meter incremented when appropriate.  If an advance finished the
// job, a newline is output after the 100% marker.
void
progress_meter_advance (ProgressMeter *self, size_t units_of_work);

// Free progress meter.
void
progress_meter_free (ProgressMeter *self);

#endif // #ifndef PROGRESS_METER_H
