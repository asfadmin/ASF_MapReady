/* Implementation of the interface in diagnostics.h.  */

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "diagnostics.h"

static char *elfn = NULL;	/* Expanded log file name.  */
static FILE *asf_log_file;	/* Log file pointer.  */

/* Determine the $HOME-expanded asf log file name and open it, if this
   hasn't already been done.  */
static void initialize_asf_log_if_needed (void) { 
  /* Load expanded log file name on the first time through.  */
  if ( elfn == NULL ) {
    char *home_ptr;
    char *home_string;
    size_t home_length;
    size_t home_index;
    char *post_home_string;

    elfn = malloc ((strlen (ASF_LOG_FILE) + 1) *  sizeof (char));
    strcpy (elfn, ASF_LOG_FILE);
    /* Expand $HOME in the log file name, if it is present.  */
    home_ptr = strstr (elfn, "$HOME");
    if ( home_ptr != NULL ) {
      home_string = getenv ("HOME");
      assert (home_string != NULL);
      home_length = strlen (home_string);
      home_index = home_ptr - elfn;
      if ( home_length > strlen ("$HOME") ) {
	elfn = realloc (elfn, strlen (ASF_LOG_FILE) - strlen ("$HOME") 
			+ home_length + 1);
      }
      home_ptr = elfn + home_index;
      post_home_string = malloc (strlen (home_ptr) - strlen ("$HOME")
				       + 1);
      strcpy (post_home_string, home_ptr + strlen ("$HOME"));
      *home_ptr = '\0';		/* Snip everything from $HOME onwards.  */
      strcat (elfn, home_string);
      strcat (elfn, post_home_string);
      free (post_home_string);
    }
  }

  /* Open the log file for appending, if it isn't already open.  */
  if ( asf_log_file == NULL ) {
    /* FIXME: we don't yet bound the log size at all.  */
    asf_log_file = fopen (elfn, "a");
    if ( asf_log_file == NULL ) {
#ifdef _GNU_SOURCE
      /* If we have the reentrant version of strerror, use it,  */
      const size_t max_error_string_length = 2000;
      char *error_string 
	= malloc ((max_error_string_length + 1) * sizeof (char));
      int return_code = strerror_r (errno, error_string, 
				    max_error_string_length);
      assert (return_code == 0);
      fprintf (stderr, "couldn't open log file %s: %s at source file %s line "
	       "%d\n", elfn, error_string, __FILE__, __LINE__);
#else
      /* otherwise, take a deep breath and try strerror.  Note that at
	 some point strerror_r will presumably make it into a
	 standard, at which time this #else brance should be removed
	 in favor of the above code.  */
      fprintf (stderr, "couldn't open log file %s: %s at source file %s line "
	       "%d\n", elfn, strerror (errno), __FILE__, __LINE__);
#endif /* _GNU_SOURCE */
      exit (EXIT_FAILURE);
    }
  }
}  

void
asf_log (const char *message, ...)
{
  initialize_asf_log_if_needed ();

  /* Write printf()-style arguments to the log.  */
  va_list ap;			/* Argument pointer.  */
  va_start (ap, message);
  vfprintf (asf_log_file, message, ap);
  va_end (ap);
}

void 
require_function (const char *file, int line, int condition, 
		  const char *message, ...)
{
  if ( !condition ) {
    initialize_asf_log_if_needed();

    va_list ap;			/* Variadic arguments pointer.  */

    /* Print to log file, prefixing source file and line information.  */
    fprintf (asf_log_file, "At source file %s line %d: ", file, line);
    va_start (ap, message);
    vfprintf (asf_log_file, message, ap);
    va_end (ap);

    /* Print to standard error.  */
    va_start (ap, message);
    vfprintf (stderr, message, ap);
    va_end (ap);

    exit (EXIT_FAILURE);
  }
}

void
die_function (const char *file, int line, const char *message, ...)
{
  va_list ap;			/* Variadic arguments pointer.  */
 
  initialize_asf_log_if_needed();
 
  /* Print to log file, prefixing source file and line information.  */
  fprintf (asf_log_file, "At source file %s line %d: ", file, line);
  va_start (ap, message);
  vfprintf (asf_log_file, message, ap);
  va_end (ap);
  
  /* Print to standard error.  */
  va_start (ap, message);
  vfprintf (stderr, message, ap);
  va_end (ap);

  exit (EXIT_FAILURE);
}
