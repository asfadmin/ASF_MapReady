/* Functions for logging, error printing, and assertion checking.
   These routines provide a more functionality than plain assert,
   while not being nearly as complicated as the ones in glib.
   However, In cases where full blown upward error propagation is
   required, the GError mechanism from glib should be used.  */

/* Default location for log messages.  This may be overridden by the
   build system for non-unix operating systems.  The string '$HOME' is
   specially handled: it is expanded to the HOME environment variable.
   There is no gaurantee that general environment variables or shell
   code will work here, however.*/
#define ASF_LOG_FILE "$HOME/.asf_tools_log"

/* The log file is gauranteed to remember at least this many bytes
   worth of log data (assuming this many bytes worth of data have ever
   been logged).  */
#define ASF_LOG_FILE_MIN_SIZE 500000

/* Log a message to the ASF_LOG_FILE.  A printf()-style message and
   variadic arguments may be used.  Include a trailing newline if you
   want one.  */
void
asf_log (const char *message, ...) /* The ';' is coming, don't worry.  */
#ifdef __GNUC__
     /* Hint compiler that function is variadic a la printf, and
	should give warning s in the same circumstances.  */
     __attribute__ ((format (printf, 1, 2)))
#endif /* __GNUC__ */
; /* <-- Semicolon for prototype.  */

/* Require that condition be true.  If it isn't, log and print message
   together with file and line information, and exit with non-zero
   exit status.  It is expected that __FILE__ and __LINE will be used
   for the file and line arguments to this routine.  The message
   argument works like printf, looking for variadic arguments after
   the line argument.  The file and line information and a trailing
   newline are automaticly appended to the formatted message.  */
void
require (int condition, const char *message, const char *file, int line, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 2, 5)))
#endif /* __GNUC__ */
;

/* Die with message, printing and logging the error to have been found
   at file, line.  It is expected that __FILE__ and __LINE__ will be
   used for the file and line arguments to this routine.  The message
   argument works like printf, looking for variadic arguments after
   the line argument.  The file and line information and a trailing
   newline are automaticly appended to the formatted message.  */
void die (const char *message, const char *file, int line, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 1, 4), noreturn))
#endif /* __GNUC__ */
;
