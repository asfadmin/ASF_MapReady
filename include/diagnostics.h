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
	should give warnings in the same circumstances.  */
     __attribute__ ((format (printf, 1, 2)))
#endif /* __GNUC__ */
; /* <-- Semicolon for prototype.  */


/* Given a condition, a printf()-style message string, and additional
   variadic arguments for the message string, in that order, this
   macro will print message to stderr, and log message prefixed with
   source file and line information to the ASF log file, iff condition
   is false.  For example:

        require (a > b, "a not greater than b: a = %d, b = %d", a, b)

   Sounds complicated, but it does what you want, hopefully.  */
#define require(...) (require_function (__FILE__, __LINE__, __VA_ARGS__))

/* This routine is used by the require macro.  It should never be used
   anywhere else.  */
void
require_function (const char *file, int line, int condition, 
		  const char *message, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 4, 5)))
#endif /* __GNUC__ */
;

/* Given a printf()-style message string and additional variadic
   arguments for the message string, in that order, this macro will
   print message to stderr, and log message prefixed with source file
   and line information to the ASF log file.  For example:

        die ("bad some_value: %d\n", some_value);
*/
#define die(...) (die_function (__FILE__, __LINE__, __VA_ARGS__))

/* This routine is used by the die macro.  It should never be used
   anywhere else.  */
void die_function (const char *file, int line, const char *message, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 3, 4), noreturn))
#endif /* __GNUC__ */
;
