// This is just a wrapper for meta_read that protects the whole thing
// with a mutex.  This is useful when calling meta_read from
// multithreaded code, since meta_read calls lex and yacc code which
// renders it unexpectedly totally non-reentrant.

#include <asf_meta.h>

meta_parameters *
meta_read_wrapper (const char *inName);
