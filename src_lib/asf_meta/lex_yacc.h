/* Macros that get definded in the yacc (.y) file aren't visible in
   the %defines header it writes out for lex, so the common macros go
   here.  */

#ifndef __LEX_YACC__
# define __LEX_YACC__

/* Maximum lengths of some character strings, not including
   terminating '\0' characters.  */
#define MAX_ERROR_STRING 1000
#define MAX_SYMBOL_STRING 1000
 
#endif /* not __LEX_YACC__ */
