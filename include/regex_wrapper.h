/* Easy but machine inefficient interface to libc regex facilities.  

   Example:

        {
          matched_subexps_t my_subexps = MATCHED_SUBEXPS_INITIALIZER;

          if ( regex_match(&my_subexps, "what now?", "(what)\\.*(ow)") )
            printf("%s %s? %s!\n", get_subexp_string(&my_subexps, 0),
                   get_subexp_string(&my_subexps, 1), 
	           get_subexp_string(&my_subexps, 2));
          }

          matched_subexps_free(&my_subexps);
        }

   will very inefficiently print out:
   
        what now? what? ow! 

   IMPORTANT: the string pointers returned by get_subexp_string point
   into the matched_subexps_t (in this case my_subexps) and so are
   only usable until matched_subexps_free is called.  */

#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Structure for holding matched (improper) subexpressions.  Define
   and initialeze with MATCHED_SUBEXPS_INITIALIZER, but otherwise
   don't touch (use interface functions below).  */
typedef struct 
{
  /* Flag true if the structure has been filled in but not freed.  */
  int is_dirty;	
  /* Number of (improper) subexpressions, including the entire pattern.  */
  int subexp_count;		
  /* Array of subexpression strings (or NULLs where subexps in the
     regex didn't get used in a match).  */
  char **subexp_strings;
} matched_subexps_t;

/* For initializing variables of type matched_subexps_t.  */
#define MATCHED_SUBEXPS_INITIALIZER {0, 0, NULL}

/* Perform extended regular expression match of 'string' against
   'regex'.  Returns one if the match succeeds, zero otherwise.  */
int regex_match(matched_subexps_t *msubs, const char *string, 
		const char *regex);

/* Return pointer to a string in the matched_subexps_t structure
   corresponding to matched subexpression at position 'pos' (position
   0 holds the entire match).  This string is only usable until
   matched_subexps_free is done.  A null string is returned if the
   subexpression matched an empty string.  A NULL pointer is returned
   if the subexpression wasn't used in the match at all.  Complain and
   die if called on a matched_subexps_t that didn't match. */
char *get_subexp_string(matched_subexps_t *msubs, int pos);

/* Free storage used by matched_subexps_t and make structure ready for
   possible reuse.  */
void matched_subexps_free(matched_subexps_t *msubs);
