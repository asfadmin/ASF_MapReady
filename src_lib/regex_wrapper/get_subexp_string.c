/* Part of an easy but machine inefficient interface to libc regex
   facilities.

   See the public header regex_wrapper.h for details.  */

#include "regex_wrapper.h"

/* Return pointer to a string in the matched_subexps_t structure
   corresponding to matched subexpression at position 'pos' (position
   0 holds the entire match).  This string is only usable until
   matched_subexps_free is done.  A null string is returned if the
   subexpression matched an empty string.  A NULL pointer is returned
   if the subexpression wasn't used in the match at all.  Complain and
   die if called on a matched_subexps_t that didn't match. */
char *get_subexp_string(matched_subexps_t *msubs, int pos)
{
  /* Don't allow calls on structures not yet filled in.  */
  if ( !msubs->is_dirty ) {
    fprintf(stderr, "'%s' called on a matched_subexps_t that hadn't been filled in (by 'regex_match')\n", __func__);
    exit(EXIT_FAILURE);
  }
  
  /* Can't return matching subexps if the regex failed to match.  */
  if ( msubs->subexp_strings == NULL ) {
    fprintf(stderr, "'%s' function called on a matched_subexps_t that didn't match\n", __func__);
    exit(EXIT_FAILURE);
  }
  
  /* Complain and die if the index is out of range.  */
  if ( pos < 0 ) {
    fprintf(stderr, "'%s' function got bad (negative) pos argument value: %d\n"
	    , __func__, pos);
    exit(EXIT_FAILURE);
  }
  if ( pos >= msubs->subexp_count ) {
    fprintf(stderr, "'%s' function got bad (out of range high) pos argument value: %d\n", __func__, pos);
    exit(EXIT_FAILURE);
  }
  
  /* Return value is a pointer into the msubs argument structure.  */
  return msubs->subexp_strings[pos];
}
