/* Part of an easy but machine inefficient interface to libc regex
   facilities.

   See the public header regex_wrapper.h for details.  */

#include "regex_wrapper.h"

/* Free storage used by matched_subexps_t, make structure ready for
   possible reuse.  */
void matched_subexps_free(matched_subexps_t *msubs)
{
  int idx;			/* Index variable.  */
  
  /* Don't allow freeing of already free structures.  */
  if ( !msubs->is_dirty ) {
    fprintf(stderr, "'" __func__ "' called on a matched_subexps_t that hadn't been filled in (by 'regex_match')\n");
    exit(EXIT_FAILURE);
  }

  if ( msubs->subexp_strings == NULL ) { /* If the match failed...  */
    /* Nothing to free, so reset structure and return.  */
    *msubs = (matched_subexps_t) MATCHED_SUBEXPS_INITIALIZER;
    return;
  }
  
  /* Free the subexpression strings themselves.  */
  for ( idx = 0 ; idx < msubs->subexp_count ; idx++ )
    if ( msubs->subexp_strings[idx] != NULL )
      free(msubs->subexp_strings[idx]);
  /* Free the array of strings itself.  */
  free(msubs->subexp_strings);
  
  /* Reset structure.  */
  *msubs = (matched_subexps_t) MATCHED_SUBEXPS_INITIALIZER;
}
