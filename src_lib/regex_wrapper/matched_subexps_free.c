/* Part of an easy but machine inefficient interface to libc regex
   facilities.

   See the public header regex_wrapper.h for details.  */

#include "regex_wrapper.h"


#ifdef _ISOC99_SOURCE
#define CURRENT_FUNC __func__
#else
#define CURRENT_FUNC "matched_subexps_free"
#endif
/* Free storage used by matched_subexps_t, make structure ready for
   possible reuse.  */
void matched_subexps_free(matched_subexps_t *msubs)
{
  int idx;			/* Index variable.  */
  
  /* Don't allow freeing of already free structures.  */
  if ( !msubs->is_dirty ) {
    fprintf(stderr, "'%s' called on a matched_subexps_t that hadn't been filled in (by 'regex_match')\n", CURRENT_FUNC);
    exit(EXIT_FAILURE);
  }

  if ( msubs->subexp_strings == NULL ) { /* If the match failed...  */
    /* Nothing to free, so reset structure and return.  */
    matched_subexps_t tmp = MATCHED_SUBEXPS_INITIALIZER;
    *msubs = tmp;
     
    return;
  }
  
  /* Free the subexpression strings themselves.  */
  for ( idx = 0 ; idx < msubs->subexp_count ; idx++ )
    if ( msubs->subexp_strings[idx] != NULL )
      free(msubs->subexp_strings[idx]);
  /* Free the array of strings itself.  */
  free(msubs->subexp_strings);
  
  {
    /* Reset structure.  */
    matched_subexps_t tmp = MATCHED_SUBEXPS_INITIALIZER;
    *msubs = tmp;
  }
}
