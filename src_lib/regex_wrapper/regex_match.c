/* Part of an easy but machine inefficient interface to libc regex
   facilities.

   See the public header regex_wrapper.h for details.  */

#include "regex_wrapper.h"

/* Perform extended regular expression match of 'string' against 'regex'.  */
int regex_match(matched_subexps_t *msubs, const char *string, 
		const char *regex)
{
  int idx;			/* Loop index.  */
  regex_t compiled_regex;	/* libc compiled regex.  */
  regmatch_t *subexps;		/* Structures describing subexpressions.  */

  /* Don't allow refilling of already filled in structures.  */
  if ( msubs->is_dirty ) {
#ifdef _ISOC99_SOURCE
    fprintf(stderr, "'%s' called on a matched_subexps_t that had already been filled in (by '%s')\n", __func__, __func__);
#else
    fprintf(stderr, "'regex_match' called on a matched_subexps_t that had already been filled in (by 'regex_match')\n");
#endif
    exit(EXIT_FAILURE);
  }
  
  /* Mark structure dirty.  */
  msubs->is_dirty = 1;

  /* Count the subexpressions in regex by looking for left parens.  */
  msubs->subexp_count = 0;
  for ( idx = 0 ; (size_t) idx < strlen(regex) ; idx++ )
    if ( regex[idx] == '(' )
      msubs->subexp_count++;
  /* Subexpressions are improper.  One of them is the entire pattern.  */
  msubs->subexp_count++;

  /* Allocate memory for temporary storage of array of regmatch_t.  */
  if ( (subexps = calloc( (size_t) msubs->subexp_count, sizeof(regex_t))) 
       == NULL ) {
#ifdef _ISOC99_SOURCE    
    fprintf(stderr, "libc function 'calloc' failed in function '%s\n'",
	    __func__);
#else
    fprintf(stderr, 
	    "libc function 'calloc' failed in function 'regex_match'\n");
#endif
    exit(EXIT_FAILURE);
  }

  switch ( regcomp(&compiled_regex, regex, REG_EXTENDED) ) {
  case 0:   	                /* Success.  */
    break;	
  case REG_ESPACE:		/* Not enough memory.  */
    fprintf(stderr, "libc function 'regcomp' (called from function '%s') ran out of memory\n", __func__);
    exit(EXIT_FAILURE);
  default:			/* Malformed regex.  */
#ifdef _ISOC99_SOURCE
    fprintf(stderr, "libc function 'regcomp' (called from function '%s') failed due to badly formed regex argument '%s':", __func__, regex);
#else
    fprintf(stderr, "libc function 'regcomp' (called from function 'regex_match') failed due to badly formed regex argument '%s':", regex);
#endif
    /* This block prints the rest of the error message.  */
    {
      int err_code;		/* Error code returned by regcomp.  */
      size_t err_len;		/* Length of error message.  */
      char *err_buf;		/* Buffer for error message.  */
      
      regfree(&compiled_regex);	/* Free regex for recompile.  */
      /* Recompile regex, remembering error code this time.  */
      err_code = regcomp(&compiled_regex, regex, REG_EXTENDED);
      /* Funny way to get the length of the error message.  */
      err_len = regerror (err_code, &compiled_regex, NULL, 0);
      if ( (err_buf = (char *) malloc(err_len * sizeof(char))) == NULL ) {
	fprintf(stderr, "\nlibc function 'malloc' failed in function '%s' while trying to get memory to report another error\n", __func__);
	exit(EXIT_FAILURE);
      }
      /* Get the actual error message.  */
      (void) regerror (err_code, &compiled_regex, err_buf, err_len);
      fprintf(stderr, "%s\n", err_buf);
    }
    exit(EXIT_FAILURE);
  }

  switch ( regexec(&compiled_regex, string, (size_t) msubs->subexp_count, 
		   subexps, 0 /* no flags */) ) {
  case 0:			/* Success.  */
    break;
  case REG_ESPACE:		/* Not enouth memory.  */
    fprintf(stderr, "libc function 'regexec' (called from function '%s') ran out of memory\n", __func__);
    exit(EXIT_FAILURE);
  case REG_NOMATCH:		      /* Pattern didn't match.  */
    free(subexps);		/* Free array of libc regmatch_t.  */
    regfree(&compiled_regex);	/* Free libc compiled regex.  */
    msubs->subexp_strings = NULL;     /* Set sentinel value.  */
    msubs->subexp_count = 0;	/* Ensure subexp count is correct. */
    msubs->is_dirty = 0;	/* Structure is not dirty.  */
    return 0;			/* Return false.  */
  }

  /* Allocate memory for array of strings.  */
  if ( (msubs->subexp_strings = calloc( (size_t) msubs->subexp_count + 1, 
				       sizeof(char *))) == NULL ) {
    fprintf(stderr, "libc function 'calloc' failed in function '%s'\n",
	    __func__);
    exit(EXIT_FAILURE);
  }

  for ( idx = 0 ; idx < msubs->subexp_count + 1 ; idx++ ) {
    int start_off, end_off;   /* Offsets of substring endpoints in 'string'. */

    start_off = subexps[idx].rm_so;
    end_off = subexps[idx].rm_eo;
    if  ( start_off == -1 ) {	/* libc magic meaning unused subexp.  */
      msubs->subexp_strings[idx] = NULL;
    } else {
      if ( (msubs->subexp_strings[idx] 
	    = calloc( (size_t) (end_off - start_off + 1), sizeof(char))) 
	   == NULL ) {
	fprintf(stderr, "libc function 'calloc' failed in function '%s'\n",
		__func__);
	exit(EXIT_FAILURE);
      }
    }
    strncpy(msubs->subexp_strings[idx], string + start_off, 
	    (size_t) (end_off - start_off));
    msubs->subexp_strings[idx][end_off - start_off] = '\0';
  }
  
  free(subexps);               /* Free the libc subexp storage.  */
  regfree(&compiled_regex);    /* Free libc compiled regex.  */

  return 1;		       /* Return true.  */
}
