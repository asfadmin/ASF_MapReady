/* Test regex_wrapper functions.  Program exits successfully if the
   things tested for work as expected.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "regex_wrapper.h"

/* Maximum size for output test strings.  */
#define OUT_MAX 2000

int main(void)
{
  matched_subexps_t my_subexps = MATCHED_SUBEXPS_INITIALIZER;
  char regex_match_output[OUT_MAX];   /* For looking at output strings. */

  printf("Testing functions 'regex_match' and 'get_subexp_string'...\n");
  regex_match_output[0] = '\0';
  if ( regex_match(&my_subexps, "what now?", "(what).*(ow)\\?") ) {
    strcat(regex_match_output, get_subexp_string(&my_subexps, 0));
    strcat(regex_match_output, " ");
    strcat(regex_match_output, get_subexp_string(&my_subexps, 1));
    strcat(regex_match_output, "? ");
    strcat(regex_match_output, get_subexp_string(&my_subexps, 2));
    strcat(regex_match_output, "!\n");
    if ( strcmp(regex_match_output, "what now? what? ow!\n") == 0 ) {
      printf("'regex_match' and 'get_subexp_string' succeeded.\n");
    } else {
      printf("%s\n", regex_match_output);
      printf("'regex_match' or 'get_subexp_string' failed, subexpressions didn't work as expected.\n");
      exit(EXIT_FAILURE);
    }
  } else {
    printf("'regex_match' or 'get_subexp_string' failed, expression that was expected to match didn't\n");
  }
  
  printf("Testing function 'matched_subexps_free'...\n");
  matched_subexps_free(&my_subexps);
  printf("'matched_subexps_free' succeeded.\n");

  printf("\nAll tests succeeded.\n");
  exit(EXIT_SUCCESS);
}

