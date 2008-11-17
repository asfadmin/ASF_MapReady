#include "asf_vector.h"
#include <ctype.h>

void strip_end_whitesp_inplace(char *s)
{
    char *p = s + strlen(s) - 1;
    while (isspace(*p) && p>s)
        *p-- = '\0';
}

static void consolidate_quotes(char *s)
{
    int i,j=0,l=strlen(s)+1;
    char *buf = MALLOC(sizeof(char)*l);
    for (i=0; i<l; ++i) {
        if (s[i]=='\"' && s[i+1]=='\"') {
            buf[j] = '\"';
            ++i;
        } else {
            buf[j] = s[i];
        }
        ++j;
    }
    strcpy(s,buf);
    FREE(buf);
}

char *quoted_string_parse(char *p, char *s, int max_len, int line_num)
{
  if (!p) {
    // reached end of the line prematurely
    if (line_num>0)
      printf("Line %d: Not enough data columns.\n", line_num);
    strcpy(s,"");
    return NULL;
  }

  // starting at p, eat characters, putting into s (allocated by caller),
  // until we reach the ending comma, or max_len.  For quoted strings,
  // we eat until we find the closing quote.

  // first, we eat whitespace
  while (isspace(*p))
    ++p;

  // now see if we have a quoted string or not
  if (*p == '\"') {
    // a quoted string... scan ahead to next quote instead of comma
    char *q = strchr(p+1, '\"');
    if (q) {
      // after the quote, the next non-whitespace character should be a comma
      // if it is another quote, then we need to keep going
      char *r = q+1;
      while (*r == '\"') {
        char *q1 = strchr(r+1, '\"');
        if (!q1)
          break;
        else {
          q = q1;
          r = q+1;
        }
      }

      // eat whitespace, until we get to what should be a comma
      while (isspace(*r))
        ++r;
      // now, eat the comma
      if (*r != ',') {
        if (line_num>0)
          printf("Line %d: Column entry has extra characters\n", line_num);
        // scan ahead to the next comma...
        r = strchr(r,',');        
      }
      
      // do not include the quotes in the returned value
      *q = '\0'; // temporary
      strncpy_safe(s, p+1, max_len);
      *q = '\"';

      // convert all instances of "" to just plain "
      consolidate_quotes(s);

      // now return pointer to just after the comma (if we found one)
      if (r)
        return r+1;
      else
        return NULL;
    }
    else {
      // no trailing quote found!
      // just put everything that's left into the output string
      strncpy_safe(s, p+1, max_len);
      // return NULL (to indicate this is the last column)
      return NULL;
    }
  }
  else {
    // non-quoted string
    char *q = strchr(p, ',');
    if (q) {
      *q = '\0'; // temporary
      strncpy_safe(s, p, max_len);
      strip_end_whitesp_inplace(s);
      *q = ',';

      // return pointer to just after the comma
      return q+1;
    }
    else {
      // no more commas, just return what is left in the string
      // and return NULL to indicate that we have everything now
      strncpy_safe(s, p, max_len);
      strip_end_whitesp_inplace(s);
      return NULL;
    } 
  }
}

void split_into_array(char *str, char sep, int *nelem, char ***parr)
{
  char *p = str;
  int i,n=0;
  do {
    char val[512];
    p = quoted_string_parse(p, val, 512, -1);
    ++n;
  } while (p);

  char **arr = MALLOC(sizeof(char*)*n);
  p = str;
  for (i=0; i<n; ++i) {
    char val[512];
    p = quoted_string_parse(p, val, 512, -1);
    arr[i] = MALLOC(sizeof(char)*(strlen(val)+5));
    strcpy(arr[i], val);
  }

  *parr = arr;
  *nelem = n;
}
