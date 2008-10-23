#include <ctype.h>
#include <asf.h>

char *uc (const char *string)
{
  static char out[1024];

  int ii,n = strlen(string);
  if (n>1023) n=1023;

  for (ii=0; ii<n; ii++)
    out[ii]=toupper(string[ii]);
  out[ii]='\0';

  return out;
}

char *lc (const char *string)
{
  static char out[1024];

  int ii,n = strlen(string);
  if (n>1023) n=1023;

  for (ii=0; ii<n; ii++)
    out[ii]=tolower(string[ii]);
  out[ii]='\0';

  return out;
}

int strcmp_case(const char *s1, const char *s2)
{
    const char *p1 = s1;
    const char *p2 = s2;

    while (toupper(*p1)==toupper(*p2++))
        if (*p1++ == '\0') return 0;
    return (toupper(*p1) - toupper(*--p2));
}

int strncmp_case(const char *s1, const char *s2, size_t n)
{
    const char *p1 = s1;
    const char *p2 = s2;
    int i=0;

    while (toupper(*p1)==toupper(*p2++)) {
        if (*p1++ == '\0') return 0;
        if (++i == n) return 0;
    }
    return (toupper(*p1) - toupper(*--p2));
}

char *appendStr(const char *s1, const char *s2)
{
    char *ret = MALLOC((strlen(s1)+strlen(s2)+1)*sizeof(char));
    strcat(strcpy(ret, s1), s2);
    return ret;
}

// copies not more than len-1 characters from the string 'src'
// (including any terminating null characters), to 'dst'
// If strlen(src)<len, null characters are appended to dst
// to make the total len.
// If strlen(src)>=len, src is truncated to len-1 characters,
// and a null terminating character is appended to dst.
// Differs from strncpy in that:
//  - only copies len-1 characters, instead of len.
//  - dst is guaranteed null-terminated
char *strncpy_safe(char *dst, const char *src, size_t len)
{
    strncpy(dst, src, len-1);
    dst[len-1] = '\0';
    return dst;
}

char *trim_spaces(const char *s)
{
  char *dup = STRDUP(s);

  // strip end whitespace
  int n = strlen(dup)-1;
  while (isspace(dup[n]) && n>0)
    dup[n--] = '\0';

  // handle all-spaces case
  if (n==0)
    return STRDUP("");

  // now strip beginning whitespace, may assume not all spaces
  char *p = dup;
  while (isspace(*p))
    ++p;

  char *ret = STRDUP(p);
  FREE(dup);

  return ret;
}

void chomp(char *str)
{
  if (str[strlen(str)-1] == '\n')
    str[strlen(str)-1] = '\0';
}

// returns TRUE if "str" ends with the characters in "tail"
int endsWith(const char *str, const char *tail)
{
  if (strlen(tail) > strlen(str))
    return FALSE;
  else if (strlen(tail) == strlen(str))
    return strcmp_case(str, tail)==0;
  else
  {
    const char *p = str + strlen(str) - strlen(tail);
    return strcmp_case(p, tail)==0;
  }
}

// Reimplementation of strtok_r for Windows, mingw does not have it
char *STRTOK_R(char *str1, const char *str2, char **str3)
{
#ifdef win32
  char *ret;

  if (!str1)
    str1 = *str3;

  while (*str1 && strchr(str2, *str1))
    ++str1;
  if (*str1 == '\0')
    return NULL;

  ret = str1;

  while (*str1 && !strchr(str2, *str1))
    ++str1;

  if (*str1)
    *str1++ = '\0';

  *str3 = str1;

  return ret;
#else
  return strtok_r(str1, str2, str3);
#endif
}

// return the number of occurences of char 'c' in string 's'
int count_char(const char *s, char c)
{
  // allow passing in NULL for the string
  if (!s) return 0; 

  int i,n=0;
  for (i=0; i<strlen(s); ++i)
    if (s[i]==c) ++n;

  return n;
}

// a version of strstr() ignoring case.
char *strstr_case(const char *str, const char *key)
{
  const char *p1, *p2;

  while (*str != '\0') {
    p1 = str;
    p2 = key;
    while (tolower(*p2) == tolower(*p1) && *p1 != '\0') {
      p1++;
      p2++;
    }
    if (*p2 == '\0') {
      return (char *)str;
    }
    str++;
  }
  return NULL;
}
