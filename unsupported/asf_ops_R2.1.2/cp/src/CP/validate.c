static char sccsid_validate_c[] =  "@(#)validate.c	1.5 97/01/15 11:01:34";

/* #define DEBUG /* */

#include <X11/Intrinsic.h>

#include <sys/time.h>
#include "odl.h"
#include "validate.h"

int validateDouble(ODL msg, char *keyword, double allowed[], int numAllowed)
{
  int i, retval;
  double val;

  val = ODLGetDouble(msg, keyword, &retval);
  if (retval == -1)
    return 0;

#ifdef DEBUG
  printf( "validateDouble: allowed: ");
  for (i=0; i < numAllowed; i++)
    printf("%lf ", allowed[i]);
  printf( "\n");
#endif

  for (i=0; i < numAllowed; i++) {
    if (val == allowed[i]) {
#ifdef DEBUG
      printf("validateDouble returning SUCCESS for %lf\n", val);
#endif
      return(1);
    }
  }
#ifdef DEBUG
  printf("validateDouble returning FAILURE for %lf\n", val);
#endif

  return(0);
}


int validateStr(ODL msg, char *keyword, char* allowed[], int numAllowed)
{
  int i;
  char *str;

  str = ODLGetStr(msg, keyword);
  if (str == NULL)
    return(0);

#ifdef DEBUG
  printf("validateStr: %s allowed: ", str);
  for (i=0; i < numAllowed && allowed[i]; i++) 
    printf("%s ", allowed[i]);
  printf( "\n");
#endif

  for (i=0; i < numAllowed && allowed[i]; i++) {
    if (strcmp(str, allowed[i]) == 0) {
#ifdef DEBUG
      printf("validateStr: %s SUCCESS for value of %s\n", keyword, str); 
#endif
      return(1);
    }
  }

#ifdef DEBUG
  printf("validateStr: %s FAILURE for value of %s\n", keyword, str);
#endif
  return(0);
}

int validateInt(ODL msg, char *keyword, int allowed[], int numAllowed)
{
  int i, retval=0, val;

  val = ODLGetInt(msg, keyword, &retval);
#ifdef DEBUG
  printf( "validateInt: allowed: ");
  for (i=0; i < numAllowed; i++)
    printf("%d ", allowed[i]);
  printf( "\n");
#endif

  for (i=0; i < numAllowed; i++) {
#ifdef DEBUG
    printf("\tvalidate comparing %d and %d\n", val, allowed[i]);
#endif
    if (val == allowed[i]) {
#ifdef DEBUG
  printf("validateInt returning SUCCESS for %d\n", val);
#endif
      return(1);
    }
  }

#ifdef DEBUG
  printf("validateInt returning FAILURE for %d\n", val);
#endif
  return(retval);
}

int validateTime(struct timeval tval)
{

  return(0);
}

int intInRange(int val, int min, int max)
{

#ifdef DEBUG
printf("intInRange: %d withing %d and %d? ", val, min, max);
#endif
  if (min <= val && max >= val) {
#ifdef DEBUG
printf("YES!\n");
#endif
    return(1);
  }

#ifdef DEBUG
printf("NO!\n");
#endif
  return(0);
}

int doubleInRange(double val, double dmin, double dmax)
{
#ifdef DEBUG
printf("doubleInRange: %lf withing %lf and %lf? ", val, dmin, dmax);
#endif
  if (dmin <= val && dmax >= val) {
#ifdef DEBUG
printf("YES!\n");
#endif
    return(1);
  }

#ifdef DEBUG
printf("NO!\n");
#endif
  return(0);
}
