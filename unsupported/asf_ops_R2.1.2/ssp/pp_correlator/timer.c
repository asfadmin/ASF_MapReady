/* SccsId[]= @(#)timer.c	2.41 3/24/98 */
static char sccsid_timer[]= "@(#)PPtimer.c:2.41";

/******************/
/*  UNIX dtime()  */
/******************/
#include <sys/time.h>
#include <sys/resource.h>

#ifdef hpux
#include <sys/syscall.h>
#define getrusage(a,b) syscall(SYS_getrusage,a,b)
#endif

struct rusage rusage;

/* #ifdef hpux */

float dtime(p)

/*#else
float dtime_(p)
#endif */

float p[];
{
   float q;

   q = p[1];

   getrusage(RUSAGE_SELF,&rusage);

   p[1] = (float)(rusage.ru_utime.tv_sec);
   p[1] = p[1] + (float)(rusage.ru_utime.tv_usec)/1.e+6;
   p[0] = p[1] - q;
   return p[0];
}
/* Returns user time in p[0] and total in p[1].  Both times are in       */
/* seconds.  Note that the return type is float.  To call this function  */
/* in Fortran one needs to declare:                                      */
/*         real*4 tarray(2), t1                                          */
/*         t1 = dtime(tarray)                                            */
/*	   do work here                                                  */
/*	   t1 = dtime(tarray)                                            */
/* On return, tarray(1) contains user delta time, and tarray(2) contains */
/* total accumulated time.                                               */




