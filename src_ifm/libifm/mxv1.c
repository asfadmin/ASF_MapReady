/****************************************************************
FUNCTION NAME: mxv1.c

SYNTAX:

PARAMETERS:
    NAME: TYPE:  PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
   matrix * vector u(m) = A(mn) * v(n)/

   A floating point version of DMXV1.c.

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "ifm.h"

/* prototype */
int fcpvec(float *from, float *to, int nitems);

void mxv1(float *u, float **a, float *v, int m, int n) 
{
   static int already = 0;
   static float *s;
   register int i, j;
   register float *p;
   int sav=0, sd = sizeof (float);
   float *tmp=NULL;

   if (u == v) {
     if (!already) {
       s = (float *)MALLOC(m * sd);
       already = 1;
     }
     sav = 1;
   } else {
     if (already) tmp = s;
     s = u;
   }
   for (i = 0; i < m; i++) {
     s[i] = 0.; p = a[i];
     for (j = 0; j < n; j++)
       s[i] += p[j] * v[j];
   }
   if (sav)  
     fcpvec(s, u, m);
   else if (already) 
     s = tmp;

   return;
}
