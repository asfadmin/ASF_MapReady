/****************************************************************
FUNCTION NAME: minv1()

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:
   A floating point version of DMINV1().

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"


#include "ifm.h"

#ifdef EPSILON
#undef EPSILON
#endif
#define EPSILON 1.3877787807814457e-17

void minv1(float **a, int n)		/* find A[nn] = A[nn]^-1 */
{
   static int already = 0, lastn;
   register int i, j, k, i0=0, j0=0;
   register float *p1, *p2;
   static int *p, *q;
   static float *b, *c;
   int sd = sizeof (float);
   float w, z;

   if (!already || n != lastn) {
     p = (int *) MALLOC (n * sizeof (int));
	 q = (int *) MALLOC (n *sizeof (int));
	 b = (float *)MALLOC (n * sd);
	 c = (float *)MALLOC (n * sd);
     lastn = n;
     already = 1;
   }

   for (k = 0; k < n; k++) {
     w = 0.0;
     for(i = k; i < n; i++) {
	p1 = a[i];
	for(j = k; j < n; j++)
	   if (fabs (p1[j]) > fabs (w)) {
		w = p1[j];
		i0 = i; j0 = j;
  	   }
     }
     if (fabs(w) < EPSILON) {
	Exit("In minv1(): fabs(w) < EPSISON - w = %f",w);
     }
     if (i0 != k) {
        fcpvec(a[i0], c, n);
        fcpvec(a[k], a[i0], n);
	fcpvec(c, a[k], n);
     }
     if (j0 != k)
	for (i = 0; i < n; i++) {
	  p1 = a[i];
	  z = p1[j0];
	  p1[j0] = p1[k];
	  p1[k] = z;
        }
    p[k] = i0;	
    q[k] = j0;
    for (j = 0; j < n; j++) {
      p1 = a[k];
      p2 = a[j];
      if (j == k) {
	 b[j] = 1. / w;
	 c[j] = 1.;
      } else {
	b[j] = -p1[j] / w;
	c[j] = p2[k];
      }
      p1[j] = p2[k] = 0.;
    }
    for (i = 0; i < n; i++) {
       p1 = a[i];
       for (j = 0; j < n; j++)
	 p1[j] += c[i] * b[j];
    }
   }

   for (k = n - 1; k > -1; k--) {
     i0 = p[k];	
     j0 = q[k];
     if (i0 != k)
       for (i = 0; i < n; i++) {
	 p1 = a[i];
	 z = p1[i0];
	 p1[i0] = p1[k];
	 p1[k] = z;
       }
     if (j0 != k) {
       fcpvec(a[j0], b, n);
       fcpvec(a[k], a[j0], n);
       fcpvec(b, a[k], n);
     }
   }

   return;
}
