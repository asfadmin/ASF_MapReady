/* PACKED_DEG.C - converts an angle in decimal degrees to packed
		  decimal format used by LAS.


		  Mike Shindle
		  14 March 1995
*/
#include "asf.h"
#include "ddr.h"


/* function declarations */
double sgn(double);

/* constants */
#define SMALL  1.0e-10    /* small floating number */

double packed_deg(x)
double x; 
{


   double sign;
   int deg;  /* degrees */
   int min;  /* minutes */
   double sec;  /* seconds */
   double q;

   sign=sgn(x);
   deg = (int)(x=fabs(x)+SMALL);
   min = (int)(q=(x-(double)deg)*60.0+SMALL);
   sec = ((double)q-min)*60.0+SMALL;

   if (sec > 59.95) {
     sec = 0.0;
     min++;
     if (min > 59) {
       min = 0;
       deg++;
     }
   }

   return ((double)(sign * ((deg * 1.0e+6) + (min * 1.0e+3) + sec)));
}

double sgn(x)
  double x;
{
  if (x >= 0.0)
    return 1.0;

  return (-1.0);
}

