#include "asf.h"


   /* find the magnitude of a vector */




double mag(a)

double a[];
       

   {
 
   double sum;
   int i;

   sum = 0.0;

   for (i = 1; i <= 3; i++)
      sum = (a[i] * a[i]) + sum;
    
   sum = sqrt(sum);

   return (sum);
   }

