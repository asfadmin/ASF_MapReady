#include "asf.h"

   /* calculate the distance from the radar to the target */





double los(sat,tar)

double sat[],
       tar[];

   {
   double difdif,
          dif,
          clos;

   int i;

   difdif = 0.0;

   for (i = 1; i <= 3; i++)
      {
      dif = tar[i] - sat[i];
      difdif = dif * dif + difdif;
      }
   clos = sqrt(difdif);
   return(clos);
   }


