#include "asf.h"
/* calculate the normal of the surface at the point of interest */


#include "constants.h"

void calcnorm (tar,tar1,tar2,normal)

double tar[],
       tar1[],
       tar2[],
       normal[];

   {
   double vec1[4],
          vec2[4],
          norm1[4];
   double r;
   short  i;

   for (i = 1; i <= 3; i++)
      {
      vec1[i] = tar1[i] - tar[i];
      vec2[i] = tar2[i] - tar[i];
      }

   norm1[1] = vec1[2] * vec2[3] - vec1[3] * vec2[2];
   norm1[2] = vec1[3] * vec2[1] - vec1[1] * vec2[3];
   norm1[3] = vec1[1] * vec2[2] - vec1[2] * vec2[1];

   r = mag(norm1);

   for (i = 1; i <= 3; i++) normal[i] = norm1[i] / r;

   return;
   }
