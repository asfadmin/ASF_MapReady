#include "asf.h"


#include "constants.h"

double calcfd2 (sat,tar,clos,wavelength,dt)

double sat[],
       tar[],
       clos,
       wavelength,
       dt;
         
{
   double vec1[4], vec2[4], value, cfd2;
   int i, j;

   /* calc the cfd2 and costheta */
   for (i=1;i<=3;i++)
    {
     j = i + 4;
     vec1[i] = tar[i] - sat[i];
     vec2[i] = tar[j] - sat[j];
    }

   value = dot(vec1,vec2);
   cfd2 = (-2.0 / wavelength) * (value / clos);

   return (cfd2);
}
