#include "asf.h"

#include "constants.h"

double fd2dot (sat,tar,clos,wavelength,dt)

double sat[],
       tar[],
       clos,
       wavelength,
       dt;
 {
   double vec1[4], vec2[4], vec3[4],
          value, value2,
          cfd2dot, clos2;
   int i, j, k;

   /* calc the cfd2dot */
   value = 0.0;
   value2 = 0.0;
   clos2 = clos * clos;

   for (i=1;i<=3;i++)
    {
     j = i + 4;
     k = i + 8;
     vec1[i] = tar[i] - sat[i];
     vec2[i] = tar[j] - sat[j];
     vec3[i] = tar[k] - sat[k];
     value =  vec1[i] * vec3[i] +
              vec2[i] * vec2[i] + value; 
     value2 = vec1[i] * vec2[i] + value2;
    }
/*
   cfd2dot = -2.0 *frequency * (value - value2 * value2 / clos2)
             / ( LITESPEED * clos); 
*/
   cfd2dot = -2.0 * (value - value2 * value2 / clos2) / (wavelength * clos);
   return (cfd2dot);
 }
