#include "asf.h"

#include "constants.h"

void calcmask(sat,tar,normal,mask)
double sat[],
       tar[],
       normal[],
       *mask;
{
   double neglos[4], vec[4],
          rnangle, rlangle, nlangle;
   short i;

   for ( i = 1; i <= 3; i++)
      {
      vec[i] = tar[i];
      neglos[i] = sat[i] - tar[i];
      }
   
   rnangle = acos(dot(vec,normal) / (mag(vec) * mag(normal)));
   rlangle = acos(dot(vec,neglos) / (mag(vec) * mag(neglos)));
   nlangle = acos(dot(normal,neglos) / (mag(normal) * mag(neglos)));

   *mask = 0.0;
   if (nlangle >= PIE / 2.0) *mask = 100.0;  /* shadow */
   if ( rnangle >= rlangle && nlangle < rlangle) *mask = 200.0;   /* layover */
   return;
}
