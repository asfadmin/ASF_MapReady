#include "asf.h"
   /* move the targets position from its initial position to a new position
      dtime later */


#include "constants.h"

   void movetar (tarint,dtime,tar)

   double tarint[],
          tar[],
          dtime;

   {
       double dl,
              cosdl,
              sindl,
              sindlperwe,
              wesindl,
              wewe;

       int i,
            j,
            k;

   /* constants */
       
       dl = WE * dtime;
       sindl = sin(dl);
       wesindl = WE * sindl;
       sindlperwe = sindl / WE;
       cosdl = cos(dl);
       wewe = WE * WE;

       for ( i = 1 ; i <= 2 ; i++)
          {
          j = i + 4;
          k = i + 8;
          tar[i] = tarint[i] * cosdl + tarint[j] * sindlperwe;  
          tar[j] = tarint[j] * cosdl - tarint[i] * wesindl;
          tar[k] = -wewe * tarint[i];
          }
       return;
    }

/* note: tar[3] = tarint[3]
         tar[7] = 0 = tarint[7]
         tar[4] = tarint[4]
         tar[8] = tarint[8] */
