#include "asf.h"


   /* calculate starting location of target in cart. coord.s */



#include "constants.h"

/* latp, longp are geocentric */

void inttar(latp,longp,r,tarint)

double latp,
       longp,
       r,
       tarint[];

   {
   double rcoslt,
          rwe,
          wewe;

    rcoslt = r * cos(latp);
    rwe = r * WE;
    wewe = WE * WE;
    tarint[1] = rcoslt * cos(longp);
    tarint[2] = rcoslt * sin(longp);
    tarint[3] = r * sin(latp);
    tarint[4] = r;
    tarint[5] = -1.0 * WE * tarint[2];
    tarint[6] = WE * tarint[1];
    tarint[7] = 0.0;
    tarint[8] = WE * rcoslt;
    tarint[9] = -wewe * tarint[1];
    tarint[10] = -wewe * tarint[2];
    tarint[11] = 0.0;
    tarint[12] = wewe * sqrt(tarint[1] * tarint[1] + tarint[2] * tarint[2]);
    tarint[13] = WE;
    return;
    }


