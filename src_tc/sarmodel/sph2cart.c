#include "asf.h"

   /* convert spherical coord.s to cart. coord.s */




   void fortran_sph2cart(latp,longp,r,vec)

   double latp,
          longp,
          r,
          vec[];

   {

   double rcoslt;

   rcoslt = r * cos(latp);
   vec[1] = rcoslt * cos(longp);
   vec[2] = rcoslt * sin(longp);
   vec[3] = r * sin(latp);
   return;

   }

