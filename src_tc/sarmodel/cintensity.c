#include "asf.h"



#include "constants.h"

   void cintensity(sat,tar,normal,bytes,sini)

   double sat[],
          tar[],
          normal[],
          *bytes,
          *sini;

   {
   double diff[4],
          cosi,
          bottem,          
          sigma,
          magdiff,
          iang;

   int i;

   for (i = 1 ; i <= 3 ; i++)
      {
      diff[i] = sat[i] - tar[i];
      }

   magdiff = mag(diff);
   cosi = dot(diff,normal);
   cosi = cosi / magdiff;
   iang = acos(cosi);   
   *sini = sin(iang);
   bottem = *sini + 0.1 * cosi;
   sigma = .0133 * cosi / (bottem * bottem * bottem);
   if (sigma < 0.0) sigma = 0.0;
   *bytes = 267.0000 * sqrt(sigma);
   if (*bytes > 255.0) *bytes = 255.0;
   return;
   }   
