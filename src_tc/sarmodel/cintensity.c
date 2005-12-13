#include "asf.h"



#include "constants.h"

   void cintensity(sat,tar,normal,doubles,sini)

   double sat[],
          tar[],
          normal[],
          *doubles,
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

   // not clear what this really does
   bottem = *sini + 0.1 * cosi;

   // replace this ....
//   sigma = .0133 * cosi / (bottem * bottem * bottem);
//   if (sigma < 0.0) sigma = 0.0;
//   *bytes = 267.0000 * sqrt(sigma);
//   if (*bytes > 255.0) *bytes = 255.0;

   // .... with this   --kh
   sigma = cosi / (bottem * bottem * bottem);
   if (sigma < 0.0) sigma = 0.0;
   *doubles = sqrt(sigma);

   return;
   }   
