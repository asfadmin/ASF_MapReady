#include "asf.h"




#include "constants.h"

void fortran_cart2sph (vec,lat,longs,r)

double vec[],
       *lat,
       *longs,
       *r;

   {

   double long1;

   *r = sqrt(vec[1] * vec[1] + vec[2] * vec[2] + vec[3] * vec[3]);
   *lat = asin(vec[3] / *r);

   if (vec[1] > 0.0 && vec[2] >= 0.0 )
      {
      long1 = atan(vec[2] / vec[1]);
      *longs = long1;
      }
   else if (vec[1] < 0.0 && vec[2] >= 0.0)
      {
      long1 = atan(vec[2] / vec[1]);  
      *longs = PIE + long1;
      } 
   else if (vec[1] < 0.0 && vec[2] < 0.0)
      {
      long1 = atan(vec[2] / vec[1]);
      *longs = PIE + long1;
      }
   else if (vec[1] > 0.0 && vec[2] < 0.0)
      {
      long1 = atan(vec[2] / vec[1]);
      *longs = PIE2 + long1;
      }
   else if (vec[1] == 0.0 && vec[2] < 0.0)
      {
      *longs = 3.0 * PIE / 4.0;
      }
   else if (vec[1] == 0.0 && vec[2] > 0.0)
      {
      *longs = PIE / 2.0;
      }
   return;
   }




