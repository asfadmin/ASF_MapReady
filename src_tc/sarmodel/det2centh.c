#include "asf.h"


   /* calculate the geocentric latitude and the radius of a point above the
      ellipse, given the goedetic latitude and the height h */



void det2centh(smsm,latd,h,latc,rc)

double smsm[],
       latd,
       h,              /* height in meters */
       *latc,
       *rc;

   {
   double sinlt,
          w,
          g1,
          g2,
          xc,
          zc;

   sinlt = sin(latd);
   w = sqrt(1.0 - smsm[4] * sinlt * sinlt);

   g1 = smsm[1] / w + h;
   g2 = (smsm[1] * smsm[5]) / w + h;
   xc = g1 * cos(latd);
   zc = g2 * sinlt;

   *rc = sqrt((xc * xc) + (zc * zc));
   *latc = asin(zc / *rc);

   return;
   }
