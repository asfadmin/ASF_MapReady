#include "asf.h"
/* calculate the geodetic latitude and the height of a point above the
   ellipse, given the geocentric latitude and the radius  
   ref.  escobal, methods of orbit determination page 398 */



void centh2det(smsm,latc,rc,latd,h)
    double smsm[],
           latc,
           rc,
           *latd,
           *h;
 {
    double rc2,
           h2,
           hold = 0.0,
           phi, phip,
           dphip, dphis;
   int    i;
       
   phip = latc;
   for (i = 1 ; i <= 20 ; i++)
    {
      rc2 = smsm[1] * sqrt(smsm[5] / (1.0 - smsm[4] * cos(phip) * cos(phip)));
      phi = atan(1.0 / smsm[5] * tan(phip));
      dphis = phi - phip;
      h2 = sqrt(rc*rc - rc2*rc2*sin(dphis)*sin(dphis)) - rc2*cos(dphis);
      if (fabs(h2 - hold) <= 0.0001) break;
      hold = h2;
      dphip = asin(h2 / rc * sin(dphis));
      phip = latc - dphip; /* phip - dphip; */
    }
   *latd = phi;
   *h = h2;
   return;
 }





