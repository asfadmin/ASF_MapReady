#include "asf.h"
  /* change inertial cart. to fixed earth cart. at ref. time */
  /* ref. practical astronomy with your calculator by peter duffett-smith */


#include "constants.h"

void ic2efc (satint,timeref)
double satint[],
       timeref[];
{
   double bigb,
          aprime, cprime,
          t0,
          gst, gmt, gha,
          intlongp, intlongv,
          latp, longp, latv, longv,
          r, v,
          wewe;

   static double a = 0.0657098;
   static double c = 1.002738;

   wewe = WE * WE;
   r = satint[4];
   v = satint[8];

   latp = asin(satint[3] / r);
   latv = asin(satint[7] / v);

   intlongp = atan2(satint[2] , satint[1]);
   if (intlongp < 0.0) intlongp = PIE2 + intlongp;

   intlongv = atan2(satint[6] , satint[5]);
   if (intlongv < 0.0) intlongv = PIE2 + intlongv;
  
   bigb = calcbee(timeref[1]);

   aprime = a * timeref[2];
   t0 = aprime - bigb;
   gmt = timeref[3] / 3600.0;
   cprime = gmt * c;
   gst = cprime + t0;
   if (gst >= 24.0) gst = gst - 24.0;
   if (gst < 0.0) gst = 24.0 + gst;

   gha = gst * 15.0 * RPD;

   longp = intlongp - gha;
   longv = intlongv - gha;

   fortran_sph2cart (latp,longp,r,satint);

   satint[5] = v * cos(latv) * cos(longv);
   satint[6] = v * cos(latv) * sin(longv);
   satint[7] = v * sin(latv);

   return;
}


/* calculate the   b   value for the calculation of the greenwich hour
   angle ( from practical astronomy with your calculator by
   peter duffett-smith ) */

double calcbee(yr)
  double yr;
{
   double fjd, s, t, r, u, bigbee, mo, day;

   mo = 1.0;
   day = 0.0;
   fjd = fulljd(yr,mo,day);
   s = fjd - 2415020.0;
   t = s / 36525.0;
   r = 6.6460656 + 2400.051262 * t + 0.00002581 * t * t;
   u = r - 24.0 * (yr - 1900);
   bigbee = 24.0 - u;
   return (bigbee);
}


/* calculate the full julian day ( from practical astronomy with your
   calculator by peter duffett-smith ) */

double fulljd (yr,mo,day)
   double day, yr, mo;
{
   double fjd;
   int iyr, imo, a, b, c, d;

   if (mo == 1 || mo == 2)
      {
      iyr = (int)(yr - 1);
      imo = (int)(mo + 12);
      }
   else
      {
      iyr = (int)(yr);
      imo = (int)(mo);
      }

    a = (int)((double)(iyr) / 100);
    b = 2 - a + (int)((double)(a) / 4);
    c = (int)(365.25 * iyr);
    d = (int)(30.6001 * (imo + 1));
    fjd = b + c + d + day + 1720994.5;
    return (fjd);
}
