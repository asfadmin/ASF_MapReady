/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* float pathval(coarse,fine,a,twod)  ---------------------

  done by quyen dinh nguyen
  6/29/89
  pathval.f --- to calculate the max of the path, given
                by the equation:
                  path = coarse+fine+ az(z+twod)
                where z can be from -1/2 to 1/2
 */

#include <math.h>
float pathval(coarse,fine,a,twod)
float coarse,fine,a,twod;
{
     float xpos,xneg;
     xpos = coarse+fine + 0.5*a*(0.5+twod);
     xneg = coarse+fine + (-0.5)*a*((-0.5) + twod);
     return ( xpos > xneg ? xpos : xneg);
}
