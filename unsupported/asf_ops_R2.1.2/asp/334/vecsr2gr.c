/* Alaska SAR Processor (ASP) %W% %E% %U% */

/* vecsr2gr(rsc,rearth, srfirst, srout1st, srinc,grinc, sr2gr,ngrsample)------

   done by Quyen D Nguyen
   8/31/89: this subroutine constructs the slant range to ground range
            interpoltion vector.
            The algorithm is based on the IOM-3347-88-023.

   Input:     rsc= distance from Earth center to SAR (meters)
           rearth= radius of Earth at center swath (meters)
          srfirst= slant range distance to the first range sample (meters)
         srout1st= slant range distance to the first output interpolation
                   point (meters)
          srinc  = slant range increment in meters. The value is
                   (c/2.0)*adrate
          grinc  = ground range increment in meters. For ASF it is 12.5
        ngrsample= number of ground range sample to be generated.

   Output:  sr2gr= 8K real vector that contains the intepolation points
                   for slant range to ground range conversion. 
                   Note that the first element is not zero.

 */

#include <math.h>

vecsr2gr(rsc,rearth, srfirst, srout1st, srinc,grinc, sr2gr,ngrsample)

double rsc,rearth, srfirst,srout1st, srinc, grinc;
float  sr2gr[1];
int   ngrsample;
{
   double xa;
   double xrg,xrg0,xsr;
   int    i;
   double xtemp;

   float   itemp;

   
   xa = (rsc - rearth)/rearth;
   /* calculate the ground range of the first slant range */
   xrg0 = rearth * acos( (1.0+(1.0+xa)*(1.0+xa) - (srout1st/rearth)*
                       (srout1st/rearth))/ (2.0*(1.0+xa))) ;
   for(i=0; i<ngrsample; i++) {
      xrg = xrg0 + ((double)i * grinc);
      xsr = rearth*sqrt(2.0*(1.0+xa)*(1.0-cos(xrg/rearth))+xa*xa);
      sr2gr[i] = (float)( (xsr - srfirst)/srinc); 
   }
}
