#include "asf.h"
/* move the satellite from its initial position to a new position dtime later */


#include "constants.h"

void movesat (satint,rcoefs,vcoefs,gammacoefs,mcoefs,dtime,sat)
   double satint[], sat[], rcoefs[], vcoefs[], gammacoefs[], mcoefs[], dtime;
{
     double dm,
             v2, r2,
             posc[4], velc[4], vel2[4],
             angle, gamma2;
      int i, j, k;

   /* constants */
      dm = mcoefs[1] * dtime * dtime + mcoefs[2] * dtime + mcoefs[3];
      r2 = rcoefs[1] * dtime * dtime + rcoefs[2] * dtime + rcoefs[3];
      v2 = vcoefs[1] * dtime * dtime + vcoefs[2] * dtime + vcoefs[3];
      gamma2 = gammacoefs[1] * dtime * dtime + 
               gammacoefs[2] * dtime + 
               gammacoefs[3];

   /* calc. new position, velocity and acceleration vectors */
      for ( i = 1 ; i <= 3 ; i++)
         {
         j = i + 4;
         posc[i] = satint[i];
         velc[i] = satint[j];
         } 

      newvec(posc,velc,dm,r2,sat);
      angle = dm + gamma2;
      newvec(posc,velc,angle,v2,vel2);

      for (i = 1 ; i <= 3 ; i++)
         {
          j = i + 4;
          k = i + 8;
          sat[j] = vel2[i];
          sat[k] = -satint[13] * satint[13] * sat[i];
         }
      sat[4] = r2;
      sat[8] = v2;
      return;
}
   /* note: sat[12] = satint[12] and sat[13] = satint[13]    */


/* calc. a new vector (vec3) in the same plane defined by the two input vectors
   given the angle from the first vector to the new vector and the magnitude
   of the new vector */
void newvec(vec1,vec2,angle,vecmag,vec3)
double vec1[], vec2[], vec3[], angle, vecmag;
{
   double angle12,
          magv1,magv2,
          sinang,
          cotang12,
          ratio1,
          const1,const2;
   int i;

   angle12 = acos(dot(vec1,vec2) / (mag(vec1) * mag(vec2)));
   cotang12 = 1.0 / tan(angle12);
   magv1 = mag(vec1);
   magv2 = mag(vec2);
   sinang = sin(angle);
   ratio1 = vecmag / magv1;
   const1 = ratio1 * (cos(angle) - cotang12 * sinang);
   const2 = (vecmag * sinang) / (magv2 * sin(angle12));

   for (i=1 ; i <= 3 ; i++)
      vec3[i] = vec1[i] * const1 + vec2[i] * const2;

   return;
}

