#include "asf.h"
/* calculate the normal of the surface at the point of interest */


#include "constants.h"

void calc4norm (tar,tar1,tar2,tar3,tar4,normal)
  double tar[], tar1[], tar2[], tar3[], tar4[], normal[];
 {
   double vec1[4], vec2[4], vec3[4], vec4[4],
          norm1[4], norm2[4], norm3[4], norm4[4];
   double r, r1, r2, r3, r4;
   short  i;

   for (i = 1; i <= 3; i++)
      {
      vec1[i] = tar1[i] - tar[i];
      vec2[i] = tar2[i] - tar[i];
      vec3[i] = tar3[i] - tar[i];
      vec4[i] = tar4[i] - tar[i];
      }

   norm1[1] = vec1[2] * vec2[3] - vec1[3] * vec2[2];
   norm1[2] = vec1[3] * vec2[1] - vec1[1] * vec2[3];
   norm1[3] = vec1[1] * vec2[2] - vec1[2] * vec2[1];
   r1 = mag(norm1);

   norm2[1] = vec2[2] * vec3[3] - vec2[3] * vec3[2];
   norm2[2] = vec2[3] * vec3[1] - vec2[1] * vec3[3];
   norm2[3] = vec2[1] * vec3[2] - vec2[2] * vec3[1];
   r2 = mag(norm2);

   norm3[1] = vec3[2] * vec4[3] - vec3[3] * vec4[2];
   norm3[2] = vec3[3] * vec4[1] - vec3[1] * vec4[3];
   norm3[3] = vec3[1] * vec4[2] - vec3[2] * vec4[1];
   r3 = mag(norm3);

   norm4[1] = vec4[2] * vec1[3] - vec4[3] * vec1[2];
   norm4[2] = vec4[3] * vec1[1] - vec4[1] * vec1[3];
   norm4[3] = vec4[1] * vec1[2] - vec4[2] * vec1[1];
   r4 = mag(norm4);

   for (i = 1; i <= 3; i++)
     { norm1[i] /= r1; norm2[i] /= r2; norm3[i] /= r3; norm4[i] /= r4; }
   for (i = 1; i <= 3; i++)
     normal[i] = (norm1[i] + norm2[i] + norm3[i] + norm4[i]) / 4.0;

   r = mag(normal);
   for (i = 1; i <= 3; i++) normal[i] /= r;

   return;
 }
