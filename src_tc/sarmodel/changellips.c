#include "asf.h"
/* ************************************************************************
PURPOSE: to change from one ellipse to another.

Input:

geodetic latitude, longitude, height
orginal semi-major and semi-minor axis
geocentric centers offset from new ellipse to original ellipse and Euler angles.
new ellipse semi-major and semi-minor axis

Output:

New ellipsoid geodetic latitude,longitude, and height.
****************************************************************************/

#include "constants.h"

void changellips (ollh,osmsm,offcent,nsmsm,nllh)

double ollh[],   /* old latitude, longitude, height */
       osmsm[],  /* old semi-major,semi-minor */
       offcent[], /* center offset vector and rotation angles */
       nsmsm[],   /* new semi-major, semi-minor axis */
       nllh[];    /* new latitude,longitude,height */

   {
   double lat1d,
          h1,
          latc,
          rc,
          lat2d,
          h2,
          long1d,
          long2d,
          pos1[4],
          pos2[4],
          ca,
          sa,
          cb,
          sb,
          cg,
          sg,
          rot11,
          rot12,
          rot13,
          rot21,
          rot22,
          rot23,
          rot31,
          rot32,
          rot33,
          tempx,
          tempy,
          tempz;


   int i;

   lat1d = ollh[1];
   long1d = ollh[2];
   h1 = ollh[3];

   det2centh(osmsm,lat1d,h1,&latc,&rc);
   fortran_sph2cart(latc,long1d,rc,pos1);

   ca = cos(offcent[4]);
   sa = sin(offcent[4]);

   cb = cos(offcent[5]);
   sb = sin(offcent[5]);

   cg = cos(offcent[6]);
   sg = sin(offcent[6]);

   rot11 = cg * ca - sg * cb * sa;
   rot12 = cg * sa + sg * cb * ca;
   rot13 = sg * sb;
   rot21 = -sg * ca - cg * cb * sa;
   rot22 = -sg * sa + cg * cb * ca;
   rot23 = cg * sb;
   rot31 = sb * sa;
   rot32 = -sb * ca;
   rot33 = cb;

   tempx = rot11 * pos1[1] + rot12 * pos1[2] + rot13 * pos1[3];
   tempy = rot21 * pos1[1] + rot22 * pos1[2] + rot23 * pos1[3];
   tempz = rot31 * pos1[1] + rot32 * pos1[2] + rot33 * pos1[3];

   pos1[1] = tempx;
   pos1[2] = tempy;
   pos1[3] = tempz;

   for (i = 1 ; i <= 3 ; i++)
      {
      pos2[i] = pos1[i] + offcent[i];
      }

   fortran_cart2sph(pos2,&latc,&long2d,&rc);
   centh2det(nsmsm,latc,rc,&lat2d,&h2);

   nllh[1] = lat2d;
   nllh[2] = long2d;
   nllh[3] = h2;
   }
     
   
