#include "asf.h"




void calcrcoefs(strtrc,centrc,endrc,strttime,centtime,endtime,rcoefs)

double strtrc,
       centrc,
       endrc,
       strttime,
       centtime,
       endtime,
       rcoefs[];

  {
   double dt1,
          dt2,
          dtt1,
          dtt2;

   dt1 = strttime - centtime;
   dt2 = endtime - centtime;
   dtt1 = strttime * strttime - centtime * centtime;
   dtt2 = endtime * endtime - centtime * centtime;

   rcoefs[3] = centrc;

   rcoefs[1] =((endrc - centrc) / dt2 + (centrc - strtrc) / dt1) /
              ((dtt2) / dt2 - (dtt1) / dt1);

   rcoefs[2] = (strtrc - rcoefs[1] * dtt1 - centrc) / dt1;


   return;
   }
