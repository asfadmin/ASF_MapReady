static char sccsid_froff_c[] =
    "@(#)froff.c	1.2 96/04/09 19:13:24";

/* froff( imax50, imin, delbin, e)  ------------------------

 * done by quyen dinh nguyen
 * 9/11/89: this program will find the value of frameoffset such that
 *          it will introduce the least error amount in offseting the
 *          frame.
 * 3/28/90: modified to return froff value such that it wiil be even
 *          number.
 */

#include <math.h>

int  froff( imax50, imin, delbin, e)
int      imax50,imin;
float    delbin,e;
{
  float    xmin,xrem,e_i;
  double   ip;
  int      i,ifrstbin,ixmin,ifroff;
  
  ifrstbin = 0;
  xmin = 1.0;
  ixmin = imax50;
  for(i=imax50; i>imin ; i--){
      e_i = e*((float)(i));
      xrem = (float)(modf((double)(e*(float)(i)), &ip));
      if(xrem < delbin && ifrstbin == 0 && ( ((int)(e_i)%2) == 0) ) {
          ifroff = i;
          ifrstbin = 1;
      }
      if(xrem < xmin && ( ((int)(e_i)%2) == 0) ) {
          ixmin = i;
          xmin = xrem;
      }
  }
  if(ifrstbin == 0) ifroff = ixmin;
  printf(" frame offset = %d (value =%f) %f\n",ifroff,delbin,
                    (float)(ifroff)*e);
  printf(" the min frame offset error = %d (value =%f) %f\n",
                ixmin, xmin, (float)(ixmin)*e);
  return(ifroff);
}
