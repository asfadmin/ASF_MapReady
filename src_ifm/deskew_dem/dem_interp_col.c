#include "deskew_dem.h"

void dem_interp_col(float *demLine,int ns,int nl)
{
#define buf(y) (demLine[ns*(y)])
  int y,lastOutY=0;
  float lastOutVal=badDEMht;
  for (y=0;y<nl;y++)
  {
    float height=buf(y);
    if (height!=badDEMht)
     {
	if (lastOutY!=(y-1))
  	  {
	   int yInterp;
	   if (lastOutY&&(y-lastOutY<maxBreakLen))
	     {
	        float curr=lastOutVal;
		float delt=(height-lastOutVal)/(y-lastOutY);
		curr+=delt;
		for (yInterp=lastOutY+1;yInterp<=y;yInterp++)
	 	 {
		   buf(yInterp)=curr;
		   curr+=delt;
		 }
	     }	
	   }
	lastOutY=y;
	lastOutVal=height;
     }
  }
}
