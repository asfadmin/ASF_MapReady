#include "deskew_dem.h"

float badDEMht=0.0;
int maxBreakLen=20;

void dem_sr2gr(float *inBuf,float *outBuf,int ns)
{
	int outX=0,inX,xInterp;
	int lastOutX=-1;
	float lastOutValue=badDEMht;
	for (inX=0;inX<ns;inX++)
	{
		float height=inBuf[inX];
		outX=(int)sr2gr((float)inX,height);
		if ((height!=badDEMht)&&(outX>=0)&&(outX<ns))
		{
		        /*if ((outX-lastOutX<maxBreakLen)&&(lastOutValue!=badDEMht))*/
		        if (lastOutValue!=badDEMht)
			{
				float curr=lastOutValue;
				float delt=(height-lastOutValue)/(outX-lastOutX);
				curr+=delt;
				for (xInterp=lastOutX+1;xInterp<=outX;xInterp++)
				{
					outBuf[xInterp]=curr;
					curr+=delt;
				}
			} else {
				for (xInterp=lastOutX+1;xInterp<=outX;xInterp++)
					outBuf[xInterp]=badDEMht;
			}
			lastOutValue=height;
			lastOutX=outX;
		}
	}
	for (outX=lastOutX+1;outX<ns;outX++)
		outBuf[outX]=badDEMht;
}
