#include "deskew_dem.h"

void geo_compensate(float *grDEM,float *in,float *out,int ns)
{
	int outX;
	for (outX=0;outX<ns;outX++)
	{
		double height=grDEM[outX];
		double inX=gr2sr(outX,height);
		
		if ((height!=badDEMht)&&(inX>=0)&&(inX<(ns-1)))
		{
			int x=floor(inX);
			double dx=inX-x;
			out[outX]=(1-dx)*in[x]+dx*in[x+1];
		}
		else
			out[outX]=0.0;
	}
}
