/*
Minimize.c:
	Orion Lawlor, 1998
	ASF-STEP
See header file for usage, philosophy.
*/
#include "asf.h"

#include "minimize.h"
/*
minFuncSlope:
	Compute the slope of the given function about the
given point, by the numeric divided difference method.
*/
double minFuncSlope(minFunc this,void *params,double x,double h)
{
	return ((*this)(x+h,params)-(*this)(x-h,params))/(2*h);
}


/*
Minimize:
	Use the secant method to find a zero crossing of the 
slope of the given function.  This should correspond to
a (local) minimum.
*/
double minimize(minFunc this,void *params,double x_init,double x_err)
{
	double xLeft=x_init,xRight=x_init+100*x_err,errxLeft,errxRight;
	double h=x_err/2;
	int iteration=0;
	
	printf("\tBegin refining...\n");
	
	errxLeft=minFuncSlope(this,params,xLeft,h);
	errxRight=minFuncSlope(this,params,xRight,h);
	
	while (fabs(xRight-xLeft)>x_err)
	{
	/*Compute the "slope of the slope"-- to find the zero of the function slope.*/
		double slope=(xRight-xLeft)/(errxRight-errxLeft);
		/*xNew is secant method's estimate of a zero of the function's slope.*/
		double xNew=xLeft-slope*errxLeft;
		double errxNew=minFuncSlope(this,params,xNew,h);
		
		/*printf("\tRefining: (%f,%f).  Error=%f\n",xLeft,xRight,(*this)(xLeft,params));*/
		
		if (iteration%2==0)
			{xRight=xNew;errxRight=errxNew;}
		else
			{xLeft=xNew;errxLeft=errxNew;}
		
		iteration++;
		if (iteration>1000)
		{
			printf("ERROR! Cannot minimize function--\n");
			printf("Too many iterations with secant method!\n");
			return x_init;
		}
	}
	
	/*printf("\tDone Refining: (%f,%f)\n",xLeft,xRight);
	printf("\tTiming offset=%f, error=%f\n",xLeft,(*this)(xLeft,params));*/	
	return (xLeft+xRight)/2;
}

