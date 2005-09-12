/*
extents.c:
Spans are implemented here-- they can be thought of as
intervals on the real number line.  They contain a "range"
of numbers between the min and max values.  The only 
operation we define on them is the "sum" operation-- 
the sum of two spans is the smallest span that completely 
contains both original spans.

	Routines to manipulate an "extents" structure--
an axis-aligned box in the plane.  An extent consists of two spans,
one on the horizontal (h) and one on the vertical (v).

Similarly, the sum of two extents is the smallest extent which
completely contains both original extents.
*/

#include "asf.h"
#include "ddr.h"
#include "caplib.h"
#include "dem.h"

/*Returns a "null" span--
this span contains no numbers, because its minimum 
is (much) less than its maximum.*/
span nullSpan(void)
{
#ifndef HUGE_VAL
	#define HUGE_VAL 100000000000.0
#endif
	span ret;
	ret.min=HUGE_VAL;
	ret.max=-HUGE_VAL;
	return ret;
}
/*newSpan creates a new span containing the two given numbers.*/
span newSpan(double a,double b)
{
	span ret;
	if (a<b)
		ret.min=a,ret.max=b;
	else
		ret.max=a,ret.min=b;
	return ret;
}
/*SumSpan finds the union of two spans.*/
span sumSpan(span a,span b)
{
	span ret;
	if (a.min<b.min)
		ret.min=a.min;
	else
		ret.min=b.min;
	if (a.max>b.max)
		ret.max=a.max;
	else
		ret.max=b.max;
	return ret;
}
/*Returns a "null" extents struct--
this extents covers no area (actually, less than none!)*/
extents nullExtents(void)
{
	extents ret;
	ret.h=nullSpan();
	ret.v=nullSpan();
	return ret;
}
/*SumExtents pastes two extents together.*/
extents sumExtents(extents a,extents b)
{
	extents ret;
	ret.h=sumSpan(a.h,b.h);
	ret.v=sumSpan(a.v,b.v);
	return ret;
}
