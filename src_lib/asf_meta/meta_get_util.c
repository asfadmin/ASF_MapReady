/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Utilites for easy extraction of parameters.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"

/*get_units: in meta_get_util.c
	Used to resolve difference between millimeters,
meters, and kilometers in state vectors and slant ranges.
Returns power of 1000.0 which must be applied to
given data to make it match the expected value.
*/
double get_units(double value, double expectedValue)
{
	double factor=1000.0;
	double factorTol=sqrt(factor);
	double ret=1.0;
	if (fabs(value)<0.0000000001)
		return 1.0;/*Don't know what to do with zero values*/
	if (value*expectedValue<=0.0)
		return 1.0;/*Don't know what to do with zero or opposite-sign numbers.*/
	while ((ret*value)/expectedValue<1/factorTol)
		ret*=factor;
	while ((ret*value)/expectedValue>factorTol)
		ret/=factor;
	return ret;
}

