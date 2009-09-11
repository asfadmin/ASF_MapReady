/*Minimize.h:
	Interface to a 1-D function minimizer.
These routines work best if the function to be
minimized is quadratic (like a sum-of-squares).

	The function to be minimized must be a
simple function of one variable.  Minimize seeks
the local minimum nearest to the given point (x_init),
using the secant method on the function's (numeric)
derivative.

	"params" is just a parameter that's passed down to 
minFunc.  It it's used for anything within minimize.
If you don't use it, just set it to NULL (or any convenient value).
*/

typedef double (*minFunc)(double x,void *params);

double minimize(minFunc this,void *params,double x_init,double x_tolerance);
