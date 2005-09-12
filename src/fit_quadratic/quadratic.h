/***************************************
Include file for: Quadratic 2D functions.

This header describes a set of routines for
manipulating a Quadratic 2D function-- there
are routines for evaluating, printing, and
finding (in a least-squares fasion) these functions.
*/

/*
Quadratic warping coefficients.  Represents a low-order 
real-valued function of the plane of the form:
f(x,y)=A+Bx+Cy+Dxx+Exy+Fyy
*/
typedef struct {
	double A,B,C,D,E,F;
} quadratic_2d;

/*Evaluate quadratic warp at given location*/
double quadratic_eval(const quadratic_2d *c,double x,double y);

/*Fit a quadratic warping function to the given points
in a least-squares fashion.  Resulting function
maps x,y onto out, so
out[i] nearly = quadratic_eval(ret,x[i],y[i]);
*/
quadratic_2d find_quadratic(const double *out,
	const double *x,const double *y,int numPts);

/*Write the given quadratic to the given stream.*/
void quadratic_write(const quadratic_2d *c,FILE *stream);

/*Read a quadratic from the given stream.*/
void quadratic_read(quadratic_2d *c,FILE *stream);
