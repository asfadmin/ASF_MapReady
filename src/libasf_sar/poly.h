/***************************************
Include file for: Polynomial 2D functions.

This header describes a set of routines for
manipulating a Polynomial 2D function-- there
are routines for evaluating, printing, and
finding (in a least-squares fasion) these functions.
*/

/*
Polynomial warping coefficients.


*/
typedef struct {
	/**
	  Degree of polynomial: 
	0 is constant: 
		v[0]
	1 is linear:
		v[0] + v[1]*x+v[2]*y
	2 is quadratic:
		v[0] + v[1]*x+v[2]*y + v[3]*x*x+v[4]*x*y+v[5]*y*y
	3 is cubic, etc. 
	*/
	int degree;
	
	/**
	  Number of terms in polynomial.
	    degree 0: 1
	    degree 1: 3
	    degree 2: 6
	*/
	int nTerms;
	
	/** 
	  Coefficients that go with each term of the polynomial.
	  nTerms, allocated using malloc.
	*/
	double *v;
} poly_2d;

/*Return the value of the i'th term of this polynomial.
  E.g., term 2 is y; term 4 is x*y, etc.
*/
double poly_term(int termNo,double x,double y);

/*Evaluate polynomial warp at given location*/
double poly_eval(const poly_2d *c,double x,double y);

/*Write the given poly to the given stream.*/
void poly_write(const poly_2d *c,FILE *stream);

/*Read a poly from the given stream.*/
poly_2d *poly_read(FILE *stream);

/* Allocate a polynomial of this degree */
poly_2d *poly_allocate(int degree);

/* Delete this polynomial */
void poly_delete(poly_2d *c);

