/*********************************
Implementation for quadratic 2D function 
utility routines.
Orion Sky Lawlor, 3/99
*/
#include "asf.h"
#include "poly.h"


/*Return the value of the i'th term of this polynomial.
  E.g., term 2 is y; term 4 is x*y, etc.
 Degree  TermNo     Reduced    Value
 0       0	    0	       1
 1       1 2	    0 1        x    y
 2       3 4 5      0 1 2      x^2  x*y    y^2
 3       6 7 8 9    0 1 2 3    x^3  x^2*y  x*y^2  y^3
 ...
*/
double poly_term(int termNo,double x,double y)
{
	int degree=0;
	int reduced=termNo;
	/* Search to find the term's degree */
	while (reduced>degree) {
		degree++;
		reduced-=degree;
	}
	/* TermNo now gives number within row */
	return pow(x,degree-reduced)*pow(y,reduced);
}


/* Allocate a polynomial of this degree */
poly_2d *poly_allocate(int degree)
{
	int d,nTerms;
	poly_2d *ret=(poly_2d *)malloc(sizeof(poly_2d));
	ret->degree=degree;
	nTerms=0;
	for (d=0;d<=degree;d++) nTerms+=1+d;
	ret->nTerms=nTerms;
	ret->v=(double *)malloc(sizeof(double)*nTerms);
	return ret;
}

/* Delete this polynomial */
void poly_delete(poly_2d *c) {
	free(c->v);
	c->v=NULL;
	free(c);
}


/*Evaluate quadratic warp at given location*/
double poly_eval(const poly_2d *c,double x,double y)
{
	double ret=0;
	int t;
	for (t=0;t<c->nTerms;t++)
		ret+=c->v[t]*poly_term(t,x,y);
	return ret;
}

void poly_write(const poly_2d *c,FILE *stream)
{
	int t;
	fprintf(stream,"%d     ",c->degree);
	for (t=0;t<c->nTerms;t++)
		fprintf(stream,"%.18g  ",c->v[t]);
	fprintf(stream,"\n");
}

poly_2d *poly_read(FILE *stream)
{
	poly_2d *c=NULL;
	int degree=-1;
	int t;
	if (1!=fscanf(stream,"%d",&degree)) goto bad;
	c=poly_allocate(degree);
	for (t=0;t<c->nTerms;t++)
		if (1!=fscanf(stream,"%lg",&c->v[t]))
			goto bad;
	return c;
bad:
/*Some input error has occured*/
	fprintf(stderr,"Couldn't read a polynomial from the given file\n");
	exit(1);	
}


