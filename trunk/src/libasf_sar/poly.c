/*********************************
Implementation for quadratic 2D function 
utility routines.
Orion Sky Lawlor, 3/99
*/
#include "asf.h"
#include "poly.h"


/*Return the value of the i'th term of this polynomial.
  E.g., term 2 is y; term 4 is x*y, etc.
 Degree TermNo            Reduced     Value
 0      0                 0           1
 1      1 2               0 1         x    y
 2      3 4 5             0 1 2       x^2  x*y    y^2
 3      6 7 8 9           0 1 2 3     x^3  x^2*y  x*y^2    y^3
 4      10 11 12 13 14    0 1 2 3 4   x^4  x^3*y  x^2*y^2  x*y^3    y^4
 5      15 16 17 18 19 20 0 1 2 3 4 5 x^5  x^4*y  x^3*y^2  x^2*y^3  x*y^4  y^5
 ...
*/
double poly_term(int termNo,double x,double y)
{
    // profiling reveals this as a bottle-neck, so optimze.

    // attempt to optimze some of the more-easily calculated terms
    // do up to degree 5, as that is what we generally use

    // formerly this function was just the "default" case, below.

    switch (termNo) {
        case 0:
            return 1;
        case 1:
            return x;
        case 2:
            return y;
        case 3:
            return x*x;
        case 4:
            return x*y;
        case 5:
            return y*y;
        case 6:
            return x*x*x;
        case 7:
            return x*x*y;
        case 8:
            return x*y*y;
        case 9:
            return y*y*y;
        case 10:
        {
            // I'd hope that the compiler would do this for us if we just put:
            //   return x*x*x*x;
            // but we're here to optimze!
            double x2=x*x;
            return x2*x2;
        }
        case 11:
            return x*x*x*y;
        case 12:
        {
            double xy=x*y;
            return xy*xy;
        }
        case 13:
            return x*y*y*y;
        case 14:
        {
            double y2=y*y;
            return y2*y2;
        }
        case 15:
        {
            double x2=x*x;
            return x2*x2*x;
        }
        case 16:
        {
            double x2=x*x;
            return x2*x2*y;
        }
        case 17:
        {
            double xy=x*y;
            return xy*xy*x;
        }
        case 18:
        {
            double xy=x*y;
            return xy*xy*y;
        }
        case 19:
        {
            double y2=y*y;
            return x*y2*y2;
        }
        case 20:
        {
            double y2=y*y;
            return y*y2*y2;
        }
        default:
        {
            // For terrain correction, we should never hit this code
            // asfPrintError("Yoicks!");
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
    }        
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
	for (t=0;t<c->nTerms;t++) {
		ret+=c->v[t]*poly_term(t,x,y);
	}
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


