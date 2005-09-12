/*********************************
Implementation for quadratic 2D function 
utility routines.
Orion Sky Lawlor, 3/99
*/
#include "asf.h"
#include "quadratic.h"

/*Evaluate quadratic warp at given location*/
double quadratic_eval(const quadratic_2d *c,double x,double y)
{
	return c->A+c->B*x+c->C*y+c->D*x*x+c->E*x*y+c->F*y*y;
}

void quadratic_write(const quadratic_2d *c,FILE *stream)
{
	fprintf(stream,"%.14f %.14f %.14f %.14f %.14f %.14f\n",
		c->A,c->B,c->C,c->D,c->E,c->F);
}

void quadratic_read(quadratic_2d *c,FILE *stream)
{
	if (6!=
		fscanf(stream,"%lf %lf %lf %lf %lf %lf",
			&c->A,&c->B,&c->C,&c->D,&c->E,&c->F))
	{/*Some input error has occured*/
		fprintf(stderr,"Couldn't read a quadratic function from the given file\n");
		exit(1);
	}

		
}





