/***************************************
Include file for: Quadratic 2D functions.

This header describes a set of routines for
manipulating a Quadratic 2D function-- there
are routines for evaluating, printing, and
finding (in a least-squares fasion) these functions.
*/

#include "asf_meta.h"

/*Evaluate quadratic warp at given location*/
double quadratic_eval(const quadratic_2d *c,double x,double y);

/*Read a quadratic from the given stream.*/
void quadratic_read(quadratic_2d *c,FILE *stream);
