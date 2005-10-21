/* A 3D vector of doubles.  */

#ifndef VECTOR_H
#define VECTOR_H

/* These class data members may be read directly from the structure,
   but should generally be created or modified through the interface
   functions.  However, since it is sometimes useful to reuse static
   vectors, the values may be set directly, and the vector_set method
   is garanteed to work on explicity declared Vector structures,
   i.e. 'static Vector p_st; Vector *p = &p_st; vector_set (p, xc, yc,
   zc)' will do what you expect.  */
typedef struct {
  double x, y, z;
} Vector;

/* Create a new vector.  */
Vector *
vector_new (double x, double y, double z);

/* Create a new independent copy of model vector a.  */
Vector *
vector_copy (Vector *a);

/* Create a new vector a x b.  */
Vector *
vector_cross (Vector *a, Vector *b);

/* Set self to x, y, z.  */
void
vector_set (Vector *self, double x, double y, double z);

/* Compute dot product of self and other.  */
double
vector_dot (Vector *self, Vector *other);

/* Add other to self.  */
void
vector_add (Vector *self, Vector *other);

/* Subtract other from self.  */
void
vector_subtract (Vector *self, Vector *other);

/* Multiply self by a constant factor.  */
void
vector_multiply (Vector *self, double factor);

/* Get the magnitude of self.  */
double
vector_magnitude (Vector *self);

/* Get the angle between self and other, in radians.  Both self and
   other must have magnitude greater than zero.  
   Returns a value between 0 and PI. */
double
vector_angle (Vector *self, Vector *other);

/* scale the vector so that its magnitude is 1 */
void
vector_normalize(Vector *self);

/* Return a newly allocated vector that is the projection of self
   to the plane defined by other1 and other2. */
Vector *
vector_project (Vector *self, Vector *other1, Vector *other2);

/* Free a vector.  */
void
vector_free (Vector *self);

#endif /* VECTOR_H */

