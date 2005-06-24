/* A 3D vector of doubles.  This class features some implicit caching
   to make vector instantiation and destruction efficient.  */

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

/* Block size to use when allocating memory for instances (see
   vector_new() description).  */
#define VECTOR_INSTANCE_CACHE_SIZE 10000

/* Create a new vector.  Internally, GMemChunks big enough to hold
   VECTOR_INSTANCE_CACHE_SIZE Vector instances are used, so its safe
   to use vector allocation in the inner loops of computationally
   intensive algorithms.  */
Vector *
vector_new (double x, double y, double z);

/* Create a new independent copy of model vector a.  New instances
   created this way are allocated in a cache as described above in the
   vector_new() method description.  */
Vector *
vector_copy (Vector *a);

/* Create a new vector a x b. New instances created this way are
   allocated in a cache as described above in the vector_new() method
   description.  */
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
   other must have magnitude greater than zero.  */
double
vector_angle (Vector *self, Vector *other);

/* Free self.  */
void
vector_free (Vector *self);

#endif /* VECTOR_H */

