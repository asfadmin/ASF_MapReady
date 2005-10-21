/* Implementation of the interface in vector.h.  */

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "vector.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

Vector *
vector_new (double x, double y, double z)
{
  Vector *new = malloc (sizeof (Vector));

  new->x = x;
  new->y = y;
  new->z = z;

  return new;
}

Vector *
vector_copy (Vector *a)
{
  Vector *new = malloc (sizeof (Vector));

  new->x = a->x;
  new->y = a->y;
  new->z = a->z;

  return new;
}

Vector *
vector_cross (Vector *a, Vector *b)
{
  Vector *new = malloc (sizeof (Vector));
  
  new->x = a->y * b->z - a->z * b->y;
  new->y = a->z * b->x - a->x * b->z;
  new->z = a->x * b->y - a->y * b->x;

  return new;
}

void
vector_set (Vector *self, double x, double y, double z)
{
  self->x = x;
  self->y = y;
  self->z = z;
}

double
vector_dot (Vector *self, Vector *other)
{
  return self->x * other->x + self->y * other->y + self->z * other->z;
}

void
vector_add (Vector *self, Vector *other)
{
  self->x += other->x;
  self->y += other->y;
  self->z += other->z;
}

void
vector_subtract (Vector *self, Vector *other)
{
  self->x -= other->x;
  self->y -= other->y;
  self->z -= other->z;
}

void
vector_multiply (Vector *self, double factor)
{
  self->x *= factor;
  self->y *= factor;
  self->z *= factor;
}

double
vector_magnitude (Vector *self)
{
  return sqrtl (powl (self->x, 2) + powl (self->y, 2) + powl (self->z, 2));
}

double
vector_angle (Vector *self, Vector *other)
{
  assert (vector_magnitude (self) > 0.0);
  assert (vector_magnitude (other) > 0.0);

  return acos (vector_dot (self, other) / (vector_magnitude (self) 
					   * vector_magnitude (other)));
}

void
vector_normalize(Vector *self)
{
  vector_multiply (self, 1.0/vector_magnitude(self));
}

Vector *
vector_project (Vector *self, Vector *other1, Vector *other2)
{
  assert (vector_magnitude (self) > 0.0);
  assert (vector_magnitude (other1) > 0.0);
  assert (vector_magnitude (other2) > 0.0);

  /* compute the normal for the plane */
  Vector *n = vector_cross (other1, other2);
  vector_normalize(n);

  assert (vector_magnitude (n) > 0.0);

  Vector *ns = vector_cross (n, self);
  vector_normalize(ns);

  Vector * s2 = vector_copy(self);
  vector_normalize(s2);

  assert (vector_magnitude (ns) > 0.0);

  Vector *ret = vector_cross (ns, n);
  vector_normalize(ret);

  double a = vector_angle (ret, self);

  if (a > M_PI)
    vector_multiply (ret, -1);

  assert (vector_angle(ret, self) < 1.6);

  vector_free(n);
  vector_free(ns);

  return ret;
}

void
vector_free (Vector *self)
{
  free (self);
}
