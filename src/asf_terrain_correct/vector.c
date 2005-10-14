/* Implementation of the interface in vector.h.  */

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h> // FIXME can take this out
#include "vector.h"

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

static void
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

  double m = vector_magnitude (other1);

  /* compute the normal for the plane */
  Vector *n = vector_cross (other1, other2);
  vector_normalize(n);

  assert (vector_magnitude (n) > 0.0);
  assert (is_relatively_small(vector_dot (n, other1), m));
  assert (is_relatively_small(vector_dot (n, other2), m));

  Vector *ns = vector_cross (n, self);
  vector_normalize(ns);

  Vector * s2 = vector_copy(self);
  vector_normalize(s2);

  assert (vector_magnitude (ns) > 0.0);
  assert (is_relatively_small(vector_dot (ns, n), m));
  assert (is_relatively_small(vector_dot (ns, s2), m));

  Vector *ret = vector_cross (ns, n);
  vector_normalize(ret);

  assert (is_relatively_small(vector_dot (ret, n), m));
  assert (is_relatively_small(vector_dot (ret, ns), m));

  double a = vector_angle (ret, self);

  if (a > 3.141592654)
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
