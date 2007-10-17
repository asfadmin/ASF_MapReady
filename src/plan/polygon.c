#include "polygon.h"

Polygon *polygon_new(int n, double *x, double *y)
{
  Polygon *self = MALLOC(sizeof(Polygon));
  self->n = n;
  self->x = MALLOC(sizeof(double)*n);
  self->y = MALLOC(sizeof(double)*n);

  int i;
  for (i=0; i<n; ++i) {
    self->x[i] = x[i];
    self->y[i] = y[i];
  }

  return self;
}

Polygon *polygon_new_closed(int n, double *x, double *y)
{
  Polygon *self = MALLOC(sizeof(Polygon));
  self->n = n+1;
  self->x = MALLOC(sizeof(double)*(n+1));
  self->y = MALLOC(sizeof(double)*(n+1));

  int i;
  for (i=0; i<n; ++i) {
    self->x[i] = x[i];
    self->y[i] = y[i];
  }

  // add the first point again at the end (close the polygon)
  self->x[n] = x[0];
  self->y[n] = y[0];

  return self;
}

static int point_in_polygon(Polygon *self, double x, double y)
{
  int i, j, c = 0;
  for (i = 0, j = self->n-1; i < self->n; j = i++) {
    if ((((self->y[i]<=y) && (y<self->y[j])) ||
      ((self->y[j]<=y) && (y<self->y[i]))) &&
      (x < (self->x[j] - self->x[i]) * (y - self->y[i]) /
       (self->y[j] - self->y[i]) + self->x[i]))
      c = !c;
  }
  return c;
}

void polygon_free(Polygon *self)
{
  if (self) {
    if (self->x)
      free(self->x);
    if (self->y)
      free(self->y);
    free(self);
  }
}

