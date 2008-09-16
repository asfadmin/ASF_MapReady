#include "polygon.h"
#include "asf.h"
#include <assert.h>

Poly *polygon_new(int n, double *x, double *y)
{
  Poly *self = MALLOC(sizeof(Poly));
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

Poly *polygon_new_closed(int n, double *x, double *y)
{
  Poly *self = MALLOC(sizeof(Poly));
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

// this is from the comp.graphics.algorithms FAQ
// see http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
int point_in_polygon(Poly *self, double x, double y)
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

//  public domain function by Darel Rex Finley, 2006
//  modified for ASF by kh.  Code was found at:
//     http://alienryderflex.com/intersect/
//  Determines the intersection point of the line segment defined by points
//  A and B with the line segment defined by points C and D.
//
//  Returns YES if the intersection point was found.
//  Returns NO if there is no determinable intersection point.

//  Known bug: returns FALSE if the segments are colinear,
//  even if they overlap
static int lineSegmentsIntersect(
    double Ax, double Ay,
    double Bx, double By,
    double Cx, double Cy,
    double Dx, double Dy)
{
  double  distAB, theCos, theSin, newX, ABpos;

  //  Fail if either line segment is zero-length.
  if ((Ax==Bx && Ay==By) || (Cx==Dx && Cy==Dy)) return FALSE;

  //  (1) Translate the system so that point A is on the origin.
  Bx-=Ax; By-=Ay;
  Cx-=Ax; Cy-=Ay;
  Dx-=Ax; Dy-=Ay;

  //  Discover the length of segment A-B.
  distAB=sqrt(Bx*Bx+By*By);

  //  (2) Rotate the system so that point B is on the positive X axis.
  theCos=Bx/distAB;
  theSin=By/distAB;
  newX=Cx*theCos+Cy*theSin;
  Cy  =Cy*theCos-Cx*theSin; Cx=newX;
  newX=Dx*theCos+Dy*theSin;
  Dy  =Dy*theCos-Dx*theSin; Dx=newX;

  //  Fail if segment C-D doesn't cross line A-B.
  if ((Cy<0. && Dy<0.) || (Cy>=0. && Dy>=0.)) return FALSE;

  //  (3) Discover the position of the intersection point along line A-B.
  ABpos=Dx+(Cx-Dx)*Dy/(Dy-Cy);

  //  Fail if segment C-D crosses line A-B outside of segment A-B.
  if (ABpos<0. || ABpos>distAB) return FALSE;

  //  Success.
  return TRUE;
}

int polygon_overlap(Poly *p1, Poly *p2)
{
  // loop over each pair of line segments, testing for intersection
  int i, j;
  for (i=0; i<p1->n-1; ++i) {
    for (j=0; j<p2->n-1; ++j) {
      if (lineSegmentsIntersect(
            p1->x[i], p1->y[i], p1->x[i+1], p1->y[i+1],
            p2->x[j], p2->y[j], p2->x[j+1], p2->y[j+1]))
      {
        return TRUE;
      }
    }
  }
  
  // test for containment: p2 in p1
  int all_in=TRUE;
  for (i=0; i<p2->n; ++i) {
    if (!point_in_polygon(p1, p2->x[i], p2->y[i])) {
      all_in=FALSE;
      break;
    }
  }
  
  if (all_in)
    return TRUE;
  
  // test for containment: p1 in p2
  all_in = TRUE;
  for (i=0; i<p1->n; ++i) {
    if (!point_in_polygon(p2, p1->x[i], p1->y[i])) {
      all_in=FALSE;
      break;
    }
  }
  
  if (all_in)
    return TRUE;
  
  // no overlap
  return FALSE;  
}

void polygon_get_bbox(Poly *p, double *xmin, double *xmax,
                      double *ymin, double *ymax)
{
  if (p->n == 0) {
    *xmin = *xmax = *ymin = *ymax = 0;
    return;
  }

  *xmin = *xmax = p->x[0];
  *ymin = *ymax = p->y[0];

  if (p->n == 1)
    return;

  int i;
  for (i=1; i<p->n; ++i) {
    if (p->x[i] > *xmax) *xmax = p->x[i];
    if (p->x[i] < *xmin) *xmin = p->x[i];

    if (p->y[i] > *ymax) *ymax = p->y[i];
    if (p->y[i] < *ymin) *ymin = p->y[i];
  }
}

double polygon_area(Poly *p)
{
  if (p->n <= 1)
    return 0.0;

  int i;
  double A=0.0;

  for (i=1; i<p->n; ++i)
    A += p->x[i-1] * p->y[i] - p->x[i] * p->y[i-1];

  // close the polygon automatically -- if the polygon is already
  // closed, this will just add 0
  A += p->x[p->n-1] * p->y[0] - p->x[0] * p->y[p->n-1];

  return fabs(A/2.);
}

double polygon_perimeter(Poly *p)
{
  if (p->n <= 1)
    return 0.0;

  int i;
  double d=0.0;

  for (i=1; i<p->n; ++i)
    d += hypot(p->x[i]-p->x[i-1], p->y[i]-p->y[i-1]);

  // no automatic closing for the perimeter calculation
  return d;
}

void polygon_free(Poly *self)
{
  if (self) {
    if (self->x)
      free(self->x);
    if (self->y)
      free(self->y);
    free(self);
  }
}

