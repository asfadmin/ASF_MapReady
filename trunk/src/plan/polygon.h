#ifndef POLYGON_H
#define POLYGON_H

typedef struct
{
    int n;
    double *x;
    double *y;
} Poly;

Poly *polygon_new(int n, double *x, double *y);
Poly *polygon_new_closed(int n, double *x, double *y);

int point_in_polygon(Poly *self, double x, double y);
int polygon_overlap(Poly *p1, Poly *p2);
void polygon_get_bbox(Poly *p, double *xmin, double *xmax,
                      double *ymin, double *ymax);
double polygon_area(Poly *p);
double polygon_perimeter(Poly *p);
void polygon_free(Poly *self);

#endif
