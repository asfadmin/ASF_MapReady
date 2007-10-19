#ifndef POLYGON_H
#define POLYGON_H

typedef struct
{
    int n;
    double *x;
    double *y;
} Polygon;

Polygon *polygon_new(int n, double *x, double *y);
Polygon *polygon_new_closed(int n, double *x, double *y);

int point_in_polygon(Polygon *self, double x, double y);
int polygon_overlap(Polygon *p1, Polygon *p2);
void polygon_get_bbox(Polygon *p, double *xmin, double *xmax,
                      double *ymin, double *ymax);

void polygon_free(Polygon *self);

#endif
