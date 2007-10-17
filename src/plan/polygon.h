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
void polygon_free(Polygon *self);

#endif
