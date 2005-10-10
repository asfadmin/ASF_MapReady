/* Functions to deal with polygon calculations  */

#ifndef POLYGON_H
#define POLYGON_H

#include "asf.h"
#include "ifm.h"

typedef struct {
   double x,y;
} Point;

/* Function prototypes */
int insidePolygon(Point *polygon, int nPoints, Point point);

#endif /* POLYGON_H */

