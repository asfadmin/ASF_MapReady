#include "asf.h"
#include "polygon.h"

int insidePolygon(Point *polygon, int nPoints, Point point)
{
  int i, j;
  int inside=FALSE;
  for (i=0, j=nPoints-1; i<nPoints; j=i++) {
    if ((((polygon[i].y <= point.y) && (point.y < polygon[j].y)) ||
	 ((polygon[j].y <= point.y) && (point.y < polygon[i].y))) &&
	(point.x < (polygon[j].x - polygon[i].x) * (point.y - polygon[i].y) / 
	 (polygon[j].y - polygon[i].y) + polygon[i].x))
      inside=TRUE;
  }
  return inside;
}


