/* DRAWLINE.C - draw a line between coordinates X1,Y1 and X2,Y2. Fill in
		each pixel with fillvalue.
		
		Mike Shindle
*/

#include "asf.h"


#include "gps_vector.h"

#define ABS(a)		(((a) < 0) ? -(a) : (a))
#define SGN(a)		(((a) < 0) ? -1 : 1)
#define ARROWD(a,b)	((a) > (b) ? 1 : -1) 

void draw_vector_arrow(image,ax,ay,bx,by,fillvalue,mag,nrow,ncol)
BYTEIMAGE image;
int ax, bx;
int ay, by;
int fillvalue;
int mag;
int nrow,ncol;
{
  double angle, diff_angle;
  double vector_length;
  double diff_x, diff_y;
  int end_x, end_y;

  /* Lengthen vector by factor mag. Recalculate B pos. */
  if (mag != 1) {
    diff_x = (double)(bx - ax);
    diff_y = (double)(by - ay);
    angle = find_angle(diff_y,diff_x);
    vector_length = (double)mag * sqrt((diff_x * diff_x) + (diff_y * diff_y));
    bx = (int)(vector_length * cos(angle)) + ax;
    by = (int)(vector_length * sin(angle)) + ay;
  } 

  /* draw main vector */
  check_bound(&bx,&by,nrow,ncol);
  drawline(image,ax,ay,bx,by,fillvalue);
  
  /* calculate slope and degree of angle of vector */
  diff_x = (double)(bx - ax);
  diff_y = (double)(by - ay);
  angle = find_angle(diff_y,diff_x);
 
  /* draw right half of arrow head */
  diff_angle = angle - (RAD * 150.0);
  end_x = (ARROW_LENGTH * cos(diff_angle)) + bx;
  end_y = (ARROW_LENGTH * sin(diff_angle)) + by;
  check_bound(&end_x,&end_y,nrow,ncol);
  drawline(image,bx,by,end_x,end_y,fillvalue);
  
  /* draw left half of arrow head */
  diff_angle = angle + (RAD * 150.0);
  end_x = (ARROW_LENGTH * cos(diff_angle)) + bx;
  end_y = (ARROW_LENGTH * sin(diff_angle)) + by;
  check_bound(&end_x,&end_y,nrow,ncol);
  drawline(image,bx,by,end_x,end_y,fillvalue);

  return;
}

void check_bound(x,y,nr,nc)
int *x, *y;
int nr, nc;
{

   if (*x < 0)
      *x = 0;
   else if (*x > nc-1)
      *x = nc-1;

   if (*y < 0)
      *y = 0;
   else if (*y > nr-1)
      *y = nr-1;

   return;
}

double find_angle(diff_y,diff_x)
double diff_y;
double diff_x;
{
  double angle;

  if (diff_x == 0) 
     angle = SGN(diff_y) * RAD * 90.0;
  else if (diff_y == 0)
     if (diff_x < 0)
       angle = PI;
     else
       angle = 0.0;
  else if (diff_x < 0) 
     angle = atan(diff_y/diff_x) + PI;
  else
     angle = atan(diff_y/diff_x);

  return(angle);
}

void drawline(image_array,x1,y1,x2,y2,fillvalue)
BYTEIMAGE image_array;   /* 2D image that is to have areas filled */
int x1, y1;
int x2, y2;
int fillvalue;           /* value of coords in fill region */
{
  int d, x, y, ax, ay, sx, sy, dx, dy;

  dx = x2 - x1; 
  ax = ABS(dx)<<1;
  sx = SGN(dx);

  dy = y2 - y1;
  ay = ABS(dy)<<1;
  sy = SGN(dy);

  x = x1;
  y = y1;

  if (ax > ay) {
	d = ay - (ax >> 1);
	for (;;) {
	   image_array[y][x] = fillvalue;
	   if (x == x2) return;
	   if (d >= 0) {
	     y += sy;
	     d -= ax;
           }
	   x += sx;
	   d += ay;
        }
  } else {
	d = ax - (ay >> 1);
	for (;;) {
	   image_array[y][x] = fillvalue;
	   if (y == y2) return;
	   if (d >= 0) {
	     x += sx;
	     d -= ay;
           }
	   y += sy;
	   d += ax;
        }
  }
}
