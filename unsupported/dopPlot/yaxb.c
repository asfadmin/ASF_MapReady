/******************************************************************************
NAME: yaxb.c

SYNOPSIS:	yaxb(float x_vec[], float y_vec[], int n, float *a, float *b)

DESCRIPTION:	Computes a and b for y = ax + b using linear regression
		given float vectors y and x of length n.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


yaxb(x_vec, y_vec, n, a, b)

 float x_vec[], y_vec[];
 int   n;
 float *a, *b;
{
 float sum_x, sum_xx, sum_xy, sum_y;
 float d, at, bt;
 int   i;

 sum_x = 0.0;
 sum_xx = 0.0;
 sum_y = 0.0;
 sum_xy = 0.0;

 for (i=0; i<n; i++)
  {
    sum_x += x_vec[i];
    sum_y += y_vec[i];
    sum_xx += x_vec[i] * x_vec[i];
    sum_xy += x_vec[i] * y_vec[i];
  }

 d =  n * sum_xx - sum_x*sum_x;
 at = n * sum_xy - sum_x*sum_y;
 bt = sum_xx * sum_y - sum_x * sum_xy;

 *a = at/d;
 *b = bt/d;

 return(1);
}

