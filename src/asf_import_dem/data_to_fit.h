
#ifndef _DATA_TO_FIT_H_
#define _DATA_TO_FIT_H_

#include <glib.h>

#include <asf.h>

// This is the form of the input data we want to fit splines to.
typedef struct {
  int grid_size;                // Size of grid of points, in points on a side.
  int n;                        // Number of transformed points (grid_size^2).
  double *x_proj;               // Projection x coordinates.
  double *y_proj;               // Projection y coordinates.
  // Input image pixel coordinates put 0, 0 at the top left.
  double *x_pix;                // Input image pixel x coordinate.
  double *y_pix;                // Input image pixel y coordinate.

  // These values are like the above ones, and should form a grid
  // covering the same area, but are considerably more sparse.
  int sparse_grid_size;
  int sparse_n;
  double *sparse_x_proj;
  double *sparse_y_proj;
  double *sparse_x_pix;
  double *sparse_y_pix;
} data_to_fit_t;


data_to_fit_t *data_to_fit_new(int grid_size, int sparse_grid_size);
void data_to_fit_destroy(data_to_fit_t *dtf);

#endif //_DATA_TO_FIT_H_
