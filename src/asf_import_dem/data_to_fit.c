
#include "data_to_fit.h"

data_to_fit_t *
data_to_fit_new(const int grid_size, const int sparse_grid_stride)
{
  g_assert (grid_size % 2 == 1);
  data_to_fit_t *dtf = g_new(data_to_fit_t, 1);

  int mapping_count = pow ((double) grid_size, 2.0);
  dtf->grid_size = grid_size;
  dtf->n = mapping_count;
  dtf->x_proj = g_new0 (double, mapping_count);
  dtf->y_proj = g_new0 (double, mapping_count);
  dtf->x_pix = g_new0 (double, mapping_count);
  dtf->y_pix = g_new0 (double, mapping_count);

  const int sparse_grid_size = grid_size / sparse_grid_stride + 1;
  int sparse_mapping_count = pow ((double) sparse_grid_size, 2.0);
  dtf->sparse_grid_size = sparse_grid_size;
  dtf->sparse_n = sparse_mapping_count;
  dtf->sparse_x_proj = g_new0 (double, sparse_mapping_count);
  dtf->sparse_y_proj = g_new0 (double, sparse_mapping_count);
  dtf->sparse_x_pix = g_new0 (double, sparse_mapping_count);
  dtf->sparse_y_pix = g_new0 (double, sparse_mapping_count);

  return dtf;
}

void
data_to_fit_destroy(data_to_fit_t *dtf)
{
  g_free (dtf->sparse_y_pix);
  g_free (dtf->sparse_x_pix);
  g_free (dtf->sparse_y_proj);
  g_free (dtf->sparse_x_proj);
  g_free (dtf->y_pix);
  g_free (dtf->x_pix);
  g_free (dtf->y_proj);
  g_free (dtf->x_proj);
  g_free (dtf);
}
