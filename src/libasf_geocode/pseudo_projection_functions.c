// These functions are analagous to the ones like project_utm,
// project_utm_inv, project_utm_arr_inv, etc. in (as of this writing)
// libasf_proj, only they do pretty much nothing except convert
// degrees to radians.  FIXME: they should do datum transformations
// and should be moved into libasf_proj proper.

int 
project_pseudo (project_parameters_t *pps, double lat, double lon,
		double height, double *x, double *y, double *z)
{
  // At the moment we don't honor all the stuff in libasf_proj that
  // lets the clients signal that they don't care about heights.
  g_assert (z != ASF_PROJ_NO_HEIGHT && height != NULL);

  /* Silence compiler warning about unused argument.  */
  pps = pps;		

  *x = lon * R2D;
  *y = lat * R2D;
  *z = height;

  return TRUE;
}

int
project_pseudo_inv (project_parameters_t *pps, double x, double y,
		    double z, double *lat, double *lon, double *height)
{
  // At the moment we don't honor all the stuff in libasf_proj that
  // lets the clients signal that they don't care about heights.
  g_assert (z != ASF_PROJ_NO_HEIGHT && height != NULL);

  /* Silence compiler warning about unused argument.  */
  pps = pps;

  *lat = y * D2R;
  *lon = x * D2R;
  *height = z;

  return TRUE;
}

int 
project_pseudo_arr (project_parameters_t *pps, double *lat, double *lon,
		    double *height, double **x, double **y, double **z,
		    long length)
{
  // At the moment we don't honor all the stuff in libasf_proj that
  // lets the clients signal that they don't care about heights.
  g_assert (height != NULL);

  // Honor the convention used in libasf_proj that says that passing
  // pointers to NULL pointers means that the memory to hold the
  // return values is to be allocated by by the called routing.
  if ( !(*x) || !(*y) || !(*z) ) {
    g_assert (!(*x) && !(*y) && !(*z)); // All or none.

    *x = MALLOC (sizeof (double) * length);
    *y = MALLOC (sizeof (double) * length);
    *z = MALLOC (sizeof (double) * length);
  }

  /* Silence compiler warning about unused argument.  */
  pps = pps;		

  long ii;
  for ( ii = 0 ; ii < length ; ii++ ) {
    (*x)[ii] = lon[ii] * R2D;
    (*y)[ii] = lat[ii] * R2D;
    (*z)[ii] = height[ii];
  }

  return TRUE;
}

int
project_pseudo_inv_arr (project_parameters_t *pps, double *x, double *y,
			double *z, double **lat, double **lon,
			double **height)
{
  // At the moment we don't honor all the stuff in libasf_proj that
  // lets the clients signal that they don't care about heights.
  g_assert (height != NULL);

  // Honor the convention used in libasf_proj that says that passing
  // pointers to NULL pointers means that the memory to hold the
  // return values is to be allocated by by the called routing.
  if ( !(*lat) || !(*lon) || !(*height) ) {
    g_assert (!(*lat) && !(*lon) && !(*height)); // All or none.

    *lat = MALLOC (sizeof (double) * length);
    *lon = MALLOC (sizeof (double) * length);
    *height = MALLOC (sizeof (double) * length);
  }

  /* Silence compiler warning about unused argument.  */
  pps = pps;


  long ii;
  for ( ii = 0 ; ii < length ; ii++ ) {
    (*lat)[ii] = y[ii] * R2D;
    (*lon)[ii] = x[ii] * R2D;
    (*height)[ii] = z[ii];
  }

  return TRUE;
}
