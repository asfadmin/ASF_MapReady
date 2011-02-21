typedef int project_t(project_parameters_t *pps, double lat, double lon,
      double height, double *x, double *y, double *z, datum_type_t dtm);
typedef int unproject_t(project_parameters_t *pps, double x, double y,
      double z, double *lat, double *lon, double *height, datum_type_t dtm);

static int
project_lat_long_pseudo (project_parameters_t *pps, double lat, double lon,
       double height, double *x, double *y, double *z, datum_type_t datum)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps; datum = datum;

  *y = lon * R2D;
  *x = lat * R2D;
  if (z) *z = height;

  return TRUE;
}

static int
project_lat_long_pseudo_inv (project_parameters_t *pps, double x, double y,
           double z, double *lat, double *lon,
           double *height, datum_type_t datum)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps; datum = datum;

  *lon = y * D2R;
  *lat = x * D2R;
  if (height) *height = z;

  return TRUE;
}

static void determine_projection_fns(int projection_type,
                                     project_t **project,
                                     unproject_t **unproject)
{
  switch ( projection_type ) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      if (project) *project = project_utm;
      if (unproject) *unproject = project_utm_inv;
      break;
    case POLAR_STEREOGRAPHIC:
      if (project) *project = project_ps;
      if (unproject) *unproject = project_ps_inv;
      break;
    case ALBERS_EQUAL_AREA:
      if (project) *project = project_albers;
      if (unproject) *unproject = project_albers_inv;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      if (project) *project = project_lamcc;
      if (unproject) *unproject = project_lamcc_inv;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      if (project) *project = project_lamaz;
      if (unproject) *unproject = project_lamaz_inv;
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      if (project) *project = project_lat_long_pseudo;
      if (unproject) *unproject = project_lat_long_pseudo_inv;
      break;
    case EQUI_RECTANGULAR:
      if (project) *project = project_eqr;
      if (unproject) *unproject = project_eqr_inv;
      break;
    case MERCATOR:
      if (project) *project = project_mer;
      if (unproject) *unproject = project_mer_inv;
      break;
    default:
      if (project) *project = NULL;
      if (unproject) *unproject = NULL;
      break;
  }
}
