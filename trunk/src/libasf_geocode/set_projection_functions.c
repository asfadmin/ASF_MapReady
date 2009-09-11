#include <asf_meta.h>
#include <libasf_proj.h>

#include "asf_geocode.h"

// Given projection_type, return pointers to functions capable of
// projecting/unprojecting points or arrays of points from/to (lat,
// long, height) tuples.
void set_projection_functions (projection_type_t projection_type, 
			       projector_t *project, projector_t *unproject,
			       array_projector_t *array_project,
			       array_projector_t *array_unproject)
{
  switch ( projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    *project = project_utm;
    *unproject = project_utm_inv;
    *array_project = project_utm_arr;
    *array_unproject = project_utm_arr_inv;
    break;
  case POLAR_STEREOGRAPHIC:
    *project = project_ps;
    *unproject = project_ps_inv;
    *array_project = project_ps_arr;
    *array_unproject = project_ps_arr_inv;
    break;
  case ALBERS_EQUAL_AREA:
    *project = project_albers;
    *unproject = project_albers_inv;
    *array_project = project_albers_arr;
    *array_unproject = project_albers_arr_inv;
    break;
  case LAMBERT_CONFORMAL_CONIC:
    *project = project_lamcc;
    *unproject = project_lamcc_inv;
    *array_project = project_lamcc_arr;
    *array_unproject = project_lamcc_arr_inv;
    break;
  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    *project = project_lamaz;
    *unproject = project_lamaz_inv;
    *array_project = project_lamaz_arr;
    *array_unproject = project_lamaz_arr_inv;
    break;
  case LAT_LONG_PSEUDO_PROJECTION:
    *project = project_pseudo;
    *unproject = project_pseudo_inv;
    *array_project = project_pseudo_arr;
    *array_unproject = project_pseudo_arr_inv;
    break;
  default:
    g_assert_not_reached ();
  }
}
