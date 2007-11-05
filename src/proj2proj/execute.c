#include "proj2proj.h"

typedef int project_t(project_parameters_t *pps, double lat, double lon,
      double height, double *x, double *y, double *z, datum_type_t dtm);
typedef int unproject_t(project_parameters_t *pps, double x, double y, double z,
      double *lat, double *lon, double *height, datum_type_t dtm);

static int
project_lat_long_pseudo (project_parameters_t *pps, double lat, double lon,
       double height, double *x, double *y, double *z,
             datum_type_t datum)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps; datum = datum;

  *x = lon * R2D;
  *y = lat * R2D;
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

  *lat = y * D2R;
  *lon = x * D2R;
  if (height) *height = z;

  return TRUE;
}

static void determine_projection_fns(int projection_type, project_t **project,
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
    default:
      if (project) *project = NULL;
      if (unproject) *unproject = NULL;
      break;
  }
}

static int get_int_from_entry2(const char *str, const char *widget_name)
{
    char tmp[256];
    sprintf(tmp, "%s_%s", str, widget_name);
    return get_int_from_entry(tmp);
}

static double get_double_from_entry2(const char *str, const char *widget_name)
{
    char tmp[256];
    sprintf(tmp, "%s_%s", str, widget_name);
    return get_double_from_entry(tmp);
}

static void get_projection_info(project_parameters_t *pp, 
                                projection_type_t *projection_type,
                                datum_t *datum,
                                const char *str)
{
    GtkWidget *projection_option_menu =
        get_widget_checked2(str, "projection_option_menu");

    int projection = gtk_option_menu_get_history(
        GTK_OPTION_MENU(projection_option_menu));

    switch (projection) {
      case PROJ_UTM:
          *projection_type = UNIVERSAL_TRANSVERSE_MERCATOR;
          pp->utm.zone = get_int_from_entry2(str, "utm_zone_entry");
          break;
      case PROJ_PS:
          *projection_type = POLAR_STEREOGRAPHIC;
          pp->ps.slat = get_double_from_entry2(str, "latitude_of_origin_entry");
          pp->ps.slon = get_double_from_entry2(str, "central_meridian_entry");
          pp->ps.is_north_pole = pp->ps.slat > 0;
          break;
      case PROJ_ALBERS:
          *projection_type = ALBERS_EQUAL_AREA;
          pp->albers.std_parallel1 =
              get_double_from_entry2(str, "first_standard_parallel_entry");
          pp->albers.std_parallel2 =
              get_double_from_entry2(str, "second_standard_parallel_entry");
          pp->albers.center_meridian =
              get_double_from_entry2(str, "central_meridian_entry");
          pp->albers.orig_latitude =
              get_double_from_entry2(str, "latitude_of_origin_entry");
          break;
      case PROJ_LAMAZ:
          *projection_type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
          pp->lamaz.center_lon =
              get_double_from_entry2(str, "central_meridian_entry");
          pp->lamaz.center_lat =
              get_double_from_entry2(str, "latitude_of_origin_entry");
          break;
      case PROJ_LAMCC:
          *projection_type = LAMBERT_CONFORMAL_CONIC;
          pp->lamcc.plat1 =
              get_double_from_entry2(str, "first_standard_parallel_entry");
          pp->lamcc.plat2 =
              get_double_from_entry2(str, "second_standard_parallel_entry");
          pp->lamcc.lon0 =
              get_double_from_entry2(str, "central_meridian_entry");
          pp->lamcc.lat0 =
              get_double_from_entry2(str, "latitude_of_origin_entry");
          break;
      case PROJ_LATLON:
          *projection_type = LAT_LONG_PSEUDO_PROJECTION;
          break;
      default:
          asfPrintError("Impossible.\n");
          break;
    }

    GtkWidget *datum_option_menu =
        get_widget_checked2(str, "datum_option_menu");

    int datum_sel = gtk_option_menu_get_history(
        GTK_OPTION_MENU(datum_option_menu));

    switch (datum_sel) {
        default:
        case 0: *datum = WGS84_DATUM; break;
        case 1: *datum = NAD27_DATUM; break;
        case 2: *datum = NAD83_DATUM; break;
        case 3: *datum = HUGHES_DATUM; break;
    }
}

static void execute(const char *from, const char *to)
{
    // from/to: either "source" or "target"
    // they just indicate which textview/projparms widgets to grab from
    projection_parameters_t source_pp, target_pp;
    projection_type_t source_proj, target_proj;
    datum_t source_datum, target_datum;

    get_projection_info(&source_pp, &source_proj, &source_datum, from);
    get_projection_info(&target_pp, &target_proj, &target_datum, to);

    project_t source_proj_fn, target_proj_fn;
    unproject_t source_unproj_fn, target_unproj_fn;

    determine_projection_fns(source_proj, &source_proj_fn, &source_unproj_fn);
    determine_projection_fns(target_proj, &target_proj_fn, &target_unproj_fn);

    GtkWidget *source_tv = get_widget_checked2(from, "textview");
    GtkWidget *target_tv = get_widget_checked2(to, "textview");
}

SIGNAL_CALLBACK void
on_forward_button_clicked(GtkWidget * widget)
{
    execute("source", "target");
}

SIGNAL_CALLBACK void
on_backward_button_clicked(GtkWidget * widget)
{
    execute("target", "source");
}
