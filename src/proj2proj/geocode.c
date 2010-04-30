#include "proj2proj.h"
#include "asf_geocode.h"
#include "asf_nan.h"

const char * datum_string(int datum)
{
    switch (datum)
    {
    default:
    case DATUM_WGS84:
        return "WGS84";
    case DATUM_NAD27:
        return "NAD27";
    case DATUM_NAD83:
        return "NAD83";
    case DATUM_HUGHES:
        return "HUGHES";
    case DATUM_ITRF97:
      return "ITRF97";
    case DATUM_ED50:
      return "ED50";
    case DATUM_SAD69:
      return "SAD69";
    }
}

const char *spheroid_string(int spheroid)
{
  switch (spheroid)
    {
    default:
    case SPHEROID_UNKNOWN:
      return "";
    case SPHEROID_WGS84:
      return "WGS84";
    case SPHEROID_HUGHES:
      return "HUGHES";
    case SPHEROID_GRS1967:
      return "GRS1967";
    case SPHEROID_GRS1980:
      return "GRS1980";
    case SPHEROID_INTERNATIONAL1924:
      return "INTERNATIONAL1924";
    }
}

static const gchar * double_to_string(double value)
{
    static gchar buf[32];

    /* in this context, NAN means "not specified", so leave blank */
    if (ISNAN(value))
        return "";

    snprintf(buf, sizeof(buf), "%lf", value);
    return buf;
}

void geocode_options_changed(int is_source)
{
    GtkWidget *table_utm_projection_options =
        get_widget_checked2(is_source, "table_utm_projection_options");

    GtkWidget *table_nonutm_projection_options =
        get_widget_checked2(is_source, "table_nonutm_projection_options");

    GtkWidget *projection_option_menu =
        get_widget_checked2(is_source, "projection_option_menu");

    GtkWidget *predefined_projection_option_menu =
        get_widget_checked2(is_source, "predefined_projection_option_menu");

    GtkWidget *utm_zone_entry =
        get_widget_checked2(is_source, "utm_zone_entry");

    GtkWidget *central_meridian_entry =
        get_widget_checked2(is_source, "central_meridian_entry");

    GtkWidget *latitude_of_origin_entry =
        get_widget_checked2(is_source, "latitude_of_origin_entry");

    GtkWidget *first_standard_parallel_entry =
        get_widget_checked2(is_source, "first_standard_parallel_entry");

    GtkWidget *second_standard_parallel_entry =
        get_widget_checked2(is_source, "second_standard_parallel_entry");

    GtkWidget *datum_option_menu =
        get_widget_checked2(is_source, "datum_option_menu");

    int projection =
        gtk_option_menu_get_history(GTK_OPTION_MENU(projection_option_menu));

    int enable_table_utm_projection_options = projection == PROJ_UTM;
    int enable_table_nonutm_projection_options =
      projection != PROJ_UTM && projection != PROJ_LATLON;

    datum_type_t datum = WGS84_DATUM;
    spheroid_type_t spheroid = WGS84_SPHEROID;
    int datum_selection = DATUM_WGS84;
    int predefined_projection_is_selected =
      0 < gtk_option_menu_get_history(
        GTK_OPTION_MENU(predefined_projection_option_menu));

    int enable_projection_option_menu = TRUE;
    int enable_predefined_projection_option_menu = TRUE;
    int enable_datum_hbox = TRUE;

    int enable_utm_zone = FALSE;
    int enable_central_meridian = FALSE;
    int enable_latitude_of_origin = FALSE;
    int enable_first_standard_parallel = FALSE;
    int enable_second_standard_parallel = FALSE;

    if (predefined_projection_is_selected)
    {
      /* all widgets remain disabled -- load settings from file */
      project_parameters_t * pps =
        load_selected_predefined_projection_parameters(is_source, projection, 
						       &datum, &spheroid);

      if (!pps) {
        predefined_projection_is_selected = FALSE;
      }
      else {
        gtk_entry_set_text(GTK_ENTRY(utm_zone_entry), "");
        gtk_entry_set_text(GTK_ENTRY(central_meridian_entry), "");
        gtk_entry_set_text(GTK_ENTRY(latitude_of_origin_entry), "");
        gtk_entry_set_text(GTK_ENTRY(first_standard_parallel_entry), "");
        gtk_entry_set_text(GTK_ENTRY(second_standard_parallel_entry), "");

        switch(datum) {
          case NAD27_DATUM:
            datum_selection = DATUM_NAD27;
            break;
          case NAD83_DATUM:
            datum_selection = DATUM_NAD83;
            break;
          case HUGHES_DATUM:
            datum_selection = DATUM_HUGHES;
            break;
	  case ITRF97_DATUM:
	    datum_selection = DATUM_ITRF97;
	    break;
	  case ED50_DATUM:
	    datum_selection = DATUM_ED50;
	    break;
	  case SAD69_DATUM:
	    datum_selection = DATUM_SAD69;
	    break;
          case WGS84_DATUM:
          default:
            datum_selection = DATUM_WGS84;
            break;
        }
        set_combo_box_item(datum_option_menu, datum_selection);
        enable_datum_hbox = FALSE;

        switch (projection)
        {
          case PROJ_UTM:
            /* no UTM predefined projections -- better to use
               the "zone" entry */
            assert(FALSE);
            break;

          case PROJ_PS:
            gtk_entry_set_text(
              GTK_ENTRY(central_meridian_entry),
              double_to_string(pps->ps.slon));
            gtk_entry_set_text(
              GTK_ENTRY(first_standard_parallel_entry),
              double_to_string(pps->ps.slat));
            break;

          case PROJ_LAMCC:
            gtk_entry_set_text(
              GTK_ENTRY(first_standard_parallel_entry),
              double_to_string(pps->lamcc.plat1));
            gtk_entry_set_text(
              GTK_ENTRY(second_standard_parallel_entry),
              double_to_string(pps->lamcc.plat2));
            gtk_entry_set_text(
              GTK_ENTRY(central_meridian_entry),
              double_to_string(pps->lamcc.lon0));
            gtk_entry_set_text(
              GTK_ENTRY(latitude_of_origin_entry),
              double_to_string(pps->lamcc.lat0));
            break;

          case PROJ_LAMAZ:
            gtk_entry_set_text(
              GTK_ENTRY(central_meridian_entry),
              double_to_string(pps->lamaz.center_lon));
            gtk_entry_set_text(
              GTK_ENTRY(latitude_of_origin_entry),
              double_to_string(pps->lamaz.center_lat));
            break;

          case PROJ_ALBERS:
            gtk_entry_set_text(
              GTK_ENTRY(first_standard_parallel_entry),
              double_to_string(pps->albers.std_parallel1));
            gtk_entry_set_text(
              GTK_ENTRY(second_standard_parallel_entry),
              double_to_string(pps->albers.std_parallel2));
            gtk_entry_set_text(
              GTK_ENTRY(central_meridian_entry),
              double_to_string(pps->albers.center_meridian));
            gtk_entry_set_text(
              GTK_ENTRY(latitude_of_origin_entry),
              double_to_string(pps->albers.orig_latitude));
            break;
        }

        g_free(pps);
      }
    }

    if (!predefined_projection_is_selected)
    {
      switch (projection)
      {
        case PROJ_UTM:
          enable_utm_zone = TRUE;
          enable_predefined_projection_option_menu = FALSE;
          break;

        case PROJ_LATLON:
          enable_predefined_projection_option_menu = FALSE;
          break;

        case PROJ_PS:
          enable_central_meridian = TRUE;
          enable_first_standard_parallel = TRUE;

          gtk_entry_set_text(GTK_ENTRY(latitude_of_origin_entry), "");
          gtk_entry_set_text(GTK_ENTRY(second_standard_parallel_entry), "");

          break;

        case PROJ_LAMCC:
          enable_first_standard_parallel = TRUE;
          enable_second_standard_parallel = TRUE;
          enable_central_meridian = TRUE;
          enable_latitude_of_origin = TRUE;
          break;

        case PROJ_LAMAZ:
          enable_central_meridian = TRUE;
          enable_latitude_of_origin = TRUE;

          gtk_entry_set_text(GTK_ENTRY(first_standard_parallel_entry), "");
          gtk_entry_set_text(GTK_ENTRY(second_standard_parallel_entry), "");

          break;

        case PROJ_ALBERS:
          enable_first_standard_parallel = TRUE;
          enable_second_standard_parallel = TRUE;
          enable_central_meridian = TRUE;
          enable_latitude_of_origin = TRUE;
          break;
      }
    }

    set_predefined_projections(is_source, projection);

    if (enable_table_utm_projection_options)
    {
        gtk_widget_show(table_utm_projection_options);
        gtk_widget_hide(table_nonutm_projection_options);
    }
    else if (enable_table_nonutm_projection_options)
    {
        gtk_widget_show(table_nonutm_projection_options);
        gtk_widget_hide(table_utm_projection_options);
    }
    else
    {
        gtk_widget_hide(table_nonutm_projection_options);
        gtk_widget_hide(table_utm_projection_options);
    }

    gtk_widget_set_sensitive(projection_option_menu,
        enable_projection_option_menu);

    gtk_widget_set_sensitive(predefined_projection_option_menu,
        enable_predefined_projection_option_menu &&
        !enable_table_utm_projection_options);

    GtkWidget *utm_zone_label =
        get_widget_checked2(is_source, "utm_zone_label");
    GtkWidget *central_meridian_label =
        get_widget_checked2(is_source, "central_meridian_label");
    GtkWidget *latitude_of_origin_label =
        get_widget_checked2(is_source, "latitude_of_origin_label");
    GtkWidget *first_standard_parallel_label =
        get_widget_checked2(is_source, "first_standard_parallel_label");
    GtkWidget *second_standard_parallel_label =
        get_widget_checked2(is_source, "second_standard_parallel_label");
    GtkWidget *datum_hbox =
        get_widget_checked2(is_source, "datum_hbox");

    gtk_widget_set_sensitive(utm_zone_entry, enable_utm_zone);
    gtk_widget_set_sensitive(utm_zone_label, enable_utm_zone);

    gtk_widget_set_sensitive(central_meridian_entry, enable_central_meridian);
    gtk_widget_set_sensitive(central_meridian_label, enable_central_meridian);

    gtk_widget_set_sensitive(latitude_of_origin_entry,
        enable_latitude_of_origin);
    gtk_widget_set_sensitive(latitude_of_origin_label,
        enable_latitude_of_origin);

    gtk_widget_set_sensitive(first_standard_parallel_entry,
        enable_first_standard_parallel);
    gtk_widget_set_sensitive(first_standard_parallel_label,
        enable_first_standard_parallel);

    gtk_widget_set_sensitive(second_standard_parallel_entry,
        enable_second_standard_parallel);
    gtk_widget_set_sensitive(second_standard_parallel_label,
        enable_second_standard_parallel);

    gtk_widget_set_sensitive(datum_hbox, enable_datum_hbox);
}

SIGNAL_CALLBACK void
on_source_albers_equal_area_conic_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_lambert_conformal_conic_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_lambert_azimuthal_equal_area_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_polar_stereographic_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_utm_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_latlon_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_wgs84_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_hughes_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_nad27_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_source_nad83_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_target_albers_equal_area_conic_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_lambert_conformal_conic_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_lambert_azimuthal_equal_area_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_polar_stereographic_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_utm_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_latlon_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_wgs84_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_hughes_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_nad27_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_target_nad83_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

static void projection_option_menu_changed(int is_source)
{
  GtkWidget *utm_zone_entry =
      get_widget_checked2(is_source, "utm_zone_entry");

  GtkWidget *central_meridian_entry =
      get_widget_checked2(is_source, "central_meridian_entry");

  GtkWidget *latitude_of_origin_entry =
      get_widget_checked2(is_source, "latitude_of_origin_entry");

  GtkWidget *first_standard_parallel_entry =
      get_widget_checked2(is_source, "first_standard_parallel_entry");

  GtkWidget *second_standard_parallel_entry =
      get_widget_checked2(is_source, "second_standard_parallel_entry");

  gtk_entry_set_text(GTK_ENTRY(utm_zone_entry), "");
  gtk_entry_set_text(GTK_ENTRY(central_meridian_entry), "");
  gtk_entry_set_text(GTK_ENTRY(latitude_of_origin_entry), "");
  gtk_entry_set_text(GTK_ENTRY(first_standard_parallel_entry), "");
  gtk_entry_set_text(GTK_ENTRY(second_standard_parallel_entry), "");

  geocode_options_changed(is_source);
}

SIGNAL_CALLBACK void
on_source_projection_option_menu_changed(GtkWidget * widget)
{
    projection_option_menu_changed(TRUE);
}

SIGNAL_CALLBACK void
on_target_projection_option_menu_changed(GtkWidget * widget)
{
    projection_option_menu_changed(FALSE);
}


SIGNAL_CALLBACK void
on_source_predefined_projection_option_menu_changed(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_target_predefined_projection_option_menu_changed(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}

SIGNAL_CALLBACK void
on_source_user_defined_activate(GtkWidget * widget)
{
    geocode_options_changed(TRUE);
}

SIGNAL_CALLBACK void
on_target_user_defined_activate(GtkWidget * widget)
{
    geocode_options_changed(FALSE);
}
