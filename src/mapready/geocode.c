#include "mapready.h"
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

const char * resample_method_string(int resample_method)
{
    switch (resample_method) {
    case RESAMPLE_NEAREST_NEIGHBOR:
        return "nearest_neighbor";
    default:
    case RESAMPLE_BILINEAR:
        return "bilinear";
    case RESAMPLE_BICUBIC:
        return "bicubic";
    }
}

static int entry_has_text(const char * entry_name)
{
    GtkEntry *entry = GTK_ENTRY(get_widget_checked(entry_name));
    return strlen(gtk_entry_get_text(entry)) > 0;
}

const char * geocode_options_string(const Settings * settings)
{
    static gchar ret[1024];

    if (settings->geocode_is_checked)
    {
        gboolean enable_utm_zone = FALSE;
        gboolean enable_central_meridian = FALSE;
        gboolean enable_latitude_of_origin = FALSE;
        gboolean enable_first_standard_parallel = FALSE;
        gboolean enable_second_standard_parallel = FALSE;
        gboolean enable_pole_switch = FALSE;

        switch (settings->projection)
        {
        case PROJ_UTM:
            strcpy(ret, "--projection utm");
            enable_utm_zone =
                entry_has_text("utm_zone_entry");
            break;

        case PROJ_PS:
            strcpy(ret, "--projection ps");
            enable_central_meridian =
                entry_has_text("central_meridian_entry");
            enable_first_standard_parallel =
                entry_has_text("first_standard_parallel_entry");
            enable_pole_switch = TRUE;
            break;

        case PROJ_LAMCC:
            strcpy(ret, "--projection lamcc");
            enable_first_standard_parallel =
                entry_has_text("first_standard_parallel_entry");
            enable_second_standard_parallel =
                entry_has_text("second_standard_parallel_entry");
            enable_central_meridian =
                entry_has_text("central_meridian_entry");
            enable_latitude_of_origin =
                entry_has_text("latitude_of_origin_entry");
            break;

        case PROJ_LAMAZ:
            strcpy(ret, "--projection lamaz");
            enable_central_meridian =
                entry_has_text("central_meridian_entry");
            enable_latitude_of_origin =
                entry_has_text("latitude_of_origin_entry");
            break;

        case PROJ_ALBERS:
            strcpy(ret, "--projection albers");
            enable_first_standard_parallel =
                entry_has_text("first_standard_parallel_entry");
            enable_second_standard_parallel =
                entry_has_text("second_standard_parallel_entry");
            enable_central_meridian =
                entry_has_text("central_meridian_entry");
            enable_latitude_of_origin =
                entry_has_text("latitude_of_origin_entry");
            break;
        case PROJ_GEO:
            strcpy(ret, "--projection geographic");
            break;

        case PROJ_MER:
	  strcpy(ret, "--projection mer");
	  enable_first_standard_parallel = 
	    entry_has_text("first_standard_parallel_entry");
	  enable_central_meridian =
	    entry_has_text("central_meridian_entry");
	  enable_latitude_of_origin =
	    entry_has_text("latitude_of_origin_entry");
	  break;
	case PROJ_EQR:
	  strcpy(ret, "--projection eqr");
	  enable_central_meridian =
	    entry_has_text("central_meridian_entry");
	  enable_latitude_of_origin =
	    entry_has_text("latitude_of_origin_entry");
	  break;
	}


        if (enable_utm_zone)
            sprintf(ret, "%s --zone %d ", ret, settings->zone);

        if (enable_central_meridian)
            sprintf(ret, "%s --central-meridian %f ", ret, settings->lon0);

        if (enable_latitude_of_origin)
            sprintf(ret, "%s --latitude-of-origin %f ", ret, settings->lat0);

        if (enable_first_standard_parallel)
            sprintf(ret, "%s --first-standard-parallel %f ",
                ret, settings->plat1);

        if (enable_second_standard_parallel)
            sprintf(ret, "%s --second-standard-parallel %f ",
                ret, settings->plat2);

      if (enable_pole_switch)
          sprintf(ret, "%s --%s-pole", ret,
            settings->plat1 < 0 ? "south" : "north");

        if (settings->specified_height)
            sprintf(ret, "%s --height %f ", ret, settings->height);

        if (settings->specified_pixel_size)
            sprintf(ret, "%s --pixel-size %f ", ret, settings->pixel_size);

        sprintf(ret, "%s --datum %s ", ret, datum_string(settings->datum));

        sprintf(ret, "%s --resample-method %s ", ret,
            resample_method_string (settings->resample_method));

        if (settings->geocode_force)
            sprintf(ret, "%s --force ", ret);
    }
    else
    {
        strcpy(ret, "");
    }

    return ret;
}

static const gchar * double_to_string(double value)
{
    static gchar buf[32];

    /* in this context, NAN means "not specified", so leave blank */
    if (ISNAN(value))
        return "";

    snprintf(buf, sizeof(buf), "%f", value);
    return buf;
}

void geocode_options_changed()
{
    GtkWidget * projection_option_menu;
    GtkWidget * geocode_checkbutton;
    GtkWidget * predefined_projection_option_menu;

    GtkWidget * table_nonutm_projection_options;
    GtkWidget * table_utm_projection_options;

    GtkWidget * utm_zone_label;
    GtkWidget * utm_zone_entry;

    GtkWidget * central_meridian_entry;
    GtkWidget * central_meridian_label;

    GtkWidget * latitude_of_origin_entry;
    GtkWidget * latitude_of_origin_label;

    GtkWidget * first_standard_parallel_entry;
    GtkWidget * first_standard_parallel_label;

    GtkWidget * second_standard_parallel_entry;
    GtkWidget * second_standard_parallel_label;

    GtkWidget * false_northing_entry;
    GtkWidget * false_northing_label;

    GtkWidget * false_easting_entry;
    GtkWidget * false_easting_label;

    GtkWidget * average_height_checkbutton;
    GtkWidget * pixel_size_checkbutton;

    GtkWidget * geocode_pixel_size_label;
    const char *geocode_pixel_size_label_text = " meters";

    GtkWidget * hbox_average_height;
    GtkWidget * hbox_pixel_size;

    GtkWidget * datum_hbox;
    GtkWidget * datum_option_menu;

    GtkWidget * resample_hbox;
    GtkWidget * force_checkbutton;

    GtkWidget * vbox_geocode;
    GtkWidget * geocode_tab_label;

    GtkWidget * spheroid_entry;

    gboolean geocode_projection_is_checked;
    gboolean predefined_projection_is_selected;
    gboolean average_height_is_checked;
    gboolean pixel_size_is_checked;
    gint projection;

    gboolean enable_projection_option_menu = FALSE;
    gboolean enable_predefined_projection_option_menu = FALSE;

    gboolean geographic_is_selected = FALSE;
    gboolean enable_table_utm_projection_options = TRUE;

    gboolean enable_utm_zone = FALSE;
    gboolean enable_central_meridian = FALSE;
    gboolean enable_latitude_of_origin = FALSE;
    gboolean enable_first_standard_parallel = FALSE;
    gboolean enable_second_standard_parallel = FALSE;
    gboolean enable_false_northing = FALSE;
    gboolean enable_false_easting = FALSE;

    gboolean enable_average_height_checkbutton = FALSE;
    gboolean enable_average_height_entry = FALSE;

    gboolean enable_pixel_size_checkbutton = FALSE;
    gboolean enable_pixel_size_entry = FALSE;

    gboolean enable_datum_hbox = FALSE;

    gboolean enable_resample_hbox = FALSE;

    gboolean enable_force_checkbutton = FALSE;

    table_utm_projection_options =
        get_widget_checked("table_utm_projection_options");

    table_nonutm_projection_options =
        get_widget_checked("table_nonutm_projection_options");

    geocode_checkbutton =
        get_widget_checked("geocode_checkbutton");

    geocode_projection_is_checked =
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(geocode_checkbutton));

    average_height_checkbutton =
        get_widget_checked("average_height_checkbutton");

    pixel_size_checkbutton =
        get_widget_checked("pixel_size_checkbutton");

    geocode_pixel_size_label =
        get_widget_checked("geocode_pixel_size_label");

    projection_option_menu =
        get_widget_checked("projection_option_menu");

    predefined_projection_option_menu =
        get_widget_checked("predefined_projection_option_menu");

    utm_zone_entry =
        get_widget_checked("utm_zone_entry");

    central_meridian_entry =
        get_widget_checked("central_meridian_entry");

    latitude_of_origin_entry =
        get_widget_checked("latitude_of_origin_entry");

    first_standard_parallel_entry =
        get_widget_checked("first_standard_parallel_entry");

    second_standard_parallel_entry =
        get_widget_checked("second_standard_parallel_entry");

    false_northing_entry =
        get_widget_checked("false_northing_entry");

    false_easting_entry =
        get_widget_checked("false_easting_entry");

    datum_option_menu =
        get_widget_checked("datum_option_menu");

    spheroid_entry =
        get_widget_checked("spheroid_entry");

    projection =
        gtk_option_menu_get_history(GTK_OPTION_MENU(projection_option_menu));

    geographic_is_selected =
        projection == PROJ_GEO;

    enable_table_utm_projection_options =
        projection == PROJ_UTM;

    if (geocode_projection_is_checked)
    {
        vbox_geocode = get_widget_checked("vbox_geocode");
        geocode_tab_label = get_widget_checked("geocode_tab_label");
        gtk_widget_set_sensitive(vbox_geocode, TRUE);
        gtk_widget_set_sensitive(geocode_tab_label, TRUE);

        datum_type_t datum = WGS84_DATUM;
				spheroid_type_t spheroid = WGS84_SPHEROID;
        int datum_selection = DATUM_WGS84;
				int spheroid_selection = SPHEROID_WGS84;
        predefined_projection_is_selected =
            0 < gtk_option_menu_get_history(
            GTK_OPTION_MENU(predefined_projection_option_menu));

        enable_projection_option_menu = TRUE;
        enable_predefined_projection_option_menu = TRUE;
        enable_datum_hbox = TRUE;

        if (geographic_is_selected)
        {
            // everything is disabled except resample method, pixel size
            enable_datum_hbox = FALSE;
            enable_predefined_projection_option_menu = FALSE;
            // Commenting this out, we will specify pixel size in meters for
            // all projections, even lat/lon pseudo
            geocode_pixel_size_label_text = " degrees";
        }
        else if (predefined_projection_is_selected)
        {
            /* all widgets remain disabled -- load settings from file */
            project_parameters_t * pps =
	      load_selected_predefined_projection_parameters(projection, 
							     &datum, &spheroid);

            if (!pps)
            {
                predefined_projection_is_selected = FALSE;
            }
            else
            {
                gtk_entry_set_text(
                    GTK_ENTRY(utm_zone_entry), "");
                gtk_entry_set_text(
                    GTK_ENTRY(central_meridian_entry), "");
                gtk_entry_set_text(
                    GTK_ENTRY(latitude_of_origin_entry), "");
                gtk_entry_set_text(
                    GTK_ENTRY(first_standard_parallel_entry), "");
                gtk_entry_set_text(
                    GTK_ENTRY(second_standard_parallel_entry), "");
                gtk_entry_set_text(
                    GTK_ENTRY(false_northing_entry), "");
                gtk_entry_set_text(
                    GTK_ENTRY(false_easting_entry), "");
		gtk_entry_set_text(
                    GTK_ENTRY(spheroid_entry), "");
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

		if (spheroid == WGS84_SPHEROID)
		  spheroid_selection = SPHEROID_WGS84;
		else if (spheroid == HUGHES_SPHEROID) 
		  spheroid_selection = SPHEROID_HUGHES;
		else if (spheroid == GRS1967_SPHEROID)
		  spheroid_selection = SPHEROID_GRS1967;
		else if (spheroid == GRS1980_SPHEROID)
		  spheroid_selection = SPHEROID_GRS1980;
		else if (spheroid == INTERNATIONAL1924_SPHEROID) 
		  spheroid_selection = SPHEROID_INTERNATIONAL1924;
		else
		  spheroid_selection = SPHEROID_UNKNOWN;
		gtk_entry_set_text(GTK_ENTRY(spheroid_entry),
				   spheroid_string(spheroid_selection));


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
                    gtk_entry_set_text(
                        GTK_ENTRY(false_northing_entry),
                            double_to_string(pps->ps.false_northing));
                    gtk_entry_set_text(
                        GTK_ENTRY(false_easting_entry),
                            double_to_string(pps->ps.false_easting));
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

		case PROJ_MER:
                    gtk_entry_set_text(
                        GTK_ENTRY(first_standard_parallel_entry),
                            double_to_string(pps->mer.standard_parallel));
                    gtk_entry_set_text(
                        GTK_ENTRY(central_meridian_entry),
                            double_to_string(pps->mer.central_meridian));
                    gtk_entry_set_text(
                        GTK_ENTRY(latitude_of_origin_entry),
                            double_to_string(pps->mer.orig_latitude));
                    break;
		  
		case PROJ_EQR:
                    gtk_entry_set_text(
                        GTK_ENTRY(central_meridian_entry),
                            double_to_string(pps->eqr.central_meridian));
                    gtk_entry_set_text(
                        GTK_ENTRY(latitude_of_origin_entry),
                            double_to_string(pps->eqr.orig_latitude));
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
                break;

            case PROJ_PS:
                enable_central_meridian = TRUE;
                enable_first_standard_parallel = TRUE;
                enable_false_northing = TRUE;
                enable_false_easting = TRUE;
                break;

            case PROJ_LAMCC:
                enable_first_standard_parallel = TRUE;
                enable_second_standard_parallel = TRUE;
                enable_central_meridian = TRUE;
                enable_latitude_of_origin = TRUE;
                enable_false_northing = TRUE;
                enable_false_easting = TRUE;
                break;

            case PROJ_LAMAZ:
                enable_central_meridian = TRUE;
                enable_latitude_of_origin = TRUE;
                enable_false_northing = TRUE;
                enable_false_easting = TRUE;
                break;

            case PROJ_ALBERS:
                enable_first_standard_parallel = TRUE;
                enable_second_standard_parallel = TRUE;
                enable_central_meridian = TRUE;
                enable_latitude_of_origin = TRUE;
                enable_false_northing = TRUE;
                enable_false_easting = TRUE;
                break;

            case PROJ_MER:
                enable_first_standard_parallel = TRUE;
                enable_central_meridian = TRUE;
                enable_latitude_of_origin = TRUE;
                enable_false_northing = TRUE;
                enable_false_easting = TRUE;
                break;

            case PROJ_EQR:
                enable_central_meridian = TRUE;
                enable_latitude_of_origin = TRUE;
                enable_false_northing = TRUE;
                enable_false_easting = TRUE;
                break;
            }
        }

        enable_pixel_size_checkbutton = TRUE;
        enable_resample_hbox = TRUE;
        enable_force_checkbutton = TRUE;

        // turn off the average height checkbutton if terrain correction
        // is selected, or geographic project is selected
        GtkWidget * dem_checkbutton = get_widget_checked("dem_checkbutton");
        GtkWidget * rb_terrcorr = get_widget_checked("rb_terrcorr");

        if (geographic_is_selected ||
                (gtk_toggle_button_get_active(
                     GTK_TOGGLE_BUTTON(dem_checkbutton)) &&
                 gtk_toggle_button_get_active(
                     GTK_TOGGLE_BUTTON(rb_terrcorr))))
        {
            enable_average_height_checkbutton = FALSE;
            average_height_is_checked = FALSE;

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(average_height_checkbutton), FALSE);
        }
        else
        {
            enable_average_height_checkbutton = TRUE;

            average_height_is_checked =
                gtk_toggle_button_get_active(
                    GTK_TOGGLE_BUTTON(average_height_checkbutton));
        }

        pixel_size_is_checked =
            gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
            pixel_size_checkbutton));

        if (average_height_is_checked)
            enable_average_height_entry = TRUE;

        if (pixel_size_is_checked)
            enable_pixel_size_entry = TRUE;

        set_predefined_projections(projection);
    }
    else {
        vbox_geocode = get_widget_checked("vbox_geocode");
        geocode_tab_label = get_widget_checked("geocode_tab_label");
        gtk_widget_set_sensitive(vbox_geocode, FALSE);
        gtk_widget_set_sensitive(geocode_tab_label, FALSE);
        update_summary();
        return;
    }

    if (enable_table_utm_projection_options)
    {
        gtk_widget_show(table_utm_projection_options);
        gtk_widget_hide(table_nonutm_projection_options);
    }
    else
    {
        gtk_widget_show(table_nonutm_projection_options);
        gtk_widget_hide(table_utm_projection_options);
    }

    gtk_widget_set_sensitive(projection_option_menu,
        enable_projection_option_menu);

    gtk_widget_set_sensitive(predefined_projection_option_menu,
        enable_predefined_projection_option_menu &&
        !enable_table_utm_projection_options);

    utm_zone_label = get_widget_checked("utm_zone_label");
    central_meridian_label = get_widget_checked("central_meridian_label");
    latitude_of_origin_label = get_widget_checked("latitude_of_origin_label");
    first_standard_parallel_label =
        get_widget_checked("first_standard_parallel_label");
    second_standard_parallel_label =
        get_widget_checked("second_standard_parallel_label");
    false_northing_label = get_widget_checked("false_northing_label");
    false_easting_label = get_widget_checked("false_easting_label");
    hbox_average_height = get_widget_checked("hbox_average_height");
    hbox_pixel_size = get_widget_checked("hbox_pixel_size");
    datum_hbox = get_widget_checked("datum_hbox");
    resample_hbox = get_widget_checked("resample_hbox");
    force_checkbutton = get_widget_checked("force_checkbutton");

    gtk_widget_set_sensitive(utm_zone_entry,
        enable_utm_zone);

    gtk_widget_set_sensitive(utm_zone_label,
        enable_utm_zone);

    gtk_widget_set_sensitive(central_meridian_entry,
        enable_central_meridian);

    gtk_widget_set_sensitive(central_meridian_label,
        enable_central_meridian);

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

    gtk_widget_set_sensitive(false_northing_entry,
        enable_false_northing);

    gtk_widget_set_sensitive(false_northing_label,
        enable_false_northing);

    gtk_widget_set_sensitive(false_easting_entry,
        enable_false_easting);

    gtk_widget_set_sensitive(false_easting_label,
        enable_false_easting);

    gtk_widget_set_sensitive(average_height_checkbutton,
        enable_average_height_checkbutton);

    gtk_widget_set_sensitive(hbox_average_height,
        enable_average_height_entry);

    gtk_widget_set_sensitive(pixel_size_checkbutton,
        enable_pixel_size_checkbutton);

    gtk_label_set_text(GTK_LABEL(geocode_pixel_size_label),
        geocode_pixel_size_label_text);

    gtk_widget_set_sensitive(hbox_pixel_size,
        enable_pixel_size_entry);

    gtk_widget_set_sensitive(datum_hbox,
        enable_datum_hbox);

    gtk_widget_set_sensitive(resample_hbox,
        enable_resample_hbox);

    gtk_widget_set_sensitive(force_checkbutton,
        enable_force_checkbutton);

    /* Turn off False Easting & Northing, as they seem to not really
    be very useful. */
    gtk_widget_hide(false_northing_entry);
    gtk_widget_hide(false_northing_label);
    gtk_widget_hide(false_easting_entry);
    gtk_widget_hide(false_easting_label);

    update_summary();
}

SIGNAL_CALLBACK void
on_albers_conical_equal_area_activate(GtkWidget * widget)
{
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_lambert_conformal_conic_activate(GtkWidget * widget)
{
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_lambert_azimuthal_equal_area_activate(GtkWidget * widget)
{
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_polar_stereographic_activate(GtkWidget * widget)
{
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_geographic_activate(GtkWidget * widget)
{
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_itrf97_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_ed50_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_sad69_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_universal_transverse_mercator_activate(GtkWidget * widget)
{
    // auto-select WGS84 when switching to UTM
    // user can change it if they like...
    GtkWidget *datum_option_menu = get_widget_checked("datum_option_menu");
    set_combo_box_item(datum_option_menu, DATUM_WGS84);
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_mercator_activate(GtkWidget * widget)
{
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_equirectangular_activate(GtkWidget * widget)
{
    GtkWidget *spheroid_entry = get_widget_checked("spheroid_entry");
    gtk_entry_set_text(GTK_ENTRY(spheroid_entry), "");
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_geocode_checkbutton_toggled(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_height_checkbutton_toggled(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_pixel_size_checkbutton_toggled(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_wgs84_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_hughes_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_nad27_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_nad83_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_nearest_neighbor_activate(GtkWidget *widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_bilinear_activate(GtkWidget *widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_bicubic_activate(GtkWidget *widget)
{
    geocode_options_changed();
}


SIGNAL_CALLBACK void
on_projection_option_menu_changed(GtkWidget * widget)
{
  GtkWidget * geocode_checkbutton;

  GtkWidget * utm_zone_entry;
  GtkWidget * central_meridian_entry;
  GtkWidget * latitude_of_origin_entry;
  GtkWidget * first_standard_parallel_entry;
  GtkWidget * second_standard_parallel_entry;
  GtkWidget * false_northing_entry;
  GtkWidget * false_easting_entry;

  gboolean geocode_projection_is_checked;

  geocode_checkbutton =
      get_widget_checked("geocode_checkbutton");

  geocode_projection_is_checked =
      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(geocode_checkbutton));

  utm_zone_entry =
      get_widget_checked("utm_zone_entry");

  central_meridian_entry =
      get_widget_checked("central_meridian_entry");

  latitude_of_origin_entry =
      get_widget_checked("latitude_of_origin_entry");

  first_standard_parallel_entry =
      get_widget_checked("first_standard_parallel_entry");

  second_standard_parallel_entry =
      get_widget_checked("second_standard_parallel_entry");

  false_northing_entry =
      get_widget_checked("false_northing_entry");

  false_easting_entry =
      get_widget_checked("false_easting_entry");

  if (geocode_projection_is_checked)
  {
    gtk_entry_set_text(
        GTK_ENTRY(utm_zone_entry), "");
    gtk_entry_set_text(
        GTK_ENTRY(central_meridian_entry), "");
    gtk_entry_set_text(
        GTK_ENTRY(latitude_of_origin_entry), "");
    gtk_entry_set_text(
        GTK_ENTRY(first_standard_parallel_entry), "");
    gtk_entry_set_text(
        GTK_ENTRY(second_standard_parallel_entry), "");
    gtk_entry_set_text(
        GTK_ENTRY(false_northing_entry), "");
    gtk_entry_set_text(
        GTK_ENTRY(false_easting_entry), "");

    geocode_options_changed();
  }
}

SIGNAL_CALLBACK void
on_predefined_projection_option_menu_changed(GtkWidget * widget)
{
    geocode_options_changed();
}
