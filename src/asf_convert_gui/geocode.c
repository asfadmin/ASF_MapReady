#include "asf_convert_gui.h"
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
    }
}

const char * resample_method_string(int resample_method)
{
  switch (resample_method) {
  case RESAMPLE_NEAREST_NEIGHBOR:
    return "nearest_neighbor";
    break;
  default:
  case RESAMPLE_BILINEAR:
    return "bilinear";
    break;
  case RESAMPLE_BICUBIC:
    return "bicubic";
    break;
  }
}

static int entry_has_text(const char * entry_name)
{
    GtkEntry * entry;
    entry = GTK_ENTRY(glade_xml_get_widget(glade_xml, entry_name));
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
	gboolean enable_false_northing = FALSE;
	gboolean enable_false_easting = FALSE;

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
		enable_false_northing =
		    entry_has_text("false_northing_entry");
		enable_false_easting =
		    entry_has_text("false_easting_entry");
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
		enable_false_northing =
		    entry_has_text("false_northing_entry");
		enable_false_easting =
		    entry_has_text("false_easting_entry");
		break;

	    case PROJ_LAMAZ:
		strcpy(ret, "--projection lamaz");
		enable_central_meridian =
		    entry_has_text("central_meridian_entry");
		enable_latitude_of_origin =
		    entry_has_text("latitude_of_origin_entry");
		enable_false_northing =
		    entry_has_text("false_northing_entry");
		enable_false_easting =
		    entry_has_text("false_easting_entry");
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
		enable_false_northing =
		    entry_has_text("false_northing_entry");
		enable_false_easting =
		    entry_has_text("false_easting_entry");
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

	if (enable_false_northing)
	    sprintf(ret, "%s --false-northing %f ", ret, 
		    settings->false_northing);

	if (enable_false_easting)
	    sprintf(ret, "%s --false-easting %f ", ret, 
		    settings->false_northing);

	if (settings->specified_height)
	    sprintf(ret, "%s --height %f ", ret, settings->height);

	sprintf(ret, "%s --datum %s ", ret, datum_string(settings->datum));

	sprintf(ret, "%s --resample-method %s ", ret, 
		resample_method_string (settings->resample_method));
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

    sprintf(buf, "%lf", value);
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

    GtkWidget * hbox_average_height;

    GtkWidget * datum_hbox;

    GtkWidget * resample_hbox;

    gboolean geocode_projection_is_checked;
    gboolean predefined_projection_is_selected;
    gboolean average_height_is_checked;
    gint projection;

    gboolean enable_projection_option_menu = FALSE;
    gboolean enable_predefined_projection_option_menu = FALSE;

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

    gboolean enable_datum_hbox = FALSE;

    gboolean enable_resample_hbox = FALSE;

    table_utm_projection_options =
	glade_xml_get_widget(glade_xml, "table_utm_projection_options");

    table_nonutm_projection_options =
	glade_xml_get_widget(glade_xml, "table_nonutm_projection_options");

    geocode_checkbutton =
	glade_xml_get_widget(glade_xml, "geocode_checkbutton");

    geocode_projection_is_checked =
	gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(geocode_checkbutton));

    average_height_checkbutton =
	glade_xml_get_widget(glade_xml, "average_height_checkbutton");

    projection_option_menu =
	glade_xml_get_widget(glade_xml, "projection_option_menu");

    predefined_projection_option_menu =
	glade_xml_get_widget(glade_xml, "predefined_projection_option_menu");

    utm_zone_entry =
	glade_xml_get_widget(glade_xml, "utm_zone_entry");

    central_meridian_entry =
	glade_xml_get_widget(glade_xml, "central_meridian_entry");

    latitude_of_origin_entry =
	glade_xml_get_widget(glade_xml, "latitude_of_origin_entry");

    first_standard_parallel_entry =
	glade_xml_get_widget(glade_xml, "first_standard_parallel_entry");

    second_standard_parallel_entry =
	glade_xml_get_widget(glade_xml, "second_standard_parallel_entry");

    false_northing_entry =
	glade_xml_get_widget(glade_xml, "false_northing_entry");

    false_easting_entry =
	glade_xml_get_widget(glade_xml, "false_easting_entry");

    projection =
	gtk_option_menu_get_history(GTK_OPTION_MENU(projection_option_menu));

    enable_table_utm_projection_options =
	projection == PROJ_UTM;

    if (geocode_projection_is_checked)
    {	
	predefined_projection_is_selected =
	    0 < gtk_option_menu_get_history(
		GTK_OPTION_MENU(predefined_projection_option_menu));

	enable_projection_option_menu = TRUE;
	enable_predefined_projection_option_menu = TRUE;

	if (predefined_projection_is_selected)
	{
	    /* all widgets remain disabled -- load settings from file */
	    project_parameters_t * pps =
		load_selected_predefined_projection_parameters(projection);

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
			gtk_entry_set_text(
			    GTK_ENTRY(false_northing_entry),
			    double_to_string(pps->lamcc.false_northing));
			gtk_entry_set_text(
			    GTK_ENTRY(false_easting_entry),
			    double_to_string(pps->lamcc.false_easting));
			break;
		    
		    case PROJ_LAMAZ:
			gtk_entry_set_text(
			    GTK_ENTRY(central_meridian_entry),
			    double_to_string(pps->lamaz.center_lon));
			gtk_entry_set_text(
			    GTK_ENTRY(first_standard_parallel_entry),
			    double_to_string(pps->lamaz.center_lat));
			gtk_entry_set_text(
			    GTK_ENTRY(false_northing_entry),
			    double_to_string(pps->lamaz.false_northing));
			gtk_entry_set_text(
			    GTK_ENTRY(false_easting_entry),
			    double_to_string(pps->lamaz.false_easting));
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
			gtk_entry_set_text(
			    GTK_ENTRY(false_northing_entry),
			    double_to_string(pps->albers.false_northing));
			gtk_entry_set_text(
			    GTK_ENTRY(false_easting_entry),
			    double_to_string(pps->albers.false_easting));
			break;
		}
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

		    gtk_entry_set_text(
			GTK_ENTRY(first_standard_parallel_entry), "");
		    gtk_entry_set_text(
			GTK_ENTRY(second_standard_parallel_entry), "");

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

		    gtk_entry_set_text(
			GTK_ENTRY(first_standard_parallel_entry), "");
		    gtk_entry_set_text(
			GTK_ENTRY(second_standard_parallel_entry), "");

		    break;
		    
		case PROJ_ALBERS:
		    enable_first_standard_parallel = TRUE;
		    enable_second_standard_parallel = TRUE;
		    enable_central_meridian = TRUE;
		    enable_latitude_of_origin = TRUE;
		    enable_false_northing = TRUE;
		    enable_false_easting = TRUE;
		    break;
	    }
	}

	enable_average_height_checkbutton = TRUE;
	enable_datum_hbox = TRUE;
	enable_resample_hbox = TRUE;

	average_height_is_checked = 
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
					     average_height_checkbutton));

	if (average_height_is_checked)
	    enable_average_height_entry = TRUE;

	set_predefined_projections(projection);
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

    utm_zone_label =
	glade_xml_get_widget(glade_xml, "utm_zone_label");

    central_meridian_label =
	glade_xml_get_widget(glade_xml, "central_meridian_label");

    latitude_of_origin_label =
	glade_xml_get_widget(glade_xml, "latitude_of_origin_label");

    first_standard_parallel_label =
	glade_xml_get_widget(glade_xml, "first_standard_parallel_label");

    second_standard_parallel_label =
	glade_xml_get_widget(glade_xml, "second_standard_parallel_label");

    false_northing_label =
	glade_xml_get_widget(glade_xml, "false_northing_label");

    false_easting_label =
	glade_xml_get_widget(glade_xml, "false_easting_label");

    hbox_average_height =
	glade_xml_get_widget(glade_xml, "hbox_average_height");

    datum_hbox =
	glade_xml_get_widget(glade_xml, "datum_hbox");

    resample_hbox =
        glade_xml_get_widget(glade_xml, "resample_hbox");

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

    gtk_widget_set_sensitive(datum_hbox,
			     enable_datum_hbox);

    gtk_widget_set_sensitive(resample_hbox,
			     enable_resample_hbox);
    
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
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_lambert_conformal_conic_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_lambert_azimuthal_equal_area_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_polar_stereographic_activate(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_universal_transverse_mercator_activate(GtkWidget * widget)
{
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
on_wgs84_activate(GtkWidget * widget)
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
on_predefined_projection_option_menu_changed(GtkWidget * widget)
{
    geocode_options_changed();
}

SIGNAL_CALLBACK void
on_user_defined_activate(GtkWidget * widget)
{
}
