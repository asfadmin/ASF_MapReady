#include "asf_convert_gui.h"


const char * geocode_options_string(const Settings * settings)
{
    static gchar ret[1024];

    if (settings->geocode_is_checked)
    {	
	gboolean enable_central_meridian = FALSE;
	gboolean enable_latitude_of_origin = FALSE;
	gboolean enable_first_standard_parallel = FALSE;
	gboolean enable_second_standard_parallel = FALSE;
	gboolean enable_false_northing = FALSE;
	gboolean enable_false_easting = FALSE;

	switch (settings->projection)
	{
	    case UNIVERSAL_TRANSVERSE_MERCATOR:
		strcpy(ret, "--projection utm");
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		break;
	    case POLAR_STEREOGRAPHIC:
		strcpy(ret, "--projection ps");
		enable_central_meridian = TRUE;
		enable_first_standard_parallel = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;
	    case LAMBERT_CONFORMAL_CONIC:
		strcpy(ret, "--projection lamcc");
		enable_first_standard_parallel = TRUE;
		enable_second_standard_parallel = TRUE;
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;
	    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
		strcpy(ret, "--projection lamaz");
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;
	    case ALBERS_CONICAL_EQUAL_AREA:
		strcpy(ret, "--projection albers");
		enable_first_standard_parallel = TRUE;
		enable_second_standard_parallel = TRUE;
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;
	}

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

	if (settings->specified_pixel_size)
	    sprintf(ret, "%s --pixel-size %f ", ret, settings->pixel_size);

	strcat(ret, " ");
    }
    else
    {
	strcpy(ret, "");
    }

    return ret;
}

void geocode_options_changed()
{
    GtkWidget * projection_option_menu;
    GtkWidget * geocode_checkbutton;

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

    GtkWidget * hbox_average_height;
    GtkWidget * hbox_pixel_size;

    gboolean geocode_projection_is_checked;
    gboolean average_height_is_checked;
    gboolean pixel_size_is_checked;
    gint projection;

    gboolean enable_projection_option_menu = FALSE;

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

    geocode_checkbutton =
	glade_xml_get_widget(glade_xml, "geocode_checkbutton");

    geocode_projection_is_checked =
	gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(geocode_checkbutton));

    average_height_checkbutton =
	glade_xml_get_widget(glade_xml, "average_height_checkbutton");

    pixel_size_checkbutton =
	glade_xml_get_widget(glade_xml, "pixel_size_checkbutton");

    projection_option_menu =
	glade_xml_get_widget(glade_xml, "projection_option_menu");

    if (geocode_projection_is_checked)
    {	
	enable_projection_option_menu = TRUE;

	projection =
	    gtk_option_menu_get_history(
		GTK_OPTION_MENU(projection_option_menu));
	
	switch (projection)
	{
	    case UNIVERSAL_TRANSVERSE_MERCATOR:
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		break;

	    case POLAR_STEREOGRAPHIC:
		enable_central_meridian = TRUE;
		enable_first_standard_parallel = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;

	    case LAMBERT_CONFORMAL_CONIC:
		enable_first_standard_parallel = TRUE;
		enable_second_standard_parallel = TRUE;
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;

	    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;

	    case ALBERS_CONICAL_EQUAL_AREA:
		enable_first_standard_parallel = TRUE;
		enable_second_standard_parallel = TRUE;
		enable_central_meridian = TRUE;
		enable_latitude_of_origin = TRUE;
		enable_false_northing = TRUE;
		enable_false_easting = TRUE;
		break;
	}

	enable_average_height_checkbutton = TRUE;
	enable_pixel_size_checkbutton = TRUE;

	average_height_is_checked = 
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
					     average_height_checkbutton));

	pixel_size_is_checked = 
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
					     pixel_size_checkbutton));

	if (average_height_is_checked)
	    enable_average_height_entry = TRUE;

	if (pixel_size_is_checked)
	    enable_pixel_size_entry = TRUE;
    }

    gtk_widget_set_sensitive(projection_option_menu,
			     enable_projection_option_menu);

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

    hbox_average_height =
	glade_xml_get_widget(glade_xml, "hbox_average_height");

    hbox_pixel_size =
	glade_xml_get_widget(glade_xml, "hbox_pixel_size");

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

    gtk_widget_set_sensitive(hbox_pixel_size,
			     enable_pixel_size_entry);

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
on_pixel_size_checkbutton_toggled(GtkWidget * widget)
{
    geocode_options_changed();
}
