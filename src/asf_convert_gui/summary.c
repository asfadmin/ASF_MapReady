#include "asf_convert_gui.h"

/*
int zone(float lon)
{
    return((int)(((lon + 180.0) / 6.0) + 1.0));
}
*/

SIGNAL_CALLBACK
void update_summary()
{
    GtkWidget * summary_label;
    Settings * s;
    gchar text[1024];
    gchar * type;

    s = settings_get_from_gui();

    strcpy(text, "Import: ");
    
    switch (s->input_data_format)
    {
	case INPUT_FORMAT_CEOS_LEVEL0:
	    strcat(text, "CEOS Level Zero");
	    break;
	    
	default:
	case INPUT_FORMAT_CEOS_LEVEL1:
	    switch (s->data_type)
	    {
		case INPUT_TYPE_SIGMA:
		    type = "Sigma";
		    break;

		case INPUT_TYPE_BETA:
		    type = "Beta";
		    break;

		case INPUT_TYPE_GAMMA:
		    type = "Gamma";
		    break;
		    
		default:
		case INPUT_TYPE_AMP:
		    type = "Amplitude";
		    break;
		    
		case INPUT_TYPE_POWER:
		    type = "Power";
		    break;
	    }

	    sprintf(text, "%sCEOS Level One\nData type: %s",
		    text, type);

	    break;
		   
	case INPUT_FORMAT_STF:
	    strcat(text, "STF");
	    if (s->latitude_checked)
	    {
		sprintf(text, "%s (Lat: %f - %f)", text, s->latitude_low,
			s->latitude_hi);
	    }
	    break;

	case INPUT_FORMAT_COMPLEX:
	    strcat(text, "Complex");
	    break;
    }

    strcat(text, "\nGeocoding: ");

    if (s->geocode_is_checked)
    {
	switch (s->projection)
	{
	    case UNIVERSAL_TRANSVERSE_MERCATOR:
		sprintf(text, "%sUTM\n   Zone: %d\n",
			text, s->zone);
		break;

	    case POLAR_STEREOGRAPHIC:
		sprintf(text, "%sPolar Stereo\n"
			"   Center: (%f, %f)\n",
			text, s->lat0, s->lon0);
		break;

	    case LAMBERT_CONFORMAL_CONIC:
		sprintf(text, "%sLambert Conformal Conic\n"
			"   Center: (%f, %f)\n"
			"   Standard Parallels: (%f, %f)\n",
			text, s->lat0, s->lon0,
			s->plat1, s->plat2);
		break;

	    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
		sprintf(text, "%sLambert Azimuthal Equal Area\n"
			"   Center: (%f, %f)\n",
			text, s->lat0, s->lon0);
		break;

	    case ALBERS_EQUAL_AREA:
		sprintf(text, "%sAlbers Conical Equal Area\n"
			"   Center: (%f, %f)\n"
			"   Standard Parallels: (%f, %f)\n",
			text, s->lat0, s->lon0,
			s->plat1, s->plat2);
		break;
	}

	if (s->specified_height)
	    sprintf(text, "%s   Height: %f\n", text, s->height);

	sprintf(text, "%s   Datum: %s\n", text, datum_string(s->datum));

	sprintf(text, "%s   Resampling Method: %s\n", text,
		resample_method_string(s->resample_method));
    }
    else
    {
	strcat(text, "<none>\n");
    }

    strcat(text, "Export: ");
    if (s->export_is_checked)
    {
	switch (s->output_format)
	{
	    case OUTPUT_FORMAT_JPEG:
	    default:
		strcat(text, "JPEG");
		break;
		
	    case OUTPUT_FORMAT_PPM:
		strcat(text, "PPM");
		break;
		
	    case OUTPUT_FORMAT_GEOTIFF:
		strcat(text, "geoTIFF");
		break;
		
	    case OUTPUT_FORMAT_TIFF:
		strcat(text, "TIFF");
		break;
	}

	if (s->apply_scaling)
	{
	    GtkWidget *longest_dimension_spinbutton;
	    gdouble d;
	    
	    longest_dimension_spinbutton = 
		glade_xml_get_widget(glade_xml, 
				     "longest_dimension_spinbutton");

	    d = gtk_spin_button_get_value(
		GTK_SPIN_BUTTON(longest_dimension_spinbutton));
	    
	    sprintf(text, "%s (size %d)", text, (int)floor(d + 0.5));
	}

	strcat(text, "\n");
    }
    else
    {
	strcat(text, "<none>\n");
    }

    if (s->output_bytes)
    {
	strcat(text, "   Scaling Method: ");
	switch (s->scaling_method)
	{
            default:
            case SCALING_METHOD_SIGMA:
                strcat(text, "Sigma");
                break;
                
            case SCALING_METHOD_MINMAX:
                strcat(text, "MinMax");
                break;
                
            case SCALING_METHOD_TRUNCATE:
                strcat(text, "Truncate");
                break;
	}
	strcat(text, "\n");
    }

    summary_label =
	glade_xml_get_widget(glade_xml, "summary_label");

    gtk_label_set_text(GTK_LABEL(summary_label), text);
}
