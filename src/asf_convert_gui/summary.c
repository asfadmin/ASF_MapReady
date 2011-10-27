#include "asf_convert_gui.h"

/*
int zone(float lon)
{
return((int)(((lon + 180.0) / 6.0) + 1.0));
}
*/

const char *get_summary_text()
{
    static char text[1024];

    Settings * s;
    s = settings_get_from_gui();

    const char *type;
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

    char *dbstr = "";
    if (s->output_db)
      dbstr = " (dB)";

    char *fmts = STRDUP(get_string_from_label("input_formats_label"));
    char *p = strchr(fmts, ':');
    if (strchr(fmts, ',')!=NULL)
      strcpy(text, "Formats: ");
    else
      strcpy(text, "Format: ");
    if (p)
      strcat(text, p+2);
    else
      strcat(text, fmts);
    strcat(text, "  \nData type: ");
    strcat(text, type);
    strcat(text, dbstr);
    FREE(fmts);

    if (s->data_type != INPUT_TYPE_AMP && !s->apply_ers2_gain_fix)
        sprintf(text, "%s\n  (no ERS2 gain correction)", text);

    int polarimetry_on =
      s->polarimetric_decomp_setting != POLARIMETRY_NONE || s->do_farcorr;

    if (polarimetry_on)
    {
        strcat(text, "\nPolarimetry: Yes ");
        switch (s->polarimetric_decomp_setting) {
          case POLARIMETRY_NONE:
            strcat(text, "(no decomposition)");
            break;
          case POLARIMETRY_PAULI:
            strcat(text, "(Pauli)");
            break;
          case POLARIMETRY_SINCLAIR:
            strcat(text, "(Sinclair)");
            break;
          case POLARIMETRY_CLOUDE8:
            strcat(text, "(Cloude Pottier 8)");
            break;
          case POLARIMETRY_CLOUDE16:
            strcat(text, "(Cloude Pottier 16)");
            break;
          case POLARIMETRY_CLOUDE_NOCLASSIFY:
            strcat(text, "(Entropy,Anisotropy,Alpha)");
            break;
          case POLARIMETRY_FREEMAN_DURDEN:
            strcat(text, "(Freeman Durden)");
            break;
        }

        if (s->do_farcorr) {
          if (s->farcorr_global_avg)
            strcat(text, "\n   FR Corr. w/ Global Avg");
          else
            strcat(text, "\n   FR Corr. w/ Local Avg");

          if (s->farcorr_threshold > 0)
            sprintf(text, "%s\n     Threshold: %.1f degree%s", text,
                    s->farcorr_threshold, s->farcorr_threshold==1?"":"s");
          else
            strcat(text, "\n     No threshold");
        }
    }

    if (s->terrcorr_is_checked || s->refine_geolocation_is_checked)
    {
        GtkWidget *dem_entry;
        char *dem;

        if (s->terrcorr_is_checked)
            strcat(text, "\nTerrain Correction: Yes");
        else
            strcat(text, "\nRefine Geolocation: Yes");

        if (s->do_radiometric)
        {
            strcat(text, "\n   Geometric & Radiometric correction");
            if(s->save_incid_angles)
                strcat(text, "\n      Saving Incidence Angles");
        }
        else
            strcat(text, "\n   Geometric correction only");

        dem_entry = get_widget_checked("dem_entry");
        dem = STRDUP(gtk_entry_get_text(GTK_ENTRY(dem_entry)));

        if (!dem || strlen(dem) == 0)
        {
            strcat(text, "\n   DEM: <none>");
        }
        else
        {
            char *p = strrchr(dem, DIR_SEPARATOR);
            sprintf(text, "%s\n   DEM: %s", text, p ? p+1 : dem);
        }

        free(dem);

        if (s->interp_dem_holes)
            strcat(text, "\n   Fill DEM Holes: Yes");

        if (s->terrcorr_is_checked)
        {
            if (s->no_matching)
            {
                sprintf(text, "%s\n   Skip co-registration", text);
                if (s->offset_x != 0.0 || s->offset_y != 0.0) {
                    sprintf(text, "%s\n     (offsets: %.2f,%.2f pixels)", text,
                            s->offset_x, s->offset_y);
                }
            }

            if (s->specified_tc_pixel_size)
            {
                sprintf(text, "%s\n   Pixel Size: %.2f m", text,
                        s->tc_pixel_size);
            }
            else if (s->specified_pixel_size)
            {
                sprintf(text, "%s\n   Pixel Size: %.2f m (from geocode)",
                        text, s->pixel_size);
            }
                    
            sprintf(text, "%s\n   Interpolate Layover: %s", text,
                    s->interp ? "Yes" : "No");
        }

        if (s->mask_file_is_checked)
        {
            GtkWidget *mask_entry;
            char *mask;

            mask_entry = get_widget_checked("mask_entry");
            mask = STRDUP(gtk_entry_get_text(GTK_ENTRY(mask_entry)));

            if (!mask || strlen(mask) == 0)
            {
                strcat(text, "\n   Mask: <none>");
            }
            else
            {
                char *p = strrchr(mask, DIR_SEPARATOR);
                sprintf(text, "%s\n   Mask: %s", text, p ? p+1 : mask);
            }
        }
        else if (s->auto_water_mask_is_checked)
        {
            strcat(text, "\n   Automatic Mask");
        }

        if (s->generate_dem && s->generate_layover_mask)
        {
            strcat(text, "\n   Save DEM & Layover Mask");
        }
        else if (s->generate_dem)
        {
            strcat(text, "\n   Save DEM");
        }
        else if (s->generate_layover_mask)
        {
            strcat(text, "\n   Save Layover Mask");
        }
    }
    else
    {
        strcat(text, "\nTerrain Correction: No");
    }


    strcat(text, "\nGeocoding: ");

    if (s->geocode_is_checked)
    {
        switch (s->projection)
        {
        case PROJ_UTM:
            if (s->zone != 0)
            {
                sprintf(text, "%sUTM\n   Zone: %d\n",
                        text, s->zone);
            }
            else
            {
                sprintf(text, "%sUTM\n   Zone: <from metadata>\n", text);
            }
            break;

        case PROJ_PS:
            sprintf(text, "%sPolar Stereo\n"
                "   Center: (%.2f, %.2f)\n",
                text, s->plat1, s->lon0);
            break;

        case PROJ_LAMCC:
            sprintf(text, "%sLambert Conformal Conic\n"
                "   Center: (%.2f, %.2f)\n"
                "   Standard Parallels: (%.2f, %.2f)\n",
                text, s->lat0, s->lon0,
                s->plat1, s->plat2);
            break;

        case PROJ_LAMAZ:
            sprintf(text, "%sLambert Azimuthal Equal Area\n"
                "   Center: (%.2f, %.2f)\n",
                text, s->lat0, s->lon0);
            break;

        case PROJ_ALBERS:
            sprintf(text, "%sAlbers Conical Equal Area\n"
                "   Center: (%.2f, %.2f)\n"
                "   Standard Parallels: (%.2f, %.2f)\n",
                text, s->lat0, s->lon0,
                s->plat1, s->plat2);
            break;
        }

        if (s->specified_height)
            sprintf(text, "%s   Height: %.2f\n", text, s->height);

        if (s->specified_pixel_size)
            sprintf(text, "%s   Pixel Size: %.2f m\n", text, s->pixel_size);

        sprintf(text, "%s   Datum: %s%s\n", text, datum_string(s->datum),
                s->datum == DATUM_HUGHES ? " Reference Spheroid" : "");

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

        case OUTPUT_FORMAT_PNG:
            strcat(text, "PNG");
            break;

        case OUTPUT_FORMAT_PGM:
            strcat(text, "PGM");
            break;

        case OUTPUT_FORMAT_GEOTIFF:
            strcat(text, "GeoTIFF");
            break;

        case OUTPUT_FORMAT_TIFF:
            strcat(text, "TIFF");
            break;

        case OUTPUT_FORMAT_POLSARPRO:
            strcat(text, "POLSARPRO");
	    break;
	}
        if (s->output_bytes)
        {
            strcat(text, " (byte)\n   Scaling Method: ");
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

                case SCALING_METHOD_HISTOGRAM_EQUALIZE:
                    strcat(text, "Histogram Equalize");
                    break;
            }
            //strcat(text, "\n");
        }
        else
        {
            strcat(text, " (float)");
        }

        if (s->export_bands)
        {
            if (s->truecolor_is_checked)
                strcat(text, "\n   RGB Banding: True Color\n");
            else if (s->falsecolor_is_checked)
                strcat(text, "\n   RGB Banding: False Color\n");
            else
                sprintf(text, "%s\n   RGB Banding: %s,%s,%s\n", text,
                        strlen(s->red) > 0 ? s->red : "-",
                        strlen(s->green) > 0 ? s->green : "-",
                        strlen(s->blue) > 0 ? s->blue : "-");
        }
        else if (s->polarimetric_decomp_setting == POLARIMETRY_PAULI)
            strcat(text, "\n   RGB Banding: Pauli\n");
        else if (s->polarimetric_decomp_setting == POLARIMETRY_SINCLAIR)
            strcat(text, "\n   RGB Banding: Sinclair\n");
        else if (s->polarimetric_decomp_setting == POLARIMETRY_CLOUDE8)
            strcat(text, "\n   RGB Banding: Cloude-Pottier (8)\n");
        else if (s->polarimetric_decomp_setting == POLARIMETRY_CLOUDE16)
            strcat(text, "\n   RGB Banding: Cloude-Pottier (16)\n");
        else if (s->polarimetric_decomp_setting == POLARIMETRY_FREEMAN_DURDEN)
            strcat(text, "\n   RGB Banding: Freeman/Durden\n");

    }
    else
    {
        strcat(text, "<none>\n");
    }

    settings_delete(s);
    return text;
}

SIGNAL_CALLBACK
void update_summary()
{
    const char *text = get_summary_text();
    GtkWidget *summary_label = get_widget_checked("summary_label");
    gtk_label_set_text(GTK_LABEL(summary_label), text);
}
