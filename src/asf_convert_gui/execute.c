#include "asf_convert_gui.h"

SIGNAL_CALLBACK void
on_execute_button_clicked (GtkWidget *button)
{
  GtkWidget
    *input_data_type_combobox,
    *input_data_format_combobox,
    *output_format_combobox,
    *scale_checkbutton,
    *longest_dimension_spinbutton;

  gint input_data_type,
    input_data_format,
    output_format;

  gchar *format_arg_to_import,
    *format_arg_to_export,
    *input_type_arg,
    *out_extension;

  gchar convert_cmd[4096], size_arg[32], latitude_arg[64];

  GtkTreeIter iter;
  gboolean valid,
    run_import,
    run_export,
    include_latitude,
    include_size;

  input_data_type_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_type_combobox");

  input_data_type =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_type_combobox));

  input_data_format_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");

  input_data_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_format_combobox));

  output_format_combobox = 
    glade_xml_get_widget(glade_xml, "output_format_combobox");

  output_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(output_format_combobox));

  scale_checkbutton = 
    glade_xml_get_widget(glade_xml, "scale_checkbutton");

  longest_dimension_spinbutton =
    glade_xml_get_widget(glade_xml, "longest_dimension_spinbutton");

  /* defaults */
  size_arg[0] = '\0';
  latitude_arg[0] = '\0';
  run_import = TRUE;
  run_export = TRUE;
  include_size = FALSE;
  include_latitude = FALSE;

  switch (input_data_type)
    {
    case INPUT_TYPE_SIGMA:
      input_type_arg = "sigma";
      break;

    case INPUT_TYPE_BETA:
      input_type_arg = "beta";
      break;

    case INPUT_TYPE_GAMMA:
      input_type_arg = "gamma";
      break;

    default:
    case INPUT_TYPE_AMP:
      input_type_arg = "amplitude";
      break;

    case INPUT_TYPE_POWER:
      input_type_arg = "power";
      break;
    }

  switch (input_data_format)
    {
    case INPUT_FORMAT_CEOS_LEVEL0:
      format_arg_to_import = "ceos";
      include_latitude = TRUE;
      break;

    default:
    case INPUT_FORMAT_CEOS_LEVEL1:
      format_arg_to_import = "ceos";
      break;

    case INPUT_FORMAT_STF:
      format_arg_to_import = "stf";
      include_latitude = TRUE;
      break;

    case INPUT_FORMAT_ESRI:
      format_arg_to_import = "esri";
      break;

    case INPUT_FORMAT_ENVI:
      format_arg_to_import = "envi";
      break;

    case INPUT_FORMAT_COMPLEX:
      format_arg_to_import = "ceos";  /* FIXME: is this correct? */
      break;

    case INPUT_FORMAT_ASF_INTERNAL:
      run_import = FALSE;
      break;
    }

  switch (output_format)
    {
    case OUTPUT_FORMAT_ASF_INTERNAL:
      run_export = FALSE;
      include_size = FALSE;
      break;

    case OUTPUT_FORMAT_CEOS:
    default:
      format_arg_to_export = "ceos";
      out_extension = "D";
      include_size = FALSE;
      break;

    case OUTPUT_FORMAT_JPEG:
      format_arg_to_export = "jpeg";
      out_extension = "jpg";
      include_size = TRUE;
      break;

    case OUTPUT_FORMAT_PPM:
      format_arg_to_export = "ppm";
      out_extension = "ppm";
      include_size = TRUE;
      break;

    case OUTPUT_FORMAT_GEOTIFF:
      format_arg_to_export = "geotiff";
      out_extension = "tif";
      include_size = FALSE;
      break;
    }

  if (include_size)
  {
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(scale_checkbutton)))
    {
      gdouble d = 
	gtk_spin_button_get_value(
		       GTK_SPIN_BUTTON(longest_dimension_spinbutton));
      sprintf(size_arg, "-size %d", (int)floor(d + 0.5));
    }
  }

  if (include_latitude)
  {
    gdouble low, hi;
    GtkWidget *latitude_low_spinbutton, *latitude_hi_spinbutton;

    latitude_low_spinbutton =
      glade_xml_get_widget(glade_xml, "latitude_low_spinbutton");

    latitude_hi_spinbutton =
      glade_xml_get_widget(glade_xml, "latitude_hi_spinbutton");

    low = gtk_spin_button_get_value(GTK_SPIN_BUTTON(latitude_low_spinbutton));
    hi = gtk_spin_button_get_value(GTK_SPIN_BUTTON(latitude_hi_spinbutton));

    sprintf(latitude_arg, "-lat %.2f %.2f", low, hi);
  }

  show_execute_button(FALSE);
  valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  keep_going = TRUE;  

  while (valid && keep_going)
  {
    gchar *in_data, *in_meta, *basename, *p;

    gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
		       0, &in_data, 1, &in_meta, -1);
    
    basename = strdup(in_data);
    p = strrchr(basename, '.');
    if (p)
      *p = '\0';

    gtk_list_store_set(list_store, &iter, 2, "Processing...", -1);

    while (gtk_events_pending())
      gtk_main_iteration();

    if (run_import)
    {
      snprintf(convert_cmd, 4096, 
	       "asf_import -quiet -%s -format %s %s %s %s %s",
	       input_type_arg,
	       format_arg_to_import,
	       latitude_arg,
	       in_data,
	       in_meta,
	       basename);

      system(convert_cmd);

      char * out_name_full = (char *)malloc(strlen(basename) + 10);
      sprintf(out_name_full, "%s.img", basename);

      if (!run_export)
	gtk_list_store_set(list_store, &iter, 2, out_name_full, -1);

      free(out_name_full);
    }

    while (gtk_events_pending())
      gtk_main_iteration();

    if (run_export)
    {
      char * out_name_full = (char *)malloc(strlen(basename) + 20);
      sprintf(out_name_full, "%s.%s", basename, out_extension);

      snprintf(convert_cmd, 4096, "asf_export -format %s %s %s %s",
	       format_arg_to_export,
	       size_arg,
	       basename,
	       out_name_full);
      
      system(convert_cmd);

      gtk_list_store_set(list_store, &iter, 2, out_name_full, -1);
      free(out_name_full);
    }
    /* printf("data: %s meta: %s\n", in_data, in_meta); */
    /* snprintf (convert_cmd, 1024, "asf_convert -format %s %s %s %s %s",
	    format,
	    size_arg,
	    in_data,
	    in_meta,
	    out_full);

    system(convert_cmd);
    */

    while (gtk_events_pending())
      gtk_main_iteration();

    free(basename);

    if (!keep_going)
      gtk_list_store_set(list_store, &iter,
			 2, "Processing stopped by user.", -1);

    valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }

  show_execute_button(TRUE);
}

SIGNAL_CALLBACK void
on_stop_button_clicked(GtkWidget * widget)
{
  keep_going = FALSE;
}
