#ifdef win32
  #define BYTE __byte
  #include "asf_meta.h"
  #undef BYTE
  #include <windows.h>
  #undef DIR_SEPARATOR
#else
  #include "asf_meta.h"
#endif

#include "asf.h"
#include "asf_convert_gui.h"
#include "asf_version.h"
#include "asf_convert.h"
#include <errno.h>

// Protos
void save_config(char *file, char* projfile);

// Functions
static int
settings_get_input_data_format_allows_latitude(const Settings *s)
{
    return -1; /*s->input_data_format == INPUT_FORMAT_CEOS_LEVEL0 ||*/
      //s->input_data_format == INPUT_FORMAT_STF;
}

static const gchar * scaling_method_string(int scaling_method)
{
  switch (scaling_method)
  {
    default:
    case SCALING_METHOD_SIGMA:
      return "sigma";

    case SCALING_METHOD_MINMAX:
      return "minmax";

    case SCALING_METHOD_TRUNCATE:
      return "truncate";

    case SCALING_METHOD_HISTOGRAM_EQUALIZE:
      return "histogram_equalize";
  }
}
//#undef USE_GTK_FILE_CHOOSER
#ifdef USE_GTK_FILE_CHOOSER

/* If the GtkFileChooser is available -- we'll use that instead of
   GtkFileSelection
*/
static GtkWidget *save_config_widget = NULL;
static int pending_save = FALSE;
static GtkWidget *load_config_widget = NULL;
static int pending_load = FALSE;

// called when "cancel" clicked on the Save Settings GtkFileChooser
static SIGNAL_CALLBACK void save_config_cancel_clicked()
{
  pending_save = FALSE;
  gtk_widget_hide(save_config_widget);
}

// called when "ok" clicked on the Save Settings GtkFileChooser
static SIGNAL_CALLBACK void save_config_ok_clicked()
{
  if (!pending_save)
    return;

  GSList *files = NULL;

  if (save_config_widget) {
    files = gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(save_config_widget));
  }

  gtk_widget_hide(save_config_widget);
  pending_save = FALSE;

  if (files) {
    char projfile[1024]="";
    save_config((char *) files->data, projfile);
    char msg[2304]=" Settings saved... "; // Room for two 1024-byte paths plus a 256-byte message
    if (strlen(projfile) > 0) {
      sprintf(msg," Settings saved to:\n  %s\n\n Projection parameters saved to:\n  %s \n",
              (char *) files->data, projfile);
    }
    else {
      sprintf(msg," Settings saved to:\n  %s \n", (char *) files->data);
    }
    message_box(msg);

    g_slist_free(files);
  }
}

// sets up the save file chooser dialog
static void create_save_file_chooser_dialog()
{
  GtkWidget *parent = get_widget_checked("asf_convert");

  assert(save_config_widget == NULL);
  save_config_widget = gtk_file_chooser_dialog_new(
      "Save Configuration (*.cfg) File", GTK_WINDOW(parent),
      GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,  //Cancel button
      GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,    //Save button
      NULL);

  // we need to extract the buttons, so we can connect them to our
  // button handlers, above
  GtkHButtonBox *box =
      (GtkHButtonBox*)(((GtkDialog*)save_config_widget)->action_area);
  GList *buttons = box->button_box.box.children;

  GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
  GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

  g_signal_connect((gpointer)cancel_btn, "clicked",
                    G_CALLBACK(save_config_cancel_clicked), NULL);
  g_signal_connect((gpointer)ok_btn, "clicked",
                    G_CALLBACK(save_config_ok_clicked), NULL);

  // add the filters
  GtkFileFilter *cfg_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(cfg_filt, "ASF Configuration Files (*.cfg)");
  gtk_file_filter_add_pattern(cfg_filt, "*.cfg");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(save_config_widget), cfg_filt);

  GtkFileFilter *all_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(all_filt, "All Files (*.*)");
  gtk_file_filter_add_pattern(all_filt, "*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(save_config_widget), all_filt);

  // we need to make these modal -- if the user opens multiple "open"
  // dialogs, we'll get confused on the callbacks
  gtk_window_set_modal(GTK_WINDOW(save_config_widget), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(save_config_widget), TRUE);
  gtk_dialog_set_default_response(GTK_DIALOG(save_config_widget),
                                  GTK_RESPONSE_OK);
}

// called when "cancel" clicked on the Load Settings GtkFileChooser
static SIGNAL_CALLBACK void load_config_cancel_clicked()
{
  pending_load = FALSE;
  gtk_widget_hide(load_config_widget);
}

// called when "ok" clicked on the Load Settings GtkFileChooser
static SIGNAL_CALLBACK void load_config_ok_clicked()
{
  if (!pending_load)
    return;

  GSList *files = gtk_file_chooser_get_filenames(
      GTK_FILE_CHOOSER(load_config_widget));

  gtk_widget_hide(load_config_widget);
  pending_load = FALSE;

  if (files) {
    int ret;
    ret = apply_settings_from_config_file((char *) files->data);
    if (ret != 0) {
      message_box("\n Unable to apply filename settings from configuration \n"
          " file.  This may occur if you hand-edited the \n"
          " configuration file but did not specify full pathnames \n"
          " for all filenames, e.g. input data file, DEMs, masks, etc \n"
          "\n Either edit the configuration file again, or browse to \n"
          " the input files from each process tab. \n");
    }
    g_slist_free(files);
  }
}

// sets up the load file chooser dialog
static void create_load_file_chooser_dialog()
{
  GtkWidget *parent = get_widget_checked("asf_convert");

  load_config_widget = gtk_file_chooser_dialog_new(
      "Load Configuration (*.cfg) File", GTK_WINDOW(parent),
  GTK_FILE_CHOOSER_ACTION_OPEN,
  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,  //Cancel button
  GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,    //Open button
  NULL);

  // we need to extract the buttons, so we can connect them to our
  // button handlers, above
  GtkHButtonBox *box =
      (GtkHButtonBox*)(((GtkDialog*)load_config_widget)->action_area);
  GList *buttons = box->button_box.box.children;

  GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
  GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

  g_signal_connect((gpointer)cancel_btn, "clicked",
                    G_CALLBACK(load_config_cancel_clicked), NULL);
  g_signal_connect((gpointer)ok_btn, "clicked",
                    G_CALLBACK(load_config_ok_clicked), NULL);

  // add the filters
  GtkFileFilter *cfg_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(cfg_filt, "ASF Configuration Files (*.cfg)");
  gtk_file_filter_add_pattern(cfg_filt, "*.cfg");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(load_config_widget), cfg_filt);

  GtkFileFilter *all_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(all_filt, "All Files (*.*)");
  gtk_file_filter_add_pattern(all_filt, "*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(load_config_widget), all_filt);

  // we need to make these modal -- if the user opens multiple "open"
  // dialogs, we'll get confused on the callbacks
  gtk_window_set_modal(GTK_WINDOW(load_config_widget), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(load_config_widget), TRUE);
  gtk_dialog_set_default_response(GTK_DIALOG(load_config_widget),
                                  GTK_RESPONSE_OK);
}

#endif // #ifdef USE_GTK_FILE_CHOOSER

SIGNAL_CALLBACK void
on_save_button_clicked(GtkWidget *w, gpointer data)
{
#ifdef win32
  OPENFILENAME of;
  int retval;
  char fname[1024];

  fname[0] = '\0';

  memset(&of, 0, sizeof(of));

# ifdef OPENFILENAME_SIZE_VERSION_400
  of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
# else
  of.lStructSize = sizeof(of);
# endif

  of.hwndOwner = NULL;
  of.lpstrFilter = "ASF MapReady Configuration Files (*.cfg)\0*.cfg\0"
      "All Files\0*\0";
  of.lpstrCustomFilter = NULL;
  of.nFilterIndex = 1;
  of.lpstrFile = fname;
  of.nMaxFile = sizeof(fname);
  of.lpstrFileTitle = NULL;
  of.lpstrInitialDir = ".";
  of.lpstrTitle = "Select Output ASF Configuration File";
  of.lpstrDefExt = NULL;
  of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

  retval = GetSaveFileName(&of);

  if (!retval) {
    if (CommDlgExtendedError()) {
      message_box("File dialog box error");
    }
  }

  if (strlen(fname) > 0) {
    char projfile[1024]="";
    save_config(fname, projfile);

    char msg[2304]=" Settings saved... "; // Room for two 1024-byte paths plus a 256-byte message
    if (strlen(projfile)) {
      sprintf(msg," Settings saved to:\n  %s\n Projection parameters saved to:\n  %s ",
              fname, projfile);
    }
    else {
      sprintf(msg," Settings saved to:\n  %s ", fname);
    }
    message_box(msg);

  }
  else {
    char msg[2304];
    sprintf(msg, " Zero length filename found ");
    message_box(msg);
  }

#else // #ifdef win32

  /* Linux version -- use GtkFileChooser if possible */

# ifdef USE_GTK_FILE_CHOOSER

  if (!save_config_widget)
    create_save_file_chooser_dialog();

  gtk_widget_show(save_config_widget);
  pending_save = TRUE;

# else // #ifdef USE_GTK_FILE_CHOOSER

  GtkWidget *file_selection_dialog =
    get_widget_checked("save_config_selection");

  gtk_widget_show(file_selection_dialog);

# endif // #ifdef USE_GTK_FILE_CHOOSER
#endif // #ifdef win32
}

void hide_save_config_file_selection_dialog()
{
  GtkWidget *save_config_file_selection_dialog =
      get_widget_checked("save_config_selection");

  gtk_widget_hide(save_config_file_selection_dialog);
}

SIGNAL_CALLBACK gboolean
on_save_config_file_selection_delete_event(GtkWidget *w)
{
  hide_save_config_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
on_save_config_file_selection_destroy_event(GtkWidget *w)
{
  hide_save_config_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
on_save_config_file_selection_destroy(GtkWidget *w)
{
  hide_save_config_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK void
on_save_config_file_selection_ok_button_clicked(GtkWidget *widget)
{
  GtkWidget *file_selection_dialog;
  gchar **selections = NULL;
  gchar **current;
  int no_selection = 1;

  file_selection_dialog =
      get_widget_checked("save_config_selection");

  selections = gtk_file_selection_get_selections(
      GTK_FILE_SELECTION(file_selection_dialog));

  current = selections;

  // If no file is selected (blank entry in dialog box), only the directory
  // is returned in 'selections'.  If this is used as a config filename, then
  // a config file will be created one directory up using the name of the
  // current directory as its basename... Bail if nothing is selected instead.
  if (!is_dir((const char *)*current)) {
    no_selection = 0;
  }

  if (!no_selection) {
    char projfile[1024]="";
    append_ext_if_needed(*current, ".cfg", ".cfg");
    save_config((char*) *current, projfile);

    char msg[2304]=" Settings saved... "; // Room for two 1024-byte paths plus a 256-byte message
    if (strlen(projfile)) {
      sprintf(msg," Settings saved to:\n  %s\n Projection parameters saved to:\n  %s ",
              *current, projfile);
    }
    else {
      sprintf(msg," Settings saved to:\n  %s ", *current);
    }
    message_box(msg);
  }

  if (selections) g_strfreev(selections);
  gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_save_config_file_selection_cancel_button_clicked(GtkWidget *widget)
{
  hide_save_config_file_selection_dialog();
}

// A config_file file name must be passed in
// A projfile projection parameters file may or may not be passed in
//   If it is passed in, then it is used for saving projection parameters into,
//   but if not, then the base name of the config file is used but with a .proj
//   file extension instead.
void save_config(char *cfgFile, char* projfile)
{
  // FIXME: The following proj and config file writing code was taken from settings.c
  // settings_to_config_file() should be refactored to take this stuff out and put
  // it into a couple of separate functions ...which we'd also use here.  But because
  // settings_to_config_file() is designed to only write temporary files, it doesn't
  // work well for when a user is saving to a permanent file...
  FILE *cf, *pf;
  Settings * s;
  char input_file[]="";
  char *input_basename=NULL;
  char output_full[]="";
  char *output_file=NULL;
  char *output_basename=NULL;
  char tmp_statfile[]="";
  char tmp_dir[]="";
  if (cfgFile == NULL || strlen(cfgFile)==0) {
    message_box("Missing config file name");
    return;
  }

  // Get current settings
  s = settings_get_from_gui();

  //////
  // Some preliminaries
  //

  // Add .cfg to config file if necessary
  char *config_file = appendExt(cfgFile, ".cfg");
  // Proj file will use the selected config file basename, but with a '.proj'
  // extension
  char *tmp_projfile;
  char *cfg_base = get_basename(config_file);
  char *cfg_path = g_path_get_dirname(config_file);
  if (strlen(projfile)) {
    tmp_projfile = (char *)MALLOC(sizeof(char)*strlen(projfile));
    strcpy(tmp_projfile, projfile);
  }
  else {
    tmp_projfile = (char *)MALLOC(sizeof(char)*(strlen(cfg_base)+strlen(cfg_path)+7));
    sprintf(tmp_projfile,"%s%c%s.proj", cfg_path, DIR_SEPARATOR, cfg_base);
  }
  FREE(cfg_base);
  g_free(cfg_path);

  if (strlen(output_full)) {
    if (s->export_is_checked) {
      output_file = STRDUP(output_full);
    } else {
      output_file = stripExt(output_full);
    }

    output_basename = get_basename(output_file);
  }
  else {
    output_basename = (char*)MALLOC(sizeof(char)*1);
    strcpy(output_basename, "");
    output_file=(char*)MALLOC(sizeof(char)*1);
    strcpy(output_file, "");
  }

  if (strlen(input_file)) {
    char *base = get_filename(input_file);
    char *path = g_path_get_dirname(input_file);
    input_basename = (char*)MALLOC(sizeof(char)*(strlen(base)+strlen(path)+2));

      // handle prepensions in the input filename
    int prepension = has_prepension(input_file);
    if (prepension > 0)
      sprintf(input_basename, "%s%c%s", path, DIR_SEPARATOR, base+prepension);
    else
      sprintf(input_basename, "%s%c%s", path, DIR_SEPARATOR, base);

    FREE(base);
    g_free(path);
  }
  else {
    input_basename = (char*)MALLOC(sizeof(char)*1);
    strcpy(input_basename, "");
  }

  // Write the config and proj files using the same base name but different extensions
  if (!s) {
    gchar msg[1024];
    g_snprintf(msg, sizeof(msg),
               "Invalid settings!");
    message_box(msg);
    return;
  }
  cf = fopen(config_file, "w");
  if (!cf)
  {
    gchar msg[1024];
    g_snprintf(msg, sizeof(msg),
               "Couldn't open configuration save file: %s", strerror(errno));
    message_box(msg);
    return;
  }

  if (s->geocode_is_checked) {
    gchar msg[1024];

    strcpy(projfile, tmp_projfile);
    pf = fopen(tmp_projfile, "w");
    if (!pf)
    {
      g_snprintf(msg, sizeof(msg),
                 "Couldn't open projection parameters save file: %s", strerror(errno));
      message_box(msg);
      return;
    }

    switch (s->projection)
    {
      case PROJ_UTM:
        fprintf(pf, "[Universal Transverse Mercator]\n");
        fprintf(pf, "Zone=%d\n", s->zone != 0 ? s->zone : 0);
        fprintf(pf, "Datum=%s\n", datum_string(s->datum));
        break;

      case PROJ_PS:
        // FIXME: Write spheroid/ellipsoid/ellipsoid-only datum to the PS proj file
        // We are not using the 'spheroid' field yet, e.g. when asf_convert uses the -rpf
        // option with a polar stereographic projection proj file, but hopefully we
        // will... mostly this is important just for the Hughes ellipsoid
        fprintf(pf, "[Polar Stereographic]\n");
        fprintf(pf, "First Standard Parallel=%.10f\n", s->plat1);
        fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
        fprintf(pf, "Area=%s\n", (s->lat0 > 0.0 || s->plat1 > 0.0) ? "North" : "South");
        if (s->datum == HUGHES_DATUM)
            fprintf(pf, "Spheroid=Hughes\n");
        else
            fprintf(pf, "Datum=%s\n", datum_string(s->datum));

        break;

      case PROJ_ALBERS:
        fprintf(pf, "[Albers Conical Equal Area]\n");
        fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
        fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
        fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
        fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
        fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
        break;

      case PROJ_LAMAZ:
        fprintf(pf, "[Lambert Azimuthal Equal Area]\n");
        fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
        fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
        fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
        break;

      case PROJ_LAMCC:
        fprintf(pf, "[Lambert Conformal Conic]\n");
        fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
        fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
        fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
        fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
        fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
        break;

      default:
        g_snprintf(msg, sizeof(msg),
                   "Unrecognized projection type found!  Projection parameters will not be saved.");
        message_box(msg);
        break;
    }
    fclose(pf);
  }

  fprintf(cf, "Temporary config file, generated by the ASF MapReady Tool\n");
  fprintf(cf, "File was generated on: %s\n\n", date_time_stamp());

  fprintf(cf, "[General]\n");
  fprintf(cf, "input file = %s\n", input_basename);
  fprintf(cf, "output file = %s\n", output_file);
  // fprintf(cf, "ancillary file = %s\n", s->ancillary_file);
  fprintf(cf, "import = 1\n");
  fprintf(cf, "external = %d\n", s->external_is_checked);
  fprintf(cf, "sar processing = %d\n", s->process_to_level1);
// fprintf(cf, "image stats=0\n");
// fprintf(cf, "detect corner reflectors = 0\n");
  int polarimetry_on =
    s->polarimetric_decomp_setting != POLARIMETRY_NONE || s->do_farcorr;
  fprintf(cf, "polarimetry = %d\n", polarimetry_on ? 1 : 0);
  fprintf(cf, "terrain correction = %d\n",
          s->terrcorr_is_checked || s->refine_geolocation_is_checked);
  fprintf(cf, "geocoding = %d\n", s->geocode_is_checked);
  fprintf(cf, "export = %d\n", s->export_is_checked);
// fprintf(cf, "default values =\n");
  // kludge on the intermediates -- we store our value +1, so that the
  // command-line tool always saves intermediates.
  fprintf(cf, "intermediates = %d\n", s->keep_files+1);
  fprintf(cf, "status file = %s\n", tmp_statfile);
  fprintf(cf, "short configuration file = 0\n");
  FILE *fpDefs = fopen_share_file("asf_mapready/asf_mapready.defaults", "rt");
  if (fpDefs) {
    fprintf(cf, "default values = %s/%s\n", get_asf_share_dir(),
            "asf_mapready/asf_mapready.defaults");
    FCLOSE(fpDefs);
  }
  fprintf(cf, "tmp dir = %s\n", tmp_dir);
//    fprintf(cf, "thumbnail = %d\n",
//            s->output_format == OUTPUT_FORMAT_GEOTIFF ? 1 : 0);
  fprintf(cf, "thumbnail = 1\n");
  fprintf(cf, "\n");

  fprintf(cf, "[Import]\n");
  fprintf(cf, "format = CEOS (1)\n");

  fprintf(cf, "radiometry = %s_image\n", settings_get_data_type_string(s));
// fprintf(cf, "look up table = \n");
  if (settings_get_input_data_format_allows_latitude(s) > 0) {
    fprintf(cf, "lat begin = %.2f\n", s->latitude_low);
    fprintf(cf, "lat end = %.2f\n", s->latitude_hi);
  }
// fprintf(cf, "precise =\n");
  fprintf(cf, "output db = %d\n", s->output_db);
  fprintf(cf, "apply ers2 gain fix = %d\n", s->apply_ers2_gain_fix);
  fprintf(cf, "polsarpro colormap = %s\n", s->polsarpro_colormap);
  fprintf(cf, "\n");

  if (s->external_is_checked) {
    fprintf(cf, "[External]\n");
    fprintf(cf, "# external selected = %d\n", s->external_selected);
    fprintf(cf, "# external params = %s\n", get_external_parameters_as_csv());
    fprintf(cf, "command = %s\n", s->cmd);
    fprintf(cf, "\n");
  }

  if (s->process_to_level1) {
    fprintf(cf, "[SAR processing]\n");
    fprintf(cf, "radiometry = %s_image\n",
            settings_get_data_type_string(s));
    fprintf(cf, "\n");
  }

  if (polarimetry_on) {
    fprintf(cf, "[Polarimetry]\n");
    fprintf(cf, "pauli = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_PAULI?1:0);
    fprintf(cf, "sinclair = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_SINCLAIR?1:0);
    fprintf(cf, "cloude pottier = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE8?1:0);
    fprintf(cf, "extended cloude pottier = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE16?1:0);
    fprintf(cf, "entropy anisotropy alpha = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE_NOCLASSIFY?1:0);
    fprintf(cf, "freeman durden = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_FREEMAN_DURDEN?1:0);

    int farcorr_code = FARCORR_OFF;
    if (s->do_farcorr)
      farcorr_code = s->farcorr_global_avg ? FARCORR_MEAN : FARCORR_SMOOTH;
    fprintf(cf, "faraday correction = %d\n", farcorr_code);
    fprintf(cf, "farcorr threshold = %f\n", s->farcorr_threshold);
    fprintf(cf, "\n");
  }

  if (s->terrcorr_is_checked || s->refine_geolocation_is_checked) {
    // Terrain correction section
    fprintf(cf, "[Terrain correction]\n");
    fprintf(cf, "digital elevation model = %s\n", s->dem_file);

    // items specific to either terrain correction or refine geolocation
    if (s->terrcorr_is_checked) {
      if (s->specified_tc_pixel_size)
        fprintf(cf, "pixel spacing = %.2lf\n", s->tc_pixel_size);
      else if (s->specified_pixel_size) // geocode pixel size
        fprintf(cf, "pixel spacing = %.2lf\n", s->pixel_size);
      fprintf(cf, "refine geolocation only = 0\n");
      fprintf(cf, "interpolate = %d\n", s->interp);
      fprintf(cf, "do radiometric = %d\n", s->do_radiometric);
      if(s->do_radiometric)
        fprintf(cf, "save incidence angles = %d\n", s->save_incid_angles);
      fprintf(cf, "save terrcorr dem = %d\n", s->generate_dem);
      fprintf(cf, "save terrcorr layover mask = %d\n",
              s->generate_layover_mask);
    } else if (s->refine_geolocation_is_checked) {
      fprintf(cf, "refine geolocation only = 1\n");
    }

    // terrain correction continued... stuff that applies to both
    fprintf(cf, "auto mask water = %d\n", s->auto_water_mask_is_checked);
    if (s->mask_file_is_checked) {
      fprintf(cf, "mask = %s\n", s->mask_file);
    }
    fprintf(cf, "smooth dem holes = %d\n", s->interp_dem_holes);
    fprintf(cf, "fill value = -1\n");
    fprintf(cf, "\n");
  }

  if (s->geocode_is_checked) {
    fprintf(cf, "[Geocoding]\n");
    fprintf(cf, "projection = %s\n", tmp_projfile);
    if (s->specified_pixel_size)
      fprintf(cf, "pixel spacing = %.2f\n", s->pixel_size);
    if (s->specified_height)
      fprintf(cf, "height = %.2f\n", s->height);
    // As of v1.1.x of MapReady, no longer write datum to
    // config file (only to proj file above)
    //fprintf(cf, "datum = %s\n", datum_string(s->datum));
    fprintf(cf, "resampling = %s\n",
            resample_method_string(s->resample_method));
    fprintf(cf, "force = %d\n", s->geocode_force);
    fprintf(cf, "\n");
  }

  if (s->export_is_checked) {
    fprintf(cf, "[Export]\n");
    fprintf(cf, "format = %s\n", settings_get_output_format_string(s));
    if (s->output_bytes && !s->truecolor_is_checked &&
        !s->falsecolor_is_checked)
    {
      fprintf(cf, "byte conversion = %s\n",
              scaling_method_string(s->scaling_method));
    } else {
      fprintf(cf, "byte conversion = none\n");
    }
    if (s->export_bands)
    {
      if (!s->truecolor_is_checked && !s->falsecolor_is_checked)
      {
        const char *r = strlen(s->red)   > 0 ? s->red   : "ignore";
        const char *g = strlen(s->green) > 0 ? s->green : "ignore";
        const char *b = strlen(s->blue)  > 0 ? s->blue  : "ignore";
        fprintf(cf, "rgb banding = %s,%s,%s\n", r, g, b);
      }
      else {
        fprintf(cf, "rgb banding = \n");
      }
      fprintf(cf, "truecolor = %d\n", s->truecolor_is_checked ? 1 : 0);
      fprintf(cf, "falsecolor = %d\n", s->falsecolor_is_checked ? 1 : 0);
    }
    fprintf(cf, "\n");
  }

  FREE(tmp_projfile);
  FREE(output_basename);
  FREE(input_basename);
  FREE(output_file);
  fclose(cf);
  settings_delete(s);
}

void hide_load_config_file_selection_dialog()
{
  GtkWidget *load_config_file_selection_dialog =
      get_widget_checked("load_config_selection");

  gtk_widget_hide(load_config_file_selection_dialog);
}

SIGNAL_CALLBACK gboolean
    on_load_config_file_selection_delete_event(GtkWidget *w)
{
  hide_load_config_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
    on_load_config_file_selection_destroy_event(GtkWidget *w)
{
  hide_load_config_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
    on_load_config_file_selection_destroy(GtkWidget *w)
{
  hide_load_config_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK void
    on_load_config_file_selection_ok_button_clicked(GtkWidget *widget)
{
  GtkWidget *file_selection_dialog;
  gchar **selections;
  gchar **current;

  file_selection_dialog =
      get_widget_checked("load_config_selection");

  selections = gtk_file_selection_get_selections(
      GTK_FILE_SELECTION(file_selection_dialog));

  current = selections;

  if (*current) {
    int ret;
    ret = apply_settings_from_config_file(*current);
    if (ret != 0) {
      message_box("\n Unable to apply filename settings from configuration \n"
          " file.  This may occur if you hand-edited the \n"
          " configuration file but did not specify full pathnames \n"
          " for all filenames, e.g. input data file, DEMs, masks, etc \n"
          "\n Either edit the configuration file again, or browse to \n"
          " the input files from each process tab. \n");
    }
  }

  g_strfreev(selections);
  gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
    on_load_config_file_selection_cancel_button_clicked(GtkWidget *widget)
{
  hide_load_config_file_selection_dialog();
}

SIGNAL_CALLBACK void
on_load_button_clicked(GtkWidget *w, gpointer data)
{
#ifdef win32
  OPENFILENAME of;
  int retval;
  char fname[1024];

  fname[0] = '\0';

  memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
  of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
  of.lStructSize = sizeof(of);
#endif

  of.hwndOwner = NULL;
  of.lpstrFilter = "ASF MapReady Configuration Files (*.cfg)\0*.cfg\0"
      "All Files\0*\0";
  of.lpstrCustomFilter = NULL;
  of.nFilterIndex = 1;
  of.lpstrFile = fname;
  of.nMaxFile = sizeof(fname);
  of.lpstrFileTitle = NULL;
  of.lpstrInitialDir = ".";
  of.lpstrTitle = "Select Input ASF Configuration File";
  of.lpstrDefExt = NULL;
  of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

  retval = GetOpenFileName(&of);

  if (!retval) {
    if (CommDlgExtendedError()) {
      message_box("File dialog box error");
    }
  }
  if (strlen(fname)) {
    int ret;
    ret = apply_settings_from_config_file(fname);
    if (ret != 0) {
      message_box("\n Unable to apply the settings from configuration file. \n"
          " This could occur because the file does not exist, or if you \n"
          " hand-edited the configuration file but did not specify full \n"
          " paths for all filenames, e.g. input data file, DEMs, masks, etc\n"
          "\n Either edit the configuration file again, or browse to \n"
          " the input files from each process tab. \n");
    }
  }
  else {
    message_box("Zero length filename found.");
  }

#else // #ifdef win32

  /* Linux version -- use GtkFileChooser if possible */

#ifdef USE_GTK_FILE_CHOOSER

  if (!load_config_widget)
    create_load_file_chooser_dialog();

  gtk_widget_show(load_config_widget);
  pending_load = TRUE;

#else // #ifdef USE_GTK_FILE_CHOOSER

  GtkWidget *file_selection_dialog =
    get_widget_checked("load_config_selection");

  gtk_widget_show(file_selection_dialog);

#endif // #ifdef USE_GTK_FILE_CHOOSER
#endif // #ifdef win32
  update_summary();
}

