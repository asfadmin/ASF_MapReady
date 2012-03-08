#ifdef win32
/*  #define BYTE __byte*/
  #include "asf_meta.h"
/*  #undef BYTE*/
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
  FILE *cf, *pf;
  Settings * s;
  char *input_basename=NULL;
  char *output_file=NULL;
  char *output_basename=NULL;
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

    char *proj_str = settings_to_proj_string(s);
    fprintf(pf, proj_str);
    fclose(pf);
    free(proj_str);
  }

  char *cfg_str = settings_to_config_string(s, "", "",
                      "", "", "", "", "", "", "", "", "", "", "", projfile);
  fprintf(cf, cfg_str);
  fclose(cf);

  FREE(tmp_projfile);
  FREE(output_basename);
  FREE(input_basename);
  FREE(output_file);
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

