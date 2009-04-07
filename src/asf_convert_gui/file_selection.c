#ifdef win32

/* ugly hack here... windef.h and asf_meta.h both define a BYTE symbol. */
/* since we don't use the BYTE from asf_meta.h here, we'll #define BYTE */
/* to something else during the processing of that header, leaving BYTE */
/* defined in windef.h alone (that's the one we want)                   */

#define BYTE __byte
#include "asf.h"
#include "asf_meta.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR
#endif

#include "asf_convert_gui.h"



#ifdef USE_GTK_FILE_CHOOSER

/* If the GtkFileChooser is available -- we'll use that instead of
   GtkFileSelection
*/
static GtkWidget *browse_widget = NULL;
static GtkWidget *ancillary_file_browse_widget = NULL;

// called when "cancel" clicked on the GtkFileChooser
static SIGNAL_CALLBACK void cancel_clicked()
{
    gtk_widget_hide(browse_widget);
}

// called when "ok" clicked on the GtkFileChooser
static SIGNAL_CALLBACK void ok_clicked()
{
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(browse_widget));
    int n_ok = 0;
    int n_not_ok = 0;

    gtk_widget_hide(browse_widget);
    if (files)
    {
        GSList *iter = files;
        gchar *s=NULL;

        do {
          s = (gchar *) iter->data;
          int ok = add_to_files_list(s);
          if (!ok)
            ++n_not_ok;
          else
            ++n_ok;
          iter =  iter->next;
        }
        while(iter);

        if (n_not_ok > 0) {
          if (s && n_not_ok == 1 && n_ok == 0) {
            // most common case -- adding a single file, and it did not work
            char *msg = MALLOC(sizeof(char)*(strlen(s)+128));
            sprintf(msg,"Unrecognized file:\n  %s\n\n"
                        "The file may be of a type not supported "
                        "by MapReady.\n", s);
            message_box(msg);
            free(msg);
          }
          else if (n_ok == 0) {
            // no files successfully added
            message_box("Couldn't add all of the selected files.\n"
                        "The files may be of a type not supported "
                        "by MapReady.\n");
          }
          else {
            // some were added, some failed
            message_box("Some of the files could not be added.\n"
                        "They may be of a type not supported "
                        "by MapReady.\n");
          }
        }

        // now free up everything
        iter = files;
        do {
          g_free((gchar *) iter->data);
          iter =  iter->next;
        }
        while(iter);
        g_slist_free(files);

        show_queued_thumbnails();
    }
}

// sets up the file chooser dialog
static void create_file_chooser_dialog()
{
    GtkWidget *parent = get_widget_checked("asf_convert");

    browse_widget = gtk_file_chooser_dialog_new(
        "Open Image File", GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_OPEN,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)browse_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(ok_clicked), NULL);

    // add the filters
    GtkFileFilter *ceos_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(ceos_filt, "All CEOS Level 1 Files");
    gtk_file_filter_add_pattern(ceos_filt, "*.L");
    gtk_file_filter_add_pattern(ceos_filt, "LED-*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), ceos_filt);

    GtkFileFilter *L_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(L_filt, "RSAT/ERS CEOS L1 (*.L)");
    gtk_file_filter_add_pattern(L_filt, "*.L");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), L_filt);

    //GtkFileFilter *stf_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(stf_filt, "STF Files (*.000)");
    //gtk_file_filter_add_pattern(stf_filt, "*.000");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), stf_filt);

    //GtkFileFilter *raw_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(raw_filt, "RAW Files (*.raw)");
    //gtk_file_filter_add_pattern(raw_filt, "*.raw");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), raw_filt);

    GtkFileFilter *geotiff_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(geotiff_filt, "GeoTIFF Files (*.tif)");
    gtk_file_filter_add_pattern(geotiff_filt, "*.tif");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), geotiff_filt);

    //GtkFileFilter *cpx_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(cpx_filt, "Complex Files (*.cpx)");
    //gtk_file_filter_add_pattern(cpx_filt, "*.cpx");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), cpx_filt);

    GtkFileFilter *alos_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(alos_filt, "ALOS Leader Files (LED-*)");
    gtk_file_filter_add_pattern(alos_filt, "LED-*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), alos_filt);

    GtkFileFilter *img_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(img_filt, "ASF Internal Files (*.img)");
    gtk_file_filter_add_pattern(img_filt, "*.img");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), img_filt);

    GtkFileFilter *airsar_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(airsar_filt, "AirSAR Leader Files (*.airsar)");
    gtk_file_filter_add_pattern(airsar_filt, "*.airsar");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), airsar_filt);

    GtkFileFilter *polsarpro_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(polsarpro_filt, "PolSARpro Classification Files (*.bin)");
    gtk_file_filter_add_pattern(polsarpro_filt, "*.bin");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), polsarpro_filt);

    GtkFileFilter *all_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(all_filt, "All Files (*.*)");
    gtk_file_filter_add_pattern(all_filt, "*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), all_filt);

    // allow multi-select
    gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(browse_widget), TRUE);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(browse_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(browse_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(browse_widget),
                                    GTK_RESPONSE_OK);
}

// called when "cancel" clicked on the GtkFileChooser
static SIGNAL_CALLBACK void ancillary_file_cancel_clicked()
{
  gtk_widget_hide(ancillary_file_browse_widget);
}

// called when "ok" clicked on the GtkFileChooser
// Note: Only adds one file to the list - into the ancillary file column
static SIGNAL_CALLBACK void ancillary_file_ok_clicked()
{
  GSList *file = gtk_file_chooser_get_filenames(
      GTK_FILE_CHOOSER(ancillary_file_browse_widget));

  gtk_widget_hide(ancillary_file_browse_widget);
  if (file)
  {
    gchar *s=NULL;

    s = (gchar *) file->data;
    int ok = add_to_ancillary_files_list(s);

    if (!ok) {
      // most common case -- adding a single file, and it did not work
      char *msg = MALLOC(sizeof(char)*(strlen(s)+128));
      sprintf(msg,"Unrecognized file or file was not added:\n  %s\n\n"
              "The file may be of a type not supported "
              "by MapReady or for some reason adding the file "
              "to the ancillary file column failed(!)", s);
      message_box(msg);
      free(msg);
    }
//    else {
//      add_thumbnail(s); // Add the ancillary file to the list of thumbnails to display
//      show_queued_thumbnails();
//    }

    // now free up everything
    g_slist_free(file);
  }
}

// sets up the file chooser dialog
static void create_ancillary_file_chooser_dialog()
{
  GtkWidget *parent = get_widget_checked("asf_convert");

  ancillary_file_browse_widget = gtk_file_chooser_dialog_new(
                                              "Select Ancillary File", GTK_WINDOW(parent),
      GTK_FILE_CHOOSER_ACTION_OPEN,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
      NULL);

  // we need to extract the buttons, so we can connect them to our
  // button handlers, above
  GtkHButtonBox *box =
      (GtkHButtonBox*)(((GtkDialog*)ancillary_file_browse_widget)->action_area);
  GList *buttons = box->button_box.box.children;

  GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
  GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

  g_signal_connect((gpointer)cancel_btn, "clicked",
                    G_CALLBACK(ancillary_file_cancel_clicked), NULL);
  g_signal_connect((gpointer)ok_btn, "clicked",
                    G_CALLBACK(ancillary_file_ok_clicked), NULL);

    // add the filters
  GtkFileFilter *ceos_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(ceos_filt, "All CEOS Level 1 Files");
  gtk_file_filter_add_pattern(ceos_filt, "*.L");
  gtk_file_filter_add_pattern(ceos_filt, "LED-*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), ceos_filt);

  GtkFileFilter *L_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(L_filt, "RSAT/ERS CEOS L1 (*.L)");
  gtk_file_filter_add_pattern(L_filt, "*.L");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), L_filt);

    //GtkFileFilter *stf_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(stf_filt, "STF Files (*.000)");
    //gtk_file_filter_add_pattern(stf_filt, "*.000");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), stf_filt);

    //GtkFileFilter *raw_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(raw_filt, "RAW Files (*.raw)");
    //gtk_file_filter_add_pattern(raw_filt, "*.raw");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), raw_filt);

  GtkFileFilter *geotiff_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(geotiff_filt, "GeoTIFF Files (*.tif)");
  gtk_file_filter_add_pattern(geotiff_filt, "*.tif");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), geotiff_filt);

    //GtkFileFilter *cpx_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(cpx_filt, "Complex Files (*.cpx)");
    //gtk_file_filter_add_pattern(cpx_filt, "*.cpx");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), cpx_filt);

  GtkFileFilter *alos_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(alos_filt, "ALOS Leader Files (LED-*)");
  gtk_file_filter_add_pattern(alos_filt, "LED-*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), alos_filt);

  GtkFileFilter *img_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(img_filt, "ASF Internal Files (*.img)");
  gtk_file_filter_add_pattern(img_filt, "*.img");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), img_filt);

  GtkFileFilter *airsar_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(airsar_filt, "AirSAR Leader Files (*.airsar)");
  gtk_file_filter_add_pattern(airsar_filt, "*.airsar");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), airsar_filt);

  GtkFileFilter *polsarpro_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(polsarpro_filt, "PolSARpro Classification Files (*.bin)");
  gtk_file_filter_add_pattern(polsarpro_filt, "*.bin");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), polsarpro_filt);

  GtkFileFilter *all_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(all_filt, "All Files (*.*)");
  gtk_file_filter_add_pattern(all_filt, "*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(ancillary_file_browse_widget), all_filt);

  // Don't allow multi-select for picking an ancillary file
  gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(ancillary_file_browse_widget), FALSE);

  // we need to make these modal -- if the user opens multiple "open"
  // dialogs, we'll get confused on the callbacks
  gtk_window_set_modal(GTK_WINDOW(ancillary_file_browse_widget), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(ancillary_file_browse_widget), TRUE);
  gtk_dialog_set_default_response(GTK_DIALOG(ancillary_file_browse_widget),
                                  GTK_RESPONSE_OK);
}

#endif // #ifdef USE_GTK_FILE_CHOOSER

/* danger: returns pointer to static data!! */
/* (Stolen from popup_menu.c)               */
static const char * imgloc(char * file)
{
  static char loc[1024];
  gchar * tmp = find_in_share(file);
  if (tmp) {
    strcpy(loc, tmp);
    g_free(tmp);
  } else {
    strcpy(loc, file);
  }

  return loc;
}

static int is_asf_complex_data(const char *meta_file)
{
    char *ext = findExt(meta_file);
    if (ext && strcmp_case(ext, ".meta")==0) {
        meta_parameters *meta = meta_read(meta_file);
        if (meta->general->data_type == COMPLEX_BYTE ||
            meta->general->data_type == COMPLEX_INTEGER16 ||
            meta->general->data_type == COMPLEX_INTEGER32 ||
            meta->general->data_type == COMPLEX_REAL32 ||
            meta->general->data_type == COMPLEX_REAL64)
        {
            meta_free(meta);
            return 1;
        }
        meta_free(meta);
    }

    return 0;
}

SIGNAL_CALLBACK void
on_browse_input_files_button_clicked(GtkWidget *widget)
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
    of.lpstrFilter =
            "CEOS Level 1 Files\0*.L;LED-*\0"
            "RSAT/ERS CEOS L1 (*.L)\0*.D\0"
            //"CEOS Level 0 (*.raw)\0*.raw\0"
            //"STF Files (*.000)\0*.000\0"
            "GeoTIFF Files (*.tif)\0*.tif\0"
             //"Complex Files (*.cpx)\0*.cpx\0"
            "ALOS Files (LED-*)\0LED-*\0"
            "AirSAR Files (*.airsar)\0*.airsar\0"
            "ASF Internal Files (*.img)\0*.img\0"
            "PolSARpro Classification Files (*.bin)\0*.img\0"
            "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_ALLOWMULTISELECT | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            message_box("File dialog box error");
        return;
    }

    /* the returned "fname" has the following form:            */
    /*   <directory>\0<first file>\0<second file>\0<third ...  */
    char * dir = strdup(fname);
    char * p = fname + strlen(dir) + 1;

    if (*p) {
        while (*p) {
            char * dir_and_file =
                malloc(sizeof(char)*(strlen(dir)+strlen(p)+5));
            sprintf(dir_and_file, "%s%c%s", dir, DIR_SEPARATOR, p);
            add_to_files_list(dir_and_file);
            p += strlen(p) + 1;
            free(dir_and_file);
        }
    } else {
        add_to_files_list(dir);
    }

    free(dir);
    show_queued_thumbnails();

#else // #ifdef win32

    /* Linux version -- use GtkFileChooser if possible */

#ifdef USE_GTK_FILE_CHOOSER

    if (!browse_widget)
        create_file_chooser_dialog();

    gtk_widget_show(browse_widget);

#else // #ifdef USE_GTK_FILE_CHOOSER

    GtkWidget *file_selection_dialog =
        get_widget_checked("input_file_selection");

    gtk_widget_show(file_selection_dialog);

#endif // #ifdef USE_GTK_FILE_CHOOSER
#endif // #ifdef win32
}

void
hide_input_file_selection_dialog()
{
    GtkWidget *file_selection_dialog =
        get_widget_checked("input_file_selection");

    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_input_file_selection_cancel_button_clicked(GtkWidget *widget)
{
    hide_input_file_selection_dialog();
}

SIGNAL_CALLBACK gboolean
on_input_file_selection_delete_event(GtkWidget *w)
{
    hide_input_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_input_file_selection_destroy_event(GtkWidget *w)
{
    hide_input_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_input_file_selection_destroy(GtkWidget *w)
{
    hide_input_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK void
on_input_file_selection_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *file_selection_dialog;
    gchar **selections;
    gchar **current;
    int i, n;

    file_selection_dialog =
        get_widget_checked("input_file_selection");

    selections = gtk_file_selection_get_selections(
        GTK_FILE_SELECTION(file_selection_dialog));

    current = selections;
    i = n = 0;

    while (*current)
    {
        /* second clause here allows silent fail for .L files, PR 92 */
        if ((add_to_files_list(*current) || is_meta_file(*current)) &&
             !is_asf_complex_data((const char *)(*current)))
        {
            ++i;
        }

        ++current;
        ++n;
    }

    if (i != n)
    {
        if (n == 1 || i == 0)
        {
            message_box(
              "Error: Unrecognized file type, file extension, or unsupported product level.  MapReady\n"
              "does not currently support Level 0, complex-valued, or ALOS PRISM or AVNIR2 Level 1A\n"
              "and 1B1 files.\n\n"
              " Please select the leader (.L, LED-, etc) file for product types higher than Level 0 to\n"
              "add files to the input file list.\n\n"
              " See 'asf_import' or the ASF SAR Training Processor ('stp') for more information\n"
              "on Level 0 processing.\n");
        }
        else
        {
            message_box("Some of the files were not added -- "
                        "unknown types or extensions.");
        }
    }

    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);

  show_queued_thumbnails();
}

void
hide_ancillary_file_selection_dialog()
{
  GtkWidget *file_selection_dialog =
      get_widget_checked("ancillary_file_selection");

  gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_ancillary_file_selection_cancel_button_clicked(GtkWidget *widget)
{
  hide_ancillary_file_selection_dialog();
}

SIGNAL_CALLBACK void
on_ancillary_file_selection_ok_button_clicked(GtkWidget *widget)
{
  GtkWidget *file_selection_dialog;
  gchar **selections;
  gchar **current, *input_file=NULL;
  int i, n;

  file_selection_dialog =
      get_widget_checked("ancillary_file_selection");

  selections = gtk_file_selection_get_selections(
      GTK_FILE_SELECTION(file_selection_dialog));

  current = selections;
  i = n = 0;

  while (*current)
  {
    /* second clause here allows silent fail for .L files, PR 92 */
    if ((add_to_ancillary_files_list(*current) || is_meta_file(*current)) &&
         !is_asf_complex_data((const char *)(*current)))
    {
      ++i;
    }
    if (i == 1) input_file = *current;

    ++current;
    ++n;
  }

  if (i != n)
  {
    if (n == 1 || i == 0)
    {
      message_box(
        "Error: Unrecognized file type, file extension, or unsupported product level.  MapReady\n"
        "does not currently support Level 0, complex-valued, or ALOS PRISM or AVNIR2 Level 1A\n"
        "and 1B1 files.\n\n"
        " Please select the leader (.L, LED-, etc) file for product types higher than Level 0 to\n"
        "add a file to the ancillary file list.\n\n");
    }
    else
    {
      message_box("The file was not added -- "
                  "unknown type or extension");
    }
  }

  g_strfreev(selections);
  gtk_widget_hide(file_selection_dialog);

//  add_thumbnail(input_file);
//  show_queued_thumbnails();
}

SIGNAL_CALLBACK gboolean
on_ancillary_file_selection_delete_event(GtkWidget *w)
{
  hide_ancillary_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
on_ancillary_file_selection_destroy_event(GtkWidget *w)
{
  hide_ancillary_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
on_ancillary_file_selection_destroy(GtkWidget *w)
{
  hide_ancillary_file_selection_dialog();
  return TRUE;
}

SIGNAL_CALLBACK void
on_ancillary_files_button_clicked(GtkWidget *widget)
{
  handle_browse_ancillary_file();
}

void handle_browse_ancillary_file()
{
  GtkWidget *files_list;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  //GtkTreeIter iter;
  gchar *input_file=NULL;
  int num_selected = 0;

  // If the Add Ancillary Files button's image is insensitive, then
  // that means it is disabled ...return with no action take.  (Note:
  // Yes, I could have set sensitivity to FALSE on the button itself,
  // but this not only greys it out, but also gives it a funky 'depressed
  // button' look that seems to imply the button has been clicked ...I like
  // how the GUI appears when taking this approach (below) better.)
  GtkWidget * w = get_widget_checked("ancillary_files_image");
  if (!GTK_WIDGET_SENSITIVE(GTK_WIDGET(w))) return;

  // Make sure a PolSARpro (or GAMMA) file is selected before firing off the dialog
  files_list = get_widget_checked("files_list");
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
  if (selection) {
    num_selected = gtk_tree_selection_count_selected_rows(selection);
    if (num_selected != 1) {
      // User must select a single input file
      message_box("\n Please select just one PolSARpro input file...  \n");
      return;
    }
    model = GTK_TREE_MODEL(list_store);
    GList *selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);
    if (!selected_rows) {
    // No input files were selected
      message_box("\n  Please select a PolSARpro file...  \n");
      return;
    }
    GtkTreePath *path;
    GtkTreeIter iter;
    path = (GtkTreePath *)selected_rows->data;
    gtk_tree_model_get_iter(model, &iter, path);
    gtk_tree_model_get(model, &iter, COL_INPUT_FILE, &input_file, -1);
    g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_rows);
  }
  if (num_selected <= 0 || !is_polsarpro((char *)input_file)) {
    // No input files were selected
    message_box("\n  Please select a PolSARpro file...  \n");
    g_free(input_file);
    return;
  }
  g_free(input_file);

  // Replace button image with non-animated version now that the user has clicked
  // on the button once...
  w = get_widget_checked("ancillary_files_image");
  gtk_image_set_from_file(GTK_IMAGE(w), imgloc("add_files_s.png"));

  // Get first selected input file from the files list so we can
  // associate the ancillary file with it

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
  of.lpstrFilter =
      "CEOS Level 1 Files\0*.L;LED-*\0"
      "RSAT/ERS CEOS L1 (*.L)\0*.D\0"
            //"CEOS Level 0 (*.raw)\0*.raw\0"
            //"STF Files (*.000)\0*.000\0"
      "GeoTIFF Files (*.tif)\0*.tif\0"
             //"Complex Files (*.cpx)\0*.cpx\0"
      "ALOS Files (LED-*)\0LED-*\0"
      "AirSAR Files (*.airsar)\0*.airsar\0"
      "ASF Internal Files (*.img)\0*.img\0"
      "PolSARpro Classification Files (*.bin)\0*.bin\0"
      "All Files (*.*)\0*\0";
  of.lpstrCustomFilter = NULL;
  of.nFilterIndex = 1;
  of.lpstrFile = fname;
  of.nMaxFile = sizeof(fname);
  of.lpstrFileTitle = NULL;
  of.lpstrInitialDir = ".";
  of.lpstrTitle = "Select Ancillary File";
  of.lpstrDefExt = NULL;
  //of.Flags = OFN_HIDEREADONLY | OFN_ALLOWMULTISELECT | OFN_EXPLORER;
  of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;  // Only allow selecting one ancillary file

  retval = GetOpenFileName(&of);

  if (!retval) {
    if (CommDlgExtendedError())
      message_box("File dialog box error");
    return;
  }

  /* the returned "fname" has the following form:            */
  /*   <directory>\0<first file>\0<second file>\0<third ...  */
  char * dir = strdup(fname); // Contains only the directory
  char * p = fname + strlen(dir) + 1; // Points at first file
  input_file = STRDUP(p);

  if (*p) {
    char * dir_and_file =
          malloc(sizeof(char)*(strlen(dir)+strlen(p)+5));
    sprintf(dir_and_file, "%s%c%s", dir, DIR_SEPARATOR, p);
    add_to_ancillary_files_list(dir_and_file);
    free(dir_and_file);
  } else {
    add_to_ancillary_files_list(dir);
  }

  free(dir);
//  add_thumbnail(input_file);
//  show_queued_thumbnails();
  FREE(input_file);

#else // #ifdef win32

  /* Linux version -- use GtkFileChooser if possible */

#ifdef USE_GTK_FILE_CHOOSER

  if (!ancillary_file_browse_widget)
    create_ancillary_file_chooser_dialog();

  gtk_widget_show(ancillary_file_browse_widget);

#else // #ifdef USE_GTK_FILE_CHOOSER

  GtkWidget *file_selection_dialog =
      get_widget_checked("input_file_selection");

  gtk_widget_show(file_selection_dialog);

#endif // #ifdef USE_GTK_FILE_CHOOSER
#endif // #ifdef win32
}

//-----------------------------------------------------------------------------
// Code supporting the "Add File With Ancillary Files" button
//-----------------------------------------------------------------------------

// These need to match the order of items specified for the
// "add_file_with_ancillary_format_combobox" widget in the .glade file
#define ADD_FILE_WITH_ANCILLARY_FORMAT_POLSARPRO 0
#define ADD_FILE_WITH_ANCILLARY_FORMAT_GAMMA 1
#define ADD_FILE_WITH_ANCILLARY_FORMAT_CEOS 2

// These specify which file filters are available when the user clicks
// one of the various "browse" buttons in the "add with ancillary" dialog
#define D_FILT 1
#define L_FILT 2
#define IMG_FILT 4
#define LED_FILT 8
#define ALL_CEOS_DATA_FILT 16
#define ALL_CEOS_LEADER_FILT 32
#define HDR_FILT 64
#define BIN_FILT 128

static void do_browse_ok_clicked(gpointer button)
{
  GtkWidget *browse_widget =
    GTK_WIDGET(g_object_get_data(G_OBJECT(button), "browse_widget"));
  char *entry_to_populate =
    (char*)g_object_get_data(G_OBJECT(browse_widget), "entry");

  GSList *files = gtk_file_chooser_get_filenames(
    GTK_FILE_CHOOSER(browse_widget));
    
  // done with this, now
  gtk_widget_destroy(browse_widget);

  if (files)
  {
    // we only have one file to add in the list
    gchar *s = (gchar *) files->data;
    put_string_to_entry(entry_to_populate, s);

    // now free up everything (use full-list freeing code, just in case)
    GSList *iter = files;
    do {
      g_free((gchar *) iter->data);
      iter =  iter->next;
    }
    while(iter);
    g_slist_free(files);
  }
}

static void do_browse_cancel_clicked(gpointer button)
{
  GtkWidget *browse_widget =
    GTK_WIDGET(g_object_get_data(G_OBJECT(button), "browse_widget"));
  gtk_widget_destroy(browse_widget);
}

static void do_browse(const char *title, const char *entry_to_populate,
                      int filts)
{
    GtkWidget *parent = get_widget_checked("asf_convert");

    GtkWidget *browse_widget = gtk_file_chooser_dialog_new(
        title, GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_OPEN,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)browse_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(do_browse_cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(do_browse_ok_clicked), NULL);

    // store the entry that should be populated as aux data in the widget
    g_object_set_data(G_OBJECT(browse_widget), "entry",
                      (gpointer)entry_to_populate);

    // store a pointer to the browse widget as aux data in the buttons
    g_object_set_data(G_OBJECT(cancel_btn), "browse_widget",
                      (gpointer)browse_widget);
    g_object_set_data(G_OBJECT(ok_btn), "browse_widget",
                      (gpointer)browse_widget);

    // add the filters
    if (filts & ALL_CEOS_DATA_FILT) {
      GtkFileFilter *ceos_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(ceos_filt, "All CEOS Level 1 Files");
      gtk_file_filter_add_pattern(ceos_filt, "*.D");
      gtk_file_filter_add_pattern(ceos_filt, "IMG-*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), ceos_filt);
    }
    if (filts & ALL_CEOS_LEADER_FILT) {
      GtkFileFilter *ceos_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(ceos_filt, "All CEOS Level 1 Files");
      gtk_file_filter_add_pattern(ceos_filt, "*.L");
      gtk_file_filter_add_pattern(ceos_filt, "LED-*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), ceos_filt);
    }
    if (filts & D_FILT) {
      GtkFileFilter *D_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(D_filt, "RSAT/ERS CEOS L1 (*.D)");
      gtk_file_filter_add_pattern(D_filt, "*.D");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), D_filt);
    }
    if (filts & L_FILT) {
      GtkFileFilter *L_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(L_filt, "RSAT/ERS CEOS L1 (*.L)");
      gtk_file_filter_add_pattern(L_filt, "*.L");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), L_filt);
    }
    if (filts & LED_FILT) {
      GtkFileFilter *alos_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(alos_filt, "ALOS Leader Files (LED-*)");
      gtk_file_filter_add_pattern(alos_filt, "LED-*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), alos_filt);
    }
    if (filts & IMG_FILT) {
      GtkFileFilter *img_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(img_filt, "ALOS Data Files (IMG-*)");
      gtk_file_filter_add_pattern(img_filt, "IMG-*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), img_filt);
    }
    if (filts & BIN_FILT) {
      GtkFileFilter *polsarpro_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(polsarpro_filt,
                               "PolSARpro Classification Files (*.bin)");
      gtk_file_filter_add_pattern(polsarpro_filt, "*.bin");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget),
                                  polsarpro_filt);
    }
    if (filts & HDR_FILT) {
      GtkFileFilter *hdr_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(hdr_filt, "PolSARpro Header Files (*.hdr)");
      gtk_file_filter_add_pattern(hdr_filt, "*.hdr");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), hdr_filt);
    }

    GtkFileFilter *all_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(all_filt, "All Files (*.*)");
    gtk_file_filter_add_pattern(all_filt, "*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), all_filt);

    // do not allow multi-select
    gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(browse_widget),
                                         FALSE);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(browse_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(browse_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(browse_widget),
                                    GTK_RESPONSE_OK);

    gtk_widget_show(browse_widget);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_button_clicked(GtkWidget *widget)
{
  show_widget("add_file_with_ancillary_dialog", TRUE);
}

static void clear_entries()
{
  put_string_to_entry("add_file_with_ancillary_gamma_ceos_entry", "");
  put_string_to_entry("add_file_with_ancillary_gamma_data_entry", "");
  put_string_to_entry("add_file_with_ancillary_gamma_metadata_entry", "");
  put_string_to_entry("add_file_with_ancillary_polsarpro_image_entry", "");
  put_string_to_entry("add_file_with_ancillary_polsarpro_ceos_entry", "");
  put_string_to_entry("add_file_with_ancillary_ceos_data_entry", "");
  put_string_to_entry("add_file_with_ancillary_ceos_leader_entry", "");
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_dialog_delete_event(GtkWidget *widget)
{
  clear_entries();
  show_widget("add_file_with_ancillary_dialog", FALSE);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_ceos_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add CEOS File", "add_file_with_ancillary_gamma_ceos_entry",
            L_FILT | LED_FILT | ALL_CEOS_LEADER_FILT);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_data_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Data File",
            "add_file_with_ancillary_gamma_data_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_metadata_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Metadata File",
            "add_file_with_ancillary_gamma_metadata_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_polsarpro_image_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add PolSARPro File",
            "add_file_with_ancillary_polsarpro_image_entry", BIN_FILT);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_polsarpro_ceos_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add CEOS File", "add_file_with_ancillary_polsarpro_ceos_entry",
            L_FILT | LED_FILT | ALL_CEOS_LEADER_FILT);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_ceos_data_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add CEOS Data File", "add_file_with_ancillary_ceos_data_entry",
            D_FILT | IMG_FILT | ALL_CEOS_DATA_FILT);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_ceos_leader_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add CEOS Leader File",
            "add_file_with_ancillary_ceos_leader_entry",
            L_FILT | LED_FILT | ALL_CEOS_LEADER_FILT);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_ok_button_clicked(GtkWidget *w)
{
  GtkWidget *combo =
    get_widget_checked("add_file_with_ancillary_format_combobox");
  int sel = gtk_combo_box_get_active(GTK_COMBO_BOX(combo));

  GtkTreeIter iter;
  int ok=TRUE;
  char *data=NULL;

  switch (sel) {
    case ADD_FILE_WITH_ANCILLARY_FORMAT_POLSARPRO:
    {
      char *ceos =
        get_string_from_entry("add_file_with_ancillary_polsarpro_ceos_entry");
      data =
        get_string_from_entry("add_file_with_ancillary_polsarpro_image_entry");
      ok = add_to_files_list_iter(data, ceos, NULL, &iter);
      break;
    }

    case ADD_FILE_WITH_ANCILLARY_FORMAT_GAMMA:
    {
      data = get_string_from_entry("add_file_with_ancillary_gamma_data_entry");
      char *meta =
        get_string_from_entry("add_file_with_ancillary_gamma_metadata_entry");
      char *ceos =
        get_string_from_entry("add_file_with_ancillary_gamma_ceos_entry");
      ok = add_to_files_list_iter(data, ceos, meta, &iter);
      break;
    }

    case ADD_FILE_WITH_ANCILLARY_FORMAT_CEOS:
    {
      data = get_string_from_entry("add_file_with_ancillary_ceos_data_entry");
      char *leader =
        get_string_from_entry("add_file_with_ancillary_ceos_leader_entry");
      ok = add_to_files_list_iter(data, NULL, leader, &iter);
      break;
    }
  }

  if (!ok) {
    if (data) {
      char *msg = MALLOC(sizeof(char)*(strlen(data)+128));
      sprintf(msg, "Unrecognized file:\n  %s\n\n"
              "The file may be of a type not supported by MapReady.\n", data);
      message_box(msg);
      free(msg);
    }
    else {
      char *msg = MALLOC(sizeof(char)*256);
      sprintf(msg, "Unrecognized file!\n\n"
              "The file may be of a type not supported by MapReady.\n");
      message_box(msg);
      free(msg);
    }
  }
  else {
    show_widget("add_file_with_ancillary_dialog", FALSE);
    clear_entries();

    show_queued_thumbnails();
  }
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_cancel_button_clicked(GtkWidget *w)
{
  clear_entries();
  show_widget("add_file_with_ancillary_dialog", FALSE);
}

static void ancillary_format_combobox_changed()
{
  GtkWidget *w = get_widget_checked("add_file_with_ancillary_format_combobox");
  int sel = gtk_combo_box_get_active(GTK_COMBO_BOX(w));

  show_widget("hbox_polsarpro", sel==ADD_FILE_WITH_ANCILLARY_FORMAT_POLSARPRO);
  show_widget("hbox_gamma", sel==ADD_FILE_WITH_ANCILLARY_FORMAT_GAMMA);
  show_widget("hbox_ceos", sel==ADD_FILE_WITH_ANCILLARY_FORMAT_CEOS);
}

void init_ancillary_format_combobox()
{
  GtkWidget *w = get_widget_checked("add_file_with_ancillary_format_combobox");
  gtk_combo_box_set_active(GTK_COMBO_BOX(w), 0);
  ancillary_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_format_combobox_changed(GtkWidget *w)
{
  ancillary_format_combobox_changed();
}
