// These need to match the order of items specified for the
// "add_file_with_ancillary_format_combobox" widget in the .glade file
#define ADD_FILE_WITH_ANCILLARY_FORMAT_POLSARPRO 0
#define ADD_FILE_WITH_ANCILLARY_FORMAT_GAMMA 1

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

// These need to match the order of items in the format selector next to the
// "Browse..." button
#define FORMAT_CEOS 0
#define FORMAT_AIRSAR 1
#define FORMAT_POLSARPRO 2
#define FORMAT_GAMMA 3
#define FORMAT_ROIPAC 4
#define FORMAT_TERRASARX 5
#define FORMAT_RADARSAT2 6
#define FORMAT_ALOS_MOSAIC 7
#define FORMAT_GEOTIFF 8
#define FORMAT_ASF_INTERNAL 9

#ifdef USE_GTK_FILE_CHOOSER
static GtkWidget *browse_widget = NULL;

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
static void create_file_chooser_dialog(int selected)
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
    if (selected==FORMAT_CEOS) {
      GtkFileFilter *ceos_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(ceos_filt, "All CEOS Level 1 Files");
      gtk_file_filter_add_pattern(ceos_filt, "*.L");
      gtk_file_filter_add_pattern(ceos_filt, "LED-*");
      gtk_file_filter_add_pattern(ceos_filt, "*.LEA");
      gtk_file_filter_add_pattern(ceos_filt, "*.lea");
      gtk_file_filter_add_pattern(ceos_filt, "LEA_*");
      gtk_file_filter_add_pattern(ceos_filt, "lea_*");
      gtk_file_filter_add_pattern(ceos_filt, "*.ldr");
      gtk_file_filter_add_pattern(ceos_filt, "*.sarl");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), ceos_filt);

      GtkFileFilter *L_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(L_filt, "RSAT/ERS/JERS CEOS L1 (*.L)");
      gtk_file_filter_add_pattern(L_filt, "*.L");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), L_filt);

      GtkFileFilter *alos_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(alos_filt, "ALOS Leader Files (LED-*)");
      gtk_file_filter_add_pattern(alos_filt, "LED-*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), alos_filt);

      GtkFileFilter *lea_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(lea_filt, "LEA Leader Files (LEA_*, lea_*, *.lea, *.LEA)");
      gtk_file_filter_add_pattern(lea_filt, "*.LEA");
      gtk_file_filter_add_pattern(lea_filt, "*.lea");
      gtk_file_filter_add_pattern(lea_filt, "LEA_*");
      gtk_file_filter_add_pattern(lea_filt, "lea_*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), lea_filt);

      GtkFileFilter *ldr_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(ldr_filt, "LDR Leader Files (*.ldr)");
      gtk_file_filter_add_pattern(ldr_filt, "*.ldr");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), ldr_filt);

      GtkFileFilter *sarl_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(sarl_filt, "SARL Leader Files (*.sarl)");
      gtk_file_filter_add_pattern(sarl_filt, "*.sarl");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), sarl_filt);
    }

    if (selected==FORMAT_GEOTIFF) {
      GtkFileFilter *geotiff_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(geotiff_filt, "GeoTIFF Files (*.tif)");
      gtk_file_filter_add_pattern(geotiff_filt, "*.tif");
      gtk_file_filter_add_pattern(geotiff_filt, "*.tiff");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget),
                                  geotiff_filt);
    }

    if (selected==FORMAT_TERRASARX) {
      GtkFileFilter *terrasar_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(terrasar_filt, "TerraSAR-X Metadata (*.xml)");
      gtk_file_filter_add_pattern(terrasar_filt, "*.xml");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget),
                                  terrasar_filt);
    }

    if (selected==FORMAT_RADARSAT2) {
      GtkFileFilter *radarsat2_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(radarsat2_filt, "Radarsat-2 Metadata (*.xml)");
      gtk_file_filter_add_pattern(radarsat2_filt, "*.xml");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget),
                                  radarsat2_filt);
    }

    if (selected==FORMAT_ROIPAC) {
      GtkFileFilter *roipac_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(roipac_filt, "ROI_PAC Metadata (*.rsc)");
      gtk_file_filter_add_pattern(roipac_filt, "*.rsc");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget),
                                  roipac_filt);
    }

    if (selected==FORMAT_ALOS_MOSAIC) {
      GtkFileFilter *alos_mosaic_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(alos_mosaic_filt, 
			       "ALOS mosaic Metadata (*HDR.txt, *HDR)");
      gtk_file_filter_add_pattern(alos_mosaic_filt, "*HDR.txt");
      gtk_file_filter_add_pattern(alos_mosaic_filt, "*HDR");      
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget),
                                  alos_mosaic_filt);
    }

    if (selected==FORMAT_ASF_INTERNAL) {
      GtkFileFilter *img_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(img_filt, "ASF Internal Files (*.img)");
      gtk_file_filter_add_pattern(img_filt, "*.img");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), img_filt);
    }

    if (selected==FORMAT_AIRSAR) {
      GtkFileFilter *airsar_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(airsar_filt, "AirSAR Leader Files (*.airsar)");
      gtk_file_filter_add_pattern(airsar_filt, "*.airsar");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), airsar_filt);
    }

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
#endif

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
  GtkWidget *combo = get_widget_checked("browse_format_combobox");
  GtkWidget *browse_select_colormap_optionmenu =
      get_widget_checked("browse_select_colormap_optionmenu");
  GtkWidget *browse_select_colormap_label =
      get_widget_checked("browse_select_colormap_label");
  GtkWidget *browse_select_image_data_type_optionmenu =
      get_widget_checked("browse_select_colormap_optionmenu");
  GtkWidget *browse_select_image_data_type_label =
      get_widget_checked("browse_select_colormap_label");
  int sel = gtk_combo_box_get_active(GTK_COMBO_BOX(combo));
  GtkWidget *ok_button =
    get_widget_checked("add_file_with_ancillary_ok_button");

  // open the "add with ancillary" if needed, otherwise we'll use the
  // normal 'open file' dialog
  if (sel==FORMAT_GAMMA || sel==FORMAT_POLSARPRO) {
    show_widget("hbox_polsarpro", sel==FORMAT_POLSARPRO);
    show_widget("hbox_gamma", sel==FORMAT_GAMMA);
    switch (sel) {
      case FORMAT_GAMMA:
        put_string_to_label("add_with_ancillary_format_label", "GAMMA");
	show_widget("hbox_gamma_description", TRUE);
	gtk_widget_set_sensitive(ok_button, TRUE);	
        break;
      case FORMAT_POLSARPRO:
        put_string_to_label("add_with_ancillary_format_label", "PolSARPro");
	show_widget("hbox_gamma_description", FALSE);
        gtk_widget_show(browse_select_colormap_optionmenu);
        gtk_widget_show(browse_select_colormap_label);
        gtk_widget_show(browse_select_image_data_type_optionmenu);
        gtk_widget_show(browse_select_image_data_type_label);
	gtk_widget_set_sensitive(ok_button, FALSE);	
	init_image_data_type_combobox();
	polsarpro_image_data_type_changed();
        break;
      default:
        put_string_to_label("add_with_ancillary_format_label", "Unknown");
        break;
    }
    show_widget("add_file_with_ancillary_dialog", TRUE);
    return;
  }

  // normal case -- not requiring ancillary files

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

    switch (sel) {
      case FORMAT_CEOS:
        of.lpstrFilter =
            "CEOS Level 1 Files\0*.L;LED-*;*.LEA;*.lea;LEA_*;lea_*;*.ldr;*.sarl\0"
            "RSAT/ERS CEOS L1 (*.L)\0*.L\0"
            "ALOS Files (LED-*)\0LED-*\0"
            "LEA Leader Files (LEA_*, lea_*, *.lea, *.LEA)\0LEA_*;lea_*;*.lea;*.LEA\0"
            "LDR Leader Files (*.ldr)\0*.ldr\0"
            "SARL Leader Files (*.sarl)\0*.sarl\0"
            "All Files\0*\0";
        break;

      case FORMAT_AIRSAR:
        of.lpstrFilter =
            "AirSAR Files (*.airsar)\0*.airsar\0"
            "All Files\0*\0";
        break;

      case FORMAT_TERRASARX:
        of.lpstrFilter =
            "TerraSAR-X Metadata Files (*.xml)\0*.xml\0"
            "All Files\0*\0";
        break;

      case FORMAT_RADARSAT2:
        of.lpstrFilter =
            "Radarsat-2 Metadata Files (*.xml)\0*.xml\0"
            "All Files\0*\0";
        break;

      case FORMAT_ROIPAC:
        of.lpstrFilter =
            "ROI_PAC Metadata Files (*.rsc)\0*.rsc\0"
            "All Files\0*\0";
        break;

      case FORMAT_ALOS_MOSAIC:
        of.lpstrFilter =
            "ALOS mosaic Metadata Files (*HDR.txt, *HDR.txt)\0*HDR.txt;*HDR\0"
            "All Files\0*\0";
        break;

      case FORMAT_GEOTIFF:
        of.lpstrFilter =
            "GeoTIFF Files (*.tif, *.tiff)\0*.tif;*.tiff\0"
            "All Files\0*\0";
        break;

      case FORMAT_ASF_INTERNAL:
        of.lpstrFilter =
            "ASF Internal Files (*.img)\0*.img\0"
            "All Files\0*\0";
        break;
    }

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
    char * dir = STRDUP(fname);
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
    }
    else {
        add_to_files_list(dir);
    }

    free(dir);
    show_queued_thumbnails();

#else // #ifdef win32

    /* Linux version -- use GtkFileChooser if possible */

#ifdef USE_GTK_FILE_CHOOSER

    if (browse_widget)
      gtk_widget_destroy(browse_widget);
    create_file_chooser_dialog(sel);

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
      printf("in while loop\n");
        /* second clause here allows silent fail for .L files, PR 92 */
        if ((add_to_files_list(*current) || is_meta_file(*current)) &&
             !is_asf_complex_data((const char *)(*current)))
        {
	  printf("passed conditions - counting up\n");
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
              "Error: Unrecognized file type, file extension, or unsupported "
              "product level.  MapReady\ndoes not currently support Level 0, "
              "complex-valued, or ALOS PRISM or AVNIR2 Level 1A\nand 1B1 "
              "files.\n\n"
              "Please select the leader (.L, LED-, etc) file for product "
              "types higher than Level 0 to\nadd files to the input file "
              "list.\n\n"
              "See 'asf_import' or the ASF SAR Training Processor ('stp') "
              "for more information\non Level 0 processing.\n");
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
#define RSC_FILT 256
#define XML_FILT 512
#define MOSAIC_FILT 1024
#define DIR_FILT 2048

#ifndef win32
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
#endif

static void do_browse(const char *title, const char *entry_to_populate,
                      int filts)
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

    if (filts == (L_FILT | LED_FILT | ALL_CEOS_LEADER_FILT | XML_FILT | 
		  RSC_FILT | MOSAIC_FILT)) {
      of.lpstrFilter =
        "CEOS Level 1 Files\0*.L;LED-*\0"
        "RSAT/ERS CEOS L1\0*.L\0"
        "ALOS Leader Files\0LED-*\0"
	"TerraSAR-X/Radarsat-2\0*.xml\0"
	"ALOS mosaics\0*HDR.txt;*HDR\0"
	"ROI_PAC Files\0*.rsc\0"
        "All Files\0*\0";
    }
    else if (filts == BIN_FILT) {
      of.lpstrFilter =
        "PolSARPro Files\0*.bin\0"
        "All Files\0*\0";
    }
    else {
      of.lpstrFilter = "All Files\0*\0";
    }

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
    char * dir = STRDUP(fname);
    char * p = fname + strlen(dir) + 1;

    if (*p) {
        while (*p) {
            char * dir_and_file =
                malloc(sizeof(char)*(strlen(dir)+strlen(p)+5));
            sprintf(dir_and_file, "%s%c%s", dir, DIR_SEPARATOR, p);
            put_string_to_entry(entry_to_populate, dir_and_file);
            p += strlen(p) + 1;
            free(dir_and_file);
        }
    }
    else {
      put_string_to_entry(entry_to_populate, dir);
    }

    free(dir);

#else // #ifdef win32

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
                               "PolSARpro Data Files (*.bin)");
      gtk_file_filter_add_pattern(polsarpro_filt, "*.bin");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget),
                                  polsarpro_filt);
    }
    if (filts & HDR_FILT) {
      GtkFileFilter *hdr_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(hdr_filt, "PolSARPro Header Files (*.hdr)");
      gtk_file_filter_add_pattern(hdr_filt, "*.hdr");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), hdr_filt);
    }
    if (filts & XML_FILT) {
      GtkFileFilter *xml_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(xml_filt, "TerraSAR-X/Radarsat-2 Files (*.xml)");
      gtk_file_filter_add_pattern(xml_filt, "*.xml");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), xml_filt);
    }
    if (filts & RSC_FILT) {
      GtkFileFilter *rsc_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(rsc_filt, "ROI_PAC Files (*.rsc)");
      gtk_file_filter_add_pattern(rsc_filt, "*.rsc");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), rsc_filt);
    }
    if (filts & MOSAIC_FILT) {
      GtkFileFilter *mosaic_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(mosaic_filt, "ALOS mosaic Files (*HDR.txt, *HDR)");
      gtk_file_filter_add_pattern(mosaic_filt, "*HDR.txt");
      gtk_file_filter_add_pattern(mosaic_filt, "*HDR");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), mosaic_filt);
    }
    if (filts & DIR_FILT) {
      GtkFileFilter *dir_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(dir_filt, "PolSARPro matrix directory");
      gtk_file_filter_add_pattern(dir_filt, "*.");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), dir_filt);
    }
    else {
      GtkFileFilter *all_filt = gtk_file_filter_new();
      gtk_file_filter_set_name(all_filt, "All Files (*.*)");
      gtk_file_filter_add_pattern(all_filt, "*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), all_filt);
    }

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
#endif
}

void clear_entries()
{
  put_string_to_entry("add_file_with_ancillary_gamma_baseline_entry", "");
  put_string_to_entry("add_file_with_ancillary_gamma_slave_entry", "");
  put_string_to_entry("add_file_with_ancillary_gamma_coh_entry", "");
  put_string_to_entry("add_file_with_ancillary_gamma_igram_entry", "");
  put_string_to_entry("add_file_with_ancillary_gamma_data_entry", "");
  put_string_to_entry("add_file_with_ancillary_gamma_metadata_entry", "");
  put_string_to_entry("add_file_with_ancillary_polsarpro_image_entry", "");
  put_string_to_entry("add_file_with_ancillary_polsarpro_ceos_entry", "");
  put_string_to_label("add_with_ancillary_error_label", "");
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_dialog_delete_event(GtkWidget *widget)
{
  clear_entries();
  show_widget("add_file_with_ancillary_dialog", FALSE);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_baseline_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Baseline File", 
	    "add_file_with_ancillary_gamma_baseline_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_slave_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Slave Image Metadata", 
	    "add_file_with_ancillary_gamma_slave_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_coh_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Coherence Image", 
	    "add_file_with_ancillary_gamma_coh_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_igram_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Interferogram", 
	    "add_file_with_ancillary_gamma_igram_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_data_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Amplitude Data File",
            "add_file_with_ancillary_gamma_data_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_gamma_metadata_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add GAMMA Ampliutde Metadata File",
            "add_file_with_ancillary_gamma_metadata_entry", 0);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_polsarpro_image_browse_button_clicked(GtkWidget *w)
{
  GtkWidget *combo = 
    get_widget_checked("browse_select_image_data_type_optionmenu");
  int selected = gtk_combo_box_get_active(GTK_COMBO_BOX(combo));
  if (selected == SELECT_POLARIMETRIC_MATRIX)
    do_browse("Add PolSARPro Matrix File",
            "add_file_with_ancillary_polsarpro_image_entry", BIN_FILT);
  else
    do_browse("Add PolSARPro File",
	      "add_file_with_ancillary_polsarpro_image_entry", BIN_FILT);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_polsarpro_ceos_browse_button_clicked(GtkWidget *w)
{
  do_browse("Add CEOS File", "add_file_with_ancillary_polsarpro_ceos_entry",
            L_FILT | LED_FILT | ALL_CEOS_LEADER_FILT | XML_FILT);
}

SIGNAL_CALLBACK void
on_add_file_with_ancillary_ok_button_clicked(GtkWidget *w)
{
  const char *type = get_string_from_label("add_with_ancillary_format_label");
  int sel=-1;
  if (strcmp_case(type, "GAMMA")==0)
    sel = ADD_FILE_WITH_ANCILLARY_FORMAT_GAMMA;
  else if (strcmp_case(type, "PolSARPro")==0)
    sel = ADD_FILE_WITH_ANCILLARY_FORMAT_POLSARPRO;
  GtkTreeIter iter;
  int ok=TRUE;
  char *dataFile=NULL, *data=NULL, *ceos=NULL;
  char *aux_info="";

  switch (sel) {
    case ADD_FILE_WITH_ANCILLARY_FORMAT_POLSARPRO:
    {
      GtkWidget *ok_button =
	get_widget_checked("add_file_with_ancillary_ok_button");
      gtk_widget_set_sensitive(ok_button, FALSE);
      dataFile =
        get_string_from_entry("add_file_with_ancillary_polsarpro_image_entry");
      char *matrixType, *error, *decompositionType, *derror;
      char *serror, *perror;
      int is_polsarpro_matrix = 
	isPolsarproMatrix(dataFile, &matrixType, &error);
      int is_polsarpro_decomposition =
	isPolsarproDecomposition(dataFile, &decompositionType, &derror);
      int is_polsarpro_segmentation =
	isPolsarproSegmentation(dataFile, &serror);
      int is_polsarpro_parameter =
	isPolsarproParameter(dataFile, &perror);
      if (is_polsarpro_matrix && !is_polsarpro_decomposition &&
	  !is_polsarpro_segmentation && !is_polsarpro_parameter) {
	data = (char *) MALLOC(sizeof(char)*(strlen(dataFile) + 15));
	if (!is_dir(dataFile)) {
	  char *tmp = (char *) MALLOC(sizeof(char)*(strlen(dataFile)+1));
	  tmp = get_dirname(dataFile);
	  dataFile[strlen(tmp)-1] = '\0';
	  FREE(tmp);
	}
	if (strcmp(matrixType, "T3") == 0 || strcmp(matrixType, "T4") == 0)
	  sprintf(data, "%s/T11.bin", dataFile);
	else if (strcmp(matrixType, "C2") == 0 || 
		 strcmp(matrixType, "C3") == 0 ||
		 strcmp(matrixType, "C4") == 0)
	  sprintf(data, "%s/C11.bin", dataFile);
      }
      else
	data = STRDUP(dataFile);
      free(matrixType);
      int is_geocoded = isGeocoded(data);
      if (!is_geocoded)
	ceos = get_string_from_entry(
	  "add_file_with_ancillary_polsarpro_ceos_entry");
      if (!is_geocoded && (strlen(ceos)==0 || strlen(data)==0)) {
        put_string_to_label("add_with_ancillary_error_label",
                            "Please choose all required files!");
        return;
      }
      else {
        GtkWidget *browse_option_menu =
          get_widget_checked("browse_select_colormap_optionmenu");
        GtkWidget *menu = gtk_option_menu_get_menu(
          GTK_OPTION_MENU(browse_option_menu));
        GtkWidget *selected_item = gtk_menu_get_active(GTK_MENU(menu));
	GtkWidget *combo = 
	  get_widget_checked("browse_select_image_data_type_optionmenu");
	int image_data_type = gtk_combo_box_get_active(GTK_COMBO_BOX(combo));
	char *lut_basename = g_object_get_data(G_OBJECT(selected_item), "file");
	aux_info = encode_polsarpro_aux_info(image_data_type, lut_basename);
        put_string_to_label("add_with_ancillary_error_label", "");
        ok = add_to_files_list_iter(data, ceos, NULL, aux_info, 
				    NULL, NULL, NULL, NULL, &iter);

        free(aux_info);
      }
      break;
    }

    case ADD_FILE_WITH_ANCILLARY_FORMAT_GAMMA:
    {
      GtkWidget *ok_button =
	get_widget_checked("add_file_with_ancillary_ok_button");
      gtk_widget_set_sensitive(ok_button, TRUE);
      data = get_string_from_entry("add_file_with_ancillary_gamma_data_entry");
      char *meta =
        get_string_from_entry("add_file_with_ancillary_gamma_metadata_entry");
      if (strlen(data)==0 || strlen(meta)==0) {
        put_string_to_label("add_with_ancillary_error_label",
                            "Please choose all required files!");
        return;
      }
      else {
        put_string_to_label("add_with_ancillary_error_label", "");
	char *interferogram, *coherence, *slave_metadata, *baseline;
	interferogram =
	  get_string_from_entry("add_file_with_ancillary_gamma_igram_entry");
	coherence =
	  get_string_from_entry("add_file_with_ancillary_gamma_coh_entry");
	slave_metadata =
	  get_string_from_entry("add_file_with_ancillary_gamma_slave_entry");
	baseline =
	  get_string_from_entry("add_file_with_ancillary_gamma_baseline_entry");
	if (strlen(interferogram) == 0) {
	  FREE(interferogram);
	  interferogram = NULL;
	}
	if (strlen(coherence) == 0) {
	  FREE(coherence);
	  coherence = NULL;
	}
	if (strlen(slave_metadata) == 0) {
	  FREE(slave_metadata);
	  slave_metadata = NULL;
	}
	if (strlen(baseline) == 0) {
	  FREE(baseline);
	  baseline = NULL;
	}
        ok = add_to_files_list_iter(data, ceos, meta, aux_info, 
				    interferogram, coherence, 
				    slave_metadata, baseline, &iter);
      }
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

void init_browse_format_combobox()
{
  GtkWidget *w = get_widget_checked("browse_format_combobox");
  gtk_combo_box_set_active(GTK_COMBO_BOX(w), 0);
}

void init_image_data_type_combobox()
{
  GtkWidget *w = 
    get_widget_checked("browse_select_image_data_type_optionmenu");
  gtk_combo_box_set_active(GTK_COMBO_BOX(w), 0);
}
