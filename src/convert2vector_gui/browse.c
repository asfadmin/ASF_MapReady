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
#include <shlobj.h>
#endif

#include "c2v.h"
//  Commenting out the include for asf_vector, and just re-declaring the
//  needed prototypes... we have a POINT definition conflict to resolve...
//#include "asf_vector.h"
int ismetadata(char *inFile);
int isleader(char *inFile);
int ispoint(char *inFile);
int ispolygon(char *inFile);
int isshape(char *inFile);
int isgeotiff(char *inFile);
int isrgps(char *inFile);

void clear_results_message()
{
    put_string_to_label("result_label", "");
}

static int isauig(char *f)
{
    char *ext = findExt(f);
    if (!ext) {
        return FALSE;
    } else {
        if (strcmp_case(ext,".csv")==0) {
            FILE *fp = FOPEN(f,"r");
            if (!fp) return FALSE;
            char line[1024];
            if (!fgets(line,1023,fp)) return FALSE;
            // look for known column headers
            int ret =
                strstr(line,"SCNID")!=NULL && strstr(line,"SCN_LULAT")!=NULL;
            FCLOSE(fp);
            return ret;
        }
        else {
            return FALSE;
        }
    }
}

static int isursa(char *f)
{
    char *ext = findExt(f);
    if (!ext) {
        return FALSE;
    } else {
        if (strcmp_case(ext,".csv")==0) {
            FILE *fp = FOPEN(f,"r");
            if (!fp) return FALSE;
            char line[1024];
            if (!fgets(line,1023,fp)) return FALSE;
            // look for known column headers
            int ret =
                  strstr(line,"Granule Name")!=NULL &&
                  strstr(line,"Granule Type")!=NULL &&
                  strstr(line,"Platform")!=NULL;
            FCLOSE(fp);
            return ret;
        }
        else {
            return FALSE;
        }
    }
}

void change_output_extension(char *current)
{
    char *ext=NULL;
    int output_format = get_combo_box_item("output_format_combobox");
    switch (output_format) {
      case OUTPUT_KML:
        ext=".kml";
        break;
      case OUTPUT_SHAPE:
        ext=".shp";
        break;
      case OUTPUT_TEXT:
        ext=".csv";
        break;
      case OUTPUT_ALOS_CSV:
        ext=".csv";
        break;
      default:
        break;
    }

    if (ext) {
      char *base = get_filename(current);
      char *out_file = appendExt(base, ext);
      put_string_to_entry("output_file_entry", out_file);
      free(out_file);
      free(base);
    }
}

void select_defaults_by_file_type(char *f, int set_output_also)
{
    char *ext = findExt(f);

    // if we can figure it out by the extension, do that first
    if (ext && strcmp_case(ext, ".meta") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_META);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (ext && strcmp_case(ext, ".L") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_LEADER);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (ext && strcmp_case(ext, ".shp") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_SHAPE);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (ext && strcmp_case(ext, ".kml") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_KML);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_ALOS_CSV);
    }
    else if (isgeotiff(f)) {
      set_combo_box_item("input_format_combobox", INPUT_GEOTIFF);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (ismetadata(f)) {
      set_combo_box_item("input_format_combobox", INPUT_META);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (ispoint(f)) {
      set_combo_box_item("input_format_combobox", INPUT_POINT);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (ispolygon(f)) {
      set_combo_box_item("input_format_combobox", INPUT_POLYGON);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (isshape(f)) {
      set_combo_box_item("input_format_combobox", INPUT_SHAPE);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (isleader(f)) {
      set_combo_box_item("input_format_combobox", INPUT_LEADER);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    // Removing RGPS for now
    //else if (isrgps(f)) {
    //  set_combo_box_item("input_format_combobox", INPUT_RGPS);
    //  if (set_output_also)
    //    set_combo_box_item("output_format_combobox", OUTPUT_KML);
    //}
    else if (isauig(f)) {
      set_combo_box_item("input_format_combobox", INPUT_ALOS_CSV);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    else if (isursa(f)) {
      set_combo_box_item("input_format_combobox", INPUT_URSA);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
    // the generic csv case should be last, so that the auig/ursa cases
    // can take precedence
    else if (ext && strcmp_case(ext, ".csv") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_GENERIC_CSV);
      if (set_output_also)
        set_combo_box_item("output_format_combobox", OUTPUT_KML);
    }
}

void add_input_file(char *file)
{
    put_string_to_entry("input_file_entry", file);

    // set the input file type, based on the extension of this file
    select_defaults_by_file_type(file,FALSE);

    // set the output directory, if it is currently blank, to the
    // same as the input directory
    if (strlen(get_string_from_entry("output_directory_entry"))==0) {
      char *dir = get_dirname(file);
      put_string_to_entry("output_directory_entry", dir);
      free(dir);
    }
    clear_results_message();

    // change output filename, regardless
    change_output_extension(file);
}

SIGNAL_CALLBACK void on_output_format_combobox_changed(GtkWidget *w)
{
    // update the output file extension when user changes the output
    // format selection
    char *curr = get_string_from_entry("output_file_entry");
    char *ext = findExt(curr);
    if (ext) {
        // only change if current extension is something we know
        change_output_extension(curr);
    }

    // the "open output" checkbox is enabled only for Google Earth
    // and AUIG
    int enabled=FALSE;
    int output_format = get_combo_box_item("output_format_combobox");
    switch (output_format) {
      case OUTPUT_ALOS_CSV:
      case OUTPUT_KML:
        enabled=TRUE;
        break;
    }

    enable_widget("open_output_checkbutton", enabled);
    enable_widget("open_output_label", enabled);
    if (!enabled)
      set_checked("open_output_checkbutton", FALSE);
    else {
      if (output_format==OUTPUT_ALOS_CSV)
#ifdef win32
        put_string_to_label("open_output_label",
                            "Open Output:\n(in Excel)");
#else
        put_string_to_label("open_output_label","Open Output:");
#endif
      else if (output_format==OUTPUT_KML)
        put_string_to_label("open_output_label",
                            "Open Output:\n(in Google Earth)");
      else
        put_string_to_label("open_output_label","Open Output:");
    }
    clear_results_message();
}

SIGNAL_CALLBACK void on_input_format_combobox_changed(GtkWidget *w)
{
    clear_results_message();
}

#ifndef win32

static GtkWidget *input_browse_widget = NULL;
static GtkWidget *output_browse_widget = NULL;

// called when "cancel" clicked on the GtkFileChooser
SIGNAL_CALLBACK void input_browse_cancel_clicked()
{
    gtk_widget_hide(input_browse_widget);
}

SIGNAL_CALLBACK void output_browse_cancel_clicked()
{
    gtk_widget_hide(output_browse_widget);
}

// called when "ok" clicked on the GtkFileChooser
SIGNAL_CALLBACK void input_browse_ok_clicked()
{
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(input_browse_widget));

    gtk_widget_hide(input_browse_widget);
    if (files)
    {
        GSList *iter = files;

        do {
          gchar *s = (gchar *) iter->data;
          add_input_file(s);
          g_free(s);
          iter =  iter->next;
        }
        while(iter);

        g_slist_free(files);
    }
}

SIGNAL_CALLBACK void output_browse_ok_clicked()
{
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(output_browse_widget));

    gtk_widget_hide(output_browse_widget);
    if (files)
    {
        GSList *iter = files;

        do {
          gchar *s = (gchar *) iter->data;
          put_string_to_entry("output_directory_entry", s);
          g_free(s);
          iter =  iter->next;
        }
        while(iter);

        g_slist_free(files);
    }
}

SIGNAL_CALLBACK void input_browse_widget_destroy()
{
    gtk_widget_destroy(input_browse_widget);
    input_browse_widget = NULL;
}

SIGNAL_CALLBACK void output_browse_widget_destroy()
{
    gtk_widget_destroy(output_browse_widget);
    output_browse_widget = NULL;
}

// sets up the file chooser dialog
static void create_input_file_chooser_dialog()
{
    GtkWidget *parent = get_widget_checked("c2v_window");

    input_browse_widget = gtk_file_chooser_dialog_new(
        "Open Image File", GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_OPEN,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)input_browse_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(input_browse_cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(input_browse_ok_clicked), NULL);
    g_signal_connect(input_browse_widget, "destroy",
        G_CALLBACK(input_browse_widget_destroy), NULL);
    g_signal_connect(input_browse_widget, "destroy_event",
        G_CALLBACK(input_browse_widget_destroy), NULL);
    g_signal_connect(input_browse_widget, "delete_event",
        G_CALLBACK(input_browse_widget_destroy), NULL);

    // add the filters
    GtkFileFilter *csv_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(csv_filt,
                             "CSV (Point, Polygon, AUIG, etc) (*.csv)");
    gtk_file_filter_add_pattern(csv_filt, "*.csv");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                csv_filt);

    GtkFileFilter *meta_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(meta_filt, "Metadata Files (*.meta)");
    gtk_file_filter_add_pattern(meta_filt, "*.meta");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                meta_filt);

    GtkFileFilter *L_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(L_filt, "Leader Files (*.L,LED-*)");
    gtk_file_filter_add_pattern(L_filt, "*.L");
    gtk_file_filter_add_pattern(L_filt, "LED-*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                L_filt);

    //GtkFileFilter *LED_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(LED_filt, "ALOS Leader Files (LED-*)");
    //gtk_file_filter_add_pattern(LED_filt, "LED-*");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
    //                            LED_filt);

    //GtkFileFilter *pt_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(pt_filt, "Point/Polygon Files (*.csv)");
    //gtk_file_filter_add_pattern(pt_filt, "*.csv");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
    //                            pt_filt);

    //GtkFileFilter *poly_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(poly_filt, "Polygon Files (*.txt)");
    //gtk_file_filter_add_pattern(poly_filt, "*.txt");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
    //                            poly_filt);

    GtkFileFilter *shp_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(shp_filt, "Shape Files (*.shp)");
    gtk_file_filter_add_pattern(shp_filt, "*.shp");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                shp_filt);

    GtkFileFilter *kml_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(kml_filt, "KML Files (*.kml)");
    gtk_file_filter_add_pattern(kml_filt, "*.kml");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                kml_filt);

    GtkFileFilter *gtif_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(gtif_filt, "Geotiff Files (*.tif)");
    gtk_file_filter_add_pattern(gtif_filt, "*.tif");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                gtif_filt);

    //GtkFileFilter *rgps_filt = gtk_file_filter_new();
    //gtk_file_filter_set_name(rgps_filt, "RPGS Cell Files (*.rpgs)");
    //gtk_file_filter_add_pattern(rgps_filt, "*.rpgs");
    //gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
    //                            rgps_filt);

    GtkFileFilter *all_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(all_filt, "All Files (*.*)");
    gtk_file_filter_add_pattern(all_filt, "*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                all_filt);

    // allow multi-select ... turned off until we get that working
    //gtk_file_chooser_set_select_multiple(
    //    GTK_FILE_CHOOSER(input_browse_widget), TRUE);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(input_browse_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(input_browse_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(input_browse_widget),
                                    GTK_RESPONSE_OK);
}

static void create_output_file_chooser_dialog()
{
    GtkWidget *parent = get_widget_checked("c2v_window");

    output_browse_widget = gtk_file_chooser_dialog_new(
        "Select Output Directory", GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)output_browse_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(output_browse_cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(output_browse_ok_clicked), NULL);
    g_signal_connect(output_browse_widget, "destroy",
        G_CALLBACK(output_browse_widget_destroy), NULL);
    g_signal_connect(output_browse_widget, "destroy_event",
        G_CALLBACK(output_browse_widget_destroy), NULL);
    g_signal_connect(output_browse_widget, "delete_event",
        G_CALLBACK(output_browse_widget_destroy), NULL);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(output_browse_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(output_browse_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(output_browse_widget),
                                    GTK_RESPONSE_OK);
}

#endif // #ifndef win32

static void input_file_browse(void)
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
        "CSV (Point, Polygon, AUIG, etc) (*.csv)\0*.csv\0"
        "Metadata Files (*.meta)\0*.meta\0"
        "Leader Files (*.L,LED-*)\0*.L;LED-*\0"
        "ALOS Leader Files (LED-*)\0LED-*\0"
        "Point/Polygon Files (*.csv)\0*.csv\0"
        "Shape Files (*.shp)\0*.shp\0"
        "KML Files (*.kml)\0*.kml\0"
        "Geotiff Files (*.tif)\0*.tif\0"
      //"RPGS Files (*.rgps)\0*.rgps\0"
        "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            message_box("File dialog box error");
        return;
    }

    add_input_file(fname);
#else // #ifdef win32
    if (!input_browse_widget)
        create_input_file_chooser_dialog();

    gtk_widget_show(input_browse_widget);
#endif // #ifdef win32
}

static void output_file_browse(void)
{
#ifdef win32
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = "Select Output Directory";
    LPITEMIDLIST pidl = SHBrowseForFolder(&bi);
    if (pidl != 0)
    {
        TCHAR path[MAX_PATH];
        if (SHGetPathFromIDList(pidl, path))
            put_string_to_entry("output_directory_entry", path);
    }
#else // #ifdef win32
    if (!output_browse_widget)
        create_output_file_chooser_dialog();

    gtk_widget_show(output_browse_widget);
#endif // #ifdef win32
}

SIGNAL_CALLBACK void
on_input_browse_button_clicked(GtkWidget *w)
{
    input_file_browse();
}

SIGNAL_CALLBACK void
on_output_browse_button_clicked(GtkWidget *w)
{
    output_file_browse();
}

