#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <sys/wait.h>

#include "find_in_path.h"


/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

GladeXML *glade_xml;
gboolean user_modified_output_file = FALSE;

/* danger: returns pointer to static data!! */
static const char * imgloc(char * file)
{
    static char loc[1024];
    gchar * tmp = find_in_path(file);
    if (tmp) {
      strcpy(loc, tmp);
      g_free(tmp);
    } else {
      strcpy(loc, file);
    }

    return loc;
}

static void set_help_image(int step)
{
    char widget_name[256];
    sprintf(widget_name, "step%d_help_image", step);

    GtkWidget * w =
      glade_xml_get_widget(glade_xml, widget_name);

    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("info_on_sml.gif"));
}

static void set_images()
{
    GtkWidget * range_compression_image;
    GtkWidget * azimuth_compression_image;

    range_compression_image =
	glade_xml_get_widget(glade_xml, "range_compression_image");

    azimuth_compression_image =
	glade_xml_get_widget(glade_xml, "azimuth_compression_image");

    gtk_image_set_from_file(GTK_IMAGE(range_compression_image),
			    imgloc("rc.png"));
    gtk_image_set_from_file(GTK_IMAGE(azimuth_compression_image),
			    imgloc("ac.png"));

    GtkWidget * flowchart_image;

    flowchart_image =
	glade_xml_get_widget(glade_xml, "flowchart_image");
    
    int i;
    for (i = 1; i <= 12; ++i)
      set_help_image(i);
}

SIGNAL_CALLBACK void
on_input_file_browse_button_clicked(GtkWidget *button)
{
    GtkWidget * file_selection_dialog =
	glade_xml_get_widget (glade_xml, "file_selection_dialog");

    gtk_widget_show (file_selection_dialog);
}

static void
hide_file_selection_dialog ()
{
    GtkWidget *file_selection_dialog =
	glade_xml_get_widget(glade_xml, "file_selection_dialog");
 
    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_file_selection_dialog_cancel_button_clicked(GtkWidget *w)
{
    hide_file_selection_dialog ();
}

SIGNAL_CALLBACK gboolean
on_file_selection_dialog_delete_event(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}
 
SIGNAL_CALLBACK gboolean
on_file_selection_dialog_destroy_event(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_file_selection_dialog_destroy(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

static void
add_file (const gchar * filename)
{
    GtkWidget *input_file_entry;

    input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    gtk_entry_set_text(GTK_ENTRY(input_file_entry), filename);
}

SIGNAL_CALLBACK void
on_file_selection_dialog_ok_button_clicked(GtkWidget *w)
{
    GtkWidget *file_selection_dialog;

    gchar **selections;
    gchar **current;

    file_selection_dialog =
	glade_xml_get_widget(glade_xml, "file_selection_dialog");

    selections = gtk_file_selection_get_selections(
	GTK_FILE_SELECTION(file_selection_dialog));

    current = selections;
    
    while (*current)
    {	
	add_file(*current);
	++current;
    }
    
    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_aisp_main_destroy(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

void
set_step_enabled(int step, gboolean enabled)
{
    char label_id[32];
    char table_id[32];

    sprintf(label_id, "step%d_label", step);
    sprintf(table_id, "step%d_table", step);

    GtkWidget * label =
	glade_xml_get_widget(glade_xml, label_id);

    GtkWidget * table =
	glade_xml_get_widget(glade_xml, table_id);

    gtk_widget_set_sensitive(label, enabled);
    gtk_widget_set_sensitive(table, enabled);
}

void
range_complex_multiply_checkbutton_toggled()
{
    GtkWidget * range_complex_multiply_checkbutton =
	glade_xml_get_widget(glade_xml, "range_complex_multiply_checkbutton");

    gboolean is_checked =
	gtk_toggle_button_get_active(
	    GTK_TOGGLE_BUTTON(range_complex_multiply_checkbutton));

    set_step_enabled(3, is_checked);
    set_step_enabled(4, is_checked);
    set_step_enabled(5, is_checked);
}

void
range_migration_checkbutton_toggled()
{
    GtkWidget * range_migration_checkbutton =
	glade_xml_get_widget(glade_xml, "range_migration_checkbutton");

    gboolean is_checked =
	gtk_toggle_button_get_active(
	    GTK_TOGGLE_BUTTON(range_migration_checkbutton));

//  set_step_enabled(7, is_checked);
    set_step_enabled(8, is_checked);
}

void
azimuth_complex_multiply_checkbutton_toggled()
{
    GtkWidget * azimuth_complex_multiply_checkbutton =
	glade_xml_get_widget(glade_xml,
			     "azimuth_complex_multiply_checkbutton");

    gboolean is_checked =
	gtk_toggle_button_get_active(
	    GTK_TOGGLE_BUTTON(azimuth_complex_multiply_checkbutton));

    set_step_enabled(9, is_checked);
    set_step_enabled(10, is_checked);
    set_step_enabled(11, is_checked);
}

SIGNAL_CALLBACK void
on_range_complex_multiply_checkbutton_toggled(GtkWidget *widget)
{
    range_complex_multiply_checkbutton_toggled();
}

SIGNAL_CALLBACK void
on_range_migration_checkbutton_toggled(GtkWidget *widget)
{
    range_migration_checkbutton_toggled();
}

SIGNAL_CALLBACK void
on_azimuth_complex_multiply_checkbutton_toggled(GtkWidget *widget)
{
    azimuth_complex_multiply_checkbutton_toggled();
}

void
set_button_text(int step, const char *file, const char *postfix)
{
    gchar tmp[1024];
    sprintf(tmp, "%s%s", file, postfix);

    char button_id[32];

    sprintf(button_id, "step%d_togglebutton", step);

    GtkWidget * button =
	glade_xml_get_widget(glade_xml, button_id);

    gtk_button_set_label(GTK_BUTTON(button), tmp);
}

static void
set_widget_sensitive(const char * w, gboolean setting)
{
    GtkWidget *aisp_main_scrolledwindow = glade_xml_get_widget(glade_xml, w);
    gtk_widget_set_sensitive(aisp_main_scrolledwindow, setting);
}

const gchar *
update_output_filename(const gchar * input_file_and_path)
{
    GtkWidget * output_file_entry =
	glade_xml_get_widget(glade_xml, "output_file_entry");

    if (user_modified_output_file || strlen(input_file_and_path) == 0) {
        return gtk_entry_get_text(GTK_ENTRY(output_file_entry));
    }
    
    gchar *output_file_and_path;

    output_file_and_path = 
      (gchar *) g_malloc(strlen(input_file_and_path) + 32);

    strcpy(output_file_and_path, input_file_and_path);

    char * p = strrchr(input_file_and_path, '.');

    if (p) {
        gchar * ext = g_strdup(p + 1);
	*p = '\0';
	strcat(output_file_and_path, "_cpx.");
	strcat(output_file_and_path, ext);
	g_free (ext);
    } else {
        strcat(output_file_and_path, "_cpx");
    }

    gtk_entry_set_text(GTK_ENTRY(output_file_entry), output_file_and_path);

    return output_file_and_path;
}

void
update_buttons()
{
    GtkWidget * input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    const gchar * input_file_and_path =
	gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    set_widget_sensitive("execute_button", strlen(input_file_and_path) > 0);

    const gchar * output_file_and_path =
        update_output_filename(input_file_and_path);

    gchar * input_file =
	g_path_get_basename(output_file_and_path);

    if (strlen(output_file_and_path) == 0 || strlen(input_file) == 0) {
	g_free(input_file);
	input_file = g_strdup("*");
    } else {
	char * p = strrchr(input_file, '.');
	if (p)
	    *p = '\0';
    }

    set_button_text(1, input_file, "_range_raw_t");
    set_button_text(2, input_file, "_range_raw_f");
    set_button_text(3, input_file, "_range_ref_t");
    set_button_text(4, input_file, "_range_ref_f");
    set_button_text(5, input_file, "_range_X_f");
    set_button_text(6, input_file, "_az_raw_t");
    set_button_text(7, input_file, "_az_raw_f");
    set_button_text(8, input_file, "_az_mig_f");
    set_button_text(9, input_file, "_az_ref_t");
    set_button_text(10, input_file, "_az_ref_f");
    set_button_text(11, input_file, "_az_X_f");
    set_button_text(12, input_file, "_az_X_t");

    g_free(input_file);
}

SIGNAL_CALLBACK void
on_input_file_entry_changed(GtkEditable *editable, gpointer user_data)
{
    update_buttons();
}

SIGNAL_CALLBACK gboolean
on_output_file_entry_key_press_event(GtkEditable *editable, gpointer user_data)
{
    user_modified_output_file = TRUE;
    return FALSE;
}

int
keep_flag(int step, int value)
{
    char button_id[32];
    sprintf(button_id, "step%d_togglebutton", step);

    GtkWidget * toggle_button =
	glade_xml_get_widget(glade_xml, button_id);

    if (!GTK_WIDGET_IS_SENSITIVE(toggle_button))
	return 0;

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(toggle_button)))
	return value;
    else
	return 0;
}

int
execute_flag(const char *id, int value)
{
    char button_id[128];
    sprintf(button_id, "%s_checkbutton", id);

    GtkWidget * checkbutton =
	glade_xml_get_widget(glade_xml, button_id);

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton)))
	return 0;
    else
	return value;
}

static void
set_widgets_sensitive(gboolean setting)
{
    set_widget_sensitive("execute_button", setting);
    set_widget_sensitive("azimuth_complex_multiply_checkbutton", setting);
    set_widget_sensitive("range_complex_multiply_checkbutton", setting);
    set_widget_sensitive("range_migration_checkbutton", setting);
    set_widget_sensitive("step1_togglebutton", setting);
    set_widget_sensitive("step2_togglebutton", setting);
    set_widget_sensitive("step3_togglebutton", setting);
    set_widget_sensitive("step4_togglebutton", setting);
    set_widget_sensitive("step5_togglebutton", setting);
    set_widget_sensitive("step6_togglebutton", setting);
    set_widget_sensitive("step7_togglebutton", setting);
    set_widget_sensitive("step8_togglebutton", setting);
    set_widget_sensitive("step9_togglebutton", setting);
    set_widget_sensitive("step10_togglebutton", setting);
    set_widget_sensitive("step11_togglebutton", setting);
    set_widget_sensitive("step12_togglebutton", setting);
    set_widget_sensitive("input_file_entry", setting);
    set_widget_sensitive("input_file_browse_button", setting);
}

void
execute()
{
    int debug_flag = 
	keep_flag(1, 8192) |
	keep_flag(2, 4096) |
	keep_flag(3, 2048) |
	keep_flag(4, 1024) |
	keep_flag(5, 512) |
	keep_flag(6, 128) |
	keep_flag(7, 64) |
	keep_flag(8, 32) |
	keep_flag(9, 16) |
	keep_flag(10, 8) |
	keep_flag(11, 4) |
	keep_flag(12, 2) |
	execute_flag("range_complex_multiply", 16384) |
	execute_flag("range_migration", 32768) |
        execute_flag("azimuth_complex_multiply", 65536);

    if (debug_flag == 0)
      debug_flag = 1;

    GtkWidget * input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    const char * input_file =
	gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    char * p = strrchr(input_file, '.');

    char * output_file =
	(char *) malloc(sizeof(char) * (strlen(input_file) + 20));
    
    if (p) {
	char * ext = strdup(p + 1);
	strcpy(output_file, input_file);
	*(output_file + (p - input_file)) = '\0';
	strcat(output_file, "_cpx");
	//strcat(output_file, ext);
	free(ext);
    } else {
	sprintf(output_file, "%s%s", input_file, "_output");
    }

    set_widgets_sensitive(FALSE);

    char cmd[1024];
    sprintf(cmd, "aisp -p 1 -debug %d %s %s",
	    debug_flag, input_file, output_file);

    free(output_file);
    printf("%s\n", cmd);

    system(cmd);

    set_widgets_sensitive(TRUE);
}

SIGNAL_CALLBACK void
on_execute_button_clicked(GtkWidget *button, gpointer user_data)
{
    int pid = fork();

    if (pid == 0)
    {
      execute();
      exit(EXIT_SUCCESS);
    }
    else
    {
      while (waitpid(-1, NULL, WNOHANG) == 0)
      {
	while (gtk_events_pending())
	  gtk_main_iteration();

	g_usleep(50);
      }
    }
}

void
set_underlines_off(int step)
{
    char button_id[32];

    sprintf(button_id, "step%d_togglebutton", step);

    GtkWidget * button =
	glade_xml_get_widget(glade_xml, button_id);

    gtk_button_set_use_underline(GTK_BUTTON(button), FALSE);
}

void
toggle_button(const char * name, gboolean setting)
{
    GtkWidget * btn =
      glade_xml_get_widget(glade_xml, name);

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(btn), setting);
}

void
set_toggles()
{
    toggle_button("range_migration_checkbutton", TRUE);
    toggle_button("range_complex_multiply_checkbutton", TRUE);
    toggle_button("azimuth_complex_multiply_checkbutton", TRUE);

    range_complex_multiply_checkbutton_toggled();
    range_migration_checkbutton_toggled();
    azimuth_complex_multiply_checkbutton_toggled();

    int i;
    for (i = 1; i <= 12; ++i)
	set_underlines_off(i);
}

void
help_text(int step)
{
  static const char * step1_help_title =
    "Step 1: Raw data (time)";

  static const char * step1_help =
    "The raw data is, at its simplest, a series of complex numbers "
    "(I and Q) that were sampled at the instrument and recorded at "
    "the ground station. By the time the data gets to the processor, "
    "it typically is digitized and stripped of encoding and metadata. "
    "Values are generally 4-bit or 5-bit.";

  static const char * step2_help_title =
    "Step 2: Range FFT";

  static const char * step2_help =
    "A fast Fourier transform converts the data from time domain (the "
    "way it was recorded at the receiver) to frequency domain. The data "
    "were acquired as a function of range, so each range line is "
    "transformed one at a time.\n"
    "\n"
    "This raw data set has the same information as the one in the time "
    "domain, but is now in the frequency domain.";

  static const char * step3_help_title =
    "Step 3: Range reference function";

  static const char * step3_help =
    "The range reference function is a series of complex numbers that "
    "represents the original chirp transmitted by the antenna. It is "
    "sampled at the same rate as the raw data, and is either pulled "
    "from the raw data or generated from metadata values such as chirp "
    "rate, center frequency, or bandwidth.";

  static const char * step4_help_title =
    "Step 4: Range reference function (frequency domain)";

  static const char * step4_help =
    "This range reference function has the same information as the one "
    "in the time domain, but is now in the frequency domain.";

  static const char * step5_help_title =
    "Step 5: Complex multiplication";

  static const char * step5_help =
    "Each element of the raw data (in frequency domain) is multiplied "
    "by the complex conjugate of the corresponding element of the "
    "range reference function (in frequency domain).\n"
    "\n"
    "The result from the complex multiplication is now ready for an "
    "inverse FFT.";

  static const char * step6_help_title =
    "Step 6: Range Inverse FFT";

  static const char * step6_help =
    "The result from the complex multiplication is converted back to "
    "time domain with the inverse fast Fourier transform.\n"
    "\n"
    "Range compressed data consists of the convolution of the range "
    "reference function with the raw data in the range direction. "
    "According to convolution theorem, converting two functions to "
    "frequency domain followed by a complex multiply, and converting "
    "back to time domain is the equivalent of the convolution of the "
    "two functions. The data is said to be 'compressed' because the "
    "chirp has been removed and all the signal returns from a single "
    "target are compressed into a single area. ";

  static const char * step7_help_title =
    "Step 7: Azimuth FFT";

  static const char * step7_help =
    "A fast Fourier transform converts the data from time domain (the "
    "way it was recorded at the receiver) to frequency domain. The "
    "data are typically corner-turned so that what was a column of "
    "data in the raw data array is now a row. This is the physical "
    "equivalent of ordering values from different pulses that were "
    "acquired at the same relative time within the pulse. These "
    "iso-range azimuth lines are transformed one at a time.\n"
    "\n"
    "This data set has the same information as the one in the time "
    "domain, but is now in the frequency domain after the time domain "
    "data was corner turned, changing the orientation from range to "
    "azimuth.";

  static const char * step8_help_title =
    "Step 8: Range Cell migration";

  static const char * step8_help =
    "As a target was sampled in the azimuth direction, the returns "
    "are not necessarily contained within a signal iso-range line. "
    "The migration of the signal from one bin into another means "
    "that some of the power you want to compress is in a completely "
    "different column of data. Range cell migration realigns all "
    "the returns for a single target into an appropriate single "
    "line of data in preparation for azimuth compression.\n"
    "\n"
    "All the azimuth returns from a single target have now been "
    "shifted into their respective lines so that azimuth compression "
    "will lead to well-focussed data. If you leave out range cell "
    "migration, returns from targets can be distributed across two "
    "or three azimuth lines. The result in the final image is "
    "poor resolution and smearing of the target in the range "
    "direction.";

  static const char * step9_help_title =
    "Step 9: Azimuth reference function (time)";

  static const char * step9_help =
    "The azimuth reference function is similar to the range reference "
    "function. In this case, however, the geometry of the satellite "
    "moving past the target changes the frequency on the return signal "
    "in the azimuth direction.  This Doppler effect, with higher "
    "frequencies as the satellite is moving toward the target, and "
    "lower frequencies as it moves away, is a sort of 'chirp'. The "
    "azimuth reference function is an array of numbers that represent "
    "the chirp, and is calculated uniquely for each image, and each "
    "iso-range line in the azimuth direction.\n"
    "\n"
    "It is critical that the geometry is estimated accurately, but this "
    "can be difficult, especially for low signal-to-noise data. Most "
    "processing errors come from getting this geometry wrong. The "
    "geometry is also referred to as the Doppler Centroid.";

  static const char * step10_help_title =
    "Step 10: Azimuth reference function (frequency)";

  static const char * step10_help =
    "This azimuth reference function has the same information as the "
    "one in the time domain, but is now in the frequency domain.";

  static const char * step11_help_title =
    "Step 11: Complex multiplication";

  static const char * step11_help =
    "Each element of the data (in frequency domain) is multiplied by "
    "the complex conjugate of the corresponding element of the "
    "azimuth reference function (in frequency domain).";

  static const char * step12_help_title =
    "Step 12: Azimuth compressed data (time domain - final product)";

  static const char * step12_help =
    "The result from the complex multiplication is converted back to "
    "time domain with the inverse fast Fourier transform.\n"
    "\n"
    "The data have now been compressed in both dimensions (range "
    "and azimuth) and include adjustments for range-cell migration. "
    "A few steps remain to get the final image. The data are typically "
    "corner turned again (to return it to the recognizable range-azimuth "
    "direction), and then converted from complex data to an amplitude "
    "and a phase image. The images are then resampled to the desired "
    "pixel size, and any radiometric calibration or adjustments are "
    "applied. For visualization purposes, we have taken the phase "
    "image and converted it to color assigned by phase range.";

  const char *title;
  const char *help_text;

  switch (step) {
    default:
      title = "";                help_text = "";          break;
    case 1:
      title = step1_help_title;  help_text = step1_help;  break;
    case 2:
      title = step2_help_title;  help_text = step2_help;  break;
    case 3:
      title = step3_help_title;  help_text = step3_help;  break;
    case 4:
      title = step4_help_title;  help_text = step4_help;  break;
    case 5:
      title = step5_help_title;  help_text = step5_help;  break;
    case 6:
      title = step6_help_title;  help_text = step6_help;  break;
    case 7:
      title = step7_help_title;  help_text = step7_help;  break;
    case 8:
      title = step8_help_title;  help_text = step8_help;  break;
    case 9:
      title = step9_help_title;  help_text = step9_help;  break;
    case 10:
      title = step10_help_title; help_text = step10_help; break;
    case 11:
      title = step11_help_title; help_text = step11_help; break;
    case 12:
      title = step12_help_title; help_text = step12_help; break;
  }

  GtkWidget * help_label =
    glade_xml_get_widget(glade_xml, "help_label");

  char label_text[2048];
  sprintf(label_text, "\n%s\n\n%s\n", title, help_text);

  gtk_label_set_text(GTK_LABEL(help_label), label_text);

  GtkWidget * flowchart_image;

  flowchart_image =
    glade_xml_get_widget(glade_xml, "flowchart_image");

  char image_file[128];
  sprintf(image_file, "step%d.gif", step);

  gtk_image_set_from_file(GTK_IMAGE(flowchart_image), imgloc(image_file));
}

SIGNAL_CALLBACK void
on_step1_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(1);
}

SIGNAL_CALLBACK void
on_step2_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(2);
}

SIGNAL_CALLBACK void
on_step3_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(3);
}

SIGNAL_CALLBACK void
on_step4_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(4);
}

SIGNAL_CALLBACK void
on_step5_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(5);
}

SIGNAL_CALLBACK void
on_step6_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(6);
}

SIGNAL_CALLBACK void
on_step7_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(7);
}

SIGNAL_CALLBACK void
on_step8_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(8);
}

SIGNAL_CALLBACK void
on_step9_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(9);
}

SIGNAL_CALLBACK void
on_step10_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(10);
}

SIGNAL_CALLBACK void
on_step11_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(11);
}

SIGNAL_CALLBACK void
on_step12_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(12);
}

int
main(int argc, char **argv)
{
    gchar *glade_xml_file;

    gtk_init(&argc, &argv);
    // set_font();

    glade_xml_file = (gchar *) find_in_path("aisp_gui.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    if (argc > 1)
      add_file(argv[1]);

    set_images();
    set_toggles();
    help_text(1);
    update_buttons();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();
    
    exit (EXIT_SUCCESS);
}
