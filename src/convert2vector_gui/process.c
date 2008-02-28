#include "c2v.h"

SIGNAL_CALLBACK void on_convert_button_clicked(GtkWidget *w)
{
  char in_file[1024];
  char out_file[1024];

  strcpy(in_file, get_string_from_entry("input_file_entry"));

  char *odir = get_string_from_entry("output_directory_entry");
  char *ofile = get_string_from_entry("output_file_entry");
  if (strlen(odir) > 0)
    sprintf(out_file, "%s/%s", odir, ofile);
  else
    strcpy(out_file, ofile);

  if (strlen(out_file)==0) {
    message_box("No outfile file selected!");
    return;
  } else if (strlen(in_file)==0) {
    message_box("No input file specified!");
    return;
  }

  if (!fileExists(in_file)) {
    message_box("Input file %s not found!", in_file);
    return;
  }

  int ret, nfiles=1;
  int input_format = get_combo_box_item("input_format_combobox");
  int output_format = get_combo_box_item("output_format_combobox");

  if (input_format == INPUT_FORMAT_ALOS_CSV &&
      output_format == OUTPUT_FORMAT_KML)
  {
    printf("Converting ALOS CSV to kml.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    ret = alos_csv_to_kml(in_file, out_file);
  }
  else
  {
    message_box(
      "Unsupported combination of Input and Output formats selected.");
  }

  int open_output = get_checked("open_output_checkbutton");

  char msg[255];
  if (ret == nfiles) {
    if (nfiles == 1) {
      sprintf(msg, "Processed successfully!");
    } else {
      sprintf(msg, "Processed all %d files successfully!", nfiles);
    }
  } else {
    if (nfiles == 1) {
      sprintf(msg, "Processing failed!");
    } else {
      sprintf(msg, "Processed %d of %d file%s successfully, %d file%s failed.",
              ret, nfiles, nfiles==1?"":"s", nfiles-ret, nfiles-ret==1?"":"s");
    }
    // don't open, if not completely successful
    open_output = FALSE;
  }
  put_string_to_label("result_label", msg);

  if (open_output) {
    switch (output_format) {
      case OUTPUT_FORMAT_KML:
        open_in_google_earth(out_file);
        break;
      default:
        // should never get here
        assert(FALSE);
    }
  }
}
