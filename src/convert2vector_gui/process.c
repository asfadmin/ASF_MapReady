#include "c2v.h"
#include "asf_vector.h"

SIGNAL_CALLBACK void on_convert_button_clicked(GtkWidget *w)
{
  process();
}

const char *input_format_to_str(int input_format)
{
  switch (input_format) {
    case INPUT_META: return "META";
    case INPUT_LEADER: return "LEADER";
    case INPUT_POINT: return "POINT";
    case INPUT_POLYGON: return "POLYGON";
    case INPUT_SHAPE: return "SHAPE";
    case INPUT_KML: return "KML";
    case INPUT_GEOTIFF: return "GEOTIFF";
    case INPUT_ALOS_CSV: return "AUIG";
    case INPUT_GENERIC_CSV: return "CSV";
    default: assert(0); return "";
  }
}

const char *output_format_to_str(int output_format)
{
  switch (output_format) {
    case OUTPUT_TEXT: return "CSV";
    case OUTPUT_SHAPE: return "SHAPE";
    case OUTPUT_KML: return "KML";
    case OUTPUT_ALOS_CSV: return "AUIG";
    default: assert(0); return "";
  }
}

void process()
{
  char in_file[1024];
  char out_file[1024];

  clear_results_message();

  strcpy(in_file, get_string_from_entry("input_file_entry"));

  char *odir = get_string_from_entry("output_directory_entry");
  char *ofile = get_string_from_entry("output_file_entry");

  if (strlen(odir) > 0) {
#ifdef win32
    if (odir[strlen(odir)-1]=='/' || odir[strlen(odir)-1]=='\\')
#else
    if (odir[strlen(odir)-1]=='/')
#endif
      sprintf(out_file, "%s%s", odir, ofile);
    else
      sprintf(out_file, "%s/%s", odir, ofile);
  }
  else {
    strcpy(out_file, ofile);
  }

  if (strlen(out_file)==0) {
    message_box("No outfile file selected!");
    return;
  }
  else if (strlen(in_file)==0) {
    message_box("No input file specified!");
    return;
  }

  if (!fileExists(in_file)) {
    message_box("Input file %s not found!", in_file);
    return;
  }

  int ret=1, nfiles=1;
  int input_format = get_combo_box_item("input_format_combobox");
  const char *inFormat = input_format_to_str(input_format);

  int output_format = get_combo_box_item("output_format_combobox");
  const char *outFormat = output_format_to_str(output_format);

  ret = convert2vector(in_file, inFormat, out_file, outFormat, FALSE);

  int open_output = get_checked("open_output_checkbutton");

  char msg[255];
  if (ret == nfiles) {
    if (nfiles == 1) {
      sprintf(msg, "Processed successfully!");
    }
    else {
      sprintf(msg, "Processed all %d files successfully!", nfiles);
    }
  }
  else {
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
  asfPrintStatus(msg);
  asfPrintStatus("\n\nDone.\n\n");

  if (open_output) {
    switch (output_format) {
      case OUTPUT_KML:
        open_in_google_earth(out_file);
        break;
      case OUTPUT_ALOS_CSV:
        open_in_excel(out_file);
      default:
        // do nothing, output type has no natural associated app
        break;
    }
  }
}
