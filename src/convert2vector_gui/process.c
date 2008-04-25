#include "c2v.h"
#include "asf_vector.h"

SIGNAL_CALLBACK void on_convert_button_clicked(GtkWidget *w)
{
  process();
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
  } else if (strlen(in_file)==0) {
    message_box("No input file specified!");
    return;
  }

  if (!fileExists(in_file)) {
    message_box("Input file %s not found!", in_file);
    return;
  }

  int ret=1, nfiles=1;
  int input_format = get_combo_box_item("input_format_combobox");
  int output_format = get_combo_box_item("output_format_combobox");

  // some conversions perfer to be passed the basename
  char *in_base = stripExt(in_file);
  char *out_base = stripExt(out_file);

  if (input_format == INPUT_ALOS_CSV && output_format == OUTPUT_KML)
  {
    printf("Converting ALOS CSV to kml.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    ret = alos_csv_to_kml(in_file, out_file);
  }
  else if (input_format == INPUT_KML && output_format == OUTPUT_ALOS_CSV)
  {
    printf("Converting kml to ALOS CSV.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    ret = kml_to_alos_csv(in_file, out_file);
  }
  else if (input_format == INPUT_META && output_format == OUTPUT_SHAPE)
  {
    printf("Converting metadata file to a shape file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_shape(in_base, out_base, META, 0);
  }
  else if (input_format == INPUT_META && output_format == OUTPUT_KML)
  {
    printf("Converting metadata file to a kml file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_kml(in_base, out_base, META, 0);
  }
  else if (input_format == INPUT_META && output_format == OUTPUT_TEXT)
  {
    printf("Converting metadata file to a CSV polygon text file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_text(in_base, out_base, META, 0);
  }
  else if (input_format == INPUT_LEADER && output_format == OUTPUT_SHAPE)
  {
    printf("Converting leader file to a shape file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_shape(in_base, out_base, META, 0);
  }
  else if (input_format == INPUT_LEADER && output_format == OUTPUT_KML)
  {
    printf("Converting leader file to a kml file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_kml(in_base, out_base, META, 0);
  }
  else if (input_format == INPUT_LEADER && output_format == OUTPUT_TEXT)
  {
    printf("Converting leader file to a CSV polygon text file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_text(in_base, out_base, META, 0);
  }
  else if (input_format == INPUT_POINT && output_format == OUTPUT_SHAPE)
  {
    printf("Converting point file to a shape file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_shape(in_file, out_base, POINT, 0);
  }
  else if (input_format == INPUT_POINT && output_format == OUTPUT_KML)
  {
    printf("Converting point file to a kml file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_kml(in_file, out_base, POINT, 0);
  }
  else if (input_format == INPUT_POLYGON && output_format == OUTPUT_SHAPE)
  {
    printf("Converting polygon file to a shape file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    polygon2shape_new(in_file, out_base);
  }
  else if (input_format == INPUT_POLYGON && output_format == OUTPUT_KML)
  {
    printf("Converting polygon file to a kml file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_kml(in_file, out_base, POLYGON, 0);
  }
  else if (input_format == INPUT_SHAPE && output_format == OUTPUT_KML)
  {
    printf("Converting shape file to a kml file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    ret = read_shape(in_base, out_base, KMLFILE, 0);
    // read_shape returns 0 for success
    ret = !ret;
  }
  else if (input_format == INPUT_SHAPE && output_format == OUTPUT_TEXT)
  {
    printf("Converting shape file to a text file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    read_shape(in_base, out_base, TEXT, 0);
  }
  else if (input_format == INPUT_GEOTIFF && output_format == OUTPUT_SHAPE)
  {
    printf("Converting geotiff file to a shape file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_shape(in_file, out_base, GEOTIFF_META, 0);
  }
  else if (input_format == INPUT_GEOTIFF && output_format == OUTPUT_KML)
  {
    printf("Converting geotiff file to a kml file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_kml(in_file, out_base, GEOTIFF_META, 0);
  }
  else if (input_format == INPUT_GEOTIFF && output_format == OUTPUT_TEXT)
  {
    printf("Converting geotiff file to a text file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    write_text(in_file, out_base, GEOTIFF_META, 0);
  }
  else if (input_format == INPUT_GENERIC_CSV && output_format == OUTPUT_SHAPE)
  {
    printf("Converting generic CSV file to a shape file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    ret=csv2shape(in_file, out_base);
  }
  else if (input_format == INPUT_GENERIC_CSV && output_format == OUTPUT_KML)
  {
    printf("Converting generic CSV file to a kml file.\n");
    printf("File %d: %s -> %s\n", 1, in_file, out_file);
    ret=csv2kml(in_file, out_file);
  }
  //else if (input_format == INPUT_RGPS && output_format == OUTPUT_SHAPE)
  //{
  //  printf("Converting list of RGPS cells to a shape file.\n");
  //  printf("File %d: %s -> %s\n", 1, in_file, out_file);
  //  write_shape(in_file, out_base, RGPS, 0);
  //}
  //else if (input_format == INPUT_RGPS && output_format == OUTPUT_KML)
  //{
  //  printf("Converting list of RGPS cells to a kml file.\n");
  //  printf("File %d: %s -> %s\n", 1, in_file, out_file);
  //  write_kml(in_file, out_base, RGPS, 0);
  //}
  else
  {
    put_string_to_label("result_label",
      "Unsupported combination of Input and Output formats selected.");
    FREE(in_base);
    FREE(out_base);
    return;
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

  FREE(in_base);
  FREE(out_base);
}
