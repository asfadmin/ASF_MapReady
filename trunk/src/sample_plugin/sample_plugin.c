#include "asf_meta.h"

//
// This is a very simple program that illustrates how a person might write
// a "plugin" for the MapReady GUI.  (I.e., an item in the dropdown list on
// the "External" tab.)
//
// Hopefully there is enough here to serve as a starting point for writing
// your own tools that can be used by MapReady.  All plugins run after
// import, but before any additional processing starts (such as terrain
// correction or geocoding).
//
// This program just adds a given offset to all pixels in an image, producing
// an output image.  To add this to the GUI, you would need to modify the
// "plugins.cfg" file in the MapReady directory (asf_convert_gui).  Add
// lines like the following:
//
// Name=Sample
// Command=sample_plugin -log {Log} -offset $P1 {Input} {Output}
// Comment=Adds the given offset to all pixels in the image.
// P1=double,required,"%f","Offset"
//
// This sample actually has the offset as optional, however there isn't
// much point in that (adds 0 to all pixels, leaving the output exactly
// the same as the input), so we can mark the offset value as required. 
//


void usage()
{
  printf("sample_plugin [-offset <offset>] <input_file> <output_file>\n");
  printf("\nSample code for writing plugins.  This sample adds the given\n"
         "offset to all pixels in the image.  Default: 0 (which reduces\n"
         "to an image copy).\n");
  exit(EXIT_SUCCESS);
}

int main(int argc, char *argv[])
{
  // This function will process the options "-log" "-quiet".  They'll
  // be removed from the command line, if they are there.
  handle_common_asf_args(&argc, &argv, "sample_plugin");

  // Using asfPrintStatus instead of printf will hook up both the log
  // and quiet options -- this won't be printed if the user specified
  // -quiet, and will be put into the log file if a log was given.
  asfPrintStatus("Welcome to the sample_plugin!\n");

  // Other command-line options can be parsed using the utility
  // routines.  If an argument is found, it is removed.
  if (detect_flag_options(argc, argv, "-help", "--help", "-h"))
    usage();
  double offset=0;
  extract_double_options(&argc, &argv, &offset, "-offset", "-o", NULL);
  asfPrintStatus("Using offset: %f\n", offset);

  // Assuming all other command-line args are removed, we should just
  // be left with input and output files
  if (argc < 2) {
    asfPrintStatus("*** Not enough arguments\n\n");
    usage();
  }
  char *in = argv[1];
  char *out = argv[2];

  // User can pass in the basename (no extension), though if an
  // extension is present, it will be removed by appendExt().
  char *input_img_file = appendExt(in, ".img");
  char *input_meta_file = appendExt(in, ".meta");
  char *output_img_file = appendExt(out, ".img");
  char *output_meta_file = appendExt(out, ".meta");

  // Reading the metadata.  See asf_meta.h for what is in the metadata
  // structure, or have a look at a .meta file, it is plain ascii text.
  meta_parameters *meta = meta_read(input_meta_file);

  // FOPEN() is just like fopen(), except it aborts if the file is not
  // found (when reading), or otherwise couldn't be opened.
  FILE *in_fp = FOPEN(input_img_file, "rb");
  FILE *out_fp = FOPEN(output_img_file, "wb");

  // In this example, we'll just go through the input file line by line
  // and add the offset value.

  // allocate a place to put the read-in values -- one line's worth
  // MALLOC() is just like malloc() except it aborts if memory could
  // not be allocated.  We create an array of floats, even though we
  // may not be reading float-point data... see get_float_line(), below.
  float *buf = MALLOC(sizeof(float) * meta->general->sample_count);

  int i,j;
  for (i=0; i<meta->general->line_count; ++i) {
    // get_float_line() reads in a line of data from the file and populates
    // the data array.  The "float" refers to the kind of array you must pass,
    // not the data in the file -- if the data is BYTE data (as specified by
    // the metadata that is passed in), it'll be read as bytes and then put
    // into the floating-point array.  So, for any data type supported by
    // the ASF tools, you'll end up with floating points number after they
    // are read in.
    get_float_line(in_fp, meta, i, buf);

    // for this sample program we just add the offset to each pixel
    for (j=0; j<meta->general->sample_count; ++j)
      buf[j] += offset;

    // put_float_line() is similar to get_float_line() in that it does not
    // necessarily write floating point, the float refers to the type of
    // array that you pass in.  The type of the data written out is determined
    // from meta->general->data_type.  We're using the same metadata for the
    // input and output -- so the same data type is written as was read.
    put_float_line(out_fp, meta, i, buf);

    // asfLineMeter is the code the prints "Processed x of y lines"
    // every so often.  -quiet will turn it off.
    asfLineMeter(i,meta->general->line_count);
  }

  // In this simple example the output metadata is the same as the input
  // so we just write it out without changing it.
  meta_write(meta, output_meta_file);
  
  // That's the end of this example!  Cleanup is all the remains.
  FCLOSE(in_fp);
  FCLOSE(out_fp);
  meta_free(meta);
  free(input_img_file);
  free(input_meta_file);
  free(output_img_file);
  free(output_meta_file);
  free(buf);

  asfPrintStatus("Done!\n");
  return EXIT_SUCCESS;
}
