/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"   asf_export"

#define ASF_USAGE_STRING \
"[-format <output_format>] [-size <max_dimension>]\n"\
"              [-lut <leader file> <cal params file> <cal comment>]\n"\
"              [-byte <sample mapping option> ] [-log <log_file>] [-quiet]\n"\
"              [-help] <in_base_name> <out_full_name>\n"

#define ASF_DESCRIPTION_STRING \
"   This program ingests ASF internal format data and exports said data to a\n"\
"   number of output formats. If the input data was geocoded and the ouput\n"\
"   format supports geocoding, that information will be included."

#define ASF_INPUT_STRING \
"   This must be an ASF internal format data file."

#define ASF_OUTPUT_STRING \
"   The converted data in the output file."

#define ASF_OPTIONS_STRING \
"\n"\
"   -format <format>\n"\
"        Format to export to. Must be one of the following:\n"\
"            CEOS    - Committee for Earth Observing Systems format\n"\
"	    tiff    - Tagged Image File Format, with byte valued pixels\n"\
"	    geotiff - GeoTIFF file, with floating point valued pixels\n"\
"	    jpeg    - Lossy compressed image, with byte valued pixels\n"\
"            ppm     - portable pixmap image, with byte valued pixels\n"\
"	\n"\
"   -size <size>\n"\
"        Scale image so that its largest dimension is, at most, size.\n"\
"\n"\
"   -lut <leader file> <cal params file> <cal comment>\n"\
"        Updates the original leader file with the calibration parameter\n"\
"        file and the calibration comment. Exports image into CEOS format.\n"\
"\n"\
"   -byte <sample mapping option>\n"\
"        Converts output image to byte using the following options:\n"\
"             truncate - truncates the input values regardless of their\n"\
"                        value range.\n"\
"             minmax   - determines the minimum and maximum values of the\n"\
"                        input image and maps those values to the byte range\n"\
"                        of 0 to 255.\n"\
"             sigma    - determines the mean and standard deviation of an\n"\
"                        image and applies a buffer of 2 sigma around the\n"\
"                        mean value (it adjusts this buffer if the 2 sigma\n"\
"                        buffer is outside the value range)."

#define ASF_EXAMPLES_STRING \
"   To export to the default geotiff format from file1.img and file1.meta\n"\
"   to file1.jpg:\n"\
"\n"\
"        example> asf_export file1 file1\n"\
"\n"\
"   To export to file2.jpg in the jpeg format:\n"\
"\n"\
"        example> asf_export -format jpeg file1 file2\n"\
"\n"\
"   To export file1 to a jpeg called file3.jpg no larger than 800x800:\n"\
"\n"\
"        example> asf_export -format jpeg -size 800 file1 file3"

#define ASF_LIMITATIONS_STRING \
"   Currently only supports ingest of ASF format floating point data.\\n\n"\
"   Floating-point image formats (i.e., geotiff) are not generally\n"\
"   supported in many image viewing programs."

#define ASF_SEE_ALSO_STRING \
"   asf_convert, asf_import"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"   No history."

#define ASF_VERSION_MAJOR_STRING \
"0.30"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/


#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include <cla.h>
#include <envi.h>
#include <esri.h>
#include <geokeys.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_statistics.h>
#include <jpeglib.h>
/*#include <proj_api.h>*/
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>
#include <asf_reporting.h>


/* Print invocation information.  */
void usage()
{
  printf ("\n"
    "USAGE:\n"
    ASF_NAME_STRING
    " "
    ASF_USAGE_STRING
    "\n\n");
  exit (EXIT_FAILURE);
}


void help_page()
{
  char happy_string[4066];
  char command[4096];

  /* What to print out for help */
  sprintf(happy_string,
          "\n\n\n"
          "Tool name:\n" ASF_NAME_STRING "\n\n\n"
          "Usage:\n" ASF_NAME_STRING " " ASF_USAGE_STRING "\n\n\n"
          "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
          "Input:\n" ASF_INPUT_STRING "\n\n\n"
          "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
          "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
          "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
          "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
          "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
          "Version:\n" CONVERT_PACKAGE_VERSION_STRING "\n\n\n"
          "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n");

  /* If we can, use less */
  sprintf (command, "echo '%s' | less --prompt='Type q to quit help, h for "
	   "help with help browser'", happy_string);
  if ( system (command) == 0 )
    exit (EXIT_SUCCESS);

  /* Hmmm, less didn't work cause we got here, try using more */
  sprintf (command,"echo '%s' | more",happy_string);
  if ( system (command) == 0 )
    exit (EXIT_SUCCESS);

  /* Okay, neither less or more work (obviously if we made it here),
   * just print the info straight to stdout and exit */
  printf (happy_string);
  exit (EXIT_SUCCESS);
}


int
checkForOption (char *key, int argc, char *argv[])
{
  int ii = 0;
  while ( ii < argc ) {
    if ( strmatch (key, argv[ii]) )
      return ii;
    ++ii;
  }
  return FLAG_NOT_SET;
}


/* Check the return value of a function and display an error message
   if it's a bad return.*/
void
check_return (int ret, char *msg)
{
  if ( ret != 0 )
    asfPrintError (msg);
}


/* Main program body. */
int
main (int argc, char *argv[])
{

  output_format_t format;
  meta_parameters *md;

/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  /* Command line input goes in it's own structure.  */
  command_line_parameters_t command_line;

  int formatFlag, sizeFlag, logFlag, quietFlag, lutFlag, byteFlag;
  int needed_args = 3;/*command & argument & argument*/
  int ii;
  char sample_mapping_string[25];

  /*Check to see which options were specified*/
  if ( checkForOption ("-help", argc, argv) != -1
       || checkForOption ("--help", argc, argv) != -1 ) {
    help_page ();
  }
  formatFlag = checkForOption ("-format", argc, argv);
  sizeFlag = checkForOption ("-size", argc, argv);
  logFlag = checkForOption ("-log", argc, argv);
  quietFlag = checkForOption ("-quiet", argc, argv);
  lutFlag = checkForOption ("-lut", argc, argv);
  byteFlag = checkForOption ("-byte", argc, argv);

  if ( formatFlag != FLAG_NOT_SET ) { 
    needed_args += 2;		/* Option & parameter.  */ 
  }
  if ( sizeFlag != FLAG_NOT_SET ) {
    needed_args += 2;		/* Option & parameter.  */
  }
  if ( quietFlag != FLAG_NOT_SET ) {
    needed_args += 1;		/* Option & parameter.  */
  }
  if ( logFlag != FLAG_NOT_SET ) {
    needed_args += 2;		/* Option & parameter.  */
  }
  if ( lutFlag != FLAG_NOT_SET ) {
    needed_args += 4;		/* Option & parameters.  */
  }
  if ( byteFlag != FLAG_NOT_SET ) {
    needed_args += 2;		/* Option & parameter.  */
  }

  if ( argc != needed_args ) {
    usage ();			/* This exits with a failure.  */
  }

  /* We also need to make sure the last three options are close to
     what we expect.  */
  if ( argv[argc - 1][0] == '-' || argv[argc - 2][0] == '-' ) {
    usage (); /* This exits with a failure.  */
  }

  /* Make sure any options that have parameters are followed by
     parameters (and not other options) Also make sure options'
     parameters don't bleed into required arguments.  */
  if ( formatFlag != FLAG_NOT_SET ) {
    if ( argv[formatFlag + 1][0] == '-' || formatFlag >= argc - 3 ) {
      usage ();
    }
  }
  if ( sizeFlag != FLAG_NOT_SET ) {
    if ( argv[sizeFlag + 1][0] == '-' || sizeFlag >= argc - 3 ) {
      usage ();
    }
  }
  if ( lutFlag != FLAG_NOT_SET ) {
    if ( argv[lutFlag + 1][0] == '-' || argv[lutFlag + 2][0] == '-' ||
	 argv[lutFlag + 3][0] == '-' || lutFlag >= argc - 5 ) {
      usage ();
    }
  }
  if ( byteFlag != FLAG_NOT_SET ) {
    if ( argv[byteFlag + 1][0] == '-' || byteFlag >= argc -3 ) {
      usage ();
    }
  }
  if ( logFlag != FLAG_NOT_SET ) {
    if ( argv[logFlag + 1][0] == '-' || logFlag >= argc - 3 ) {
      usage ();
    }
  }

  if ( logFlag != FLAG_NOT_SET ) {
    strcpy (logFile, argv[logFlag + 1]);
    fLog = fopen (logFile, "a");
    if ( fLog == NULL ) {
      logflag = FALSE;
    }
    else {
      logflag = TRUE;
    }
  }
  else {
    logflag = FALSE;
    //    sprintf (logFile, "tmp%i.log", (int) getpid ());
  }

  /* Set old school quiet flag (for use in our libraries) */
  quietflag = ( quietFlag != FLAG_NOT_SET ) ? TRUE : FALSE;

  /* We're good enough at this point... print the splash screen.  */
  asfSplashScreen (argc, argv);

  if( formatFlag != FLAG_NOT_SET ) {
    strcpy (command_line.format, argv[formatFlag + 1]);
  }
  else {
    /* Default behavior: produce a geotiff.  */
    strcpy (command_line.format, "geotiff");
  }

  /* Convert the string to upper case.  */
  for ( ii = 0 ; ii < strlen (command_line.format) ; ++ii ) {
    command_line.format[ii] = toupper (command_line.format[ii]);
  }

  /* Set the default byte scaling mechanisms */
  if ( strcmp (command_line.format, "TIFF") == 0 
       || strcmp (command_line.format, "TIF") == 0
       || strcmp (command_line.format, "JPEG") == 0)
    command_line.sample_mapping = SIGMA;
  if ( strcmp (command_line.format, "GEOTIFF") == 0 )
    command_line.sample_mapping = NONE;

  if ( sizeFlag != FLAG_NOT_SET )
    command_line.size = atol (argv[sizeFlag + 1]);
  else
    command_line.size = NO_MAXIMUM_OUTPUT_SIZE;

  if ( quietFlag != FLAG_NOT_SET )
    command_line.quiet = TRUE;
  else
    command_line.quiet = FALSE;

  if ( lutFlag != FLAG_NOT_SET ) {
    strcpy(command_line.format, "CEOS");
    strcpy(command_line.leader_name, argv[lutFlag + 1]);
    strcpy(command_line.cal_params_file, argv[lutFlag + 2]);
    strcpy(command_line.cal_comment, argv[lutFlag + 3]);
  }
  if ( byteFlag != FLAG_NOT_SET ) {
    strcpy (sample_mapping_string, argv[byteFlag + 1]);
    for ( ii = 0 ; ii < strlen (sample_mapping_string) ; ii++)
      sample_mapping_string[ii] = toupper (sample_mapping_string[ii]);

    /* Set scaling mechanism */
    if ( strcmp (sample_mapping_string, "TRUNCATE") == 0 )
      command_line.sample_mapping = TRUNCATE;
    else if ( strcmp(sample_mapping_string, "MINMAX") == 0 )
      command_line.sample_mapping = MINMAX;
    else if ( strcmp(sample_mapping_string, "SIGMA") == 0 )
      command_line.sample_mapping = SIGMA;
  }

  /*Grab/construct the data file name*/
  strcpy (command_line.in_data_name, argv[argc - 2]);
  strcat (command_line.in_data_name, ".img");
  /*Grab/construct the meta file name*/
  strcpy (command_line.in_meta_name, argv[argc - 2]);
  strcat (command_line.in_meta_name, ".meta");
  /*Grab the output name*/
  strcpy (command_line.output_name, argv[argc - 1]);


/***********************END COMMAND LINE PARSING STUFF***********************/

  if ( strcmp (command_line.format, "ENVI") == 0 ) {
    format = ENVI;
  }
  else if ( strcmp (command_line.format, "ESRI") == 0 ) {
    format = ESRI;
  }
  else if ( strcmp (command_line.format, "GEOTIFF") == 0 ||
    strcmp (command_line.format, "GEOTIF") == 0) {
    append_ext_if_needed (command_line.output_name, ".tif", ".tiff");
    format = GEOTIFF;
  }
  else if ( strcmp (command_line.format, "TIFF") == 0 ||
	    strcmp (command_line.format, "TIF") == 0) {
    append_ext_if_needed (command_line.output_name, ".tif", ".tiff");
    format = TIF;
  }
  else if ( strcmp (command_line.format, "JPEG") == 0 ||
    strcmp (command_line.format, "JPG") == 0) {
    append_ext_if_needed (command_line.output_name, ".jpg", ".jpeg");
    format = JPEG;
  }
  else if ( strcmp (command_line.format, "PPM") == 0 ) {
    append_ext_if_needed (command_line.output_name, ".ppm", NULL);
    format = PPM;
  }
  else if ( strcmp (command_line.format, "CEOS") == 0 ) {
    format = CEOS;
  }
  else {
    asfPrintError("Unrecognized output format specified");
  }

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  md = meta_read (command_line.in_meta_name);
  asfRequire (   md->general->data_type == BYTE
              || md->general->data_type == INTEGER16
              || md->general->data_type == INTEGER32
              || md->general->data_type == REAL32
              || md->general->data_type == REAL64,
              "Cannot cope with complex data, exiting...\n");
  meta_free (md);
  if ( format == ENVI ) {
    export_as_envi (command_line.in_meta_name, command_line.in_data_name,
		    command_line.output_name);
  }
  else if ( format == ESRI ) {
    export_as_esri (command_line.in_meta_name, command_line.in_data_name,
		    command_line.output_name);
  }
  else if ( format == TIF ) {
    export_as_tiff (command_line.in_meta_name, command_line.in_data_name,
		    command_line.output_name, command_line.size,
		    command_line.sample_mapping);
  }
  else if ( format == GEOTIFF ) {
    export_as_geotiff (command_line.in_meta_name, command_line.in_data_name,
		       command_line.output_name, command_line.size,
		       command_line.sample_mapping);
  }
  else if ( format == JPEG ) {
    export_as_jpeg (command_line.in_meta_name, command_line.in_data_name,
		    command_line.output_name, command_line.size,
		    command_line.sample_mapping);
  }
  else if ( format == PPM ) {
    export_as_ppm (command_line.in_meta_name, command_line.in_data_name,
		   command_line.output_name, command_line.size,
		   command_line.sample_mapping);
  }
  else if ( format == CEOS ) {
    export_as_ceos (command_line.in_meta_name, command_line.in_data_name,
                    command_line.output_name, command_line.leader_name,
                    command_line.cal_params_file, command_line.cal_comment);
  }

  exit (EXIT_SUCCESS);
}

