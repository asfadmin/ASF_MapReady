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
"asf_export"

#define ASF_USAGE_STRING \
"[-format <output_format>] [-size <max_dimension>] [-lut <leader file>\n"\
"<cal params file> <cal comment> ] [-byte <scale option> ]\n"\
"<in_base_name> <out_full_name>\n"\
"Additional options: -help, -log <log_file>, -quiet"

#define ASF_DESCRIPTION_STRING \
"This program ingests ASF internal format data and exports said data to a number of output formats. If the input data was geocoded and the ouput format supports geocoding, that information will be included."

#define ASF_INPUT_STRING \
"This must be an ASF internal format data file."

#define ASF_OUTPUT_STRING \
"The converted data in the output file."

#define ASF_OPTIONS_STRING \
"-format <format>  Format to export to. Must be one of the following:\n"\
"                     CEOS, envi, esri, tiff, geotiff, jpeg, ppm\n"\
"-size <size>      Scale image so that its largest dimension is, at\n"\
"		  most, size.\n"\
"-lut <leader file> <cal params file> <cal comment>\n"\
"		  Updates the original leader file with the\n"\
"		  calibration parameter file and the calibration\n"\
"		  comment. Exports image into CEOS format.\n"\
"-byte <scale option>\n"\
"                  Converts output image to byte using the following\n"\
"		  options:\n"\
"		  truncate - truncates the input values regardless of\n"\
"		             their value range.\n"\
"	          minmax   - determines the minimum and maximum values\n"\
"	                     of the input image and maps those values\n"\
"			     to the byte range of 0 to 255.\n"\
"                  sigma    - determines the mean and standard\n"\
"		             deviation of an image and applies a\n"\
"			     buffer of 2 sigma around the mean value\n"\
"                             (it adjusts this buffer if the 2 sigma\n"\
"			     buffer is outside the value range).\n"\
""

#define ASF_EXAMPLES_STRING \
"To export to the default geotiff format from file1.img and file1.meta\n"\
"to file1.jpg:\n"\
"   asf_export file1 file1\n"\
"To export to file2.jpg in the jpeg format:\n"\
"   asf_export -format jpeg file1 file2\n"\
"To export file1 to a jpeg called file3.jpg no larger than 800x800:\n"\
"   asf_export -format jpeg -size 800 file1 file3"

#define ASF_LIMITATIONS_STRING \
"Currently only supports ingest of ASF format floating point data.\n"\
"GeoTIFF images will not be scaled."

#define ASF_SEE_ALSO_STRING \
"asf_convert, asf_import"

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
"No history."

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/


#include <assert.h>
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



void print_splash_screen(int argc, char* argv[])
{
	char temp1[255];
	char temp2[255];
	int ii;
	struct tm *ptr;
	time_t tm;

	sprintf(temp1, "\nCommand line:");
	for (ii = 0; ii < argc; ii++)
	{
		sprintf(temp2, " %s",argv[ii]);
		strcat(temp1, temp2);
	}
	strcat(temp1, "\n");
	printf("%s", temp1);
	printLog(temp1);

	tm = time(NULL);
	ptr = localtime(&tm);
	printf(asctime(ptr));
	printLog(asctime(ptr));

	/*system("date");*/
	printf("PID: %i\n", (int)getpid());
}

int
checkForOption (char *key, int argc, char *argv[])
{
  int ii = 0;
  while ( ii < argc ) {
    if ( strmatch(key, argv[ii]) )
      return ii;
    ++ii;
  }
  return FLAG_NOT_SET;
}

/* Print an error message. This is just here for circumventing
   check_return.  Also, it makes it possible to reformat all the error
   messages at once.  */
void
print_error (char *msg)
{
	char tmp[255];
	/* I made "ERROR:" red...Yay! :D */
	sprintf (tmp, "\n   \033[31;1mERROR:\033[0m %s\n\n", msg);
	printErr (tmp);
	sprintf(tmp, "\n   ERROR: %s\n\n", msg);
	printLog(tmp);
	exit (EXIT_FAILURE);
}

/* Check the return value of a function and display an error message
   if it's a bad return.*/
void
check_return (int ret, char *msg)
{
  if ( ret != 0 )
    print_error (msg);
}


void help_page()
{
	if(system("echo '"
		"\n\n\n"
		"Tool name: " ASF_NAME_STRING "\n\n\n"
		"Usage: " ASF_USAGE_STRING "\n\n\n"
		"Description: " ASF_DESCRIPTION_STRING "\n\n\n"
		"Input: " ASF_INPUT_STRING "\n\n\n"
		"Output: "ASF_OUTPUT_STRING "\n\n\n"
		"Options: " ASF_OPTIONS_STRING "\n\n\n"
		"Examples: " ASF_EXAMPLES_STRING "\n\n\n"
		"Limitations: " ASF_LIMITATIONS_STRING "\n\n\n"
		"See also: " ASF_SEE_ALSO_STRING "\n\n\n"
		"Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
		"Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
		"' | less") != -1)
		exit(EXIT_SUCCESS);

	else if(system("echo '"
		"\n\n\n"
		"Tool name: " ASF_NAME_STRING "\n\n\n"
		"Usage: " ASF_USAGE_STRING "\n\n\n"
		"Description: " ASF_DESCRIPTION_STRING "\n\n\n"
		"Input: " ASF_INPUT_STRING "\n\n\n"
		"Output: "ASF_OUTPUT_STRING "\n\n\n"
		"Options: " ASF_OPTIONS_STRING "\n\n\n"
		"Examples: " ASF_EXAMPLES_STRING "\n\n\n"
		"Limitations: " ASF_LIMITATIONS_STRING "\n\n\n"
		"See also: " ASF_SEE_ALSO_STRING "\n\n\n"
		"Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
		"Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
		"' | more") != -1)
		exit(EXIT_SUCCESS);

	else
		printf("\n\n\n"
		"Tool name: " ASF_NAME_STRING "\n\n\n"
		"Usage: " ASF_USAGE_STRING "\n\n\n"
		"Description: " ASF_DESCRIPTION_STRING "\n\n\n"
		"Input: " ASF_INPUT_STRING "\n\n\n"
		"Output: "ASF_OUTPUT_STRING "\n\n\n"
		"Options: " ASF_OPTIONS_STRING "\n\n\n"
		"Examples: " ASF_EXAMPLES_STRING "\n\n\n"
		"Limitations: " ASF_LIMITATIONS_STRING "\n\n\n"
		"See also: " ASF_SEE_ALSO_STRING "\n\n\n"
		"Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
		"Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n");
		exit(EXIT_SUCCESS);
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
	char scaleStr[25];

	/*Check to see which options were specified*/
	if(checkForOption("-help", argc, argv) != -1)/*Most important*/
		help_page();
	formatFlag = checkForOption("-format", argc, argv);
	sizeFlag = checkForOption("-size", argc, argv);
	logFlag = checkForOption("-log", argc, argv);
	quietFlag = checkForOption("-quiet", argc, argv);
	lutFlag = checkForOption("-lut", argc, argv);
	byteFlag = checkForOption("-byte", argc, argv);

	if(formatFlag != FLAG_NOT_SET)
		needed_args += 2;/*option & parameter*/
	if(sizeFlag != FLAG_NOT_SET)
		needed_args += 2;/*option & parameter*/
	if(quietFlag != FLAG_NOT_SET)
		needed_args += 1;/*option*/
	if(logFlag != FLAG_NOT_SET)
		needed_args += 2;/*option & parameter*/
	if(lutFlag != FLAG_NOT_SET)
	  needed_args += 4;/*option & parameters */
	if(byteFlag != FLAG_NOT_SET)
	  needed_args += 2;/*option & parameter*/

	if(argc != needed_args)
		usage();/*This exits with a failure*/

	/*We also need to make sure the last three options are close to what we expect*/
	if(argv[argc - 1][0] == '-' || argv[argc - 2][0] == '-')
		usage();/*This exits with a failure*/

	/*Make sure any options that have parameters are followed by parameters (and not other options)
	Also make sure options' parameters don't bleed into required arguments*/
	if(formatFlag != FLAG_NOT_SET)
		if(argv[formatFlag + 1][0] == '-' || formatFlag >= argc - 3)
			usage();
	if(sizeFlag != FLAG_NOT_SET)
		if(argv[sizeFlag + 1][0] == '-' || sizeFlag >= argc - 3)
			usage();
	if(lutFlag != FLAG_NOT_SET)
	  if(argv[lutFlag + 1][0] == '-' || argv[lutFlag + 2][0] == '-' ||
	     argv[lutFlag + 3][0] == '-' || lutFlag >= argc - 5)
	    usage();
	if(byteFlag != FLAG_NOT_SET)
	  if(argv[byteFlag + 1][0] == '-' || byteFlag >= argc -3)
	    usage();
	if(logFlag != FLAG_NOT_SET)
	  if(argv[logFlag + 1][0] == '-' || logFlag >= argc - 3)
	    usage();

	if (logFlag != FLAG_NOT_SET)
	  strcpy(logFile, argv[logFlag + 1]);
	else
	  sprintf(logFile, "tmp%i.log", (int)getpid());
	fLog = FOPEN(logFile, "a");

	/* We're good enough at this point... print the splash screen
	   and start filling in whatever needs to be filled in.  */
	if ( quietFlag == FLAG_NOT_SET )
		print_splash_screen(argc, argv);

	if(formatFlag != FLAG_NOT_SET)
		strcpy(command_line.format, argv[formatFlag + 1]);
	else
		strcpy(command_line.format, "geotiff");/*Default behavior: produce a geotiff*/

	for(ii = 0; ii < strlen(command_line.format); ++ii)/*convert the string to upper case*/
		command_line.format[ii] = toupper(command_line.format[ii]);

	/* Set the default byte scaling mechanisms */
	if (strcmp(command_line.format, "TIFF") == 0 ||
	    strcmp(command_line.format, "JPEG") == 0) 
	  command_line.scale = SIGMA;
	if (strcmp(command_line.format, "GEOTIFF") == 0)
	  command_line.scale = NONE;
	    
	if(sizeFlag != FLAG_NOT_SET)
		command_line.size = atol(argv[sizeFlag + 1]);
	else
		command_line.size = NO_MAXIMUM_OUTPUT_SIZE;

	if(quietFlag != FLAG_NOT_SET)
		command_line.quiet = TRUE;
	else
	        command_line.quiet = FALSE;

        if(lutFlag != FLAG_NOT_SET) {
          strcpy(command_line.format, "CEOS");
          strcpy(command_line.leader_name, argv[lutFlag + 1]);
          strcpy(command_line.cal_params_file, argv[lutFlag + 2]);
          strcpy(command_line.cal_comment, argv[lutFlag + 3]);
        }
	if(byteFlag != FLAG_NOT_SET) {
	  strcpy(scaleStr, argv[byteFlag + 1]);
	  for(ii=0; ii<strlen(scaleStr); ii++)
	    scaleStr[ii] = toupper(scaleStr[ii]);

	  /* Set scaling mechanism */
	  if(strcmp(scaleStr, "TRUNCATE") == 0)
	    command_line.scale = TRUNCATE;
	  else if(strcmp(scaleStr, "MINMAX") == 0)
	    command_line.scale = MINMAX;
	  else if(strcmp(scaleStr, "SIGMA") == 0)
	    command_line.scale = SIGMA;
	}

	/*Grab/construct the data file name*/
	strcpy(command_line.in_data_name, argv[argc - 2]);
	strcat(command_line.in_data_name, ".img");
	/*Grab/construct the meta file name*/
	strcpy(command_line.in_meta_name, argv[argc - 2]);
	strcat(command_line.in_meta_name, ".meta");
	/*Grab the output name*/
	strcpy(command_line.output_name, argv[argc - 1]);

/***********************END COMMAND LINE PARSING STUFF***********************/

  if ( strcmp (command_line.format, "ENVI") == 0 ) {
    format = ENVI;
  }
  else if ( strcmp (command_line.format, "ESRI") == 0 ) {
    format = ESRI;
  }
  else if ( strcmp (command_line.format, "GEOTIFF") == 0 ||
    strcmp(command_line.format, "GEOTIF") == 0) {
    append_ext_if_needed(command_line.output_name, ".tif", ".tiff");
    format = GEOTIFF;
  }
  else if ( strcmp (command_line.format, "TIFF") == 0 ||
	    strcmp(command_line.format, "TIF") == 0) {
    append_ext_if_needed(command_line.output_name, ".tif", ".tiff");
    format = TIF;
  }
  else if ( strcmp (command_line.format, "JPEG") == 0 ||
    strcmp(command_line.format, "JPG") == 0) {
    append_ext_if_needed(command_line.output_name, ".jpg", ".jpeg");
    format = JPEG;
  }
  else if ( strcmp (command_line.format, "PPM") == 0 ) {
    append_ext_if_needed(command_line.output_name, ".ppm", NULL);
    format = PPM;
  }
  else if ( strcmp (command_line.format, "CEOS") == 0 ) {
    format = CEOS;
  }
  else {
    print_error("Unrecognized output format specified");
  }

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  md = meta_read (command_line.in_meta_name);
  assert (md->general->data_type == BYTE
  	  || md->general->data_type == INTEGER16
  	  || md->general->data_type == INTEGER32
  	  || md->general->data_type == REAL32
  	  || md->general->data_type == REAL64);
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
		    command_line.scale);
  }
  else if ( format == GEOTIFF ) {
    export_as_geotiff (command_line.in_meta_name, command_line.in_data_name,
		       command_line.output_name, command_line.scale);
  }
  else if ( format == JPEG ) {
    export_as_jpeg (command_line.in_meta_name, command_line.in_data_name,
		    command_line.output_name, command_line.size,
		    command_line.scale);
  }
  else if ( format == PPM ) {
    export_as_ppm (command_line.in_meta_name, command_line.in_data_name,
		   command_line.output_name, command_line.size);
  }
  else if ( format == CEOS ) {
    export_as_ceos (command_line.in_meta_name, command_line.in_data_name,
                    command_line.output_name, command_line.leader_name,
                    command_line.cal_params_file, command_line.cal_comment);
  }

  exit (EXIT_SUCCESS);
}

