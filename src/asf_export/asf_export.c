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
"[-format <output_format>] [-size <max_dimension>] <in_base_name> <out_full_name>\n"\
"Additional options: -help, -log <log_file>, -quiet"

#define ASF_DESCRIPTION_STRING \
"This program ingests ASF internal format data and exports said data to a number of output formats. If the input data was geocoded and the ouput format supports geocoding, that information will be included."

#define ASF_INPUT_STRING \
"This must be an ASF internal format data file."

#define ASF_OUTPUT_STRING \
"The converted data in the output file."

#define ASF_OPTIONS_STRING \
"-format <format>  Format to export to. Must be one of the following:\n"\
"                     CEOS, envi, esri, geotiff, jpeg, ppm\n"\
"-size <size>      Scale image so that its largest dimension is, at most, size."

#define ASF_EXAMPLES_STRING \
"To export to the default geotiff format from file1:\n"\
"   asf_export file1\n"\
"To export file1 to the jpeg format:\n"\
"   asf_export -format jpeg file1\n"\
"To export file1 to a jpeg no larger than 800x800:\n"\
"   asf_export -format jpeg -size 800 file1"

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


void help_page();


void print_splash_screen(int argc, char* argv[])
{
	char temp1[255];
	char temp2[255];
	int ii;
	sprintf(temp1, "\nCommand line:");
	for (ii = 0; ii < argc; ii++)
	{
		sprintf(temp2, " %s",argv[ii]);
		strcat(temp1, temp2);
	}
	strcat(temp1, "\n");
	printf("%s", temp1);
	printLog(temp1);
	system("date");
	printf("PID: %i\n", (int)getpid());
}

/* Evaluate to true if floats are within tolerance of each other.  */
#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)

/* Default tolerance used for many floating point comparisons in this
   program.  */
#define ASF_EXPORT_FLOAT_MICRON 0.000000001

/* Compare floats using the default tolerance for this program.  */
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

/* Maximum image name length we can accept from the user.  Since the
   user may enter a base name only, the actual file name strings need
   to be slightly longer, to accomodate extensions.  */
#define MAX_IMAGE_NAME_LENGTH 240
/* Maximum extension length that will ever be automaticly added to a
   file name specified by the user.  */
#define MAX_EXTENSION_LENGTH 10
/* Maximum length allowed for arguments to format (-f or --format)
   options.  */
#define MAX_FORMAT_STRING_LENGTH 50
/* This value signifies that there is no maximum size is some contexts.  */
#define NO_MAXIMUM_OUTPUT_SIZE -1

#define FLAG_NOT_SET -1

/* Structure to hold elements of the command line.  */
typedef struct {
  /* Output format to use.  */
  char format[MAX_FORMAT_STRING_LENGTH];
  /* Maximum size of largest output dimension, in pixels, or
     NO_MAXIMUM_OUTPUT_SIZE.  The output image will be scaled to honor
     this value, if its maximum dimension is not already less than
     this.  */
  long size;
  /* Name of data and metadata input files  */
  char in_data_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char in_meta_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  /* Output name to use.  */
  char output_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  int verbose;			/* Flag true if in verbose mode.  */
  int quiet;			/* True if quiet mode is active.  */
} command_line_parameters_t;

/* Output format to use.  */
typedef enum {
  ENVI,				/* ENVI software package.  */
  ESRI,				/* ESRI GIS package.  */
  GEOTIFF,			/* Geotiff.  */
  JPEG,				/* Joint Photographic Experts Group.  */
  PPM				/* Portable PixMap.  */
} output_format_t;

/* Ellipsoid used for the data.  */
typedef enum {
  CLARKE1866,
  GEM10C,			/* Ellipsoid in GEM6 earth model.  */
  WGS84,			/* Ellipsoid in EGM96 earth model.  */
  WGS66,			/* Ancient crummy ellipsoid.  */
  USER_DEFINED			/* Some unknown user defined ellipsoid.  */
} asf_export_ellipsoid_t;

/* Check to see if an option was supplied or not If it was found,
   return its argument number.  Otherwise, return FLAG_NOT_SET. */
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


/* Forward declarations.  */
void
export_as_envi (const char *metadata_file_name,
		const char *image_data_file_name,
		const char *output_file_name);

void
export_as_esri (const char *metadata_file_name,
		const char *image_data_file_name,
		const char *output_file_name);

void
export_as_geotiff (const char *metadata_file_name,
		   const char *image_data_file_name,
		   const char *output_file_name);

void
export_as_jpeg (const char *metadata_file_name,
		const char *image_data_file_name, const char *output_file_name,
		long max_size);

void
export_as_ppm (const char *metadata_file_name,
	       const char *image_data_file_name, const char *output_file_name,
	       long max_size);

/* Main program body. */
int
main (int argc, char *argv[])
{

  output_format_t format;
  meta_parameters *md;

/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
	/* Command line input goes in it's own structure.  */
	command_line_parameters_t command_line;

	int formatFlag, sizeFlag, logFlag, quietFlag;
	int needed_args = 3;/*command & argument & argument*/
	int ii;

	/*Check to see which options were specified*/
	if(checkForOption("-help", argc, argv) != -1)/*Most important*/
		help_page();
	formatFlag = checkForOption("-format", argc, argv);
	sizeFlag = checkForOption("-size", argc, argv);
	logFlag = checkForOption("-log", argc, argv);
	quietFlag = checkForOption("-quiet", argc, argv);

	if(formatFlag != FLAG_NOT_SET)
		needed_args += 2;/*option & parameter*/
	if(sizeFlag != FLAG_NOT_SET)
		needed_args += 2;/*option & parameter*/
	if(quietFlag != FLAG_NOT_SET)
		needed_args += 1;/*option*/
	if(logFlag != FLAG_NOT_SET)
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
			usage();/*This exits with a failure*/
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

	if(sizeFlag != FLAG_NOT_SET)
		command_line.size = atol(argv[sizeFlag + 1]);
	else
		command_line.size = NO_MAXIMUM_OUTPUT_SIZE;

	if(quietFlag != FLAG_NOT_SET)
		command_line.quiet = TRUE;
	else
		command_line.quiet = FALSE;

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
    format = GEOTIFF;
    strcpy(command_line.output_name,
	   appendExt(command_line.output_name, ".tif"));
  }
  else if ( strcmp (command_line.format, "JPEG") == 0 ||
    strcmp(command_line.format, "JPG") == 0) {
    format = JPEG;
    strcpy(command_line.output_name,
	   appendExt(command_line.output_name, ".jpg"));
  }
  else if ( strcmp (command_line.format, "PPM") == 0 ) {
    format = PPM;
    strcpy(command_line.output_name,
	   appendExt(command_line.output_name, ".ppm"));
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
  else if ( format == GEOTIFF ) {
    export_as_geotiff (command_line.in_meta_name, command_line.in_data_name,
		       command_line.output_name);
  }
  else if ( format == JPEG ) {
    export_as_jpeg (command_line.in_meta_name, command_line.in_data_name,
		    command_line.output_name, command_line.size);
  }
  else if ( format == PPM ) {
    export_as_ppm (command_line.in_meta_name, command_line.in_data_name,
		   command_line.output_name, command_line.size);
  }

  exit (EXIT_SUCCESS);
}

/* Get sample size in bytes of the data types represented by the
   meta_parameters_t.  */
static size_t
get_sample_size (meta_parameters *metadata)
{
  size_t sample_size;
  switch ( metadata->general->data_type ) {
  case BYTE:
    sample_size = sizeof (int8_t);
    break;
  case INTEGER16:
    sample_size = sizeof (int16_t);
    break;
  case INTEGER32:
    sample_size = sizeof (int32_t);
    break;
  case REAL32:
    sample_size = sizeof (float);
    break;
  case REAL64:
    sample_size = sizeof (double);
    break;
  default:
    assert (FALSE);		/* Other types aren't handled.  */
    break;
  }

  return sample_size;
}

/* Get the image data in data file image_data_file_name, using
   metadata metadata.  A pointer to new memory containing the image
   data is returned.  */
static void *
get_image_data (meta_parameters *metadata, const char *image_data_file)
{
  size_t sample_size = get_sample_size (metadata);
  size_t pixel_count;
  void *data;
  size_t read_count;
  int return_code;

  /* Read the image data itself.  */
  FILE *ifp = fopen (image_data_file, "r");
  if ( ifp == NULL ) {
	char* temp;
	sprintf(temp, "Failed to open %s: %s", image_data_file, strerror(errno));
	print_error(temp);
    exit (EXIT_FAILURE);
  }
  /* Total number of samples in image.  */
  pixel_count = metadata->general->line_count * metadata->general->sample_count;
  data = MALLOC (pixel_count * sample_size);
  read_count = fread (data, sample_size, pixel_count, ifp);
  if ( read_count != pixel_count ) {
    if ( feof (ifp) ) {
	  char* temp;
	  sprintf(temp, "Read wrong amoutn of data from %s", image_data_file);
	  print_error(temp);
    }
    else if ( ferror (ifp) ) {
	  char* temp;
	  sprintf(temp, "Read of file %s failed: %s", image_data_file, 
		  strerror(errno));
	  print_error(temp);
    }
    else {
      assert (FALSE);		/* Shouldn't be here.  */
    }
    exit (EXIT_FAILURE);
  }

  return_code = fclose (ifp);
  assert (return_code == 0);

  return data;
}

void
export_as_envi (const char *metadata_file_name,
		const char *image_data_file_name,
		const char *output_file_name)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  char envi_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  envi_header *envi;
  FILE *fp;
  time_t time;
  char t_stamp[15];
  char envi_data_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  char command[10000];
  int return_code;

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  assert (md->general->data_type == BYTE
	  || md->general->data_type == INTEGER16
	  || md->general->data_type == INTEGER32
	  || md->general->data_type == REAL32
	  || md->general->data_type == REAL64);

  create_name (envi_file_name, output_file_name, ".hdr");
  envi = meta2envi (md);

  /* Write ENVI header file */
  fp = FOPEN(envi_file_name, "w");
  strftime(t_stamp, 12, "%d-%b-%Y", localtime(&time));
  fprintf(fp, "ENVI\n");
  fprintf(fp, "description = {\n"
	      "  Converted to ENVI format on (%s)}\n", t_stamp);
  fprintf(fp, "samples = %i\n", envi->samples);
  fprintf(fp, "lines = %i\n", envi->lines);
  fprintf(fp, "bands = %i\n", envi->bands);
  fprintf(fp, "header offset = %i\n", envi->header_offset);
  fprintf(fp, "file type = %s\n", envi->file_type);
  fprintf(fp, "data type = %i\n", envi->data_type);
  fprintf(fp, "interleave = %s\n", envi->interleave);
  fprintf(fp, "sensor type = %s\n", envi->sensor_type);
  fprintf(fp, "byte order = %i\n", envi->byte_order);
  if (md->projection) {
    switch (md->projection->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      fprintf(fp,
	      "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %i, %s}\n",
	      envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
	      envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
	      envi->proj_dist_y, envi->projection_zone, envi->hemisphere);
      fprintf(fp,
	      "projection info = {3, %.4f, %.4f, %.4f, %.4f, "
	      "0.0, 0.0, 0.99996, %s}\n",
	      envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
	      envi->center_lon, envi->projection);
      break;
    case POLAR_STEREOGRAPHIC:
      fprintf(fp,
	      "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %s}\n",
	      envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
	      envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
	      envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
	      "projection info = {31, %.4f, %.4f, %.4f, %.4f, "
	      "0.0, 0.0, %s}\n",
	      envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
	      envi->center_lon, envi->projection);
      break;
    case ALBERS_EQUAL_AREA:
      fprintf(fp,
	      "map info = {%s, %i, %i  , %.4f, %.4f, %.4f, %.4f, %s}\n",
	      envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
	      envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
	      envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
	      "projection info = {9, %.4f, %.4f, %.4f, %.4f, "
                "0.0, 0.0, %.4f, %.4f, %s}\n",
	      envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
	      envi->center_lon, envi->standard_parallel1,
	      envi->standard_parallel2, envi->projection);
      break;
    case LAMBERT_CONFORMAL_CONIC:
      fprintf(fp,
	      "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %s}\n",
	      envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
	      envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
	      envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
	      "projection info = {4, %.4f, %.4f, %.4f, %.4f, "
	      "0.0, 0.0, %.4f, %.4f, %s}\n",
	      envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
	      envi->center_lon, envi->standard_parallel1,
	      envi->standard_parallel2, envi->projection);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      fprintf(fp,
	      "map info = {%s, %i, %i, %.4f, %.4f, %.4f, %.4f, %s}\n",
	      envi->projection, envi->ref_pixel_x, envi->ref_pixel_y,
	      envi->pixel_easting, envi->pixel_northing, envi->proj_dist_x,
	      envi->proj_dist_y, envi->hemisphere);
      fprintf(fp,
	      "projection info = {11, %.4f, %.4f, %.4f, %.4f, "
	      "0.0, 0.0, %s}\n",
	      envi->semimajor_axis, envi->semiminor_axis, envi->center_lat,
	      envi->center_lon, envi->projection);
      break;
    case STATE_PLANE:
      break;
    case SCANSAR_PROJECTION:
      break;
    }
  }
  fprintf(fp, "wavelength units = %s\n", envi->wavelength_units);
  /*** wavelength, data ignore and default stretch currently not used ***/
  FCLOSE(fp);

  /* Clean and report */
  free (envi);
  meta_free (md);

  strcpy (envi_data_file_name, output_file_name);
  strcat (envi_data_file_name, ".bil");
  sprintf (command, "cp %s %s\n", image_data_file_name, envi_data_file_name);
  return_code = system (command);
  if ( return_code != 0 ) {
    char* temp;
    sprintf(temp, "System command '%s' failed", command);
    print_error(temp);
    exit (EXIT_FAILURE);
  }
}

void
export_as_esri (const char *metadata_file_name,
		const char *image_data_file_name,
		const char *output_file_name)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  char esri_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  esri_header *esri;
  FILE *fp;
  time_t time;
  char t_stamp[15];
  char esri_prj_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  char projection[100], datum[100], spheroid_str[100]="", semimajor[25]="";
  char flattening[25];
  double central_meridian;
  spheroid_type_t spheroid;
  char esri_data_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  char command[10000];
  int return_code;

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  assert (md->general->data_type == BYTE
	  || md->general->data_type == INTEGER16
	  || md->general->data_type == INTEGER32
	  || md->general->data_type == REAL32
	  || md->general->data_type == REAL64);

  create_name (esri_file_name, output_file_name, ".hdr");
  esri = meta2esri (md);

  /* Write ESRI header file */
  fp = FOPEN(esri_file_name, "w");
  strftime(t_stamp, 12, "%d-%b-%Y", localtime(&time));
  fprintf(fp, "ESRI header file (created %s)\n\n", t_stamp);
  fprintf(fp, "NROWS            %i\n", esri->nrows);
  fprintf(fp, "NCOLS            %i\n", esri->ncols);
  fprintf(fp, "NBANDS           %i\n", esri->nbands);
  fprintf(fp, "NBITS            %i\n", esri->nbits);
  fprintf(fp, "BYTEORDER        %c\n", esri->byteorder);
  fprintf(fp, "LAYOUT           %s\n", esri->layout);
  fprintf(fp, "SKIPBYTES        %i\n", esri->skipbytes);
  if (md->projection) {
    fprintf(fp, "ULXMAP           %.3f\n", esri->ulxmap);
    fprintf(fp, "ULYMAP           %.3f\n", esri->ulymap);
  }
  fprintf(fp, "XDIM             %.3f\n", esri->xdim);
  fprintf(fp, "YDIM             %.3f\n", esri->ydim);
  /* bandrowbytes, totalrowbytes, bandgapdata and nodata currently not used */
  FCLOSE(fp);

  free (esri);

  /* Write ESRI projection file */
  create_name (esri_prj_file_name, output_file_name, ".prj");

  if (md->projection && md->projection->type < SCANSAR_PROJECTION) {

    if (FLOAT_EQUIVALENT(md->general->re_major,6378137) &&
        FLOAT_EQUIVALENT(md->general->re_minor,6356752.3143))
        spheroid = WGS_1984;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378135.0) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356750.519915))
             spheroid = WGS_1972;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378206.4) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356583.8))
             spheroid = CLARKE_1866;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378249.145) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356514.86955))
             spheroid = CLARKE_1880;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6377397.155) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356078.9628))
             spheroid = BESSEL_1841;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378157.5) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356772.2))
             spheroid = INTERNATIONAL_1967;
    else if (FLOAT_EQUIVALENT(md->general->re_major,6378137.0) &&
             FLOAT_EQUIVALENT(md->general->re_minor,6356752.31414))
             spheroid = GRS_1980;

    switch (spheroid) {
    case WGS_1984:
      strcpy(spheroid_str,"WGS_1984");
      strcpy(semimajor,"6378137");
      strcpy(flattening,"298.257223563");
      break;
    case WGS_1972:
      strcpy(spheroid_str,"WGS_1972");
      strcpy(semimajor,"6378135");
      strcpy(flattening,"298.26");
      break;
    case CLARKE_1866:
      strcpy(spheroid_str,"CLARKE_1866");
      strcpy(semimajor,"6378206.4");
      strcpy(flattening,"294.9786982");
      break;
    case CLARKE_1880:
      strcpy(spheroid_str,"CLARKE_1880");
      strcpy(semimajor,"6378249.138");
      strcpy(flattening,"293.466307656");
      break;
    case GRS_1967:
      strcpy(spheroid_str,"GRS_1967_Truncated");
      strcpy(semimajor,"6378160");
      strcpy(flattening,"298.25");
      break;
    case GRS_1980:
      strcpy(spheroid_str,"GRS_1980");
      strcpy(semimajor,"6378137");
      strcpy(flattening,"298.257222101");
      break;
    case BESSEL_1841:
      strcpy(spheroid_str,"BESSEL_1841");
      strcpy(semimajor,"6377397.155");
      strcpy(flattening,"299.1528128");
      break;
    case INTERNATIONAL_1924:
      strcpy(spheroid_str,"INTERNATIONAL_1924");
      strcpy(semimajor,"6378388");
      strcpy(flattening,"297");
      break;
    case INTERNATIONAL_1967:
      strcpy(spheroid_str,"INTERNATIONAL_1967");
      strcpy(semimajor,"6378160");
      strcpy(flattening,"298.25");
      break;
    }

    switch (md->projection->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      switch (spheroid) {
      case WGS_1984:
	strcpy(projection, "WGS_1984");
	strcpy(datum, "WGS_1984");
	break;
      case WGS_1972:
        strcpy(projection, "WGS_1972");
        strcpy(datum, "WGS_1972");
        break;
      case GRS_1980:
	strcpy(projection, "NAD_1983");
	strcpy(datum, "North_American_1983");
	break;
      case CLARKE_1866:
	strcpy(projection, "NAD_1927");
	strcpy(datum, "North_American_1927");
	break;
      case CLARKE_1880:
	assert (FALSE);		/* Not implemented yet.  */
	break;
      case BESSEL_1841:
	assert (FALSE);		/* Not implemented yet.  */
	break;
      case INTERNATIONAL_1924:
	assert (FALSE);		/* Not implemented yet.  */
	break;
      case INTERNATIONAL_1967:
	assert (FALSE);		/* Not implemented yet.  */
	break;
      case GRS_1967:
	assert (FALSE);		/* Not implemented yet.  */
	break;
      default:
	assert (FALSE);		/* Shouldn't be here.  */
	break;
      }
      central_meridian = ((6 * abs(md->projection->param.utm.zone)) - 183);
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
	      "PROJCS[\"%s_UTM_Zone_%d%c\","
	      "GEOGCS[\"GCS_%s\","
	      "DATUM[\"D_%s\","
	      "SPHEROID[\"%s\",%s,%s]],"
	      "PRIMEM[\"Greenwich\",0],"
	      "UNIT[\"Degree\",0.017453292519943295]],"
	      "PROJECTION[\"Transverse_Mercator\"],"
	      "PARAMETER[\"False_Easting\",500000],"
	      "PARAMETER[\"False_Northing\",0],"
	      "PARAMETER[\"Central_Meridian\",%.0f],"
	      "PARAMETER[\"Scale_Factor\",0.9996],"
	      "PARAMETER[\"Latitude_Of_Origin\",0],"
	      "UNIT[\"Meter\",1]]",
	      projection, md->projection->param.utm.zone, md->projection->hem, datum,
	      datum, spheroid_str, semimajor, flattening, central_meridian);
      FCLOSE(fp);
      break;
    case POLAR_STEREOGRAPHIC:
      if (md->projection->hem == 'N') strcpy(projection, "North_Pole_Stereographic");
      else if (md->projection->hem == 'S') strcpy(projection, "South_Pole_Stereographic");
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
	      "PROJCS[\"%s\","
	      "GEOGCS[\"GCS_WGS_1984\","
	      "DATUM[\"D_WGS_1984\","
	      "SPHEROID[\"%s\",%s,%s]],"
	      "PRIMEM[\"Greenwich\",0],"
	      "UNIT[\"Degree\",0.017453292519943295]],"
	      "PROJECTION[\"Stereographic\"],"
	      "PARAMETER[\"False_Easting\",0],"
	      "PARAMETER[\"False_Northing\",0],"
	      "PARAMETER[\"Central_Meridian\",%.0f],"
	      "PARAMETER[\"Scale_Factor\",1],"
	      "PARAMETER[\"Latitude_Of_Origin\",%.0f],"
	      "UNIT[\"Meter\",1]]",
	      projection, spheroid_str, semimajor, flattening,
	      md->projection->param.ps.slon, md->projection->param.ps.slat);
      FCLOSE(fp);
      break;
    case ALBERS_EQUAL_AREA:
      if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,25) &&
	  FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,20) &&
	  FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,-23) &&
	  FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,0)) {
	strcpy(projection, "Africa_Albers_Equal_Area_Conic");
	strcpy(datum, "WGS_1984");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-154) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,55) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,65) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,50)) {
	strcpy(projection, "Alaska_Albers_Equal_Area_Conic");
	strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,95) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,15) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,65) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,30)) {
	strcpy(projection, "Asia_North_Albers_Equal_Area_Conic");
	strcpy(datum, "WGS_1984");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,125) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,7) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,-32) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,-15)) {
	strcpy(projection, "Asia_South_Albers_Equal_Area_Conic");
	strcpy(datum, "WGS_1984");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,50) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,70) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,40)) {
	strcpy(projection, "Canada_Albers_Equal_Area_Conic");
	strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,10) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,43) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,62) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,30)) {
	strcpy(projection, "Europe_Albers_Equal_Area_Conic");
	strcpy(datum, "European_1950");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-157) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,8) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,18) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,13)) {
	strcpy(projection, "Hawaii_Albers_Equal_Area_Conic");
	strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,20) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,60) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,40)) {
	strcpy(projection, "North_America_Albers_Equal_Area_Conic");
	strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-60) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,-5) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,-42) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,-32)) {
	strcpy(projection, "South_America_Albers_Equal_Area_Conic");
	strcpy(datum, "South_American_1969");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,29.5) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,45.5) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,37.5)) {
	strcpy(projection, "USA_Contiguous_Albers_Equal_Area_Conic");
	strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.albers.center_meridian,-96) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel1,29.5) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.std_parallel2,45.5) &&
	       FLOAT_EQUIVALENT(md->projection->param.albers.orig_latitude,23.0)) {
	strcpy(projection, "USA_Contiguous_Albers_Equal_Area_Conic_USGS_version");
	strcpy(datum, "North_American_1983");
      }
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
	      "PROJCS[\"%s\","
	      "GEOGCS[\"GCS_%s\","
	      "DATUM[\"D_%s\","
	      "SPHEROID[\"%s\",%s,%s]],"
	      "PRIMEM[\"Greenwich\",0],"
	      "UNIT[\"Degree\",0.0174532925199432955]],"
	      "PROJECTION[\"Albers\"],"
	      "PARAMETER[\"False_Easting\",0],"
	      "PARAMETER[\"False_Northing\",0],"
	      "PARAMETER[\"Central_Meridian\",%.1f],"
	      "PARAMETER[\"Standard_Parallel_1\",%.1f],"
	      "PARAMETER[\"Standard_Parallel_2\",%.1f],"
	      "PARAMETER[\"Latitude_Of_Origin\",%.1f],"
	      "UNIT[\"Meter\",1]],",
	      projection, datum, datum, spheroid_str, semimajor, flattening,
	      md->projection->param.albers.center_meridian,
	      md->projection->param.albers.std_parallel1,
	      md->projection->param.albers.std_parallel2,
	      md->projection->param.albers.orig_latitude);
      FCLOSE(fp);
     break;
    case LAMBERT_CONFORMAL_CONIC:
      if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,25) &&
	  FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,20) &&
	  FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,-23) &&
	  FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,0)) {
	strcpy(projection, "Africa_Lambert_Conformal_Conic");
	strcpy(datum, "WGS_1984");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,105) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,30) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,62) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,0)) {
	strcpy(projection, "Asia_Lambert_Conformal_Conic");
	strcpy(datum, "WGS_1984");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,95) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,15) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,65) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,30)) {
	strcpy(projection, "Asia_North_Lambert_Conformal_Conic");
	strcpy(datum, "WGS_1984");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,125) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,7) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,-32) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,-15)) {
	strcpy(projection, "Asia_South_Albers_Equal_Area_Conic");
	strcpy(datum, "WGS_1984");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-96) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,50) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,70) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,40)) {
	strcpy(projection, "Canada_Lambert_Conformal_Conic");
	strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,10) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,43) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,62) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,30)) {
	strcpy(projection, "Europe_Lambert_Conformal_Conic");
	strcpy(datum, "European_1950");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-96) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,20) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,60) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,40)) {
	strcpy(projection, "North_America_Lambert_Conformal_Conic");
	strcpy(datum, "North_American_1983");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-60) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,-5) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,-42) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,-32)) {
	strcpy(projection, "South_America_Lambert_Conformal_Conic");
	strcpy(datum, "South_American_1969");
      }
      else if (FLOAT_EQUIVALENT(md->projection->param.lamcc.lon0,-96) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat1,33) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.plat2,45) &&
	       FLOAT_EQUIVALENT(md->projection->param.lamcc.lat0,39)) {
	strcpy(projection, "USA_Contiguous_Lambert_Conformal_Conic");
	strcpy(datum, "North_American_1983");
      }
      fp = FOPEN(esri_prj_file_name, "w");
      fprintf(fp,
	      "PROJCS[\"%s\","
	      "GEOGCS[\"GCS_%s\","
	      "DATUM[\"D_%s\","
	      "SPHEROID[\"%s\",%s,%s]],"
	      "PRIMEM[\"Greenwich\",0],"
	      "UNIT[\"Degree\",0.0174532925199432955]],"
	      "PROJECTION[\"Lambert_Conformal_Conic\"],"
	      "PARAMETER[\"False_Easting\",0],"
	      "PARAMETER[\"False_Northing\",0],"
	      "PARAMETER[\"Central_Meridian\",%.0f],"
	      "PARAMETER[\"Standard_Parallel_1\",%.0f],"
	      "PARAMETER[\"Standard_Parallel_2\",%.0f],"
	      "PARAMETER[\"Latitude_Of_Origin\",%.0f],"
	      "UNIT[\"Meter\",1]]",
	      projection, datum, datum, spheroid_str, semimajor, flattening,
	      md->projection->param.lamcc.lon0,
	      md->projection->param.lamcc.plat1,
	      md->projection->param.lamcc.plat2,
	      md->projection->param.lamcc.lat0);
      FCLOSE(fp);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      break;
    case STATE_PLANE:
      break;
    case SCANSAR_PROJECTION:
      break;
    }
  }

  meta_free(md);

  /* Write ESRI data file */
  strcpy (esri_data_file_name, output_file_name);
  strcat (esri_data_file_name, ".bil");
  sprintf (command, "cp %s %s\n", image_data_file_name, esri_data_file_name);
  return_code = system (command);
  if ( return_code != 0 ) {
    char* temp;
	sprintf(temp, "System command '%s' failed", command);
	print_error(temp);
    exit (EXIT_FAILURE);
  }
}

/* Return the average of all the kernel_size * kernel_size elements
   centered around element i, j.  The i and j arguments must be within
   the bounds of img, and the kernel_size must be odd.  The image is
   reflected at the edges and corners for the purpose of determining
   this average.  */
static unsigned char
averaging_kernel (gsl_matrix_uchar *img, int kernel_size, size_t i, size_t j)
{
  int i_idx, j_idx;
  int i_min = i - kernel_size / 2;
  int i_max = i + kernel_size / 2;
  int j_min = j - kernel_size / 2;
  int j_max = j + kernel_size / 2;
  int sum = 0;
  int average; /* Truncated average.  */

  assert (kernel_size % 2 != 0); /* Odd-sized kernels only.  */

  for ( i_idx = i_min ; i_idx < i_max ; i_idx++ ) {
    /* The i index to use, adjusted in case we are off the edge of the
       image.  This choice (and the corresponding choice for j)
       implement a kernel that pretends that a mirror image of the
       image exists at the image edges (and corners). */
    int itu = i_idx;
    if ( itu < 0 )
      itu = -itu - 1;
    else if ( itu >= img->size1 )
      itu = img->size1 - (itu - img->size1) - 1;
    for ( j_idx = j_min ; j_idx < j_max ; j_idx++ ) {
      /* See the comment for variable itu above.  */
      int jtu = j_idx;
      if ( jtu < 0 )
	jtu = -jtu - 1;
      else if ( jtu >= img->size2 )
	jtu = img->size2 - (jtu - img->size2) - 1;
      sum += gsl_matrix_uchar_get (img, itu, jtu);
    }
  }

  average = sum / pow (kernel_size, 2); /* Truncated average.  */
  /* Since we are averaging unsigned char values, this should always
     be true.  */
  assert (average <= UCHAR_MAX);

  return average;
}

/* Average together tiles of kernel_size pixels in the *width by
   *height image at pixels, reducing its size by a factor of about
   kernel size in each dimension.  The new image replaces the old in
   pixels (extra memory is freed) and a pointer to the possibly
   relocated pixel data is returned.  The new image width and height
   replace the input width and height arguments.  */
static unsigned char *
average_unsigned_char_pixels (unsigned char *pixels, unsigned long *width,
			      unsigned long *height, int kernel_size)
{
  /* Input width and height.  */
  size_t iwidth = *width, iheight = *height;
  gsl_matrix_uchar *iimg;
  size_t ii, jj;
  size_t owidth;
  size_t oheight;
  gsl_matrix_uchar *oimg;
  unsigned char *reallocated_pixels;

  assert (kernel_size % 2 != 0); /* Odd-sized kernels only.  */

  /* Make a matrix form of the input image for easy indexing.  */
  iimg = gsl_matrix_uchar_alloc (iheight, iwidth);
  for ( ii = 0 ; ii < iheight ; ii++ ) {
    for ( jj = 0 ; jj < iwidth ; jj++ ) {
      gsl_matrix_uchar_set (iimg, ii, jj, pixels[ii * iwidth + jj]);
    }
  }

  /* Dimensions of the averaged image.  */
  owidth = ceil (iwidth / kernel_size);
  oheight = ceil (iheight / kernel_size);

  /* Form the output image.  */
  oimg = gsl_matrix_uchar_alloc (oheight, owidth);
  for ( ii = 0 ; ii < oheight ; ii++ ) {
    for ( jj = 0 ; jj < owidth ; jj++ ) {
      gsl_matrix_uchar_set
	(oimg, ii, jj, averaging_kernel (iimg, kernel_size,
					 ii * kernel_size + kernel_size / 2,
					 jj * kernel_size + kernel_size / 2));
    }
  }

  /* Write output image back into pixel memory.  */
  for ( ii = 0 ; ii < oheight ; ii++ ) {
    for ( jj = 0 ; jj < owidth ; jj++ ) {
      pixels[ii * owidth + jj] = gsl_matrix_uchar_get (oimg, ii, jj);
    }
  }

  /* Done with matrix forms of the image.  */
  gsl_matrix_uchar_free (oimg);
  gsl_matrix_uchar_free (iimg);

  /* Modify the input/output arguments and resize the allocated space
     as promised.  */
  *width = owidth;
  *height = oheight;
  reallocated_pixels = realloc (pixels, owidth * oheight);

  return reallocated_pixels;
}

/* Given a block of pixel_count floats, map them linearly from range
   [max (minsample, mean - 3 * sigma), min (maxsample, mean + 3 * sigma)]
   into the unsigned char range, with input samples outside the above
   range clamped.  Return the data in new malloc()ed memory.  */
static unsigned char *
scale_floats_to_unsigned_bytes (float *daf, size_t pixel_count)
{
  unsigned char *pixels = malloc (pixel_count * sizeof (unsigned char));

  /* Minimum and maximum values in the input data.  */
  float imin = gsl_stats_float_min (daf, 1, pixel_count);
  float imax = gsl_stats_float_max (daf, 1, pixel_count);
  /* Mean value of input data.  */
  float imean = gsl_stats_float_mean (daf, 1, pixel_count);
  /* Standard deviation of input data.  */
  float isdev = gsl_stats_float_sd (daf, 1, pixel_count);
  /* Minimum and maximum after clamping.  */
  float omin = GSL_MAX (imean - 3 * isdev, imin);
  float omax = GSL_MIN (imean + 3 * isdev, imax);

  /* Shift we need to apply to the data to get it to fall in the
     range of the unsigned chars.  */
  float bias = -omin + 0.25;

  /* Compute all the output pixels.  */
  size_t ii;
  for ( ii = 0 ; ii < pixel_count ; ii++ ) {
    if ( daf[ii] < omin ) {
      pixels[ii] = 0;		/* Clamp low.  */
    }
    else if ( daf[ii] > omax ) {
      pixels[ii] = UCHAR_MAX;	/* Clamp high.  */
    }
    else {
      pixels[ii] = (daf[ii] + bias) * (UCHAR_MAX / (omax - omin));
    }
  }

  return pixels;
}

/* Scale the *width x *height image at pixels st its large dimension
   is less than or equal to max_large_dimension.  The memory pointed
   to by pixels is resized and possible relocated, with the new
   location being returned.  The new image width and height are
   returned in *width and *height.  */
static unsigned char *
scale_unsigned_char_image_dimensions (unsigned char *pixels,
				      unsigned long max_large_dimension,
				      unsigned long *width,
				      unsigned long *height)
{
  /* This assertion is pretty obvious, but since the algorithm needs
     it to work correctly, its included.  */
  assert (max_large_dimension > 1);
  if ( GSL_MAX (*width, *height) > max_large_dimension ) {
    int kernel_size = GSL_MAX (*width, *height) / max_large_dimension + 1;
    if ( kernel_size % 2 != 1 ) {
      kernel_size++;
    }
    pixels = average_unsigned_char_pixels (pixels, width, height, kernel_size);
  }

  return pixels;
}

void
export_as_jpeg (const char *metadata_file_name,
		const char *image_data_file_name, const char *output_file_name,
		long max_size)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  /* Maximum large dimension to allow in the output.  */
  unsigned long max_large_dimension;
  size_t pixel_count;
  float *daf;
  int jj;
  JSAMPLE test_jsample;
  unsigned char *pixels;
  unsigned long width, height;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  FILE *ofp;
  int return_code;

  assert (md->general->data_type == REAL32);

  if ( (max_size > line_count && max_size > sample_count)
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    max_large_dimension = GSL_MAX (line_count, sample_count);
  }
  else {
    max_large_dimension = max_size;
  }

  pixel_count = (size_t) line_count * sample_count;

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  daf = get_image_data (md, image_data_file_name);
  /* It supposed to be big endian data, this converts to host byte
     order.  */
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* We need a version of the data in JSAMPLE form, so we have to
     form a scaled version of the input data.  */
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  assert (sizeof (unsigned char) == 1);
  assert (sizeof (unsigned char) == sizeof (JSAMPLE));
  test_jsample = 0;
  test_jsample--;
  assert (test_jsample == UCHAR_MAX); /* Did we wrap?  */
  /* This space is resized later (with realloc) if the image is
     scaled.  */
  pixels = scale_floats_to_unsigned_bytes (daf, pixel_count);

  /* We want to scale the image st the long dimesion is less than or
     equal to this value the prescribed maximum.  */
  /* Current size of the image.  */
  width = sample_count;
  height = line_count;
  /* Scale the image, modifying width and height to reflect the new
     image size.  */
  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
						 &width, &height);

  /* Initializae libjpg structures.  */
  cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_compress (&cinfo);

  /* Open the output file to be used.  */
  ofp = fopen (output_file_name, "w");
  if ( ofp == NULL ) {
    char *temp;
	sprintf(temp, "Open of %s for writing failed: %s", output_file_name, strerror(errno));
	print_error(temp);
    exit (EXIT_FAILURE);
  }

  /* Connect jpeg output to the output file to be used.  */
  jpeg_stdio_dest (&cinfo, ofp);

  /* Set image parameters that libjpeg needs to know about.  */
  cinfo.image_width = width;
  cinfo.image_height = height;
  cinfo.input_components = 1;	/* Grey scale => 1 color component / pixel.  */
  cinfo.in_color_space = JCS_GRAYSCALE;
  jpeg_set_defaults (&cinfo);	/* Use default compression parameters.  */
  /* Reassure libjpeg that we will be writing a complete JPEG file.  */
  jpeg_start_compress (&cinfo, TRUE);

  /* Write the jpeg.  */
  while ( cinfo.next_scanline < cinfo.image_height ) {
    /* We are writing one row at a time.  */
    const int rows_to_write = 1;
    int rows_written;
    JSAMPROW *row_pointer = MALLOC (rows_to_write * sizeof (JSAMPROW));
    row_pointer[0] = &(pixels[cinfo.next_scanline * width]);
    rows_written = jpeg_write_scanlines (&cinfo, row_pointer, rows_to_write);
    assert (rows_written == rows_to_write);
  }

  /* Finsh compression and close the jpeg.  */
  jpeg_finish_compress (&cinfo);
  return_code = fclose (ofp);
  assert (return_code == 0);
  jpeg_destroy_compress (&cinfo);

  free (pixels);
  free (daf);
  meta_free (md);
}

#define PPM_MAGIC_NUMBER "P6"

void
export_as_ppm (const char *metadata_file_name,
	       const char *image_data_file_name, const char *output_file_name,
	       long max_size)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  /* Get image dimensions.  */
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  /* Maximum large dimension to allow in the output.  */
  unsigned long max_large_dimension;
  size_t pixel_count;
  float *daf;
  int jj;
  unsigned char *pixels;
  unsigned long width,height;
  FILE *ofp;
  const char *ppm_magic_number = PPM_MAGIC_NUMBER;
  int print_count;
  const int max_color_value = 255;
  size_t ii;
  int return_code;

  assert (md->general->data_type == REAL32);

  if ( (max_size > line_count && max_size > sample_count)
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    max_large_dimension = GSL_MAX (line_count, sample_count);
  }
  else {
    max_large_dimension = max_size;
  }

  pixel_count = (size_t) line_count * sample_count;

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  daf = get_image_data (md, image_data_file_name);
  /* Input is supposed to be big endian data, this converts to host
     byte order.  */
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* We need a version of the data in unsigned byte form, so we have
     to form a scaled version of the input data.  */
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  assert (sizeof (unsigned char) == 1);

  /* This pixel space is resized later (with realloc) if the image
     dimensions are scaled.  */
  pixels = scale_floats_to_unsigned_bytes (daf, pixel_count);

  /* We want to scale the image st the long dimension is less than or
     equal to the prescribed maximum.  */
  /* Current size of the image.  */
  width = sample_count;
  height = line_count;
  /* Scale the image, modifying width and height to reflect the new
     image size.  */
  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
						 &width, &height);

  /* Open the output file to be used.  */
  ofp = fopen (output_file_name, "w");
  if ( ofp == NULL ) {
    char* temp;
	sprintf(temp, "Open of %s for writing failed: %s", output_file_name, strerror(errno));
	print_error(temp);
    exit (EXIT_FAILURE);
  }

  /* Write the ppm header.  */
  print_count = fprintf (ofp, PPM_MAGIC_NUMBER);
  /* After this we will assume that writing to the new file will work
     correctly.  */
  assert (print_count == strlen (ppm_magic_number));
  fprintf (ofp, "\n");
  fprintf (ofp, "%ld\n", width);
  fprintf (ofp, "%ld\n", height);
  fprintf (ofp, "%d\n", max_color_value);

  /* Write the pixels themselves.  */
  for ( ii = 0 ; ii < height ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < width ; jj++ ) {
      /* Write red, green, and blue the same to get grey scale.  */
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
    }
  }

  return_code = fclose (ofp);
  assert (return_code == 0);

  free (pixels);
  free (daf);
  meta_free (md);
}

void
export_as_geotiff (const char *metadata_file_name,
		   const char *image_data_file_name,
		   const char *output_file_name)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  unsigned short sample_size = 4;
  unsigned short sample_format;
  unsigned int line_count = md->general->line_count;
  unsigned int sample_count = md->general->sample_count;
  size_t pixel_count = line_count * sample_count;
  float *daf;
  int jj;
  TIFF *otif;
  GTIF *ogtif;
  size_t ii;
  int return_code;

  /* This constant is from the GeoTIFF spec.  It basically means that
     the system which would normally be specified by the field
     (projected coordinate system, datum, ellipsoid, whatever), in
     instead going to be specified by more detailed low level
     tags.  */
  const int user_defined_value_code = 32767;
  /* Major and minor ellipse axis lengths.  This shows up in two
     different places in our metadata, we want the projected one if
     its available, otherwise the one from general.  */
  double re_major, re_minor;
  /* Nail down which ellipsoid we are on exactly.  The ASF metadata
     doesn't specify this though, so we take a look at the major and
     minor axis values and try to find a matching ellipsoid.  */
  asf_export_ellipsoid_t ellipsoid;
  const double clarke1866_major_axis = 6378206.4;
  const double clarke1866_minor_axis = 6356583.8;
  const double gem10c_major_axis = 6378144;
  const double gem10c_minor_axis = 6356759;
  const double wgs66_major_axis = 6378145.0;
  const double wgs66_minor_axis = 6356759.769356;
  const double wgs84_major_axis = 6378137;
  const double wgs84_flattening = 1.0 / 298.257223563;
  const double wgs84_minor_axis = wgs84_major_axis * (1 - wgs84_flattening);

  /* Insist that the minor axis match what we are expecting to within
     this tolerance.  */
  double axis_tolerance = 0.2;

  assert (md->general->data_type == REAL32);
  assert (sizeof (unsigned short) == 2);
  assert (sizeof (unsigned int) == 4);

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  daf = get_image_data (md, image_data_file_name);
  /* It supposed to be big endian data, this converts to host byte
     order.  */
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* Open output tiff file and GeoKey file descriptor.  */
  otif = XTIFFOpen (output_file_name, "w");
  assert (otif != NULL);
  ogtif = GTIFNew (otif);
  assert (ogtif != NULL);

  /* Set the normal TIFF image tags.  */
  TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, sample_count);
  TIFFSetField(otif, TIFFTAG_IMAGELENGTH, line_count);
  TIFFSetField(otif, TIFFTAG_BITSPERSAMPLE, sample_size * 8);
  TIFFSetField(otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(otif, TIFFTAG_ROWSPERSTRIP,1);
  TIFFSetField(otif, TIFFTAG_XRESOLUTION,1);
  TIFFSetField(otif, TIFFTAG_YRESOLUTION,1);
  TIFFSetField(otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  switch ( md->general->data_type ) {
  case BYTE:
    sample_format = SAMPLEFORMAT_UINT;
    break;
  case INTEGER16:
    sample_format = SAMPLEFORMAT_INT;
    break;
  case INTEGER32:
    sample_format = SAMPLEFORMAT_INT;
    break;
  case REAL32:
    sample_format = SAMPLEFORMAT_IEEEFP;
    break;
  case REAL64:
    assert (FALSE);		/* Not sure which format we want here.  */
    break;
  default:
    assert (FALSE);		/* Shouldn't be here.  */
    break;
  }
  TIFFSetField(otif, TIFFTAG_SAMPLEFORMAT, sample_format);
  TIFFSetField(otif, TIFFTAG_DATATYPE, sample_format);

  /* Set the GeoTIFF extension image tags.  */

  /* FIXME: its not good to say all our products are
     RasterPixelIsArea, but for now that's what we do.  */
  GTIFKeySet (ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);

  if ( md->sar->image_type == 'P' ) {
    re_major = md->projection->re_major;
    re_minor = md->projection->re_minor;
  }
  else {
    re_major = md->general->re_major;
    re_minor = md->general->re_minor;
  }

  if ( FLOAT_COMPARE_TOLERANCE (re_major, clarke1866_major_axis,
				axis_tolerance)
       && FLOAT_COMPARE_TOLERANCE (re_minor, clarke1866_minor_axis,
				   axis_tolerance) ) {
    ellipsoid = CLARKE1866;
  }
  else if ( FLOAT_COMPARE_TOLERANCE (re_major, wgs84_major_axis,
				     axis_tolerance)
	    && FLOAT_COMPARE_TOLERANCE (re_minor, wgs84_minor_axis,
					axis_tolerance) ) {
    ellipsoid = WGS84;
  }
  else if ( FLOAT_COMPARE_TOLERANCE (re_major, gem10c_major_axis,
				     axis_tolerance)
	    && FLOAT_COMPARE_TOLERANCE (re_minor, gem10c_minor_axis,
					axis_tolerance) ) {
    ellipsoid = GEM10C;
  }
  /* FIXME: I have some ellipsoid that looks slightly like this thing,
     so in order to give us a datum to use for geotiffs we pretend its
     this.  The geolocation is bad because of range migration
     anyway.  */
  else if ( FLOAT_COMPARE_TOLERANCE (re_major, wgs66_major_axis, 2)
	    && FLOAT_COMPARE_TOLERANCE (re_minor, wgs66_minor_axis, 5) ) {
    ellipsoid = WGS66;
  }
  else {
    /* FIXME: we badly need to get the ellipsoid/datum mess sorted
       out.  This problem goes deeper than asf_export, however.  */
    char errmsg[512];
    sprintf (errmsg, "%s: warning: couldn't conclude which ellipsoid is "
	     "being used from ellipsoid axis dimensions in metadata, "
	     "using user defined ellipsoid\n", ASF_NAME_STRING);
    fprintf(stderr, errmsg);
    printLog(errmsg);
    ellipsoid = USER_DEFINED;
  }

  /* If we have a map projected image, write the projection
     information into the GeoTIFF.  */
  /* FIXME: this is a terrible hack to deal with scansar crap.  */
  if ( md->sar->image_type == 'P'
       && md->projection->type != SCANSAR_PROJECTION) {
    /* Tie points for image corners.  There is space for four tie
       points, each consisting of three raster coordinates, followed
       by three geospatial coordinates.  */
    double tie_points[4][6];
    double pixel_scale[3];
    short projection_code;
    int max_citation_length = 500;
    char *citation;
    int citation_length;

    if ( FLOAT_COMPARE_TOLERANCE (md->projection->re_major,
				  clarke1866_major_axis, axis_tolerance)
	 && FLOAT_COMPARE_TOLERANCE (md->projection->re_minor,
				     clarke1866_minor_axis,
				     axis_tolerance) ) {
      ellipsoid = CLARKE1866;
    }
    else if ( FLOAT_COMPARE_TOLERANCE (md->projection->re_major,
				       wgs84_major_axis, axis_tolerance)
	      && FLOAT_COMPARE_TOLERANCE (md->projection->re_minor,
					  wgs84_minor_axis, axis_tolerance) ) {
      ellipsoid = WGS84;
    }
    else if ( FLOAT_COMPARE_TOLERANCE (md->projection->re_major,
				       gem10c_major_axis, axis_tolerance)
	      && FLOAT_COMPARE_TOLERANCE (md->projection->re_minor,
					  gem10c_minor_axis,
					  axis_tolerance) ) {
      ellipsoid = GEM10C;
    }
    else {
      /* FIXME: we badly need to get the ellipsoid/datum mess sorted
	 out.  This problem goes deeper than asf_export, however.  */
      const int max_error_string_length = 2000;
      char *temp = malloc ((max_error_string_length + 1) * sizeof (char));
      sprintf (temp, "\n\nWARNING: Couldn't conclude which ellipsoid is "
	       "being used from ellipsoid axis dimensions in metadata, "
	       "assuming WGS84 ellipsoid\n");
      printf (temp);
      printLog (temp);
      ellipsoid = WGS84;
    }

    /* We will tie down the corner of the image (which has raster coordinates
       0, 0, 0).  */
    tie_points[0][0] = 0.0;
    tie_points[0][1] = 0.0;
    tie_points[0][2] = 0.0;
    /* FIXME: we should be getting the actual corner of the image
       here, not the center of the corner pixel, and I'm not sure that
       startX and startY are what we want (verify and fix if
       needed.  */
    tie_points[0][3] = md->projection->startX;
    tie_points[0][4] = md->projection->startY;
    tie_points[0][5] = 0.0;
    /* Some applications (e.g., ArcView) won't handle GeoTIFF images
       with more than one tie point pair.  Therefore, only the upper
       left corner is being written to the GeoTIFF file.  In order to
       write all computed tie points to the GeoTIFF, change the 6 to
       size in the line below.  */
    TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 6, tie_points);

    /* Set the scale of the pixels, in projection coordinates.  */
    pixel_scale[0] = md->projection->perX;
    /* Note: we take -perY here because our tools treat the upper left
       as startX, startY, and then count down in the y direction with
       lower scan lines of the image.  The GeoTIFF world uses
       increasing y coordinates in the downward direction.  */
    pixel_scale[1] = -md->projection->perY;
    pixel_scale[2] = 0;
    TIFFSetField (otif, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);

    GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1,
		ModelTypeProjected);

    switch ( md->projection->type ) {
      case UNIVERSAL_TRANSVERSE_MERCATOR:
	/* For now we only handle UTM data that is referenced to the
	   WGS84 ellipsoid.  */
	assert (ellipsoid == WGS84);

	/* This weird paranoid assertion is because I remember once
           when we couln't figure out how to set some datum code
           right, we set it to -1.  */
	assert (md->projection->param.utm.zone != -1);

	/* Here we use some funky arithmetic to get the correct
	   geotiff coordinate system type key from our zone code.
	   Here are a few assertions to try to ensure that the
	   convention used for the libgeotiff constants is as
	   expected.  Also note that we have already verified that we
	   are on a WGS84 ellipsoid.  */
	assert (PCS_WGS84_UTM_zone_60N - PCS_WGS84_UTM_zone_1N == 59);
	assert (PCS_WGS84_UTM_zone_60S - PCS_WGS84_UTM_zone_1S== 59);

	if ( md->projection->hem == 'N' ) {
	  const int northern_utm_zone_base = PCS_WGS84_UTM_zone_1N - 1;
	  projection_code = northern_utm_zone_base;
	}
	else if ( md->projection->hem == 'S' ) {
	  const int southern_utm_zone_base = PCS_WGS84_UTM_zone_1S - 1;
	  projection_code = southern_utm_zone_base;
	}
	else {
	  assert (FALSE);		/* Shouldn't be here.  */
	}
	projection_code += md->projection->param.utm.zone;

	GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
		    projection_code);
	GTIFKeySet (ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
	citation = MALLOC ((max_citation_length + 1) * sizeof (char));
	citation_length
	  = snprintf (citation, max_citation_length + 1,
                      "UTM zone %d %c projected GeoTIFF written by Alaska "
		      "Satellite Facility tools",
		      md->projection->param.utm.zone,
		      md->projection->hem);
	assert (citation_length >= 0
		&& citation_length <= max_citation_length);
	GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
	free (citation);
	break;
      case POLAR_STEREOGRAPHIC:
	GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
		    user_defined_value_code);
	GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
		    user_defined_value_code);
	GTIFKeySet (ogtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
	GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
		    CT_PolarStereographic);
	GTIFKeySet (ogtif, ProjStraightVertPoleLongGeoKey, TYPE_DOUBLE, 1,
		    md->projection->param.ps.slon);
	GTIFKeySet (ogtif, ProjOriginLatGeoKey, TYPE_DOUBLE, 1,
		    md->projection->param.ps.slat);
	GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
	GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
	GTIFKeySet (ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
	GTIFKeySet (ogtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, 
		    Angular_Degree);
/*
*        ///////////////////////////////////////////////////////////////////
*	//
*	// Here we emplay a slightly weird strategy: we always use a
*	// WGS84 datum, no matter what the ASF metadata for the
*	// product we are exporting says.  This is ok because the
*	// error introduced is very small compared to other error
*	// sources, and the geotiff viewers handle WGS84 datums
*	// much better than user defined ones.
*	//
*	///////////////////////////////////////////////////////////////////
*
*	** Maximum lenght of coordinate system description strings
*	   used with the proj library. **
*	const size_t max_coordinate_system_description_length = 1000;
*	** The coordinate system as described by the ASF metadata. **
*	char *tmp = malloc (max_coordinate_system_description_length + 1);
*	int write_count 
*	  = snprintf (tmp, max_coordinate_system_description_length + 1,
*		      "+proj=stere +a=%lf +b=%lf +lat_0=%lf +lon_0=%lf "
*		      "+lat_ts=%lf", re_major, re_minor, 
*		      (md->projection->param.ps.slat > 0.0 ? 90.0 : -90.0),
*		      md->projection->param.ps.slon, 
*		      md->projection->param.ps.slat);
*	assert (write_count < max_coordinate_system_description_length + 1);
*	projPJ input_coordinate_system = pj_init_plus (tmp);
*	assert (input_coordinate_system != NULL);
*	** The coordinate system to be used for the output geotiff.  **
*	write_count
*	  = snprintf (tmp, max_coordinate_system_description_length + 1,
*		      "+proj=stere +datum=WGS84 +lat_0=%lf +lon_0=%lf "
*		      "+lat_ts=%lf", 
*		      (md->projection->param.ps.slat > 0.0 ? 90.0 : -90.0),
*		      md->projection->param.ps.slon, 
*		      md->projection->param.ps.slat);
*	assert (write_count < max_coordinate_system_description_length + 1);
*	projPJ geotiff_coordinate_system = pj_init_plus (tmp);
*	assert (geotiff_coordinate_system != NULL);
*	double tmp1 = md->projection->startX;
*	double tmp2 = md->projection->startY;
*	double tmp3 = 0;
*	return_code = pj_transform (input_coordinate_system, 
*				    geotiff_coordinate_system, 1, 1, &tmp1, 
*				    &tmp2, &tmp3);
*	assert (return_code == 0);
*	** The maximum allowable projection error.  If changing the
*	   datum from the one in the metadata to the WGS84 datum moves
*	   the projection corner point by this amount or more in
*	   projection coordinates, an exception is triggered.  This
*	   value was chosen based ona seat-of-the-pants feel which
*	   accounts for the various error sources: range migration,
*	   etc.  If the geolocation process is anywhere near this
*	   accurate, we are doing really good. **
*	const double max_allowable_projection_error = 30.0;
*	assert (sqrt (pow (fabs (tmp1 - md->projection->startX), 2)
*		      + pow (fabs (tmp2 - md->projection->startY), 2))
*		< max_allowable_projection_error);
*	free (tmp);
*
*        ///////////////////////////////////////////////////////////////////
*/
	/* Fill in the details of the geographic coordinate system used.  */
	switch ( ellipsoid ) {
	case CLARKE1866:
	  GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
		      GCSE_Clarke1866);
	  break;
	case GEM10C:
	  GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_GEM10C);
	  break;
	case WGS84:
	  GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_84);
	  break;
	case USER_DEFINED:
	  GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
		      user_defined_value_code);
	  GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
		      user_defined_value_code);
	  /* The angular units are degrees and the meridian is
	     Greenwitch, so we don't need to define them explicitly.
	     The GeogCitation key will be filled in later.  */
	  GTIFKeySet (ogtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1,
		      user_defined_value_code);
	  GTIFKeySet (ogtif, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1,
		      re_major);
	  GTIFKeySet (ogtif, GeogSemiMinorAxisGeoKey, TYPE_DOUBLE, 1,
		      re_minor);
	  citation = MALLOC ((max_citation_length + 1) * sizeof (char));
	  citation_length
	    = snprintf (citation, max_citation_length + 1,
			"Geographic coordinate system using reference "
			"ellipsoid with semimajor axis of %f meters and "
			"semiminor axis of %f meters",
			re_major, re_minor);
	  GTIFKeySet (ogtif, GeogCitationGeoKey, TYPE_ASCII, 1, citation);
	  free (citation);
	  break;
	default:
	  assert (FALSE);	/* Shouldn't be here.  */
	}

	/* Set the citation key.  */
	citation = MALLOC ((max_citation_length + 1) * sizeof (char));
	citation_length
	  = snprintf (citation, max_citation_length + 1,
                      "Polar stereographic projected GeoTIFF using ");
	switch ( ellipsoid ) {
	case CLARKE1866:
	  citation_length
	    += snprintf (citation + citation_length,
			 max_citation_length - citation_length + 1,
			 "CLARKE1866");
	  assert (citation_length >= 0
		  && citation_length <= max_citation_length);
	  break;
	case GEM10C:
	  citation_length
	    += snprintf (citation + citation_length,
			 max_citation_length - citation_length + 1,
			 "GEM10C ");
	  assert (citation_length >= 0
		  && citation_length <= max_citation_length);
	  break;
	case WGS84:
	  citation_length
	    += snprintf (citation + citation_length,
			 max_citation_length - citation_length + 1,
			 "WGS84 ");
	  assert (citation_length >= 0
		  && citation_length <= max_citation_length);
	  break;
	case USER_DEFINED:
	  citation_length
	    += snprintf (citation + citation_length,
			 max_citation_length - citation_length + 1,
			 "user defined ");
	  assert (citation_length >= 0
		  && citation_length <= max_citation_length);
	  break;
	default:
	  assert (FALSE);	/* Shouldn't be here.  */
	}
	citation_length
	  += snprintf (citation + citation_length,
		       max_citation_length - citation_length + 1,
		       "ellipsoid datum written by Alaska Satellite Facility "
		       "tools");
	assert (citation_length >= 0
		&& citation_length <= max_citation_length);
	GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
	free (citation);

	break;
      default:
	assert (FALSE);		/* Shouldn't be here.  */
    }
  }

  /* FIXME: this is a terrible hack to deal with scansar crap.  */
  else if ( md->sar->image_type == 'G' 
	    || (md->sar->image_type == 'P' 
		&& md->projection->type == SCANSAR_PROJECTION) ) {
    GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeGeographic);

    /*    if ( ellipsoid == WGS84 ) {*/
      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_WGS84);
      /*    }
       *    else {
       *      ** User defined geographic coordinate system.  **
       *      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
       *		  user_defined_value_code);
       *      switch ( ellipsoid ) {
       *      case CLARKE1866:
       *	GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
       *		    DatumE_Clarke1866);
       *	break;
       *      case GEM10C:
       *	GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
       *		    DatumE_GEM10C);
       *	break;
       *      case WGS66:
       *	** Set to a newrby available ellipsoid.  We have far worse
       *	   problems than the ellipsoid being a bit wrong.  **
       *	GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
       *		    DatumE_WGS84);
       *	break;
       *      case WGS84:
       *	** Shouldn't be here (this should have been handled using the
       *	   non-user defined GeographicTypeGeoKey).  **
       *	assert (FALSE);
       *	break;
       *      default:
       *	assert (FALSE);		** Shouldn't be here.  **
       *      }
       *    }
       */
    GTIFKeySet (ogtif, GeogPrimeMeridianGeoKey, TYPE_SHORT, 1, PM_Greenwich);
    GTIFKeySet (ogtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
    {
      /* Tie points for image corners.  There is space for four tie
	 points, each consisting of three raster coordinates, followed
	 by three geospatial coordinates.  */
      double tie_points[4][6];

      /* Get the lat/longs of three image corners.  */
      double c1_lat, c1_long, c2_lat, c2_long, c3_lat, c3_long;
      meta_get_latLon (md, 0, 0, 0, &c1_lat, &c1_long);
      meta_get_latLon (md, 0, md->general->sample_count, 0, &c2_lat, &c2_long);
      meta_get_latLon (md, md->general->line_count, 0, 0, &c3_lat, &c3_long);

      /* Put three tie points in the image, as described in 2.6.2 of the
	 geotiff spec..  */
      tie_points[0][0] = 0.0;
      tie_points[0][1] = 0.0;
      tie_points[0][2] = 0.0;
      tie_points[0][3] = c1_lat;
      tie_points[0][4] = c1_long;
      tie_points[0][5] = 0.0;
      tie_points[1][0] = 0.0;
      tie_points[1][1] = md->general->sample_count;
      tie_points[1][2] = 0.0;
      tie_points[1][4] = c2_lat;
      tie_points[1][5] = c2_long;
      tie_points[1][6] = 0.0;
      tie_points[2][0] = md->general->line_count;
      tie_points[2][1] = 0.0;
      tie_points[2][2] = 0.0;
      tie_points[2][4] = c3_lat;
      tie_points[2][5] = c3_long;
      tie_points[2][6] = 0.0;

      /* Write the eighteen values that make up the three tie
	 points.  */
      TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 18, tie_points);
    }
  }

  else if ( md->sar->image_type == 'S' ) {
    /* Slant range image conversion not implemented yet.  */
    assert (FALSE);
  }

  else {
    /* Shouldn't be here (unrecognized image type). */
    assert (FALSE);
  }

  /* Write the actual image data.  */
  for ( ii = 0 ; ii < line_count ; ii++ ) {
    if ( TIFFWriteScanline (otif, daf + sample_count * ii, ii, 0) < 0 ) {
	  char* temp;
	  sprintf(temp, "Error writing to output geotiff file %s", output_file_name);
	  print_error(temp);
      exit (EXIT_FAILURE);
    }
  }

  return_code = GTIFWriteKeys (ogtif);
  assert (return_code);

  GTIFFree (ogtif);
  XTIFFClose (otif);
  free (daf);
  meta_free (md);
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
