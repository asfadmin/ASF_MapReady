/*******************************************************************************
<documentation>
<name>
   asf_export
</name>

<synopsis>
   asf_export [ -f <format> ] [ -s <size> ] [ -o <outFile> ] <inFile>
</synopsis>

<description>
   This program ingests ASF internal format data and exports said data to
   a number of output formats. If the input data was geocoded and the ouput
   format supports geocoding, that information will be included.
</description>

<input>
   This must be an ASF internal format data file.
</input>

<output>
   The converted data in the output file. This file can be specified explicitly
   with the -o option, or the base name from the input file will be used with an
   appropriate extension.
</output>

<options>
   -f             Format to export to. Must be one of the following:
                      CEOS, envi, esri, geotiff, jpeg, png, ppm
   -s             Scale image so that its largest dimension is, at most, size.
   -o             Name of the output file.
</options>

<examples>
   To export to the default geotiff format from file1:
      asf_export file1
   To file1 export to the jpeg format:
      asf_export -f jpeg file1
   To export file1 to a jpeg no larger than 800x800:
      asf_export -f jpeg -s 800 file1
</examples>

<limitations>
   Currently only supports ingest of ASF format floating point data.
   Geotiffs will not be scaled.
</limitations>

<see_also>
   asf_convert, asf_import
</see_also>

<copyright>
*******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
*******************************************************************************
</copyright>
</documentation>

PROGRAM HISTORY:
	VERS:   DATE:   AUTHOR:
	----------------------------------------------------------------------
	?

*******************************************************************************/

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
#include <png/png.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>

#define MICRON 0.00000001
#define FLOAT_EQUIVALENT(a, b) (fabs(a - b) < MICRON ? 1 : 0)

static char *program_name = "asf_export";

/* Print invocation information.  */
void 
usage (char *program_name)
{
  printf ("\nUsage:\n"
	  "%s: [-f FORMAT] [-s size] [-o OUTPUT_FILE] INPUT_FILE\n", 
	  program_name);
}

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

/* Structure to hold elements of the command line.  */
typedef struct {
  /* Output format to use.  */
  char format[MAX_FORMAT_STRING_LENGTH];
  /* Maximum size of largest output dimension, in pixels, or
     NO_MAXIMUM_OUTPUT_SIZE.  The output image will be scaled to honor
     this value, if its maximum dimension is not already less than
     this.  */
  long size;			
  /* Name or base name of input file(s) to use.  */
  char input_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  /* Output name to use.  */
  char output_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  int verbose;			/* Flag true iff in verbose mode.  */
} command_line_parameters_t;

typedef enum {
  ENVI,				/* ENVI software package.  */
  ESRI,				/* ESRI GIS package.  */
  GEOTIFF,			/* Geotiff.  */
  JPEG,				/* Joint Photographic Experts Group.  */
  PNG,				/* Portable Network Graphics.  */
  PPM				/* Portable PixMap.  */
} output_format_t;

/* We don't have strncpy everywhere, so here is a substitude.  */
static char *
my_strncpy (char *dest, const char *src, size_t n)
{
  size_t ii;
  for ( ii = 0 ; ii < n ; ii++ ) {
    dest[ii] = src[ii];
    if ( dest[ii] == '\0' ) {
      return dest;
    }
  }
  
  return dest;
}

/* We don't have strnlen everywhere, so here is a substitute.  */
static size_t
my_strnlen (const char *s, size_t max_len)
{
  size_t ii;
  for ( ii = 0 ; ii < max_len ; ii++ ) {
    if ( s[ii] == '\0' ) {
      return ii;
    }
  }
  
  return max_len;
}

/* Some forward declarations.  */
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
export_as_png (const char *metadata_file_name,
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
  /* Command line input goes in it's own structure.  */
  command_line_parameters_t command_line;

  /* Defaults for options.  These should all be considered immutable
     after they are set here.  */
  char const_default_format[MAX_FORMAT_STRING_LENGTH + 1];
  /* Produce geotiff output by default.  */
  my_strncpy (const_default_format, "geotiff", 
	      (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
  const long default_size = NO_MAXIMUM_OUTPUT_SIZE;
  /* By default, construct the output base name from the input base
     name.  */

  /* Options are initialized with their default values.  */
  my_strncpy (command_line.format, const_default_format,
	      (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
  command_line.size = default_size;
  /* If the output_name is still an empty string after option
     processing, we will conclude that it needs to be formed from the
     input string.  */
  command_line.output_name[0] = '\0';

  const int num_args = 1;
  while ( currArg < (argc - num_args) ) {
    char *key = argv[currArg++];
    if ( strmatch (key, "-f") ) {
      CHECK_ARG (1);		/* One string argument: format string.  */
      strcpy (command_line.format, GET_ARG (1));
      if ( !((strcmp (command_line.format, "CEOS") == 0) 
	     || (strcmp (command_line.format, "envi") == 0) 
	     || (strcmp (command_line.format, "esri") == 0)
	     || (strcmp (command_line.format, "geotiff") == 0)
	     || (strcmp (command_line.format, "jpeg") == 0)
	     || (strcmp (command_line.format, "png") == 0)
	     || (strcmp (command_line.format, "ppm") == 0)) ) {
	fprintf (stderr, "%s: bad format (-f argument): %s\n", program_name, 
		 command_line.format);
	usage (program_name);
	exit (EXIT_FAILURE);
      }
    }
    else if ( strmatch (key, "-s") ) {
      char *endptr;
      int base = 10;		/* Size is a base 10 integer.  */
      CHECK_ARG (1);
      command_line.size = strtol (GET_ARG (1), &endptr, base);
      for ( ; *endptr != '\0' ; endptr++ ) {
	if ( !isspace (*endptr) ) {
	  fprintf (stderr, "%s: bad size (-s argument): %s\n", program_name, 
		   GET_ARG (1));
	  usage (program_name);
	  exit (EXIT_FAILURE);
	}
      }
    }
    else if ( strmatch (key, "-o") ) {
      CHECK_ARG (1);
      strcpy (command_line.output_name, GET_ARG (1));
    }
  }		

  output_format_t format;
  if ( strcmp (command_line.format, "envi") == 0 ) {
    format = ENVI;
  }
  else if ( strcmp (command_line.format, "esri") == 0 ) {
    format = ESRI;
  }
  else if ( strcmp (command_line.format, "geotiff") == 0 ) {
    format = GEOTIFF;
  }
  else if ( strcmp (command_line.format, "jpeg") == 0 ) {
    format = JPEG;
  }
  else if ( strcmp (command_line.format, "png") == 0 ) {
    format = PNG;
  }
  else if ( strcmp (command_line.format, "ppm") == 0 ) {
    format = PPM;
  }
  else {
    assert (FALSE);		/* Not implemented yet.  */
  }

  /* The only argument is the name of the input image.  This may be a
     base name, or include either the .meta or .img extensions,
     correct names for both constituent parts will then be deduced
     automaticly.  We don't validate these much, since opening them is
     the first thing this program attempts.  */
  if ( argc - currArg != num_args ) {
    fprintf (stderr, "%s: wrong number of arguments\n", program_name);
    usage (program_name);
    exit (EXIT_FAILURE);
  }
  if ( my_strnlen (argv[currArg], MAX_IMAGE_NAME_LENGTH + 1)
       > MAX_IMAGE_NAME_LENGTH ) {
    fprintf (stderr, "%s: input image name argument too long\n", program_name);
    exit (EXIT_FAILURE);
  }
  my_strncpy (command_line.input_name, argv[currArg], 
	      MAX_IMAGE_NAME_LENGTH + 1);

  /* Construct the actual file names from the names the user supplied.  */
  char image_data_file_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char metadata_file_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  strcpy (image_data_file_name, command_line.input_name);
  if ( findExt (command_line.input_name) 
       && strcmp (findExt (command_line.input_name), ".img") ) {
    strcpy (image_data_file_name, command_line.input_name);
    create_name (metadata_file_name, command_line.input_name, ".meta");
  } else if ( findExt (command_line.input_name) 
	      && strcmp (findExt (command_line.input_name), ".meta") ) {
    create_name (image_data_file_name, command_line.input_name, ".img");
    strcpy (metadata_file_name, command_line.input_name);
  } else {
    create_name (metadata_file_name, command_line.input_name, ".meta");
    create_name (image_data_file_name, command_line.input_name, ".img");
  }

  /* If we didn't get an output file option, we construct the default
     output name from the input name.  */
  if ( command_line.output_name[0] == '\0' ) {
    if ( format == GEOTIFF ) {
      create_name (command_line.output_name, command_line.input_name, 
		   ".geotiff");
    } 
    else if ( format == JPEG ) {
      create_name (command_line.output_name, command_line.input_name, ".jpeg");
    }
    else if ( format == PNG ) {
      create_name (command_line.output_name, command_line.input_name, ".png");
    }
    else if ( format == PPM ) {
      create_name (command_line.output_name, command_line.input_name, ".ppm");
    }
    else {
      assert (FALSE);		/* Not implemented yet.  */
    }
  }

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  meta_parameters *md = meta_read (metadata_file_name);
  assert (md->general->data_type == BYTE
	  || md->general->data_type == INTEGER16
	  || md->general->data_type == INTEGER32
	  || md->general->data_type == REAL32
	  || md->general->data_type == REAL64);
  meta_free (md);

  if ( format == ENVI ) {
    export_as_envi (metadata_file_name, image_data_file_name, 
		    command_line.output_name);
  }
  else if ( format == ESRI ) {
    export_as_esri (metadata_file_name, image_data_file_name,
		    command_line.output_name);    
  }
  else if ( format == GEOTIFF ) {
    export_as_geotiff (metadata_file_name, image_data_file_name,
		       command_line.output_name);
  } 
  else if ( format == JPEG ) {
    export_as_jpeg (metadata_file_name, image_data_file_name,
		    command_line.output_name, command_line.size);
  } 
  else if ( format == PNG ) {
    export_as_png (metadata_file_name, image_data_file_name,
		   command_line.output_name, command_line.size);
  }
  else if ( format == PPM ) {
    export_as_ppm (metadata_file_name, image_data_file_name,
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

static void *
get_image_data (meta_parameters *metadata, const char *image_data_file_name)
{
  size_t sample_size = get_sample_size (metadata);

  /* Read the image data itself.  */
  FILE *ifp = fopen (image_data_file_name, "r");
  if ( ifp == NULL ) {
    fprintf (stderr, "%s: failed to open %s: %s\n", program_name, 
	     image_data_file_name, strerror (errno));
    exit (EXIT_FAILURE);
  }
  /* Total number of samples in image.  */
  size_t pixel_count
    = metadata->general->line_count * metadata->general->sample_count;
  void *data = malloc (pixel_count * sample_size);
  size_t read_count = fread (data, sample_size, pixel_count, ifp);
  if ( read_count != pixel_count ) {
    if ( feof (ifp) ) {
      fprintf (stderr, "%s: read wrong amount of data from %s\n", program_name,
	       image_data_file_name);
    }
    else if ( ferror (ifp) ) {
      fprintf (stderr, "%s: read of file %s failed: %s\n", program_name, 
	       image_data_file_name, strerror (errno));
    }
    else {
      assert (FALSE);		/* Shouldn't be here.  */
    }
    exit (EXIT_FAILURE);
  }
  int return_code = fclose (ifp);
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
  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  assert (md->general->data_type == BYTE
	  || md->general->data_type == INTEGER16
	  || md->general->data_type == INTEGER32
	  || md->general->data_type == REAL32
	  || md->general->data_type == REAL64);

  char envi_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  create_name (envi_file_name, output_file_name, ".hdr");
  envi_header *envi = meta2envi (md);

  /* Write ENVI header file */
  FILE *fp = FOPEN(envi_file_name, "w");
  time_t time;
  char t_stamp[15];
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

  char envi_data_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  strcpy (envi_data_file_name, output_file_name);
  strcat (envi_data_file_name, ".bil");
  char command[10000];
  sprintf (command, "cp %s %s\n", image_data_file_name, envi_data_file_name); 
  int return_code = system (command);
  if ( return_code != 0 ) {
    fprintf (stderr, "%s: system command '%s' failed", program_name, command);
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
  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  assert (md->general->data_type == BYTE
	  || md->general->data_type == INTEGER16
	  || md->general->data_type == INTEGER32
	  || md->general->data_type == REAL32
	  || md->general->data_type == REAL64);

  char esri_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  create_name (esri_file_name, output_file_name, ".hdr");
  esri_header *esri = meta2esri (md);

  /* Write ESRI header file */
  FILE *fp = FOPEN(esri_file_name, "w");
  time_t time;
  char t_stamp[15];
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
  char esri_prj_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  char projection[100], datum[100], spheroid_str[100]="", semimajor[25]="";
  char flattening[25];  
  double central_meridian, latitude_of_origin, standard_parallel_1, standard_parallel_2;
  spheroid_type_t spheroid;
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
  char esri_data_file_name[2 * MAX_IMAGE_NAME_LENGTH];
  strcpy (esri_data_file_name, output_file_name);
  strcat (esri_data_file_name, ".bil");
  char command[10000];
  sprintf (command, "cp %s %s\n", image_data_file_name, esri_data_file_name); 
  int return_code = system (command);
  if ( return_code != 0 ) {
    fprintf (stderr, "%s: system command '%s' failed", program_name, command);
    exit (EXIT_FAILURE);
  }
}

static unsigned char
averaging_kernel (gsl_matrix_uchar *img, int kernel_size, size_t i, size_t j)
{
  int i_idx, j_idx;
  int i_min = i - kernel_size / 2;
  int i_max = i + kernel_size / 2;
  int j_min = j - kernel_size / 2;
  int j_max = j + kernel_size / 2;
    
  int sum = 0;

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

  int average = sum / pow (kernel_size, 2); /* Truncated average.  */
  /* Since we are averaging unsigned char values, this should always
     be true.  */
  assert (average <= UCHAR_MAX);

  return average;
}

/* Average together tiles of kernel_size pixels in the *width x
   *height image at pixels, reducing its size by a factor of about
   kernel size in each dimension.  The new image replaces the old in
   pixels (extra memory is freed) and a pointer to the possibly
   relocated pixel data is returned.  The new image width and height
   replace the input width and height arguments.  */
static unsigned char *
average_unsigned_char_pixels (unsigned char *pixels, unsigned long *width, 
			      unsigned long *height, int kernel_size)
{
  assert (kernel_size % 2 != 0); /* Odd-sized kernels only.  */

  /* Input width and height.  */
  size_t iwidth = *width, iheight = *height;

  /* Make a matrix form of the input image for easy indexing.  */
  gsl_matrix_uchar *iimg = gsl_matrix_uchar_alloc (iheight, iwidth);
  size_t ii, jj;
  for ( ii = 0 ; ii < iheight ; ii++ ) {
    for ( jj = 0 ; jj < iwidth ; jj++ ) {
      gsl_matrix_uchar_set (iimg, ii, jj, pixels[ii * iwidth + jj]);
    }
  }

  /* Dimensions of the averaged image.  */
  size_t owidth = ceil (iwidth / kernel_size);
  size_t oheight = ceil (iheight / kernel_size);

  /* Form the output image.  */
  gsl_matrix_uchar *oimg = gsl_matrix_uchar_alloc (oheight, owidth);
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
  unsigned char *reallocated_pixels = realloc (pixels, owidth * oheight);

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
  assert (md->general->data_type == REAL32);

  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  /* Maximum large dimension to allow in the output.  */
  unsigned long max_large_dimension;
  if ( (max_size > line_count && max_size > sample_count) 
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    max_large_dimension = GSL_MAX (line_count, sample_count);
  } 
  else {
    max_large_dimension = max_size;
  }

  size_t pixel_count = (size_t) line_count * sample_count;

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  float *daf = get_image_data (md, image_data_file_name);
  /* It supposed to be big endian data, this converts to host byte
     order.  */
  int jj;
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* We need a version of the data in JSAMPLE form, so we have to
     form a scaled version of the input data.  */
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  assert (sizeof (unsigned char) == 1);
  assert (sizeof (unsigned char) == sizeof (JSAMPLE));
  JSAMPLE test_jsample = 0;
  test_jsample--;
  assert (test_jsample == UCHAR_MAX); /* Did we wrap?  */
  /* This space is resized later (with realloc) if the image is
     scaled.  */
  unsigned char *pixels = scale_floats_to_unsigned_bytes (daf, pixel_count);

  /* We want to scale the image st the long dimesion is less than or
     equal to this value the prescribed maximum.  */
  /* Current size of the image.  */
  unsigned long width = sample_count, height = line_count;
  /* Scale the image, modifying width and height to reflect the new
     image size.  */
  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
						 &width, &height);

  /* Initializae libjpg structures.  */
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_compress (&cinfo);

  /* Open the output file to be used.  */
  FILE *ofp = fopen (output_file_name, "w");
  if ( ofp == NULL ) {
    fprintf (stderr, "%s: open of %s for writing failed: %s\n", program_name,
  	     output_file_name, strerror (errno));
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
    JSAMPROW *row_pointer = malloc (rows_to_write * sizeof (JSAMPROW));
    row_pointer[0] = &(pixels[cinfo.next_scanline * width]);
    int rows_written = jpeg_write_scanlines (&cinfo, row_pointer, 
  					     rows_to_write);
    assert (rows_written == rows_to_write);
  }

  /* Finsh compression and close the jpeg.  */
  jpeg_finish_compress (&cinfo);
  int return_code = fclose (ofp);
  assert (return_code == 0);
  jpeg_destroy_compress (&cinfo);

  free (pixels);
  free (daf);
  meta_free (md);
}

void
export_as_png (const char *metadata_file_name,
	       const char *image_data_file_name, const char *output_file_name,
	       long max_size)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);

  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  /* Maximum large dimension to allow in the output.  */
  unsigned long max_large_dimension;
  if ( (max_size > line_count && max_size > sample_count) 
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    max_large_dimension = GSL_MAX (line_count, sample_count);
  } 
  else {
    max_large_dimension = max_size;
  }

  size_t pixel_count = (size_t) line_count * sample_count;

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  float *daf = get_image_data (md, image_data_file_name);
  /* It supposed to be big endian data, this converts to host byte
     order.  */
  int jj;
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* This space is resized later (with realloc) if the image is
     scaled.  */
  unsigned char *pixels = scale_floats_to_unsigned_bytes (daf, pixel_count);

  /* We want to scale the image st the long dimesion is less than or
     equal to this value the prescribed maximum.  */
  /* Current size of the image.  */
  unsigned long width = sample_count, height = line_count;
  /* Scale the image, modifying width and height to reflect the new
     image size.  */
  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
						 &width, &height);

  /* Open the output file to be used.  */
  FILE *ofp = fopen (output_file_name, "w");
  if ( ofp == NULL ) {
    fprintf (stderr, "%s: open of %s for writing failed: %s\n", program_name,
	     output_file_name, strerror (errno));
    exit (EXIT_FAILURE);
  }

  /* Initialize libpng structures and error handling.  */
  png_structp png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, 
						 (png_voidp) NULL, NULL, NULL);
  assert (png_ptr != NULL);
  png_infop info_ptr = png_create_info_struct (png_ptr);
  assert (png_ptr != NULL);
  if ( setjmp (png_jmpbuf (png_ptr)) ) {
    fprintf (stderr, "%s: some libpng call failed somehow or other\n", 
	     program_name);
    exit (EXIT_FAILURE);
  }
  
  /* Write the png.  */
  png_init_io (png_ptr, ofp);
  const int bit_depth = 8;		/* We are writing a byte image.  */
  png_set_IHDR (png_ptr, info_ptr, width, height, bit_depth, 
		PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE, 
		PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
  const int text_fields_count = 1; /* We write only one field of text.  */
  /* FIXME: confirm: I guess we have to malloc our own png_text blocks?  */
  png_text *text_ptr = malloc (text_fields_count * sizeof (png_text));
  text_ptr[0].key = "Description";
  text_ptr[0].text = "Alaska Satellite Facility Image";
  text_ptr[0].compression = PNG_TEXT_COMPRESSION_NONE;
  png_set_text (png_ptr, info_ptr, text_ptr, 1);
  png_write_info (png_ptr, info_ptr);
  png_uint_32 ii;
  png_bytep row_pointers[height];
  for ( ii = 0 ; ii < height ; ii++ ) {
    const int bytes_per_pixel = 1; /* We are writing a byte image.  */
    row_pointers[ii] = pixels + ii * width * bytes_per_pixel;
  }
  png_write_image (png_ptr, row_pointers);
  png_write_end (png_ptr, info_ptr);

  png_destroy_write_struct (&png_ptr, &info_ptr);

  int return_code = fclose (ofp); /* Close png file.  */
  assert (return_code == 0);

  free (pixels);
  free (daf);
  meta_free (md);

}

void
export_as_ppm (const char *metadata_file_name, 
	       const char *image_data_file_name, const char *output_file_name,
	       long max_size)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  assert (md->general->data_type == REAL32);

  /* Get image dimensions.  */
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  /* Maximum large dimension to allow in the output.  */
  unsigned long max_large_dimension;
  if ( (max_size > line_count && max_size > sample_count) 
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    max_large_dimension = GSL_MAX (line_count, sample_count);
  } 
  else {
    max_large_dimension = max_size;
  }

  size_t pixel_count = (size_t) line_count * sample_count;

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  float *daf = get_image_data (md, image_data_file_name);
  /* Input is supposed to be big endian data, this converts to host
     byte order.  */
  int jj;
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
  unsigned char *pixels = scale_floats_to_unsigned_bytes (daf, pixel_count);

  /* We want to scale the image st the long dimension is less than or
     equal to the prescribed maximum.  */
  /* Current size of the image.  */
  unsigned long width = sample_count, height = line_count;
  /* Scale the image, modifying width and height to reflect the new
     image size.  */
  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
						 &width, &height);

  /* Open the output file to be used.  */
  FILE *ofp = fopen (output_file_name, "w");
  if ( ofp == NULL ) {
    fprintf (stderr, "%s: open of %s for writing failed: %s\n", program_name,
  	     output_file_name, strerror (errno));
    exit (EXIT_FAILURE);
  }

  /* Write the ppm header.  */
  const char *ppm_magic_number = "P6";
  int print_count = fprintf (ofp, "P6"); 
  /* After this we will assume that writing to the new file will work
     correctly.  */
  assert (print_count == strlen (ppm_magic_number));
  fprintf (ofp, "\n");
  fprintf (ofp, "%ld\n", width);
  fprintf (ofp, "%ld\n", height);
  const int max_color_value = 255;
  fprintf (ofp, "%d\n", max_color_value);

  /* Write the pixels themselves.  */
  size_t ii;
  for ( ii = 0 ; ii < height ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < width ; jj++ ) {
      /* Write red, green, and blue the same to get grey scale.  */
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
    }
  }

  int return_code = fclose (ofp);
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
  assert (md->general->data_type == REAL32);
  uint16_t sample_size = 4;

  /* Get image dimensions. */
  uint32_t line_count = md->general->line_count;
  uint32_t sample_count = md->general->sample_count;

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  float *daf = get_image_data (md, image_data_file_name);

  /* Open output tiff file and GeoKey file descriptor.  */
  TIFF *otif = XTIFFOpen (output_file_name, "w");
  assert (otif != NULL);
  GTIF *ogtif = GTIFNew (otif);
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
  uint16_t sample_format;
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
 
  /* Set the GeoTiff extension image tags.  */

  /* If we have a map projected image, write the projection
     information into the GeoTiff.  */
  if ( md->sar->image_type == 'P' ) {
    /* FIXME: Set these.  */
    double tie_points[4][6];
    /* Some applications (e.g., ArcView) won't handle geoTIFF images
       with more than one tie point pair.  Therefore, only the upper
       left corner is being written to the geoTIFF file.  In order to
       write all computed tie points to the geoTIFF, change the 6 to
       size in the line below.  */
    TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 6, tie_points);

    /* Set the scale of the pixels, in projection coordinates.  */
    double pixel_scale[3];
    pixel_scale[0] = md->projection->perX;
    pixel_scale[1] = md->projection->perY;
    pixel_scale[2] = 0;
    TIFFSetField(otif, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);

    /* This is the geotiff code values which means 'meters'.  */
    const unsigned short meters_units_code = 9001;

    switch ( md->projection->type ) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      GTIFKeySet (ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
      GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);

      /* This weird assertion is because I remember once when we
         couln't figure out how to set some datum code right, we set it
         to -1.  */
      assert (md->projection->param.utm.zone != -1);
      GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 
		  md->projection->param.utm.zone);
      assert (FALSE);		/* Unfinished.  */
      GTIFKeySet(ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, 
		 meters_units_code);
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 1,
		 "UTM projected Geotiff written by Alaska Satellite Facility "
		 "tools");
      break;
    case POLAR_STEREOGRAPHIC:
      GTIFKeySet (ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
      GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
      /* This constant is from the geotiff spec.  */
      const int user_defined_projected_coordinate_system_type_code = 32767;
      GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 
		  user_defined_projected_coordinate_system_type_code);
      /* This constant is from the geotiff spec.  */
      const int user_defined_projection_geo_key_type_code = 32767;
      GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
		  user_defined_projection_geo_key_type_code); 
      GTIFKeySet(ogtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, 
		 meters_units_code);
      /* This is longitude which will point toward the top of the map.
	 This is often called the reference of central longitude in
	 the context of polar stereo map projections.  */
      GTIFKeySet (ogtif, ProjStraightVertPoleLongGeoKey, TYPE_DOUBLE, 1, 
		  md->projection->param.ps.slon);
      /* Set the latitude of true scale, or reference latitude.  */
      GTIFKeySet (ogtif, ProjOriginLatGeoKey, TYPE_DOUBLE, 1,
		  md->projection->param.ps.slat);
      /* Set the false easting and false northing to zero.  */
      GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0);
      GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0);
      GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1,
		 "Polar stereographic projected Geotiff written by Alaska "
		 "Satellite Facility tools");
      break;
    default:
      assert (FALSE);		/* Shouldn't be here.  */
    }
  }

  /* Write the actual image data.  */
  size_t ii;
  for ( ii = 0 ; ii < line_count ; ii++ ) {
    if ( TIFFWriteScanline (otif, daf + sample_count * ii, ii, 0) < 0 ) {
      fprintf (stderr, "%s: error writing to output geotiff file %s\n", 
	       program_name, output_file_name);
      exit (EXIT_FAILURE);
    }
  }

  int return_code = GTIFWriteKeys (ogtif);
  assert (return_code);

  GTIFFree (ogtif);
  XTIFFClose (otif);
  free (daf);
  meta_free (md);
}
