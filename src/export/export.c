/* Export an image from the ASF tools internal format to one of a variety
   of supported external formats.  */

#include <assert.h>
#include <errno.h>
#include <getopt.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <jpeglib.h>
#include "geo_config.h"
#include "geokeys.h"

static char *program_name = "export";
static char *program_version = "0.1.0";

/* Print invocation information.  */
void 
usage (void)
{
  printf ("\nUsage:\n"
	  "%s: [-f FORMAT] [-o OUTPUT_FILE] INPUT_FILE", program_name);
}

/* Maximum image name length we can accept from the user.  Since the
   user may enter a base name only, the actual file name strings need
   to be slightly longer, to accomodate extensions.  */
#define MAX_IMAGE_NAME_LENGTH 240
/* Maximum extension length that will ever be automaticly added to a
   file name specified by the user.  */
#define MAX_EXTENSION_LENGTH 10
/* Magic string pointer value, see its usage context to understand.  */
#define SAME_AS_INPUT_BASE_NAME NULL
/* Maximum length allowed for arguments to format (-f or --format)
   options.  */
#define MAX_FORMAT_STRING_LENGTH 50

/* Structure to hold elements of the command line.  */
typedef struct {
  char format[MAX_FORMAT_STRING_LENGTH];
  char input_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char output_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
} command_line_parameters_t;

typedef enum {
  GEOTIFF
} output_format_t;

int 
main (int argc, char *argv[])
{
  /* Command line input goes in it's own structure.  */
  command_line_parameters_t command_line;

  /* getopt() related variables.  */
  int optc;
  /* The --version flag is handled a bit differenty.  If we see it
     getopt_long stops processing options and sets this flag, which we
     must then check when we exit the getopt loop.  */
  int version_flag = 0;
  struct option long_options[] 
    = {
        /* Interesting options.  */
        {"output", required_argument, NULL, 'o'}, /* Output file name.  */
        {"format", required_argument, NULL, 'f'}, /* Output format to use.  */

	/* Standart options.  */
	{"verbose", no_argument, NULL, 'v'},
	{"version", no_argument, &version_flag, 1},
	{"help", no_argument, NULL, '?'},
	{0, 0, 0, 0}		/* Sentinel for end of option description.  */
      };

  /* Defaults for options.  These should all be considered immutable
     after they are set here.  */
  char const_default_format[MAX_FORMAT_STRING_LENGTH + 1];
  /* Produce geotiff output by default.  */
  strncpy (const_default_format, "geotiff", 
	   (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
  char const_default_output_file[MAX_IMAGE_NAME_LENGTH + 1];
  /* By default, construct the output base name from the input base
     name.  */
  char *const const_default_output_file = SAME_AS_INPUT_BASE_NAME;
  const int default_verbosity = 0; /* Default is false.  */

  /* Options are initialized with their default values.  */
  strncpy (command_line.format, const_default_format,
	   (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
  command_line.output_name = const_default_output_file;
  command_line.verbose = default_verbosity;

  /* Parse command line.  */
  while ( (optc = getopt_long (argc, argv, "f:v?", long_options, NULL)) 
	  != EOF ) {
    switch (optc) {
    case 'f':
      if ( (strcmp (optarg, "CEOS") == 0) 
	   || (strcmp (optarg, "geotiff") == 0)
	   || (strcmp (optarg, "jpeg") == 0)
	   || (strcmp (optarg, "ppm") == 0) ) {
	strncpy (command_line.format, optarg, 
		 (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
      } else {
	fprintf (stderr, "%s: bad format (-f or --format argument): %s\n",
		 program_name, optarg);
	usage ();
	exit (EXIT_FAILURE);
      }
      break;
    case 'o';
      /* Ensure file name isn't too long.  */
      if ( strnlen(optarg, MAX_IMAGE_NAME_LENGTH + 1) 
	   > MAX_IMAGE_NAME_LENGTH ) {
        fprintf (stderr, 
		 "%s: output file name (-o or --output argument) too long\n", 
		 program_name);
	exit (EXIT_FAILURE);
      }
      /* Open the file to be sure we can, and to create a file system
         place holder for other programs to respect if they want to.  */
      FILE *output_file = fopen (optarg, "w");
      if ( output_file == NULL ) {
	fprintf (stderr, "%s: couldn't open file %s for writing\n", 
		 program_name, optarg);
	exit (EXIT_FAILURE);
      } else {
	int return_code = fclose (output_file);
	assert (return_code != 0);
	strcpy (command_line.output, optarg);
      }
    case 'v':
      command_line.verbose = 1;	/* Set flag true.  */
      break;
    case '?':
      usage ();
      exit (EXIT_SUCCESS);	/* usage() was called deliberately.  */
      break;
    case 0:
      /* In this case, we have had getopt_long use the alternate form
         of long option processing and a flag has been set.  We only
         do this for flags which cause option processing to stop.  */
      break;
    default:
      usage ();
      exit (EXIT_FAILURE);	/* usage() called due to user error.  */
      break;
    }
  }
  /* If getopt_long() saw the --version flag, version_flag got set.  */
  if ( version_flag ) {
    printf(
"%s version %s\n"
"\n"
"Copyright (C) 2004 Alaska Satellite Facility\n"
           , program_name, program_version);
    exit (EXIT_SUCCESS);
  }

  output_format_t format;
  if ( strcmp (command_line.format, "geotiff") == 0 ) {
    format = GEOTIFF;
  }
  else {
    assert (FALSE);		/* Not implemented yet.  */
  }

  /* If we didn't get an output file option, we construct the default
     output name from the input name.  */
  if ( command_line.output_name == SAME_AS_INPUT_HASE_NAME ) {
    if ( format == GEOTIFF ) {
      create_name (command_line.output_name, command_ilne.input_name, 
		   ".geotiff");
    } 
    else {
      assert (FALSE);		/* Not implemented yet.  */
    }
  }

  /* The only argument is the name of the input image.  This may be a
     base name, or include either the .meta or .img extensions,
     correct names for both constituent parts will then be deduced
     automaticly.  We don't validate these much, since opening them is
     the first thing this program attempts.  */
  if ( optind != argc - 1 ) {
    fprintf (stderr, "%s: wrong number of arguments\n", program_name);
    usage ();
    exit (EXIT_FAILURE);
  }
  if ( strnlen (argv[optind], MAX_IMAGE_NAME_LENGTH + 1)
       > MAX_IMAGE_NAME_LENGTH ) {
    fprintf (stderr, "%s: input image name argument too long\n", program_name);
    exit (EXIT_FAILURE);
  }
  strncpy (command_line.input_name, argv[optind], MAX_IMAGE_NAME_LENGTH + 1);

  /* Construct the actual file names from the names the user supplied.  */
  char image_data_file_name[MAX_FILE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char metadata_file_name[MAX_FILE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  strcpy (image_data_file_name, command_line.input_name);
  if ( extExists (command_line.input_name, ".img") ) {
    strcpy (image_data_file_name, command_line.input_name);
    create_name (metadata_file_name, command_line.input_name, ".meta");
  } else if ( extExists (command_line.input_name, ".meta") ) {
    create_name (image_data_file_name, command_line.input_name, ".img");
    strcpy (metadata_file_name, command_line.input_name);
  } else {
    create_name (metadata_file_name, command_line.input_name, ".meta");
    create_name (image_data_file_name, command_line.input_name, ".img");
  }

  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  assert (md->general->data_type == BYTE
	  || md->general->data_type == INTEGER16
	  || md->general->data_type == INTEGER32
	  || md->general->data_type == REAL32
	  || md->general->data_type == REAL64);

}

/* Get sample size in bytes of the data types represented by the
   meta_parameters_t.  */
static size_t
get_sample_size (meta_parameters_t *metadata)
{
  size_t sample_size;
  switch ( md->general->data_type ) {
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
}

static void *
get_image_data (meta_parameters_t *metadata, const char *image_data_file_name)
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
  long long int pixel_count 
    = md->general->line_count * md->general->sample_count;
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
export_as_png (const char *metadata_file_name,
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

  void *data = get_image_data (md, image_data_file_name);
  
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
  png_set_IHDR (png_ptr, info_ptr, md->general->line_count, 
		md->general->sample_count, bit_depth, PNG_COLOR_TYPE_GRAY,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, 
		PNG_FILTER_TYPE_DEFAULT);
  png_write_info (png_ptr, info_ptr);
  png_uint_32 k, height, width;
 
  assert (FALSE);		/* Not finished yet.  */
}

void
export_as_jpeg (const char *metadata_file_name,
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

  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  size_t pixel_count = (size_t) line_count * sample_count;

  /* Get the image data.  */
  void *data = get_image_data (md, image_data_file_name);

  /* We need a version of the data in JSAMPLE form, so we have to
     form a scaled version of the input data.  */
  /* Here is a very funky check to try to ensure that the JSAMPLE really
     is the type we expect, so we can scale properly.  */
  assert (sizeof (JSAMPLE) == 1);
  JSAMPLE test_jsample = 0;
  test_jsample--;
  assert (test_jsample == UCHAR_MAX); /* Did we wrap?  */
  JSAMPLE *pixels = malloc (pixel_count, sizeof (JSAMPLE));
  /* Bin the input pixels.  */
  assert (md->general->data_type == REAL32);
  float *daf = data;		/* Data as type float.  */
  /* Minimum and maximum values in the input data.  */
  float imin = gsl_stats_float_min (daf, 1, pixel_count);
  float imax = gsl_stats_float_max (daf, 1, pixel_count);
  /* Standard deviation of input data.  */
  float sdev = gsl_stats_float_sd (daf, 1, pixel_count);
  /* Minimum and maximum after clamping of 3 * sdev outliers.  */
  float omin = imin + 3 * sdev;
  float omax = imax - 3 * sdev;
  /* Shift we need to apply to the data to get it to fall in the
     range of the JSAMPLEs.  */
  float bias = -omin + 0.25;
  size_t ii;
  for ( ii = 0 ; ii < pixel_count ; ii++ ) {
    if ( dat[ii] < omin ) {
      pixels[ii] = 0;		/* Clamp low.  */
    }
    else if ( dat[ii] > omax ) {
      pixels[ii] == UCHAR_MAX;	/* Clamp high.  */
    }
    else {
      pixels[ii] = (daf[ii] + bias) * (UCHAR_MAX / (omax - o_min));
    }
  }

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
  cinfo.image_width = line_count;
  cinfo.image_height = sample_count;
  cinfo.input_components = 1;	/* Grey scale => 1 color component / pixel.  */
  cinfo.in_color_space = JCS_GRAYSCALE;
  jpeg_set_defaults (&cinfo);	/* Use default compression parameters.  */
  /* Reassure libjpeg that we will be writing a complete JPEG file.  */
  jpeg_start_compress (&cinfo, TRUE);

  /* Write the jpeg.  */
  while ( cinfo.next_scanline < cinfo.image_height ) {
    JSAMPROW *row_pointer = &(pixels[cinfo.next_scanline * sample_count]);
    jpeg_write_scanlines (&cinfo, row_pointer, 1);
  }

  /* Finsh compression and close the jpeg.  */
  jpeg_finish_compress (&cinfo);
  int return_code = fclose (ofp);
  assert (return_code == 0);
  jpeg_destroy_compress (&cinfo);

  free (pixels);
}

void 
export_as_geotiff (const char *metadata_file_name, 
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

  size_t sample_size = get_sample_size (md);
  void *data = get_image_data (md, image_data_file_name);

  /* Open output tiff file and GeoKey file descriptor.  */
  TIFF *otif = XTIFFOpen (output_file_name, "w");
  assert (otif != NULL);
  GIFF *ogtif = GTIFNew (otif);
  assert (ogtif != NULL);

  /* Set the normal TIFF image tags.  */
  TIFFSetField(out, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(out, TIFFTAG_IMAGEWIDTH, md->general->sample_count);
  TIFFSetField(out, TIFFTAG_IMAGELENGTH, md->general->line_count);
  TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, sample_size * 8);
  TIFFSetField(out, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,1);
  TIFFSetField(out, TIFFTAG_XRESOLUTION,1);
  TIFFSetField(out, TIFFTAG_YRESOLUTION,1);
  TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  int sample_format;
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
    sample_format = SAMPLEFORMAT_IEEEFP;
    break;
  default:
    assert (FALSE);		/* Shouldn't be here.  */
    break;
  }
  TIFFSetField(out, TIFFTAG_SAMPLEFORMAT, sample_format);
  TIFFSetField(out, TIFFTAG_DATATYPE, sample_format);
 
  /* Set the GeoTiff extension image tags.  */

  /* If we have a map projected image, write the projection
     information into the GeoTiff.  */
  if ( md->sar->image_type == 'P' ) {
    double tie_points[4][6];
    /* Some applications (e.g., ArcView) won't handle geoTIFF images
       with more than one tie point pair.  Therefore, only the upper
       left corner is being written to the geoTIFF file.  In order to
       write all computed tie points to the geoTIFF, change the 6 to
       size in the line below.  */
    TIFFSetField(out, TIFFTAG_GEOTIEPOINTS, 6, tie_points);

    /* Set the scale of the pixels, in projection coordinates.  */
    double pixel_scale[3];
    pixel_scale[0] = md->projection->perx;
    pixel_scale[1] = md->projection->pery;
    pixel_scale[2] = 0;
    TIFFSetField(out, TIFFTAG_GEOPIXELSCALE, 3, pixelscale);

    switch ( md->projection->type ) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      GTIFKeySet (ogtif, RTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
      GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
      /* This weird assertion is because I remember once when we
         couln't figure out how to set some datum code right, we set it
         to -1.  */
      assert (md->projection->param.proj_utm.zone != -1);
      GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 
		  md->projection->param.proj_utm.zone);
      assert (FALSE);		/* Unfinished.  */
       /* All our UTM projected images use meters in their
          coordinates, this is the geotiff code values which means
          'meters'.  */
      const unsigned short units_code = 9001;
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, units_code);
      GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 0,
		 "UTM converted from Alaska Satellite Facility image");
      break;
    default:
      assert (FALSE);		/* Shouldn't be here.  */
    }
  }
}






