/* Export an image from the ASF tools internal format to one of a variety
   of supported external formats.  */

#include <assert.h>
#include <errno.h>
#include <getopt.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include <envi.h>
#include <esri.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_statistics.h>
#include <jpeglib.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>

static char *program_name = "export";
static char *program_version = "0.1.0";

/* Print invocation information.  */
static void 
my_usage (void)
{
  printf ("\nUsage:\n"
	  "%s: [-f FORMAT] [-o OUTPUT_FILE] INPUT_FILE\n", program_name);
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

/* Structure to hold elements of the command line.  */
typedef struct {
  char format[MAX_FORMAT_STRING_LENGTH];
  char input_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char output_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  int verbose;			/* Flag true iff in verbose mode.  */
} command_line_parameters_t;

typedef enum {
  ENVI,
  ESRI,
  GEOTIFF,
  JPEG
} output_format_t;

/* We dn't have strncpy everywhere, so here is a substitude.  */
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
		const char *image_data_file_name,
		const char *output_file_name);

/* Main program body. */
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
        {"format", required_argument, NULL, 'f'}, /* Output format to use.  */
        {"output", required_argument, NULL, 'o'}, /* Output file name.  */

	/* Standard options.  */
	{"verbose", no_argument, NULL, 'v'},
	{"version", no_argument, &version_flag, 1},
	{"help", no_argument, NULL, '?'},
	{0, 0, 0, 0}		/* Sentinel for end of option description.  */
      };

  /* Defaults for options.  These should all be considered immutable
     after they are set here.  */
  char const_default_format[MAX_FORMAT_STRING_LENGTH + 1];
  /* Produce geotiff output by default.  */
  my_strncpy (const_default_format, "geotiff", 
	      (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
  /* By default, construct the output base name from the input base
     name.  */
  const int default_verbosity = 0; /* Default is false.  */

  /* Options are initialized with their default values.  */
  my_strncpy (command_line.format, const_default_format,
	      (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
  /* If the output_name is still an empty string after option
     processing, we will conclude that it needs to be formed from the
     input string.  */
  command_line.output_name[0] = '\0';
  command_line.verbose = default_verbosity;

  /* Parse command line.  */
  while ( (optc = getopt_long (argc, argv, "f:o:v?", long_options, NULL)) 
	  != EOF ) {
    switch (optc) {
    case 'f':
      if ( (strcmp (optarg, "CEOS") == 0) 
	   || (strcmp (optarg, "envi") == 0) 
	   || (strcmp (optarg, "esri") == 0)
	   || (strcmp (optarg, "geotiff") == 0)
	   || (strcmp (optarg, "jpeg") == 0)
	   || (strcmp (optarg, "ppm") == 0) ) {
	my_strncpy (command_line.format, optarg, 
		    (size_t) (MAX_FORMAT_STRING_LENGTH + 1));
      } else {
	fprintf (stderr, "%s: bad format (-f or --format argument): %s\n",
		 program_name, optarg);
	my_usage ();
	exit (EXIT_FAILURE);
      }
      break;
    case 'o':
      /* Ensure file name isn't too long.  */
      if ( my_strnlen(optarg, MAX_IMAGE_NAME_LENGTH + 1) 
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
	assert (return_code == 0);
	strcpy (command_line.output_name, optarg);
      }
      break;
    case 'v':
      command_line.verbose = 1;	/* Set flag true.  */
      break;
    case '?':
      my_usage ();
      exit (EXIT_SUCCESS);	/* my_usage() was called deliberately.  */
      break;
    case 0:
      /* In this case, we have had getopt_long use the alternate form
         of long option processing and a flag has been set.  We only
         do this for flags which cause option processing to stop.  */
      break;
    default:
      my_usage ();
      exit (EXIT_FAILURE);	/* my_usage() called due to user error.  */
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
  else {
    assert (FALSE);		/* Not implemented yet.  */
  }

  /* The only argument is the name of the input image.  This may be a
     base name, or include either the .meta or .img extensions,
     correct names for both constituent parts will then be deduced
     automaticly.  We don't validate these much, since opening them is
     the first thing this program attempts.  */
  if ( optind != argc - 1 ) {
    fprintf (stderr, "%s: wrong number of arguments\n", program_name);
    my_usage ();
    exit (EXIT_FAILURE);
  }
  if ( my_strnlen (argv[optind], MAX_IMAGE_NAME_LENGTH + 1)
       > MAX_IMAGE_NAME_LENGTH ) {
    fprintf (stderr, "%s: input image name argument too long\n", program_name);
    exit (EXIT_FAILURE);
  }
  my_strncpy (command_line.input_name, argv[optind], 
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
    else {
      assert (FALSE);		/* Not implemented yet.  */
    }
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

  if ( format == ENVI ) {
    export_as_envi (metadata_file_name, image_data_file_name, 
		    command_line.output_name);
  }
  if ( format == ESRI ) {
    export_as_esri (metadata_file_name, image_data_file_name,
		    command_line.output_name);    
  }
  if ( format == GEOTIFF ) {
    export_as_geotiff (metadata_file_name, image_data_file_name,
		       command_line.output_name);
  } else if ( format == JPEG ) {
    export_as_jpeg (metadata_file_name, image_data_file_name,
		    command_line.output_name);
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

//void
//export_as_png (const char *metadata_file_name,
//	       const char *image_data_file_name,
//	       const char *output_file_name)
//{
//  /* Get the image metadata.  */
//  meta_parameters *md = meta_read (metadata_file_name);
//
//  /* Complex data generally can't be output into meaningful images, so
//     we refuse to deal with it.  */
//  assert (md->general->data_type == BYTE
//	  || md->general->data_type == INTEGER16
//	  || md->general->data_type == INTEGER32
//	  || md->general->data_type == REAL32
//	  || md->general->data_type == REAL64);
//
//  void *data = get_image_data (md, image_data_file_name);
//  
//  /* Open the output file to be used.  */
//  FILE *ofp = fopen (output_file_name, "w");
//  if ( ofp == NULL ) {
//    fprintf (stderr, "%s: open of %s for writing failed: %s\n", program_name,
//	     output_file_name, strerror (errno));
//    exit (EXIT_FAILURE);
//  }
//
//  /* Initialize libpng structures and error handling.  */
//  png_structp png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, 
//						 (png_voidp) NULL, NULL, NULL);
//  assert (png_ptr != NULL);
//  png_infop info_ptr = png_create_info_struct (png_ptr);
//  assert (png_ptr != NULL);
//  if ( setjmp (png_jmpbuf (png_ptr)) ) {
//    fprintf (stderr, "%s: some libpng call failed somehow or other\n", 
//	     program_name);
//    exit (EXIT_FAILURE);
//  }
//
//  /* Write the png.  */
//  png_init_io (png_ptr, ofp);
//  png_set_IHDR (png_ptr, info_ptr, md->general->line_count, 
//		md->general->sample_count, bit_depth, PNG_COLOR_TYPE_GRAY,
//		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, 
//		PNG_FILTER_TYPE_DEFAULT);
//  png_write_info (png_ptr, info_ptr);
//  png_uint_32 k, height, width;
// 
//  assert (FALSE);		/* Not finished yet.  */
//}

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
    switch (md->projection->type)
      {
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
      case SCANSAR_PROJECTION: break;
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
  meta_free(md);

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
  int j_min = i - kernel_size / 2;
  int j_max = i + kernel_size / 2;
    
  int sum = 0;

  for ( i_idx = i_min ; i_idx < i_max ; i_idx++ ) {
    for ( j_idx = j_min ; j_idx < j_max ; j_idx++ ) {
      /* The i and j indicies to use, adjusted in case we are off the
	 edge of the image.  These choices implement a kernel that
	 pretends that a mirror image of the image exists at the image
	 edges (and corners). */      
      int itu = i_idx, jtu = j_idx;
      if ( itu < 0 ) 
	itu = -itu - 1;
      else if ( itu >= img->size1 ) 
	itu = img->size1 - (itu - img->size1) - 1;
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
  gsl_matrix_uchar *oimg = gsl_matrix_uchar_alloc (owidth, oheight);
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
  assert (sizeof (unsigned char) == 1);
  unsigned char test_jsample = 0;
  test_jsample--;
  assert (test_jsample == UCHAR_MAX); /* Did we wrap?  */
  /* This space is resized later (with realloc) if the image is
     scaled.  */
  unsigned char *pixels = malloc (pixel_count * sizeof (unsigned char));
  /* Bin the input pixels.  */
  assert (md->general->data_type == REAL32);
  float *daf = data;		/* Data as type float.  */

  int jj;
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* Minimum and maximum values in the input data.  */
  float imin = gsl_stats_float_min (daf, 1, pixel_count);
  float imax = gsl_stats_float_max (daf, 1, pixel_count);
  float imean = gsl_stats_float_mean (daf, 1, pixel_count);
  /* Standard deviation of input data.  */
  float isdev = gsl_stats_float_sd (daf, 1, pixel_count);
  /* Minimum and maximum after clamping.  */
  float omin = GSL_MAX (imean - 3 * isdev, imin);
  float omax = GSL_MIN (imean + 3 * isdev, imax);
  /* Shift we need to apply to the data to get it to fall in the
     range of the JSAMPLEs.  */
  float bias = -omin + 0.25;
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

  /* Width and height of the jpeg we intend to create.  These values
     get modified if we decide we want to scale the image.  */
  unsigned long width = sample_count, height = line_count;

  /* We want to scale the image st that long dimesion is less than or
     equal to this value.  */
  const unsigned long max_large_dimension = 2000;
  /* This assertion is pretty obvious, but since the algorithm needs
     it to work correctly, its included.  */
  assert (max_large_dimension > 1);
  if ( GSL_MAX (width, height) > max_large_dimension ) {
    int kernel_size = GSL_MAX (width, height) / max_large_dimension + 1;
    if ( kernel_size % 2 != 1 ) {
      kernel_size++;
    }
    pixels = average_unsigned_char_pixels (pixels, &width, &height, 
					   kernel_size);
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
    row_pointer[0] = &(pixels[cinfo.next_scanline * sample_count]);
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
  free (data);
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
  float *daf = get_image_data (md, image_data_file_name);
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;

  /* Open output tiff file and GeoKey file descriptor.  */
  TIFF *otif = XTIFFOpen (output_file_name, "w");
  assert (otif != NULL);
  GTIF *ogtif = GTIFNew (otif);
  assert (ogtif != NULL);

  /* Set the normal TIFF image tags.  */
  TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, md->general->sample_count);
  TIFFSetField(otif, TIFFTAG_IMAGELENGTH, md->general->line_count);
  TIFFSetField(otif, TIFFTAG_BITSPERSAMPLE, sample_size * 8);
  TIFFSetField(otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(otif, TIFFTAG_ROWSPERSTRIP,1);
  TIFFSetField(otif, TIFFTAG_XRESOLUTION,1);
  TIFFSetField(otif, TIFFTAG_YRESOLUTION,1);
  TIFFSetField(otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
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
  TIFFSetField(otif, TIFFTAG_SAMPLEFORMAT, sample_format);
  TIFFSetField(otif, TIFFTAG_DATATYPE, sample_format);
 
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
    TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 6, tie_points);

    /* Set the scale of the pixels, in projection coordinates.  */
    double pixel_scale[3];
    pixel_scale[0] = md->projection->perX;
    pixel_scale[1] = md->projection->perY;
    pixel_scale[2] = 0;
    TIFFSetField(otif, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);

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
       /* All our UTM projected images use meters in their
          coordinates, this is the geotiff code values which means
          'meters'.  */
      const unsigned short units_code = 9001;
      GTIFKeySet(ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, units_code);
      GTIFKeySet(ogtif, PCSCitationGeoKey, TYPE_ASCII, 0,
		 "UTM converted from Alaska Satellite Facility image");
      break;
    default:
      assert (FALSE);		/* Shouldn't be here.  */
    }
  }

  /* Write the actual image data.  */
  size_t ii;
  for ( ii = 0 ; ii < line_count ; ii++ ) {
    if ( TIFFWriteScanline (otif, daf + sample_count * ii, ii, 0) < 0 ) {
      XTIFFClose (otif);
      fprintf (stderr, "%s: error writing to output geotiff file %s\n", 
	       program_name, output_file_name);
    }
  }

  meta_free (md);
}
