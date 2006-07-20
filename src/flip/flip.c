// Standard libraries.
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Libraries from packages outside ASF.
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_statistics_double.h>

// Libraries developed at ASF.
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include "float_image.h"
#include <libasf_proj.h>
#include <spheroids.h>
#include <asf_contact.h>

void usage()
{
    printf("Usage: flip [v|h|vh] <input basename> <output basename>\n");
    exit(1);
}

int main(int argc, char *argv[])
{
  if (argc != 4) {
    usage();
  }

  if ( ! (strcmp(argv[1], "v") == 0 ||
	  strcmp(argv[1], "h") == 0 ||
	  strcmp(argv[1], "hv") == 0 ||
	  strcmp(argv[1], "vh") == 0))
  {
    usage();
  }

  int vert = strchr(argv[1], 'v') != NULL;
  int horz = strchr(argv[1], 'h') != NULL;

  if (!vert && !horz) {
    usage();
  }

  char input_meta_name[256], input_data_name[256];
  char output_meta_name[256], output_data_name[256];

  sprintf(input_meta_name, "%s.meta", argv[2]);
  sprintf(input_data_name, "%s.img", argv[2]);

  sprintf(output_meta_name, "%s.meta", argv[3]);
  sprintf(output_data_name, "%s.img", argv[3]);

  printf("Flipping image %s.\n", 
	 vert && horz ? "vertically and horizontally" :
	 (vert ? "vertically" : "horizontally"));

  printf("Input data file: %s\n", input_data_name);
  printf("Output data file: %s\n", output_data_name);

  meta_parameters *imd = meta_read(input_meta_name);
  meta_write(imd, output_meta_name);

  FloatImage *input =
    float_image_new_from_file(imd->general->sample_count,
			      imd->general->line_count,
			      input_data_name, 0,
			      FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  if (horz)
    float_image_flip_x(input);

  if (vert)
    float_image_flip_y(input);

  float_image_store(input, output_data_name,
		    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  meta_free(imd);
}
