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
#include <asf_reporting.h>
#include "float_image.h"
#include <libasf_proj.h>
#include <spheroids.h>
#include <asf_contact.h>

int main(int argc, char *argv[])
{
  if (argc != 3) {
    printf("Usage: flipv <input basename> <output basename>\n");
    exit(1);
  }

  char input_meta_name[256], input_data_name[256];
  char output_meta_name[256], output_data_name[256];

  sprintf(input_meta_name, "%s.meta", argv[1]);
  sprintf(input_data_name, "%s.img", argv[1]);

  sprintf(output_meta_name, "%s.meta", argv[2]);
  sprintf(output_data_name, "%s.img", argv[2]);

  printf("Input data file: %s\n", input_data_name);
  printf("Output data file: %s\n", output_data_name);

  meta_parameters *imd = meta_read(input_meta_name);
  meta_write(imd, output_meta_name);

  FloatImage *input =
    float_image_new_from_file(imd->general->sample_count,
			      imd->general->line_count,
			      input_data_name, 0,
			      FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  float_image_flip_y(input);

  float_image_store(input, output_data_name,
		    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  meta_free(imd);
}
