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
#include "uint8_image.h"
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
  handle_common_asf_args(&argc, &argv, "flip");

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

  char *input_meta_name = appendExt(argv[2], ".meta");
  char *input_data_name = appendExt(argv[2], ".img");

  char *output_meta_name = appendExt(argv[3], ".meta");
  char *output_data_name = appendExt(argv[3], ".img");

  printf("Flipping image %s.\n",
	 vert && horz ? "vertically and horizontally" :
	 (vert ? "vertically" : "horizontally"));

  printf("Input data file: %s\n", input_data_name);
  printf("Output data file: %s\n", output_data_name);

  meta_parameters *imd = meta_read(input_meta_name);
  meta_write(imd, output_meta_name);

  FloatImage *finput = NULL;
  UInt8Image *binput = NULL;
  if (imd->optical || imd->general->data_type == BYTE) {
    binput = uint8_image_new_from_file(imd->general->sample_count,
                                       imd->general->line_count,
                                       input_data_name, 0);
  }
  else {
    finput = float_image_new_from_file(imd->general->sample_count,
                                       imd->general->line_count,
                                       input_data_name, 0,
                                       FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  }

  if (imd->optical || imd->general->data_type == BYTE) {
    if (horz)
      uint8_image_flip_x(binput);

    if (vert)
      uint8_image_flip_y(binput);

    uint8_image_store(binput, output_data_name);
    uint8_image_free(binput);
  }
  else {
    if (horz)
      float_image_flip_x(finput);

    if (vert)
      float_image_flip_y(finput);

    float_image_store(finput, output_data_name,
                      FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
    float_image_free(finput);
  }

  meta_free(imd);

  free(input_data_name);
  free(input_meta_name);
  free(output_data_name);
  free(output_meta_name);

  return EXIT_SUCCESS;
}
