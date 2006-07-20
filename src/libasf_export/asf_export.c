#include <asf_contact.h>
#include <asf_license.h>
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
#include <proj_api.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>
#include <asf_reporting.h>

int asf_export(output_format_t format, long size, scale_t sample_mapping, 
	       char *in_base_name, char *output_name)
{
  char in_meta_name[255], in_data_name[255];

  sprintf(in_data_name, "%s.img", in_base_name);
  sprintf(in_meta_name, "%s.meta", in_base_name);

  // Do that exporting magic!
  if ( format == ENVI ) {
    export_as_envi (in_meta_name, in_data_name, output_name);
  }
  else if ( format == ESRI ) {
    export_as_esri (in_meta_name, in_data_name, output_name);
  }
  else if ( format == TIF ) {
    export_as_tiff (in_meta_name, in_data_name, output_name, size, 
		    sample_mapping);
  }
  else if ( format == GEOTIFF ) {
    export_as_geotiff (in_meta_name, in_data_name, output_name, size,
                       sample_mapping);
  }
  else if ( format == JPEG ) {
    export_as_jpeg (in_meta_name, in_data_name, output_name, size,
                    sample_mapping);
  }
  else if ( format == PPM ) {
    export_as_ppm (in_meta_name, in_data_name, output_name, size,
                   sample_mapping);
  }

  return (EXIT_SUCCESS);
}
