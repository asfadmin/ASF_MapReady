#include <asf.h>
#include <asf_export.h>
#include <asf_vector.h>


int asf_export(output_format_t format, long size, scale_t sample_mapping, 
               char *in_base_name, char *output_name)
{
  char in_meta_name[255], in_data_name[255];
  char *out_name = (char*)MALLOC((strlen(output_name)+5)*sizeof(char));

  sprintf(in_data_name, "%s.img", in_base_name);
  sprintf(in_meta_name, "%s.meta", in_base_name);
  strcpy(out_name, output_name);

  asfPrintStatus("Exporting: %s\n", output_name);

  // Do that exporting magic!
  if ( format == ENVI ) {
    export_as_envi (in_meta_name, in_data_name, out_name);
  }
  else if ( format == ESRI ) {
    export_as_esri (in_meta_name, in_data_name, out_name);
  }
  else if ( format == TIF ) {
    append_ext_if_needed (out_name, ".tif", ".tiff");
    export_as_tiff (in_meta_name, in_data_name, out_name, size, 
		    sample_mapping);
  }
  else if ( format == GEOTIFF ) {
    append_ext_if_needed (out_name, ".tif", ".tiff");
    export_as_geotiff (in_meta_name, in_data_name, out_name, size,
                       sample_mapping);
  }
  else if ( format == JPEG ) {
    append_ext_if_needed (out_name, ".jpg", ".jpeg");
    export_as_jpeg (in_meta_name, in_data_name, out_name, size,
                    sample_mapping);
  }
  else if ( format == PPM ) {
    append_ext_if_needed (out_name, ".ppm", NULL);
    export_as_ppm (in_meta_name, in_data_name, out_name, size,
                   sample_mapping);
  }
  else if ( format == KML ) {
    append_ext_if_needed (out_name, ".kml", NULL);
    write_kml_overlay (in_data_name);
  }

  asfPrintStatus("Export successful!\n");
  return (EXIT_SUCCESS);
}
