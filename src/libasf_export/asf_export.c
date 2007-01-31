#include <asf.h>
#include <asf_export.h>
#include <asf_vector.h>


int asf_export(output_format_t format, scale_t sample_mapping, 
               char *in_base_name, char *output_name)
{
  return asf_export_bands(format, sample_mapping, 
			  in_base_name, output_name, NULL);
}

int asf_export_bands(output_format_t format, scale_t sample_mapping, 
		     char *in_base_name, char *output_name, char **band_name)
{
  char in_meta_name[255], in_data_name[255];
  char *out_name = (char*)MALLOC(512*sizeof(char));
  int size = -1;

  asfPrintStatus("Exporting ...\n\n");

  // Do that exporting magic!
  if ( format == ENVI ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    sprintf(out_name, "%s.envi", output_name);
    export_as_envi (in_meta_name, in_data_name, out_name);
  }
  else if ( format == ESRI ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    sprintf(out_name, "%s.esri", output_name);
    export_as_esri (in_meta_name, in_data_name, out_name);
  }
  else if ( format == TIF ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    append_ext_if_needed (out_name, ".tif", ".tiff");
    export_as_tiff (in_meta_name, in_data_name, out_name, size, 
		      sample_mapping); 
  }
  else if ( format == GEOTIFF ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    append_ext_if_needed (output_name, ".tif", ".tiff");
    export_rgb_as_geotiff (in_meta_name, in_data_name, output_name, 
			   sample_mapping, band_name);
  }
  else if ( format == JPEG ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    append_ext_if_needed (out_name, ".jpg", ".jpeg");
    export_as_jpeg (in_meta_name, in_data_name, out_name, size,
		    sample_mapping);
  }
  else if ( format == PPM ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    sprintf(out_name, "%s.ppm", output_name);
    export_as_ppm (in_meta_name, in_data_name, out_name, size,
		   sample_mapping);
  }
  else if ( format == KML ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    write_kml_overlay (in_data_name);
  }

  asfPrintStatus("Export successful!\n\n");
  return (EXIT_SUCCESS);
}
