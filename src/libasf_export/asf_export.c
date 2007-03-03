#include <asf.h>
#include <asf_export.h>
#include <asf_vector.h>


int asf_export(output_format_t format, scale_t sample_mapping,
               char *in_base_name, char *output_name)
{
  return asf_export_bands(format, sample_mapping, 0, 0, 0, 0, 0, NULL,
			  in_base_name, output_name, NULL);
}

int asf_export_bands(output_format_t format, scale_t sample_mapping, int rgb,
                     int true_color, int false_color, int pauli, int sinclair,
                     char *look_up_table_name, char *in_base_name,
                     char *output_name, char **band_name)
{
  char in_meta_name[255], in_data_name[255];
  char *out_name = (char*)MALLOC(512*sizeof(char));

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
    append_ext_if_needed (output_name, ".tif", ".tiff");
    export_band_image(in_meta_name, in_data_name, output_name,
		      sample_mapping, band_name, rgb,
                      true_color, false_color, pauli, sinclair,
		      look_up_table_name, TIF);
  }
  else if ( format == GEOTIFF ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    append_ext_if_needed (output_name, ".tif", ".tiff");
    export_band_image(in_meta_name, in_data_name, output_name,
		      sample_mapping, band_name, rgb,
                      true_color, false_color, pauli, sinclair,
		      look_up_table_name, GEOTIFF);
  }
  else if ( format == JPEG ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    append_ext_if_needed (output_name, ".jpg", ".jpeg");
    export_band_image(in_meta_name, in_data_name, output_name,
		      sample_mapping, band_name, rgb,
                      true_color, false_color, pauli, sinclair,
                      look_up_table_name, JPEG);
  }
  else if ( format == PGM ) {
    if (rgb || true_color || false_color || pauli || sinclair) {
        asfPrintWarning(
          "Greyscale PGM output is not compatible with color options:\n"
          "(RGB, True Color, False Color, Pauli, or Sinclair)  ...\n"
          "Defaulting to producing separate greyscale PGM files for "
          "available bands.\n");
      rgb = 0;
      true_color = 0;
      false_color = 0;
      pauli = 0;
      sinclair = 0;
    }
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    append_ext_if_needed (output_name, ".pgm", ".pgm");
    export_band_image(in_meta_name, in_data_name, output_name,
		      sample_mapping, band_name, rgb,
                      true_color, false_color, pauli, sinclair,
                      look_up_table_name, PGM);
  }
  else if ( format == KML ) {
    sprintf(in_data_name, "%s.img", in_base_name);
    sprintf(in_meta_name, "%s.meta", in_base_name);
    write_kml_overlay (in_data_name);
  }

  asfPrintStatus("Export successful!\n\n");
  return (EXIT_SUCCESS);
}
