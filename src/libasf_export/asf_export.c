#include <asf.h>
#include <asf_export.h>
#include <asf_vector.h>


int asf_export(output_format_t format, scale_t sample_mapping,
               char *in_base_name, char *output_name)
{
  return asf_export_bands(format, sample_mapping, 0, 0, 0, 0, 0, NULL,
			  in_base_name, output_name, NULL);
}

int asf_export_with_lut(output_format_t format, scale_t sample_mapping,
			char *lutFile, char *inFile, char *outFile)
{
  return asf_export_bands(format, sample_mapping, 1, 0, 0, 0, 0,
			  lutFile, inFile, outFile, NULL);
}


int asf_export_bands(output_format_t format, scale_t sample_mapping, int rgb,
                     int true_color, int false_color, int pauli, int sinclair,
                     char *look_up_table_name, char *in_base_name,
                     char *output_name, char **band_name)
{
  char *in_meta_name=NULL, *in_data_name=NULL, *out_name=NULL;

  asfPrintStatus("Exporting ...\n\n");

  // Do that exporting magic!
  if ( format == ENVI ) {
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      out_name = appendExt(output_name, ".envi");
      export_as_envi (in_meta_name, in_data_name, out_name);
  }
  else if ( format == ESRI ) {
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      out_name = appendExt(output_name, ".esri");
      export_as_esri (in_meta_name, in_data_name, out_name);
  }
  else if ( format == TIF ) {
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+6));
      append_ext_if_needed (output_name, ".tif", ".tiff");
      export_band_image(in_meta_name, in_data_name, output_name,
		      sample_mapping, band_name, rgb,
              true_color, false_color, pauli, sinclair,
		      look_up_table_name, TIF);
  }
  else if ( format == GEOTIFF ) {
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+6));
      append_ext_if_needed (output_name, ".tif", ".tiff");
      export_band_image(in_meta_name, in_data_name, output_name,
		      sample_mapping, band_name, rgb,
              true_color, false_color, pauli, sinclair,
		      look_up_table_name, GEOTIFF);
  }
  else if ( format == JPEG ) {
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+6));
      append_ext_if_needed (output_name, ".jpg", ".jpeg");
      export_band_image(in_meta_name, in_data_name, output_name,
		      sample_mapping, band_name, rgb,
              true_color, false_color, pauli, sinclair,
              look_up_table_name, JPEG);
  }
  else if ( format == PNG ) {
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      out_name = appendExt(output_name, ".png");
      export_band_image(in_meta_name, in_data_name, output_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color, pauli, sinclair,
                        look_up_table_name, PNG);
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
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      out_name = appendExt(output_name, ".pgm");
      export_band_image(in_meta_name, in_data_name, output_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color, pauli, sinclair,
                        look_up_table_name, PGM);
  }
  else if ( format == KML ) {
      in_data_name = appendExt(in_base_name, ".img");
      in_meta_name = appendExt(in_base_name, ".meta");
      write_kml_overlay (in_data_name);
  }

  FREE(in_data_name);
  FREE(in_meta_name);
  FREE(out_name);

  asfPrintStatus("Export successful!\n\n");
  return (EXIT_SUCCESS);
}
