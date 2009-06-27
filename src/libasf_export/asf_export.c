#include <asf.h>
#include <asf_export.h>
#include <asf_vector.h>


int asf_export(output_format_t format, scale_t sample_mapping,
               char *in_base_name, char *output_name)
{
  return asf_export_bands(format, sample_mapping, 0, 0, 0, NULL,
              in_base_name, output_name, NULL, NULL, NULL);
}

int asf_export_with_lut(output_format_t format, scale_t sample_mapping,
            char *lutFile, char *inFile, char *outFile)
{
  return asf_export_bands(format, sample_mapping, 1, 0, 0,
              lutFile, inFile, outFile, NULL, NULL, NULL);
}


int asf_export_bands(output_format_t format, scale_t sample_mapping, int rgb,
                     int true_color, int false_color,
                     char *look_up_table_name, char *in_base_name,
                     char *output_name, char **band_name,
                     int *noutputs, char ***output_names)
{
  char *in_meta_name=NULL, *in_data_name=NULL, *out_name=NULL;

  asfPrintStatus("Exporting ...\n\n");

  int i, nouts = 0;
  char **outs = NULL;

  in_data_name = appendExt(in_base_name, ".img");
  in_meta_name = appendExt(in_base_name, ".meta");
  meta_parameters *md = meta_read(in_meta_name);
  int is_polsarpro = (md->general->image_data_type == POLARIMETRIC_SEGMENTATION) ? 1 : 0;
  meta_free(md);

  // Do that exporting magic!
  if ( format == ENVI ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = appendExt(output_name, ".bsq");
      export_as_envi (in_meta_name, in_data_name, out_name);
  }
  else if ( format == ESRI ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = appendExt(output_name, ".esri");
      export_as_esri (in_meta_name, in_data_name, out_name);
  }
  else if ( format == TIF ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      append_ext_if_needed(out_name, ".tif", ".tiff");
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, TIF,
                        &nouts, &outs);
  }
  else if ( format == GEOTIFF ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      append_ext_if_needed(out_name, ".tif", ".tiff");
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, GEOTIFF,
                        &nouts, &outs);
  }
  else if ( format == JPEG ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      append_ext_if_needed(out_name, ".jpg", ".jpeg");
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, JPEG,
                        &nouts, &outs);
  }
  else if ( format == PNG ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      append_ext_if_needed(out_name, ".png", NULL);
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, PNG,
                        &nouts, &outs);
  }
  else if ( format == PGM ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      if (rgb || true_color || false_color || is_polsarpro) {
          asfPrintWarning(
            "Greyscale PGM output is not compatible with color options:\n"
            "(RGB, True Color, False Color, color look-up tables, PolSARpro\n"
            "classifications, etc)  ...\n"
            "Defaulting to producing separate greyscale PGM files for "
            "available bands.\n");
        rgb = 0;
        true_color = 0;
        false_color = 0;
      }
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      append_ext_if_needed(out_name, ".pgm", NULL);
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, PGM,
                        &nouts, &outs);
  }

  if (noutputs && output_names) {
    asfPrintStatus("\n\nExport complete.\nGenerated %d output file%s:\n",
                 nouts, nouts==1?"":"s");
    for (i=0; i<nouts; ++i)
      asfPrintStatus("  %s\n", outs[i]);
    asfPrintStatus("\n");

    *noutputs = nouts;
    *output_names = outs;
  }
  else {
    for (i=0; i<nouts; ++i)
      FREE(outs[i]);
    FREE(outs);
  }

  FREE(in_data_name);
  FREE(in_meta_name);
  FREE(out_name);

  asfPrintStatus("Export successful!\n\n");
  return (EXIT_SUCCESS);
}
