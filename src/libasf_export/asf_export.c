#include <asf.h>
#include <asf_export.h>
#include <asf_vector.h>


int asf_export(output_format_t format, long size, scale_t sample_mapping, 
               char *in_base_name, char *output_name)
{
  char **in_base_names;

  in_base_names = (char **) MALLOC(MAX_BANDS*sizeof(char *));
  in_base_names[0] = (char *) MALLOC(512*sizeof(char));
  strcpy(in_base_names[0], in_base_name);

  return asf_export_bands(format, size, sample_mapping, 
			  in_base_names, output_name, 0);
}

int asf_export_bands(output_format_t format, long size, scale_t sample_mapping, 
		     char **in_base_names, char *output_name, int rgb)
{
  int ii;
  char in_meta_name[255], in_data_name[255];
  char *out_name = (char*)MALLOC(512*sizeof(char));

  asfPrintStatus("Exporting ...\n");

  // Do that exporting magic!
  if ( format == ENVI ) {
    ii = 0;
    while (in_base_names[ii] != NULL) {
      // no multi-band output for ENVI format
      sprintf(in_data_name, "%s.img", in_base_names[ii]);
      sprintf(in_meta_name, "%s.meta", in_base_names[ii]);
      sprintf(out_name, "%s.envi", output_name);
      export_as_envi (in_meta_name, in_data_name, out_name);
      ii++;
    }
  }
  else if ( format == ESRI ) {
    ii = 0;
    while (in_base_names[ii]) {
      // no multi-band output for ESRI format
      sprintf(in_data_name, "%s.img", in_base_names[ii]);
      sprintf(in_meta_name, "%s.meta", in_base_names[ii]);
      sprintf(out_name, "%s.esri", output_name);
      export_as_esri (in_meta_name, in_data_name, out_name);
      ii++;
    }
  }
  else if ( format == TIF ) {
    ii = 0;
    while (in_base_names[ii]) {
      // temporary fix
      sprintf(in_data_name, "%s.img", in_base_names[ii]);
      sprintf(in_meta_name, "%s.meta", in_base_names[ii]);
      append_ext_if_needed (out_name, ".tif", ".tiff");
      export_as_tiff (in_meta_name, in_data_name, out_name, size, 
		      sample_mapping);
      ii++;
    }
  }
  else if ( format == GEOTIFF ) {
    append_ext_if_needed (output_name, ".tif", ".tiff");
    if (rgb) // three-channel RGB image
      export_rgb_as_geotiff (in_base_names, output_name, size, sample_mapping, 1);
    else // writing as many single bands as you can find
      export_rgb_as_geotiff (in_base_names, output_name, size, sample_mapping, 0);
  }
  else if ( format == JPEG ) {
    ii = 0;
    while (in_base_names[ii]) {
      // temporary fix
      sprintf(in_data_name, "%s.img", in_base_names[ii]);
      sprintf(in_meta_name, "%s.meta", in_base_names[ii]);
      append_ext_if_needed (out_name, ".jpg", ".jpeg");
      export_as_jpeg (in_meta_name, in_data_name, out_name, size,
		      sample_mapping);
      ii++;
    }
  }
  else if ( format == PPM ) {
    ii = 0;
    while (in_base_names[ii]) {
      // no multi-band output for PPM format
      sprintf(in_data_name, "%s.img", in_base_names[ii]);
      sprintf(in_meta_name, "%s.meta", in_base_names[ii]);
      sprintf(out_name, "%s.ppm", output_name);
      export_as_ppm (in_meta_name, in_data_name, out_name, size,
		     sample_mapping);
      ii++;
    }
  }
  else if ( format == KML ) {
    ii = 0;
    while (in_base_names[ii]) {
      // no multi-band output for KML format
      sprintf(in_data_name, "%s.img", in_base_names[ii]);
      sprintf(in_meta_name, "%s.meta", in_base_names[ii]);
      write_kml_overlay (in_data_name);
      ii++;
    }
  }

  asfPrintStatus("Export successful!\n\n");
  return (EXIT_SUCCESS);
}
