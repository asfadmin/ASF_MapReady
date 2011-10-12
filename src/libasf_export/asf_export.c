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

  asfPrintStatus("Exporting ...\n");

  int i, nouts = 0;
  char **outs = NULL;

  in_data_name = appendExt(in_base_name, ".img");
  in_meta_name = appendExt(in_base_name, ".meta");
  meta_parameters *md = meta_read(in_meta_name);
  int is_polsarpro = 
    (md->general->image_data_type == POLARIMETRIC_SEGMENTATION) ? 1 : 0;
  int is_matrix =
    ((md->general->image_data_type >= POLARIMETRIC_C2_MATRIX &&
      md->general->image_data_type <= POLARIMETRIC_STOKES_MATRIX) ||
     md->general->image_data_type == POLARIMETRIC_DECOMPOSITION) ? 1 : 0;
  if (md->general->image_data_type == RGB_STACK)
    rgb = TRUE;

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
      if (!is_matrix)
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
      if (!is_matrix)
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
      if (!is_matrix)
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
      if (!is_matrix)
	append_ext_if_needed(out_name, ".png", NULL);
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, PNG,
                        &nouts, &outs);
  }
  else if ( format == PNG_ALPHA ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      if (!is_matrix)
	append_ext_if_needed(out_name, ".png", NULL);
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, PNG_ALPHA,
                        &nouts, &outs);
  }
  else if ( format == PNG_GE ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      if (!is_matrix)
	append_ext_if_needed(out_name, ".png", NULL);
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, PNG_GE,
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
      if (!is_matrix)
	append_ext_if_needed(out_name, ".pgm", NULL);
      export_band_image(in_meta_name, in_data_name, out_name,
                        sample_mapping, band_name, rgb,
                        true_color, false_color,
                        look_up_table_name, PGM,
                        &nouts, &outs);
  }
  else if ( format == POLSARPRO_HDR ) {
      //in_data_name = appendExt(in_base_name, ".img");
      //in_meta_name = appendExt(in_base_name, ".meta");
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      export_band_image(in_meta_name, in_data_name, out_name,
			NONE, band_name, rgb,
			true_color, false_color,
			look_up_table_name, POLSARPRO_HDR,
			&nouts, &outs);
  }
  else if ( format == HDF ) {
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      export_band_image(in_meta_name, in_data_name, out_name,
			NONE, band_name, rgb,
			true_color, false_color,
			look_up_table_name, HDF,
			&nouts, &outs);
  }
  else if ( format == NC ) {
      out_name = MALLOC(sizeof(char)*(strlen(output_name)+32));
      strcpy(out_name, output_name);
      export_band_image(in_meta_name, in_data_name, out_name,
			NONE, band_name, rgb,
			true_color, false_color,
			look_up_table_name, NC,
			&nouts, &outs);
  }

  if (should_write_insar_rgb(md->general->bands)) {
      write_insar_rgb(format, in_meta_name, in_data_name, out_name);
  }

  if (should_write_insar_xml_meta(md)) {
    char *xml_meta = get_insar_xml_string(md, FALSE);
      char *xml_output_file_name = 
          (char *) MALLOC(sizeof(char)*(strlen(out_name)+10));
      sprintf(xml_output_file_name, "%s.xml", stripExt(out_name));

      write_insar_xml_to_file(xml_output_file_name, xml_meta);
      FREE(xml_meta);
      FREE(xml_output_file_name);
  }
  else if (should_write_dem_xml_meta(md)) {
    char *xml_meta = get_dem_xml_string(md, FALSE);
    char *xml_output_file_name =
      (char *) MALLOC(sizeof(char)*(strlen(out_name)+10));
    sprintf(xml_output_file_name, "%s.xml", stripExt(out_name));
    write_dem_xml_to_file(xml_output_file_name, xml_meta);
    FREE(xml_meta);
    FREE(xml_output_file_name);
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
  meta_free(md);

  asfPrintStatus("Export successful!\n\n");
  return (EXIT_SUCCESS);
}

int
should_write_insar_xml_meta(meta_parameters *md) {
    return ( NULL != md->insar);
}

int
should_write_insar_rgb(char *band_name) {
    return ( NULL != strstr_case(band_name, "INTERFEROGRAM_PHASE") );
}

int should_write_dem_xml_meta(meta_parameters *md)
{
  return (NULL != md->dem);
}

/**
 * preconditions:
 *  * in_meta_name exists on filesystem and ends with .meta extension
 *  * interferogram.lut is available in the mapready share directory
 */
void 
write_insar_rgb(output_format_t format, char *in_meta_name, char *in_data_name, char *out_name)
{
    int ii, nouts = 0;
    char **outs = NULL;
    char **band_name = (char **) CALLOC(MAX_BANDS, sizeof(char*));
    band_name[0] = (char*) MALLOC(sizeof(char)*100);
    strcpy(band_name[0], "INTERFEROGRAM_PHASE");
    export_band_image(in_meta_name, in_data_name, out_name,
		      MINMAX, band_name, FALSE, FALSE, FALSE,
		      "interferogram.lut", format, &nouts, &outs);
    FREE(band_name[0]);
    FREE(band_name);
    for (ii=0; ii<nouts; ii++)
      FREE(outs[ii]);
    FREE(outs);
}

/**
 * returns a string of valid XML suitable to be stored in both
 * a TIFF tag and written to the filesystem.
 *
 * assumptions: upper bound of 2000 characters for the xml metadata
 * postcontitions: client must free the allocated memory.
 */
char*
get_insar_xml_string(meta_parameters *meta, int gdal)
{
    // assume upper bound of 2000 characters.
    char *insar_xml_string = (char *) MALLOC(sizeof(char) * 2000);
    
    if (gdal)
      sprintf(insar_xml_string, 
	      "<GDALMetadata>\n"
	      "<Item name=\"INSAR_PROCESSOR\" units=\"1\">%s</Item>\n"
	      "<Item name=\"INSAR_MASTER_IMAGE\" units=\"1\">%s</Item>\n"
	      "<Item name=\"INSAR_SLAVE_IMAGE\" units=\"1\">%s</Item>\n"
	      "<Item name=\"INSAR_MASTER_ACQUISITION_DATE\" units=\"1\">%s"
	      "</Item>\n"
	      "<Item name=\"INSAR_SLAVE_ACQUISITION_DATE\" units=\"1\">%s"
	      "</Item>\n"
	      "<Item name=\"INSAR_CENTER_LOOK_ANGLE\" units=\"degrees\">%.4lf"
	      "</Item>\n"
	      "<Item name=\"INSAR_DOPPLER\" units=\"Hz\">%.4lf</Item>\n"
	      "<Item name=\"INSAR_DOPPLER_RATE\" units=\"Hz/m\">%.8lf</Item>\n"
	      "<Item name=\"INSAR_BASELINE_LENGTH\" units=\"m\">%.1lf</Item>\n"
	      "<Item name=\"INSAR_BASELINE_PARALLEL\" units=\"m\">%.1lf"
	      "</Item>\n"
	      "<Item name=\"INSAR_BASELINE_PARALLEL_RATE\" units=\"m/s\">%.8lf"
	      "</Item>\n"
	      "<Item name=\"INSAR_BASELINE_PERPENDICULAR\" units=\"m\">%.1lf"
	      "</Item>\n"
	      "<Item name=\"INSAR_BASELINE_PERPENDICULAR_RATE\" units=\"m/s\">"
	      "%.8lf</Item>\n"
	      "<Item name=\"INSAR_BASELINE_TEMPORAL\" units=\"days\">%d"
	      "</Item>\n"
	      "<Item name=\"INSAR_BASELINE_CRITICAL\" units=\"m\">%.1lf"
	      "</Item>\n"
	      "</GDALMetadata>\n"
	      , meta->insar->processor
	      , meta->insar->master_image
	      , meta->insar->slave_image
	      , meta->insar->master_acquisition_date
	      , meta->insar->slave_acquisition_date
	      , meta->insar->center_look_angle
	      , meta->insar->doppler
	      , meta->insar->doppler_rate
	      , meta->insar->baseline_length
	      , meta->insar->baseline_parallel
	      , meta->insar->baseline_parallel_rate
	      , meta->insar->baseline_perpendicular
	      , meta->insar->baseline_perpendicular_rate
	      , meta->insar->baseline_temporal
	      , meta->insar->baseline_critical);
    else
      sprintf(insar_xml_string, 
	      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
	      "<insar>\n"
	      "  <processor>%s</processor>\n"
	      "  <master_image>%s</master_image>\n"
	      "  <slave_image>%s</slave_image>\n"
	      "  <master_acquisition_date>%s</master_acquisition_date>\n"
	      "  <slave_acquisition_date>%s</slave_acquisition_date>\n"
	      "  <center_look_angle units=\"degrees\">%.4lf"
	      "</center_look_angle>\n"
	      "  <doppler units=\"Hz\">%.11g</doppler>\n"
	      "  <doppler_rate units=\"Hz/m\">%.11g</doppler_rate>\n"
	      "  <baseline_length units=\"m\">%.1lf</baseline_length>\n"
	      "  <baseline_parallel units=\"m\">%.1lf</baseline_parallel>\n"
	      "  <baseline_parallel_rate units=\"m/s\">%.11g"
	      "</baseline_parallel_rate>\n"
	      "  <baseline_perpendicular units=\"m\">%.1lf"
	      "</baseline_perpendicular>\n"
	      "  <baseline_perpendicular_rate units=\"m/s\">%.11g"
	      "</baseline_perpendicular_rate>\n"
	      "  <baseline_temporal units=\"days\">%d</baseline_temporal>\n"
	      "  <baseline_critical units=\"m\">%.1lf</baseline_critical>\n"
	      "</insar>\n"
	      , meta->insar->processor
	      , meta->insar->master_image
	      , meta->insar->slave_image
	      , meta->insar->master_acquisition_date
	      , meta->insar->slave_acquisition_date
	      , meta->insar->center_look_angle
	      , meta->insar->doppler
	      , meta->insar->doppler_rate
	      , meta->insar->baseline_length
	      , meta->insar->baseline_parallel
	      , meta->insar->baseline_parallel_rate
	      , meta->insar->baseline_perpendicular
	      , meta->insar->baseline_perpendicular_rate
	      , meta->insar->baseline_temporal
	      , meta->insar->baseline_critical);

    return insar_xml_string;
}

void
write_insar_xml_to_file(char *output_file_name, char *insar_xml)
{
     asfPrintStatus("\nWriting InSAR metadata (%s) ...\n", output_file_name);
     FILE *fp = FOPEN(output_file_name, "wt");
    if ( NULL != fp ) {
        fprintf(fp, insar_xml);
        FCLOSE(fp);
    }
}

char*
get_dem_xml_string(meta_parameters *meta, int gdal)
{
  // assume upper bound of 2000 characters.
  char *dem_xml_string = (char *) MALLOC(sizeof(char) * 2000);
  char unit[5];
  
  if (strcmp_case(meta->dem->unit_type, "M") == 0)
    strcpy(unit, "m");
  else if (strcmp_case(meta->dem->unit_type, "FT") == 0)
    strcpy(unit, "ft");
  else
    strcpy(unit, "?");
  
  if (gdal)
    sprintf(dem_xml_string, 
	    "<GDALMetadata>\n"
	    "<Item name=\"DEM_SOURCE\" units=\"1\">%s</Item>\n"
	    "<Item name=\"DEM_FORMAT\" units=\"1\">%s</Item>\n"
	    "<Item name=\"DEM_TILES\" units=\"1\">%s</Item>\n"
	    "<Item name=\"DEM_MIN_VALUE\" units=\"%s\">%.2lf</Item>\n"
	    "<Item name=\"DEM_MAX_VALUE\" units=\"%s\">%.2lf</Item>\n"
	    "<Item name=\"DEM_MEAN_VALUE\" units=\"%s\">%.3lf</Item>\n"
	    "<Item name=\"DEM_STANDARD_DEVIATION\" units=\"%s\">%.3lf</Item>\n"
	    "<Item name=\"DEM_UNIT_TYPE\" units=\"1\">%s</Item>\n"
	    "<Item name=\"DEM_NO_DATA\" units=\"%s\">%.1lf</Item>\n"
	    "</GDALMetadata>\n"
	    , meta->dem->source
	    , meta->dem->format
	    , meta->dem->tiles
	    , unit, meta->dem->min_value
	    , unit, meta->dem->max_value
	    , unit, meta->dem->mean_value
	    , unit, meta->dem->standard_deviation
	    , meta->dem->unit_type
	    , unit, meta->dem->no_data);
  else
    sprintf(dem_xml_string, 
	    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
	    "<dem>\n"
	    "  <source>%s</source>\n"
	    "  <format>%s</format>\n"
	    "  <tiles>%s</tiles\n"
	    "  <min_value units=\"%s\">%.2lf</min_value>\n"
	    "  <max_value units=\"%s\">%.2lf</max_value>\n"
	    "  <mean_value units=\"%s\">%.3lf</mean_value>\n"
	    "  <standard_deviation units=\"%s\">%.3lf</standard_deviation>\n"
	    "  <unit_type>%s</unit_type>\n"
	    "  <no_data units=\"%s\">%.2lf</no_data>\n"
	    "</dem>\n"
	    , meta->dem->source
	    , meta->dem->format
	    , meta->dem->tiles
	    , unit, meta->dem->min_value
	    , unit, meta->dem->max_value
	    , unit, meta->dem->mean_value
	    , unit, meta->dem->standard_deviation
	    , meta->dem->unit_type
	    , unit, meta->dem->no_data);
  
  return dem_xml_string;
}

void
write_dem_xml_to_file(char *output_file_name, char *dem_xml)
{
  asfPrintStatus("\nWriting DEM metadata (%s) ...\n", output_file_name);
  FILE *fp = FOPEN(output_file_name, "wt");
  if ( NULL != fp ) {
    fprintf(fp, dem_xml);
    FCLOSE(fp);
  }
}
