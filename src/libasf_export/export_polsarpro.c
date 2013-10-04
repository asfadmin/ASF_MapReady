#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>
#include <envi.h>

static char *t3_matrix[9] = {"T11.bin","T12_real.bin","T12_imag.bin",
			     "T13_real.bin","T13_imag.bin","T22.bin",
			     "T23_real.bin","T23_imag.bin","T33.bin"};
static char *t4_matrix[16] = {"T11.bin","T12_real.bin","T12_imag.bin",
			      "T13_real.bin","T13_imag.bin","T14_real.bin",
			      "T14_imag.bin","T22.bin","T23_real.bin",
			      "T23_imag.bin","T24_real.bin","T24_imag.bin",
			      "T33.bin","T34_real.bin","T34_imag.bin",
			      "T44.bin"};
static char *c2_matrix[4] = {"C11.bin","C12_real.bin","C12_imag.bin",
			     "C22.bin"};
static char *c3_matrix[9] = {"C11.bin","C12_real.bin","C12_imag.bin",
			     "C13_real.bin","C13_imag.bin","C22.bin",
			     "C23_real.bin","C23_imag.bin","C33.bin"};
static char *c4_matrix[16] = {"C11.bin","C12_real.bin","C12_imag.bin",
			      "C13_real.bin","C13_imag.bin","C14_real.bin",
			      "C14_imag.bin","C22.bin","C23_real.bin",
			      "C23_imag.bin","C24_real.bin","C24_imag.bin",
			      "C33.bin","C34_real.bin","C34_imag.bin",
			      "C44.bin"};

static char *freeman2_decomposition[2] = 
  {"Freeman2_Ground.bin","Freeman2_Vol.bin"};
static char *freeman3_decomposition[3] = 
  {"Freeman_Dbl.bin","Freeman_Odd.bin","Freeman_Vol.bin"};
static char *vanZyl3_decomposition[3] = 
  {"VanZyl3_Dbl.bin","VanZyl3_Odd.bin","VanZyl3_Vol.bin"};
static char *yamaguchi3_decomposition[3] = 
  {"Yamaguchi3_Dbl.bin","Yamaguchi3_Odd.bin","Yamaguchi3_Vol.bin"};
static char *yamaguchi4_decomposition[4] = 
  {"Yamaguchi4_Dbl.bin","Yamaguchi4_Hlx.bin","Yamaguchi4_Odd.bin",
   "Yamaguchi4_Vol.bin"};
static char *krogager_decomposition[3] = 
  {"Krogager_Kd.bin","Krogager_Kh.bin","Krogager_Ks.bin"};
static char *touzi1_decomposition[3] = 
  {"TSVM_alpha_s1.bin","TSVM_alpha_s2.bin","TSVM_alpha_s3.bin"};
static char *touzi2_decomposition[3] = 
  {"TSVM_phi_s1.bin","TSVM_phi_s2.bin","TSVM_phi_s3.bin"};
static char *touzi3_decomposition[3] = 
  {"TSVM_tau_m1.bin","TSVM_tau_m2.bin","TSVM_tau_m3.bin"};
static char *touzi4_decomposition[3] = 
  {"TSVM_psi1.bin","TSVM_psi2.bin","TSVM_psi3.bin"};
static char *touzi5_decomposition[4] = 
  {"TSVM_alpha_s.bin","TSVM_phi_s.bin","TSVM_tau_m.bin","TSVM_psi.bin"};

void initialize_polsarpro_file(const char *output_file_name,
			       meta_parameters *meta, FILE **fpOut)
{
  *fpOut = FOPEN(output_file_name, "wb");
  
  char *output_header_file_name = 
    (char *) MALLOC(sizeof(char) * strlen(output_file_name) + 5);
  sprintf(output_header_file_name, "%s.hdr", output_file_name);
  char *band_name = get_filename(output_file_name);
  envi_header *envi = meta2envi(meta);
  envi->bands = 1;
  char *band = stripExt(band_name);
  strcpy(envi->band_name, band_name);
  envi->byte_order = 0; // PolSARPro data is little endian by default
  write_envi_header(output_header_file_name, output_file_name, meta, envi);
  FREE(output_header_file_name);
  FREE(band_name);
  FREE(band);
  FREE(envi);
  
  return;
}

void generate_config_file(char *configFile, int line_count, int sample_count)
{
  FILE *fpConfig = FOPEN(configFile, "w");
  fprintf(fpConfig, "Nrow\n%d\n", line_count);
  fprintf(fpConfig, "---------\nNcol\n%d\n", sample_count);
  fprintf(fpConfig, "---------\nPolarCase\nmonostatic\n");
  fprintf(fpConfig, "---------\nPolarType\nfull\n");
  FCLOSE(fpConfig);
}

static char *datum2envi(datum_type_t datum)
{
  char *datumStr = (char *) MALLOC(sizeof(char)*128);
  strcpy(datumStr, MAGIC_UNSET_STRING);
  switch (datum) {
  case NAD27_DATUM:
    strcpy(datumStr, "North America 1927");
    break;
  case NAD83_DATUM:
    strcpy(datumStr, "North America 1983");
    break;
  case ED50_DATUM:
    strcpy(datumStr, "European 1950");
    break;
  case WGS72_DATUM:
    strcpy(datumStr, "WGS-72");
    break;
  case WGS84_DATUM:
    strcpy(datumStr, "WGS-84");
    break;
  case HUGHES_DATUM:
    strcpy(datumStr, "Hughes");
    break;
  default:
    asfPrintError("Unsupported datum: %d\n", (int)datum);
    break;
  }
  return datumStr;
}

void generate_mapready_config_file(char *configFile, meta_parameters *meta)
{
  meta_general *gen = meta->general;
  meta_projection *proj = meta->projection;
  proj_ps ps = proj->param.ps;
  proj_albers albers = proj->param.albers;
  proj_lamaz lamaz = proj->param.lamaz;
  proj_lamcc lamcc = proj->param.lamcc;
  FILE *fpConfig = FOPEN(configFile, "w");
  fprintf(fpConfig, "Sensor\n%s\n", meta->general->sensor);
  fprintf(fpConfig, "---------\nMapInfo\nmap info = {");
  if (proj->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
    fprintf(fpConfig, "UTM, %d, %d, %.3f, %.3f, %.3f, %.3f, %d, ",
	    gen->start_line+1, gen->start_sample+1, proj->startX, proj->startY, 
	    fabs(proj->perX), fabs(proj->perY), proj->param.utm.zone);
    if (proj->hem == 'N')
      fprintf(fpConfig, "North, %s}\n", datum2envi(proj->datum));
    else
      fprintf(fpConfig, "South, %s}\n", datum2envi(proj->datum));
  }
  else if (proj->type == POLAR_STEREOGRAPHIC)
    fprintf(fpConfig, "Polar Stereographic, %d, %d, %.3f, %.3f, %.3f, %.3f, "
	    "%s}\n", gen->start_line+1, gen->start_sample+1, proj->startX, 
	    proj->startY, fabs(proj->perX), fabs(proj->perY), 
	    datum2envi(proj->datum));
  else if (proj->type == ALBERS_EQUAL_AREA)
    fprintf(fpConfig, "Albers Conical Equal Area, %d, %d, %.3f, %.3f, %.3f, "
	    "%.3f, %s}\n", gen->start_line+1, gen->start_sample+1, proj->startX,
	    proj->startY, fabs(proj->perX), fabs(proj->perY), 
	    datum2envi(proj->datum));
  else if (proj->type == LAMBERT_CONFORMAL_CONIC)
    fprintf(fpConfig, "Lambert Conformal Conic, %d, %d, %.3f, %.3f, %.3f, %.3f,"
	    " %s}\n", gen->start_line+1, gen->start_sample+1, proj->startX,
	    proj->startY, fabs(proj->perX), fabs(proj->perY),
	    datum2envi(proj->datum));
  else if (proj->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)
    fprintf(fpConfig, "Lambert Azimuthal Equal Area, %d, %d, %.3f, %.3f, %.3f, "
	    "%.3f, %s}\n", gen->start_line+1, gen->start_sample+1, proj->startX,
	    proj->startY, fabs(proj->perX), fabs(proj->perY), 
	    datum2envi(proj->datum));
  else if (proj->type == LAT_LONG_PSEUDO_PROJECTION)
    fprintf(fpConfig, "Geographic Lat/Lon, %d, %d, %.5f, %.5f, %f, %f, %s, "
	    "units=Degrees}\n", gen->start_line+1, gen->start_sample+1,
	    proj->startX, proj->startY, fabs(proj->perX), fabs(proj->perY),
	    datum2envi(proj->datum));
  if (proj->type != UNIVERSAL_TRANSVERSE_MERCATOR)
    fprintf(fpConfig, "---------\nProjInfo\nprojection info = {");
  if (proj->type == POLAR_STEREOGRAPHIC)
    fprintf(fpConfig, "31, %.3f, %.3f, %.4f, %.4f, 0.0, 0.0, %s, Polar "
	    "Stereographic}\n", proj->re_major, proj->re_minor, 
	    ps.slat, ps.slon, datum2envi(proj->datum));
  else if (proj->type == ALBERS_EQUAL_AREA)
    fprintf(fpConfig, "9, %.3f, %.3f, %.4f, %.4f, 0.0, 0.0, %.4f, %.4f, %s, "
	    "Albers Conical Equal Area}\n", proj->re_major, proj->re_minor, 
	    albers.orig_latitude, albers.center_meridian, albers.std_parallel1,
	    albers.std_parallel2, datum2envi(proj->datum));
  else if (proj->type == LAMBERT_CONFORMAL_CONIC)
    fprintf(fpConfig, "4, %.3f, %.3f, %.4f, %.4f, %.1f, %.1f, %.4f, %.4f, %s, "
	    "Lambert Conformal Conic}\n", proj->re_major, proj->re_minor,
	    lamcc.lat0, lamcc.lon0, lamcc.false_easting, lamcc.false_northing,
	    lamcc.plat1, lamcc.plat2, datum2envi(proj->datum));
  else if (proj->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)
    fprintf(fpConfig, "11, %.3f, %.3f, %.4f, %.4f, %.1f, %.1f, %s, Lambert "
	    "Azimuthal Equal Area}\n", proj->re_major, proj->re_minor,
	    lamaz.center_lat, lamaz.center_lon, lamaz.false_easting,
	    lamaz.false_northing, datum2envi(proj->datum));
  else if (proj->type == LAT_LONG_PSEUDO_PROJECTION)
    fprintf(fpConfig, "1, %.3f, %.3f, 0.0, 0.0, %s}\n", proj->re_major,
	    proj->re_minor, datum2envi(proj->datum));
  fprintf(fpConfig, "---------\nWaveUnit\nwavelength units = meters\n");
  fprintf(fpConfig, "---------\nMapProj\n");
  if (proj->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
    fprintf(fpConfig, "UTM\n");
    fprintf(fpConfig, "%f\n%f\n%f\n%f\n%d\n", proj->startX, proj->startY,
	    fabs(proj->perX), fabs(proj->perY),proj->param.utm.zone);
    if (proj->hem == 'N')
      fprintf(fpConfig, "North,\n");
    else
      fprintf(fpConfig, "South,\n");
  }
  else
    fprintf(fpConfig, "NO UTM\n");
  FCLOSE(fpConfig);
}

void export_polsarpro(const char *metadata_file_name,
		      const char *image_data_file_name,
		      char *output_file_name, 
		      int *noutputs, char ***output_names)
{
  FILE *fpIn, *fpOut;
  int ii, jj, kk;
  char base_name[255];
  strcpy(base_name, output_file_name);
  char *matrix = (char *) MALLOC(sizeof(char)*5);
  char *decomposition = (char *) MALLOC(sizeof(char)*25);
  char *path_name = (char *) MALLOC(sizeof(char)*1024);
  char *out_file = NULL;

  meta_parameters *md = meta_read (metadata_file_name);
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  int band_count = md->general->band_count;
  char **band_name = extract_band_names(md->general->bands, band_count);

  if (md->general->image_data_type >= POLARIMETRIC_C2_MATRIX &&
      md->general->image_data_type <= POLARIMETRIC_T4_MATRIX) {
    if (strstr(md->general->bands, "C44"))
      sprintf(matrix, "C4");
    else if (strstr(md->general->bands, "C33"))
      sprintf(matrix, "C3");
    else if (strstr(md->general->bands, "C22"))
      sprintf(matrix, "C2");
    else if (strstr(md->general->bands, "T44"))
      sprintf(matrix, "T4");
    else if (strstr(md->general->bands, "T33"))
      sprintf(matrix, "T3");
    else if (strstr(md->general->bands, "T22"))
      sprintf(matrix, "T2");
    else
      asfPrintError("The bands do not correspond to matrix type (%s)\n",
		    image_data_type2str(md->general->image_data_type));
  }
  
  // If we are dealing with polarimetric matrices, the output file name
  // becomes the name of an output directory. We need to figure out from
  // the bands string, what kind of matrix we have, because we need to
  // create the appropriate subdirectory. Otherwise, PolSARPro can't handle
  // the files out of the box.
  if (md->general->image_data_type >= POLARIMETRIC_C2_MATRIX &&
      md->general->image_data_type <= POLARIMETRIC_T4_MATRIX) {
    char *dirName = (char *) MALLOC(sizeof(char)*1024);
    char *fileName = (char *) MALLOC(sizeof(char)*1024);
    split_dir_and_file(output_file_name, dirName, fileName);
    char *path = get_dirname(output_file_name);
    if (strlen(dirName) <= 0) {
      path = g_get_current_dir();
      sprintf(path_name, "%s%c%s%c%s", 
	      path, DIR_SEPARATOR, output_file_name, DIR_SEPARATOR, matrix);
    }
    else
      sprintf(path_name, "%s%c%s%c%s", 
	      dirName, DIR_SEPARATOR, fileName, DIR_SEPARATOR, matrix);
    if (is_dir(path_name))
      asfPrintStatus("Output directory (%s) already exists.\n", path_name);
    else if(create_dir(path_name) == -1)
      asfPrintError("Can't generate output directory (%s).\n", path_name);
    char *configFile = 
      (char *) MALLOC(sizeof(char)*(strlen(path_name)+15));
    sprintf(configFile, "%s%cconfig.txt", path_name, DIR_SEPARATOR);
    generate_config_file(configFile, line_count, sample_count);
    FREE(configFile);
    if (md->projection) {
      char *mapreadyConfig = 
	(char *) MALLOC(sizeof(char)*(strlen(path_name)+25));
      sprintf(mapreadyConfig, "%s%cconfig_mapready.txt", 
	      path_name, DIR_SEPARATOR);
      generate_mapready_config_file(mapreadyConfig, md);
      FREE(mapreadyConfig);
    }
    FREE(path);
    FREE(dirName);
    FREE(fileName);
  }
  
  if (md->general->image_data_type == POLARIMETRIC_DECOMPOSITION)
    strcpy(decomposition, band_name[band_count-1]);
  
  // We treat polarimetric decompositions in a similar way. The output file
  // name becomes the name of an output directory again. The actual file
  // names can be extracted from the bands string.
  if (md->general->image_data_type == POLARIMETRIC_DECOMPOSITION) {
    char *dirName = (char *) MALLOC(sizeof(char)*1024);
    char *fileName = (char *) MALLOC(sizeof(char)*1024);
    split_dir_and_file(output_file_name, dirName, fileName);
    char *path = get_dirname(output_file_name);
    if (strlen(dirName) <= 0) {
      path = g_get_current_dir();
      sprintf(path_name, "%s%c%s", 
	      path, DIR_SEPARATOR, output_file_name);
    }
    else
      sprintf(path_name, "%s%c%s", 
	      dirName, DIR_SEPARATOR, fileName);
    if (is_dir(path_name))
      asfPrintStatus("Output directory (%s) already exists.\n", path_name);
    else if(create_dir(path_name) == -1)
      asfPrintError("Can't generate output directory (%s).\n", path_name);
    char *configFile = 
      (char *) MALLOC(sizeof(char)*(strlen(path_name)+15));
    sprintf(configFile, "%s%cconfig.txt", path_name, DIR_SEPARATOR);
    generate_config_file(configFile, line_count, sample_count);
    FREE(configFile);
    FREE(path);
    FREE(dirName);
    FREE(fileName);      
  }
  
  // store the names of the generated files here
  *noutputs = 0;
  *output_names = MALLOC(sizeof(char*) * band_count);
  
  for (kk=0; kk<band_count; kk++) {
    // skip the 'AMP' band if we have POlSARPro data and the user wants
    // to apply a LUT
    if (strcmp_case(band_name[kk], "AMP") == 0 && band_count > 1)
      continue;
    
    out_file = (char *) MALLOC(sizeof(char)*1024);
    strcpy(out_file, output_file_name);
    
    // Initialize the selected format
    // NOTE: For PolSARpro, the first band is amplitude and should be
    // written out as a single-band greyscale image while the second
    // band is a classification and should be written out as color ...
    // and for TIFF formats, as a palette color tiff.
    // The only exception to this rule are polarimetric matrices
    if (md->general->image_data_type >= POLARIMETRIC_C2_MATRIX &&
	md->general->image_data_type <= POLARIMETRIC_T4_MATRIX) {
      int ll, found_band = FALSE;
      int bands;
      if (strcmp(matrix, "T3") == 0)
	bands = 9;
      if (strcmp(matrix, "T4") == 0)
	bands = 16;
      if (strcmp(matrix, "C2") == 0)
	bands = 4;
      if (strcmp(matrix, "C3") == 0)
	bands = 9;
      if (strcmp(matrix, "C4") == 0)
	bands = 16;
      for (ll=0; ll<bands; ll++) {
	if (strcmp(matrix, "T3") == 0 && 
	    strncmp(band_name[kk], t3_matrix[ll], strlen(band_name[kk])) == 0)
	  found_band++;
	else if (strcmp(matrix, "T4") == 0 && 
		 strncmp(band_name[kk], t4_matrix[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(matrix, "C2") == 0 && 
		 strncmp(band_name[kk], c2_matrix[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(matrix, "C3") == 0 && 
		 strncmp(band_name[kk], c3_matrix[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(matrix, "C4") == 0 && 
		 strncmp(band_name[kk], c4_matrix[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
      }
      if (!found_band) {
	FREE(out_file);
	continue;
      }
      sprintf(out_file, "%s%c%s", path_name, DIR_SEPARATOR, band_name[kk]);
    }
    else if (md->general->image_data_type == POLARIMETRIC_DECOMPOSITION) {
      int ll, found_band = FALSE;
      int bands;
      if (strcmp(decomposition, "Freeman2_Vol") == 0)
	bands = 2;
      else if (strcmp(decomposition, "Freeman_Vol") == 0)
	bands = 3;
      else if (strcmp(decomposition, "VanZyl3_Vol") == 0)
	bands = 3;
      else if (strcmp(decomposition, "Yamaguchi3_Vol") == 0)
	bands = 3;
      else if (strcmp(decomposition, "Yamaguchi4_Vol") == 0)
	bands = 4;
      else if (strcmp(decomposition, "Krogager_Ks") == 0)
	bands = 3;
      else if (strcmp(decomposition, "TSVM_alpha_s3") == 0)
	bands = 3;
      else if (strcmp(decomposition, "TSVM_phi_s3") == 0)
	bands = 3;
      else if (strcmp(decomposition, "TSVM_tau_m3") == 0)
	bands = 3;
      else if (strcmp(decomposition, "TSVM_psi3") == 0)
	bands = 3;
      else if (strcmp(decomposition, "TSVM_psi") == 0)
	bands = 4;
      for (ll=0; ll<bands; ll++) {
	if (strcmp(decomposition, "Freeman2_Vol") == 0 && 
	    strncmp(band_name[kk], freeman2_decomposition[ll], 
		    strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "Freeman_Vol") == 0 && 
		 strncmp(band_name[kk], freeman3_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "VanZyl3_Vol") == 0 && 
		 strncmp(band_name[kk], vanZyl3_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "Yamaguchi3_Vol") == 0 && 
		 strncmp(band_name[kk], yamaguchi3_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "Yamaguchi4_Vol") == 0 && 
		 strncmp(band_name[kk], yamaguchi4_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "Krogager_Ks") == 0 && 
		 strncmp(band_name[kk], krogager_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "TSVM_alpha_s3") == 0 && 
		 strncmp(band_name[kk], touzi1_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "TSVM_phi_s3") == 0 && 
		 strncmp(band_name[kk], touzi2_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "TSVM_tau_m3") == 0 && 
		 strncmp(band_name[kk], touzi3_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "TSVM_psi3") == 0 && 
		 strncmp(band_name[kk], touzi4_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
	else if (strcmp(decomposition, "TSVM_psi") == 0 && 
		 strncmp(band_name[kk], touzi5_decomposition[ll], 
			 strlen(band_name[kk])) == 0)
	  found_band = TRUE;
      }
      if (!found_band) {
	free(out_file);
	continue;
      }
      sprintf(out_file, "%s%c%s", 
	      path_name, DIR_SEPARATOR, band_name[kk]);
      
    }
    else if (md->general->image_data_type == POLARIMETRIC_SEGMENTATION ||
	     md->general->image_data_type == POLARIMETRIC_PARAMETER)
      append_band_ext(base_name, out_file, NULL);
    
    if (strcmp(band_name[0], MAGIC_UNSET_STRING) != 0)
      asfPrintStatus("\nWriting band '%s' ...\n", band_name[kk]);
    
    append_ext_if_needed (out_file, ".bin", NULL);
    initialize_polsarpro_file(out_file, md, &fpOut);
    
    (*output_names)[*noutputs] = STRDUP(out_file);
    *noutputs += 1;
    
    // Determine which channel to read
    int channel;
    if (md->general->image_data_type >  POLARIMETRIC_IMAGE &&
	md->general->image_data_type <= POLARIMETRIC_T4_MATRIX)
      channel = kk;
    else {
      if (md->general->band_count == 1)
	channel = 0;
      else
	channel = 
	  get_band_number(md->general->bands, band_count, band_name[kk]);
      asfRequire(channel >= 0 && channel <= MAX_BANDS,
		 "Band number out of range\n");
    }
    
    // Write the output image
    fpIn = FOPEN(image_data_file_name, "rb");
    float *float_line = (float *) MALLOC(sizeof(float) * sample_count);
    
    asfPrintStatus("Writing output file...\n");
    for (ii=0; ii<line_count; ii++ ) {
      get_float_line(fpIn, md, ii+channel*line_count, float_line);
      for (jj=0; jj<sample_count; jj++)
	ieee_lil32(float_line[jj]);
      fwrite(float_line, 4, sample_count, fpOut);
      asfLineMeter(ii, line_count);
    }
    FREE(float_line);  
    FCLOSE(fpIn);
    FCLOSE(fpOut);
  }

  for (ii=0; ii<band_count; ++ii)
    FREE(band_name[ii]);
  FREE(band_name);
  FREE(path_name);
  FREE(matrix);
  FREE(decomposition);
  FREE(out_file);
  meta_free (md);
}
