#include "alos_mosaic.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include <ctype.h>

alos_mosaic_header *alos_mosaic_init(void)
{
  int ii;
  alos_mosaic_header *header;
  header = (alos_mosaic_header *) CALLOC(1, sizeof(alos_mosaic_header));
 
  strcpy(header->image_file_id, MAGIC_UNSET_STRING);
  if (strstr(header->image_file_id, "ALPSR") == NULL)
    strcpy(header->image_file_id, MAGIC_UNSET_STRING);
  strcpy(header->product_id, MAGIC_UNSET_STRING);
  strcpy(header->area, MAGIC_UNSET_STRING);
  strcpy(header->acquisition, MAGIC_UNSET_STRING);
  strcpy(header->path_mode, MAGIC_UNSET_STRING);
  strcpy(header->separate, MAGIC_UNSET_STRING);
  header->file_no = MAGIC_UNSET_INT;
  strcpy(header->product_type, MAGIC_UNSET_STRING);
  strcpy(header->mission, MAGIC_UNSET_STRING);
  strcpy(header->sensor, MAGIC_UNSET_STRING);
  strcpy(header->original_path_type, MAGIC_UNSET_STRING);
  header->pixel_spacing = MAGIC_UNSET_DOUBLE;
  strcpy(header->orbit_direction, MAGIC_UNSET_STRING);
  strcpy(header->polarization, MAGIC_UNSET_STRING);
  header->start_rsp = MAGIC_UNSET_INT;
  header->path = MAGIC_UNSET_INT;
  strcpy(header->resampling_method, MAGIC_UNSET_STRING);
  header->corner1_lat = MAGIC_UNSET_DOUBLE;
  header->corner1_lon = MAGIC_UNSET_DOUBLE;
  header->corner2_lat = MAGIC_UNSET_DOUBLE;
  header->corner2_lon = MAGIC_UNSET_DOUBLE;
  header->corner3_lat = MAGIC_UNSET_DOUBLE;
  header->corner3_lon = MAGIC_UNSET_DOUBLE;
  header->corner4_lat = MAGIC_UNSET_DOUBLE;
  header->corner4_lon = MAGIC_UNSET_DOUBLE;
  strcpy(header->continental_name, MAGIC_UNSET_STRING);
  strcpy(header->continental_code, MAGIC_UNSET_STRING);
  for (ii=0; ii<8; ii++)
    strcpy(header->national_code[ii], MAGIC_UNSET_STRING);
  strcpy(header->map_projection, MAGIC_UNSET_STRING);
  header->orig_lat = MAGIC_UNSET_DOUBLE;
  header->orig_lon = MAGIC_UNSET_DOUBLE;
  header->ps_ref_lat = MAGIC_UNSET_DOUBLE;
  header->ps_ref_lon = MAGIC_UNSET_DOUBLE;
  strcpy(header->hemisphere, MAGIC_UNSET_STRING);
  header->utm_zone = MAGIC_UNSET_INT;
  header->lcc_ref_lat1 = MAGIC_UNSET_DOUBLE;
  header->lcc_ref_lat2 = MAGIC_UNSET_DOUBLE;
  header->range_spacing = MAGIC_UNSET_DOUBLE;
  header->azimuth_spacing = MAGIC_UNSET_DOUBLE;
  header->true_north = MAGIC_UNSET_DOUBLE;
  strcpy(header->datum, MAGIC_UNSET_STRING);
  strcpy(header->ellipsoid, MAGIC_UNSET_STRING);
  header->semimajor = MAGIC_UNSET_DOUBLE; 
  header->semiminor = MAGIC_UNSET_DOUBLE;
  header->ellipticity = MAGIC_UNSET_DOUBLE;
  header->cal_factor = MAGIC_UNSET_DOUBLE;
  header->sample_count = MAGIC_UNSET_INT;
  header->line_count = MAGIC_UNSET_INT;
  header->bits_per_pixel = MAGIC_UNSET_INT;
  strcpy(header->processing_date, MAGIC_UNSET_STRING);
  strcpy(header->processing_time, MAGIC_UNSET_STRING);
  strcpy(header->creation_country, MAGIC_UNSET_STRING);
  strcpy(header->creation_agency, MAGIC_UNSET_STRING);
  strcpy(header->creation_equipment, MAGIC_UNSET_STRING);
  strcpy(header->processor_name, MAGIC_UNSET_STRING);
  strcpy(header->version_number, MAGIC_UNSET_STRING);
  strcpy(header->manual_revision, MAGIC_UNSET_STRING);
  strcpy(header->format_revision, MAGIC_UNSET_STRING);
  header->orig_path = MAGIC_UNSET_INT;
  strcpy(header->sar_image_file_id, MAGIC_UNSET_STRING);
  header->rsp_path_number = MAGIC_UNSET_INT;
  strcpy(header->downlink_segment, MAGIC_UNSET_STRING); 
  header->off_nadir = MAGIC_UNSET_DOUBLE;
  strcpy(header->observation_date, MAGIC_UNSET_STRING);
  strcpy(header->obs_polarization, MAGIC_UNSET_STRING);
  header->cycle_number = MAGIC_UNSET_INT;

  return header;
}

char *read_str(FILE *fp)
{
  char *str = (char *) MALLOC(sizeof(char)*100);
  fgets(str, 100, fp);
  chomp(str);
  return str;
}

int read_int(FILE *fp)
{
  char str[100];
  fgets(str, 100, fp);
  chomp(str);
  return atoi(str);
}

double read_double(FILE *fp)
{
  char str[100];
  fgets(str, 100, fp);
  chomp(str);
  return atof(str);
}

alos_mosaic_header *read_alos_mosaic_header(const char *dataFile)
{
  alos_mosaic_header *header = NULL;
  FILE *fp;
  int ii;
  char blank[50];

  fp = FOPEN(dataFile, "r");
  header = alos_mosaic_init();

  strcpy(header->image_file_id, read_str(fp));
  strcpy(header->product_id, read_str(fp));
  strcpy(header->area, read_str(fp));
  strcpy(header->acquisition, read_str(fp));
  strcpy(header->path_mode, read_str(fp));
  strcpy(header->separate, read_str(fp));
  header->file_no = read_int(fp);
  strcpy(header->product_type, read_str(fp));
  strcpy(header->mission, read_str(fp));
  strcpy(header->sensor, read_str(fp));
  strcpy(header->original_path_type, read_str(fp));
  header->pixel_spacing = read_double(fp);
  strcpy(header->orbit_direction, read_str(fp));
  strcpy(header->polarization, read_str(fp));
  header->start_rsp = read_int(fp);
  header->path = read_int(fp);
  strcpy(header->resampling_method, read_str(fp));
  strcpy(blank, read_str(fp));
  header->corner1_lat = read_double(fp);
  header->corner1_lon = read_double(fp);
  header->corner2_lat = read_double(fp);
  header->corner2_lon = read_double(fp);
  header->corner3_lat = read_double(fp);
  header->corner3_lon = read_double(fp);
  header->corner4_lat = read_double(fp);
  header->corner4_lon = read_double(fp);
  strcpy(header->continental_name, read_str(fp));
  strcpy(header->continental_code, read_str(fp));
  for (ii=0; ii<8; ii++)
    strcpy(header->national_code[ii], read_str(fp));
  strcpy(blank, read_str(fp));
  strcpy(header->map_projection, read_str(fp));
  header->orig_lat = read_double(fp);
  header->orig_lon = read_double(fp);
  header->ps_ref_lat = read_double(fp);
  header->ps_ref_lon = read_double(fp);
  strcpy(header->hemisphere, read_str(fp));
  header->utm_zone = read_int(fp);
  header->lcc_ref_lat1 = read_double(fp);
  header->lcc_ref_lat2 = read_double(fp);
  header->range_spacing = read_double(fp);
  header->azimuth_spacing = read_double(fp);
  header->true_north = read_double(fp);
  strcpy(blank, read_str(fp));
  strcpy(header->datum, read_str(fp));
  strcpy(header->ellipsoid, read_str(fp));
  header->semimajor = read_double(fp); 
  header->semiminor = read_double(fp);
  header->ellipticity = read_double(fp);
  strcpy(blank, read_str(fp));
  header->cal_factor = read_double(fp);
  strcpy(blank, read_str(fp));
  header->sample_count = read_int(fp);
  header->line_count = read_int(fp);
  header->bits_per_pixel = read_int(fp);
  strcpy(blank, read_str(fp));
  strcpy(header->processing_date, read_str(fp));
  strcpy(header->processing_time, read_str(fp));
  strcpy(header->creation_country, read_str(fp));
  strcpy(header->creation_agency, read_str(fp));
  strcpy(header->creation_equipment, read_str(fp));
  strcpy(header->processor_name, read_str(fp));
  strcpy(header->version_number, read_str(fp));
  strcpy(header->manual_revision, read_str(fp));
  strcpy(header->format_revision, read_str(fp));
  strcpy(blank, read_str(fp));
  header->orig_path = read_int(fp);
  strcpy(header->sar_image_file_id, read_str(fp));
  header->rsp_path_number = read_int(fp);
  strcpy(header->downlink_segment, read_str(fp)); 
  header->off_nadir = read_double(fp);
  strcpy(header->observation_date, read_str(fp));
  strcpy(header->obs_polarization, read_str(fp));
  header->cycle_number = read_int(fp);

  FCLOSE(fp);
  
  return header;
}


void import_alos_mosaic(const char *inBaseName, radiometry_t radiometry,
			const char *outBaseName)
{
  FILE *fpIn, *fpOut;
  alos_mosaic_header *alos;
  meta_parameters *meta;
  char dataName[1024], metaName[1024], outName[1024];
  unsigned char shortValue[2];
  int ii, kk;
  float *floatBuf;

  // Check radiometry
  if (radiometry != r_AMP &&
      radiometry != r_SIGMA && radiometry != r_SIGMA_DB) {
    asfPrintWarning("Radiometry other than AMPLITUDE and SIGMA is not "
		    "supported for ALOS mosaics.\nDefaulting back to "
		    "AMPLITUDE.\n");
    radiometry = r_AMP;
  }
  
  sprintf(dataName, "%s_IMG", inBaseName);
  sprintf(metaName, "%s_HDR", inBaseName);
  if (!fileExists(metaName))
    sprintf(metaName, "%s_HDR.txt");
  if (findExt(outBaseName) && strcmp_case(findExt(outBaseName), ".img") == 0)
    strcpy(outName, outBaseName);
  else
    sprintf(outName, "%s.img", outBaseName);

  alos = read_alos_mosaic_header(metaName);
  meta = alos_mosaic2meta(alos);
  meta->general->radiometry = radiometry;
  floatBuf = (float *) MALLOC(sizeof(float)*meta->general->sample_count);

  fpIn = FOPEN(dataName, "rb");
  fpOut = FOPEN(outName, "wb");
  // Input is 16-bit data that needs to be swapped.
  for (ii=0; ii<meta->general->line_count; ii++) {
    for (kk=0; kk<meta->general->sample_count; kk++) {
      FREAD(shortValue, 1, 2, fpIn);
      // Only sigma values are valid in mosaics (no incidence angle)
      if (radiometry == r_SIGMA)
	floatBuf[kk] = 
	  get_cal_dn(meta, 0.0, kk, (float) lilInt16(shortValue), NULL, FALSE);
      else if (radiometry == r_SIGMA_DB)
	floatBuf[kk] =
	  get_cal_dn(meta, 0.0, kk, (float) lilInt16(shortValue), NULL, TRUE);
      else
	floatBuf[kk] = (float) lilInt16(shortValue);
    }
    put_float_line(fpOut, meta, ii, floatBuf);
    asfLineMeter(ii, meta->general->line_count);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(floatBuf);
  meta_write(meta, outBaseName);
  FREE(alos);
  meta_free(meta);
};

