#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <asf.h>
#include "asf_tiff.h"

#include <gsl/gsl_math.h>
#include <proj_api.h>

#include "asf_jpeg.h"
#include <png.h>
#include "envi.h"

#include "dateUtil.h"
#include <time.h>
#include "matrix.h"
#include <asf_nan.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>
#include <asf_raster.h>
#include <float_image.h>
#include <spheroids.h>
#include <typlim.h>
#include <netcdf.h>

#define PGM_MAGIC_NUMBER    "P5"
#ifdef  OPT_STRIP_BYTES
#undef  OPT_STRIP_BYTES
#endif
#define OPT_STRIP_BYTES     8192

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

char *sample_mapping2string(scale_t sample_mapping);
void colormap_to_lut_file(meta_colormap *cm, const char *lut_file);

void initialize_png_file(const char *output_file_name,
			 meta_parameters *meta, FILE **opng,
			 png_structp *png_ptr, png_infop *info_ptr,
			 int rgb)
{
  return initialize_png_file_ext(output_file_name, meta, opng, png_ptr, 
				 info_ptr, rgb, FALSE);
}
void initialize_png_file_ext(const char *output_file_name,
			     meta_parameters *meta, FILE **opng,
			     png_structp *png_ptr, png_infop *info_ptr,
			     int rgb, int alpha)
{
    *opng = FOPEN(output_file_name, "wb");
    *png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
        NULL, NULL, NULL);
    if (!(*png_ptr))
        asfPrintError("Error creating PNG write structure!\n");

    *info_ptr = png_create_info_struct(*png_ptr);
    if (!(*info_ptr))
        asfPrintError("Error creating PNG info structure!\n");

    png_init_io(*png_ptr, *opng);

    int width = meta->general->sample_count;
    int height = meta->general->line_count;
    png_byte color_type;
    if (alpha == 0)
      color_type = rgb ? PNG_COLOR_TYPE_RGB : PNG_COLOR_TYPE_GRAY;
    else if (alpha == 1) {
      color_type = rgb ? PNG_COLOR_TYPE_RGB : PNG_COLOR_TYPE_GRAY;
      int ii;
      if (rgb) {
	png_color_16 trans_rgb_value[256];
	for (ii=0; ii<256; ii++) {
	  trans_rgb_value[ii].red = 255;
	  trans_rgb_value[ii].green = 255;
	  trans_rgb_value[ii].blue = 255;
	}
	// setting black to transparent
	trans_rgb_value[0].red = 0;
	trans_rgb_value[0].green = 0;
	trans_rgb_value[0].blue = 0;
	png_set_tRNS(*png_ptr, *info_ptr, trans_rgb_value, 256, NULL);
      }
      else {
	png_byte trans_values[256];
	for (ii=0; ii<256; ii++)
	  trans_values[ii] = 255;
	trans_values[0] = 0; // setting black to transparent
	png_set_tRNS(*png_ptr, *info_ptr, trans_values, 256, NULL);
      }
    }
    else if (alpha == 2)
      color_type = PNG_COLOR_TYPE_RGB_ALPHA;

    png_set_IHDR(*png_ptr, *info_ptr, width, height, 8, color_type,
            PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
            PNG_FILTER_TYPE_DEFAULT);

    png_write_info(*png_ptr, *info_ptr);
}

void initialize_jpeg_file(const char *output_file_name,
        meta_parameters *meta, FILE **ojpeg,
        struct jpeg_compress_struct *cinfo, int rgb)
{
  struct jpeg_error_mgr *jerr = MALLOC(sizeof(struct jpeg_error_mgr));

  /* We need a version of the data in JSAMPLE form, so we have to map
     floats into JSAMPLEs.  We do this by defining a region 2 sigma on
     either side of the mean to be mapped in the range of JSAMPLE
     linearly, and clamping everything outside this range at the
     limits o the JSAMPLE range.  */
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  asfRequire(sizeof(unsigned char) == 1,
             "Size of the unsigned char data type on this machine is "
             "different than expected.\n");
  asfRequire(sizeof(unsigned char) == sizeof (JSAMPLE),
             "Size of unsigned char data type on this machine is different "
             "than JPEG byte size.\n");
  JSAMPLE test_jsample = 0;
  test_jsample--;
  asfRequire(test_jsample == UCHAR_MAX,
             "Something wacky happened, like data overflow.\n");

  // Initializae libjpg structures.
  cinfo->err = jpeg_std_error (jerr);
  jpeg_create_compress (cinfo);

  // Open the output file to be used.
  *ojpeg = FOPEN(output_file_name, "wb");

  // Connect jpeg output to the output file to be used.
  jpeg_stdio_dest (cinfo, *ojpeg);

  // Set image parameters that libjpeg needs to know about.
  cinfo->image_width = meta->general->sample_count;
  cinfo->image_height = meta->general->line_count;
  if (rgb) {
    cinfo->in_color_space = JCS_RGB;
    cinfo->input_components = 3;
  }
  else {
    cinfo->in_color_space = JCS_GRAYSCALE;
    cinfo->input_components = 1;
  }
  jpeg_set_defaults (cinfo);   // Use default compression parameters.
  jpeg_set_quality(cinfo, 100, 1);


  // Reassure libjpeg that we will be writing a complete JPEG file.
  jpeg_start_compress (cinfo, TRUE);

  return;
}

void initialize_pgm_file(const char *output_file_name,
       meta_parameters *meta, FILE **opgm)
{
  const int max_color_value = 255;

  *opgm = FOPEN (output_file_name, "w");

  fprintf (*opgm, "%s\n", PGM_MAGIC_NUMBER);
  fprintf (*opgm, "%ld\n", (long int) meta->general->sample_count);
  fprintf (*opgm, "%ld\n", (long int) meta->general->line_count);
  fprintf (*opgm, "%d\n", max_color_value);

  return;
}

void initialize_polsarpro_file(const char *output_file_name,
			       meta_parameters *meta, FILE **ofp)
{
  *ofp = FOPEN(output_file_name, "wb");
  
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

void finalize_jpeg_file(FILE *ojpeg, struct jpeg_compress_struct *cinfo)
{
  jpeg_finish_compress (cinfo);
  FCLOSE (ojpeg);
  jpeg_destroy_compress (cinfo);
}

void finalize_png_file(FILE *opng, png_structp png_ptr, png_infop info_ptr)
{
    png_write_end(png_ptr, NULL);
    png_destroy_write_struct(&png_ptr, &info_ptr);
    FCLOSE(opng);
}

void finalize_ppm_file(FILE *oppm)
{
  FCLOSE(oppm);
}

void
export_band_image (const char *metadata_file_name,
                   const char *image_data_file_name,
                   char *output_file_name,
                   scale_t sample_mapping,
                   char **band_name, int rgb,
                   int true_color, int false_color,
                   char *look_up_table_name,
                   output_format_t format,
                   int *noutputs,
                   char ***output_names)
{
  int map_projected;
  int is_geotiff = 1;
  TIFF *otif = NULL; // FILE* pointer for TIFF files
  GTIF *ogtif = NULL;
  FILE *ojpeg = NULL, *opgm=NULL, *opng=NULL, *ofp=NULL;
  struct jpeg_compress_struct cinfo;
  png_structp png_ptr;
  png_infop png_info_ptr;
  //hid_t hdf5_file, hdf5_space, hdf5_data;
  netcdf_t *netcdf=NULL;
  float *nc = NULL;
  float *hdf5 = NULL;
  int ii,jj;
  int palette_color_tiff = 0;
  int have_look_up_table = look_up_table_name && strlen(look_up_table_name)>0;
  char *lut_file = NULL;

  meta_parameters *md = meta_read (metadata_file_name);
  map_projected = is_map_projected(md);

  if (format == PNG_GE)
    meta_write(md, output_file_name);

  if (md->general->image_data_type != POLARIMETRIC_MATRIX &&
      md->general->image_data_type != POLARIMETRIC_DECOMPOSITION &&
      format == POLSARPRO_HDR)
    append_ext_if_needed(output_file_name, ".bin", NULL);

  asfRequire( !(look_up_table_name == NULL &&
                sample_mapping == TRUNCATE &&
                (md->general->radiometry == r_SIGMA ||
                 md->general->radiometry == r_BETA  ||
                 md->general->radiometry == r_GAMMA)
               ),
              "Downsampling a Sigma, Beta, or Gamma type image (power or dB)\n"
              "from floating point to byte using truncation is not supported.\n"
              "All values would map to black.\n");
  if (md->general->data_type == BYTE &&
      sample_mapping != TRUNCATE && sample_mapping != NONE)
  {
      asfPrintWarning("Using %s sample remapping on BYTE data will result in\n"
                      "contrast expansion.  If you do not want contrast expansion in\n"
                      "your exported file, then you need to select either TRUNCATE or\n"
                      "NONE for your sample remapping type.\n",
                      sample_mapping2string(sample_mapping));
  }

  if (md->general->band_count < 1 || md->general->band_count > MAX_BANDS) {
    asfPrintError ("Unsupported number of channels found (%d).  Only 1 through\n"
        "%d channels are supported.\n", md->general->band_count, MAX_BANDS);
  }

  if (format == PGM && look_up_table_name != NULL && strlen(look_up_table_name) > 0) {
    asfPrintWarning("Cannot apply look up table to PGM format output files since\n"
        "color is not supported ...Ignoring the look up table and continuing.\n");
    have_look_up_table = 0;
  }

  // NOTE: if the truecolorFlag or falsecolorFlag was set, then 'rgb' is true
  // and the band assignments are in the band_name[] array already.  The true_color
  // and false_color parameters are provided separately just in case we decide
  // to do anything different, e.g. normal rgb processing plus something specific
  // to true_color or false_color (like contrast expansion)
  if (strstr(uc(md->general->bands), "POLSARPRO") != NULL && format == PGM) {
    asfPrintWarning(
        "Using PGM output for an image containing a PolSARpro classification\n"
        "band will result in separate greyscale images, and the PolSARpro classification\n"
        "will appear very DARK since all values in the image are small integers.  It is\n"
        "best to use a viewer that can apply the appropriate classification look-up table\n"
        "to the image, i.e. asf_view, a part of the ASF tool set.\n");
  }

  if (have_look_up_table) {
    lut_file = STRDUP(look_up_table_name);
  }
  else {
    // No look-up table
    lut_file = (char*)CALLOC(256, sizeof(char)); // Empty string
    if (format != PGM) {
      if (md->colormap) {
        // No look up table was provided for colormapped output ...Use the embedded
        // colormap that is in the metadata
        strcpy(lut_file, "tmp_lut_file.lut");
        colormap_to_lut_file(md->colormap, lut_file);
        have_look_up_table = TRUE;
        asfPrintStatus("\nUsing the colormap embedded in the ASF metadata (%s).\n"
            "for applicable bands.\n\n",
            md->colormap->look_up_table);
      }
    }
  }

  if (rgb && !have_look_up_table) {
    // Initialize the selected format
    if (format == TIF) {
      is_geotiff = 0;
      initialize_tiff_file(&otif, &ogtif, output_file_name,
         metadata_file_name, is_geotiff,
         sample_mapping, rgb, &palette_color_tiff, band_name, lut_file, 0);
    }
    else if (format == GEOTIFF) {
      initialize_tiff_file(&otif, &ogtif, output_file_name,
         metadata_file_name, is_geotiff,
         sample_mapping, rgb, &palette_color_tiff, band_name, lut_file, 0);
    }
    else if (format == JPEG) {
      initialize_jpeg_file(output_file_name, md, &ojpeg, &cinfo, rgb);
    }
    else if (format == PNG) {
      initialize_png_file(output_file_name, md, &opng, &png_ptr,
          &png_info_ptr, rgb);
    }
    else if (format == PNG_ALPHA) {
      initialize_png_file_ext(output_file_name, md, &opng, &png_ptr,
			      &png_info_ptr, rgb, TRUE);
    }
    else if (format == PNG_GE) {
      initialize_png_file_ext(output_file_name, md, &opng, &png_ptr,
			      &png_info_ptr, rgb, 2);
    }
    /*
    else if (format == HDF) {
      initialize_hdf5_file(output_file_name, md, 
			   &hdf5_file, &hdf5_space, &hdf5_data);
      hdf5 = (float *) 
	MALLOC(sizeof(float)*md->general->line_count*md->general->sample_count);
    }
    */
    else if (format == NC) {
      netcdf = (netcdf_t *) MALLOC(sizeof(netcdf_t));
      netcdf = initialize_netcdf_file(output_file_name, md);
      nc = (float *)
	MALLOC(sizeof(float)*md->general->line_count*md->general->sample_count);    }
    int ignored[4] = {0, 0, 0, 0};
    int red_channel=-1, green_channel=-1, blue_channel=-1;
    for (ii = 0; ii < 4; ii++) {
        if (ii < md->general->band_count) {
            ignored[ii] = strncmp("IGNORE", uc(band_name[ii]), 6) == 0 ? 1 : 0;
        }
        else {
            // Ignore bands that exceed band count
            ignored[ii] = 1;
        }
    }
    red_channel = get_band_number(md->general->bands,
                                  md->general->band_count,
                                  band_name[0]);
    if (!ignored[0] && !(red_channel >= 0 && red_channel < MAX_BANDS)) {
      asfPrintError("Band number (%d) out of range for %s channel.\n",
                    red_channel, "red");
    }
    green_channel = get_band_number(md->general->bands,
                                    md->general->band_count,
                                    band_name[1]);
    if (!ignored[1] && !(green_channel >= 0 && green_channel < MAX_BANDS)) {
      asfPrintError("Band number (%d) out of range for %s channel.\n",
                    green_channel, "green");
    }
    blue_channel = get_band_number(md->general->bands,
                                   md->general->band_count,
                                   band_name[2]);
    if (!ignored[2] && !(blue_channel >= 0 && blue_channel < MAX_BANDS)) {
      asfPrintError("Band number (%d) out of range for %s channel.\n",
                    blue_channel, "blue");
    }

    channel_stats_t red_stats, blue_stats, green_stats;
    red_stats.hist = NULL; red_stats.hist_pdf = NULL;
    green_stats.hist = NULL; green_stats.hist_pdf = NULL;
    blue_stats.hist = NULL; blue_stats.hist_pdf = NULL;

    if (!md->optical) {
      asfRequire (sizeof(unsigned char) == 1,
                  "Size of the unsigned char data type on this machine is "
                  "different than expected.\n");

        /*** Normal straight per-channel stats (no combined-band stats) */

        // Red channel statistics
        if (!ignored[red_channel]                           &&  // Non-blank band
            sample_mapping != NONE                          &&  // Float-to-byte resampling needed
            sample_mapping != HISTOGRAM_EQUALIZE            &&  // A histogram is not needed
            md->stats      != NULL                          &&  // Stats exist and are valid
            md->stats       > 0                             &&
            meta_is_valid_string(band_name[0])              &&  // Band name exists and is valid
            strlen(band_name[0]) > 0)
        {
          // If the stats already exist, then use them
          int band_no = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[0]);
          red_stats.min  = md->stats->band_stats[band_no].min;
          red_stats.max  = md->stats->band_stats[band_no].max;
          red_stats.mean = md->stats->band_stats[band_no].mean;
          red_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
          red_stats.hist     = NULL;
          red_stats.hist_pdf = NULL;
          if (sample_mapping == SIGMA) {
            double omin = red_stats.mean - 2*red_stats.standard_deviation;
            double omax = red_stats.mean + 2*red_stats.standard_deviation;
            if (omin > red_stats.min) red_stats.min = omin;
            if (omax < red_stats.max) red_stats.max = omax;
          }
        }
        else {
          // Calculate the stats if you have to...
          if (sample_mapping != NONE && !ignored[red_channel]) { // byte image
            asfPrintStatus("\nGathering red channel statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[0],
                                md->general->no_data,
                                &red_stats.min, &red_stats.max, &red_stats.mean,
                                &red_stats.standard_deviation, &red_stats.hist);
            if (sample_mapping == SIGMA) {
              double omin = red_stats.mean - 2*red_stats.standard_deviation;
              double omax = red_stats.mean + 2*red_stats.standard_deviation;
              if (omin > red_stats.min) red_stats.min = omin;
              if (omax < red_stats.max) red_stats.max = omax;
            }
            if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
              red_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
              gsl_histogram_pdf_init (red_stats.hist_pdf, red_stats.hist);
            }
          }
        }

        // Green channel statistics
        if (!ignored[green_channel]                          &&  // Non-blank band
             sample_mapping != NONE                          &&  // Float-to-byte resampling needed
             sample_mapping != HISTOGRAM_EQUALIZE            &&  // A histogram is not needed
             md->stats      != NULL                          &&  // Stats exist and are valid
             md->stats       > 0                             &&
             meta_is_valid_string(band_name[1])              &&  // Band name exists and is valid
             strlen(band_name[1]) > 0)
        {
          // If the stats already exist, then use them
          int band_no = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[1]);
          green_stats.min  = md->stats->band_stats[band_no].min;
          green_stats.max  = md->stats->band_stats[band_no].max;
          green_stats.mean = md->stats->band_stats[band_no].mean;
          green_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
          green_stats.hist     = NULL;
          green_stats.hist_pdf = NULL;
          if (sample_mapping == SIGMA) {
            double omin = green_stats.mean - 2*green_stats.standard_deviation;
            double omax = green_stats.mean + 2*green_stats.standard_deviation;
            if (omin > green_stats.min) green_stats.min = omin;
            if (omax < green_stats.max) green_stats.max = omax;
          }
        }
        else {
          // Calculate the stats if you have to...
          if (sample_mapping != NONE && !ignored[green_channel]) { // byte image
            asfPrintStatus("\nGathering green channel statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[1],
                                md->general->no_data,
                                &green_stats.min, &green_stats.max,
                                &green_stats.mean,
                                &green_stats.standard_deviation,
                                &green_stats.hist);
            if (sample_mapping == SIGMA) {
              double omin = green_stats.mean - 2*green_stats.standard_deviation;
              double omax = green_stats.mean + 2*green_stats.standard_deviation;
              if (omin > green_stats.min) green_stats.min = omin;
              if (omax < green_stats.max) green_stats.max = omax;
            }
            if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
              green_stats.hist_pdf = gsl_histogram_pdf_alloc(256);
              gsl_histogram_pdf_init (green_stats.hist_pdf, green_stats.hist);
            }
          }
        }

        // Blue channel statistics
        if (!ignored[blue_channel]                          &&  // Non-blank band
             sample_mapping != NONE                         &&  // Float-to-byte resampling needed
             sample_mapping != HISTOGRAM_EQUALIZE           &&  // A histogram is not needed
             md->stats      != NULL                         &&  // Stats exist and are valid
             md->stats       > 0                            &&
             meta_is_valid_string(band_name[2])             &&  // Band name exists and is valid
             strlen(band_name[2]) > 0)
        {
          // If the stats already exist, then use them
          int band_no = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[2]);
          blue_stats.min  = md->stats->band_stats[band_no].min;
          blue_stats.max  = md->stats->band_stats[band_no].max;
          blue_stats.mean = md->stats->band_stats[band_no].mean;
          blue_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
          blue_stats.hist     = NULL;
          blue_stats.hist_pdf = NULL;
          if (sample_mapping == SIGMA) {
            double omin = blue_stats.mean - 2*blue_stats.standard_deviation;
            double omax = blue_stats.mean + 2*blue_stats.standard_deviation;
            if (omin > blue_stats.min) blue_stats.min = omin;
            if (omax < blue_stats.max) blue_stats.max = omax;
          }
        }
        else {
          // Calculate the stats if you have to...
          if (sample_mapping != NONE && !ignored[blue_channel]) { // byte image
            asfPrintStatus("\nGathering blue channel statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[2],
                                md->general->no_data,
                                &blue_stats.min, &blue_stats.max,
                                &blue_stats.mean,
                                &blue_stats.standard_deviation,
                                &blue_stats.hist);
            if (sample_mapping == SIGMA) {
              double omin = blue_stats.mean - 2*blue_stats.standard_deviation;
              double omax = blue_stats.mean + 2*blue_stats.standard_deviation;
              if (omin > blue_stats.min) blue_stats.min = omin;
              if (omax < blue_stats.max) blue_stats.max = omax;
            }
            if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
              blue_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
              gsl_histogram_pdf_init (blue_stats.hist_pdf, blue_stats.hist);
            }
          }
        }
    }

    float *red_float_line = NULL;
    float *green_float_line = NULL;
    float *blue_float_line = NULL;

    unsigned char *red_byte_line = NULL;
    unsigned char *green_byte_line = NULL;
    unsigned char *blue_byte_line = NULL;

    // Write the data to the file
    FILE *fp = FOPEN(image_data_file_name, "rb");

    int sample_count = md->general->sample_count;
    int offset = md->general->line_count;

    // Allocate some memory
    if (md->optical || md->general->data_type == BYTE) {
      if (ignored[red_channel])
        red_byte_line = (unsigned char *) CALLOC(sample_count, sizeof(char));
      else
        red_byte_line = (unsigned char *) MALLOC(sample_count * sizeof(char));

      if (ignored[green_channel])
        green_byte_line = (unsigned char *) CALLOC(sample_count, sizeof(char));
      else
        green_byte_line = (unsigned char *) MALLOC(sample_count * sizeof(char));

      if (ignored[blue_channel])
        blue_byte_line = (unsigned char *) CALLOC(sample_count, sizeof(char));
      else
        blue_byte_line = (unsigned char *) MALLOC(sample_count * sizeof(char));
    }
    else {
      // Not optical data
      red_float_line = (float *) MALLOC(sample_count * sizeof(float));
      if (ignored[red_channel]){
        for (ii=0; ii<sample_count; ++ii) {
          red_float_line[ii] = md->general->no_data;
        }
      }

      green_float_line = (float *) MALLOC(sample_count * sizeof(float));
      if (ignored[green_channel]) {
        for (ii=0; ii<sample_count; ++ii) {
          green_float_line[ii] = md->general->no_data;
        }
      }

      blue_float_line = (float *) MALLOC(sample_count * sizeof(float));
      if (ignored[blue_channel]) {
        for (ii=0; ii<sample_count; ++ii) {
          blue_float_line[ii] = md->general->no_data;
        }
      }
    }

    double r_omin=0, r_omax=0;
    double g_omin=0, g_omax=0;
    double b_omin=0, b_omax=0;

    if (md->optical && (true_color || false_color)) {
      // NOTE: Using the stats from the metadata, if available, is only valid
      // if no histogram is necessary, else one must be generated via the
      // stats functions.  If true_color or false_color are selected, then sample_mapping
      // should NOT be HISTOGRAM_EQUALIZE in particular and should always be set
      // to SIGMA
      if (sample_mapping == NONE && (true_color || false_color)) {
          sample_mapping = SIGMA;
      }
      if (sample_mapping != SIGMA) {
        asfPrintWarning("Cannot combine true or false color options with sample mappings\n"
            "other than 2-sigma.  You selected %s.  Defaulting to 2-sigma...\n",
            sample_mapping == TRUNCATE ? "TRUNCATE" :
            sample_mapping == MINMAX ? "MINMAX" :
            sample_mapping == HISTOGRAM_EQUALIZE ? "HISTOGRAM_EQUALIZE" :
            "UNKNOWN or INVALID");
      }

      asfPrintStatus("\nSampling color channels for 2-sigma contrast-expanded %s output...\n",
                     true_color ? "True Color" : false_color ? "False Color" : "Unknown");

      // Set up red resampling
      if (md->stats                                     &&
          md->stats->band_count >= 3                    &&
          meta_is_valid_string(band_name[0])            &&
          strlen(band_name[0]) > 0                      &&
          sample_mapping != HISTOGRAM_EQUALIZE)
      {
          // If the stats already exist, then use them
        int band_no = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[0]);
        red_stats.min  = md->stats->band_stats[band_no].min;
        red_stats.max  = md->stats->band_stats[band_no].max;
        red_stats.mean = md->stats->band_stats[band_no].mean;
        red_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
        red_stats.hist     = NULL;
        red_stats.hist_pdf = NULL;
      }
      else {
        asfPrintStatus("\nGathering red channel statistics...\n");
        calc_stats_from_file(image_data_file_name, band_name[0],
                             md->general->no_data,
                             &red_stats.min, &red_stats.max, &red_stats.mean,
                             &red_stats.standard_deviation, &red_stats.hist);
      }
      r_omin = red_stats.mean - 2*red_stats.standard_deviation;
      r_omax = red_stats.mean + 2*red_stats.standard_deviation;
      if (r_omin < red_stats.min) r_omin = red_stats.min;
      if (r_omax > red_stats.max) r_omax = red_stats.max;

      // Set up green resampling
      if (md->stats                                       &&
          md->stats->band_count >= 3                      &&
          meta_is_valid_string(band_name[1])              &&
          strlen(band_name[1]) > 0                        &&
          sample_mapping != HISTOGRAM_EQUALIZE)
      {
        // If the stats already exist, then use them
        int band_no = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[1]);
        green_stats.min  = md->stats->band_stats[band_no].min;
        green_stats.max  = md->stats->band_stats[band_no].max;
        green_stats.mean = md->stats->band_stats[band_no].mean;
        green_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
        green_stats.hist     = NULL;
        green_stats.hist_pdf = NULL;
      }
      else {
        asfPrintStatus("\nGathering green channel statistics...\n");
        calc_stats_from_file(image_data_file_name, band_name[1],
                              md->general->no_data,
                              &green_stats.min, &green_stats.max, &green_stats.mean,
                              &green_stats.standard_deviation, &green_stats.hist);
      }
      g_omin = green_stats.mean - 2*green_stats.standard_deviation;
      g_omax = green_stats.mean + 2*green_stats.standard_deviation;
      if (g_omin < green_stats.min) g_omin = green_stats.min;
      if (g_omax > green_stats.max) g_omax = green_stats.max;

      // Set up blue resampling
      if (md->stats                                      &&
          md->stats->band_count >= 3                     &&
          meta_is_valid_string(band_name[2])             &&
          strlen(band_name[2]) > 0                       &&
          sample_mapping != HISTOGRAM_EQUALIZE)
      {
        // If the stats already exist, then use them
        int band_no = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[2]);
        blue_stats.min  = md->stats->band_stats[band_no].min;
        blue_stats.max  = md->stats->band_stats[band_no].max;
        blue_stats.mean = md->stats->band_stats[band_no].mean;
        blue_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
        blue_stats.hist     = NULL;
        blue_stats.hist_pdf = NULL;
      }
      else {
        asfPrintStatus("\nGathering blue channel statistics...\n\n");
        calc_stats_from_file(image_data_file_name, band_name[2],
                             md->general->no_data,
                             &blue_stats.min, &blue_stats.max, &blue_stats.mean,
                             &blue_stats.standard_deviation, &blue_stats.hist);
      }
      b_omin = blue_stats.mean - 2*blue_stats.standard_deviation;
      b_omax = blue_stats.mean + 2*blue_stats.standard_deviation;
      if (b_omin < blue_stats.min) b_omin = blue_stats.min;
      if (b_omax > blue_stats.max) b_omax = blue_stats.max;

      asfPrintStatus("Applying 2-sigma contrast expansion to color bands...\n\n");
    }

    for (ii=0; ii<md->general->line_count; ii++) {
      if (md->optical || md->general->data_type == BYTE) {
        // Optical images come as byte in the first place
        if (!ignored[red_channel])
          get_byte_line(fp, md, ii+red_channel*offset, red_byte_line);
        if (!ignored[green_channel])
          get_byte_line(fp, md, ii+green_channel*offset, green_byte_line);
        if (!ignored[blue_channel])
          get_byte_line(fp, md, ii+blue_channel*offset, blue_byte_line);
        // If true or false color flag was set, then (re)sample with 2-sigma
        // contrast expansion
        if (true_color || false_color) {
          for (jj=0; jj<sample_count; jj++) {
            red_byte_line[jj] =
                pixel_float2byte((float)red_byte_line[jj], SIGMA,
                                  r_omin, r_omax,
                                  red_stats.hist, red_stats.hist_pdf, NAN);
            green_byte_line[jj] =
                pixel_float2byte((float)green_byte_line[jj], SIGMA,
                                  g_omin, g_omax,
                                  green_stats.hist, green_stats.hist_pdf, NAN);
            blue_byte_line[jj] =
                pixel_float2byte((float)blue_byte_line[jj], SIGMA,
                                  b_omin, b_omax,
                                  blue_stats.hist, blue_stats.hist_pdf, NAN);
          }
        }
        if (format == TIF || format == GEOTIFF)
          write_rgb_tiff_byte2byte(otif, red_byte_line, green_byte_line,
                                   blue_byte_line, ii, sample_count);
        else if (format == JPEG)
          write_rgb_jpeg_byte2byte(ojpeg, red_byte_line, green_byte_line,
                                   blue_byte_line, &cinfo, sample_count);
        else if (format == PNG || format == PNG_ALPHA || format == PNG_GE)
          write_rgb_png_byte2byte(opng, red_byte_line, green_byte_line,
                                  blue_byte_line, png_ptr, png_info_ptr,
                                  sample_count);
        else
          asfPrintError("Impossible: unexpected format %d\n", format);
      }
      else if (sample_mapping == NONE) {
        // Write float->float lines if float image
        if (!ignored[red_channel])
          get_float_line(fp, md, ii+red_channel*offset, red_float_line);
        if (!ignored[green_channel])
          get_float_line(fp, md, ii+green_channel*offset, green_float_line);
        if (!ignored[blue_channel])
          get_float_line(fp, md, ii+blue_channel*offset, blue_float_line);
        if (format == GEOTIFF || format == TIF)
          write_rgb_tiff_float2float(otif, red_float_line, green_float_line,
                                     blue_float_line, ii, sample_count);
        else
          asfPrintError("Impossible: unexpected format %d\n", format);
      }
      else {
        // Write float->byte lines if byte image
        if (!ignored[red_channel])
          get_float_line(fp, md, ii+red_channel*offset, red_float_line);
        if (!ignored[green_channel])
          get_float_line(fp, md, ii+green_channel*offset, green_float_line);
        if (!ignored[blue_channel])
          get_float_line(fp, md, ii+blue_channel*offset, blue_float_line);
        if (format == TIF || format == GEOTIFF)
          write_rgb_tiff_float2byte(otif, red_float_line, green_float_line,
                                    blue_float_line, red_stats, green_stats,
                                    blue_stats, sample_mapping,
                                    md->general->no_data, ii, sample_count);
        else if (format == JPEG)
          write_rgb_jpeg_float2byte(ojpeg, red_float_line, green_float_line,
                                    blue_float_line, &cinfo, red_stats,
                                    green_stats, blue_stats, sample_mapping,
                                    md->general->no_data, sample_count);
        else if (format == PNG || format == PNG_ALPHA || format == PNG_GE)
          write_rgb_png_float2byte(opng, red_float_line, green_float_line,
                                   blue_float_line, png_ptr, png_info_ptr,
                                   red_stats, green_stats, blue_stats,
                                   sample_mapping, md->general->no_data,
                                   sample_count);
        else
          asfPrintError("Impossible: unexpected format %d\n", format);
      }

      asfLineMeter(ii, md->general->line_count);
    }

    // Free memory
    FREE(red_byte_line);
    FREE(green_byte_line);
    FREE(blue_byte_line);
    FREE(red_float_line);
    FREE(green_float_line);
    FREE(blue_float_line);

    // Finalize the chosen format
    if (format == TIF || format == GEOTIFF)
      finalize_tiff_file(otif, ogtif, is_geotiff);
    else if (format == JPEG)
      finalize_jpeg_file(ojpeg, &cinfo);
    else if (format == PNG || format == PNG_ALPHA || format == PNG_GE)
      finalize_png_file(opng, png_ptr, png_info_ptr);
    /*
    else if (format == HDF)
      finalize_hdf5_file(hdf5_file, hdf5_space, hdf5_data, hdf5);
    */
    else if (format == NC) {
      finalize_netcdf_file(netcdf, md, nc);
    }
    else
      asfPrintError("Impossible: unexpected format %d\n", format);

    if (red_stats.hist) gsl_histogram_free(red_stats.hist);
    if (red_stats.hist_pdf) gsl_histogram_pdf_free(red_stats.hist_pdf);
    if (green_stats.hist) gsl_histogram_free(green_stats.hist);
    if (green_stats.hist_pdf) gsl_histogram_pdf_free(green_stats.hist_pdf);
    if (blue_stats.hist) gsl_histogram_free(blue_stats.hist);
    if (blue_stats.hist_pdf) gsl_histogram_pdf_free(blue_stats.hist_pdf);

    FCLOSE(fp);

    // set the output filename
    *noutputs = 1;
    char **outs = MALLOC(sizeof(char*));
    outs[0] = STRDUP(output_file_name);
    *output_names = outs;
  }
  else {
    // Single-band image output (one grayscale file for each available band)
    int free_band_names=FALSE;
    int band_count = md->general->band_count;
    char base_name[255];
    char *bands = (char *) MALLOC(sizeof(char)*1024);
    strcpy(bands, md->general->bands);
    strcpy(base_name, output_file_name);
    char *matrix = (char *) MALLOC(sizeof(char)*5);
    char *decomposition = (char *) MALLOC(sizeof(char)*25);
    char *path_name = (char *) MALLOC(sizeof(char)*1024);
    char *out_file = NULL;

    if (!band_name)
    {
      // caller did not pass in the band names -- we will have
      // to come up with some band names ourselves
      if (band_count == 1) {
        // only one band, just call it "01"
        band_name = (char **) CALLOC(MAX_BANDS, sizeof(char*));
        band_name[0] = (char*) MALLOC(sizeof(char)*100);
        strcpy(band_name[0], "01");
      }
      else if (have_look_up_table) {
        // when exporting a look up table, number the bands
        band_name = (char **) CALLOC(MAX_BANDS, sizeof(char*));
        int i;
        for (i=0; i<3; i++) {
          band_name[i] = (char*) MALLOC(sizeof(char)*100);
          sprintf(band_name[i], "%02d", i + 1);
        }
      }
      else {
        // get what is in the metadata
        int n;
        char *b = stripExt(image_data_file_name);
        band_name = find_single_band(b, "all", &n);
        asfRequire (n == band_count, "Band count inconsistent: %d != %d\n",
                    n, band_count);
        FREE(b);
      }

      // in all three cases, we must free "band_name"
      // (normally not freed, it the caller's)
      free_band_names = TRUE;
    }

    // If we are dealing with polarimetric matrices, the output file name
    // becomes the name of an output directory. We need to figure out from
    // the bands string, what kind of matrix we have, because we need to
    // create the appropriate subdirectory. Otherwise, PolSARPro can't handle
    // the files out of the box.
    if (md->general->image_data_type == POLARIMETRIC_MATRIX &&
	md->general->band_count != 1) {
      snprintf(matrix, 3, "%s", band_name[band_count-1]);
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
	asfPrintError("Output directory (%s) already exists.\n", path_name);
      else if(create_dir(path_name) == -1)
	asfPrintError("Can't generate output directory (%s).\n", path_name);
      char *configFile = 
	(char *) MALLOC(sizeof(char)*(strlen(path_name)+15));
      sprintf(configFile, "%s%cconfig.txt", path_name, DIR_SEPARATOR);
      FILE *fpConfig = FOPEN(configFile, "w");
      fprintf(fpConfig, "Nrow\n%d\n", md->general->line_count);
      fprintf(fpConfig, "---------\nNcol\n%d\n", md->general->sample_count);
      fprintf(fpConfig, "---------\nPolarCase\nmonostatic\n");
      fprintf(fpConfig, "---------\nPolarType\nfull\n");
      FCLOSE(fpConfig);
      FREE(configFile);
      FREE(path);
      FREE(dirName);
      FREE(fileName);
    }

    // We treat polarimetric decompositions in a similar way. The output file
    // name becomes the name of an output directory again. The actual file
    // names can be extracted from the bands string.
    if (md->general->image_data_type == POLARIMETRIC_DECOMPOSITION &&
	md->general->band_count != 1) {
      strcpy(decomposition, band_name[band_count-1]);
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
	asfPrintError("Output directory (%s) already exists.\n", path_name);
      else if(create_dir(path_name) == -1)
	asfPrintError("Can't generate output directory (%s).\n", path_name);
      char *configFile = 
	(char *) MALLOC(sizeof(char)*(strlen(path_name)+15));
      sprintf(configFile, "%s%cconfig.txt", path_name, DIR_SEPARATOR);
      FILE *fpConfig = FOPEN(configFile, "w");
      fprintf(fpConfig, "Nrow\n%d\n", md->general->line_count);
      fprintf(fpConfig, "---------\nNcol\n%d\n", md->general->sample_count);
      fprintf(fpConfig, "---------\nPolarCase\nmonostatic\n");
      fprintf(fpConfig, "---------\nPolarType\nfull\n");
      FCLOSE(fpConfig);
      FREE(configFile);
      FREE(path);
      FREE(dirName);
      FREE(fileName);      
    }

    // store the names of the generated files here
    *noutputs = 0;
    *output_names = MALLOC(sizeof(char*) * band_count);

    int kk;
    int is_colormap_band;

    for (kk=0; kk<band_count; kk++) {
      if (band_name[kk]) {
        is_colormap_band = FALSE;
        // two ways we can be applying a colormap to this band:
        //  (1) the metadata has an embedded colormap, and it's band_id
        //      is this band.
        //  (2) user used the "-lut" option -- we have the !md->colormap
        //      here to avoid thinking we've got -lut when it was really
        //      just the md->colormap (which sets have_look_up_table, above)
        int is_polsarpro = 
          (md->general->image_data_type == POLARIMETRIC_IMAGE || 
          md->general->image_data_type == POLARIMETRIC_SEGMENTATION || 
          md->general->image_data_type == POLARIMETRIC_DECOMPOSITION || 
          md->general->image_data_type == POLARIMETRIC_PARAMETER ||
          md->general->image_data_type == POLARIMETRIC_MATRIX) ? 1 : 0;
        if (
          ( md->colormap && strcmp_case(band_name[kk], md->colormap->band_id)==0) ||
          (!md->colormap && have_look_up_table && md->general->data_type == BYTE) ||
          (!md->colormap && have_look_up_table &&
          md->general->data_type != BYTE && sample_mapping != NONE))
        {
          is_colormap_band = TRUE;
          sample_mapping = is_polsarpro ? TRUNCATE : sample_mapping;
        }
	// skip the 'AMP' band if we have POlSARPro data and the user wants
	// to apply a LUT
	if (strcmp_case(band_name[kk], "AMP") == 0 && is_polsarpro  &&
	    band_count > 1)
	  continue;

        if (format == POLSARPRO_HDR) {
          is_colormap_band = FALSE;
          sample_mapping = NONE;
        }

        if (have_look_up_table && is_colormap_band) {
          asfPrintStatus("\nApplying %s color look up table...\n\n", look_up_table_name);
        }

        out_file = (char *) MALLOC(sizeof(char)*1024);
        strcpy(out_file, output_file_name);

	// We enforce a byte conversion using truncate for polarimetric segmentation, regardless of
	// applying a look up table
	if (md->general->image_data_type == POLARIMETRIC_SEGMENTATION &&
	    md->colormap)
	  sample_mapping = TRUNCATE;

        // Initialize the selected format
        // NOTE: For PolSARpro, the first band is amplitude and should be
        // written out as a single-band greyscale image while the second
        // band is a classification and should be written out as color ...
        // and for TIFF formats, as a palette color tiff.
        // The only exception to this rule are polarimetric matrices
        if (md->general->image_data_type == POLARIMETRIC_MATRIX &&
          md->general->band_count != 1) {
            int ll, found_band = FALSE;
            int band_count;
            if (strcmp(matrix, "T3") == 0)
              band_count = 9;
            if (strcmp(matrix, "T4") == 0)
              band_count = 16;
            if (strcmp(matrix, "C2") == 0)
              band_count = 4;
            if (strcmp(matrix, "C3") == 0)
              band_count = 9;
            if (strcmp(matrix, "C4") == 0)
              band_count = 16;
            for (ll=0; ll<band_count; ll++) {
              if (strcmp(matrix, "T3") == 0 && 
                strncmp(band_name[kk], t3_matrix[ll], 
                strlen(band_name[kk])) == 0)
                found_band = TRUE;
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
            if (!found_band)
              continue;
            sprintf(out_file, "%s%c%s", 
              path_name, DIR_SEPARATOR, band_name[kk]);
          }
        else if (md->general->image_data_type == POLARIMETRIC_DECOMPOSITION &&
          md->general->band_count != 1) {
            int ll, found_band = FALSE;
            int band_count;
            if (strcmp(decomposition, "Freeman2_Vol") == 0)
              band_count = 2;
            else if (strcmp(decomposition, "Freeman_Vol") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "VanZyl3_Vol") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "Yamaguchi3_Vol") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "Yamaguchi4_Vol") == 0)
              band_count = 4;
            else if (strcmp(decomposition, "Krogager_Ks") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "TSVM_alpha_s3") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "TSVM_phi_s3") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "TSVM_tau_m3") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "TSVM_psi3") == 0)
              band_count = 3;
            else if (strcmp(decomposition, "TSVM_psi") == 0)
              band_count = 4;
            for (ll=0; ll<band_count; ll++) {
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
            if (!found_band)
              continue;
            sprintf(out_file, "%s%c%s", 
              path_name, DIR_SEPARATOR, band_name[kk]);

          }
        else if (md->general->image_data_type == POLARIMETRIC_SEGMENTATION ||
          md->general->image_data_type == POLARIMETRIC_PARAMETER)
          append_band_ext(base_name, out_file, NULL);
        else {
          if (band_count > 1) {
	    if (strcmp_case(band_name[kk], "INTERFEROGRAM_PHASE") == 0 &&
		is_colormap_band)
	      append_band_ext(base_name, out_file, "INTERFEROGRAM_RGB");
	    else
	      append_band_ext(base_name, out_file, band_name[kk]);
	  }
          else
            append_band_ext(base_name, out_file, NULL);
        }

        if (strcmp(band_name[0], MAGIC_UNSET_STRING) != 0)
          asfPrintStatus("\nWriting band '%s' ...\n", band_name[kk]);

        if (format == TIF || format == GEOTIFF) {
          is_geotiff = (format == GEOTIFF) ? 1 : 0;
          append_ext_if_needed (out_file, ".tif", ".tiff");
          if (is_colormap_band && strlen(lut_file) > 0) {
            //sample_mapping = TRUNCATE;
            rgb = FALSE;
            initialize_tiff_file(&otif, &ogtif, out_file,
              metadata_file_name, is_geotiff,
              sample_mapping, rgb,
              &palette_color_tiff, band_name,
              lut_file, TRUE);
          }
          else {
            initialize_tiff_file(&otif, &ogtif, out_file,
              metadata_file_name, is_geotiff,
              sample_mapping, rgb,
              &palette_color_tiff, band_name,
              NULL, FALSE);
          }
        }
        else if (format == JPEG) {
          append_ext_if_needed (out_file, ".jpg", ".jpeg");
          if (is_colormap_band) {
            initialize_jpeg_file(out_file, md,
              &ojpeg, &cinfo, TRUE);
          }
          else {
            initialize_jpeg_file(out_file, md,
              &ojpeg, &cinfo, rgb);
          }
        }
        else if (format == PNG) {
          append_ext_if_needed (out_file, ".png", NULL);
          if (is_colormap_band) {
            initialize_png_file(out_file, md,
              &opng, &png_ptr, &png_info_ptr, TRUE);
          }
          else {
            initialize_png_file(out_file, md,
              &opng, &png_ptr, &png_info_ptr, rgb);
          }
        }
        else if (format == PNG_ALPHA) {
          append_ext_if_needed (out_file, ".png", NULL);
          initialize_png_file_ext(out_file, md,
            &opng, &png_ptr, &png_info_ptr, FALSE, TRUE);
        }
        else if (format == PNG_GE) {
          append_ext_if_needed (out_file, ".png", NULL);
          initialize_png_file_ext(out_file, md,
            &opng, &png_ptr, &png_info_ptr, 1, 2);
        }
        else if (format == PGM) {
          append_ext_if_needed (out_file, ".pgm", ".pgm");
          initialize_pgm_file(out_file, md, &opgm);
        }
        else if (format == POLSARPRO_HDR) {
          append_ext_if_needed (out_file, ".bin", NULL);
          initialize_polsarpro_file(out_file, md, &ofp);
        }
	/*
	else if (format == HDF) {
	  append_ext_if_needed (out_file, ".he5", NULL);
	  initialize_hdf5_file(out_file, md, 
			       &hdf5_file, &hdf5_space, &hdf5_data);
	  hdf5 = (float *) 
	    MALLOC(sizeof(float)*md->general->line_count*md->general->sample_count);
	}
	*/
        else if (format == NC) {
          append_ext_if_needed (out_file, ".nc", NULL);
	  netcdf = initialize_netcdf_file(output_file_name, md);
	  nc = (float *)
	    MALLOC(sizeof(float)*md->general->line_count*
		   md->general->sample_count);        
	}

        else {
          asfPrintError("Impossible: unexpected format %d\n", format);
        }

        (*output_names)[*noutputs] = STRDUP(out_file);
        *noutputs += 1;

        // Determine which channel to read
        int channel;
        if (md->general->image_data_type >  POLARIMETRIC_IMAGE &&
          md->general->image_data_type <= POLARIMETRIC_MATRIX)
          channel = kk;
        else {
          if (md->general->band_count == 1)
            channel = 0;
          else
            channel = get_band_number(bands, band_count, band_name[kk]);
          asfRequire(channel >= 0 && channel <= MAX_BANDS,
            "Band number out of range\n");
        }

        int sample_count = md->general->sample_count;
        int offset = md->general->line_count;

        // Get the statistics if necessary
        channel_stats_t stats;
        stats.hist = NULL; stats.hist_pdf = NULL;

        if (sample_mapping != NONE && sample_mapping != TRUNCATE)
        {
          asfRequire (sizeof(unsigned char) == 1,
            "Size of the unsigned char data type on this machine is "
            "different than expected.\n");
          if (md->stats                  &&
            md->stats->band_count > 0  &&
            meta_is_valid_double(md->stats->band_stats[channel].mean) &&
            meta_is_valid_double(md->stats->band_stats[channel].min) &&
            meta_is_valid_double(md->stats->band_stats[channel].max) &&
            meta_is_valid_double(md->stats->band_stats[channel].std_deviation) &&
            sample_mapping != HISTOGRAM_EQUALIZE)
          {
            asfPrintStatus("Using metadata statistics - skipping stats computations.\n");
            stats.min  = md->stats->band_stats[channel].min;
            stats.max  = md->stats->band_stats[channel].max;
            stats.mean = md->stats->band_stats[channel].mean;
            stats.standard_deviation = md->stats->band_stats[channel].std_deviation;
            stats.hist = NULL;
          }
          else {
            asfPrintStatus("Gathering statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[kk],
              md->general->no_data,
              &stats.min, &stats.max, &stats.mean,
              &stats.standard_deviation, &stats.hist);
          }
          if (sample_mapping == TRUNCATE && !have_look_up_table) {
            if (stats.mean >= 255)
              asfPrintWarning("The image contains HIGH values and will turn out very\n"
              "bright or all-white.\n  Min : %f\n  Max : %f\n  Mean: %f\n"
              "=> Consider using a sample mapping method other than TRUNCATE\n",
              stats.min, stats.max, stats.mean);
            if (stats.mean < 10)
              asfPrintWarning("The image contains LOW values and will turn out very\n"
              "dark or all-black.\n  Min : %f\n  Max : %f\n  Mean: %f\n"
              "=> Consider using a sample mapping method other than TRUNCATE\n",
              stats.min, stats.max, stats.mean);
          }
          if (sample_mapping == SIGMA)
          {
            double omin = stats.mean - 2*stats.standard_deviation;
            double omax = stats.mean + 2*stats.standard_deviation;
            if (omin > stats.min) stats.min = omin;
            if (omax < stats.max) stats.max = omax;
          }
          if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
            stats.hist_pdf = gsl_histogram_pdf_alloc (256); //NUM_HIST_BINS);
            gsl_histogram_pdf_init (stats.hist_pdf, stats.hist);
          }
        }

        // Write the output image
        FILE *fp = FOPEN(image_data_file_name, "rb");
        float *float_line = (float *) MALLOC(sizeof(float) * sample_count);
        unsigned char *byte_line = MALLOC(sizeof(unsigned char) * sample_count);

        asfPrintStatus("Writing output file...\n");
        if (is_colormap_band)
        { // Apply look up table
          for (ii=0; ii<md->general->line_count; ii++ ) {
            if (md->optical || md->general->data_type == BYTE) {
              get_byte_line(fp, md, ii+channel*offset, byte_line);
              if (format == TIF || format == GEOTIFF)
                write_tiff_byte2lut(otif, byte_line, ii, sample_count,
                lut_file);
              else if (format == JPEG)
                write_jpeg_byte2lut(ojpeg, byte_line, &cinfo, sample_count,
                lut_file);
              else if (format == PNG || format == PNG_ALPHA || format == PNG_GE)
                write_png_byte2lut(opng, byte_line, png_ptr, png_info_ptr,
                sample_count, lut_file);
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            else {
              // Force a sample mapping of TRUNCATE for PolSARpro classifications
              // (They contain low integer values stored in floats ...contrast
              //  expansion will break the look up in the look up table.)
              get_float_line(fp, md, ii+channel*offset, float_line);
              if (format == TIF || format == GEOTIFF)
                // Unlike the other graphics file formats, the TIFF file uses an embedded
                // colormap (palette) and therefore each pixel should be a single byte value
                // where each byte value is an index into the colormap.  The other graphics
                // file formats use interlaced RGB lines and no index or colormap instead.
                write_tiff_float2byte(otif, float_line, stats,
                //is_colormap_band ? TRUNCATE : sample_mapping,
                sample_mapping,
                md->general->no_data, ii, sample_count);
              else if (format == JPEG)
                // Use lut to write an RGB line to the file
                write_jpeg_float2lut(ojpeg, float_line, &cinfo, stats,
                //is_colormap_band ? TRUNCATE : sample_mapping,
                sample_mapping,
                md->general->no_data,
                sample_count, lut_file);
              else if (format == PNG || format == PNG_ALPHA || format == PNG_GE)
                // Use lut to write an RGB line to the file
                write_png_float2lut(opng, float_line, png_ptr, png_info_ptr,
                stats,
                //is_colormap_band ? TRUNCATE : sample_mapping,
                sample_mapping,
                md->general->no_data,
                sample_count, lut_file);
              else if (format == PGM) {
                // Can't put color in a PGM file, so map it to greyscale and write that
                write_pgm_float2byte(opgm, float_line, stats,
                  //is_colormap_band ? TRUNCATE : sample_mapping,
                  sample_mapping,
                  md->general->no_data, sample_count);
              }
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            asfLineMeter(ii, md->general->line_count);
          }
        }
        else {
          // Regular old single band image (no look up table applied)
          for (ii=0; ii<md->general->line_count; ii++ ) {
            if (md->optical || md->general->data_type == BYTE) {
              get_byte_line(fp, md, ii+channel*offset, byte_line);
              if (format == TIF || format == GEOTIFF)
                write_tiff_byte2byte(otif, byte_line, stats, sample_mapping,
                sample_count, ii);
              else if (format == JPEG)
                write_jpeg_byte2byte(ojpeg, byte_line, stats, sample_mapping,
                &cinfo, sample_count);
              else if (format == PNG)
                write_png_byte2byte(opng, byte_line, stats, sample_mapping,
                png_ptr, png_info_ptr, sample_count);
              else if (format == PNG_ALPHA) {
                asfPrintError("PNG_ALPHA not supported.\n");
              }
              else if (format == PNG_GE)
                write_png_byte2rgbalpha(opng, byte_line, stats, sample_mapping,
                                        png_ptr, png_info_ptr, sample_count);
              else if (format == PGM)
                write_pgm_byte2byte(opgm, byte_line, stats, sample_mapping,
                sample_count);
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            else if (sample_mapping == NONE && !is_colormap_band) {
              get_float_line(fp, md, ii+channel*offset, float_line);
              if (format == GEOTIFF || format == TIF)
                write_tiff_float2float(otif, float_line, ii);
              else if (format == POLSARPRO_HDR) {
                int sample;
                for (sample=0; sample<sample_count; sample++)
                  ieee_lil32(float_line[sample]);
                fwrite(float_line,4,sample_count,ofp);
              }
	      /*
	      else if (format == HDF) {
		int sample;
		for (sample=0; sample<sample_count; sample++)
		  hdf5[ii*sample_count+sample] = float_line[sample];
	      }
	      */
	      else if (format == NC) {
		int sample;
		for (sample=0; sample<sample_count; sample++) {
		  nc[ii*sample_count+sample] = float_line[sample];
		}
	      }
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            else {
              get_float_line(fp, md, ii+channel*offset, float_line);
              if (format == TIF || format == GEOTIFF)
                write_tiff_float2byte(otif, float_line, stats,
                //is_colormap_band ? TRUNCATE : sample_mapping,
                sample_mapping,
                md->general->no_data, ii, sample_count);
              else if (format == JPEG)
                write_jpeg_float2byte(ojpeg, float_line, &cinfo, stats,
                //is_colormap_band ? TRUNCATE : sample_mapping,
                sample_mapping,
                md->general->no_data,
                sample_count);
              else if (format == PNG || format == PNG_ALPHA || format == PNG_GE)
                write_png_float2byte(opng, float_line, png_ptr, png_info_ptr,
                stats,
                //is_colormap_band ? TRUNCATE : sample_mapping,
                sample_mapping,
                md->general->no_data,
                sample_count);
              else if (format == PGM)
                write_pgm_float2byte(opgm, float_line, stats,
                //is_colormap_band ? TRUNCATE : sample_mapping,
                sample_mapping,
                md->general->no_data, sample_count);
              else if (format == POLSARPRO_HDR) {
                int sample;
                for (sample=0; sample<sample_count; sample++)
                  ieee_lil32(float_line[sample]);
                fwrite(float_line,4,sample_count,ofp);
              }
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            asfLineMeter(ii, md->general->line_count);
          } // End for each line
        } // End if multi or single band
        // Free memory
        FREE(float_line);
        FREE(byte_line);
        if (stats.hist) gsl_histogram_free(stats.hist);
        if (stats.hist_pdf) gsl_histogram_pdf_free(stats.hist_pdf);

        // Finalize the chosen format
        if (format == TIF || format == GEOTIFF)
          finalize_tiff_file(otif, ogtif, is_geotiff);
        else if (format == JPEG)
          finalize_jpeg_file(ojpeg, &cinfo);
        else if (format == PNG || format == PNG_ALPHA || format == PNG_GE)
          finalize_png_file(opng, png_ptr, png_info_ptr);
        else if (format == PGM)
          finalize_ppm_file(opgm);
        else if (format == POLSARPRO_HDR)
          FCLOSE(ofp);
	/*
	else if (format == HDF5)
	  finalize_hdf5_file(hdf5_file, hdf5_space, hdf5_data, hdf5);
	*/
	else if (format == NC) {
	  asfPrintStatus("Storing band '%s' ...\n", band_name[kk]);
	  nc_put_var_float(netcdf->ncid, netcdf->var_id[kk], &nc[0]);
	}

        FCLOSE(fp);
      }
    } // End for each band (kk is band number)

    if (format == NC)
      finalize_netcdf_file(netcdf, md, nc);

    if (free_band_names) {
      for (ii=0; ii<band_count; ++ii)
        FREE(band_name[ii]);
      FREE(band_name);
    }
    FREE(path_name);
    FREE(matrix);
    FREE(decomposition);
    FREE(out_file);
  }

  if (lut_file && strstr(lut_file, "tmp_lut_file.lut") && fileExists(lut_file)) {
    // If a temporary look-up table was generated (from an embedded colormap in the
    // metadata) then remove it now.
    remove(lut_file);
  }
  FREE(lut_file);
  meta_free (md);
}

char *sample_mapping2string(scale_t sample_mapping)
{
    switch(sample_mapping) {
        case TRUNCATE:
            return "TRUNCATE";
            break;
        case MINMAX:
            return "MIN-MAX";
            break;
        case SIGMA:
            return "2-SIGMA";
            break;
        case HISTOGRAM_EQUALIZE:
            return "HISTOGRAM EQUALIZE";
            break;
        case NONE:
            return "NONE";
            break;
        default:
            return "UNKNOWN or UNRECOGNIZED";
            break;
    }
}

void colormap_to_lut_file(meta_colormap *cm, const char *lut_file)
{
  int i;
  char line[256];

  FILE *fp = FOPEN(lut_file, "wt");
  fprintf(fp, "# Temporary look-up table file for asf_export\n");
  fprintf(fp, "# Look-up table     : %s\n", cm->look_up_table);
  fprintf(fp, "# Number of elements: %d\n", cm->num_elements);
  fprintf(fp, "# Index   Red   Green   Blue\n");
  for (i=0; i<cm->num_elements; i++) {
    sprintf(line, "%d %d %d %d\n",
            i, cm->rgb[i].red, cm->rgb[i].green, cm->rgb[i].blue);
    fprintf(fp, line);
  }
  FCLOSE(fp);
}

void write_insar_xml(output_format_t format, 
                     char *in_meta_name, char *in_data_name, char *out_name)
{
  if (!in_meta_name) return;
  if (!fileExists(in_meta_name)) return;
  if (!endsWith(in_meta_name, ".meta")) return;

  meta_parameters *meta = meta_read(in_meta_name);
  if (!meta) return;
  assert(meta->general);
 
  // Export interferometric phase 
  if (meta->general->image_data_type == INSAR_STACK) {
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

  // Write InSAR metadata
  if (meta->general->image_data_type == INSAR_STACK && meta->insar) {
    char *output_file_name = 
      (char *) MALLOC(sizeof(char)*(strlen(in_meta_name)+10));
    sprintf(output_file_name, "%s.xml", get_basename(out_name));
    asfPrintStatus("\nWriting InSAR metadata (%s) ...\n", output_file_name);
    FILE *fp = FOPEN(output_file_name, "wt");
    fprintf(fp, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\""
	    "?>\n");
    fprintf(fp, "<insar>\n");
    fprintf(fp, "  <processor>%s</processor>\n", meta->insar->processor);
    fprintf(fp, "  <master_image>%s</master_image>\n", 
	    meta->insar->master_image);
    fprintf(fp, "  <slave_image>%s</slave_image>\n", meta->insar->slave_image);
    fprintf(fp, "  <center_look_angle>%.4lf</center_look_angle>\n",
	    meta->insar->center_look_angle);
    fprintf(fp, "  <doppler units=\"Hz\">%.4lf</doppler>\n",
	    meta->insar->doppler);
    fprintf(fp, "  <doppler_rate units=\"Hz/m\">%.8lf</doppler_rate>\n",
	    meta->insar->doppler_rate);
    fprintf(fp, "  <baseline_length units=\"m\">%.1lf</baseline_length>\n",
	    meta->insar->baseline_length);
    fprintf(fp, "  <baseline_parallel units=\"m\">%.1lf</baseline_parallel>\n",
	    meta->insar->baseline_parallel);
    fprintf(fp, "  <baseline_parallel_rate units=\"m/s\">%.8lf"
	    "</baseline_parallel_rate>\n", 
	    meta->insar->baseline_parallel_rate);
    fprintf(fp, "  <baseline_perpendicular units=\"m\">%.1lf"
	    "</baseline_perpendicular>\n", 
	    meta->insar->baseline_perpendicular);
    fprintf(fp, "  <baseline_perpendicular_rate units=\"m/s\">%.8lf"
	    "</baseline_perpendicular_rate>\n",
	    meta->insar->baseline_perpendicular_rate);
    fprintf(fp, "  <baseline_temporal units=\"days\">%d</baseline_temporal>\n",
	    meta->insar->baseline_temporal);
    fprintf(fp, "  <baseline_critical units=\"m\">%.1lf</baseline_critical>\n",
	    meta->insar->baseline_critical);
    fprintf(fp, "</insar>\n");
    FCLOSE(fp);
    FREE(output_file_name);
  }
  meta_free(meta);
}
