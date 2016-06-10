#ifndef _ASF_IMPORT_H_
#define _ASF_IMPORT_H_

#ifndef __ASF_META_H__
#include "asf_meta.h"
#endif

#include "prc_stvecs.h"
#include <xml_util.h>

#define TOOLS_META_EXT    ".meta"
#define TOOLS_IMAGE_EXT   ".img"
#define TOOLS_RAW_EXT     ".img"
#define TOOLS_COMPLEX_EXT ".img"

#define DEFAULT_RANGE_SCALE 1.00343642612
#define DEFAULT_AZIMUTH_SCALE 1.003333505

/* This is the external interface to asf_import */
int asf_import(radiometry_t radiometry, // r_AMP,R_SIGMA,r_BETA,r_GAMMA,r_POWER
               int db_flag,   // TRUE if the output should be in decibels
                              // only ok for radiometry=SIGMA,GAMMA,BETA
               int complex_flag, // TRUE if ingested SLC should in I/Q
               int multilook_flag, // TRUE is SLC should be multilooked while
                                   // being ingested.
	       int azimuth_look_count,
	       int range_look_count,
               int amp0_flag,      // TRUE if we should generate a band 0
                                   // with amplitude data, with the rest
                                   // starting at band 1
               input_format_t format_type, // eg, STF, CEOS - etc
               char *band_id, // eg, "" (default for all bands), "VH", "03" - etc
               char *data_type,    // data type for gamma ingest
               char *image_data_type, // "geocoded_image", "dem", or "mask"
               char *lutName, // NULL for no lookup table
                              // otherwise, this is the lookup table filename
               char *prcPath, // NULL for not using precision orbit data
                              // otherwise, this is the precision state vector
                              // path
               double lowerLat, // -99 means not constrained
               double upperLat, // -99 means not constrained
	       double lowerLon,
	       double upperLon,
               int line, // start line subset - default set to 0
               int sample, // start sample subset - default set to 0
               int width, // -99 means no subsetting
               int height, // -99 means no subsetting
               int save_intermediates, // save intermediate files if any were created
               double *p_range_scale, // NULL for no scaling
               double *p_azimuth_scale, // NULL for no scaling
               double *p_correct_y_pixel_size, // NULL for no fixing
               int apply_ers2_gain_fix, // TRUE for correction of ers2 data
               char *inMetaNameOption, // NULL for normal metadata naming
                                       // otherwise, this is the meta file name
               char *inBaseName, // input file
               char *ancillary_file, // ancillary file (if needed for input file)
               char *colormapName, // colormap file
	       char *slave_file, // slave metadata file
	       char *interferogram_file, // interferogram file
	       char *coherence_file, // coherence image file
	       char *baseline_file, // baseline file
	       int complex_gamm_file, // TRUE for complex GAMMA file
	       char *uavsar_type, // data type for UAVSAR data
	       int metaonly, // flat for generating XML metadata file only
               char *outBaseName // output file
               );

/*********************************************************************/
/* The rest of these are private implementation functions for import */

/* Prototypes from lut.c */
char *check_luts(meta_parameters *meta);
void read_cal_lut(meta_parameters *meta, char *lutName, double **incid_table,
                  double **scale_table, int *min, int *max);

/* Prototypes from utilities.c */
int firstRecordLen(char *ceosName);

/* Prototypes from sprocket_layers.c */
void create_sprocket_layers(const char *asfName, const char *importName);

/* import_*() function prototypes */
void import_ceos(char *inBaseName, char *outBaseName,
                 char *band_id, char *lutName, double *p_range_scale,
                 double *p_azimuth_scale, double *p_correct_y_pixel_size,
		 int line, int sample, int width, int height,
                 char *inMetaNameOption, radiometry_t radiometry, int db_flag,
                 int complex_flag, int multilook_flag, int azimuth_look_count,
		 int range_look_count, int amp0_flag, int apply_ers2_gain_fix);
void import_stf(char *inBaseName, char *outBaseName, radiometry_t radiometry,
                char *inMetaNameOption, int lat_constrained, double lowerLat,
                double upperLat, char *prcPath);
meta_parameters *meta_read_stf(const char *inFile);

void
import_generic_geotiff (const char *inFileName, const char *outBaseName, ...);

void import_bil(char *inBaseName, char *outBaseName);
meta_parameters *read_meta_bil(char *inBaseName);
void import_gridfloat(char *inBaseName, char *outBaseName);
meta_parameters *read_meta_gridfloat(char *inBaseName);

void import_airsar(const char *inFileName, radiometry_t radiometry,
		   const char *outBaseName);
meta_parameters *import_airsar_meta(const char *dataName,
				    const char *inBaseName, int force);
void read_meta_airsar(char *inBaseName, char *outBaseName);
void import_uavsar(const char *inFileName, int line, int sample, int width,
		   int height, radiometry_t radiometry,
		   const char *data_type, const char *outBaseName);
void import_uavsar_ext(const char *inFileName, int line, int sample, int width,
		       int height, radiometry_t radiometry, int firstBandOnly,
		       const char *data_type, const char *outBaseName);
void read_meta_uavsar(const char *inFileName, const char *outBaseName);
void import_gamma_isp(const char *inDataName, const char *inMetaName,
              const char *data_type, const char *image_data_type,
              int complex_flag, int multilook_flag,
              const char *outBaseName);
void import_gamma_msp(const char *inDataName, const char *inMetaName,
              const char *data_type, const char *image_data_type,
              const char *outBaseName);
void import_seasat_h5(const char *inFileName, const char *outBaseName);

void import_vexcel_plain(const char *inBaseName, const char *outBaseName);
void import_jaxa_L0(const char *inBaseName, const char *outBaseName);
void import_alos_mosaic(const char *inFileName, radiometry_t radiometry,
			const char *outBaseName);
char *get_terrasar_browse_file(const char *xml_file_name);
void import_terrasar(const char *inFileName, radiometry_t radiometry,
		     const char *outBaseName, int ampOnly);
void import_radarsat2(const char *inBaseName, radiometry_t radiometry,
		      const char *outBaseName, int ampOnly);

void assign_band_names(meta_parameters *meta, char *outMetaName,
               char *bandExt, int band, int nBands, int nBandsOut,
               radiometry_t radiometry, int complex_flag);
void import_polsarpro(char *polsarName, char *ceosName, char *colormapName, 
		      char *image_data_type, int db_flag, char *outBaseName);
void apply_polsarpro_palette_to_metadata(const char *lut_basename,
                                         meta_parameters *imd);
void import_gamma(char *dataName, char *metaName, char *slaveName,
                  char *igramName, char *cohName, char *baselineName, 
		  int complex_gamma, char *outBaseName);
meta_parameters *meta_read_roipac(const char *in, const char *sv_file);
void import_roipac(const char *baseName, const char *outName);
void import_roipac_new(const char *baseName, const char *outName,
                       const char *sv_file, const char *rsc_baseline_file);

void import_smap(const char *inBaseName, const char *outBaseName,
		 float latUL, float lonUL, float latLR, float lonLR);

void import_sentinel(const char *inBaseName, radiometry_t radiometry,
  const char *lutFile, const char *outBaseName);

void import_netcdf_xml(const char *ncFile, char *xmlFile);
xmlDoc *netcdf2xml(const char *ncFile);

meta_parameters *meta_read_only(const char *in_fName);
meta_parameters *meta_read_raw(const char *inFile);

int init_fgdc_config(char *configFile, char *type);

#endif
