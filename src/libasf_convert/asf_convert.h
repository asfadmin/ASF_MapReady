#ifndef ASF_CONVERT_H
#define ASF_CONVERT_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "asf_meta.h"

typedef struct
{
  char *in_name;          // input file name
  char *ancillary_file;   // ancillary file (if req'd, see PolSARpro and GAMMA req'ts)
  char *out_name;         // output file name
  char *default_in_dir;   // default input directory
  char *default_out_dir;  // default output directory
  int import;             // import flag
  int external;           // external program flag
  int sar_processing;     // SAR processing flag
  int c2p;                // complex -> polar flag
  int image_stats;        // image stats flag (for internal use only)
  int detect_cr;          // detect corner reflector flag (for internal use only)
  int polarimetry;        // polarimetry flag
  int terrain_correct;    // terrain correction flag
  int geocoding;          // geocoding flag
  int export;             // export flag
  int mosaic;             // mosaic flag
  int intermediates;      // flag to keep intermediates
  int quiet;              // quiet flag
  int short_config;       // short configuration file flag;
  int dump_envi;          // true if we should dump .hdr files
  char *defaults;         // default values file
  char *batchFile;        // batch file name
  char *prefix;           // prefix for output file naming scheme
  char *suffix;           // suffix for output file naming scheme
  char *tmp_dir;          // name of the directory for intermediate files
  char *status_file;      // file in which we should dump status info
  int thumbnail;          // if true, a 48x48 jpeg thumbnail of the output
                          // image is generated in the intermediates directory
} s_general;

typedef struct
{
  char *format;           // input format: CEOS, STF, ASF
  char *radiometry;       // data type: AMPLITUDE_IMAGE,
                          //            POWER_IMAGE,
                          //            SIGMA_IMAGE,
                          //            GAMMA_IMAGE,
                          //            BETA_IMAGE
  char *lut;              // look up table file name (CIS only)
  double lat_begin;       // latitude constraint begin
  double lat_end;         // latitude constraint end
  int line;               // start line for subset
  int sample;             // start sample for subset
  int width;              // width of subset
  int height;             // height of subset
  char *image_data_type;  // image data type, e.g. POLARIMETRIC_MATRIX
  char *prc;              // precision state vector location (to be 
                          //        implemented)
  int output_db;          // TRUE if the output is db.  Only applies to
                          //        SIGMA, GAMMA, BETA radiometries.
  int complex_slc;        // flag to save complex data as I/Q
                          // otherwise SLC data as stored as amp/phase
  int multilook_slc;      // flag to multilook single look complex data
  int ers2_gain_fix;      // flag to apply ers2 gain correction
  char *polsarpro_colormap; // colormap (.pal) to apply to PolSARpro classifications
  char *metadata_file;    // Name of a (non-ASF) metadata file (if it can't
                          // be deduced)
} s_import;

typedef struct
{
  int c_vv;               // ingest the airsar C-band interferometric data?
  int l_vv;               // ingest the airsar L-band interferometric data?
  int c_pol;              // ingest the airsar C-band polarimetric image?
  int l_pol;              // ingest the airsar L-band polarimetric image?
  int p_pol;              // ingest the airsar P-band polarimetric image?
} s_airsar;

typedef struct
{
  char *cmd;
} s_external;

typedef struct
{
  char *radiometry;       // data type: AMPLITUDE_IMAGE,
                          //            POWER_IMAGE,
                          //            SIGMA_IMAGE,
                          //            GAMMA_IMAGE,
                          //            BETA_IMAGE
} s_sar_processing;

typedef struct
{
  int multilook;          // should we multilook?
} s_c2p;

typedef struct
{
  char *values;           // value axis: LOOK, INCIDENCE, RANGE
  int bins;               // number of bins
  double interval;        // interval between bins
} s_image_stats;

typedef struct
{
  char *cr_location;      // file with corner reflector locations
  int chips;              // flag to save image analysis chips as binary file
  int text;               // flag to save image analysis chips as text file
} s_detect_cr;

typedef enum {
  FARCORR_OFF=0,
  FARCORR_MEAN,
  FARCORR_SMOOTH
} farcorr_t;

typedef struct
{
  int pauli;              // Pauli decomposition for quad-pole data
  int sinclair;           // Sinclair decomposition for quad-pole data
  int cloude_pottier;     // Entropy/Alpha segmentation (8 classes)
  int cloude_pottier_ext; // Entropy/Alpha/Anisotropy segmentation (16 classes)
  int cloude_pottier_nc;  // Entropy/Alpha/Anisotropy data, not classified
  int k_means_wishart;    // K-means Wishart clustering
  int k_means_wishart_ext;// K-means Wishart Entropy/Alpha/Anisotropy clustering
  int lee_preserving;     // Lee category preserving
  int freeman_durden;     // Freeman/Durden decomposition
  farcorr_t farcorr;      // Do we want to faraday correct, if so, how?
  double farcorr_threshold; // Angle below which we do not apply the correction
} s_polarimetry;

typedef struct
{
  double pixel;           // pixel size for terrain corrected product
  char *dem;              // reference DEM file name
  char *mask;             // mask file name (should==NULL if auto_mask_water)
  int auto_mask_water;    // TRUE if we should automatically generate a mask
                          // image from the DEM which masks out water regions
  float water_height_cutoff;   // At or below this DEM pixels are auto-masked
  int refine_geolocation_only; // If TRUE, we don't actually do any terrain
                          // correction, just refine the geolocation w/ the DEM
  int interp;             // TRUE if we should interpolate layover/shadow
                          // regions, FALSE if those regions should be blank
  int fill_value;         // a fill value if >=0. LEAVE_MASK means use sar data
  int save_terrcorr_dem;  // if TRUE, save the clipped DEM for the user
  int save_terrcorr_layover_mask; // if TRUE, save the layover/shadow mask
  int do_radiometric;     // If TRUE, apply radiometric terrain correction in
                          // addition to geometric terrain correction
  int smooth_dem_holes;   // If TRUE, try to smooth over holes in the DEM
  int no_resampling;      // If TRUE, SAR image is not downsampled to match DEM
  int no_matching;        // If TRUE, SAR image and simulated SAR image are
                          // not matched
  double range_offset;    // Overwrite the range offset determined by matching
  double azimuth_offset;  // Overwrite the azimuth offset determined by matching;
  int use_gr_dem;    // If TRUE, use the ground-range DEM for terrcorr, if
                     // FALSE, use the backconverted slant-range DEM (default).
  int if_coreg_fails_use_zero_offsets; // If TRUE, if coreg fails redo with
                     // no_matching turned on
} s_terrain_correct;

typedef struct
{
  char *projection;       // projection parameters file
  double pixel;           // pixel size for geocoded product
  double height;          // average height of the data
  char *datum;            // datum: WGS84, NAD27, NAD83, ITRF97, ED50, SAD69
  char *spheroid;         // spheroids: internally passed along only
  char *resampling;       // resampling method: NEAREST_NEIGHBOR, BILINEAR, BICUBIC
  int force;              // force flag
  float background;       // value to use for pixels outside the image
} s_geocoding;

typedef struct
{
  char *format;           // output format: ASF, GEOTIFF, JPEG, PGM
  char *byte;             // conversion to byte: SIGMA, MINMAX, TRUNCATE,
                          // HISTOGRAM_EQUALIZE
  char *lut;              // A look-up-table to convert greyscale to rgb
  char *rgb;              // RGB banding setting
  int truecolor;          // True color flag (bands 3-2-1 w/2-sigma contrast expansion)
  int falsecolor;         // False color flag (ditto, but bands 4-3-2)
  char *band;             // Band ID string ("HH", "HV", "01", etc) for single-band export
} s_export;

typedef struct
{
  char *overlap;          // overlap option: MIN, MAX, OVERLAY, NEAR_RANGE
} s_mosaic;

typedef struct
{
  char comment[255];                   // first line for comments
  s_general *general;                  // general processing details
  s_import *import;                    // importing parameters
  s_external *external;                // external program to run
  s_airsar *airsar;                    // airsar processing parameters
  s_sar_processing *sar_processing;    // SAR processing parameters
  s_c2p *c2p;                          // complex -> polar parameters
  s_image_stats *image_stats;          // image stats parameters
  s_detect_cr *detect_cr;              // corner reflector detection parameters
  s_polarimetry *polarimetry;          // polarimetric parameters
  s_terrain_correct *terrain_correct;  // terrain correction parameters
  s_geocoding *geocoding;              // geocoding parameters
  s_export *export;                    // exporting parameters
  s_mosaic *mosaic;                    // mosaicking parameters
} convert_config;

meta_parameters *meta_read_cfg(const char *inName, convert_config *cfg);
// checking return values in the main program
void check_return(int ret, char *msg);
// checking whether input is sufficient for processing
void check_input(convert_config *cfg, char *processing_step, char *input);

// configuration functions
int init_convert_config(char *configFile);
void free_convert_config(convert_config *cfg);
convert_config *init_fill_convert_config(char *configFile);
convert_config *read_convert_config(char *configFile);
int write_convert_config(char *configFile, convert_config *cfg);

// function call definitions
int exit_code;

char *str2upper(char *string);
//int asf_import(char *inFile, char *outFile, char *format, char *radiometry,
//               char *prcOrbits, double lat_begin, double lat_end);
//int ardop(char *options, char *inFile, char *outFile);
int image_stats(char *inFile, char *outFile, char *values, int bins,
                double interval);
int detect_cr(char *inFile, char *crFile, char *outFile, int chips, int text);
//int asf_terrcorr(char *options, char *inFile, char *demFile, char *outFile);
//int asf_geocode(char *options, char *inFile, char *outFile);
//int asf_export(char *options, char *inFile, char *outFile);
int asf_convert(int createflag, char *configFileName);
int asf_convert_ext(int createflag, char *configFileName, int saveDEM);
int call_asf_convert(char *configFile); // FIXME: Change the name ... Now calls asf_mapready

int kml_overlay(char *inFile, char *outFile, char *demFile, 
		int terrain_correct, int refine_geolocation, int zip);
int kml_overlay_ext(char *inFile, char *outFile, char *demFile, 
		    int terrain_correct, int refine_geolocation, 
		    int reduction, int zip);

#endif
