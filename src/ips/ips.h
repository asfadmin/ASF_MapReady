#ifndef IPS_H
#define IPS_H

#include "functions.h"
#include "ceos.h"
#include "asf.h"
#include "asf_meta.h"
#include "proj.h"
#include "asf_license.h"
#include "asf_contact.h"
#include <ctype.h>
#include <time.h>
#include "lzFetch.h"
#include "asf_terrcorr.h"
#include "asf_raster.h"
#include "asf_vector.h"
#include "asf_export.h"
#include "asf_geocode.h"

typedef struct {
  char *mode;		/* processing mode: DEM or DINSAR */
  char *dem;		/* reference DEM */
  char *base;		/* base name */
  char *data_type;	/* data type: STF, CEOS RAW, CEOS SLC */		
  int deskew;		/* switch for deskewing the data */
  char *doppler;       	/* way the Doppler is processed: average, updated */
  double lat_begin;	/* latitude constraint begin */
  double lat_end;      	/* latitude constraint end */
  char *coreg;		/* type of coregistration: PATCH, FRAME */
  int max_off;		/* maximum offset allowed for coregistration */
  int mflag;		/* flag for masking out a certain area */
  char *mask;		/* name of the mask image */
  char *def_val;       	/* file with default values */
  int test;		/* processing in test mode */
  int short_config;     /* flag for short configuration file */
  char *status;		/* status of processing */
} s_general;

typedef struct {
  char *path;		/* path */
  char *data;		/* binary data file */
  char *meta;		/* metadata file */
} s_image;

typedef struct {
  char *prc_master;	/* precise orbits for master image */
  char *prc_slave;	/* precise orbits for slave image */
  int prcflag;		/* flag for using precise orbits */
  char *status;		/* status of processing */
} s_ingest;

typedef struct {
  char *status;		/* status of processing */
} s_status;

typedef struct {
  long start_offset;	/* first line of processing */
  long end_offset;	/* last line of processing */
  int patches;		/* number of patches */
  int power;		/* switch for power image */
  char *power_img;	/* name of the power image */
  char *status;		/* status of processing */
} s_ardop;

typedef struct {
  int patches;		/* number of patches */
  long start_master;    /* first line of processing */
  long start_slave;	/* last line of processing */
  int grid;		/* grid size */
  int fft;		/* switch for complex FFT match in fico */
  int sinc;		/* switch for sinc option in remap */
  int warp;		/* switch for warp option in remap */
  int off_az;		/* offset in azimuth */
  int off_rng;		/* offset in range */
  char *status;         /* status of processing */	
} s_coreg;

typedef struct {
  char *igram;		/* name of the interferogram */
  char *coh;		/* name of the coherence image */
  double min;		/* minimum coherence level to process */
  int ml;	       	/* switch for multilooked interferogram */
  char *status;         /* status of processing */	
} s_igram_coh;

typedef struct {
  char *seeds;		/* name of the seed point file */
  char *status;         /* status of processing */	
} s_sim_phase;

typedef struct {
  char *igram;		/* name of the differential interferogram */
  char *status;         /* status of processing */	
} s_dinsar;

typedef struct {
  double max;		/* maximum pixel offset allowed */
  char *status;		/* status of processing */
} s_offset;

typedef struct {
  char *algorithm;	/* name of the phase unwrapping algorithm to use */
  int flattening;      	/* switch for applying DEM flattening */
  int procs;		/* number of processors used for snaphu */
  int tiles_azimuth;	/* tiles in azimuth for snaphu algorithm */
  int tiles_range;	/* tiles in range for snaphu algorithm */
  int tiles_per_degree;	/* tiles per degree */
  int overlap_azimuth;	/* overlap in azimuth for snaphy algorithm */
  int overlap_range;	/* overlap in range for snaphu algorithm */
  double filter;       	/* filter strength */
  char *qc;		/* phase unwrapping qc */
  char *status;         /* status of processing */	
} s_unwrap;

typedef struct {
  int iter;		/* actual number of iterations for baseline refinement */
  int max;		/* maximum number of iterations for baseline refinement */
  char *status;         /* status of processing */	
} s_refine;

typedef struct {
  char *dem;		/* name of the slant range DEM file */
  char *error;		/* name of the elevation error file */
  char *status;         /* status of processing */	
} s_elev;

typedef struct {
  char *dem;		/* name of geocoded DEM */
  char *amp;		/* name of geocoded amplitude */
  char *error;		/* name of geocoded error map */
  char *coh;		/* name of geocoded coherence image */
  char *name;		/* name of projection */
  char *proj;		/* name of projection file */
  char *resample;       /* resampling method: nearest neighbor, bilinear, bicubic */
  double pixel_spacing;	/* pixel spacing */
  char *status;         /* status of processing */	
} s_geocode;

typedef struct {
  char *format;         /* export format: geotiff, jpeg etc. */
  char *status;         /* status of processing */
} s_export;

typedef struct {
  char comment[255];		/* first line for comments */
  s_general *general;		/* general processing details */
  s_image *master;		/* master image */
  s_image *slave;	       	/* slave image */
  s_ingest *ingest;		/* ingest STF data */
  s_status *doppler;		/* average Doppler processing */
  s_status *doppler_per_patch;	/* updated Doppler processing */
  s_coreg *coreg_p1;		/* coregistration of first patch */
  s_coreg *coreg_pL;		/* coregistration of last patch */
  s_ardop *ardop_master;	/* processing master image */
  s_ardop *ardop_slave;		/* processing slave image */
  s_status *cpx_autofilter;	/* filter SLC images */
  s_coreg *coreg_slave;		/* coregistration of SLC slave image */
  s_igram_coh *igram_coh;      	/* interferogram/coherence generation */
  s_offset *offset_match;      	/* pixel offset matching */
  s_sim_phase *sim_phase;      	/* simulate phase image and seed points */
  s_dinsar *dinsar;	        /* differential interferogram */
  s_status *deramp_ml;		/* multilook deramped interferogram */
  s_unwrap *unwrap;		/* phase unwrapping */
  s_refine *refine;		/* baseline refinement */
  s_elev *elevation;		/* elevation and elevation error */
  s_status *ground_range;      	/* ground range DEM */
  s_geocode *geocode;		/* geocoding */
  s_export *export;             /* export from internal to external format */
} dem_config;


/* configuration functions */
int strindex(char s[], char t[]);
char *read_param(char *line);
void read_str(char *dest, char *line, char *param);
int read_int(char *line, char *param);
double read_double(char *line, char *param);
int init_config(char *configFile);
dem_config *init_fill_config(char *configFile);
dem_config *read_config(char *configFile, int createFlag);
int write_config(char *configFile, dem_config *cfg);

// Prototypes
int check_refinement(char *base1, char *base2, char *base3);
void check_return(int ret, char *msg);
int ips(dem_config *cfg, char *configFile, int createFlag);


#endif
