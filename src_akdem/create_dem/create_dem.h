#ifndef CREATE_DEM_H
#define CREATE_DEM_H

#include <time.h>
#include "swath_bound.h"

typedef struct {
        char *dem;		/* reference DEM */
        char *base;		/* base name */
        int log;		/* log file switch */
        int quiet;		/* quiet switch */
	int procs;		/* number of processors */		
	double lat_begin;	/* latitude constraint begin */
	double lat_end;		/* latitude constraint end */
	char *def_val;		/* file with default values */
	char *status;		/* status of processing */
} s_general;

typedef struct {
	char *path;		/* path */
	char *data;		/* binary data file */
	char *par;		/* level zero par file */
} s_image;

typedef struct {
	char *prc_e1;		/* precise orbits for ERS-1 */
	char *prc_e2;		/* precise orbits for ERS-2 */
	int prcFlag;		/* flag for using precise orbits */
	char *status;		/* status of processing */
} s_lz2raw;

typedef struct {
	char *status;		/* status of processing */
} s_status;

typedef struct {
	double percent;		/* percentage for water masking */
	char *status;		/* status of processing */
} s_water_mask;

typedef struct {
	long start_offset;	/* first line of processing */
	long end_offset;	/* last line of processing */
	int patches;		/* number of patches */
	int power;		/* switch for power image */
	char *power_img;	/* name of the power image */
	char *status;		/* status of processing */
} s_aisp;

typedef struct {
        long start_master;      /* first line of processing */
        long start_slave;	/* last line of processing */
        int grid;		/* grid size */
        int fft;		/* switch for complex FFT match in fico */
        char *status;           /* status of processing */	
} s_coreg;

typedef struct {
	char *igram;		/* name of the interferogram */
	char *coh;		/* name of the coherence image */
	double min;		/* minimum coherence level to process */
        int ml;			/* switch for multilooked interferogram */
        char *status;           /* status of processing */	
} s_igram_coh;

typedef struct {
	int offset;		/* switch for using pixel offset matching */
        char *status;           /* status of processing */	
} s_offset_match;

typedef struct {
	char *seeds;		/* name of the seed point file */
	int fft;		/* FFT matching option */
        char *status;           /* status of processing */	
} s_sim_phase;

typedef struct {
	char *algorithm;	/* name of the phase unwrapping algorithm to use */
	int flattening;		/* switch for applying DEM flattening */
	int tiles_azimuth;	/* tiles in azimuth for snaphy algorithm */
	int tiles_range;	/* tiles in range for snaphu algorithm */
	double filter;		/* filter strength */
        char *status;           /* status of processing */	
} s_unwrap;

typedef struct {
	int iter;		/* actual number of iterations for baseline refinement */
	int max;		/* maximum number of iterations for baseline refinement */
        char *status;           /* status of processing */	
} s_refine;

typedef struct {
	char *dem;		/* name of the slant range DEM file */
	char *error;		/* name of the elevation error file */
        char *status;           /* status of processing */	
} s_elev;

typedef struct {
	char *dem;		/* name of geocoded DEM */
	char *amp;		/* name of geocoded amplitude */
	char *error;		/* name of geocoded error map */
	char *proj;		/* name of projection file */
	char *key;		/* projection key */
	int pix_spacing;	/* pixel spacing */
        char *status;           /* status of processing */	
} s_geocode;

typedef struct {
	char comment[255];		/* first line for comments */
        s_general *general;		/* general processing details */
	s_image *master;		/* master image */
	s_image *slave;			/* slave image */
	s_lz2raw *lz2raw;		/* STF ingest */
	s_status *fix_in_fromraw;	/* read in Doppler values */
	s_status *swath_offset;		/* calculation of swath offset */
	s_water_mask *water_mask;	/* water masking */
	s_status *avg_in_dop;		/* average Doppler */
	s_aisp *aisp_master;		/* processing master image */
	s_coreg *coreg_p1;		/* coregistration of first patch */
	s_coreg *coreg_pL;		/* coregistration of last patch */
	s_aisp *aisp_slave;		/* processing slave image */
	s_igram_coh *igram_coh;		/* interferogram/coherence generation */
	s_status *dem_sim;		/* simulate DEM */
	s_offset_match *offset_match;	/* pixel offset matching */
	s_sim_phase *sim_phase;		/* simulate phase image and seed points */
	s_status *deramp_ml;		/* multilook deramped interferogram */
	s_unwrap *unwrap;		/* phase unwrapping */
	s_refine *refine;		/* baseline refinement */
	s_elev *elevation;		/* elevation and elevation error */
	s_status *ground_range;		/* ground range DEM */
	s_geocode *geocode;		/* geocoding */
} dem_config;


/* configuration functions */
int strindex(char s[], char t[]);
char *read_param(char *line);
char *read_str(char *line, char *param);
int read_int(char *line, char *param);
double read_double(char *line, char *param);
dem_config *init_config(char *configFile);
dem_config *read_config(char *configFile, int cFlag);
int write_config(char *configFile, dem_config *cfg);

#endif
