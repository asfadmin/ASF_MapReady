#ifndef ASF_TERRCORR_H
#define ASF_TERRCORR_H

/**
   asf_terrcorr

   sarFile: Input SAR file
   demFile: Input DEM
   outFile: Output filename
   pixel_size: Desired pixel size, if negative will be left
               alone (or changed to match DEM if required
**/

int asf_terrcorr(char *sarFile, char *demFile,char *inMaskFile,
		 char *outFile, double pixel_size);

int refine_geolocation(char *sarFile, char *demFile, char *inMaskFile,
                       char *outFile, int update_metadata_flag);

int asf_check_geolocation(char *sarFile, char *demFile, char *inMaskFile,
			  char *simAmpFile, char *demSlant);

/**
   asf_terrcorr_ext

      Extended version of the above, with more options.  The above uses
      this one with sensible defaults.

   sarFile: Input SAR file
   demFile: Input DEM
   outFile: Output filename
   pixel_size: Desired pixel size, if negative will be left
               alone (or changed to match DEM if required
**/

int asf_terrcorr_ext(char *sarFile, char *demFile, char *inMaskFile,
		     char *outFile, double pixel_size, int clean_files,
		     int do_resample, int do_corner_matching, int do_interp,
		     int do_fftMatch_verification, int dem_grid_size, 
                     int do_terrain_correction, int fill_value,
                     int generate_water_mask);

/**
   Functions private to terrain correction, not meant for general use.
**/

/* Prototypes from seedsquares.c */
int lay_seeds(int num_seeds, float *mask, long ns, long nl,
              int *x_tl_list, int *y_tl_list, 
              int *x_br_list, int *y_br_list,
              float *good_pct_list);

#endif
