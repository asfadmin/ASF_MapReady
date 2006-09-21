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
		     int do_fftMatch_verification,
		     int dem_grid_size, int do_terrain_correction, int maskfill);
