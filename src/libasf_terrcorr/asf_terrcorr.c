#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdarg.h>
#include <limits.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_contact.h>
#include <asf_license.h>
#include <asf_sar.h>
#include <libasf_proj.h>
#include <poly.h>

#include <asf_terrcorr.h>

#define MASK_SEED_POINTS 20

static const int PAD = DEM_GRID_RHS_PADDING;

char *strdup(const char *);

static int int_rnd(double x)
{
  return (int)floor(x+0.5);
}

static void ensure_ext(char **filename, const char *ext)
{
  char *ret = MALLOC(sizeof(char)*(strlen(*filename)+strlen(ext)+5));
  strcpy(ret, *filename);

  // blow away current extension if necessary
  char *p = findExt(ret);
  if (p) *p = '\0';

  if (ext[0] != '.') strcat(ret, ".");
  strcat(ret, ext);

  free(*filename);
  *filename = ret;
}

static void remove_file(const char * file)
{
  if (fileExists(file)) {
    unlink(file);
  }
}

static char * appendSuffix(const char *inFile, const char *suffix)
{
  char *suffix_pid = MALLOC(sizeof(char)*(strlen(suffix)+25));
  sprintf(suffix_pid, "%s", suffix);

  char * ret = appendToBasename(inFile, suffix_pid);

  free(suffix_pid);
  return ret;
}

static char *outputName(const char *dir, const char *base, const char *suffix)
{
    int dirlen = strlen(dir);
    int baselen = strlen(base);
    int len = dirlen > baselen ? dirlen : baselen;

    char *fil = MALLOC(sizeof(char)*(dirlen+baselen+strlen(suffix)+10));
    char *dirTmp = MALLOC(sizeof(char) * (len+10));
    char *fileTmp = MALLOC(sizeof(char) * (len+10));

    split_dir_and_file(dir, dirTmp, fileTmp);
    strcpy(fil, dirTmp);

    split_dir_and_file(base, dirTmp, fileTmp);
    strcat(fil, fileTmp);

    char *ret = appendSuffix(fil, suffix);

    free(fil);
    free(dirTmp);
    free(fileTmp);

    return ret;
}

// attempt to remove "<file>.img" and "<file>.meta", etc files
static void clean(const char *file)
{
    if (file)
    {
        char * img_file = appendExt(file, ".img");
        char * meta_file = appendExt(file, ".meta");
        char * ddr_file = appendExt(file, ".ddr");

        remove_file(img_file);
        remove_file(meta_file);
        remove_file(ddr_file);
        remove_file(file);

        free(img_file);
        free(meta_file);
        free(ddr_file);
    }
}

static void
fftMatchQ(char *file1, char *file2, float *dx, float *dy, float *cert)
{
  int qf_saved = quietflag;
  quietflag = 1;
  fftMatch(file1, file2, NULL, dx, dy, cert);
  if (!meta_is_valid_double(*dx) || !meta_is_valid_double(*dy) || cert==0)
      fftMatch(file2, file1, NULL, dx, dy, cert);
  quietflag = qf_saved;
}

static int mini(int a, int b)
{
  return a < b ? a : b;
}

static void update_meta_offsets(const char *filename, double t_offset,
                                double x_offset)
{
    asfPrintStatus("Updating offsets in the metadata for: %s\n", filename);

    meta_parameters *meta = meta_read(filename);
    meta->sar->time_shift += t_offset;
    meta->sar->slant_shift += x_offset;
    meta_write(meta, filename);
    meta_free(meta);
}

static int is_alos(meta_parameters *meta)
{
    return
        meta && meta->general && 
        strstr(uc(meta->general->sensor), "ALOS") != NULL;
}

static int is_scansar(meta_parameters *meta)
{
    return
        meta && meta->projection &&
        meta->projection->type == SCANSAR_PROJECTION;
}

static void
fftMatch_atCorners(char *output_dir, char *sar, char *dem, const int size)
{
  float dx_ur, dy_ur;
  float dx_ul, dy_ul;
  float dx_lr, dy_lr;
  float dx_ll, dy_ll;
  float cert;
  double rsf, asf;

  char *chopped_sar, *chopped_dem;
  int nl, ns;
  meta_parameters *meta_sar, *meta_dem;
  long long lsz = (long long)size;

  meta_sar = meta_read(sar);
  meta_dem = meta_read(dem);

  chopped_sar = outputName(output_dir, sar, "_chip");
  chopped_dem = outputName(output_dir, dem, "_chip");

  nl = mini(meta_sar->general->line_count, meta_dem->general->line_count);
  ns = mini(meta_sar->general->sample_count, meta_dem->general->sample_count);

  meta_free(meta_dem);

  // Require the image be 4x the chip size in each direction, otherwise
  // the corner matching isn't really that meaningful
  if (nl < 4*size || ns < 4*size) {
    asfPrintStatus("Image is too small, skipping corner matching.\n");
    return;
  }

  trim(sar, chopped_sar, (long long)0, (long long)0, lsz, lsz);
  trim(dem, chopped_dem, (long long)0, (long long)0, lsz, lsz);

  fftMatchQ(chopped_sar, chopped_dem, &dx_ur, &dy_ur, &cert);
  asfPrintStatus("UR: %14.10f %14.10f %14.10f\n", dx_ur, dy_ur, cert);

  trim(sar, chopped_sar, (long long)(ns-size), (long long)0, lsz, lsz);
  trim(dem, chopped_dem, (long long)(ns-size), (long long)0, lsz, lsz);

  fftMatchQ(chopped_sar, chopped_dem, &dx_ul, &dy_ul, &cert);
  asfPrintStatus("UL: %14.10f %14.10f %14.10f\n", dx_ul, dy_ul, cert);

  trim(sar, chopped_sar, (long long)0, (long long)(nl-size), lsz, lsz);
  trim(dem, chopped_dem, (long long)0, (long long)(nl-size), lsz, lsz);

  fftMatchQ(chopped_sar, chopped_dem, &dx_lr, &dy_lr, &cert);
  asfPrintStatus("LR: %14.10f %14.10f %14.10f\n", dx_lr, dy_lr, cert);

  trim(sar, chopped_sar, (long long)(ns-size), (long long)(nl-size),
       lsz, lsz);
  trim(dem, chopped_dem, (long long)(ns-size), (long long)(nl-size),
       lsz, lsz);

  fftMatchQ(chopped_sar, chopped_dem, &dx_ll, &dy_ll, &cert);
  asfPrintStatus("LL: %14.10f %14.10f %14.10f\n", dx_ll, dy_ll, cert);

  asfPrintStatus("Range shift: %14.10lf top\n", (double)(dx_ul-dx_ur));
  asfPrintStatus("             %14.10lf bottom\n", (double)(dx_ll-dx_lr));
  asfPrintStatus("   Az shift: %14.10lf left\n", (double)(dy_ul-dy_ll));
  asfPrintStatus("             %14.10lf right\n\n", (double)(dy_ur-dy_lr));

  nl = meta_sar->general->line_count;
  ns = meta_sar->general->sample_count;

  rsf = 1 - (fabs((double)(dx_ul-dx_ur)) + fabs((double)(dx_ll-dx_lr)))/ns/2;
  asf = 1 - (fabs((double)(dy_ul-dy_ll)) + fabs((double)(dy_ur-dy_lr)))/nl/2;

  asfPrintStatus("Suggested scale factors: %14.10lf range\n", rsf);
  asfPrintStatus("                         %14.10lf azimuth\n\n", asf);

  meta_free(meta_sar);

  clean(chopped_sar);
  clean(chopped_dem);
  free(chopped_sar);
  free(chopped_dem);
}

int asf_terrcorr(char *sarFile, char *demFile, char *userMaskFile,
		 char *outFile, double pixel_size)
{
  int do_fftMatch_verification = TRUE;
  int do_corner_matching = TRUE;
  int do_resample = TRUE;
  int do_interp = FALSE;
  int clean_files = TRUE;
  int dem_grid_size = 20;
  int do_terrain_correction = TRUE;
  int generate_water_mask = FALSE;
  int save_clipped_dem = FALSE;
  int fill_value = 0;
  int doRadiometric = FALSE;
  int smooth_dem_holes = FALSE;
  int update_original_metadata_with_offsets = FALSE;
  float mask_height_cutoff = -999; // not used

  return asf_terrcorr_ext(sarFile, demFile, userMaskFile, outFile, pixel_size,
      clean_files, do_resample, do_corner_matching,
      do_interp, do_fftMatch_verification,
      dem_grid_size, do_terrain_correction, fill_value,
      generate_water_mask, save_clipped_dem,
      update_original_metadata_with_offsets, mask_height_cutoff,
      doRadiometric, smooth_dem_holes, NULL);
}

int refine_geolocation(char *sarFile, char *demFile, char *userMaskFile,
                       char *outFile, int update_flag, int auto_water_mask,
                       float mask_height_cutoff,
                       char **other_files_to_update_with_offsets)
{
  double pixel_size = -1;
  int dem_grid_size = 20;
  int clean_files = TRUE;
  int do_resample = TRUE;
  int do_interp = FALSE;
  int do_fftMatch_verification = TRUE;
  int do_corner_matching = FALSE;
  int do_terrain_correction = FALSE;
  int save_clipped_dem = FALSE;
  int doRadiometric = FALSE;
  int smooth_dem_holes = FALSE;
  int fill_value = 0;
  int update_orig_metadata_with_offsets = FALSE;

  int ret =
      asf_terrcorr_ext(sarFile, demFile, userMaskFile, outFile, pixel_size,
                       clean_files, do_resample, do_corner_matching,
                       do_interp, do_fftMatch_verification, dem_grid_size,
                       do_terrain_correction, fill_value, auto_water_mask,
                       save_clipped_dem, update_orig_metadata_with_offsets,
                       mask_height_cutoff, doRadiometric, smooth_dem_holes,
                       other_files_to_update_with_offsets);

  if (ret==0)
  {
      // asf_terrcorr_ext with do_terrain_correction turned off just
      // creates a metadata file - the actual image file is unchanged.
      if (update_flag) {
          // If the user issued the "update" flag, then we need to move
          // the new metadata file over the old metadata file.  (ie., we
          // are "updating" the existing metadata with the new shift values
          char *inMetaFile = appendExt(sarFile, ".meta");
          char *outMetaFile = appendExt(outFile, ".meta");
          fileCopy(outMetaFile, inMetaFile);
          clean(outFile);
          FREE(inMetaFile);
          FREE(outMetaFile);
      } else {
          // Otherwise, make a copy of the original image with the new name
          char *inImgFile = appendExt(sarFile, ".img");
          char *outImgFile = appendExt(outFile, ".img");
          fileCopy(inImgFile, outImgFile);
          FREE(inImgFile);
          FREE(outImgFile);
      }
  }

  return ret;
}

static char * getOutputDir(char *outFile)
{
    int len = strlen(outFile) + 2;
    char *d = MALLOC(sizeof(char) * len);
    char *f = MALLOC(sizeof(char) * len);
    split_dir_and_file(outFile, d, f);
    free(f);
    return d;
}

static void update_extents(int lineSAR, int sampSAR,
                           meta_parameters *metaDEM, meta_parameters *metaSAR,
                           int *line_min, int *line_max,
                           int *samp_min, int *samp_max)
{
    double lat, lon, x, y, z;
    int line, samp;      // line & samp in the DEM

    meta_get_latLon(metaSAR, lineSAR, sampSAR, 0.0, &lat, &lon);
    latlon_to_proj(metaDEM->projection, metaDEM->sar->look_direction,
        lat*D2R, lon*D2R, 0.0, &x, &y, &z);

    // these should work even if perX or perY is negative
    samp = (int) (.5 + (x - metaDEM->projection->startX) /
                        metaDEM->projection->perX);
    line = (int) (.5 + (y - metaDEM->projection->startY) /
                        metaDEM->projection->perY);

    // padding, allow for subsequent geocoding & height-induced offsets
    int line_lo = line - 20;
    int line_hi = line + 20;
    int samp_lo = samp - 100;
    int samp_hi = samp + 500;

    if (line_lo < 0) line_lo = 0;
    if (line_hi > metaDEM->general->line_count - 1)
        line_hi = metaDEM->general->line_count - 1;

    if (samp_lo < 0) samp_lo = 0;
    if (samp_hi > metaDEM->general->sample_count - 1)
        samp_hi = metaDEM->general->sample_count - 1;

    if (samp_lo < *samp_min) *samp_min = samp_lo;
    if (line_lo < *line_min) *line_min = line_lo;

    if (samp_hi > *samp_max) *samp_max = samp_hi;
    if (line_hi > *line_max) *line_max = line_hi;
}

static void cut_dem(meta_parameters *metaSAR, meta_parameters *metaDEM,
                    char *demFile, char *output_dir)
{
    asfRequire(metaDEM->projection != NULL, "Requires projected DEM");

    int line_min, line_max, samp_min, samp_max;
    int nl = metaSAR->general->line_count;
    int ns = metaSAR->general->sample_count;

    line_min = nl - 1;
    samp_min = ns - 1;
    line_max = 0;
    samp_max = 0;

    to_radians(metaDEM->projection->type, &metaDEM->projection->param);
    update_extents(0,    0,    metaDEM, metaSAR,
                   &line_min, &line_max, &samp_min, &samp_max);
    update_extents(nl-1, 0,    metaDEM, metaSAR,
                   &line_min, &line_max, &samp_min, &samp_max);
    update_extents(0,    ns-1, metaDEM, metaSAR,
                   &line_min, &line_max, &samp_min, &samp_max);
    update_extents(nl-1, ns-1, metaDEM, metaSAR,
                   &line_min, &line_max, &samp_min, &samp_max);

    char *cutDemFile = outputName(output_dir, demFile, "_cut");

    asfPrintStatus("Cutting DEM: lines %d-%d, samples %d-%d\n",
                   line_min, line_max, samp_min, samp_max);

    if (line_min == line_max || samp_min == samp_max) {
        asfPrintWarning("Failed to cut the DEM!\n"
                        "Using the entire DEM...\n");

        line_min = samp_min = 0;

        line_max = nl - 1;
        samp_max = ns - 1;
    }

    trim(demFile, cutDemFile, samp_min, line_min,
         samp_max - samp_min + 1, line_max - line_min + 1);

    free(cutDemFile);
}

static void
clip_dem(meta_parameters *metaSAR,
         char *srFile,
         char *demFile,   // we call this the DEM, but it could be a mask
         char *demClipped,
         char *what,      // "DEM" or "User Mask" probably
         char *otherFile, // file same size/projection as DEM to clip, or NULL
         char *otherClipped,
         char *otherWhat, // "User Mask" probably, or NULL
         char *output_dir,
         int dem_grid_size,
         int clean_files,
         int *p_demHeight)
{
    char *demGridFile = NULL;
    int polyOrder = 5;
    double coverage_pct;
    int demWidth, demHeight;

    asfPrintStatus("Now clipping %s: %s.\n", what, demFile);

    // Generate a point grid for the DEM extraction.
    // The width and height of the grid is defined in slant range image
    // coordinates, while the grid is actually calculated in DEM space.
    // There is a buffer of 400 pixels in far range added to have enough
    // DEM around when we get to correcting the terrain.
    asfPrintStatus("Generating %dx%d %s grid...\n",
		   dem_grid_size, dem_grid_size, what);

    demGridFile = outputName(output_dir, srFile, "_demgrid");
    create_dem_grid_ext(demFile, srFile, demGridFile,
			metaSAR->general->sample_count,
			metaSAR->general->line_count, dem_grid_size,
			&coverage_pct);

    if (coverage_pct <= 0) {
      asfPrintError("%s and SAR images do not overlap!\n", what);
    } else if (coverage_pct <= 25) {
      asfPrintWarning("Insufficient %s coverage!\n", what);
    } else if (coverage_pct <= 99) {
      asfPrintWarning("Incomplete %s coverage!\n", what);
    }

    // Fit a fifth order polynomial to the grid points.
    // This polynomial is then used to extract a subset out of the reference
    // DEM.
    asfPrintStatus("Fitting order %d polynomial to %s...\n", polyOrder, what);
    double maxErr;
    poly_2d *fwX, *fwY, *bwX, *bwY;
    fit_poly(demGridFile, polyOrder, &maxErr, &fwX, &fwY, &bwX, &bwY);
    asfPrintStatus("Maximum error in polynomial fit: %.2f.\n", maxErr);

    // Here is the actual work done for cutting out the DEM.
    // The adjustment of the DEM width by 400 pixels (originated in
    // create_dem_grid) needs to be factored in.

    demWidth = metaSAR->general->sample_count + DEM_GRID_RHS_PADDING;
    demHeight = metaSAR->general->line_count;

    asfPrintStatus("Clipping %s to %dx%d LxS using polynomial fit...\n",
		   what, demHeight, demWidth);

    // DEMs get a background value of NO_DEM_DATA
    // User Masks get 2 (masked & invalid).
    float background_value = strcmp(what, "DEM") == 0 ? NO_DEM_DATA : 2;
    remap_poly(fwX, fwY, bwX, bwY, demWidth, demHeight, demFile, demClipped,
               background_value);

    if (otherFile) {
        asfRequire(otherClipped && otherWhat, "required arguments were NULL");

        asfPrintStatus("Clipping %s using same clipping parameters as %s.\n",
                       otherWhat, what);
        asfPrintStatus("Clipping %s to %dx%d LxS using polynomial fit...\n",
                       otherWhat, demHeight, demWidth);

        background_value = 2; // is a mask-- outside areas should be masked
        remap_poly(fwX, fwY, bwX, bwY, demWidth, demHeight, otherFile,
                   otherClipped, background_value);
    }

    // finished with polynomials
    poly_delete(fwX);
    poly_delete(fwY);
    poly_delete(bwX);
    poly_delete(bwY);

    if (clean_files)
        clean(demGridFile);

    if (p_demHeight)
        *p_demHeight = demHeight;

    FREE(demGridFile);
}

static
int match_dem(meta_parameters *metaSAR,
              char *sarFile,
              char *demFile,
	      char *srFile,
              char *output_dir,
              char *userMaskFile,
	      char *demTrimSimAmp,
              char *demTrimSlant,
              char *userMaskClipped,
              int dem_grid_size,
	      int do_corner_matching,
              int do_fftMatch_verification,
              int do_refine_geolocation,
              int do_trim_slant_range_dem,
	      int apply_dem_padding,
              int mask_dem_same_size_and_projection,
              int clean_files,
              int interp_holes,
              double *t_offset,
	      double *x_offset)
{
  char *demClipped = NULL, *demSlant = NULL;
  char *demSimAmp = NULL;
  int num_attempts = 0;
  const int max_attempts = 5; // # of times we re-try co-registration
  const float required_match = 2.5;
  double t_off, x_off;
  int redo_clipping;
  int demHeight;
  float dx, dy, cert=0;
  int idx, idy;
  const float cert_cutoff = 0.33; // is this a good cutoff !?

  // do-while that will repeat the dem grid generation and the fftMatch
  // of the sar & simulated sar, until the fftMatch doesn't turn up a
  // big offset.
  do // matches: while (redo_clipping);
  {
    ++num_attempts;
    asfPrintStatus("Using DEM '%s' for refining geolocation ... "
                   "(iteration #%d)\n", demFile, num_attempts);

    demClipped = outputName(output_dir, demFile, "_clip");

    // Clip the DEM to the same size as the SAR image.  If a user mask was
    // provided, we must clip that one, too.
    if (userMaskFile) {
        if (mask_dem_same_size_and_projection) {
            // clip DEM & Mask at the same time, they'll use the same
            // clipping parameters
            clip_dem(metaSAR, srFile, demFile, demClipped, "DEM",
                     userMaskFile, userMaskClipped, "User Mask",
                     output_dir, dem_grid_size, clean_files, &demHeight);
        } else {
            // clip DEM & Mask separately
            clip_dem(metaSAR, srFile, demFile, demClipped, "DEM",
                     NULL, NULL, NULL, output_dir, dem_grid_size,
                     clean_files, &demHeight);
            clip_dem(metaSAR, srFile, userMaskFile, userMaskClipped,
                     "User Mask", NULL, NULL, NULL, output_dir, dem_grid_size,
                     clean_files, NULL);
        }
    } else {
        // no user mask - just clip the DEM and we're good
        clip_dem(metaSAR, srFile, demFile, demClipped, "DEM",
                 NULL, NULL, NULL, output_dir, dem_grid_size, clean_files,
                 &demHeight);
    }

    // Generate a slant range DEM and a simulated amplitude image.
    asfPrintStatus("Generating slant range DEM and "
		   "simulated sar image...\n");
    if (interp_holes)
        asfPrintStatus("Will attempt to smooth over DEM holes.\n");
    demSlant = outputName(output_dir, demFile, "_slant");
    demSimAmp = outputName(output_dir, demFile, "_sim_amp");
    reskew_dem(srFile, demClipped, demSlant, demSimAmp, userMaskClipped,
               interp_holes);

    // Resize the simulated amplitude to match the slant range SAR image.
    asfPrintStatus("Resizing simulated sar image...\n");
    trim(demSimAmp, demTrimSimAmp, 0, 0, metaSAR->general->sample_count,
	 demHeight);

    asfPrintStatus("Determining image offsets...\n");
    if (userMaskFile) {
      /* OK now if we have a mask we need to find square patches that
	 can be fftMatched without running into the mask.
	 then we average them all back together. to get the offset */
      int x_tl_list[MASK_SEED_POINTS];        // top left corner
      int y_tl_list[MASK_SEED_POINTS];        //     of seed regions
      int x_br_list[MASK_SEED_POINTS];        // bottom right corner
      int y_br_list[MASK_SEED_POINTS];        //     of seed regions
      float good_pct_list[MASK_SEED_POINTS];  // pct of non-masked pixels
      int ii,err;

      FILE *inseedmask = fopenImage(userMaskClipped,"rb");
      meta_parameters *maskmeta = meta_read(userMaskClipped);
      asfRequire(maskmeta->general->line_count==demHeight, "Bad heights.\n");
      float *mask = MALLOC(sizeof(float) * maskmeta->general->sample_count
                           * demHeight);
      for (ii=0;ii<demHeight;ii++) // read in the whole mask image
	get_float_line(inseedmask, maskmeta, ii,
                       mask + ii * maskmeta->general->sample_count);
      FCLOSE(inseedmask);
      err = lay_seeds(MASK_SEED_POINTS, mask, maskmeta->general->sample_count,
                      demHeight, x_tl_list, y_tl_list, x_br_list,
                      y_br_list, good_pct_list);
      FREE(mask);

      if (err==0) {
          //int n_attempt = 0;
          do  // matches "while (cert < cert_cutoff)"
          {
              int ii_chosen = -1;
              long good_num = 0;
              //if (++n_attempt == 1)
              //    printf("Region results:\n");

              for (ii=0; ii<MASK_SEED_POINTS; ii++)
              {
                  long region_size = (y_br_list[ii]-y_tl_list[ii])*
                                     (x_br_list[ii]-x_tl_list[ii]);

                  //if (n_attempt == 1)
                  //    printf("Region %d: [%d,%d]-[%d,%d] (%ld): "
                  //           "%.1f%% -> %ld\n", ii+1,
                  //           x_tl_list[ii], y_tl_list[ii],
                  //           x_br_list[ii], y_br_list[ii], region_size,
                  //           good_pct_list[ii]*100,
                  //           (long)(region_size*good_pct_list[ii]));

                  if (good_pct_list[ii] * region_size > good_num) {
                      ii_chosen = ii;
                      good_num = (long) (good_pct_list[ii] * region_size);
                  }
              }

              if (ii_chosen < 0) {
                  // failed to find a seed point that works!
                  err=1; break;
              }

              asfRequire (ii_chosen >= 0 && ii_chosen < MASK_SEED_POINTS,
                          "Impossible! chosen=%d\n", ii_chosen);

              int xtl = x_tl_list[ii_chosen];
              int ytl = y_tl_list[ii_chosen];
              int xbr = x_br_list[ii_chosen];
              int ybr = y_br_list[ii_chosen];

              asfPrintStatus("Chose seed region #%d: L:[%d,%d] - S:[%d,%d] "
                             "(%dx%d LxS, %.1f%% masked).\n",
                             ii_chosen+1, ytl, ybr, xtl, xbr, ybr-ytl,
                             xbr-xtl, 100*(1 - good_pct_list[ii_chosen]));

              good_pct_list[ii_chosen] = 0; // prevent future selection

              char *demTrimSimAmp_ffft = outputName(output_dir, demFile,
                                                    "_sim_amp_trim_for_fft");
              char *srTrimSimAmp = outputName(output_dir, srFile,
                                              "_src_trim_for_fft");

              //asfPrintStatus(" creating trimmed regions \n %s \n %s \n ",
              //               demTrimSimAmp_ffft, srTrimSimAmp);
              trim(demTrimSimAmp, demTrimSimAmp_ffft,xtl,ytl,xbr-xtl,ybr-ytl);
              trim(srFile, srTrimSimAmp, xtl, ytl, xbr-xtl, ybr-ytl);
              fftMatchQ(srTrimSimAmp, demTrimSimAmp_ffft, &dx, &dy, &cert);

              if (cert < cert_cutoff) {
                  asfPrintStatus("Match: %.2f%% certainty. (%f,%f)\n"
                                 "Certainty fails to meet minimum: %.1f%%\n"
                                 "Matching using another seed point.\n",
                                 100*cert, dx, dy, 100*cert_cutoff);
              }

              clean(demTrimSimAmp_ffft);
              clean(srTrimSimAmp);

              FREE(demTrimSimAmp_ffft);
              FREE(srTrimSimAmp);

          } while (!(cert > cert_cutoff)); // guard against NANs
      }

      if (err==1) {
          // failed to find a good seed region
          asfPrintWarning("Failed to find an adequate seed region to perform "
                          "an fftMatch.\nIt is likely that your mask covers "
                          "too much of the image, leaving an\ninsufficiently "
                          "large region to correct the geolocation.\n"
                          "Proceeding anyway, however your results may not "
                          "be particularly good.\n");

          // set the offsets to 0 -- with 0% certainty.
          dx = dy = cert = 0;

          // skip some additional fftMatches that we may be doing
          do_corner_matching = FALSE;
      }

      // we've already done this in the seed region -- no need to do it
      // on the whole image later
      do_fftMatch_verification = FALSE;

      meta_free(maskmeta);
    }
    else {
      // Match the real and simulated SAR image to determine the offset.
      fftMatchQ(srFile, demTrimSimAmp, &dx, &dy, &cert);
    }

    asfPrintStatus("Correlation (cert=%5.2f%%): dx=%f, dy=%f.\n",
		   100*cert, dx, dy);

    idx = - int_rnd(dx);
    idy = - int_rnd(dy);

    redo_clipping = FALSE;

    if (fabs(dy) > required_match || fabs(dx) > required_match ||
        do_refine_geolocation)
    {
        // The fftMatch resulted in a large offset

        // This means we very likely did not clip the right portion of
        // the DEM.  So, shift the slant range image and re-clip.

        if (num_attempts >= max_attempts)
        {
            // After max_attempts tries, we must not be getting good results
            // from fftMatch, since the offsets are clearly bogus.
            asfPrintWarning("Could not resolve offset!\n"
                "Continuing ... however your result may "
                "be incomplete and/or incorrect.\n");

            if (cert<cert_cutoff && userMaskFile == NULL) {
                asfPrintStatus("You may get better results by supplying "
                    "a mask file, to mask out regions\n"
                    "which provide poor matching, such as "
                    "water or glaciers.\n");
            }

            break;
        }
        else
        {
            // Correct the metadata of the SAR image for the offsets
            // that we found
            asfPrintStatus("Adjusting metadata to account for offsets...\n");
            refine_offset(dx, dy, metaSAR, &t_off, &x_off);
            asfPrintStatus("  Time Shift: %f -> %f\n"
                "  Slant Shift: %f -> %f\n",
                metaSAR->sar->time_shift,
                metaSAR->sar->time_shift + t_off,
                metaSAR->sar->slant_shift,
                metaSAR->sar->slant_shift + x_off);
            metaSAR->sar->time_shift += t_off;
            metaSAR->sar->slant_shift += x_off;
            meta_write(metaSAR, srFile);

            if (!do_refine_geolocation) {
                asfPrintStatus("Found a large offset (%dx%d LxS pixels).\n"
                    "Adjusting SAR image and re-clipping DEM.\n\n",
                    idy, idx);

                redo_clipping = TRUE;
            }
        }
    }
    else if (dx*dx+dy*dy < .5)
    {
        // Skip the verification step-- we've got an excellent match already
        do_fftMatch_verification = FALSE;
    }

  } while (redo_clipping);

  // Corner test
  if (do_corner_matching) {
    int chipsz = 256;
    asfPrintStatus("Doing corner fftMatching... (using %dx%d chips)\n",
		   chipsz, chipsz);
    fftMatch_atCorners(output_dir, srFile, demTrimSimAmp, chipsz);
  }

  // Apply the offset to the simulated amplitude image.
  asfPrintStatus("Applying offsets to simulated sar image...\n");
  trim(demSimAmp, demTrimSimAmp, idx, idy, metaSAR->general->sample_count,
       demHeight);

  // Verify that the applied offset in fact does the trick.
  if (do_fftMatch_verification) {
      float dx2, dy2;

      asfPrintStatus("Verifying offsets are now close to zero...\n");
      fftMatchQ(srFile, demTrimSimAmp, &dx2, &dy2, &cert);

      asfPrintStatus("Correlation after shift (cert=%5.2f%%): "
                     "dx=%f, dy=%f.\n",
                     100*cert, dx2, dy2);

      if (fabs(dy2) > required_match || fabs(dx2) > required_match) {
          asfPrintError("Correlated images failed to match!\n"
                        " Original fftMatch offset: "
                        "(dx,dy) = %14.9f,%14.9f\n"
                        "   After shift, offset is: "
                        "(dx,dy) = %14.9f,%14.9f\n",
                        dx, dy, dx2, dy2);
      }
  }

  if (do_trim_slant_range_dem)
  {
      // Apply the offset to the slant range DEM.
      asfPrintStatus("Applying offsets to slant range DEM...\n");

      int width = metaSAR->general->sample_count;
      if (apply_dem_padding)
          width += PAD;

      trim(demSlant, demTrimSlant, idx, idy, width, demHeight);
  }

  if (clean_files) {
      clean(demClipped);
      clean(demSimAmp);
      clean(demSlant);
  }

  FREE(demClipped);
  FREE(demSimAmp);
  FREE(demSlant);

  *t_offset = t_off;
  *x_offset = x_off;

  return 0;
}

int asf_check_geolocation(char *sarFile, char *demFile, char *userMaskFile,
			  char *simAmpFile, char *demSlant)
{
  int do_corner_matching = TRUE;
  int do_fftMatch_verification = TRUE;
  int do_refine_geolocation = TRUE;
  int do_trim_slant_range_dem = TRUE;
  int apply_dem_padding = FALSE;
  int clean_files = TRUE;
  int madssap = FALSE; // mask and dem same size and projection
  int dem_grid_size = 20;
  double t_offset, x_offset;
  char output_dir[255];
  char *userMaskClipped = NULL;
  meta_parameters *metaSAR;
  strcpy(output_dir, "");

  if (userMaskFile)
      userMaskClipped = outputName("", userMaskFile, "_clip");

  metaSAR = meta_read(sarFile);
  match_dem(metaSAR, sarFile, demFile, sarFile, output_dir, userMaskFile,
	    simAmpFile, demSlant, userMaskClipped, dem_grid_size,
            do_corner_matching, do_fftMatch_verification,
            do_refine_geolocation, do_trim_slant_range_dem, apply_dem_padding,
            madssap, clean_files, FALSE, &t_offset, &x_offset);

  if (clean_files)
      clean(userMaskClipped);

  return 0;
}

int asf_terrcorr_ext(char *sarFile, char *demFile, char *userMaskFile,
                     char *outFile, double pixel_size, int clean_files,
                     int do_resample, int do_corner_matching, int do_interp,
                     int do_fftMatch_verification, int dem_grid_size,
                     int do_terrain_correction, int fill_value,
                     int generate_water_mask, int save_clipped_dem,
                     int update_original_metadata_with_offsets,
                     float mask_height_cutoff, int doRadiometric,
                     int smooth_dem_holes,
                     char **other_files_to_update_with_offset)
{
  char *resampleFile = NULL, *srFile = NULL, *resampleFile_2 = NULL;
  char *demTrimSimAmp = NULL, *demTrimSlant = NULL;
  char *lsMaskFile, *padFile = NULL, *userMaskClipped = NULL;
  char *deskewDemFile = NULL, *deskewDemMask = NULL;
  char *output_dir;
  double demRes, sarRes, maskRes=-1;
  meta_parameters *metaSAR, *metaDEM, *metamask=NULL;
  int force_resample = FALSE;
  double t_offset, x_offset;
  int madssap = FALSE; // mask and dem same size and projection
  int clean_resample_file = TRUE;
  int image_was_ground_range = TRUE;

  // we want passing in an empty string for the mask to mean "no mask"
  if (userMaskFile && strlen(userMaskFile) == 0)
      userMaskFile = NULL;

  if (generate_water_mask && userMaskFile) {
      asfPrintWarning("A User Mask was specified, yet generate_water_mask "
                      "was set to TRUE.\nIgnoring generate_water_mask.\n");
      generate_water_mask = FALSE;
  }

  // all temporary files are going to be placed in the output directory
  output_dir = getOutputDir(outFile);

  asfPrintStatus("Reading SAR metadata from: %s\n", sarFile);
  asfRequire(extExists(sarFile, ".meta") || extExists(sarFile, ".ddr"),
             "\nSAR metadata file missing or cannot be opened\n");
  metaSAR = meta_read(sarFile);

  //if (is_alos(metaSAR))
  //    asfPrintError("Terrain correction of ALOS data is not yet supported.\n");
  //if (is_scansar(metaSAR))
  //    asfPrintError("Terrain correction of ScanSar data is not yet "
  //                  "supported.\n");

  asfPrintStatus("Reading DEM metadata from: %s\n", demFile);
  asfRequire(extExists(demFile, ".meta") || extExists(demFile, ".ddr"),
             "\nDEM metadata file missing or cannot be opened\n");
  metaDEM = meta_read(demFile);

  if (metaDEM->general->image_data_type != DEM) {
      // not a DEM?
      if (metaDEM->general->image_data_type == MAGIC_UNSET_INT ||
          metaDEM->general->image_data_type == 0) {
          // unknown image type -- probably came directly from a .ddr
          // write a metadata file with the proper image_data_type.
          metaDEM->general->image_data_type = DEM;
          meta_write(metaDEM, demFile);
      } else {
          // possibly the user got the command-line args wrong?
          asfPrintError("Unexpected image type for DEM '%s': %d\n",
                        demFile, metaDEM->general->image_data_type);
      }
  }

  // generate a water mask if asked to do so
  if (generate_water_mask) {
      userMaskFile = outputName(output_dir, demFile, "_water_mask");

      const float cutoff = mask_height_cutoff + 0.00001; // a little fudge
      asfPrintStatus("Generating a Water Mask from DEM: %s.\n", demFile);
      asfPrintStatus("Height cutoff: %.2fm.\n", mask_height_cutoff);

      dem_to_mask(demFile, userMaskFile, cutoff);

      // setting this to true allows the clipping code to apply the same
      // clipping parameters to the dem and the mask.
      madssap = TRUE;
  }

  if (userMaskFile) {
    asfPrintStatus("Reading MASK metadata from: %s\n", userMaskFile);
    asfRequire(extExists(userMaskFile, ".meta") || extExists(userMaskFile, ".ddr"),
               "\nMASK metadata file missing or cannot be opened\n");
    metamask = meta_read(userMaskFile);
  }

  // Warning regarding square pixels in the DEM.  Still not sure
  // exactly what effects this could have
  if (metaDEM->general->x_pixel_size != metaDEM->general->y_pixel_size) {
    asfPrintStatus("DEM does not have square pixels!\n"
		   "x pixel size = %gm, y pixel size = %gm.\n",
	metaDEM->general->x_pixel_size, metaDEM->general->y_pixel_size);
    asfPrintStatus("Results may not be as expected.\n");
  }

  demRes = metaDEM->general->x_pixel_size;
  sarRes = metaSAR->general->x_pixel_size;
  if (userMaskFile)
  {
      maskRes = metamask->general->x_pixel_size;
      userMaskClipped = outputName(output_dir, userMaskFile, "_clip");
  }

  // Check if the user requested a pixel size that requires
  // too much oversampling.
  if (pixel_size > 0 && pixel_size < sarRes / 2) {
      asfPrintWarning(
	"The requested a pixel size could result in a significantly\n"
        "oversampled image.  Current pixel size is %g, you requested %g.\n",
	sarRes, pixel_size);
  }

  asfPrintStatus("SAR Image is %dx%d LxS, %gm pixels.\n",
		 metaSAR->general->line_count, metaSAR->general->sample_count,
		 sarRes);
  asfPrintStatus("DEM Image is %dx%d LxS, %gm pixels.\n",
		 metaDEM->general->line_count, metaDEM->general->sample_count,
		 demRes);
  if (userMaskFile)
  {
      asfPrintStatus("User Mask Image is %dx%d LxS, %gm pixels.\n",
                     metamask->general->line_count,
                     metamask->general->sample_count, maskRes);
  }

  // Downsample the SAR image closer to the reference DEM if needed.
  // Otherwise, the quality of the resulting terrain corrected SAR image
  // suffers. We put in a threshold of twice the resolution of the SAR
  // image. The -no-resample option overrides this default behavior.
  if (do_resample &&
      (force_resample || demRes > 2 * sarRes || pixel_size > 0))
  {
    if (pixel_size <= 0)
    {
      asfPrintStatus("DEM resolution is significantly lower than "
                     "SAR resolution.\n");

      // change the behavior here --- we used to downsample to match the
      // DEM resolution but we can probably get good results with twice
      // the DEM res, so let's use that.
      pixel_size = demRes / 2.0;

      asfPrintStatus("Resampling (Downsampling) SAR image to pixel size "
                     "of %g meter%s.\n", pixel_size,
                     pixel_size==1 ? "" : "s");
    }
    else
    {
        asfPrintStatus("Resampling (%s) SAR image to requested pixel size "
                       "of %g meter%s.\n",
                       pixel_size > sarRes ? "Downsampling" : "Oversampling",
                       pixel_size, pixel_size==1 ? "" : "s");
    }
    resampleFile = outputName(output_dir, sarFile, "_resample");

    // In slant range, must scale based on azimuth.
    // In ground range, can resample directly to the proper pixel size
    if (metaSAR->sar->image_type == 'S') {
        double scalfact;
        scalfact = metaSAR->general->y_pixel_size/pixel_size;
        resample(sarFile, resampleFile, scalfact, scalfact);
    } else {
        resample_to_square_pixsiz(sarFile, resampleFile, pixel_size);
    }

    meta_free(metaSAR);
    metaSAR = meta_read(resampleFile);

    asfPrintStatus("After resampling, SAR Image is %dx%d LxS, %gm pixels.\n",
		   metaSAR->general->line_count,
		   metaSAR->general->sample_count,
		   metaSAR->general->x_pixel_size);
  } else {
    pixel_size = sarRes;
    resampleFile = strdup(sarFile);
    clean_resample_file = FALSE; // don't want to delete the user's image! :)
  }

  // Calculate the slant range pixel size to pass into the ground range to
  // slant range conversion. This ensures that we have square pixels in the
  // output and don't need to scale the image afterwards.
  if (metaSAR->sar->image_type == 'G') {
    srFile = appendSuffix(sarFile, "_slant");
    double sr_pixel_size =
      (meta_get_slant(metaSAR,0,metaSAR->general->sample_count) -
       meta_get_slant(metaSAR,0,0)) / metaSAR->general->sample_count;
    asfPrintStatus("Converting to Slant Range...\n");

    gr2sr_pixsiz_pp(resampleFile, srFile, sr_pixel_size);

    meta_free(metaSAR);
    metaSAR = meta_read(srFile);

    asfPrintStatus("In slant range, SAR Image is %dx%d LxS, %gm pixels.\n",
		   metaSAR->general->line_count,
		   metaSAR->general->sample_count,
		   metaSAR->general->x_pixel_size);
  }
  else if (metaSAR->sar->image_type == 'P' ||
           metaSAR->sar->image_type == 'R')
  {
    // map projected... at the moment, just support scansar projection
    srFile = appendSuffix(sarFile, "_slant");
    //double sr_pixel_size =
    //  (meta_get_slant(metaSAR,0,metaSAR->general->sample_count) -
    //   meta_get_slant(metaSAR,0,0)) / metaSAR->general->sample_count;

    asfPrintStatus("Converting projected data to Slant Range...\n");
    to_sr(resampleFile, srFile);

    meta_free(metaSAR);
    metaSAR = meta_read(srFile);

    asfPrintStatus("In slant range, SAR Image is %dx%d LxS, %gm pixels.\n",
		   metaSAR->general->line_count,
		   metaSAR->general->sample_count,
		   metaSAR->general->x_pixel_size);
  } else {
    // image already in slant range - no action necessary
    srFile = strdup(resampleFile);
    image_was_ground_range = FALSE;
  }

  lsMaskFile = appendToBasename(outFile, "_mask");

  // Assign a couple of file names and match the DEM
  demTrimSimAmp = outputName(output_dir, demFile, "_sim_amp_trim");
  demTrimSlant = outputName(output_dir, demFile, "_slant_trim");
  match_dem(metaSAR, sarFile, demFile, srFile, output_dir, userMaskFile,
            demTrimSimAmp, demTrimSlant, userMaskClipped, dem_grid_size,
            do_corner_matching, do_fftMatch_verification, FALSE,
            TRUE, TRUE, madssap, clean_files, smooth_dem_holes,
            &t_offset, &x_offset);

  if (update_original_metadata_with_offsets)
  {
      // update input data file with the calculated offsets
      asfPrintStatus("Updating original metadata with offsets...\n");
      update_meta_offsets(sarFile, t_offset, x_offset);
  }

  if (other_files_to_update_with_offset)
  {
      char **other_file = other_files_to_update_with_offset;
      while (*other_file) {
          update_meta_offsets(*other_file, t_offset, x_offset);
          ++other_file;
      }
  }

  if (do_terrain_correction)
  {
      // Terrain correct the slant range image while bringing it back to
      // ground range geometry. This is done without radiometric correction
      // of the values.
      ensure_ext(&demTrimSlant, "img");
      ensure_ext(&srFile, "img");
      asfPrintStatus("Terrain correcting slant range image...\n");
      padFile = outputName(output_dir, srFile, "_pad");
      deskewDemFile = outputName(output_dir, srFile, "_dd");
      deskewDemMask = outputName(output_dir, srFile, "_ddm");
      trim(srFile, padFile, 0, 0, metaSAR->general->sample_count + PAD,
           metaSAR->general->line_count);
      deskew_dem(demTrimSlant, deskewDemFile, padFile, doRadiometric,
                 userMaskClipped, deskewDemMask, do_interp, fill_value);

      // After deskew_dem, there will likely be zeros on the left & right edges
      // of the image, we trim those off before finishing up.
      int startx, endx;
      trim_zeros(deskewDemFile, outFile, &startx, &endx);
      trim(deskewDemMask, lsMaskFile, startx, 0, endx,
           metaSAR->general->line_count);
      clean(padFile);
      clean(deskewDemFile);
      clean(deskewDemMask);

      meta_free(metaSAR);
      metaSAR = meta_read(outFile);

      // Because of the PP earth radius sr->gr fix, we may not have ended
      // up with the same x pixel size that the user requested.  So we will
      // just resample to the size that was requested.  This correction is
      // not necessary if the given image was in slant range.
      if (image_was_ground_range &&
          fabs(metaSAR->general->x_pixel_size - pixel_size) > 0.01)
      {
          asfPrintStatus("Resampling to proper range pixel size...\n");
          resampleFile_2 = outputName(output_dir, outFile, "_resample");
          renameImgAndMeta(outFile, resampleFile_2);
          resample_to_square_pixsiz(resampleFile_2, outFile, pixel_size);
          char *lsMaskFile_2 =
              outputName(output_dir, lsMaskFile, "_resample");
          renameImgAndMeta(lsMaskFile, lsMaskFile_2);
          resample_to_square_pixsiz(lsMaskFile_2, lsMaskFile, pixel_size);
          clean(lsMaskFile_2);
      } else {
          resampleFile_2 = NULL;
      }
  }
  else
  {
      // Just need to update the original metadata
      meta_free(metaSAR);

      metaSAR = meta_read(sarFile);
      metaSAR->sar->time_shift += t_offset;
      metaSAR->sar->slant_shift += x_offset;
      meta_write(metaSAR, outFile);
  }

  if (save_clipped_dem)
  {
    asfPrintStatus("Cutting out portion of the DEM...\n");
    cut_dem(metaSAR, metaDEM, demFile, output_dir);
  }

  if (clean_files) {
    asfPrintStatus("Removing intermediate files...\n");
    clean(demTrimSlant);
    clean(demTrimSimAmp);
    if (clean_resample_file) // false when resample file is the original image
        clean(resampleFile);
    clean(srFile);
    clean(resampleFile_2);
    clean(userMaskClipped);
  }

  // if we generated the mask, we should clean it up
  //  ... actually, keep the auto-generated mask.  If user is using
  //      asf_convert or the gui it will be deleted with the temporary
  //      files, unless they've elected to keep it (and in that case,
  //      we definitely don't want to have deleted it!)
  if (generate_water_mask)
    FREE(userMaskFile);

  FREE(resampleFile);
  FREE(srFile);
  FREE(padFile);
  FREE(deskewDemFile);
  FREE(deskewDemMask);
  FREE(lsMaskFile);
  FREE(resampleFile_2);
  FREE(userMaskClipped);

  meta_free(metaSAR);
  meta_free(metaDEM);

  free(output_dir);

  asfPrintStatus("Done!\n");
  return 0; // success
}
