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
#include <asf_reporting.h>
#include <asf_contact.h>
#include <asf_copyright.h>
#include <asf_license.h>
#include <asf_sar.h>
#include <poly.h>

#include <asf_terrcorr.h>

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
  sprintf(suffix_pid, "%s_tctmp%d", suffix, (int)getpid());

  char * ret = appendToBasename(inFile, suffix_pid);

  free(suffix_pid);
  return ret;
}

static char *outputName(const char *dir, const char *base, const char *suffix)
{
    int dirlen = strlen(dir);
    int baselen = strlen(base);
    int len = dirlen > baselen ? dirlen : baselen;

    char *fil = MALLOC(sizeof(char)*(dirlen+baselen+strlen(suffix)));
    char *dirTmp = MALLOC(sizeof(char) * len);
    char *fileTmp = MALLOC(sizeof(char) * len);

    split_dir_and_file(dir, dirTmp, fileTmp);
    strcpy(fil, dirTmp);

    split_dir_and_file(base, dirTmp, fileTmp);
    strcat(fil, fileTmp);

    char *ret = appendSuffix(fil, suffix);

    free(fil);
    free(dirTmp);
    free(fileTmp);

//    printf("Output name: %s %s %s\n"
//           "          -> %s\n", dir, base, suffix, ret);

    return ret;
}

// attempt to remove "<file>.img" and "<file>.meta", etc files
static void clean(const char *file)
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

static void 
fftMatchQ(char *file1, char *file2, float *dx, float *dy, float *cert)
{
  int qf_saved = quietflag;
  quietflag = 1;
  fftMatch(file1, file2, NULL, dx, dy, cert);
  quietflag = qf_saved;
}

static int mini(int a, int b)
{
  return a < b ? a : b;
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

int asf_terrcorr(char *sarFile, char *demFile,
		 char *outFile, double pixel_size)
{
  int do_fftMatch_verification = TRUE;
  int do_corner_matching = TRUE;
  int do_resample = TRUE;
  int clean_files = TRUE;
  int dem_grid_size = 20;
  
  return asf_terrcorr_ext(sarFile, demFile, outFile, pixel_size,
			  clean_files, do_resample, do_corner_matching,
			  do_fftMatch_verification, dem_grid_size);
}

static char * getOutputDir(char *outFile)
{
    char *d = MALLOC(sizeof(char) * strlen(outFile));
    char *f = MALLOC(sizeof(char) * strlen(outFile));
    split_dir_and_file(outFile, d, f);
    free(f);
    return d;
}

int asf_terrcorr_ext(char *sarFile, char *demFile,
		     char *outFile, double pixel_size, int clean_files,
		     int do_resample, int do_corner_matching,
		     int do_fftMatch_verification, int dem_grid_size)
{
  char *resampleFile, *srFile, *resampleFile_2;
  char *demGridFile, *demClipped, *demSlant, *demSimAmp;
  char *demTrimSimAmp, *demTrimSlant;
  char *maskFile, *outMaskFile;
  char *output_dir;
  double demRes, sarRes;
  int demWidth, demHeight;
  meta_parameters *metaSAR, *metaDEM;
  float dx, dy, cert;
  int idx, idy;
  int polyOrder = 5;
  int force_resample = FALSE;
  int loop_count = 0;
  const float required_vertical_match = 5;
  float vertical_fudge = 0.0;
  double coverage_pct;

  asfPrintStatus("Reading metadata...\n");
  metaSAR = meta_read(sarFile);
  metaDEM = meta_read(demFile);

  output_dir = getOutputDir(outFile);

  // some checks for square pixels, as the algorithm assumes it
  // taking this out... I don't think the algorithm assumes this.
//  if (metaSAR->general->x_pixel_size != metaSAR->general->y_pixel_size) {
//    asfPrintStatus("SAR image does not have square pixels!\n"
//		   "x pixel size = %gm, y pixel size = %gm.\n",
//	metaSAR->general->x_pixel_size, metaSAR->general->y_pixel_size);
//    if (do_resample) {
//      asfPrintStatus("Will resample SAR image to square pixels.\n");
//      force_resample = TRUE;
//    } else {
//      asfPrintStatus("Terrain Correction results may not be as expected.\n");
//    }
//  }
  
  if (metaDEM->general->x_pixel_size != metaDEM->general->y_pixel_size) {
    asfPrintStatus("DEM does not have square pixels!\n"
		   "x pixel size = %gm, y pixel size = %gm.\n",
	metaDEM->general->x_pixel_size, metaDEM->general->y_pixel_size);
    asfPrintStatus("Terrain Correction results may not be as expected.\n");
  }

  demRes = metaDEM->general->x_pixel_size;
  sarRes = metaSAR->general->x_pixel_size;

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

  // Downsample the SAR image closer to the reference DEM if needed.
  // Otherwise, the quality of the resulting terrain corrected SAR image 
  // suffers. We put in a threshold of 1.5 times the resolution of the SAR
  // image. The -no-resample option overrides this default behavior.
  if (do_resample && 
      (force_resample || demRes > 1.5 * sarRes || pixel_size > 0)) 
  {
    if (pixel_size <= 0)
    {
      asfPrintStatus("DEM resolution is significantly lower than SAR resolution.\n");
      pixel_size = demRes;
      asfPrintStatus("Resampling (Downsampling) SAR image to pixel size of %g meters.\n", pixel_size);
    }
    else
    {
	    asfPrintStatus("Resampling (Oversampling) SAR image to pixel size of %g meters.\n",
			   pixel_size);
    }
    resampleFile = outputName(output_dir, sarFile, "_resample");
    resample_to_square_pixsiz(sarFile, resampleFile, pixel_size);
    meta_free(metaSAR);
    metaSAR = meta_read(resampleFile);

    asfPrintStatus("After resampling, SAR Image is %dx%d LxS, %gm pixels.\n",
		   metaSAR->general->line_count,
		   metaSAR->general->sample_count,
		   metaSAR->general->x_pixel_size);
  } else {
    pixel_size = sarRes;
    resampleFile = strdup(sarFile);
  }

  // Calculate the slant range pixel size to pass into the ground range to
  // slant range conversion. This ensures that we have square pixels in the
  // output and don't need to scale the image afterwards.
  if (metaSAR->sar->image_type != 'S') {
    srFile = appendSuffix(sarFile, "_slant");
    double sr_pixel_size = 
      (meta_get_slant(metaSAR,0,metaSAR->general->sample_count) -
       meta_get_slant(metaSAR,0,0)) / metaSAR->general->sample_count;
    asfPrintStatus("Converting to Slant Range...\n");

    gr2sr_pixsiz(resampleFile, srFile, sr_pixel_size);

    meta_free(metaSAR);
    metaSAR = meta_read(srFile);

    asfPrintStatus("In slant range, SAR Image is %dx%d LxS, %gm pixels.\n",
		   metaSAR->general->line_count,
		   metaSAR->general->sample_count,
		   metaSAR->general->x_pixel_size);
  } else {
    srFile = strdup(resampleFile);
  }

  maskFile = outputName(output_dir, outFile, "_mask_slant");
  outMaskFile = appendToBasename(outFile, "_mask");

  // do-while that will repeat the dem grid generation and the fftMatch
  // of the sar & simulated sar, until the fftMatch doesn't turn up a
  // big vertical offset.
  do
  {
      ++loop_count;

      // Generate a point grid for the DEM extraction.
      // The width and height of the grid is defined in slant range image
      // coordinates, while the grid is actually calculated in DEM space.
      // There is a buffer of 400 pixels in far range added to have enough
      // DEM around when we get to correcting the terrain.
      asfPrintStatus("Generating %dx%d DEM grid...\n",
		     dem_grid_size, dem_grid_size);
      demGridFile = outputName(output_dir, sarFile, "_demgrid");
      create_dem_grid_ext(demFile, srFile, demGridFile,
			  metaSAR->general->sample_count,
			  metaSAR->general->line_count, dem_grid_size,
			  vertical_fudge, &coverage_pct);
      
      if (coverage_pct <= 0) {
	asfPrintError("DEM and SAR images do not overlap!\n");
      } else if (coverage_pct <= 25) {
	asfPrintError("Insufficient DEM coverage!\n");
      } else if (coverage_pct <= 99) {
	asfPrintWarning(
	  "Incomplete DEM coverage, your result will be clipped in the\n"
	  "areas where no DEM data is available.\n");
      }

      // Fit a fifth order polynomial to the grid points.
      // This polynomial is then used to extract a subset out of the reference 
      // DEM.
      asfPrintStatus("Fitting order %d polynomial to DEM...\n", polyOrder);
      double maxErr;
      poly_2d *fwX, *fwY, *bwX, *bwY;
      fit_poly(demGridFile, polyOrder, &maxErr, &fwX, &fwY, &bwX, &bwY);
      asfPrintStatus("Maximum error in polynomial fit: %g.\n", maxErr);

      // Here is the actual work done for cutting out the DEM.
      // The adjustment of the DEM width by 400 pixels (originated in
      // create_dem_grid) needs to be factored in.
      demClipped = appendSuffix(demFile, "_clip");
      demWidth = metaSAR->general->sample_count + 400;
      demHeight = metaSAR->general->line_count;
      
      asfPrintStatus("Clipping DEM to %dx%d LxS using polynomial fit...\n",
		     demHeight, demWidth);
      remap_poly(fwX, fwY, bwX, bwY, demWidth, demHeight, demFile, demClipped);
      poly_delete(fwX);
      poly_delete(fwY);
      poly_delete(bwX);
      poly_delete(bwY);
      
      // Generate a slant range DEM and a simulated amplitude image.
      asfPrintStatus("Generating slant range DEM and "
		     "simulated sar image...\n");
      demSlant = outputName(output_dir, demFile, "_slant");
      demSimAmp = outputName(output_dir, demFile, "_sim_amp");
      reskew_dem(srFile, demClipped, demSlant, demSimAmp, maskFile);

      // Resize the simulated amplitude to match the slant range SAR image.
      asfPrintStatus("Resizing simulated sar image...\n");
      demTrimSimAmp = outputName(output_dir, demFile, "_sim_amp_trim");
      trim(demSimAmp, demTrimSimAmp, 0, 0, metaSAR->general->sample_count,
	   demHeight);

      // Match the real and simulated SAR image to determine the offset.
      // Read the offset out of the offset file.
      asfPrintStatus("Determining image offsets...\n");
      fftMatchQ(srFile, demTrimSimAmp, &dx, &dy, &cert);
      asfPrintStatus("Correlation (cert=%5.2f%%): dx=%f dy=%f\n",
		     100*cert, dx, dy);

      idx = - int_rnd(dx);
      idy = - int_rnd(dy);

      if (fabs(dy) > required_vertical_match)
      {
	  // The fftMatch resulted in a large vertical offset!
	  // This means we very likely did not clip the right portion of
	  // the DEM.  So, shift the slant range image and re-clip.

	  if (loop_count >= 3)
	  {
	      asfPrintWarning(
		"Could not resolve vertical offset!\n"
		"Continuing, however your terrain correction result may\n"
		"be incomplete and/or incorrect.\n");
	      break;
	  }
	  else
	  {
	      asfPrintStatus("Found a large vertical offset (%d pixels)\n"
			     "Adjusting SAR image and re-clipping DEM.\n",
			     idy);

	      vertical_fudge = -dy;
	  }
      }
  } while (fabs(dy) > required_vertical_match);

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

    asfPrintStatus("Correlation after shift (cert=%5.2f%%): dx=%f dy=%f\n", 
		   100*cert, dx2, dy2);

    double match_tolerance = 1.0;
    if (sqrt(dx2*dx2 + dy2*dy2) > match_tolerance) {
      asfPrintError("Correlated images failed to match!\n"
		    " Original fftMatch offset: (dx,dy) = %14.9f,%14.9f\n"
		    "   After shift, offset is: (dx,dy) = %14.9f,%14.9f\n",
		    dx, dy, dx2, dy2);
    }
  }

  // Apply the offset to the slant range DEM.
  asfPrintStatus("Applying offsets to slant range DEM...\n");
  demTrimSlant = outputName(output_dir, demFile, "_slant_trim");
  trim(demSlant, demTrimSlant, idx, idy, metaSAR->general->sample_count,
       demHeight);

  // Terrain correct the slant range image while bringing it back to
  // ground range geometry. This is done without radiometric correction
  // of the values.
  ensure_ext(&demTrimSlant, "img");
  ensure_ext(&srFile, "img");
  asfPrintStatus("Terrain correcting slant range image...\n");
  deskew_dem(demTrimSlant, outFile, srFile, 0, maskFile, outMaskFile);

  // Because of the PP earth radius sr->gr fix, we may not have ended
  // up with the same x pixel size that the user requested.  So we will
  // just resample to the size that was requested.
  meta_free(metaSAR);
  metaSAR = meta_read(outFile);
  if (fabs(metaSAR->general->x_pixel_size - pixel_size) > 0.01) {
      asfPrintStatus("Resampling to proper range pixel size...\n");
      resampleFile_2 = outputName(output_dir, outFile, "_resample");
      renameImgAndMeta(outFile, resampleFile_2);
      resample_to_square_pixsiz(resampleFile_2, outFile, pixel_size);
  } else {
      resampleFile_2 = NULL;
  }

  if (clean_files) {
    asfPrintStatus("Removing intermediate files...\n");
    clean(resampleFile);
    clean(srFile);
    clean(demClipped);
    clean(demGridFile);
    clean(demTrimSlant);
    clean(demTrimSimAmp);
    clean(demSimAmp);
    clean(demSlant);
    clean(maskFile);
    if (resampleFile_2)
        clean(resampleFile_2);
  }

  asfPrintStatus("Terrain Correction Complete!\n");

  free(resampleFile);
  free(srFile);
  free(demClipped);
  free(demGridFile);
  free(demTrimSlant);
  free(demTrimSimAmp);
  free(demSimAmp);
  free(demSlant);
  free(maskFile);
  free(outMaskFile);
  if (resampleFile_2)
      free(resampleFile_2);

  meta_free(metaSAR);
  meta_free(metaDEM);

  free(output_dir);

  return 0;
}
