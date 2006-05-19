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

static int int_rnd(double x)
{
  return (int)floor(x+0.5);
}

static char * change_extension(const char * file, const char * ext)
{
  char * replaced = (char *)
    MALLOC(sizeof(char) * (strlen(file) + strlen(ext) + 10));
  
  strcpy(replaced, file);
  char * p = findExt(file);
  
  if (p)
    *p = '\0';
  
  strcat(replaced, ".");
  strcat(replaced, ext);
  
  return replaced;
}

static char * appendSuffix(const char *inFile, const char *suffix)
{
  char *suffix_pid = MALLOC(sizeof(char)*(strlen(suffix)+25));
  sprintf(suffix_pid, "%s_tctmp%d", suffix, (int)getpid());

  char *ret = MALLOC(sizeof(char)*(strlen(inFile)+strlen(suffix_pid)+5));
  strcpy(ret, inFile);
  char *p = findExt(ret);
  if (p && p != ret) {
    char *ext;
    *p++ = '\0';
    ext = strdup(p);
    strcat(ret, suffix_pid);
    strcat(ret, ".");
    strcat(ret, ext);
    free(ext);
  } else {
    strcat(ret, suffix_pid);
  }

  free(suffix_pid);
  return ret;
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

static int file_exists(const char * file)
{
  int fd = open(file, 0);
  int stat = fd >= 3;
  close(fd);
  return stat;
}

static void remove_file(const char * file)
{
  if (file_exists(file)) {
    // asfPrintStatus("Removing intermediate file: %s\n", file);
    unlink(file);
  }
}

// attempt to remove "<file>.img" and "<file>.meta", etc files
static void clean(const char *file)
{
  char * img_file = change_extension(file, "img");
  char * meta_file = change_extension(file, "meta");
  char * ddr_file = change_extension(file, "ddr");

  remove_file(img_file);
  remove_file(meta_file);
  remove_file(ddr_file);
  remove_file(file);

  free(img_file);
  free(meta_file);
  free(ddr_file);
}

static void fftMatch_atCornersQ(char *srFile, char *demTrimSimAmp)
{
  int qf_saved = quietflag;
  quietflag = 1;
  fftMatch_atCorners(srFile, demTrimSimAmp);  
  quietflag = qf_saved;
}

static void fftMatchQ(char *file1, char *file2, float *dx, float *dy)
{
  float cert;
  int qf_saved = quietflag;
  quietflag = 1;
  fftMatch(file1, file2, NULL, dx, dy, &cert);
  quietflag = qf_saved;
}

int asf_terrcorr(char *sarFile, char *demFile,
		 char *outFile, double pixel_size)
{
  int do_fftMatch_verification = TRUE;
  int do_resample = TRUE;
  int clean_files = TRUE;
  int dem_grid_size = 20;
  
  return asf_terrcorr_ext(sarFile, demFile, outFile, pixel_size,
			  clean_files, do_resample, do_fftMatch_verification,
			  dem_grid_size);
}

int asf_terrcorr_ext(char *sarFile, char *demFile,
		     char *outFile, double pixel_size,
		     int clean_files, int do_resample,
		     int do_fftMatch_verification, int dem_grid_size)
{
  char *resampleFile, *srFile;
  char *demGridFile, *demClipped, *demSlant, *demSimAmp;
  char *demTrimSimAmp, *demTrimSlant;
  double demRes, sarRes;
  int demWidth, demHeight;
  meta_parameters *metaSAR, *metaDEM;
  float dx, dy;
  int idx, idy;
  int polyOrder = 5;

  asfPrintStatus("Reading metadata...\n");
  metaSAR = meta_read(sarFile);
  metaDEM = meta_read(demFile);

  demRes = metaDEM->general->x_pixel_size;
  sarRes = metaSAR->general->x_pixel_size;
    
  asfPrintStatus("SAR Image is %dx%d, %gm pixels.\n",
		 metaSAR->general->line_count, metaSAR->general->sample_count,
		 sarRes);
  asfPrintStatus("DEM Image is %dx%d, %gm pixels.\n",
		 metaDEM->general->line_count, metaDEM->general->sample_count,
		 demRes);

  // Downsample the SAR image closer to the reference DEM if needed.
  // Otherwise, the quality of the resulting terrain corrected SAR image 
  // suffers. We put in a threshold of 1.5 times the resolution of the SAR
  // image. The -no-resample option overwrites this default behavior.
  if (do_resample && (demRes > 1.5 * sarRes || pixel_size > 0)) {
    if (pixel_size <= 0)
    {
      asfPrintStatus(
	"DEM resolution is significantly higher than SAR resolution.\n");
      pixel_size = demRes;
    }

    asfPrintStatus("Resampling SAR image to pixel size of %g meters.\n",
		   pixel_size);

    resampleFile = appendSuffix(sarFile, "_resample");
    resample_to_square_pixsiz(sarFile, resampleFile, pixel_size);
    meta_free(metaSAR);
    metaSAR = meta_read(resampleFile);

    asfPrintStatus("After resmapling, SAR Image is %dx%d, %gm pixels.\n",
		   metaSAR->general->line_count,
		   metaSAR->general->sample_count,
		   metaSAR->general->x_pixel_size);
  } else {
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

    asfPrintStatus("In slant range, SAR Image is %dx%d, %gm pixels.\n",
		   metaSAR->general->line_count,
		   metaSAR->general->sample_count,
		   metaSAR->general->x_pixel_size);
  } else {
    srFile = strdup(resampleFile);
  }

  // Generate a point grid for the DEM extraction.
  // The width and height of the grid is defined in slant range image
  // coordinates, while the grid is actually calculated in DEM space.
  // There is a buffer of 400 pixels in far range added to have enough
  // DEM around when we get to correcting the terrain.
  asfPrintStatus("Generating %dx%d DEM grid...\n",
		 dem_grid_size, dem_grid_size);
  demGridFile = appendSuffix(sarFile, "_demgrid");
  create_dem_grid_ext(demFile, srFile, demGridFile,
		      metaSAR->general->sample_count,
		      metaSAR->general->line_count, dem_grid_size);


  // Fit a fifth order polynomial to the grid points.
  // This polynomial is then used to extract a subset out of the reference 
  // DEM.
  asfPrintStatus("Fitting order %d polynomial to DEM...\n", polyOrder);
  double maxErr;
  poly_2d *fwX, *fwY, *bwX, *bwY;
  fit_poly(demGridFile, polyOrder, &maxErr, &fwX, &fwY, &bwX, &bwY);
  asfPrintStatus("Maximum error in polynomial fit: %g.\n", maxErr);

  int delete_this_old_crap = 0;
  char *demPolyFileOld;
  if (delete_this_old_crap) {
    demPolyFileOld = appendSuffix(sarFile, "_dempoly_old");
    asfSystem("fit_poly %s %d %s", demGridFile, polyOrder, demPolyFileOld);
  }

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
  asfPrintStatus("Generating slant range DEM and simulated sar image...\n");
  demSlant = appendSuffix(demFile, "_slant");
  demSimAmp = appendSuffix(demFile, "_sim_amp");
  reskew_dem(srFile, demClipped, demSlant, demSimAmp);

  // Resize the simulated amplitude to match the slant range SAR image.
  asfPrintStatus("Resizing simulated sar image...\n");
  demTrimSimAmp = appendSuffix(demFile, "_sim_amp_trim");
  trim(demSimAmp, demTrimSimAmp, 0, 0, metaSAR->general->sample_count,
       demHeight);

  // Match the real and simulated SAR image to determine the offset.
  // Read the offset out of the offset file.
  asfPrintStatus("Determining image offsets...\n");
  fftMatchQ(srFile, demTrimSimAmp, &dx, &dy);
  asfPrintStatus("Correlation: dx=%f dy=%f\n", dx, dy);
  idx = - int_rnd(dx);
  idy = - int_rnd(dy);

  // Corner test
  asfPrintStatus("Doing corner fftMatching...\n");
  fftMatch_atCornersQ(srFile, demTrimSimAmp);

  // Apply the offset to the simulated amplitude image.
  asfPrintStatus("Applying offsets to simulated sar image...\n");
  trim(demSimAmp, demTrimSimAmp, idx, idy, metaSAR->general->sample_count,
       demHeight);

  // Verify that the applied offset in fact does the trick.
  if (do_fftMatch_verification) {
    float dx2, dy2;

    asfPrintStatus("Verifying offsets are now close to zero...\n");
    fftMatchQ(srFile, demTrimSimAmp, &dx2, &dy2);

    asfPrintStatus("Correlation after shift: dx=%f dy=%f\n", dx2, dy2);

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
  demTrimSlant = appendSuffix(demFile, "_slant_trim");
  trim(demSlant, demTrimSlant, idx, idy, metaSAR->general->sample_count,
       demHeight);

  // Terrain correct the slant range image while bringing it back to
  // ground range geometry. This is done without radiometric correction
  // of the values.
  ensure_ext(&demTrimSlant, "img");
  ensure_ext(&srFile, "img");
  asfPrintStatus("Terrain correcting slant range image...\n");
  deskew_dem(demTrimSlant, outFile, srFile, 0);

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

  meta_free(metaSAR);
  meta_free(metaDEM);

  return 0;
}
