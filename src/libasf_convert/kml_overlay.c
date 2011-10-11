#include <asf_contact.h>
#include <asf_license.h>

#include <asf_convert.h>
#include <asf_vector.h>
#include <sys/types.h> /* 'DIR' structure (for opendir) */
#include <dirent.h>    /* for opendir itself            */

#include "asf.h"
#include "ceos.h"
#include "envi.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "asf_raster.h"
#include "proj.h"
#include "asf_contact.h"
#include <unistd.h>
#include <assert.h>
#include "zlib.h"

#define CHUNK 16384

extern int statusflag;

static double max2(double a, double b)
{
  return a > b ? a : b;
}

static double max4(double a, double b, double c, double d)
{
  return max2(max2(a,b), max2(c,d));
}

static double min2(double a, double b)
{
  return a < b ? a : b;
}

static double min4(double a, double b, double c, double d)
{
  return min2(min2(a,b), min2(c,d));
}

// Zlib does not handle zip archives. 
// Compresses just fine but can't put two files in one archive.
static int zipFiles(char *kmzFile, char *kmlFile, char *pngFile)
{
  FILE *fpKML = NULL, *fpPNG = NULL, *fpOut = NULL;
  z_stream stream;
  int flush, ret;
  unsigned int outBytes;
  unsigned char inBuf[CHUNK], outBuf[CHUNK];

  // Open files
  fpKML = FOPEN(kmlFile, "rb");
  fpPNG = FOPEN(pngFile, "rb");
  fpOut = FOPEN(kmzFile, "wb");

  // Initialization with average compression level with
  // usual compromise of size versus speed
  stream.zalloc = Z_NULL;
  stream.zfree = Z_NULL;
  stream.opaque = Z_NULL;
  ret = deflateInit(&stream, Z_DEFAULT_COMPRESSION);
  if (ret != Z_OK) {
    return ret;
  }

  // Compress KML file
  do {
    stream.avail_in = fread(inBuf, 1, CHUNK, fpKML);
    if (ferror(fpKML)) {
      (void)deflateEnd(&stream);
      return Z_ERRNO;
    }
    flush = feof(fpKML) ? Z_FINISH : Z_NO_FLUSH;
    stream.next_in = inBuf;
    do {
      stream.avail_out = CHUNK;
      stream.next_out = outBuf;
      ret = deflate(&stream, flush);
      assert(ret != Z_STREAM_ERROR);
      outBytes = CHUNK - stream.avail_out;
      if (fwrite(outBuf, 1, outBytes, fpOut) != outBytes || ferror(fpOut)) {
	(void)deflateEnd(&stream);
	return Z_ERRNO;
      }
    } while (stream.avail_out == 0);
    assert(stream.avail_in == 0);
  } while (flush != Z_FINISH);

  deflateReset(&stream);

  // Compress PNG file
  do {
    stream.avail_in = fread(inBuf, 1, CHUNK, fpPNG);
    if (ferror(fpPNG)) {
      (void)deflateEnd(&stream);
      return Z_ERRNO;
    }
    flush = feof(fpPNG) ? Z_FINISH : Z_NO_FLUSH;
    stream.next_in = inBuf;
    do {
      stream.avail_out = CHUNK;
      stream.next_out = outBuf;
      ret = deflate(&stream, flush);
      assert(ret != Z_STREAM_ERROR);
      outBytes = CHUNK - stream.avail_out;
      if (fwrite(outBuf, 1, outBytes, fpOut) != outBytes || ferror(fpOut)) {
	(void)deflateEnd(&stream);
	return Z_ERRNO;
      }
    } while (stream.avail_out == 0);
    assert(stream.avail_in == 0);
  } while (flush != Z_FINISH);

  // Clean up
  (void)deflateEnd(&stream);
  FCLOSE(fpKML);
  FCLOSE(fpPNG);
  FCLOSE(fpOut);
  //remove_file(kmlFile);

  return FALSE;
}

int kml_overlay(char *inFile, char *outFile, int zip)
{
  return kml_overlay_ext(inFile, outFile, 8, 0, NULL, NULL, NULL, NULL, zip);
}

int kml_overlay_ext(char *inFile, char *outFile, int reduction, 
		    int transparency, char *colormap, char *rgb, 
		    char *polsarpro, char *band, int zip)
{
  meta_parameters *meta;
  double pixel_size;
  int is_insar = isInSAR(inFile);
  if (is_insar)
    printf("InSAR file\n");
  int is_polsarpro = isPolSARpro(inFile);

  // Create temporary processing directory
  char cwd[1024];
  getcwd(cwd, 1024);
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);
  sprintf(tmpDir, "%s-", outFile);
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);

  // Determine size of output
  if (is_polsarpro) {
    // Only going to work safely when PolSARPro data is already geocoded
    char *polsarName = (char *) MALLOC(sizeof(char)*(strlen(inFile) + 20));
    sprintf(polsarName, "%s", inFile);
    char *ext = findExt(polsarName);
    if (!ext || (ext && (strcmp_case(ext, ".bin") != 0)))
      polsarName = appendExt(inFile, ".bin");
    char *headerName = (char *) MALLOC(sizeof(char)*(strlen(polsarName) + 10));
    sprintf(headerName, "%s.hdr", polsarName);
    envi_header *envi = read_envi(headerName);
    meta = envi2meta(envi);
    if (!meta->projection) {
      meta_free(meta);
      FREE(envi);
      asfPrintError("PolSARPro data (%s) is not map projected\n", polsarName);
    }
    else {
      pixel_size = meta->projection->perX;
      if (meta->general->line_count > 1024)
	pixel_size *= reduction;
      meta_free(meta);
      FREE(envi);
    }

    // Only colormap or rgb option can be chosen. Not both.
    if (colormap && strlen(colormap) && rgb && strlen(rgb))
      asfPrintError("Only colormap or rgb option can be chosen. Not both.\n");

    // Check if colormap is supposed to be applied. Needs to have a PolSARPro 
    // data type defined as well.
    if (colormap && strlen(colormap) && polsarpro && strlen(polsarpro))
      asfPrintStatus("Colormap (%s) is applied to PolSARPro %s ...\n",
		     colormap, polsarpro);
    else if (colormap && strlen(colormap))
      asfPrintError("When applying a colormap to PolSARPro data, the "
		    "PolSARPro\ndata type needs to be defined as well.\n");

    // Check if PolSARPro decomposition is chosen. Needs to have the bands in
    // the RGB option defined as well.
    if (polsarpro && strcmp_case(polsarpro, "DECOMPOSITION") == 0 && 
	rgb && strlen(rgb))
      asfPrintStatus("PolSARPro decomposition stored as RGB (%s)\n", rgb);
    else if (rgb && strlen(rgb))
      asfPrintError("For PolSARPro decomposition the RGB channels need to be "
		    "\ndefined as well.\n");
  }
  else {
    meta = meta_read(inFile);
    pixel_size = meta->general->x_pixel_size;
    if (meta->general->line_count > 1024)
      pixel_size *= reduction;
    meta_free(meta);
  }

  // Generate input names
  char *inName = (char *) MALLOC(sizeof(char)*(strlen(inFile)+1));
  char *inDir = (char *) MALLOC(sizeof(char)*1024);
  split_dir_and_file(inFile, inDir, inName);
  if (strlen(inDir) == 0)
    sprintf(inDir, "%s%c", cwd, DIR_SEPARATOR);

  // Generate output names
  char *baseName = (char *) MALLOC(sizeof(char)*(strlen(outFile)+1));
  char *outputFile = (char *) MALLOC(sizeof(char)*(strlen(outFile)+1));
  char *outDir = (char *) MALLOC(sizeof(char)*1024);
  split_dir_and_file(outFile, outDir, outputFile);
  baseName = stripExt(outputFile);
  if (strlen(outDir) == 0)
    sprintf(outDir, "%s%c", cwd, DIR_SEPARATOR);
  char metaFile[512], pngFile[512], kmlFile[512], kmzFile[512];
  sprintf(pngFile, "%s%s.png", outDir, baseName);
  sprintf(kmlFile, "%s.kml", baseName);
  sprintf(kmzFile, "%s%s.kmz", outDir, baseName);

  // Generating a customized configuration for asf_mapready
  chdir(tmpDir);
  char configFileName[255];
  sprintf(configFileName, "asf_mapready.config");
  FILE *fp = FOPEN(configFileName, "w");
  fprintf(fp, "Temporary asf_mapready configuration file\n\n");
  fprintf(fp, "[General]\n");
  fprintf(fp, "input file = %s%s\n", inDir, inName);
  fprintf(fp, "output file = %s\n", pngFile);
  fprintf(fp, "project = 0\n");
  fprintf(fp, "files = 0\n");
  if (is_polsarpro)
    fprintf(fp, "import = 1\n");
  else
    fprintf(fp, "import = 0\n");
  fprintf(fp, "terrain correction = 0\n");
  fprintf(fp, "geocoding = 1\n");
  fprintf(fp, "export = 1\n");
  fprintf(fp, "dump envi header = 0\n");
  fprintf(fp, "short configuration file = 1\n\n");
  if (is_polsarpro) {
    fprintf(fp, "[Import]\n");
    fprintf(fp, "format = POLSARPRO\n");
    fprintf(fp, "radiometry = AMPLITUDE_IMAGE\n");
    if (colormap)
      fprintf(fp, "polsarpro colormap = %s\n", colormap);
    if (polsarpro && strlen(polsarpro)) {
      if (strcmp_case(polsarpro, "SEGMENTATION") == 0)
	fprintf(fp, "image data type = POLARIMETRIC_SEGMENTATION\n");
      else if (strcmp_case(polsarpro, "DECOMPOSITION") == 0) {
	fprintf(fp, "image data type = POLARIMETRIC_DECOMPOSITION\n");
	fprintf(fp, "output db = 1\n");
      }
      else if (strcmp_case(polsarpro, "PARAMETER") == 0)
	fprintf(fp, "image data type = POLARIMETRIC_PARAMETER\n");
    }
    fprintf(fp, "\n");
  }
  fprintf(fp, "[Geocoding]\n");
  fprintf(fp, "projection = %s/projections/equi_rectangular/"
	  "equi_rectangular_world.proj\n", get_asf_share_dir());
  fprintf(fp, "pixel spacing = %.2lf\n", pixel_size);
  fprintf(fp, "force = 1\n\n");
  fprintf(fp, "[Export]\n");
  fprintf(fp, "format = PNG_GE\n");
  if (colormap && strlen(colormap))
    fprintf(fp, "byte conversion = TRUNCATE\n");
  else if (polsarpro && strlen(polsarpro) &&
	   strcmp_case(polsarpro, "DECOMPOSITION") == 0) 
    fprintf(fp, "byte conversion = MINMAX_MEDIAN\n");
  else
    fprintf(fp, "byte conversion = SIGMA\n");
  if (rgb && strlen(rgb))
    fprintf(fp, "rgb banding = %s\n", rgb);
  if (band && strlen(band))
    fprintf(fp, "band = %s\n", band);
  //if (is_insar)
  //fprintf(fp, "rgb look up table = %s\n", colormap);
  FCLOSE(fp);

  // Save the asf tmp directory that we're using, we will need to
  // restore it later.  Same for the other stuff.
  const char *current_tmp_dir = get_asf_tmp_dir();
  int saved_statusflag = statusflag;
  int saved_quietflag = quietflag;
  quietflag = 2;
  statusflag = FALSE;

  // Run input file through asf_mapready
  asfPrintStatus("\n\nGenerating overlay PNG file ...\n\n");
  asf_convert(FALSE, configFileName);

  // restore original tmp dir
  set_asf_tmp_dir(current_tmp_dir);
  current_tmp_dir = NULL;

  baseName = get_basename(outFile);
  sprintf(kmlFile, "%s.kml", baseName);
  sprintf(metaFile, "%s%s.meta", outDir, baseName);

  // Calculate the lat/lon extents from the geocoded browse image
  meta = meta_read(metaFile);
  double startX = meta->projection->startX;
  double startY = meta->projection->startY;
  double perX = meta->projection->perX;
  double perY = meta->projection->perY;
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;

  double lat_UL, lon_UL, lat_UR, lon_UR, lat_LL, lon_LL, lat_LR, lon_LR;
    
  double ul_x = startX;
  double ul_y = startY;
  double ur_x = startX + perX * ns;
  double ur_y = startY;
  double ll_x = startX;
  double ll_y = startY + perY * nl;
  double lr_x = startX + perX * ns;
  double lr_y = startY + perY * nl;
  
  EQR2latLon(ul_x, ul_y, &lat_UL, &lon_UL);
  EQR2latLon(ur_x, ur_y, &lat_UR, &lon_UR);
  EQR2latLon(ll_x, ll_y, &lat_LL, &lon_LL);
  EQR2latLon(lr_x, lr_y, &lat_LR, &lon_LR);
  
  double north = max4(lat_UL, lat_LL, lat_LR, lat_UR);
  double south = min4(lat_UL, lat_LL, lat_LR, lat_UR);
  double east = max4(lon_UL, lon_LL, lon_LR, lon_UR);
  double west = min4(lon_UL, lon_LL, lon_LR, lon_UR);

  // Determine 
  int ii;
  int band_count = meta->general->band_count;
  char **bands = extract_band_names(meta->general->bands, band_count);
  meta_free(meta);
  if (band_count == 1 || (rgb && strlen(rgb)))
    sprintf(pngFile, "%s.png", baseName);
  else if (band && strlen(band) > 0)
    sprintf(pngFile, "%s_%s.png", baseName, band);
  else
    sprintf(pngFile, "%s_%s.png", baseName, bands[0]);

  // Generate a configuration file for convert2vector
  asfPrintStatus("\n\nGenerating KML file ...\n\n");
  sprintf(configFileName, "convert2vector.config");
  fp = FOPEN(configFileName, "w");
  fprintf(fp, "[General]\n");
  fprintf(fp, "input file = %s\n", metaFile);
  fprintf(fp, "output file = %s%s\n", outDir, kmlFile);
  fprintf(fp, "input format = META\n");
  fprintf(fp, "output format = KML\n");
  fprintf(fp, "list = 0\n\n");
  fprintf(fp, "[KML]\n");
  fprintf(fp, "time = 0\n");
  fprintf(fp, "boundary = line\n");
  fprintf(fp, "height = clampToGround\n");
  fprintf(fp, "width = 2\n");
  fprintf(fp, "color = ffff9900\n");
  if (band && strlen(band) > 0 &&
      strcmp_case(band, "INTERFEROGRAM_PHASE") == 0 &&
      colormap && strlen(colormap) > 0)
    fprintf(fp, "overlay = %s_INTERFEROGRAM_RGB.png\n", baseName);
  else
    fprintf(fp, "overlay = %s\n", pngFile);
  fprintf(fp, "north = %.4lf\n", north);
  fprintf(fp, "south = %.4lf\n", south);
  fprintf(fp, "east = %.4lf\n", east);
  fprintf(fp, "west = %.4lf\n", west);
  fprintf(fp, "transparency = %d\n", transparency);
  FCLOSE(fp);

  // Run configuration file through convert2vector
  c2v_config *cfg = read_c2v_config(configFileName);
  convert2vector(cfg);
  FREE(cfg);
  asfPrintStatus("\nGenerated %s ...\n", kmlFile);
  asfPrintStatus("Generated %s ...\n", pngFile);

  // Will revisit the zipping later
  // Could not get it to work - will go with the uncompressed KML file for now

  // Zip the KML and PNG into a KMZ fill
  chdir(outDir);
  if (zip) {
    asfPrintStatus("\n\nGenerating KMZ file ...\n\n");
    zipFiles(kmzFile, kmlFile, pngFile);
  }

  // Clean up
  if (band_count == 1) {
    if (zip) {
      sprintf(pngFile, "%s%s.png", outDir, baseName);
      //remove_file(pngFile);
    }
    FREE(bands[0]);
  }
  else {
    if (rgb && strlen(rgb))
      asfPrintStatus("Generating %s ...\n", pngFile);
    for (ii=0; ii<band_count; ii++) {
      //if (zip) {
      sprintf(pngFile, "%s_%s.png", baseName, bands[ii]);
      if (band && strlen(band) && strcmp_case(band, bands[ii]) == 0)
	asfPrintStatus("Generating %s ...\n", pngFile);
      else
	remove_file(pngFile);
	//}
      FREE(bands[ii]);
    }
  }
  FREE(bands);
  FREE(outputFile);
  FREE(baseName);
  FREE(outDir);
  FREE(inName);
  FREE(inDir);
  remove_file(metaFile);

  chdir(cwd);
  remove_dir(tmpDir);
  FREE(tmpDir);

  statusflag = saved_statusflag;
  quietflag = saved_quietflag;
 
  return(EXIT_SUCCESS);
}
