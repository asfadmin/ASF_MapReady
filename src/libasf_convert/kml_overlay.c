#include <asf_contact.h>
#include <asf_license.h>

#include <asf_convert.h>
#include <asf_vector.h>
#include <sys/types.h> /* 'DIR' structure (for opendir) */
#include <dirent.h>    /* for opendir itself            */

#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "asf_raster.h"
#include "proj.h"
#include "asf_contact.h"
#include <unistd.h>

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

int kml_overlay(char *inFile, char *outFile, char *demFile, 
		int terrain_correct, int refine_geolocation, int zip)
{
  // Check whether required files actually exist
  ceos_file_pairs_t pair;
  char **dataName=NULL, **metaName=NULL, base[512];
  int nBands, trailer;
  pair = get_ceos_names(inFile, base, &dataName, &metaName, &nBands, &trailer);
  if (demFile && !fileExists(demFile)) {
    asfPrintWarning("DEM does not exist. Will leave out terrain correction!\n");
    terrain_correct = FALSE;
    refine_geolocation = FALSE;
  }

  // Create temporary processing directory
  char cwd[1024];
  getcwd(cwd, 1024);
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);
  sprintf(tmpDir, "%s-", outFile);
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);

  // Determine size of output
  meta_parameters *meta;
  meta = meta_read(inFile);
  double pixel_size = meta->general->x_pixel_size;
  if (meta->general->line_count > 1024)
    pixel_size *= 8.0;
  meta_free(meta);

  // Generate input names
  char *inName = (char *) MALLOC(sizeof(char)*(strlen(inFile)+1));
  char *inDir = (char *) MALLOC(sizeof(char)*1024);
  split_dir_and_file(inFile, inDir, inName);
  if (strlen(inDir) == 0)
    sprintf(inDir, "%s%c", cwd, DIR_SEPARATOR);

  // Generate output names
  char *baseName = (char *) MALLOC(sizeof(char)*(strlen(outFile)+1));
  char *outDir = (char *) MALLOC(sizeof(char)*1024);
  split_dir_and_file(outFile, outDir, baseName);
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
  if (pair != NO_CEOS_FILE_PAIR)
    fprintf(fp, "import = 1\n");
  else
    fprintf(fp, "import = 0\n");
  if (terrain_correct || refine_geolocation)
    fprintf(fp, "terrain correction = 1\n");
  else
    fprintf(fp, "terrain correction = 0\n");
  fprintf(fp, "geocoding = 1\n");
  fprintf(fp, "export =1\n");
  fprintf(fp, "dump envi header = 0\n");
  fprintf(fp, "short configuration file = 1\n\n");
  if (pair != NO_CEOS_FILE_PAIR) {
    fprintf(fp, "[Import]\n");
    fprintf(fp, "format = CEOS\n");
    fprintf(fp, "radiometry = AMPLITUDE_IMAGE\n\n");
  }
  if (terrain_correct || refine_geolocation) {
    fprintf(fp, "[Terrain correction]\n");
    fprintf(fp, "digital elevation model = %s\n", demFile);
    fprintf(fp, "pixel spacing = 30\n");
    fprintf(fp, "auto mask water = 1\n");
    fprintf(fp, "water height cutoff = 1.0\n");
    fprintf(fp, "smooth dem holes = 1\n");
    fprintf(fp, "interpolate = 1\n");
    if (refine_geolocation)
      fprintf(fp, "refine geolocation only = 1\n\n");
    else
      fprintf(fp, "refine geolocation only = 0\n\n");
  }
  fprintf(fp, "[Geocoding]\n");
  fprintf(fp, "projection = %s/projections/equi_rectangular/"
	  "equi_rectangular_world.proj\n", get_asf_share_dir());
  fprintf(fp, "pixel spacing = %.2lf\n", pixel_size);
  fprintf(fp, "force = 1\n\n");
  fprintf(fp, "[Export]\n");
  fprintf(fp, "format = PNG_GE\n");
  fprintf(fp, "byte conversion = SIGMA\n");
  FCLOSE(fp);

  // Run input file through asf_mapready
  asfPrintStatus("\n\nGenerating overlay PNG file ...\n\n");
  asf_convert(FALSE, configFileName);

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
  if (band_count == 1)
    sprintf(pngFile, "%s.png", baseName);
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
  fprintf(fp, "overlay = %s\n", pngFile);
  fprintf(fp, "north = %.4lf\n", north);
  fprintf(fp, "south = %.4lf\n", south);
  fprintf(fp, "east = %.4lf\n", east);
  fprintf(fp, "west = %.4lf\n", west);
  fprintf(fp, "transparency = 50\n");
  FCLOSE(fp);

  // Run configuration file through convert2vector
  c2v_config *cfg = read_c2v_config(configFileName);
  convert2vector(cfg);
  FREE(cfg);

  // Zip the KML and PNG into a KMZ fill
  chdir(outDir);
  if (zip) {
    char cmd[512];
    asfPrintStatus("\n\nGenerating KMZ file ...\n\n");
    sprintf(cmd, "zip %s %s %s", kmzFile, kmlFile, pngFile);
    asfSystem(cmd);
  }

  // Clean up
  remove_file(kmlFile);
  if (band_count == 1) {
    if (zip) {
      sprintf(pngFile, "%s%s.png", outDir, baseName);
      remove_file(pngFile);
    }
    FREE(bands[0]);
  }
  else {
    for (ii=0; ii<band_count; ii++) {
      if (zip) {
	sprintf(pngFile, "%s, %s_%s.png", outDir, baseName, bands[ii]);
	remove_file(pngFile);
      }
      FREE(bands[ii]);
    }
  }
  chdir(cwd);
  FREE(bands);
  remove_file(metaFile);
  remove_dir(tmpDir);
  FREE(tmpDir);
  FREE(baseName);
  FREE(outDir);
  FREE(inName);
  FREE(inDir);
 
  asfPrintStatus("\nSuccessful completion!\n\n");

  return(EXIT_SUCCESS);
}
