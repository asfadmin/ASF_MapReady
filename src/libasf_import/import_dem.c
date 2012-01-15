#include "dem_meta.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_import.h"
#include "asf_raster.h"
#include "asf_endian.h"
#include "libasf_proj.h"

dem_meta *dem_meta_init(void)
{
  dem_meta *dem = (dem_meta *) CALLOC(1, sizeof(dem_meta));
 
  strcpy(dem->dataset, MAGIC_UNSET_STRING);
  strcpy(dem->type, MAGIC_UNSET_STRING);
  dem->format = MAGIC_UNSET_INT;
  dem->data_type = MAGIC_UNSET_INT;
  dem->row_count = MAGIC_UNSET_INT;
  dem->column_count = MAGIC_UNSET_INT;
  dem->band_count = MAGIC_UNSET_INT;
  dem->min_value = MAGIC_UNSET_DOUBLE;
  dem->max_value = MAGIC_UNSET_DOUBLE;
  dem->mean_value = MAGIC_UNSET_DOUBLE;
  dem->standard_deviation = MAGIC_UNSET_DOUBLE;
  dem->no_data = MAGIC_UNSET_DOUBLE;
  strcpy(dem->tiles, MAGIC_UNSET_STRING);
  strcpy(dem->unit_type, MAGIC_UNSET_STRING);

  return dem;
}

meta_parameters *dem2meta(dem_meta *dem)
{
  meta_parameters *meta = raw_init();

  // general block
  strcpy(meta->general->basename, dem->dataset);
  meta->general->data_type = REAL32;
  meta->general->image_data_type = DEM;
  meta->general->radiometry = r_AMP;
  meta->general->band_count = dem->band_count;
  strcpy(meta->general->bands, "DEM");
  meta->general->line_count = dem->row_count;
  meta->general->sample_count = dem->column_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->line_scaling = 1;
  meta->general->sample_scaling = 1;
  //general->center_latitude = MAGIC_UNSET_DOUBLE;
  //general->center_longitude = MAGIC_UNSET_DOUBLE;
  //general->re_major = MAGIC_UNSET_DOUBLE;
  //general->re_minor = MAGIC_UNSET_DOUBLE;
  meta->general->no_data = dem->no_data;

  // DEM block
  meta->dem = meta_dem_init();
  strcpy(meta->dem->source, dem->type);
  if (dem->format == DEM_GEOTIFF)
    strcpy(meta->dem->format, "GeoTIFF");
  else if (dem->format == DEM_BIL_GRIDFLOAT)
    strcpy(meta->dem->format, "BIL/GRIDFLOAT");
  else if (dem->format == DEM_ARCGRID)
    strcpy(meta->dem->format, "ArcGrid");
  else if (dem->format == DEM_HGT)
    strcpy(meta->dem->format, "HGT");
  else if (dem->format == DEM_DTED)
    strcpy(meta->dem->format, "DTED");
  else if (dem->format == DEM_DEM)
    strcpy(meta->dem->format, "USGS DEM");
  meta->dem->min_value = dem->min_value;
  meta->dem->max_value = dem->max_value;
  meta->dem->mean_value = dem->mean_value;
  meta->dem->standard_deviation = dem->standard_deviation;
  strcpy(meta->dem->unit_type, dem->unit_type);
  meta->dem->no_data = dem->no_data;

  meta->location = meta_location_init();

  return meta;
}

static void extract_file(unzFile *file, char *szFileName, const char *path)
{
  int bytes;
  if (findExt(szFileName)) {
    cpl_unzOpenCurrentFile(file);
    char *buf = (char *) MALLOC(sizeof(char)*8192);
    char *fileName = 
      (char *) MALLOC(sizeof(char)*(strlen(path)+strlen(szFileName)+3));
    sprintf(fileName, "%s%c%s", path, DIR_SEPARATOR, szFileName);
    FILE *fp = FOPEN(fileName, "wb");
    while ((bytes = cpl_unzReadCurrentFile(file, buf, 8192)) > 0)
      FWRITE(buf, bytes, 1, fp);
    FCLOSE(fp);
    FREE(buf);
    FREE(fileName);
    cpl_unzCloseCurrentFile(file);
  }
  else { // has to be a directory 
    char *dir = 
      (char *) MALLOC(sizeof(char)*(strlen(path)+strlen(szFileName)+3));
    sprintf(dir, "%s%c%s", path, DIR_SEPARATOR, szFileName);
    create_clean_dir(dir);
    FREE(dir);
  }
}

static int check_aster_lat_lon(char *szFileName, int *lat, int *lon)
{
  int sign;

  // Check for ASTER naming scheme - ASTGTM_N36W122
  if (strstr(szFileName, "ASTGTM_")) {
    char tiles[10], *p;
    sprintf(tiles, "%s", szFileName+7);
    if (tiles[0] == 'N')
      sign = 1;
    else
      sign = -1;
    sscanf(tiles+1, "%d", lat);
    *lat *= sign;
    p = strchr(tiles, 'W');
    if (p)
      sign = -1;
    else {
      p = strchr(tiles, 'E');
      sign = 1;
    }
    sscanf(p+1, "%d", lon);
    *lon *= sign;
    return TRUE;
  }
  else
    return FALSE;
}

static char *extract_aster_dem(char *szFileName, const char *tmp_dir)
{
  unzFile *aster_file;
  char aster_dem[25];
  char *aster_dem_file = (char *) MALLOC(sizeof(char)*100);

  sprintf(aster_dem_file, "%s%c%s", tmp_dir, DIR_SEPARATOR, szFileName);
  aster_file = cpl_unzOpen(aster_dem_file);
  strcpy(aster_dem, stripExt(szFileName));
  aster_dem_file = (char *) MALLOC(sizeof(char)*50);
  sprintf(aster_dem_file, 
  	  "%s%c%s_dem.tif", aster_dem, DIR_SEPARATOR, aster_dem);
  cpl_unzLocateFile(aster_file, aster_dem_file, 2);
  sprintf(aster_dem_file, "%s_dem.tif", aster_dem);
  asfPrintStatus("Extracting %s ...\n", aster_dem_file);
  extract_file(aster_file, aster_dem_file, tmp_dir);
  cpl_unzClose(aster_file);
  sprintf(aster_dem_file, "%s%c%s_dem.tif", tmp_dir, DIR_SEPARATOR, aster_dem);

  return (aster_dem_file);
}

static char **read_aster_dem(unzFile *file, int file_count, 
			     const char *tmp_dir, char *tiles)
{
  int ii, lat_min=90, lat_max=-90, lon_min=180, lon_max=-180, lat, lon;
  char tmp[10];
  unz_file_info pfile_info;
  char szFileName[1024], szComment[1024], extraField[1024];
  uLong fileNameBufferSize, extraFieldBufferSize, commentBufferSize;

  char **dem_files = (char **) MALLOC(sizeof(char *)*file_count);
  for (ii=0; ii<file_count; ii++)
    dem_files[ii] = (char *) MALLOC(sizeof(char)*512);

  // ASTER DEM comes as zip files for the individual tiles
  // The overall zip file is called 'Tiles_<time stamp>'
  // The individual ASTER naming scheme: ASTGTM_N36W122
  cpl_unzGoToFirstFile(file);
  cpl_unzGetCurrentFileInfo(file, &pfile_info, szFileName, 
			    fileNameBufferSize, extraField,
			    extraFieldBufferSize, szComment, 
			    commentBufferSize);
  if (check_aster_lat_lon(szFileName, &lat, &lon)) {
    if (lat < lat_min)
      lat_min = lat;
    if (lat > lat_max)
      lat_max = lat;
    if (lon < lon_min)
      lon_min = lon;
    if (lon > lon_max)
      lon_max = lon;
  }
  extract_file(file, szFileName, tmp_dir);
  if (strncmp_case(szFileName, "ASTGTM", 6) == 0 &&
      strcmp_case(findExt(szFileName), ".zip") == 0)
    dem_files[0] = extract_aster_dem(szFileName, tmp_dir);
  for (ii=1; ii<file_count; ii++) {
    cpl_unzGoToNextFile(file);
    cpl_unzGetCurrentFileInfo(file, &pfile_info, szFileName, 
			      fileNameBufferSize, extraField,
			      extraFieldBufferSize, szComment, 
			      commentBufferSize);
    if (check_aster_lat_lon(szFileName, &lat, &lon)) {
      if (lat < lat_min)
	lat_min = lat;
      if (lat > lat_max)
	lat_max = lat;
      if (lon < lon_min)
	lon_min = lon;
      if (lon > lon_max)
	lon_max = lon;
    }
    extract_file(file, szFileName, tmp_dir);
    if (strncmp_case(szFileName, "ASTGTM", 6) == 0 &&
	strcmp_case(findExt(szFileName), ".zip") == 0)
      dem_files[ii] = extract_aster_dem(szFileName, tmp_dir);
  }
  if (lat_min >= 0)
    sprintf(tiles, "N%d", lat_min);
  else
    sprintf(tiles, "S%d", abs(lat_max));
  if (lat_max >= 0)
    sprintf(tmp, "N%d", lat_max);
  else
    sprintf(tmp, "S%d", abs(lat_min));
  strcat(tiles, tmp);
  if (lon_min >= 0)
    sprintf(tmp, "_E%d", lon_min);
  else
    sprintf(tmp, "_W%d", abs(lon_max));
  strcat(tiles, tmp);
  if (lon_max >= 0)
      sprintf(tmp, "E%d", lon_max);
  else
    sprintf(tmp, "W%d", abs(lon_min));
  strcat(tiles, tmp);

  return (dem_files);
}

static int check_jpl_srtm_lat_lon(char *szFileName, int *lat, int *lon)
{
  int sign;

  // Check for JPL SRTM naming scheme - S10W068.hgt
  if ((szFileName[0] == 'N' || szFileName[0] == 'S') &&
      (szFileName[3] == 'W' || szFileName[3] == 'E')) {  
    char tiles[10];
    sprintf(tiles, "%s", szFileName);
    if (tiles[0] == 'N')
      sign = 1;
    else if (tiles[0] == 'S')
      sign = -1;
    sscanf(tiles+1, "%d", lat);
    *lat *= sign;
    sprintf(tiles, "%s", szFileName+3);
    if (tiles[0] == 'W')
      sign = -1;
    else if (tiles[0] == 'E')
      sign = 1;
    sscanf(tiles+1, "%d", lon);
    *lon *= sign;
    return TRUE;
  }
  else
    return FALSE;
}

static char **read_jpl_srtm_dem(const char *infile, const char *tmp_dir, 
				int *file_count, char *tiles)
{
  int ii, lat_min=90, lat_max=-90, lon_min=180, lon_max=-180, lat, lon;
  char tmp[10];
  unz_file_info pfile_info;
  char szFileName[1024], szComment[1024], extraField[1024];
  uLong fileNameBufferSize, extraFieldBufferSize, commentBufferSize;

  char *line = (char *) MALLOC(sizeof(char)*512);
  int count = 0;
  FILE *fpList = FOPEN(infile, "r");
  while (fgets(line, 512, fpList)) {
    if (strlen(line) > 0)
      count++;
  }
  FCLOSE(fpList);
  char **dem_files = (char **) MALLOC(sizeof(char *)*count);
  fpList = FOPEN(infile, "r");
  for (ii=0; ii<count; ii++) {
    fgets(line, 512, fpList);
    chomp(line);

    // SRTM DEM (JPL style) comes as zip file (e.g. S10W068.hgt.zip)
    unzFile *file = cpl_unzOpen(line);
    cpl_unzGoToFirstFile(file);
    //cpl_unzGoToNextFile(file);
    cpl_unzGetCurrentFileInfo(file, &pfile_info, szFileName, 
			      fileNameBufferSize, extraField,
			      extraFieldBufferSize, szComment, 
			      commentBufferSize);
    extract_file(file, szFileName, tmp_dir);
    cpl_unzClose(file);

    dem_files[ii] = (char *) MALLOC(sizeof(char)*512);
    //strcpy(dem_files[ii], szFileName);
    sprintf(dem_files[ii], "%s%c%s", tmp_dir, DIR_SEPARATOR, szFileName);

    if (check_jpl_srtm_lat_lon(szFileName, &lat, &lon)) {
      if (lat < lat_min)
	lat_min = lat;
      if (lat > lat_max)
	lat_max = lat;
      if (lon < lon_min)
	lon_min = lon;
      if (lon > lon_max)
	lon_max = lon;
    }
    if (lat_min >= 0)
      sprintf(tiles, "N%d", lat_min);
    else
      sprintf(tiles, "S%d", abs(lat_max));
    if (lat_max >= 0)
      sprintf(tmp, "N%d", lat_max);
    else
      sprintf(tmp, "S%d", abs(lat_min));
    strcat(tiles, tmp);
    if (lon_min >= 0)
      sprintf(tmp, "_E%d", lon_min);
    else
      sprintf(tmp, "_W%d", abs(lon_max));
    strcat(tiles, tmp);
    if (lon_max >= 0)
      sprintf(tmp, "E%d", lon_max);
    else
      sprintf(tmp, "W%d", abs(lon_min));
    strcat(tiles, tmp);
  }
  FCLOSE(fpList);
  FREE(line);  
  *file_count = count;

  return (dem_files);
}

static int check_usgs_ned_lat_lon(char *szFileName, int *lat, int *lon)
{
  int sign;

  // Check for USGS NED tiles naming scheme - n64W144
  if ((szFileName[0] == 'N' || szFileName[0] == 'S') &&
      (szFileName[3] == 'W' || szFileName[3] == 'E')) {  
    char tiles[10];
    sprintf(tiles, "%s", szFileName);
    if (tiles[0] == 'N')
      sign = 1;
    else if (tiles[0] == 'S')
      sign = -1;
    sscanf(tiles+1, "%d", lat);
    *lat *= sign;
    sprintf(tiles, "%s", szFileName+3);
    if (tiles[0] == 'W')
      sign = -1;
    else if (tiles[0] == 'E')
      sign = 1;
    sscanf(tiles+1, "%d", lon);
    *lon *= sign;
    return TRUE;
  }
  else
    return FALSE;
}

static char **read_usgs_ned_dem(unzFile *file, int file_count, 
				const char *tmp_dir, char *tiles)
{
  int ii, lat_min=90, lat_max=-90, lon_min=180, lon_max=-180, lat, lon;
  char tmp[10];
  unz_file_info pfile_info;
  char szFileName[1024], szComment[1024], extraField[1024];
  uLong fileNameBufferSize, extraFieldBufferSize, commentBufferSize;

  char **dem_files = (char **) MALLOC(sizeof(char *)*file_count);
  for (ii=0; ii<file_count; ii++)
    dem_files[ii] = (char *) MALLOC(sizeof(char)*512);

  // NED DEM come as zip files for the individual tiles
  // The individual tile directory naming scheme: n36w122
  cpl_unzGoToFirstFile(file);
  cpl_unzGetCurrentFileInfo(file, &pfile_info, szFileName, 
			    fileNameBufferSize, extraField,
			    extraFieldBufferSize, szComment, 
			    commentBufferSize);
  if (check_usgs_ned_lat_lon(szFileName, &lat, &lon)) {
    if (lat < lat_min)
      lat_min = lat;
    if (lat > lat_max)
      lat_max = lat;
    if (lon < lon_min)
      lon_min = lon;
    if (lon > lon_max)
      lon_max = lon;
  }
  extract_file(file, szFileName, tmp_dir);

  /*
    unzFile *file = cpl_unzOpen(inBaseName);
    if (file) {
      unz_global_info pglobal_info;
      asfPrintStatus("Zip file: %s\n", inBaseName);
      cpl_unzGetGlobalInfo(file, &pglobal_info);
      file_count = pglobal_info.number_entry;
      dem_files = read_aster_dem(file, file_count, tmp_dir, tiles);
      cpl_unzClose(file);
      zip = TRUE;
    }
  */

  if (strncmp_case(szFileName, "ASTGTM", 6) == 0 &&
      strcmp_case(findExt(szFileName), ".zip") == 0)
    dem_files[0] = extract_aster_dem(szFileName, tmp_dir);
  for (ii=1; ii<file_count; ii++) {
    cpl_unzGoToNextFile(file);
    cpl_unzGetCurrentFileInfo(file, &pfile_info, szFileName, 
			      fileNameBufferSize, extraField,
			      extraFieldBufferSize, szComment, 
			      commentBufferSize);
    if (check_aster_lat_lon(szFileName, &lat, &lon)) {
      if (lat < lat_min)
	lat_min = lat;
      if (lat > lat_max)
	lat_max = lat;
      if (lon < lon_min)
	lon_min = lon;
      if (lon > lon_max)
	lon_max = lon;
    }
    extract_file(file, szFileName, tmp_dir);
    if (strncmp_case(szFileName, "ASTGTM", 6) == 0 &&
	strcmp_case(findExt(szFileName), ".zip") == 0)
      dem_files[ii] = extract_aster_dem(szFileName, tmp_dir);
  }
  if (lat_min >= 0)
    sprintf(tiles, "N%d", lat_min);
  else
    sprintf(tiles, "S%d", abs(lat_max));
  if (lat_max >= 0)
    sprintf(tmp, "N%d", lat_max);
  else
    sprintf(tmp, "S%d", abs(lat_min));
  strcat(tiles, tmp);
  if (lon_min >= 0)
    sprintf(tmp, "_E%d", lon_min);
  else
    sprintf(tmp, "_W%d", abs(lon_max));
  strcat(tiles, tmp);
  if (lon_max >= 0)
      sprintf(tmp, "E%d", lon_max);
  else
    sprintf(tmp, "W%d", abs(lon_min));
  strcat(tiles, tmp);

  return (dem_files);
}

void import_dem(const char *inBaseName, int list, const char *outBaseName,
		const char *dem_type, const char *tmp_dir,
		char ***pImportFiles, int *nFiles)
{
  int nn, file_count, zip = FALSE, jpl = FALSE;
  char **dem_files, **import_files, tiles[25]="";

  // DEM type
  printf("  DEM type: %s\n", uc(dem_type));

  // Check whether input file is zipped
  // We currently cover the following cases: ASTER, SRTM JPL
  if (list) {
    char *line = (char *) MALLOC(sizeof(char)*512);
    FILE *fpList = FOPEN(inBaseName, "r");
    while (fgets(line, 512, fpList))
      if (strlen(line) > 0 && strstr(line, ".hgt.zip"))
	jpl = TRUE;
    FCLOSE(fpList);
    if (strcmp_case(dem_type, "SRTM") == 0 && jpl)
      dem_files = read_jpl_srtm_dem(inBaseName, tmp_dir, &file_count, tiles);
    else if (strcmp_case(dem_type, "NED") == 0)
      dem_files = read_usgs_ned_dem(inBaseName, tmp_dir, &file_count, tiles);
    zip = TRUE;
  }
  else if (findExt(inBaseName) && 
	   strcmp_case(findExt(inBaseName), ".ZIP") == 0) {
    unzFile *file = cpl_unzOpen(inBaseName);
    if (file) {
      unz_global_info pglobal_info;
      asfPrintStatus("Zip file: %s\n", inBaseName);
      cpl_unzGetGlobalInfo(file, &pglobal_info);
      file_count = pglobal_info.number_entry;
      dem_files = read_aster_dem(file, file_count, tmp_dir, tiles);
      cpl_unzClose(file);
      zip = TRUE;
    }
  }
  else {
    file_count = 1;
    dem_files = (char **) MALLOC(sizeof(char *));
    dem_files[0] = (char *) MALLOC(sizeof(char)*512);
    strcpy(dem_files[0], inBaseName);
  }

  // Assign file names
  import_files = (char **) MALLOC(sizeof(char *)*file_count+1);
  for (nn=0; nn<file_count; nn++) {
    import_files[nn] = (char *) MALLOC(sizeof(char)*512);
    if (zip)
      sprintf(import_files[nn], "%s%c%s_import.img", tmp_dir, DIR_SEPARATOR,
	      get_basename(dem_files[nn]));
    else if (!findExt(outBaseName) || 
	     strcmp_case(findExt(outBaseName), ".IMG") != 0) 
      sprintf(import_files[nn], "%s%c%s.img", 
	      tmp_dir, DIR_SEPARATOR, get_basename(outBaseName));
    else
      strcpy(import_files[nn], outBaseName);
  }
  import_files[file_count] = NULL;
  
  FILE *fp;
  int ii, dataType, no_data;
  float *floatline;
  char driver_name[25];
  GDALDatasetH hGdal;
  GDALRasterBandH hBand;
  GDALDriverH hDriver;
  meta_parameters *meta;
  dem_meta *dem;

  GDALAllRegister();

  for (nn=0; nn<file_count; nn++) {
    
    // Extract metadata
    dem = dem_meta_init();
    
    if (!fileExists(dem_files[nn]))
      asfPrintError("File (%s) does not exist!\n", dem_files[nn]);

    // Open file
    hGdal = GDALOpen(dem_files[nn], GA_ReadOnly);
    if (!hGdal)
      asfPrintError("Failed to open file (%s)\n", dem_files[nn]);
    
    strcpy(dem->type, uc(dem_type));
    
    // Determine what format we are dealing with
    hDriver = GDALGetDatasetDriver(hGdal);
    strcpy(driver_name, GDALGetDriverShortName(hDriver));
    if (strcmp_case(driver_name, "GTiff") == 0)
      dem->format = DEM_GEOTIFF;
    else if (strcmp_case(driver_name, "EHdr") == 0) {
      dem->format = DEM_BIL_GRIDFLOAT;
      // Make sure that projection file exists
      char *projFile = (char *) MALLOC(sizeof(char)*512);
      sprintf(projFile, "%s.prj", get_basename(dem_files[nn]));
      if (!fileExists(projFile)) {
	char *testFile = (char *) MALLOC(sizeof(char)*512);
	if (projFile[0] >= 97 && projFile[0] <= 122)
	  sprintf(testFile, "%s.prj", uc(get_basename(dem_files[nn])));
	else
	  sprintf(testFile, "%s.prj", lc(get_basename(dem_files[nn])));
	if (fileExists(testFile)) {
	  fileCopy(testFile, projFile);
	  asfPrintError("Fixed projection file (%s). Run again.\n", testFile);
	}
      }
      FREE(projFile);
    }
    else if (strcmp_case(driver_name, "AIG") == 0)
      dem->format = DEM_ARCGRID;
    else if (strcmp_case(driver_name, "SRTMHGT") == 0)
      dem->format = DEM_HGT;
    else if (strcmp_case(driver_name, "DTED") == 0)
      dem->format = DEM_DTED;
    else if (strcmp_case(driver_name, "USGSDEM") == 0)
      dem->format = DEM_DEM;
    
    // Determine data type
    hBand = GDALGetRasterBand(hGdal, 1);
    dataType = GDALGetRasterDataType(hBand);
    
    // Calculate DEM statistics
    GDALGetRasterStatistics(hBand, FALSE, TRUE, &dem->min_value, &dem->max_value,
			    &dem->mean_value, &dem->standard_deviation);
    
    // Check for a no data value
    dem->no_data = GDALGetRasterNoDataValue(hBand, &no_data);
    if (!no_data)
      dem->no_data = MAGIC_UNSET_DOUBLE;

    // Apparently GDAL does not know anything about complex_byte
    if (dataType == GDT_Byte)
      dem->data_type = BYTE;
    else if (dataType == GDT_UInt16 || dataType == GDT_Int16)
      dem->data_type = INTEGER16;
    else if (dataType == GDT_UInt32 || dataType == GDT_Int32)
      dem->data_type = INTEGER32;
    else if (dataType == GDT_Float32)
      dem->data_type = REAL32;
    else if (dataType == GDT_Float64)
      dem->data_type = REAL64;
    else // assume DEM in INT16
      dem->data_type = INTEGER16;
    
    // Check image dimensions
    dem->column_count = GDALGetRasterXSize(hGdal);
    dem->row_count = GDALGetRasterYSize(hGdal);
    dem->band_count = GDALGetRasterCount(hGdal);
    if (strlen(GDALGetRasterUnitType(hBand)) > 0)
      sprintf(dem->unit_type, "%s", GDALGetRasterUnitType(hBand));
    else // assume meters
      sprintf(dem->unit_type, "m");
        
    if (strlen(tiles) > 0)
      strcpy(dem->tiles, tiles);
    strcpy(dem->dataset, dem_files[nn]);
    meta = dem2meta(dem);
    strcpy(meta->dem->tiles, tiles);
    meta->projection = 
      gdal2meta_projection(hGdal, dem->row_count, dem->column_count);
    double pixel_size = meta->projection->perX*D2R*meta->projection->re_minor;
    meta->general->x_pixel_size = (int) pixel_size;
    meta->general->y_pixel_size = (int) pixel_size;
    if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION) {
      meta->general->center_latitude = 
	meta->projection->startY + 
	meta->projection->perY*meta->general->line_count/2;
      meta->general->center_longitude = 
	meta->projection->startX + 
	meta->projection->perX*meta->general->sample_count/2;
    }
    meta_write(meta, import_files[nn]);
    
    fp = FOPEN(import_files[nn], "wb");

    floatline = (float *) MALLOC(sizeof(float)*dem->column_count);
    for (ii=0; ii<dem->row_count; ii++) {
      GDALRasterIO(hBand, GF_Read, 0, ii, dem->column_count, 1, floatline, 
		   dem->column_count, 1, GDT_Float32, 0, 0);
      put_float_line(fp, meta, ii, floatline);
      asfLineMeter(ii, dem->row_count);
    }

    FCLOSE(fp);
    GDALClose(hGdal);
    meta_free(meta);
  }
  FREE(dem);
  GDALDestroyDriverManager();
  *pImportFiles = import_files;
  *nFiles = file_count;
}
