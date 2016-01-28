#include <stdio.h>
#include <ctype.h>

#include "asf_tiff.h"
#include "geo_tiffp.h"
#include "geo_keyp.h"
#include "asf.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "asf_vector.h"
#include "geotiff_support.h"
#include "get_ceos_names.h"
#include "terrasar.h"
#include "xml_util.h"

#define FILENAME_LINE_MAX   (1024)
#define FILENAME_LEN        (256)
#define LINE_MAX_LEN        (1024)

int isleader(char *inFile)
{
    int isLeader = 0;
    int trailer;
    ceos_metadata_ext_t ceos_meta_type;
    char **metaName=NULL;

    ceos_meta_type = get_ceos_metadata_name(inFile, &metaName, &trailer);
    if (ceos_meta_type != NO_CEOS_METADATA) {
        isLeader = 1;
    }

    return isLeader;
}

int isshape(char *inFile)
{
    char *ext = findExt(inFile);
    if (ext && strcmp_case(ext,".shp")!=0) {
        return FALSE;
    }

    char *dbaseFile, *basename;
    int isShape = 0;
    int nEntities, pointType;
    DBFHandle dbase;
    SHPHandle shape;

    dbaseFile = (char *)MALLOC(sizeof(char)*(strlen(inFile)+5));
    basename = get_basename(inFile);
    sprintf(dbaseFile, "%s.dbf", basename);
    dbase = DBFOpen(dbaseFile, "r+b");
    shape = SHPOpen(inFile, "r+b");
    if (dbase != NULL && shape != NULL) {
        SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
        if (nEntities >= 1 &&
            (pointType == SHPT_POLYGON   ||
             pointType == SHPT_POINT     ||
             pointType == SHPT_ARC       ||
             pointType == SHPT_MULTIPOINT )
        )
        {
            isShape = 1;
        }
    }
    if (shape) SHPClose(shape);
    if (dbase) DBFClose(dbase);
    FREE(basename);
    FREE(dbaseFile);

    return isShape;
}

int isgeotiff(char *inFile)
{
    FILE *fp;
    TIFF *tiff=NULL;
    GTIF *gtif=NULL;
    int isGeotiff = 0;
    char magic[2];

    fp = fopen(inFile, "r");
    if (!fp) {
        char n[1024];
        sprintf(n, "%s.tif", inFile);
        fp = fopen(n, "r");
    }
    if (!fp) {
        char *basename = get_basename(inFile);
        char n[1024];
        sprintf(n, "%s.tif", basename);
        fp = fopen(n, "r");
        FREE(basename);
    }
    if (!fp) {
        char *basename = get_basename(inFile);
        char n[1024];
        sprintf(n, "%s.tiff", basename);
        fp = fopen(n, "r");
        FREE(basename);
    }
    if (fp) {
        magic[0] = fgetc(fp);
        magic[1] = fgetc(fp);
        if ((magic[0] == 'I' && magic[1] == 'I') ||
            (magic[0] == 'M' && magic[1] == 'M')  )
        {
            // Looks like a TIFF file, so let's see if it's a GeoTIFF file
            tiff = XTIFFOpen(inFile, "r");
            if (!tiff) isGeotiff = 0; // Not a TIFF (or GeoTIFF) file ...rare if the magic bytes were correct.
            if (tiff) gtif = GTIFNew(tiff);
            if (tiff && !gtif) isGeotiff = 0; // Usually GTIFNew() succeeds even if not a GeoTIFF ...

            if (tiff && gtif) {
                double *tie_point=NULL;
                double *pixel_scale=NULL;
                int tp_count=0, ps_count=0;
                (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_TIEPOINTS, &tp_count, &tie_point);
                (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_PIXELSCALE, &ps_count, &pixel_scale);

                if (tie_point && pixel_scale && tp_count == 6 && ps_count == 3) {
                    // Assume that if tie points and pixel scales exist that the file is a
                    // GeoTIFF ...it's still possible that read_generic_geotiff_metadata() will
                    // fail due to the file only being a georeferenced rather than a geocoded
                    // GeoTIFF, but that's a different problem...
                    isGeotiff = 1;
                }
                FREE(tie_point);
                FREE(pixel_scale);
            }

            if (gtif) GTIFFree(gtif);
            if (tiff) XTIFFClose(tiff);
        }
        fclose(fp);
    }

    return isGeotiff;
}

int isterrasar(char *inFile)
{
  int found = FALSE;
  char *ext = findExt(inFile);

  if (ext && strcmp_case(ext, ".xml") == 0) {
    char *satellite = (char *) MALLOC(sizeof(char)*25);
    xmlDoc *doc = xmlReadFile(inFile, NULL, 0);
    if (doc) {
      strcpy(satellite, xml_get_string_value(doc, 
        "level1Product.productInfo.missionInfo.mission"));
      if (satellite &&
    	  (strncmp_case(satellite, "TSX", 3) == 0 ||
	       strncmp_case(satellite, "TDX", 3) == 0))
        found = TRUE;
    }
    xmlFreeDoc(doc);
    xmlCleanupParser();
    if (satellite)
      FREE(satellite);
  }
  return found;
}

int read_header_config(const char *format, dbf_header_t **dbf, 
	int *nAttr, char *shape_type)
{
  if (!format)
    return FALSE;
  strcpy(shape_type, "UNKNOWN");

  char header_file[1024];
  sprintf(header_file, "%s%cdata_dictionaries%c%s", 
	  get_asf_share_dir(), DIR_SEPARATOR, DIR_SEPARATOR, "header.lst");
  
  FILE *fp;
  char line[1024], params[255], format_str[255], dictionary[255], *str=NULL;
  int found_format=FALSE;
  sprintf(format_str, "[%s]", uc(format));

  // Check how many parameters we have in the section
  fp = FOPEN(header_file, "r");
  while (fgets(line, 255, fp)) {
    if (strncmp_case(line, format_str, strlen(format_str)-1) == 0)
      strcpy(params, format);
    if (strcmp_case(params, format) == 0) {
      found_format = TRUE;
      str = strstr(line, "=");
      if (strncmp_case(line, "type =", 6) == 0 && str) 
	      sprintf(shape_type, "%s", trim_spaces(str+1));
      str = strstr(line, "=");
      if (strncmp_case(line, "dictionary =", 12) == 0 && str)
        sprintf(dictionary, "%s", trim_spaces(str+1));
      if (line[0] == '[' && 
        strncmp(line, format_str, strlen(format_str)-1) != 0) {
	      break;
      }
    }
  }
  FCLOSE(fp);

  // Return if we can't find the format we are looking for
  if (!found_format)
    return FALSE;

  // Fill the header information
  char dictionary_file[1024], type[25];
  sprintf(dictionary_file, "%s%cdata_dictionaries%c%s", 
    get_asf_share_dir(), DIR_SEPARATOR, DIR_SEPARATOR, dictionary);
  fp = FOPEN(dictionary_file, "r");
  fgets(line, 1024, fp);
  int n = 0;
  while (fgets(line, 1024, fp))
    n++;
  FCLOSE(fp);
  dbf_header_t *header = (dbf_header_t *) MALLOC(sizeof(dbf_header_t)*n);

  fp = FOPEN(dictionary_file, "r");
  fgets(line, 1024, fp);
  n = 0;
  int nCols;
  char **column;
  while (fgets(line, 1024, fp)) {
    chomp(line);
    split_into_array(line, ',', &nCols, &column);
    header[n].meta = STRDUP(column[0]);
    header[n].shape = STRDUP(column[1]);
    sprintf(type, "%s", column[2]);
    if (strncmp_case(type, "DOUBLE", 6) == 0)
      header[n].format = DBF_DOUBLE;
    else if (strncmp_case(type, "INTEGER", 7) == 0)
      header[n].format = DBF_INTEGER;
    else if (strncmp_case(type, "STRING", 6) == 0)
      header[n].format = DBF_STRING;
    else
      asfPrintError("Unknown data format (%s) for '%s'!\n", 
        type, header[n].meta);
    header[n].length = atoi(column[3]);
    header[n].decimals = atoi(column[4]);
    header[n].definition = STRDUP(column[5]); 
  	n++;
  }
  FCLOSE(fp);

  *dbf = header;
  *nAttr = n;

  return TRUE;
}

void split_polygon(double *lat, double *lon, int nCoords, 
  int *start, double *mLat, double *mLon, double tolerance)
{
  double projX1, projY1, projX2, projY2, mProjX, mProjY;
  double dateX1, dateY1, dateX2, dateY2, m, mDate, height, maxLat = 0.0;
  double minLon = 180.0, maxLon = -180.0;
  int nCols = nCoords + 5;
  int ii, nNegative = 0;
  for (ii=0; ii<nCoords-1; ii++) {
    if (lon[ii] < 0)
      nNegative++;
    if (lat[ii] > 0 && lat[ii] > maxLat)
      maxLat = lat[ii];
    else if (lat[ii] < 0 && lat[ii] < maxLat)
      maxLat = lat[ii];
    if (lon[ii] > maxLon)
      maxLon = lon[ii];
    if (lon[ii] < minLon)
      minLon = lon[ii];
  }
  int nNeg = start[0] = 0;
  int nPos = start[1] = nNegative + 3;
  for (ii=0; ii<nCoords-1; ii++) {
    // normal case
    if (lon[ii] > 0) {
      mLat[nPos] = lat[ii];
      mLon[nPos] = lon[ii];
      nPos++;
    }
    if (lon[ii] < 0) {
      mLat[nNeg] = lat[ii];
      mLon[nNeg] = lon[ii];
      nNeg++;
    }
    // check longitude range - stay with geographic coordinates
    if (maxLon > (180 - tolerance) && minLon < (-180.0 + tolerance)) {
      // crossing dateline
      if ((lon[ii] < 0 && lon[ii+1] > 0) || (lon[ii] > 0 && lon[ii+1] < 0)) {
        char projFile[255];
        datum_type_t datum = WGS84_DATUM;
        spheroid_type_t spheroid = WGS84_SPHEROID;
        project_parameters_t pps;
        projection_type_t proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;
        meta_projection *proj = meta_projection_init();
        if (fabs(maxLat) < 66.5) {
          pps.utm.zone = 60;
          pps.utm.scale_factor = 0.9996;
          pps.utm.lat0 = 0.0;
          pps.utm.lon0 = (double) (pps.utm.zone - 1) * 6.0 - 177.0;
          if (maxLat > 0.0)
            pps.utm.false_northing = 0.0;
          else
            pps.utm.false_northing = 10000000.0;
        }
        else {
          proj->type = POLAR_STEREOGRAPHIC;
          if (maxLat > 0) {
            strcpy(projFile, "polar_stereographic_north_ssmi.proj");
          }
          else
            strcpy(projFile, "polar_stereographic_south_ssmi.proj");
          read_proj_file(projFile, &pps, &proj_type, &datum, &spheroid);
        }      
        proj->type = proj_type;
        proj->datum = datum;
        proj->spheroid = spheroid;
        proj->param = pps;
        latlon_to_proj(proj, 'R', lat[ii]*D2R, 180.0*D2R, 0.0, 
          &dateX1, &dateY1, &height);
        latlon_to_proj(proj, 'R', lat[ii+1]*D2R, 180.0*D2R, 0.0, 
          &dateX2, &dateY2, &height);
        mDate = (dateY2 - dateY1)/(dateX2 - dateX1);
        latlon_to_proj(proj, 'R', lat[ii]*D2R, lon[ii]*D2R, 0.0, 
          &projX1, &projY1, &height);
        latlon_to_proj(proj, 'R', lat[ii+1]*D2R, lon[ii+1]*D2R, 0.0, 
          &projX2, &projY2, &height);
        m = (projY1 - projY2)/(projX1 - projX2);
        mProjX = (m*projX2 - mDate*dateX2 + dateY2 - projY2)/(m - mDate);
        mProjY = m*(mProjX - projX2) + projY2;
        proj_to_latlon(proj, mProjX, mProjY, 0.0, 
          &mLat[nPos], &mLon[nPos], &height);
        mLat[nPos] *= R2D;
        mLat[nNeg] = mLat[nPos];
        mLon[nPos] = 180.0;
        mLon[nNeg] = -180.0;
        nPos++;
        nNeg++;
      }
    }
  }
  mLat[nNegative+2] = mLat[0];
  mLon[nNegative+2] = mLon[0];
  mLat[nCols-1] = mLat[nNegative+3];
  mLon[nCols-1] = mLon[nNegative+3];
}
