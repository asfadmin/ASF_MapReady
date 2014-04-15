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

// Convert metadata to text
// NOTE: inFile will either be a leader data file, a geotiff,
// or an ASF metadata file
void meta2text(char *inFile, FILE *outFP)
{
    double ulLong=0.0, urLong=0.0, lrLong=0.0, llLong=0.0; // Clockwise polygon
    double ulLat=0.0, urLat=0.0, lrLat=0.0, llLat=0.0;
    int no_location_info=1;
    meta_parameters *meta = NULL;

    if (isgeotiff(inFile)) {
        int i, ignore[MAX_BANDS];
        for (i=0; i<MAX_BANDS; i++) ignore[i] = 0; // Default to ignoring no bands
        meta = read_generic_geotiff_metadata(inFile, ignore, NULL);
    }
    else if (isleader(inFile)) {
        meta = meta_create(inFile);
    }
    else if (ismetadata(inFile)) {
        meta = meta_read(inFile);
    }
    if (meta && meta->location) {
        meta_location *ml = meta->location; // Convenience pointer
        no_location_info = 0; // false ...location info was found
        ulLong = ml->lon_start_near_range;
        ulLat  = ml->lat_start_near_range;
        urLong = ml->lon_start_far_range;
        urLat  = ml->lat_start_far_range;
        lrLong = ml->lon_end_far_range;
        lrLat  = ml->lat_end_far_range;
        llLong = ml->lon_end_near_range;
        llLat  = ml->lat_end_near_range;
    }
    meta_free(meta);

    if (no_location_info)
      asfPrintWarning("No location coordinates found in %s\n", inFile);
    fprintf(outFP, "# File type        , polygon\n");
    // Use inFile for name ...for lack of a better idea
    fprintf(outFP, "# Polygon ID (name), %s\n", inFile);
    fprintf(outFP, "#\n");
    fprintf(outFP, "# Latitude, Longitude\n");
    if (no_location_info) {
      fprintf(outFP, "# WARNING: No location information found in "
              "source file (%s)\n", inFile);
      fprintf(outFP, "#          Values shown below are invalid\n");
    }
    fprintf(outFP, "%f, %f\n", ulLat, ulLong);
    fprintf(outFP, "%f, %f\n", urLat, urLong);
    fprintf(outFP, "%f, %f\n", lrLat, lrLong);
    fprintf(outFP, "%f, %f\n", llLat, llLong);
    fprintf(outFP, "\n");
    // FCLOSE() is called by the calling function

    return;
}

void geotiff2text(char *inFile, FILE *outFP)
{
    meta2text(inFile, outFP);

    return;
}

int ismetadata(char *inFile)
{
    int isMetadata=0;
    int foundGeneral=0;
    char *line=NULL, *s;
    FILE *fp = NULL;

    fp = fopen(inFile, "r");
    if (!fp) {
        char n[1024];
        sprintf(n, "%s.meta", inFile);
        fp = fopen(n, "r");
    }
    if (!fp) {
        char *basename = get_basename(inFile);
        char n[1024];
        sprintf(n, "%s.meta", basename);
        fp = fopen(n, "r");
        FREE(basename);
    }
    if (fp) {
        int line_count = 0;
        line = (char*)MALLOC(sizeof(char)*(1+LINE_MAX_LEN));
        while (fgets(line, LINE_MAX_LEN, fp)) {
            line[strlen(line)-1] = '\0';
            s=line;
            while(isspace((int)(*s))) ++s;
            if (s && strlen(s) && strncmp(uc(s), "GENERAL {", 9) == 0) {
                foundGeneral = 1;
            }
            if (foundGeneral && s && strlen(s) && strncmp_case(s, "NAME:", 5) == 0) {
                isMetadata = 1;
                break;
            }
            // avoid scanning the entire contents of a huge file
            if (++line_count>100)
              break;
        }
    }
    FREE(line);
    FCLOSE(fp);

    return isMetadata;
}

int isparfile(char *file)
{
  char *inFile = STRDUP(file);
  int isParfile=0;
  char *line=NULL, *s;
  FILE *fp = NULL;

  if (findExt(inFile) && (strcmp_case(findExt(inFile), ".PAR") != 0)) {
    strcat(inFile, ".par");
    if (!fileExists(inFile))
      return FALSE;
  }

  fp = fopen(inFile, "r");
  if (fp) {
    int line_count = 0;
    line = (char*)MALLOC(sizeof(char)*(1+LINE_MAX_LEN));
    while (fgets(line, LINE_MAX_LEN, fp)) {
      line[strlen(line)-1] = '\0';
      s=line;
      while(isspace((int)(*s))) ++s;
      if (s && strlen(s) && 
	  strncmp_case(s, "processor_name: SKY", 19) == 0) {
	isParfile = 1;
	break;
      }
      // avoid scanning the entire contents of a huge file
      if (++line_count>10000)
	break;
    }
  }
  FREE(line);
  FREE(inFile);
  FCLOSE(fp);
  
  return isParfile;
}

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

int ispoint(char *inFile)
{
    int isPoint=0;
    char *line=NULL, *s;
    FILE *fp;

    fp = fopen(inFile, "r");
    if (!fp) {
        char n[1024];
        sprintf(n, "%s.csv", inFile);
        fp = fopen(n, "r");
    }
    if (!fp) {
        char *basename = get_basename(inFile);
        char n[1024];
        sprintf(n, "%s.csv", basename);
        fp = fopen(n, "r");
        FREE(basename);
    }
    if (fp) {
        line = (char*)MALLOC(sizeof(char)*(1+LINE_MAX_LEN));
        int line_count=0;
        while (fgets(line, LINE_MAX_LEN, fp)) {
            line[strlen(line)-1] = '\0';
            s=line;
            while(isspace((int)(*s))) ++s;
            if (*s == '#') {
                char *tok = strtok(s,",");
                if (tok) {
                    s = strstr(uc(tok), "FILE");
                    if (s && strncmp(uc(s), "FILE", 4) == 0) {
                        tok = strtok(NULL, ",");
                        if (tok) {
                            s = strstr(uc(tok), "POINT");
                            if (s && strncmp(uc(s), "POINT", 5) == 0) {
                                isPoint = 1;
                                break;
                            }
                        }
                    }
                }
            }
            // avoid scanning the entire contents of a huge file
            if (++line_count>100)
              break;
        }
    }
    FREE(line);
    FCLOSE(fp);

    return isPoint;
}

int ispolygon(char *inFile)
{
    int isPolygon=0;
    char *line=NULL, *s;
    FILE *fp;

    fp = fopen(inFile, "r");
    if (!fp) {
        char n[1024];
        sprintf(n, "%s.csv", inFile);
        fp = fopen(n, "r");
    }
    if (!fp) {
        char *basename = get_basename(inFile);
        char n[1024];
        sprintf(n, "%s.csv", basename);
        fp = fopen(n, "r");
        FREE(basename);
    }
    if (fp) {
        line = (char*)MALLOC(sizeof(char)*(1+LINE_MAX_LEN));
        int line_count=0;
        while (fgets(line, LINE_MAX_LEN, fp)) {
            line[strlen(line)-1] = '\0';
            s=line;
            while(isspace((int)(*s))) ++s;
            if (*s == '#') {
                char *tok = strtok(s,",");
                if (tok) {
                    s = strstr(uc(tok), "FILE");
                    if (s && strncmp(uc(s), "FILE", 4) == 0) {
                        tok = strtok(NULL, ",");
                        if (tok) {
                            s = strstr(uc(tok), "POLYGON");
                            if (s && strncmp(uc(s), "POLYGON", 7) == 0) {
                                isPolygon = 1;
                                break;
                            }
                        }
                    }
                }
            }
            // avoid scanning the entire contents of a huge file
            if (++line_count>100)
              break;
        }
    }
    FREE(line);
    FCLOSE(fp);

    return isPolygon;
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

int isrgps(char *inFile)
{
    // FIXME: Don't know how to check this yet ...Rudi?
    int isRGPS = 0;

    return isRGPS;
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

void split_polygon(double *lat, double *lon, int nCoords, 
  int *start, double *mLat, double *mLon)
{
  int nCols = nCoords + 5;
  int ii, nNegative = 0;
  double m;
  for (ii=0; ii<nCoords-1; ii++) {
    if (lon[ii] < 0)
      nNegative++;
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
    // crossing dateline
    if (lon[ii] < 0 && lon[ii+1] > 0) { 
      m = (lat[ii+1] - lat[ii])/(fabs(lon[ii+1] - lon[ii]) - 360.0);
      mLat[nPos] = mLat[nNeg] = m*(180.0 - lon[ii+1]) + lat[ii+1];
      mLon[nPos] = 180.0;
      mLon[nNeg] = -180.0;
      nPos++;
      nNeg++;
    }
    else if (lon[ii] > 0 && lon[ii+1] < 0) {
      m = (lat[ii+1] - lat[ii])/(360.0 - fabs(lon[ii+1] - lon[ii]));
      mLat[nPos] = mLat[nNeg] = m*(180.0 - lon[ii]) + lat[ii];
      mLon[nPos] = 180.0;
      mLon[nNeg] = -180.0;
      nPos++;
      nNeg++;
    }
  }
  mLat[nNegative+2] = mLat[0];
  mLon[nNegative+2] = mLon[0];
  mLat[nCols-1] = mLat[nNegative+3];
  mLon[nCols-1] = mLon[nNegative+3];
}
