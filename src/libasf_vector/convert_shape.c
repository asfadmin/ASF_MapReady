#include <stdio.h>
#include <ctype.h>

#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include "asf.h"
#include "dateUtil.h"
#include "shapefil.h"
#include "asf_vector.h"
#include "geotiff_support.h"

#define LINE_MAX    (1024)

char *fix_attribute_name(const char *name)
{
  int len = strlen(name);
  if (len>10) len=10; // truncate to 10 characters

  char *ret = CALLOC(sizeof(char),len+1);

  int i;
  for (i=0; i<len; ++i) {
    // allowed characters are any except: space, period, comma
    if (isspace(name[i]) || name[i]=='.' || name[i]==',')
      ret[i]='_';
    else
      ret[i]=name[i];
  }

  if (strcmp(ret,name)!=0)
    asfPrintStatus("Attribute name changed: %s -> %s\n", name, ret);

  return ret;
}

int csv2shape(char *inFile, char *outFile)
{
  int num_meta_cols, num_data_cols;
  csv_meta_column_t *meta_column_info;
  csv_data_column_t *data_column_info;

  FILE *fp = csv_open(inFile,
                      &num_meta_cols, &meta_column_info,
                      &num_data_cols, &data_column_info);

  // csv_open() returns NULL if the file can't be processed
  if (!fp)
    return FALSE;

  // this is just for debugging, if you want to print out what was found
  csv_info(num_meta_cols, meta_column_info, num_data_cols, data_column_info);

  // start line counter at 2 (header line is not part of this loop)
  int line_num=2;
  int entry_num=0;

  // shapefile stuff
  DBFHandle dbase;
  SHPHandle shape;

  // Initialize output
  char *dbaseFile = appendExt(outFile, ".dbf");
  //printf("Creating dbase file: %s\n", dbaseFile);
  dbase = DBFCreate(dbaseFile);
  if (!dbase) {
    asfPrintWarning("Could not create database file '%s'\n", dbaseFile);
    FCLOSE(fp);
    return FALSE;
  }

  int i;
  for (i=0; i<num_meta_cols; i++) {
    char *name = fix_attribute_name(meta_column_info[i].column_name);
    switch (meta_column_info[i].data_type)
      {
      case CSV_STRING:
      case CSV_DATE:
        if (DBFAddField(dbase, name, FTString, 50, 0) == -1) {
          asfPrintWarning("Could not add %s field to database file\n",
                          meta_column_info[i].column_name);
          FCLOSE(fp);
          DBFClose(dbase);
          return FALSE;
        }
        break;
      case CSV_DOUBLE:
        if (DBFAddField(dbase, name, FTDouble, 16, 7) == -1) {
          asfPrintWarning("Could not add %s field to database file\n",
                          meta_column_info[i].column_name);
          FCLOSE(fp);
          DBFClose(dbase);
          return FALSE;
        }
        break;
      case CSV_INTEGER:
      case CSV_LOGICAL:
        if (DBFAddField(dbase, name, FTInteger,15, 0) == -1) {
          asfPrintWarning("Could not add %s field to database file\n",
                          meta_column_info[i].column_name);
          FCLOSE(fp);
          DBFClose(dbase);
          return FALSE;
        }
        break;
      default:
        asfPrintWarning("DBF column type not supported!\n");
        break;
      }
    FREE(name);
  }

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  char *shpfile = appendExt(outFile, ".shp");
  if (num_data_cols == 1)
    shape = SHPCreate(shpfile, SHPT_POINT);
  else if (num_data_cols > 1)
    shape = SHPCreate(shpfile, SHPT_POLYGON);
  else {
    asfPrintWarning("No geolocation information in the input file (%s).\n",
		    inFile);
    FCLOSE(fp);
    DBFClose(dbase);
    return FALSE;
  }

  // Close shapefile for initialization
  SHPClose(shape);
  FREE(dbaseFile);

  open_shape(shpfile, &dbase, &shape);

  // these store the output lat/lon
  double *write_lon = MALLOC(sizeof(double)*(num_data_cols+1));
  double *write_lat = MALLOC(sizeof(double)*(num_data_cols+1));

  char line[1024];
  while (fgets(line, 1023, fp)) {

    char **column_data;
    double *lats, *lons;
    int ok = csv_line_parse(line,
                            num_meta_cols, meta_column_info,
                            num_data_cols, data_column_info,
                            &column_data, &lats, &lons);

    // csv_line_parse() will return FALSE when the line is invalid
    if (!ok)
      continue;

    // dealing with metadata
    for (i=0; i<num_meta_cols; ++i) {

      char *val = column_data[i];
      char *name = meta_column_info[i].column_name;

      switch (meta_column_info[i].data_type) {
        case CSV_STRING:
        case CSV_DATE:
          DBFWriteStringAttribute(dbase, entry_num, i, val);
          break;
          
        case CSV_DOUBLE:
          DBFWriteDoubleAttribute(dbase, entry_num, i, atof(val));
          break;
          
        case CSV_INTEGER:
        case CSV_LOGICAL:
          DBFWriteIntegerAttribute(dbase, entry_num, i, atoi(val));
          break;

        case CSV_UNKNOWN:
          // should never happen
          asfPrintStatus("%s (unknown): %s\n", name, val);
          break;
      }
    }

    // Write shape object
    SHPObject *shapeObject=NULL;
    if (num_data_cols == 1) {
      shapeObject = 
        SHPCreateSimpleObject(SHPT_POINT, 1, &lons[0], &lats[0], NULL);
    }
    else {
      for (i=0; i<num_data_cols; ++i) {
        write_lat[i] = lats[i];
        write_lon[i] = lons[i];
      }
      write_lat[num_data_cols]=write_lat[0]; // closing the polygon
      write_lon[num_data_cols]=write_lon[0];
      shapeObject =
        SHPCreateSimpleObject(SHPT_POLYGON, num_data_cols+1,
                              write_lon, write_lat, NULL);
    }
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);

    csv_free(num_meta_cols, column_data, lats, lons);

    ++line_num;
    ++entry_num;
  }
  FCLOSE(fp);
  FREE(write_lon);
  FREE(write_lat);
  FREE(shpfile);

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return TRUE;
}

// Convert metadata to shape
void meta2shape(char *line, DBFHandle dbase, SHPHandle shape, int n)
{
  meta_parameters *meta;
  double lat[5], lon[5];

  // Read metadata
  meta = meta_read(line);

  // Determine corner coordinates
  meta_get_latLon(meta, 0, 0, 0.0, &lat[0], &lon[0]);
  meta_get_latLon(meta, 0, meta->general->sample_count, 0.0,
          &lat[1], &lon[1]);
  meta_get_latLon(meta, meta->general->line_count, meta->general->sample_count,
                  0.0, &lat[2], &lon[2]);
  meta_get_latLon(meta, meta->general->line_count, 0, 0.0,
          &lat[3], &lon[3]);
  lat[4] = lat[0];
  lon[4] = lon[0];

  // Write information into database file
  DBFWriteStringAttribute(dbase, n, 0, meta->general->sensor);
  DBFWriteStringAttribute(dbase, n, 1, meta->general->sensor_name);
  DBFWriteStringAttribute(dbase, n, 2, meta->general->mode);
  if (meta->sar)
    DBFWriteStringAttribute(dbase, n, 3, meta->sar->polarization);
  DBFWriteIntegerAttribute(dbase, n, 4, meta->general->orbit);
  DBFWriteIntegerAttribute(dbase, n, 5, meta->general->frame);
  DBFWriteStringAttribute(dbase, n, 6, meta->general->acquisition_date);
  if (meta->general->orbit_direction == 'D')
    DBFWriteStringAttribute(dbase, n, 7, "Descending");
  else if (meta->general->orbit_direction == 'A')
    DBFWriteStringAttribute(dbase, n, 7, "Ascending");
  DBFWriteDoubleAttribute(dbase, n, 8, meta->general->center_latitude);
  DBFWriteDoubleAttribute(dbase, n, 9, meta->general->center_longitude);
  DBFWriteDoubleAttribute(dbase, n, 10, lat[0]);
  DBFWriteDoubleAttribute(dbase, n, 11, lon[0]);
  DBFWriteDoubleAttribute(dbase, n, 12, lat[1]);
  DBFWriteDoubleAttribute(dbase, n, 13, lon[1]);
  DBFWriteDoubleAttribute(dbase, n, 14, lat[2]);
  DBFWriteDoubleAttribute(dbase, n, 15, lon[2]);
  DBFWriteDoubleAttribute(dbase, n, 16, lat[3]);
  DBFWriteDoubleAttribute(dbase, n, 17, lon[3]);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  meta_free(meta);

  return;
}

// Convert point to shape
void point2shape(char *inFile, DBFHandle dbase, SHPHandle shape)
{
    double lat, lon;
    char id[255], *p;
    int n;
    char *line;
    FILE *fp;

    fp = FOPEN(inFile, "r");
    line = (char*)MALLOC(sizeof(char)*LINE_MAX);
    n=0;
    while (fgets(line, LINE_MAX, fp)) {
        // Read ID and lat/lon;
        line[strlen(line)-1] = '\0';
        p = line;
        while (isspace((int)(*p))) p++;
        if (*p != '#') {
            p = strchr(line, ',');
            if (p && *p == ',') {
                int iid;
                sscanf(p+1, "%lf,%lf", &lat, &lon);
                *p = '\0';
                iid = strtol(line, (char**)NULL, 10);
                sprintf(id, "%d", iid);
                *p = ',';

                // Write information into database file
                DBFWriteStringAttribute(dbase, n, 0, id);
                DBFWriteDoubleAttribute(dbase, n, 1, lat);
                DBFWriteDoubleAttribute(dbase, n++, 2, lon);

                // Write shape object
                SHPObject *shapeObject=NULL;
                shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &lon, &lat, NULL);
                SHPWriteObject(shape, -1, shapeObject);
                SHPDestroyObject(shapeObject);
            }
        }
    }
    FREE(line);
    FCLOSE(fp);

    return;
}

// Convert polygon to shape
void polygon2shape_new(char *inFile, char *outFile)
{
  FILE *fp;
  double *lat, *lon;
  char line[1024], id[255], *p;
  int vertices=0, ii=0;
  DBFHandle dbase;
  SHPHandle shape;

  // Initialize output
  shape_init(outFile, POLYGON);
  open_shape(outFile, &dbase, &shape);

  // See how many vertices we have
  fp = FOPEN(inFile, "r");
  while (fgets(line, 1024, fp)) {
    if (line[0] != '#')
      vertices++;
  }
  FCLOSE(fp);

  // Allocate memory
  lat = (double *) MALLOC(sizeof(double)*(vertices+1));
  lon = (double *) MALLOC(sizeof(double)*(vertices+1));

  // Read file again: ID and lat/lon
  fp = FOPEN(inFile, "r");
  while (fgets(line, 1024, fp)) {
    p = strchr(line, ',');
    if (p && line[0] != '#') {
      lat[ii] = strtod(line,(char **)NULL);
      p++;
      lon[ii] = strtod(p,(char **)NULL);
      //sscanf(p+1, "%lf,%lf", &lat[ii], &lon[ii]);
      //*p = '\0';
      sprintf(id, "%s", line);
      *p = ',';
      ii++;
    }
  }
  FCLOSE(fp);
  lat[vertices] = lat[0];
  lon[vertices] = lon[0];
  sprintf(id, "%s", outFile);

  // Write information into database file
  DBFWriteStringAttribute(dbase, 0, 0, id);
  DBFWriteIntegerAttribute(dbase, 0, 1, vertices);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject =
    SHPCreateSimpleObject(SHPT_POLYGON, vertices+1, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);
  FREE(lat);
  FREE(lon);

  return;
}


// Convert polygon to shape
// FIXME: This function is broken ...it gets called by write_shape(), but
// in all places where write_shape() would normally have been called (as is
// done for POINT files) ...we call polygon2shape_new() instead.  For
// consistency, we should fix polygon2shape() and then use write_shape() as
// with other formats ...but calling polygon2shape_new() works for now ...
// 
void polygon2shape(char *line, DBFHandle dbase, SHPHandle shape, int n)
{
  int ii, vertices;
  double *lat, *lon;
  char id[255];
  char *p, *p_lat, *p_lon;

  // Allocate memory
  p_lat = (char *) MALLOC(sizeof(char)*255);
  p_lon = (char *) MALLOC(sizeof(char)*255);

  // Read ID and number of vertices;
  p = strchr(line, ',');
  if (p) {
    sscanf(p+1, "%d", &vertices);
    *p = '\0';
    sprintf(id, "%s", line);
    *p = ',';
    line = strchr(p+1, ',');
  }

  // Read coordinates of the vertices
  lat = (double *) MALLOC(sizeof(double)*(vertices+1));
  lon = (double *) MALLOC(sizeof(double)*(vertices+1));
  p_lat = line;
  for (ii=0; ii<vertices; ii++) {
    sscanf(p_lat+1, "%lf", &lat[ii]);
    p_lon = strchr(p_lat+1, ',');
    sscanf(p_lon+1, "%lf", &lon[ii]);
    p_lat = strchr(p_lon+1, ',');
  }
  lat[vertices] = lat[0];
  lon[vertices] = lon[0];

  // Write information into database file
  DBFWriteStringAttribute(dbase, n, 0, id);
  DBFWriteIntegerAttribute(dbase, n, 1, vertices);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, vertices+1, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  FREE(lat);
  FREE(lon);
  FREE(p_lat);
  FREE(p_lon);

  return;
}

void geotiff2shape(char *inFile, DBFHandle dbase, SHPHandle shape, int n)
{
  double lat[5], lon[5];
  int no_location_info=1;
  meta_parameters *meta = NULL; 
  int ignore[MAX_BANDS];

  meta = read_generic_geotiff_metadata(inFile, ignore);
  if (meta && meta->location) {
    meta_location *ml = meta->location; // Convenience pointer
    no_location_info = 0; // false ...location info was found
    lon[0] = ml->lon_start_near_range;
    lat[0] = ml->lat_start_near_range;
    lon[1] = ml->lon_start_far_range;
    lat[1] = ml->lat_start_far_range;
    lon[2] = ml->lon_end_far_range;
    lat[2] = ml->lat_end_far_range;
    lon[3] = ml->lon_end_near_range;
    lat[3] = ml->lat_end_near_range;
    lon[4] = lon[0];
    lat[4] = lat[0];
  }
  else {
    meta_free(meta);
    asfPrintError("GeoTIFF %s contains no location information\n", inFile);
  }

  // Write information into database file
  /* Can be used again once we write the entire metadata structure in a
     GeoTIFF tag.
  DBFWriteStringAttribute(dbase, n, 0, meta->general->sensor);
  DBFWriteStringAttribute(dbase, n, 1, meta->general->sensor_name);
  DBFWriteStringAttribute(dbase, n, 2, meta->general->mode);
  if (meta->sar)
    DBFWriteStringAttribute(dbase, n, 3, meta->sar->polarization);
  DBFWriteIntegerAttribute(dbase, n, 4, meta->general->orbit);
  DBFWriteIntegerAttribute(dbase, n, 5, meta->general->frame);
  DBFWriteStringAttribute(dbase, n, 6, meta->general->acquisition_date);
  if (meta->general->orbit_direction == 'D')
    DBFWriteStringAttribute(dbase, n, 7, "Descending");
  else if (meta->general->orbit_direction == 'A')
    DBFWriteStringAttribute(dbase, n, 7, "Ascending");
  */
  DBFWriteStringAttribute(dbase, n, 0, "unknown");
  DBFWriteStringAttribute(dbase, n, 1, "unknown");
  DBFWriteStringAttribute(dbase, n, 2, "unknown");
  DBFWriteStringAttribute(dbase, n, 3, "unknown");
  DBFWriteIntegerAttribute(dbase, n, 4, -99);
  DBFWriteIntegerAttribute(dbase, n, 5, -99);
  DBFWriteStringAttribute(dbase, n, 6, "unknown");
  DBFWriteStringAttribute(dbase, n, 7, "unknown");
  DBFWriteDoubleAttribute(dbase, n, 8, meta->general->center_latitude);
  DBFWriteDoubleAttribute(dbase, n, 9, meta->general->center_longitude);
  DBFWriteDoubleAttribute(dbase, n, 10, lat[0]);
  DBFWriteDoubleAttribute(dbase, n, 11, lon[0]);
  DBFWriteDoubleAttribute(dbase, n, 12, lat[1]);
  DBFWriteDoubleAttribute(dbase, n, 13, lon[1]);
  DBFWriteDoubleAttribute(dbase, n, 14, lat[2]);
  DBFWriteDoubleAttribute(dbase, n, 15, lon[2]);
  DBFWriteDoubleAttribute(dbase, n, 16, lat[3]);
  DBFWriteDoubleAttribute(dbase, n, 17, lon[3]);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  meta_free(meta);

  return;
}

// Convert RGPS cell to shape
void rgps2shape(cell_t cell, double *lat, double *lon, int vertices,
        DBFHandle dbase, SHPHandle shape, int n)
{
  // Write information into database file
  DBFWriteIntegerAttribute(dbase, n, 0, cell.cell_id);
  DBFWriteIntegerAttribute(dbase, n, 1, cell.nVertices);
  DBFWriteStringAttribute(dbase, n, 2, cell.date);
  DBFWriteStringAttribute(dbase, n, 3, cell.sourceImage);
  DBFWriteStringAttribute(dbase, n, 4, cell.targetImage);
  DBFWriteStringAttribute(dbase, n, 5, cell.stream);
  DBFWriteDoubleAttribute(dbase, n, 6, cell.area);
  DBFWriteDoubleAttribute(dbase, n, 7, cell.multi_year_ice);
  DBFWriteDoubleAttribute(dbase, n, 8, cell.open_water);
  DBFWriteDoubleAttribute(dbase, n, 9, cell.incidence_angle);
  DBFWriteDoubleAttribute(dbase, n, 10, cell.cell_x);
  DBFWriteDoubleAttribute(dbase, n, 11, cell.cell_y);
  DBFWriteDoubleAttribute(dbase, n, 12, cell.dudx);
  DBFWriteDoubleAttribute(dbase, n, 13, cell.dudy);
  DBFWriteDoubleAttribute(dbase, n, 14, cell.dvdx);
  DBFWriteDoubleAttribute(dbase, n, 15, cell.dvdy);
  DBFWriteDoubleAttribute(dbase, n, 16, cell.dtp);
  DBFWriteDoubleAttribute(dbase, n, 17, cell.temperature);
  DBFWriteDoubleAttribute(dbase, n, 18, cell.u_wind);
  DBFWriteDoubleAttribute(dbase, n, 19, cell.v_wind);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, vertices+1,
                      lon, lat, NULL);
  if (shapeObject == NULL)
    asfPrintError("Could not create shape object (%d)\n", n);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}

// Convert RGPS grid points to shape
void rgps_grid2shape(grid_attr_t grid, DBFHandle dbase, SHPHandle shape, int n)
{
  // Write information into database file
  DBFWriteIntegerAttribute(dbase, n, 0, grid.grid_id);
  DBFWriteStringAttribute(dbase, n, 1, grid.date);
  DBFWriteDoubleAttribute(dbase, n, 2, grid.day);
  DBFWriteDoubleAttribute(dbase, n, 3, grid.grid_x);
  DBFWriteDoubleAttribute(dbase, n, 4, grid.grid_y);
  DBFWriteStringAttribute(dbase, n, 5, grid.sourceImage);
  DBFWriteStringAttribute(dbase, n, 6, grid.targetImage);
  DBFWriteStringAttribute(dbase, n, 7, grid.stream);
  DBFWriteIntegerAttribute(dbase, n, 8, grid.quality);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1,
                      &grid.lon, &grid.lat, NULL);
  if (shapeObject == NULL)
    asfPrintError("Could not create shape object (%d)\n", n);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}

// Convert RGPS weather data to shape
void rgps_weather2shape(char *line, DBFHandle dbase, SHPHandle shape, int n)
{
  double lat, lon, direction, speed, temperature, pressure;
  char date[15], *p;

  // Read weather information;
  p = strchr(line, ',');
  if (p) {
    sscanf(p+1, "%lf,%lf,%lf,%lf,%lf,%lf",
       &lat, &lon, &direction, &speed, &temperature, &pressure);
    *p = 0;
    sprintf(date, "%s", line);
  }

  // Write information into database file
  DBFWriteStringAttribute(dbase, n, 0, date);
  DBFWriteDoubleAttribute(dbase, n, 1, lat);
  DBFWriteDoubleAttribute(dbase, n, 2, lon);
  DBFWriteDoubleAttribute(dbase, n, 3, direction);
  DBFWriteDoubleAttribute(dbase, n, 4, speed);
  DBFWriteDoubleAttribute(dbase, n, 5, temperature);
  DBFWriteDoubleAttribute(dbase, n, 6, pressure);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &lon, &lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}

// Convert multimatch to shape file
void multimatch2shape(char *line, DBFHandle dbase, SHPHandle shape, int n)
{
  double lat, lon, ref_x, ref_y, ref_z, search_x, search_y, search_z;
  double dx, dy, dz, direction, speed;

  // Read information from line
  sscanf(line, "%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf",
     &lat, &lon, &ref_x, &ref_y, &ref_z, &search_x, &search_y, &search_z,
     &dx, &dy, &dz, &direction, &speed);

  // Write information into database file
  DBFWriteDoubleAttribute(dbase, n, 0, ref_x);
  DBFWriteDoubleAttribute(dbase, n, 1, ref_y);
  DBFWriteDoubleAttribute(dbase, n, 2, ref_z);
  DBFWriteDoubleAttribute(dbase, n, 3, search_x);
  DBFWriteDoubleAttribute(dbase, n, 4, search_y);
  DBFWriteDoubleAttribute(dbase, n, 5, search_z);
  DBFWriteDoubleAttribute(dbase, n, 6, dx);
  DBFWriteDoubleAttribute(dbase, n, 7, dy);
  DBFWriteDoubleAttribute(dbase, n, 8, dz);
  DBFWriteDoubleAttribute(dbase, n, 9, direction);
  DBFWriteDoubleAttribute(dbase, n, 10, speed);

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &lon, &lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  return;
}

// Convert shapefile to text file - general dump function
void shape2text(char *inFile, char *outfile)
{
    int skipLastVertice = 0;
    DBFHandle dbase;
    SHPHandle shape;
    DBFFieldType dbaseType;
    SHPObject *shapeObject;
    char fieldName[25], str[50];
    char textFileType[256];
    char outFile[1024], *basename;
    FILE *fp = NULL;

    int ii, kk, nEntities, nVertices, nParts;
    int nFields, nWidth, nDecimals, nValue, pointType;
    double fValue;
    const char *sValue;

    // Open shapefile
    open_shape(inFile, &dbase, &shape);

    // Extract the vital information out of the shapefile
    SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
    switch (pointType) {
        case SHPT_POLYGON:
            strcpy(textFileType, "Polygon");
            skipLastVertice = 1;
            break;
        case SHPT_POINT:
            strcpy(textFileType, "Point");
            skipLastVertice = 0;
            break;
        case SHPT_ARC:
            asfPrintError("Shape file data type 'Arc' currently not supported\n");
            break;
        case SHPT_MULTIPOINT:
            asfPrintError("Shape file data type 'Multipoint' currently not supported\n");
            break;
        default:
            asfPrintError("Unexpected or unrecognized shape file data format\n");
            break;
    }
    // Write the text file:
    strcpy(outFile, outfile);
    for (ii=0; ii<nEntities; ii++) {
        // (For each shape... for each polygon or point... Write a separate CSV file)

        // Open file and write header
        if (nEntities > 1) {
            char *ext = findExt(outfile);
            basename = get_basename(outfile);
            sprintf(outFile, "%s_shape_%03d%s", basename, ii, ext);
        }
        asfPrintStatus("\nWriting to file %s...\n", outFile);
        fp = FOPEN(outFile, "w");
        fprintf(fp, "# File type        , %s\n", textFileType);
        // Read object and report basic information
        shapeObject = SHPReadObject(shape, ii);
        nParts = shapeObject->nParts;
        nVertices = shapeObject->nVertices;
        if (nParts > 1) {
            asfPrintWarning("Shape files with multi-part shapes not supported.\n"
                            "All vertices will be extracted but into a single\n"
                            "entity of the type specified in the shapefile (polygon or point etc)\n");
        }

        // Extract the attributes out of the database file
        nFields = DBFGetFieldCount(dbase);
        sValue = (char *) MALLOC(sizeof(char)*255);
        char id[256];
        for (kk=0; kk<nFields; kk++) {
            dbaseType = DBFGetFieldInfo(dbase, kk, fieldName, &nWidth, &nDecimals);
            switch (dbaseType) {
                case FTString:
                    sValue = DBFReadStringAttribute(dbase, ii, kk);
                    break;
                case FTInteger:
                    nValue = DBFReadIntegerAttribute(dbase, ii, kk);
                    break;
                case FTDouble:
                    fValue = DBFReadDoubleAttribute(dbase, ii, kk);
                    sprintf(str, "%%s: %%%d.%dlf\n", nWidth, nDecimals);
                    break;
                case FTLogical:
                case FTInvalid:
                default:
                    break;
            }
            if (strncmp(uc(fieldName), "ID", 2) == 0) {
                sprintf(id,"%s (input shape file: %s)", sValue, inFile);
            }
            else {
                sprintf(id,"%03d (input shape file: %s)", ii, inFile);
            }
        }
        // Write shape header information to output file
        fprintf(fp, "# Polygon ID (name), %s\n", id);
        fprintf(fp, "#\n");
        fprintf(fp, "# Latitude, Longitude\n");

        // Write each vertice to output file (whether for polygon or point shape file)
        int j;
        if (skipLastVertice) nVertices--;
        for (j=0; j<nVertices; j++) {
            fprintf(fp, "%12.6f,%12.6f\n",
                    shapeObject->padfX[j], shapeObject->padfY[j]);
        }

        FCLOSE(fp);
        SHPDestroyObject(shapeObject);
    }

  // Close shapefile
  close_shape(dbase, shape);

  return;
}

// Convert shapefile to csv file - general dump function but preserving
// header names
void shape2csv(char *inFile, char *outfile)
{
  DBFHandle dbase;
  SHPHandle shape;
  DBFFieldType dbaseType;
  SHPObject *shapeObject;
  char fieldName[25], str[50];;
  FILE *fp = NULL;

  int ii, kk, nEntities, pointType, nFields, nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Extract the vital information out of the shapefile
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);

  // Extract the attributes out of the database file
  nFields = DBFGetFieldCount(dbase);
  sValue = (char *) MALLOC(sizeof(char)*255);

  // Open output file for writing
  fp = FOPEN(outfile, "w");

  // Write the header names
  for (kk=0; kk<nFields-1; kk++) {
    dbaseType = DBFGetFieldInfo(dbase, kk, fieldName, &nWidth, &nDecimals);
    fprintf(fp, "%s,", fieldName);
  }
  dbaseType = DBFGetFieldInfo(dbase, nFields-1, fieldName, 
			      &nWidth, &nDecimals);
  fprintf(fp, "%s\n", fieldName);

  for (ii=0; ii<nEntities; ii++) {
    // Read object and report basic information
    shapeObject = SHPReadObject(shape, ii);
    for (kk=0; kk<nFields; kk++) {
      dbaseType = DBFGetFieldInfo(dbase, kk, fieldName, &nWidth, &nDecimals);
      switch (dbaseType) {
      case FTString:
	sValue = DBFReadStringAttribute(dbase, ii, kk);
	fprintf(fp, "%s", sValue);
	break;
      case FTInteger:
	nValue = DBFReadIntegerAttribute(dbase, ii, kk);
	fprintf(fp, "%d", nValue);
	break;
      case FTDouble:
	fValue = DBFReadDoubleAttribute(dbase, ii, kk);
	sprintf(str, "%%.%dlf", nDecimals);
	fprintf(fp, str, fValue);
	break;
      case FTLogical:
      case FTInvalid:
      default:
	break;
      }
      if (kk < nFields-1)
	fprintf(fp, ",");
      else
	fprintf(fp, "\n");
    }
    SHPDestroyObject(shapeObject);
  }
  FCLOSE(fp);

  // Close shapefile
  close_shape(dbase, shape);

}
