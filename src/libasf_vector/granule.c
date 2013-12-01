#include "asf_vector.h"
#include "shapefil.h"
#include "asf_nan.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include "ursa.h"
#include "dateUtil.h"

typedef struct {
  int stack_id;
  char granule[20];
  char satellite[5];
  char beam_mode[5];
  double off_nadir;
  int orbit;
  int frame;
  char acq_date[25];
  char orbit_dir[15];
  int path;
  int terrain;
  int insar;
  double lat1, lon1;
  double lat2, lon2;
  double lat3, lon3;
  double lat4, lon4;
} granule_type_t;

void gran_init(granule_type_t *gran)
{
  gran->stack_id = MAGIC_UNSET_INT;
  strcpy(gran->granule, MAGIC_UNSET_STRING);
  strcpy(gran->satellite, MAGIC_UNSET_STRING);
  strcpy(gran->beam_mode, MAGIC_UNSET_STRING);
  gran->off_nadir = MAGIC_UNSET_DOUBLE;
  gran->orbit = MAGIC_UNSET_INT;
  gran->frame = MAGIC_UNSET_INT;
  strcpy(gran->acq_date, MAGIC_UNSET_STRING);
  strcpy(gran->orbit_dir, MAGIC_UNSET_STRING);
  gran->path = MAGIC_UNSET_INT;
  gran->terrain = -1;
  gran->insar = -1;
  gran->lat1 = gran->lon1 = MAGIC_UNSET_DOUBLE;
  gran->lat2 = gran->lon2 = MAGIC_UNSET_DOUBLE;
  gran->lat3 = gran->lon3 = MAGIC_UNSET_DOUBLE;
  gran->lat4 = gran->lon4 = MAGIC_UNSET_DOUBLE;
}

static void strip_end_whitesp(char *s)
{
    char *p = s + strlen(s) - 1;
    while (isspace(*p) && p>s)
        *p-- = '\0';
}

static int read_gran_line(char *header, int n, char *line, granule_type_t *gran)
{
  int ii, ok;
  char *test = (char *) MALLOC(sizeof(char)*255);
  for (ii=0; ii<n; ii++) {
    test = get_column(header, ii);
    //test[strlen(test)-1] = '\0';
    //test++;
    if (strcmp(test, "Stack ID") == 0)
      gran->stack_id = get_int(line, ii);
    else if (strcmp(test, "Granule") == 0)
      strcpy(gran->granule, get_str(line, ii));
    else if (strcmp(test, "Satellite") == 0)
      strcpy(gran->satellite, get_str(line, ii));
    else if (strcmp(test, "Beam Mode") == 0)
      strcpy(gran->beam_mode, get_str(line, ii));
    else if (strcmp(test, "Off Nadir") == 0)
      gran->off_nadir = get_double(line, ii);
    else if (strcmp(test, "Orbit") == 0)
      gran->orbit = get_int(line, ii);
    else if (strcmp(test, "Frame") == 0)
      gran->frame = get_int(line, ii);
    else if (strcmp(test, "Acq Date") == 0)
      strcpy(gran->acq_date, get_str(line, ii));
    else if (strcmp(test, "Direction") == 0)
      strcpy(gran->orbit_dir, get_str(line, ii));
    else if (strcmp(test, "Path") == 0)
      gran->path = get_int(line, ii);
    else if (strcmp(test, "Terrain") == 0)
      gran->terrain = get_int(line, ii);
    else if (strcmp(test, "InSAR") == 0)
      gran->insar = get_int(line, ii);
    else if (strcmp(test, "Lat1") == 0)
      gran->lat1 = get_req_double(line, ii, &ok);
    else if (strcmp(test, "Lon1") == 0)
      gran->lon1 = get_req_double(line, ii, &ok);
    else if (strcmp(test, "Lat2") == 0)
      gran->lat2 = get_req_double(line, ii, &ok);
    else if (strcmp(test, "Lon2") == 0)
      gran->lon2 = get_req_double(line, ii, &ok);
    else if (strcmp(test, "Lat3") == 0)
      gran->lat3 = get_req_double(line, ii, &ok);
    else if (strcmp(test, "Lon3") == 0)
      gran->lon3 = get_req_double(line, ii, &ok);
    else if (strcmp(test, "Lat4") == 0)
      gran->lat4 = get_req_double(line, ii, &ok);
    else if (strcmp(test, "Lon4") == 0)
      gran->lon4 = get_req_double(line, ii, &ok);
  }

  return ok;
}

// Check location information
static int check_gran_location(FILE *ifp, char **header_line, int *n)
{
  dbf_header_t *dbf;
  int ii, nCols;
  char *header = (char *) MALLOC(sizeof(char)*1024);
  fgets(header, 1024, ifp);
  strip_end_whitesp(header);
  int nColumns = get_number_columns(header);

  // Read configuration file
  read_header_config("URSA", &dbf, &nCols);

  // ensure we have the columns we need
  int granule_col = find_str(header, "Granule");
  int near_start_lat_col = find_str(header, "Lat1");
  int near_start_lon_col = find_str(header, "Lon1");
  int far_start_lat_col = find_str(header, "Lat2");
  int far_start_lon_col = find_str(header, "Lon2");
  int near_end_lat_col = find_str(header, "Lat3");
  int near_end_lon_col = find_str(header, "Lon3");
  int far_end_lat_col = find_str(header, "Lat4");
  int far_end_lon_col = find_str(header, "Lon4");

  // Check whether all visible columns are actually available in the file
  for (ii=0; ii<nCols; ii++) {
    if (find_str(header, dbf[ii].header) < 0)
      dbf[ii].visible = FALSE;
  }

  int all_ok=TRUE;
  if (granule_col < 0) {
    printf("Missing: Granule\n");
    all_ok=FALSE;
  }
  if (near_start_lat_col < 0) {
    printf("Missing: Lat1\n");
    all_ok=FALSE;
  }
  if (near_start_lon_col < 0) {
    printf("Missing: Lon1\n");
    all_ok=FALSE;
  }
  if (far_start_lat_col < 0) {
    printf("Missing: Lat2\n");
    all_ok=FALSE;
  }
  if (far_start_lon_col < 0) {
    printf("Missing: Lon2\n");
    all_ok=FALSE;
  }
  if (near_end_lat_col < 0) {
    printf("Missing: Lat3\n");
    all_ok=FALSE;
  }
  if (near_end_lon_col < 0) {
    printf("Missing: Lon3\n");
    all_ok=FALSE;
  }
  if (far_end_lat_col < 0) {
    printf("Missing: Lat4\n");
    all_ok=FALSE;
  }
  if (far_end_lon_col < 0) {
    printf("Missing: Lon4\n");
    all_ok=FALSE;
  }
  if (!all_ok) {
    printf("Required data columns missing, cannot process this file.\n");
    return 0;
  }
  *header_line = header;
  *n = nColumns;

  return 1;
}

static void get_polygon_location(granule_type_t *gran, 
                                 double **plat, double **plon)
{
  double *lat = (double *) MALLOC(sizeof(double)*5);
  double *lon = (double *) MALLOC(sizeof(double)*5);
  lat[0] = lat[4] = gran->lat1;
  lon[0] = lon[4] = gran->lon1;
  lat[1] = gran->lat2;
  lon[1] = gran->lon2;
  lat[2] = gran->lat3;
  lon[2] = gran->lon3;
  lat[3] = gran->lat4;
  lon[3] = gran->lon4;
  *plat = lat;
  *plon = lon;
}

static void add_to_shape(DBFHandle dbase, SHPHandle shape, granule_type_t *gran,
			 dbf_header_t *dbf, int nCols, int n, 
			 double *lat, double *lon)
{
  int ii, field = 0;

  // Write fields into the database
  for (ii=0; ii<nCols; ii++) {
  
    if (strcmp(dbf[ii].header, "Stack ID") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, gran->stack_id);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Granule") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, gran->granule);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Satellite") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, gran->satellite);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Beam Mode") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, gran->beam_mode);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Off Nadir") == 0 &&
         dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, gran->off_nadir);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Orbit") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, gran->orbit);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Frame") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, gran->frame);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Acq Date") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, gran->acq_date);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Direction") == 0 && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, gran->orbit_dir);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Path") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, gran->path);
      field++;
    }
    else if (strcmp(dbf[ii].header, "Terrain") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, gran->terrain);
      field++;
    }
    else if (strcmp(dbf[ii].header, "InSAR") == 0 && dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, gran->insar);
      field++;
    }
  }

  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);
}

int granule2shape(char *inFile, char *outFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  granule_type_t gran;
  dbf_header_t *dbf;
  char *header, line[1024];
  int nCols, nColumns, ii=0;
  double *lat, *lon;

  // Read configuration file
  read_header_config("GRANULE", &dbf, &nCols);

  // Read granule file
  FILE *ifp = FOPEN(inFile, "r");
  assert(ifp);
  check_gran_location(ifp, &header, &nColumns);

  // Initialize the database file
  shape_init(outFile, GRANULE);
  open_shape(outFile, &dbase, &shape);

  while (fgets(line, 1022, ifp) != NULL) {
    strip_end_whitesp(line);

    // now get the individual column values
    gran_init(&gran);
    if (read_gran_line(header, nColumns, line, &gran)) {
	  get_polygon_location(&gran, &lat, &lon);
      add_to_shape(dbase, shape, &gran, dbf, nCols, ii, lat, lon);
      ii++;
    }
  }

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  FCLOSE(ifp);

  return 1;
}
