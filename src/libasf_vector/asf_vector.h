#ifndef _ASF_VECTOR_H_
#define _ASF_VECTOR_H_
/******************************************************************************
NAME:
 asf_vector.h

DESCRIPTION:
 C header with definitions & prototypes for libasf_vector library

******************************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "shapefil.h"
#include "asf_import.h"
#include "terrasar.h"
#include "radarsat2.h"
#include "smap.h"
#include "asf_license.h"
#include "xml_util.h"

// NOTE: The META format type applies to both ASF metadata files and all
// leader data files (.L, .par, LED- etcetera)
typedef enum {
  META=1,
  LEADER,
  KMLFILE,
  SHAPEFILE,
  TEXT,
  POINT,
  POLYGON,
  SMAP_BOUNDARY,
  SENTINEL_META,
  RGPS,
  RGPS_GRID,
  RGPS_WEATHER,
  GRANULE_COUNT,
  GRANULE_LIST,
  GRANULE_DETAILS_A3,
  GRANULE_DETAILS,
  GRANULE,
  FOOT_PRINT,
  MULTIMATCH,
  URSA,
  DATAPOOL,
  GEOTIFF_META,
  AUIG,
  HAP,
  TERRASAR_META,
  STF_META,
  CSV,
  LATLON,
  CUSTOM_FORMAT
} format_type_t;

typedef enum {
  CSV_UNKNOWN=0,
  CSV_STRING,
  CSV_DOUBLE,
  CSV_INTEGER,
  CSV_LOGICAL,
  CSV_DATE
} column_data_type_t;

typedef struct {
  char column_name[64];
  column_data_type_t data_type;
  int column_number;
} csv_meta_column_t;

typedef struct {
  char column_name[64];
  int column_number;
  int is_lat; // TRUE for lat, FALSE for lon
} csv_data_column_t;

// Database fields
typedef enum {
  DBF_STRING=1,
  DBF_DOUBLE,
  DBF_INTEGER
} dbf_format_t;

// Generic CSV structure
typedef struct {
  char *meta;
  char *shape;
  char *definition;
  int length;
  int decimals;
  dbf_format_t format;
  char *sValue;
  double fValue;
  int nValue;
  int column;
  int visible;
} dbf_header_t;

// RGPS grid
typedef struct {
  char image_id[20];
  int gpid;
  int obs_year;
  double obs_time;
  int birth_year;
  double birth_time;
  int death_year;
  double death_time;
  double x;
  double y;
} rgps_grid_t;

// RGPS cell
typedef struct {
  int cell_id;
  int grid_id;
  int order;
  int birth_year;
  double birth_time;
  int death_year;
  double death_time;
} rgps_cell_t;

// Prototype from config.c
typedef struct
{
  char comment[255];             // first line for comments
  char directory[1024];          // default directory
  char input_file[512];          // input file name
  char output_file[512];         // output file name
  char input_format[25];         // input format
  char output_format[25];        // output format
  char overlay[512];             // overlay file name
  double north;                  // northern extent of the overlay
  double south;                  // southern extent of the overlay
  double east;                   // eastern extent of the overlay
  double west;                   // western extent of the overlay
  int transparency;              // transparency of the overlay
  int list;                      // list of files flag
  char boundary[25];             // polygon/line
  char altitude[25];             // ref: clampToGround/relativeToGround/absolute
  int height;                    // height
  int range;                     // range
  int width;                     // width of boundary line
  char color[25];                // color of boundary line
  int short_config;              // short configuration file flag
  int nosplit;                   // no splitting of vectors at the dateline
  double wrapdateline;           // tolerance for wrapping around the dateline
                                 // default of -1 means no wrapping
  int debug;                     // debugging flag
} c2v_config;

int init_c2v_config(char *configFile);
c2v_config *init_fill_c2v_config();
c2v_config *read_c2v_config(char *configFile);
int write_c2v_config(char *configFile, c2v_config *cfg);

// Prototypes from convert2vector.c
int convert2vector(c2v_config *cfg);

// Prototypes from utils.c
int ismetadata(char *inFile);
int isleader(char *inFile);
int ispoint(char *inFile);
int ispolygon(char *inFile);
int isshape(char *inFile);
int isgeotiff(char *inFile);
int isrgps(char *inFile);
int isparfile(char *inFile);
int isterrasar(char *inFile);
void split_polygon(double *lat, double *lon, int nCoords, 
  int *start, double *mLat, double *mLon, double tolerance);

// Prototypes from header.c
char *get_column(char *line, int column);
int get_number_columns(char *line);
int read_header_config(const char *format, dbf_header_t **dbf, int *nAttr, 
  char *shape_type);

// Prototypes from kml.c
void kml_header(FILE *kml_file);
void kml_footer(FILE *kml_file);
void write_kml_style_keys(FILE *kml_file);
const char *altitude_mode();
void write_kml_placemark(FILE *kml_file, char *name, double center_lat,
  double center_lon, char *png_filename, dbf_header_t *dbf, int nAttr,
  double *lat, double *lon, int nCoords, c2v_config *cfg);
int point2kml(char *inFile, char *outFile);
void csv2kml(char *inFile, char *outFile, char *format, c2v_config *cfg);
int convert2kml(char *inFile, char *outFile, char *format, int list, 
  c2v_config *cfg);

// Prototypes from shape.c
void shapefile_init(char *inFile, char *outFile, char *format, 
  meta_parameters *meta);
void shape_init(char *inFile, format_type_t format);
void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape);
void close_shape(DBFHandle dbase, SHPHandle shape);
int latlon2shape(char *inFile, char *outFile);
int smap2shape(char *inFile, char *outFile);
int sentinel2shape(char *inFile, char *outFile);
int point2shape(char *inFile, char *outFile);
void csv2shape(char *inFile, char *format, char *outFile, c2v_config *cfg);
void shape2latlon(char *infile, double **latArray, double **lonArray, 
  int **startArray, int *nPoly, int *nCoords);
int convert2shape(char *inFile, char *outFile, char *format, int list,
  c2v_config *cfg);

// Prototypes from vector.c
meta_parameters *meta2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords);
void geotiff2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords);
void polygon2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords);
void smap2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords);
void sentinel2vector(char *inFile, char *type, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords);
void satellite2vector(char *line, char *type, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords);

#endif
