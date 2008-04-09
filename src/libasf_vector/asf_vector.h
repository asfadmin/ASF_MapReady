#ifndef _ASF_VECTOR_H_
#define _ASF_VECTOR_H_
/******************************************************************************
NAME:
 asf_vector.h

DESCRIPTION:
 C header with definitions & prototypes for libasf_vector library

******************************************************************************/

#include "asf_meta.h"
#include "shapefil.h"

// NOTE: The META format type applies to both ASF metadata files and all
// leader data files (.L, .par, LED- etcetera)
typedef enum {
  META=1,
  KMLFILE,
  SHAPEFILE,
  TEXT,
  POINT,
  POLYGON,
  RGPS,
  RGPS_GRID,
  RGPS_WEATHER,
  MULTIMATCH,
  URSA,
  GEOTIFF_META,
  CSV
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
  char *header;
  dbf_format_t format;
} dbf_header_t;

// Location header style
typedef enum {
  LOC_UNKNOWN=1,
  LOC_ALOS_CSV,
  LOC_LAT_LON
} loc_style_t;

// RGPS grid point definition
typedef struct {
  long cell_id;
  long grid_id;
  int ordering;
  double lat;
  double lon;
  int alive;
} grid_t;

typedef struct {
  long grid_id;
  char date[20];
  double day;
  double grid_x;
  double grid_y;
  double lat;
  double lon;
  char sourceImage[20];
  char targetImage[20];
  char stream[3];
  int quality;
  int alive;
  int done;
} grid_attr_t;

// RGPS cell definition
typedef struct {
  long cell_id;
  int nVertices;
  char date[20];
  double day;
  char sourceImage[20];
  char targetImage[20];
  char stream[3];
  double area;
  double multi_year_ice;
  double open_water;
  double incidence_angle;
  double cell_x;
  double cell_y;
  double dudx;
  double dudy;
  double dvdx;
  double dvdy;
  double dtp;
  double temperature;
  double u_wind;
  double v_wind;
  int alive;
} cell_t;

// Prototypes from convert_kml.c
void meta2kml(char *line, FILE *fp);
void point2kml(char *line, FILE *fp);
void polygon2kml(char *line, FILE *fp);
void rgps2kml(cell_t cell, double *lat, double *lon, FILE *fp);
void rgps_grid2kml(grid_attr_t grid, FILE *fp);
void shape2kml(char *inFile, FILE *fp, char *name);
void geotiff2kml(char *line, FILE *fp);

// Prototypes from kml.c
void kml_header(FILE *kml_file);
void kml_entry_with_overlay(FILE *kml_file, meta_parameters *meta,
                            char *name, char *ceos_fileame,
                            char *jpeg_dir);
void kml_entry(FILE *kml_file, meta_parameters *meta, char *name);
void kml_point_entry(FILE *kml_file, char *name, float lat, float lon);
void kml_polygon_entry(FILE *kml_file, char *name, char **id, float *lat,
               float *lon, int n);
void kml_footer(FILE *kml_file);
void write_kml(char *inFile, char *outFile, format_type_t format, int list);
void write_kml_overlay(char *filename);
void write_kml_style_keys(FILE *kml_file);

// Prototypes from convert_shape.c
void meta2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void point2shape(char *line, DBFHandle dbase, SHPHandle shape);
void polygon2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void polygon2shape_new(char *inFile, char *outFile);
void geotiff2shape(char *filename, DBFHandle dbase, SHPHandle shape, int n);
void rgps2shape(cell_t cell, double *lat, double *lon, int vertices,
                DBFHandle dbase, SHPHandle shape, int n);
void rgps_grid2shape(grid_attr_t grid, DBFHandle dbase, SHPHandle shape,
             int n);
void rgps_weather2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void multimatch2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void shape2text(char *inFile, char *outFile);
void shape2csv(char *inFile, char *outfile);

// Prototypes from shape.c
void shape_generic_init(char *inFile, dbf_header_t *dbf, int nColumns,
			format_type_t format);
void shape_init(char *inFile, format_type_t format);
int read_shape(char *inFile, char *outFile, format_type_t format, int list);
int write_shape(char *inFile, char *outFile, format_type_t format, int list);
void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape);
void close_shape(DBFHandle dbase, SHPHandle shape);
void write_esri_proj_file(char *inFile);

// Prototypes from convert_text.c
void meta2text(char *inFile, FILE *outFP);
void geotiff2text(char *inFile, FILE *outFP);
int ismetadata(char *inFile);
int isleader(char *inFile);
int ispoint(char *inFile);
int ispolygon(char *inFile);
int isshape(char *inFile);
int isgeotiff(char *inFile);
int isrgps(char *inFile);

// Prototypes from text.c
int write_text(char *inFile, char *outfile, format_type_t format, int list);

// Prototypes from generic_csv.c
FILE *csv_open(const char *filename,
               int *num_meta_cols, csv_meta_column_t **meta_column_info,
               int *num_data_cols, csv_data_column_t **data_column_info);
void csv_info(int num_meta_cols, csv_meta_column_t *meta_column_info,
              int num_data_cols, csv_data_column_t *data_column_info);
int csv_line_parse(const char *line,
                   int num_meta_cols, csv_meta_column_t *meta_column_info,
                   int num_data_cols, csv_data_column_t *data_column_info,
                   char ***column_data_o, double **lats_o, double **lons_o);
void csv_free(int num_meta_cols, char **column_data,
              double *lats, double *lons);
void csv_dump(const char *filename);
int csv2kml(const char *in_file, const char *out_file);

int csv2shape(char *inFile, char *outFile);

// Prototypes from ingest.c
void read_dbf_header_info(char *inFile, dbf_header_t **dbf, int *nCols, 
			  int *nLatLons, loc_style_t *locStyle);
int is_lat_name(const char *header);
int is_lon_name(const char *header);

#endif
