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
  URSA
} format_type_t;

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
void polygon2kml(char *line, FILE *fp, char *name);
void rgps2kml(cell_t cell, double *lat, double *lon, FILE *fp);
void rgps_grid2kml(grid_attr_t grid, FILE *fp);
void shape2kml(char *inFile, FILE *fp, char *name);

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
void point2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void polygon2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void rgps2shape(cell_t cell, double *lat, double *lon, int vertices,
                DBFHandle dbase, SHPHandle shape, int n);
void rgps_grid2shape(grid_attr_t grid, DBFHandle dbase, SHPHandle shape, 
		     int n);
void rgps_weather2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void shape2text(char *inFile, FILE *fp);

// Prototypes from shape.c
void shape_init(char *inFile, format_type_t format);
int read_shape(char *inFile, char *outFile, format_type_t format, int list);
int write_shape(char *inFile, char *outFile, format_type_t format, int list);
void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape);
void close_shape(DBFHandle dbase, SHPHandle shape);
void write_esri_proj_file(char *inFile);

#endif
