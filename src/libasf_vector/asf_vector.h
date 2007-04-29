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
  URSA,
  BASELINE
} format_type_t;

// Prototypes from convert_kml.c
void meta2kml(char *line, FILE *fp);
void point2kml(char *line, FILE *fp);
void polygon2kml(char *line, FILE *fp, char *name);
void rgps2kml(char *line, FILE *fp, char *name);
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

// Prototypes from convert_shape.c
void meta2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void point2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void polygon2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void rgps2shape(char *line, DBFHandle dbase, SHPHandle shape, int n);
void shape2text(char *inFile, FILE *fp);

// Prototypes from shape.c
void shape_init(char *inFile, format_type_t format);
int read_shape(char *inFile, char *outFile, format_type_t format, int list);
int write_shape(char *inFile, char *outFile, format_type_t format, int list);
void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape);
void close_shape(DBFHandle dbase, SHPHandle shape);

#endif
