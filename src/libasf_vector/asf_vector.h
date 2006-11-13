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

// Prototypes from write_shape.c
int write_point_shapefile(char *baseFile, char *pointFile);
int write_polygon_shapefile(char *baseFile, char *pointFile, char *comment);
int write_shapefile (char *dbaseFile, char *shapeFile, char *pointFile,
                     int pointType, char *comment);

// Prototypes from read_shape.c
int read_shapefile (char *baseFile, char *pointFile);

// Prototypes from mask.c
int create_mask(char *imageFile, char *shapeFile, char *maskFile);
void invert_mask(char *maskInFile, char *maskOutFile);
void read_mask(char *maskFile, unsigned char **p_mask, meta_parameters **p_meta);

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
void write_kml(char *filename);
void write_kml_overlay(char *filename);
void meta2kml(char *kml_filename, meta_parameters *meta);
void meta2kml_list(char *list, char *filename);
void point2kml_list(char *list, char *filename);
void polygon2kml_list(char *list, char *filename);
void leader2kml(char *leaderFile, char *filename);

// Prototypes from meta2shape.c
void meta2shape(char *metaFile, char *shapeFile);
void meta2shape_list(char *list, char *shapeFile);
void leader2shape(char *leaderFile, char *shapeFile);
void leader2shape_list(char *list, char *shapeFile);
void leader2kml_list(char *list, char *filename);

#endif
