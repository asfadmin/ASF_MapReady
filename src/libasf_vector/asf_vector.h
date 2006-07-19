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
void read_mask(char *maskFile, unsigned char *mask, meta_parameters *meta);

// Prototypes from kml.c
void kml_header(FILE *kml_file);
void kml_entry(FILE *kml_file, meta_parameters *meta, char *name);
void kml_footer(FILE *kml_file);
void write_kml(char *filename);
void meta2kml(char *kml_filename, meta_parameters *meta);

// Prototypes from meta2shape.c
void meta2shape(char *metaFile, char *shapeFile);
void meta2shape_list(char *list, char *shapeFile);


#endif
