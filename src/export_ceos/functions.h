#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int exit_code;

/* Function definitions*/
char *uc(char *string);
int import2asf(char *inFile, char *metaFile, char *type, char *outFile);
int projprm(char *projection, char *projkey, char *outFile, char *parameters);
int geocode(char *inFile, char *projFile, char *projkey, float pix_size, 
	    float height, char *outFile);
int corner_coords(char *inFile);
int filter(char *options, char *inFile, char *outFile);
int stats(char *inFile, float trim_fraction);
int convert2byte(char *inFile, char *outFile);
int convert2geotiff(char *inFile, char *outFile);
int convert2jpeg(char *inFile, char *outFile);
int export_ceos(char *configFile);

#endif
