#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "asf_convert.h"

int exit_code;

/* Function definitions*/
char *str2upper(char *string);
int asf_import(char *inFile, char *metaFile, char *type, char *outFile);
int projprm(char *projection, char *projkey, char *outFile, char *parameters);
int geocode(char *inFile, char *projFile, char *projkey, float pix_size,
            float height, char *outFile);
int corner_coords(char *inFile);
int filter(char *options, char *inFile, char *outFile);
int asf_export(int format, char *inFile, char *outFile);
int asf_convert(char *configFile);

#endif
