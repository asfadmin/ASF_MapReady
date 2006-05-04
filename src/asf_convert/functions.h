#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "asf_convert.h"

int exit_code;

/* Function definitions*/
char *str2upper(char *string);
int asf_import(char *inFile, char *outFile, char *format, char *radiometry,
	       char *prcOrbits, double lat_begin, double lat_end);
int ardop(char *options, char *inFile, char *outFile);
int image_stats(char *inFile, char *outFile, char *values, int bins, 
		double interval);
int detect_cr(char *inFile, char *crFile, char *outFile, int chips, int text);
int asf_terrcorr(char *options, char *inFile, char *demFile, char *outFile);
int asf_geocode(char *options, char *inFile, char *outFile);
int asf_export(char *options, char *inFile, char *outFile);
int call_asf_convert(char *configFile);

#endif
