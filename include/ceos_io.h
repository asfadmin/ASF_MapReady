/*Ceos File Input and Output
*/

#ifndef _CEOS_IO_H_
#define _CEOS_IO_H_

#include "ddr.h"

/*Ceos2ddr converts a CEOS file into
a DDR.  If headerBytes and lineBytes are not
NULL, ceos2ddr returns these values.*/
void ceos2ddr(char *ceosName,struct DDR *ddrOut,
	int *headerBytes,int *lineBytes);


typedef struct {
	struct DDR ddr;/*This DDR is created by ceos2ddr*/
	int headerBytes;
	int lineBytes;
	FILE *f_in;
	char name[1024];/*Name of image data file.*/
} CEOS_FILE;


CEOS_FILE *fopenCeos(char *fName);
void readCeosLine(int *dest,int y,CEOS_FILE *in);
void closeCeos(CEOS_FILE *in);

#endif
