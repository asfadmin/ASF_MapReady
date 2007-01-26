#ifndef _AIRSAR_IO_H_
#define _AIRSAR_IO_H_

#include "ddr.h"
#include "worgen.h"

void airsar2ddr(char *airsarname, struct DDR *ddrOut);
void airsar2meta(char *airsarname, meta_parameters *meta);
void readAirSARLine(FILE *fp,int *dest,int hb,int lb,int y,meta_parameters *meta);
char* get_airsar(char *fname, char *Header, char *Record);
void linetail(char* strIn, char *chOut);

typedef struct {
	struct DDR ddr;
	int headerBytes;
	int lineBytes;
	FILE *f_in;
	char name[1024];
} AirSAR_FILE;

AirSAR_FILE *fopenAirSAR(char *fName);

void closeAirSAR(AirSAR_FILE *in);

#endif
