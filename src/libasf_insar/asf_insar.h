#ifndef _ASF_INSAR_H_
#define _ASF_INSAR_H_

#define WINDOW_SIZE  3
#define HIST_SIZE   10

#include <fftw3.h>

typedef struct {
        double Bn;       /* Normal Baseline: perpendicular to look direction.*/
        double dBn;      /* Change in normal baseline per scene.*/
        double Bp;       /* Parallel Baseline: parallel to look direction.*/
        double dBp;      /* Change in parallel baseline per scene.*/
        double temporal; /* Temporal baseline, in fractional days.*/
} baseline;

typedef struct {
   unsigned char real;
   unsigned char imag;
} complexByte;

typedef struct {
   short int real;
   short int imag;
} complexShortInt;

typedef struct {
   int real;
   int imag;
} complexInt;

typedef struct {
   float real;
   float imag;
} complexFloat;

typedef struct {
   double real;
   double imag;
} complexDouble;

typedef fftwf_complex fcpx;


// Prototypes from complex2polar.c
int complex2polar(char *cpxfile, char *ampfile, char *phsfile);

// Prototypes from polar2complex.c
int polar2complex(char *ampName, char *phsName, char *cpxName);

// Prototypes from baseline.c
baseline find_baseline(char *file1,char *file2);
baseline read_baseline(char *fName);
void write_baseline(char *fnm, baseline b);

// Prototypes from igram_coh.c
int asf_igram_coh(int lookLine, int lookSample, int stepLine, int stepSample,
		  char *masterFile, char *slaveFile, char *outBase);


#endif
