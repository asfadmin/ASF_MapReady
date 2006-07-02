#include "asf.h"
#include "asf_meta.h"
#include "asf_complex.h"
#include "asf_raster.h"

#define VERSION      1.0
#define WINDOW_SIZE  3
#define HIST_SIZE   10

/* Macro to figure amplitude */
#define AMP(cpx) sqrt((cpx).real*(cpx).real + (cpx).imag*(cpx).imag)

// Prototype
int asf_igram_coh(int lookLine, int lookSample, int stepLine, int stepSample,
		  char *masterFile, char *slaveFile, char *outBase);
