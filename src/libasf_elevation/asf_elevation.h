#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_sar.h"

#define FLOAT_EQUALS_ZERO(X) (X<0.000000000001 && X>-0.000000000001)

// Prototype
void asf_elevation(char *logFile, char *unwrapped_phase, char *phase_mask, 
		   char *baseFile, char *seeds, char *slant_amplitude, 
		   char *slant_coherence, char *ground_elevation, 
		   char *ground_elevation_error, char *ground_amplitude, 
		   char *ground_coherence);
