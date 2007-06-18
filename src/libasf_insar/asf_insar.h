#ifndef _ASF_INSAR_H_
#define _ASF_INSAR_H_

#define WINDOW_SIZE  3
#define HIST_SIZE   10

#include <fftw3.h>
#include <asf_complex.h>

typedef struct {
        double Bn;       /* Normal Baseline: perpendicular to look direction.*/
        double dBn;      /* Change in normal baseline per scene.*/
        double Bp;       /* Parallel Baseline: parallel to look direction.*/
        double dBp;      /* Change in parallel baseline per scene.*/
        double temporal; /* Temporal baseline, in fractional days.*/
} baseline;

typedef fftwf_complex fcpx;

// Prototypes from complex2polar.c
int complex2polar(char *cpxfile, char *ampfile, char *phsfile);

// Prototypes from polar2complex.c
int polar2complex(char *ampName, char *phsName, char *cpxName);

// Prototypes from baseline.c
baseline find_baseline(char *file1,char *file2);
baseline read_baseline(char *fName);
void write_baseline(char *fnm, baseline b);
/*Interpolate the given baseline for the (original image) line y.*/
void meta_interp_baseline(meta_parameters *sar,const baseline base,
	int y,double *Bn_y,double *Bp_y);
/*Return the expected phase of the given point for a bald earth-- elevation 0.*/
double meta_flat_phase(meta_parameters *sar,const baseline base,int y,int x);

/*Return the "phase rate"-- the number of meters of elevation per radian of phase.*/
double meta_phase_rate(meta_parameters *sar,const baseline base,int y,int x);

// Prototypes from asf_coregister.c
int average_in_doppler(char *inFileMaster, char *inFileSlave, char *outFile);
int asf_coregister(int datatype, char *coregType, char *baseName, int deskew,
		   long *p1_master_start, long *p1_slave_start, int p1_patches,
		   long *pL_master_start, long *pL_slave_start, int pL_patches,
		   long master_offset, long slave_offset, int maximum_offset,
		   int *master_patches, int *slave_patches, int *p1_range_offset, 
		   int *p1_azimuth_offset, int *pL_range_offset, 
		   int *pL_azimuth_offset, int *grid, int *fft, int power,
		   char *masterFile, char *slaveFile);

// Prototypes from asf_igram_coh.c
int asf_igram_coh(int lookLine, int lookSample, int stepLine, int stepSample,
		  char *masterFile, char *slaveFile, char *outBase,
		  float *average);

// Prototypes from asf_phase_unwrap.c
int dem2phase(char *demFile, char *baseFile, char *phaseFile);
int asf_phase_unwrap(char *algorithm, char *interferogram, char *metaFile, 
		     char *demFile, char *baseline, double filter_strength, 
		     int flattening, char *mask, char *unwrapped_phase);

// Prototypes from asf_elevation.c
int asf_elevation(char *unwrapped_phase, char *phase_mask, 
		  char *baseFile, char *seeds, char *slant_elevation,
		  char *slant_elevation_error, char *slant_amplitude, 
		  char *slant_coherence, char *ground_elevation, 
		  char *ground_elevation_error, char *ground_amplitude, 
		  char *ground_coherence);

// Prototypes from asf_baseline.c
char *base2str(int baseNo, char *base);
int asf_baseline(char *baseName, char *interferogram, char *seeds, 
		 int max_iterations, int *iterations);

// Prototypes from multilook.c
int multilook(char *inFile, char *outFile, char *metaFile, char *overlay);

// Prototypes from deramp.c
int deramp(char *inFile, char *baseFile, char *outFile, int back);

// Prototypes from phase_filter.c
int phase_filter(char *inFile, double strength, char *outFile);
int zeroify(char *inFile, char *testFile, char *outFile);

// Prototypes from escher.c
int escher(char *inFile, char *outFile);

// Prototypes from refine_baseline.c
int refine_baseline(char *phaseFile, char *seeds, char *oldBase, 
		    char *newBase);

#endif
