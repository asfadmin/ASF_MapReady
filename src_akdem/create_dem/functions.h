#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int exit_code;

/* Function definitions*/
int lz2raw_flywheel(char *inFile, char *outFile, char *prcOrbits, int prcFlag, double lat_begin, double lat_end);
int ceos2raw(char *inFile, char *outFile);
int trim_slc(char *inFile, char *outFile, int line, int sample, int length, int width);
int avg_in_dop(char *inFile1, char *inFile2, char *outFile1);
int doppler_per_patch(char *parFile1, char *parFile2, char *metaFile1, char *metaFile2, char *deltaFile, 
			char *outFile1, char *outFile2);
int aisp(char *options, int startLineNum, int numPatches, char *inFile, char *outFile);
int paisp(char *options, int startLineNum, int numPatches, int numProcessors, char *inFile, char *outFile);
int c2p(char *inFile, char *outFile);
int cpx_autofilter(char *inFile1, char *inFile2, char *outFile1, char *outFile2);
int pcpx_autofilter(char *inFile1, char *inFile2, char *outFile1, char *outFile2, int procs);
int resolve(char *inFile1, char *inFile2, char *outFile);
int fico(char *inFile1, char *inFile2, char *inCtrlFile, char *outFile, int gridSize, int useFFT);
int fit_line(char *inFile, char *outFile);
int calc_deltas(char *inFile1, char *inFile2, int lineDiff, char *outFile);
int igram(char *inFile1, char *inFile2, char *outFile);	
int pigram(char *inFile1, char *inFile2, char *outFile, int numProcessors);
int coh(char *inFile1, char *inFile2, char *outFile);
int pcoh(char *inFile1, char *inFile2, char *outFile, int numProcessors);
int ml(char *inFile, char *outFile, char *metaFile);
int pml(char *inFile, char *outFile ,int numProcessors, char *metaFile);
int amp2img(char *inFile, char *outFile, int nLooks, int nSmooth);
int create_dem_grid(char *demFile, char *sarFile, char *ceosFile, char *gridFile);
int fit_plane(char *inFile, char *outFile, double fraction);
int fit_warp(char *inFile1, char *inFile2, char *outFile);
int remap(char *inFile, char *outFile, char *options);
int make_ddr(char *outFile, int nl, int ns, char *type);
int reskew_dem(char *demFile, char *metaFile, char *outFile1, char *outFile2);
int trim(char *inFile, char *outFile, int sX, int sY, int nl, int ns);
int fftMatch(char *inFile1, char *inFile2, char *outFile);
int dem2phase(char *demFile, char *metaFile, char *baseFile, char *phaseFile);
int dem2seeds(char *demFile, char *ampFile, char *seedsFile, int fft);
int deramp(char *demFile, char *metaFile, char *baseFile, char *outFile, int back);
int snaphu(char *snaphu_version, char *phaseFile, char *ampFile, char *pwrFile1, char *pwrFile2, char *config, char *outFile, 
                        int nAzimuth, int nRange, int nOverAzi, int nOverRng, int nProcs);
int refine_base(char *phaseFile, char *seeds, char *metaFile, char *oldBase, char *newBase);
int las_op(char *outFile,char *operation);
int las2ppm(char *inFile, char *outFile);
int phase_filter(char *inFile, double strength, char *outFile);
int zeroify(char *phaseFile1, char *phaseFile2, char *outFile);
int escher(char *inFile, char *outFile);
int elev(char *phaseFile, char *baseFile, char *metaFile, char *outFile, char *seeds);
int eleverr(char *cohFile, char *baseFile, char *metaFile, char *maskFile, char *outFile);
int deskew_dem(char *inFile1, char *metaFile, char *outFile, char *inFile2, int radiometric);
int projprm(char *projection, char *projkey, char *outFile, char *options);
int geocode(char *metaFile, char *inFile, char *projFile, char *projkey, int pix_size, char *outFile);

#define AISP_VALID_PATCH_LENGTH 3300

#endif
