#include "asf_meta.h"
#include "libasf_proj.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <input> <output> <grid line count>\n", name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   input:       Name of input file (ASF internal format)\n"
	 "   output:      Name of output file (ASF internal format)\n"
	 "   grid count:  Number of grid lines to be produced\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   Replaces a satellite image with a mapping grid.\n");
  printf("\n"
	 "Version: %.2f, ASF Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  // Allocate some memory
  char *inFile = (char *) MALLOC(sizeof(char)*512);
  char *outFile = (char *) MALLOC(sizeof(char)*512);

  // Parse command line
  if (argc < 3) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }  
  strcpy(inFile, argv[1]);
  strcpy(outFile, argv[2]);
  int grid_line_count = atoi(argv[3]);

  asfSplashScreen (argc, argv);

  // Deal with metadata
  meta_parameters *metaIn = meta_read(inFile);
  meta_parameters *metaOut = meta_read(inFile);
  int nl = metaIn->general->line_count;
  int ns = metaIn->general->sample_count;
  metaOut->general->data_type = BYTE;
  meta_write(metaOut, outFile);

  // Replace image with grid
  float on_grid = 0.0;
  float off_grid = 255.0;
  float *outLine = (float *) MALLOC(sizeof(float)*ns);
  float *inLine = (float *) MALLOC(sizeof(float)*ns);

  FILE *fpIn = FOPEN(appendExt(inFile, ".img"), "rb");
  FILE *fpOut = FOPEN(appendExt(outFile, ".img"), "wb");
  int ii, kk;;
  for (ii=0; ii<nl; ii++) {
    get_float_line(fpIn, metaIn, ii, inLine);
    for (kk=0; kk<ns; kk++) {
      if (ii == 0 || ii == nl -1 || ii % (ns/grid_line_count) == 0)
	outLine[kk] = on_grid;
      else if (kk % (ns/grid_line_count) == 0)
	outLine[kk] = on_grid;
      else
	outLine[kk] = off_grid;
    }
    outLine[0] = outLine[ns-1] = on_grid;
    put_float_line(fpOut, metaOut, ii, outLine);
    asfLineMeter(ii, nl);
  }

  // Clean up
  FREE(inFile);
  FREE(outFile);
  meta_free(metaIn);
  meta_free(metaOut);

  exit(0);
}
