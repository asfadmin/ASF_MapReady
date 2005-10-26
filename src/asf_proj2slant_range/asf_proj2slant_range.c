#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_reporting.h"
#include "asf_raster.h"
#include "libasf_proj.h"
#include "proj.h"

#define VERSION 1.0

void usage(char *name)
{
 printf("\n"
        "USAGE:\n"
        "   %s <sarName> <mapDemName> <slantDemName> <simAmpName>\n",name);
 printf("\n"
        "REQUIRED ARGUMENTS:\n"
        "   <sarName>        slant range SAR image\n"
        "   <mapDemName>     map projected reference DEM\n"
	"   <slantDemName>   reference DEM in slant range geometry\n"
        "   <simAmpName>     simulated amplitude image\n"
	"   All filenames require extensions.\n");
   printf("\n"
        "DESCRIPTION:\n"
        "   Reprojects a DEM and simulates an amplitude image in slant range geometry\n"
	"   using a map projected reference DEM.\n");
 printf("\n"
        "Version %.2f, ASF SAR TOOLS\n"
        "\n",VERSION);
 exit(1);
}


int main(int argc, char *argv[])
{
  char sarName[255]="", mapDemName[255]="", slantDemName[255]="", simAmpName[255]="";
  char gridName[255]="", sarByteName[255]="", simByteName[255]="", cmd[255];
  int ii, nSamples, nLines;
  float *float_data;
  unsigned char *byte_data;
  meta_parameters *metaSAR, *metaDEM;
  FILE *fpIn, *fpOut;

  logflag=0;
  quietflag=1;
  currArg=1;      /* from cla.h in asf.h, points to current argv string */
  
  printf("%s\n",date_time_stamp());
  printf("Program: asf_proj2slant_range\n\n");  

  if ((argc-currArg) < 4) {printf("   Insufficient arguments.\n"); usage(argv[0]);}

  /* Fetch required arguments */
  strcpy(sarName, argv[argc - 4]);
  strcpy(mapDemName, argv[argc - 3]);
  strcpy(slantDemName, argv[argc - 2]);
  strcpy(simAmpName, argv[argc - 1]);

  // Converting slant range image to byte.
  metaSAR = meta_read(sarName);
  nSamples = metaSAR->general->sample_count;
  nLines = metaSAR->general->line_count;
  create_name(sarByteName, sarName, "_byte.img");
  float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
  byte_data = (unsigned char *) MALLOC(sizeof(char)*nLines*nSamples);
  fpIn = FOPEN(sarName,"rb");
  fpOut = FOPEN(sarByteName, "wb");
  get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
  /* Chose the two sigma method (SIGMA) for conversion to byte because the minimum/
     maximum stretch (MINMAX) sometimes leads to dark images. Interestingly, the
     confidence level of sigma versus minmax for the test case was lower. -RG */
  byte_data = floats_to_bytes(float_data, nLines*nSamples, NAN, SIGMA);
  FWRITE(byte_data, 1, nLines*nSamples, fpOut);
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(float_data);
  FREE(byte_data);
  metaSAR->general->data_type = BYTE;
  meta_write(metaSAR, sarByteName);
  meta_free(metaSAR);

  /* There is scope to generalize this tool to allow the ingest of ortho-rectified
     optical data and the works. The principle remains the same, getting some data
     from a map projection into the SAR slant range geometry. */

  // Creating a grid of points to map DEM into slant range
  create_name(gridName, mapDemName, ".grid");
  create_dem_grid(mapDemName, sarByteName, gridName);

  // Generate mapping function for remap
  fit_poly(gridName, 5, "poly.5");

  // Cutting out the DEM subset we need
  /* Running remap with 5th order polynomial fit is dreadfully slow. We might want
     do something about that in future. -RG */
  sprintf(cmd, "remap -width %i -height %i -poly poly.5 -float %s dem_big.dem", 
          nSamples, nLines, mapDemName);
  system(cmd);

  // Mapping DEM from projection into slant range
  reskew_dem(sarName, "dem_big.dem", slantDemName, simAmpName);

  // Converting simulated amplitude image to byte
  metaSAR = meta_read(sarName);
  create_name(simByteName, simAmpName, "_byte.img");
  float_data = (float *) MALLOC(sizeof(float)*nLines*nSamples);
  byte_data = (unsigned char *) MALLOC(sizeof(char)*nLines*nSamples);
  fpIn = FOPEN(simAmpName,"rb");
  fpOut = FOPEN(simByteName, "wb");
  get_float_lines(fpIn, metaSAR, 0, nLines, float_data);
  /* Same as initial byte conversion. MINMAX is the alternative. */
  byte_data = floats_to_bytes(float_data, nLines*nSamples, NAN, SIGMA);
  FWRITE(byte_data, 1, nLines*nSamples, fpOut);
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(float_data);
  FREE(byte_data);
  metaSAR->general->data_type = BYTE;
  meta_write(metaSAR, simByteName);
  meta_free(metaSAR);

  // Determine the offset to evaluate quality 
  fftMatch(sarByteName, simByteName, NULL, "offset");

  exit(0);
}
