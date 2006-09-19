#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "libasf_proj.h"
#include "proj.h"
#include "poly.h"
#include "asf_sar.h"

#define VERSION 1.0

static char * appendSuffix(const char *inFile, const char *suffix)
{
  char *suffix_pid = MALLOC(sizeof(char)*(strlen(suffix)+25));
  sprintf(suffix_pid, "%s_tmp%d", suffix, (int)getpid());

  char * ret = appendToBasename(inFile, suffix_pid);

  free(suffix_pid);
  return ret;
}

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -refine ] [ -polynomial <order> ] [ -log <logfile> ]\n"
	 "      <sarName> <mapDemName> <offset> <simAmpName> [ <sarDemName> ]\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   <sarName>        SAR image\n"
	 "   <mapDemName>     map projected reference DEM\n"
	 "   <offset>         offset file\n"
	 "   <simAmpName>     simulated amplitude image\n"
	 "   All filenames require extensions.\n");
  printf("\n"
	 "OPTIONS:\n"
	 "   -refine               applies the offset determined to improve the\n"
	 "                         the geolocation.\n"
	 "   -polynomial <order>   uses a polynomial fit of given order to\n"
	 "                         reproject DEM into SAR geometry (only applies\n"
	 "                         to non-projected imagea).\n"
	 "   <sarDemName>          reference DEM in SAR geometry (only applies to\n"
	 "                         non-projected images\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   For images in a SAR geometry (ground range or slant range) it reprojects\n"
	 "   a DEM and simulates an amplitude image into SAR \n"
	 "   geometry using a map projected reference DEM. The mapping can be done\n"
	 "   using a polynomial or a least-square (default) fitting approach.\n"
	 "   For map projected SAR images it simulates an amplitude image for the\n"
	 "   reference DEM in its map projection.\n"
	 "   Matching the real and simulated amplitude image \n"
	 "   in frequency domain is used to determine the offsets to correct\n"
	 "   the geolocation.\n");
  printf("\n"
	 "Version %.2f, ASF SAR TOOLS\n"
	 "\n",VERSION);
  exit(1);
}


int main(int argc, char *argv[])
{
  char *demGridFile, *demClipped, *demSlant, *demSimAmp;
  int order, gridSize, demWidth, demHeight;
  double maxErr;
  poly_2d *fwX, *fwY, *bwX, *bwY;
  meta_parameters *metaSAR;

  currArg=1;      /* from cla.h in asf.h, points to current argv string */
  
  printf("%s\n",date_time_stamp());
  printf("Program: asf_check_geolocation\n\n");  

  // Parse command line
  logflag = FALSE;
  //quietflag = TRUE;
  while (currArg < (argc-5)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag=TRUE;
    }
    else {printf( "\n   **Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 5) {printf("   Insufficient arguments.\n"); usage(argv[0]);}
  
  order = 5;
  gridSize = 30;

  // Fetch required arguments
  char *sarFile, *demFile, *offsetFile;
  sarFile = argv[currArg++];
  demFile = argv[currArg++];
  offsetFile = argv[currArg++];
  demSimAmp = argv[currArg++];
  demSlant = argv[currArg];

  metaSAR = meta_read(sarFile);

  /* There is scope to generalize this tool to allow the ingest of ortho-rectified
     optical data and the works. The principle remains the same, getting some data
     from a map projection into the SAR geometry. */
  
  // Image in slant range or ground range geometry
  if (metaSAR->sar->image_type=='S')
    asfPrintStatus("   Detected slant range SAR image\n");
  else if (metaSAR->sar->image_type=='G') 
    asfPrintStatus("   Detected ground range SAR image\n");
  
  // Create a grid of points to map DEM into slant range
  asfPrintStatus("   Generating %dx%d DEM grid ...\n", gridSize, gridSize);
  demGridFile = appendSuffix(sarFile, "_grid");
  create_dem_grid_ext(demFile, sarFile, demGridFile, -1, -1, gridSize, NULL);
    
  // Fit a fifth order polynomial to the grid points.
  // This polynomial is then used to extract a subset out of the reference
  // DEM.
  asfPrintStatus("   Fitting order %d polynomial to DEM ...\n", order);
  fit_poly(demGridFile, order, &maxErr, &fwX, &fwY, &bwX, &bwY);
  asfPrintStatus("   Maximum error in polynomial fit: %g.\n", maxErr);
  
  // Clip DEM using polynomial fit
  demClipped = appendSuffix(demFile, "_clip");
  demWidth = metaSAR->general->sample_count + 400;
  demHeight = metaSAR->general->line_count;
  asfPrintStatus("   Clipping DEM to %dx%d LxS using polynomial fit ...\n", 
		 demHeight, demWidth);
  remap_poly(fwX, fwY, bwX, bwY, demWidth, demHeight, demFile, demClipped);
  poly_delete(fwX);
  poly_delete(fwY);
  poly_delete(bwX);
  poly_delete(bwY);
  
  // Simulate an amplitude image
  asfPrintStatus("   Generating slant range DEM and simulating amplitude image "
		 "...\n");
  reskew_dem(sarFile, demClipped, demSlant, demSimAmp, NULL);
  
  /*
  // Determine offset 
  asfPrintStatus("\n   Determining offset ...");
  fftMatch(sarFile, demTrimSimAmp, NULL, &dx, &dy, &cert);
  asfPrintStatus("   Correlation (cert=%5.2f%%): dx = %f, dy = %f\n\n");
  
  // Still need to apply the offset
  */
  meta_free(metaSAR);
  
  exit(0);
}
