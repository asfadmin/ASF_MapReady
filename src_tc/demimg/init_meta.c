/****************************************************************
FUNCTION NAME: init_meta -- initializes image metadata

SYNTAX: init_meta(demfile,sarfile,&in_nl,&in_ns,&dem_nl,&dem_ns,
                  &zone,&pixsiz,upleft)

EXTERNAL ASSOCIATE: c_getddr  Reads a LAS format data descriptor record

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    ----------------------------------------------------------
    demfile     char *          name of input DEM file
    sarfile     char *          name of input SAR file
    in_nl       int *		input (SAR) number of lines
    in_ns       int *		input (SAR) number of samples
    dem_nl      int *		input (DEM) number of lines
    dem_ns	int *          input (DEM) number of samples
    zone	int *		UTM zone number of DEM
    pixsiz	double *	Pixel resolution of processing 
    upleft      double *        upper left DEM UTM coordinates

DESCRIPTION:  Read projection information from the DEM , 
              Calculate output image size and windowing,
              Set globals

RETURN VALUE: None

SPECIAL CONSIDERATIONS:
****************************************************************/
#include "asf.h"
#include "asf_las.h"
#include "ddr.h"
#include "dem.h"

void init_meta(char *demfile, char *sarfile, int *in_nl, int *in_ns,
  	       int *dem_nl, int *dem_ns, int *zone, 
	       double *pixsiz, double *upleft)
{
  struct DDR ddr;       /* DDR metedata structure        */
  double temp;
 
  /* Read DEM ddr metadata file 
   ---------------------------*/
  if (c_getddr(demfile, &ddr)!= 0)
    { printf("Unable to get ddr file for image %s\n",demfile); exit(1); }
  if (ddr.valid[1] == INVAL) ddr.zone_code = 62;
  if (ddr.valid[2] == INVAL) ddr.datum_code= 0;
  *zone = ddr.zone_code;
  *dem_ns = (int) ddr.ns;
  *dem_nl = (int) ddr.nl;
  upleft[0] = (double) ddr.upleft[0];
  upleft[1] = (double) ddr.upleft[1];
  *pixsiz = ddr.pdist_x;

  /* Read SAR ddr metadata file
   ---------------------------*/

  if (c_getddr(sarfile, &ddr)!= 0)
    { printf("Unable to get ddr file for image %s\n",sarfile); exit(1); }
  *in_nl = ddr.nl;
  *in_ns = ddr.ns;
  temp = ddr.pdist_x;
  return;
}
