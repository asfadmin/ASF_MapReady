/*****************************************************************
FUNCTION NAME: init_support -- initailizes DEM info and globals

SYNTAX:  init_support(line_off, samp_off, zone_code, file)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------------
    line_off    double *        window line offset for input SAR
    samp_off    double *        window sample offset for input SAR
    zone_code   int *          UTM zone code for input DEM
    file        char *          Name of DEM file to access

GLOBALS ACCESSED:
    NAME:          ACCESS:      PURPOSE:
    --------------------------------------------------------------
    in_block_sl    update       set simulation start line
    in_block_ss    update       set simulation start sample
    in_block_nl    update       set simulation number of lines
    in_block_ns    update       set simulation number of samples
    dem_ns         update       set number of samples in DEM
    dem_nl         update       set number of lines in DEM
    upleft         update       DEM upperleft corner UTM coordinate

DESCRIPTION:  Read DEM DDR information & determine simulation size
    Extract DEM information dem_ns, dem_nl, upleft, and zone_code
    from the DEMs .ddr metadata file.  Calculate offsets for windowing.

RETURN VALUE: All four of the parameters and 7 globals initialized

SPECIAL CONSIDERATIONS:
******************************************************************/
/*******   GLOBAL VARIABLES DECLARED IN SARSIM MAIN  **********/
extern int    in_block_sl;  /* simulation start line         */
extern int    in_block_ss;  /* simulation start sample       */
extern int    in_block_nl;  /* simulation number of lines    */
extern int    in_block_ns;  /* simulation number of samples  */
extern int    dem_ns;       /* number of samples in dem line */
extern int    dem_nl;       /* number of lines in dem file   */
extern double  upleft[2];    /* Upper left coordinates of DEM */
/**************************************************************/
#include "asf.h"
#include "ddr.h"
#include "sarsim.h"
#include "sarmodel.h"
#define  DPR	57.2957795 

int init_support(
	double  *line_off, double *samp_off,int    *zone_code,char    *file)
{
  struct DDR ddr;                 /* DDR structure                  */
  double pc[8];                   /* Proj coords of 4 image corners */
  double max_line, max_samp;
  double min_line, min_samp;
  double lat, lon, line, samp;
  double lat1,lon1,z1,lat2,lon2,z2,
         lat3,lon3,z3,lat4,lon4,z4;
  double intensity, sini, mask;
  int    viewflg = 0;
  int   i,j,k;

  /* Read image size and projection information from the DEM
   ---------------------------------------------------------*/
  c_getddr(file, &ddr);
  if (ddr.valid[1] == INVAL) ddr.zone_code = 62;
  if (ddr.valid[2] == INVAL) ddr.datum_code= 0;
  dem_ns = (int) ddr.ns;
  dem_nl = (int) ddr.nl;
  upleft[0] = (double) ddr.upleft[0];
  upleft[1] = (double) ddr.upleft[1];
  *zone_code = (int) ddr.zone_code;
  utm_init(*zone_code);

  /*
  printf("Reading DEM information:\n");
  printf("       dem_ns    : %i\n",dem_ns);
  printf("       dem_nl    : %i\n",dem_nl);
  printf("       UTM Zone  : %i\n",*zone_code);
  printf("       upleft[0] : %f\n",upleft[0]);
  printf("       upleft[1] : %f\n",upleft[1]);
  printf("\n\n");
  */

  /* Setup coeffs mapping line, sample coordiantes to projection coordinates
   ------------------------------------------------------------------------*/
  pc[0] = ddr.upleft[0];  pc[1] = ddr.upleft[1];
  pc[2] = ddr.loleft[0];  pc[3] = ddr.loleft[1];
  pc[4] = ddr.upright[0]; pc[5] = ddr.upright[1];
  pc[6] = ddr.loright[0]; pc[7] = ddr.loright[1];
  min_line = min_samp = 999999.0;
  max_line = max_samp = -999999.0;
  lat1 = lon1 = z1 = 0.0; lat2 = lon2 = z2 = 0.0;
  lat3 = lon3 = z3 = 0.0; lat4 = lon4 = z4 = 0.0;
  /* printf("    lat,      lon     <->   utm coordinates\n"); */

  for (i=0, k=0, j=1; i<4; i++, j+=2, k+=2)
   {
    tm_inverse(pc[j],pc[k],&lon,&lat);
    /* printf("%f, %f\t %f, %f\n",lat*DPR,lon*DPR,pc[j],pc[k]); */
    sar_sim(lat, lon, 0.0, lat1, lon1, z1 ,lat2 , lon2 , z2 ,
            lat3 , lon3 , z3 , lat4, lon4 , z4 ,viewflg,
            &intensity , &sini , &mask , &line , &samp);
    if (line > max_line) max_line = line;
    if (line < min_line) min_line = line;
    if (samp > max_samp) max_samp = samp;
    if (samp < min_samp) min_samp = samp;
    sar_sim(lat, lon, 6500.0, lat1, lon1, z1 ,lat2 , lon2 , z2 ,
            lat3 , lon3 , z3 , lat4, lon4 , z4 ,viewflg,
            &intensity , &sini , &mask , &line , &samp);
    if (line > max_line) max_line = line;
    if (line < min_line) min_line = line;
    if (samp > max_samp) max_samp = samp;
    if (samp < min_samp) min_samp = samp;
   }
  if (min_line < 1) min_line = 1;
  if (min_samp < 1) min_samp = 1;
  in_block_sl = (int) min_line;
  in_block_ss = (int) min_samp;
  in_block_nl = (int) ((max_line - min_line) + 1.0);
  in_block_ns = (int) ((max_samp - min_samp) + 1.0);
  *line_off = in_block_sl - 1.0;
  *samp_off = in_block_ss - 1.0;

  printf("in_block: sl,ss = %i,%i nl,ns = %i,%i\n",
       in_block_sl,in_block_ss,in_block_nl,in_block_ns); 
  return(0);
}

