/*****************************************************************************
NAME: GET_TIEPOINTS

PURPOSE:  To add the coordinate information into the geoTIFF image.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "ddr.h"
#include "geotiffio.h"
#include "protos.h"

void get_tiepoints 
(
struct DDR ddr,              /* I: LAS image file descriptor record     */
double tiepoints[][6],       /* O: coordinate array for GEOTIEPOINTS    */
long *size                   /* O: number of elements in array          */
)
{

  /* Upper left corner of image
     -------------------------- */
  tiepoints[0][0] = 0;
  tiepoints[0][1] = 0;
  tiepoints[0][2] = 0;
  /* need the actual corner of the image, not the center of the 
     corner pixel
     ---------------------------------------------------------- */
  tiepoints[0][3] = ddr.upleft[1] +
                 (ddr.upleft[1] - ddr.upright[1]) / (ddr.ns - 1) / 2;
  tiepoints[0][4] = ddr.upleft[0] +
                 (ddr.upleft[0] - ddr.loleft[0]) / (ddr.nl - 1) / 2;
  tiepoints[0][5] = 0.0;

  /* Upper right corner of the image
     ------------------------------- */
  tiepoints[1][0] = ddr.ns;
  tiepoints[1][1] = 0;
  tiepoints[1][2] = 0;
  tiepoints[1][3] = ddr.upright[1] -
                 (ddr.upleft[1] - ddr.upright[1]) / (ddr.ns - 1) / 2;
  tiepoints[1][4] = ddr.upright[0] +
                 (ddr.upright[0] - ddr.loright[0]) / (ddr.nl - 1) / 2;
  tiepoints[1][5] = 0.0;

  /* Lower left corner of the image
     ------------------------------ */
  tiepoints[2][0] = 0;
  tiepoints[2][1] = ddr.nl;
  tiepoints[2][2] = 0;
  tiepoints[2][3] = ddr.loleft[1] +
                 (ddr.loleft[1] - ddr.loright[1]) / (ddr.ns - 1) / 2;
  tiepoints[2][4] = ddr.loleft[0] -
                 (ddr.upleft[0] - ddr.loleft[0]) / (ddr.nl - 1) / 2;
  tiepoints[2][5] = 0.0;

  /* Lower right corner of the image
     ------------------------------- */
  tiepoints[3][0] = ddr.ns;
  tiepoints[3][1] = ddr.nl;
  tiepoints[3][2] = 0;
  tiepoints[3][3] = ddr.loright[1] -
                 (ddr.loleft[1] - ddr.loright[1]) / (ddr.ns - 1) / 2;
  tiepoints[3][4] = ddr.loright[0] -
                 (ddr.upright[0] - ddr.loright[0]) / (ddr.nl - 1) / 2;
  tiepoints[3][5] = 0.0;

  *size = 24;

  /* (If the functionality to add points from a tiepoint file is added 
     to the module, this is where it should be placed)
     ----------------------------------------------------------------- */
}
