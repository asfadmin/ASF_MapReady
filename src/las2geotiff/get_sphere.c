/****************************************************************************
NAME: GET_SPHERE

PURPOSE:  Returns the value of the semimajor or semiminor axis for a
          sphere (both are the same for a sphere)

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/
#include "ddr.h"
#include "fhdr.h"
#include "protos.h"

SETSPHEREDEF (spheredef);     /* Spheroid keywords, names, and axes */

void get_sphere
(
    struct DDR ddr,            /* I: LAS image file descriptor record    */
    double *sphere             /* O: radius of sphere                    */
)

{
  /* Projection coeficients should be valid, but check anyway
     -------------------------------------------------------- */
  if (ddr.valid[3])
  {
    if (ddr.proj_coef[0] == 0)
    {
      *sphere = spheredef[ddr.datum_code].smjaxis;
    }
    else
    {
      /* A radius is given so use it
         --------------------------- */
      *sphere = ddr.proj_coef[0];
    }
  }
  else
  {
    printf("\nLAS image does not contain valid projection coefficients\n");
    *sphere = 0.0;
  }
}
