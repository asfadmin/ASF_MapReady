#ifndef _FLAGERROR_H
#define _FLAGERROR_H

static char sccsid_flagerror_h[] = 
    "@(#)flagerror.h	1.2 96/04/09 20:29:07";

/* flagerror.h
 * 12/18/89
 * This file contains the definitions for the ERROR code for
 * ASF parameter setup.
 */

#define OVRFL_PATH 0x0002  /* (RMC) no path fits within first 100 range
                                    cells */
#define OVRFL_A    0x0004  /* (RMC) coefficient A overflow */
#define OVRFL_2D   0x0008  /* (RMC) twod overflow */
#define OVRFL_CRS  0x0010  /* (RMC) coarse overflow */
#define OVRFL_FIN  0x0020  /* (RMC) fine overflow */

#endif /* ! _FLAGERROR_H */
