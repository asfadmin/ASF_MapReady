/*****************************************************************************
FUNCTION:   			rotcor

PURPOSE:    Calculates projection coordinates of a rotated projection space
	    from an input image window specification.

CALLING SEQUENCE:  status = c_rotcor(proj, image, wind, proj_wind);

     parameter       dtype       I/O        Comments
     ---------       -----       ---        --------
      proj           double       I         Contains projection coordinates
					    of the image corners.  Given in
			  		    y,x order, ul, ll, ur, lr.
      image          int         I         Contains image coordintes (line,
					    sample) of the image corners.
					    Given in ul, ll, ur, lr.
      wind	     double	  I	    Window specification:  SL,SS,NL,NS
      proj_wind	     double       O         Contains projection coordinates
					    of the window image corners.  Given
					    in y,x order, ul, ll, ur, lr.
      status	     int	 Return     Routine returns E_SUCC for a
					    successful computation or E_FAIL
					    if a fatal error is encountered.

PROGRAM HISTORY:
  Version	Date       Author         Code/Contr     Request
  -------	----	   ------         ----------     -------
    1.0         5/88       D. Steinwand   EROS-CSB       DDR development
    1.1		8/88	   B. Ailts	  EROS-CSB	 Make application 
							   callable
    1.2	       12/88	   D. Steinwand	  EROS-CSB	 Moved main processing
							 to generic routine
							 c_rtcoef
    1.3		1/89	   D. Steinwand   EROS-CSB	 Misc clean-up
    1.4		12/90	   B. Ailtsq      EROS-CSB       Updated error messages

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	C callable support routine

PROJECT:  LAS

ALGORITHM (FLOW):

    Call c_rtcoef to calculate first order transformation coefficients to
	map line, sample image coordinates to rotated projection coordinates
    Calculate projection coordinates for each corner of the new image as
     specified by the image window.
    Return to the calling program.
*****************************************************************************/
#include "las.h"

lasErr FUNCTION c_rotcor(const double proj[8], const int image[8], double window[4],double proj_wind[8])
{
double coef[6];			/* Transformation coefficients */
double line,samp;		/* Temp var...current image coordinate */
int err_flag;			/* TRUE/FALSE error print flag */

/*--------------------------------------------------------------------------
  Calculate first order transformation coefficients
 --------------------------------------------------------------------------*/
err_flag = TRUE;
if (c_rtcoef(proj, image, coef, &err_flag) != E_SUCC)
   {
   c_errmsg("Error returned from rtcoef","rotcor-call",NON_FATAL);
   return(E_FAIL);
   }

/*--------------------------------------------------------------------------
  Calculate Projection Coordinates for the corners of the window 
 --------------------------------------------------------------------------*/
line = window[0];
samp = window[1];
proj_wind[0] = (coef[0] * line)+ (coef[1] * samp) + coef[2];
proj_wind[1] = (coef[3] * line)+ (coef[4] * samp) + coef[5];
line = (window[0] + window[2]) - 1;
samp = window[1];
proj_wind[2] = (coef[0] * line)+ (coef[1] * samp) + coef[2];
proj_wind[3] = (coef[3] * line)+ (coef[4] * samp) + coef[5];
line = window[0];
samp = (window[1] + window[3]) - 1;
proj_wind[4] = (coef[0] * line)+ (coef[1] * samp) + coef[2];
proj_wind[5] = (coef[3] * line)+ (coef[4] * samp) + coef[5];
line = (window[0] + window[2]) - 1;
samp = (window[1] + window[3]) - 1;
proj_wind[6] = (coef[0] * line)+ (coef[1] * samp) + coef[2];
proj_wind[7] = (coef[3] * line)+ (coef[4] * samp) + coef[5];

return(E_SUCC);
}
