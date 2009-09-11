/*****************************************************************************

FUNCTION:   rtcoef

PURPOSE:    Calculates a set of first order coefficients for transforming
	    line,sample image coordintes to rotated projection coordinates
	    of a rotated projection space.

CALLING SEQUENCE:  status = c_rtcoef(proj, image, coefs, flag);

     parameter       dtype       I/O        Comments
     ---------       -----       ---        --------
      proj           double       I         Contains projection coordinates
					    of the image corners.  Given in
			  		    y,x order, ul, ll, ur, lr.
      image          int         I         Contains image coordintes (line,
					    sample) of the image corners.
					    Given in ul, ll, ur, lr.
      coefs	     double       O         Contains transformation coefficients
					    for line,sample to rotated proj-
				            ection coordinates.
      flag	     int	  I	    Specifies whether or not error
					    messages that are generated within
					    the routine are to be output to
					    the terminal; FALSE specifies that
					    no messages are to be ouptut.
      status	     int	 Return     Routine returns E_SUCC for a
					    successful computation or E_FAIL
					    if a fatal error is encountered.

EXTERNAL ASSOCIATIONS:  This function includes "las.h" and <math.h>

FILE REFERENCES:   None.

SUBPROGRAMS CALLED:   None.
  
INFORMATIONAL MESSAGES AND NON-FATAL ERROR CONDITIONS:

  Error Messages:
  --------------
   [geolib-rot_coords]  Invalid projection or window coordinates
 
   Rtcoef was not able to calculate a difference between either the
   upper left and upper right image coordinates or the upper left and upper
   right projection coordinates.  Check the values of the image and projection
   coordinates sent to this routine.

   [geolib-rot_scale]  Scale factor out of range

   The computed scale factor to map between projection and image coordinates
   is out of range.  This check is put here for numerical purposes.  Check the
   validity of the projection and image coordinates input to this routine.  If
   this error occurs frequently (which is not expected), the rtcoef
   algorithm should be changed to handle these extreme scale factors with 
   greater accuracy.
   

PROGRAM HISTORY:
  Version	Date       Author         Code/Contr     Request
  -------	----	   ------         ----------     -------
    1.0         12/88      D. Steinwand   EROS-CSB       Development
							 (Taken from c_rotcor)
    1.1		12/88	   D. Akkerman	  EROS-CSB	 Added FLAG parameter
							 for optional error
							 messages.
    1.2  	 7/89	   D. Steinwand   EROS-CSB	 Removed scale-factor
							 check--was running
							 into problems with
							 global data sets
							 and very large pixel
							 sizes
    1.3  	 12/90	   B. Ailts       EROS-CSB	 Updated error messages

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	C callable support routine

PROJECT:  LAS

ALGORITHM (FLOW):

    Assign the upper left projection coordinates to a program variable.
    Assign the upper left image coordinates to a program variable.
    Calculate the difference between the upper left and upper right 
     projection coordinates.
    Calculate the difference between the upper left and upper right
     image coordinates.
    Calculate the difference between the upper left and lower left projection
     coordinates.
    Calculate the difference between the upper left and lower left image
     coordinates.
    Compute the  x & y pythagorian distance between the two points in 
     projection space.
    Compute the  x & y pythagorian distance between the two points in 
     image space.
    If either of the pythagorian distances is zero, fatal error.
    Compute the sine and cosine of the angle between the two coordinate systems
     in both x & y.
    Compute the scale factor between the two coordinate systems in both x & y.
    Calculate mapping coefficients.
    Return to the calling program.

ALGORITHM REFERENCES:

1.  This function was initally pulled from the LAS 5.0 (C version) of
    the ROTRNSCL routine to calculate mapping coefficients from two
    tie point coordinates.  It has since been heavily modified to perform
    a simular but more specialized function.

*****************************************************************************/
#include "worgen.h"


lasErr FUNCTION c_rtcoef(const double proj[8], const int image[8],double coef[6], int *flag)

				/* The following parameters with 8 elements are
				   ordered as follows:
				     [0] = upper left y
				     [1] = upper left x
				     [2] = lower left y
				     [3] = lower left x
 	  			     [4] = upper right y
				     [5] = upper right x
				     [6] = lower right y
  				     [7] = lower right x     */
/*double proj[8];			 Input image corner projection coordinates */
/*int image[8];			 Input image corner line,sample coordinates */
/*double coef[6];			 Transformation coefficients */
/*int *flag;			 Flag indicating if error messages are to be
				   output (FALSE specifies that no messages
				   are to be output). */
{

double p_x1;			/* First projection point--x */
double p_y1;			/* First projection point--y */
double i_x1;			/* First Image point--x */
double i_y1;			/* First Image point--y */

double py2_1;			/* Second projection point - First (in y) */
double px2_1;			/* Second projection point - First (in x) */
double iy2_1;			/* Second image point - First (in y) */
double ix2_1;			/* Second image point - First (in x) */

double py3_1;			/* Third projection point - First (in y) */
double px3_1;			/* Third projection point - First (in x) */
double iy3_1;			/* Third image point - First (in y) */
double ix3_1;			/* Third image point - First (in x) */

double yscale;			/* scale factor in y */
double xscale;			/* scale factor in x */
double xsinang;			/* sine of rotation angle in x */
double xcosang;			/* cosine of rotation angle in x */
double ysinang;			/* sine of rotation angle in y */
double ycosang;			/* cosine of rotation angle in y */
double xp_dist;			/* x Distance between projection tiepoints */
double xi_dist;			/* x Distance between image tiepoints */
double xp_i_dist;		/* x p_dist * i_dist */
double yp_dist;			/* y Distance between projection tiepoints */
double yi_dist;			/* y Distance between image tiepoints */
double yp_i_dist;		/* y p_dist * i_dist */

/*--------------------------------------------------------------------------
  Assign the upper left coordinates to program variables.  Note that the 
  upper left corner of the image must be used (image coordinates 1,1) as
  some of the equations later in processing assume this...
 ---------------------------------------------------------------------------*/
p_y1 = proj[0];
p_x1 = proj[1];
i_y1 = image[0];
i_x1 = image[1];

/*--------------------------------------------------------------------------
  Calculate the difference between the upper left image and projection 
  coordinates and the upper right image and projection coordinates.
 ---------------------------------------------------------------------------*/
px2_1 = proj[5] - p_x1;
py2_1 = proj[4] - p_y1;
ix2_1 = image[5] - i_x1;
iy2_1 = image[4] - i_y1;

/*--------------------------------------------------------------------------
  Calculate the difference between the upper left image and projection
  coordinates and the lower left image and projection coordinates.
 ---------------------------------------------------------------------------*/
px3_1 = proj[3] - p_x1;
py3_1 = proj[2] - p_y1;
ix3_1 = image[3] - i_x1;
iy3_1 = image[2] - i_y1;

/*--------------------------------------------------------------------------
  Compute the pythagorian distance of the image space and the projection
  space in x.
 --------------------------------------------------------------------------*/
xp_dist = sqrt((px2_1 * px2_1) + (py2_1 * py2_1));
xi_dist = sqrt((ix2_1 * ix2_1) + (iy2_1 * iy2_1)); 
xp_i_dist = xp_dist * xi_dist;

if(xp_i_dist == 0.0)
 {
  if (*flag)
   c_errmsg("Invalid projection or window coordinates","rtcoef-rot_coords",
             NON_FATAL);
  return(E_FAIL);

 }

/*--------------------------------------------------------------------------
  Compute the pythagorian distance of the image space and the projection
  space in y.
 --------------------------------------------------------------------------*/
yp_dist = sqrt((px3_1 * px3_1) + (py3_1 * py3_1));
yi_dist = sqrt((ix3_1 * ix3_1) + (iy3_1 * iy3_1)); 
yp_i_dist = yp_dist * yi_dist;

if(yp_i_dist == 0.0)
 {
  if (*flag)
   c_errmsg("Invalid projection or window coordinates","rtcoef-rot_coords",
             NON_FATAL);
  return(E_FAIL);
 }

/*--------------------------------------------------------------------------
  Compute angle and scale factor 
 --------------------------------------------------------------------------*/
xsinang = ((px2_1 * iy2_1) - (py2_1 * ix2_1)) / (xp_i_dist);
xcosang = ((py2_1 * iy2_1) + (px2_1 * ix2_1)) / (xp_i_dist);
ysinang = ((px3_1 * iy3_1) - (py3_1 * ix3_1)) / (yp_i_dist);
ycosang = ((py3_1 * iy3_1) + (px3_1 * ix3_1)) / (yp_i_dist);

xscale = xi_dist/xp_dist;
yscale = yi_dist/yp_dist;

/*--------------------------------------------------------------------------
  Calculate mapping coefficients.  Again, these equations assume the upper
  left image coordinate corner is 1,1! 
 ---------------------------------------------------------------------------*/
coef[0] = ycosang / yscale;
coef[1] = -(xsinang) / xscale;
coef[2] = p_y1 - coef[0] - coef[1];
coef[3] = ysinang / yscale;
coef[4] = xcosang / xscale;
coef[5] = p_x1 - coef[3] - coef[4];

return(E_SUCC);
}
