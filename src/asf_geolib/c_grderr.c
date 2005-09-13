/****************************************************************************
NAME:			      c_grderr

PURPOSE:  Calculates errors which occured during the gridding process

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
  5.0		1/89	D. Steinwand	   CSB      Original development
  5.1		6/90	D. Etrheim         CSB      Initialize max_x_index and
						    max_y_index

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Based on the number of coefficients, find degree of polynomial
	Split x & y portions of the polynomial and palce in aa & bb
	Initialize accumulators
	Find residual errors for 16 points 3/126, 43/126, 83/126, & 123/126
	  of the distance from one edge of the mapping grid to the other, in
	  both x and in y
	Store average error in x and in y
	Store average RMS error
	Store maximum grid error in x and in y
	Return
*****************************************************************************/


#include "proj.h"

#include "worgen.h"
#include "geompak.h"

void grid_coef(
	double tl,double tr,double bl,double br,	/* Input grid cell corners */
	double *c0,double *c1,double *c2,double *c3,	/* Coefficients */
	int dx,int dy);

void grid_eval(
	struct GEOGRID *grid,		/* Geometric mapping grid */
	double outx, double outy,		/* Output space locations */
	double *grid_x, double *grid_y);

void c_grderr(
	struct GEOGRID *grid,		/* Geometric mapping grid */
	double *tol)			/* Grid reduction tolerance */
{
double aa[15], bb[15];		/* Coefficients in x and in y */
double out_x,out_y;		/* Output space coordinates for error stats */
double grid_x, grid_y;		/* Input space coords--calculated via grid */
double poly_x, poly_y;		/* Input space coords--calc via polynomial */
double x_acc, y_acc, rms_acc;	/* Accumulators for average calculation */
double offset_y, offset_x;	/* Offsets for grid error point calculations */
double max_x, max_y;		/* Maximum gridding residuals in x and y */
double delta_x, delta_y;	/* Gridding residuals in x and y */
int degree;			/* Degree of mapping polynomial */
int acc;			/* Loop accumulator */
int cnt, count;		/* Loop counters */
int index;			/* Array index */
int max_x_index, max_y_index;	/* Index value of max x & y residuals */

/* Find degree of polynomial
  -------------------------*/
if (grid->ncoeff == 3) degree = 1;
if (grid->ncoeff == 6) degree = 2;
if (grid->ncoeff == 10) degree = 3;
if (grid->ncoeff == 15) degree = 4;

/* Pack polynomial into aa and bb
  ------------------------------*/
for (count = 0; count < grid->ncoeff; count++)
   {
   bb[count] = grid->coeffs[count];
   aa[count] = grid->coeffs[count + grid->ncoeff];
   }

/* Calculate errors due to the gridding process
  --------------------------------------------*/
x_acc = y_acc = rms_acc = 0.0;
max_x_index = max_y_index = 0;
acc = 0;
max_x = max_y = 0.0;
for (index = 0, offset_y = 3, count = 0; count < 4; count++, offset_y += 40)
   {
   out_y = (grid->out_lines[grid->nrows - 1] * offset_y) / 126.0;
   for (offset_x = 3, cnt = 0; cnt < 4; cnt++, offset_x += 40)
	{
	out_x = (grid->out_samps[grid->ncols - 1] * offset_x) / 126.0;
 	poly_x = c_eval(&degree, aa, &out_x, &out_y);
 	poly_y = c_eval(&degree, bb, &out_x, &out_y);
	grid_eval(grid, out_x, out_y, &grid_x, &grid_y);
	delta_x = poly_x - grid_x;
	delta_y = poly_y - grid_y;
	delta_x = fabs(delta_x);
	delta_y = fabs(delta_y);
	grid->act_grid_err[index] = delta_x;
	if (delta_x > max_x)
	   {
	   max_x = delta_x;
	   max_x_index = index;
	   }
	index++;
	grid->act_grid_err[index] = delta_y;
	if (delta_y > max_y)
	   {
	   max_y = delta_y;
	   max_y_index = index;
	   }
	index++;
	x_acc += delta_x;
	y_acc += delta_y;
	rms_acc += ((delta_x * delta_x) + (delta_y * delta_y));
	acc++;
        }
   }
/* Pack grid error fields in the geometric mapping grid structure 
  --------------------------------------------------------------*/
grid->ave_grid_err[0] = y_acc / acc;
grid->ave_grid_err[1] = x_acc / acc;
grid->rms_grid_err = sqrt(rms_acc / acc);
grid->max_grid_err[0] = grid->act_grid_err[max_y_index];
grid->max_grid_err[1] = grid->act_grid_err[max_x_index];
grid->griderr_valid = GEO_VALID;
grid->grid_tol = *tol;

return;
}

/****************************************************************************
NAME:				grid_eval

PURPOSE:  Finds an input space location given an x,y coordinate and a 
	  Geometric Mapping Grid

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Determine which grid cell in output space the x,y coordinate is in
	Calculate bilinear mapping coeffcients mapping output space to input
	Apply coefficients to obtain location in input space 
	Return
*****************************************************************************/
void grid_eval(
	struct GEOGRID *grid,		/* Geometric mapping grid */
	double outx, double outy,		/* Output space locations */
	double *grid_x, double *grid_y)	/* Input space locations to be found */
{
double a0,a1,a2,a3;		/* Grid mapping coefficients--x */
double b0,b1,b2,b3;		/* Grid mapping coefficients--y */
double xtl,xbl,xtr,xbr;		/* Input grid cell x corner coordinates */
double ytl,ybl,ytr,ybr;		/* Input grid cell y corner coordinates */
double square;			/* outx * outy */
int count;			/* Loop counter */
int dx, dy;			/* Space in output grid between grid corners */
int row, col, offset;		/* Offsets into grid structure */


/* Determine in which grid cell in the output space outx, outy lies
  ----------------------------------------------------------------*/
for (row = 0, count = 0; count < (grid->nrows - 1); count++)
   if( outy > grid->out_lines[count]) row = count;
for (col = 0, count = 0; count < (grid->ncols - 1); count++)
   if( outx > grid->out_samps[count]) col = count;

/* Find coefficients mapping from output grid cell to input grid cell
  ------------------------------------------------------------------*/
offset = (row * grid->ncols) + col;
xtl = grid->in_samps[offset];
ytl = grid->in_lines[offset++];
xtr = grid->in_samps[offset];
ytr = grid->in_lines[offset];
offset = ((row + 1) * grid->ncols) + col;
xbl = grid->in_samps[offset];
ybl = grid->in_lines[offset++];
xbr = grid->in_samps[offset];
ybr = grid->in_lines[offset];

dx = grid->out_samps[1] - grid->out_samps[0];
dy = grid->out_lines[1] - grid->out_lines[0];

grid_coef(xtl, xtr, xbl, xbr, &a0, &a1, &a2, &a3, dx, dy);
grid_coef(ytl, ytr, ybl, ybr, &b0, &b1, &b2, &b3, dx, dy);

/* Apply coefficients to calculate gridded location of outx, outy (outx & outy
   must be adjusted to be zero relative to the current grid cell)
  --------------------------------------------------------------*/
outy = outy - grid->out_lines[row];
outx = outx - grid->out_samps[col];

square = outx * outy;
*grid_x = a0 + (a1 * outx) + (a2 * outy) + (a3 * square);
*grid_y = b0 + (b1 * outx) + (b2 * outy) + (b3 * square);

return;
}

/****************************************************************************
NAME:				grid_coef	

PURPOSE:  Calculates coefficients given four corners of input space and
	  output space dimensions

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Calculate coefficients C0, C1, C2, and C3
	Return
*****************************************************************************/
void grid_coef(
	double tl,double tr,double bl,double br,	/* Input grid cell corners */
	double *c0,double *c1,double *c2,double *c3,	/* Coefficients */
	int dx,int dy)		/* Size of output space */
{
*c0 = tl;
*c1 = (tr - tl) / dx;
*c2 = (bl - tl) / dy;
*c3 = (br - tr - bl + tl) / (dx * dy);
return;
}
