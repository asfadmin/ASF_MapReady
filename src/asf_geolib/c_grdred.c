/****************************************************************************
NAME:				c_grdred	

PURPOSE:  Discard rows and columns of the geometric mapping grid that are
	  within the tolerance of linear approximation across neighboring
	  rows and columns.  Note:  Output grid cells are rectangular and
	  are all identical in size.

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
  1.0      	 5/84  	M. NISHIHAMA       SAR        N/A
  5.0		 1/89	D. Steinwand	   CSB      LAS 5.0 C conversion
  5.0.1		 3/89   D. Steinwand       CSB      Redesigned for LAS 5.0
						    GEOM buffering

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Eliminate columns within tolerance
	Eliminate rows within tolerance
	If grid cannot be reduced, return
	Loop through original grid, searching for the maximum input sample
	Calculate the maximum number of lines allowed in the input grid
	Loop through the reduced grid, keeping track of min/max line/sample
	    Calculate the number of lines and samples per grid cell
	    Keep track of the larges grid cell
	  Check the largest grid cell against GEOM buffering limits
	  Reduce cell size & repeat the process if grid cells are too large
	Remove columns not needed in output space
	Remove rows not needed in output space
	Loop through new output space, reducing the input space in both the
		row and column direction
	Return
*****************************************************************************/

#include "worgen.h"
#include "geompak.h"

#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif



int  c_grdred(grid, tolerance)

struct GEOGRID *grid;		/* Geometric mapping grid */
double *tolerance;		/* Tolerance for linear approximation */
{
int count, cnt;		/* loop counters */
int loc;			/* Location in non-reduced grid */
int loc_reduced;		/* Location in reduced grid */
int col_status;		/* Reduction of grid columns flag */
int row_status;		/* Reduction of grid rows flag */
int next[12];			/* Column/row increments allowable */
int row_inc;			/* Increment into NEXT array, row direction */
int col_inc;			/* Increment into NEXT array, col direction */
int k;				/* Local counter */
float mostln, mostsm;		/* Largest grid cell */
double *col;			/* Temporary pointer into grid for loop */
float max_line, max_samp;	/* Maximum line, sample in input grid cell */
float min_line, min_samp;	/* Minimum line, sample in input grid cell */
float nlines, nsamps;		/* Number of lines, samps in input grid cell */
float max_lines_allowed;	/* Maximum number of input grid cell lines */
float max_samps_allowed;	/* Maximum number of input grid cell samples */
int satisfied;			/* Boolean value for loop completion */
int fudge = 9;			/* "Fudge-factor" for extra resampling pixels */

/* The "next" array contains increments to test which will keep the output 
   space grid evenly spaced.  This scheme also saves time
  ------------------------------------------------------*/
next[0] = 1;
next[1] = 2;	
next[2] = 3;
next[3] = 6;
next[4] = 7;
next[5] = 9;
next[6] = 14;
next[7] = 18;
next[8] = 21;
next[9] = 42;
next[10] = 63;
next[11] = 126;

/* Test grid for column and row elimination
  ----------------------------------------*/
col_status = elim_col(grid, *tolerance, next, &col_inc);
row_status = elim_row(grid, *tolerance, next, &row_inc);

/* If grid cannot be reduced, return (fail).  NOTE:  The routine did not get
   a fatal error--the grid failed to reduce.
  -----------------------------------------*/
if ((col_status == E_FAIL) && (row_status == E_FAIL)) return(E_FAIL);

/* Test the new grid size against GEOM buffer space.  Adjust the reduction
   if necessary
  ------------*/
max_samp = 0.0;
col = grid->in_samps;
for (cnt = (grid->nrows - 1); cnt--; col++)
  for (count = (grid->ncols - 1); count--; col++)
     {
     max_samp = GMAX(max_samp, *col);
     max_samp = GMAX(max_samp, *(col + 1));
     max_samp = GMAX(max_samp, *(col + grid->ncols));
     max_samp = GMAX(max_samp, *(col + grid->ncols + 1));
     }
max_lines_allowed = MAX_GEOM_INBUF / (max_samp * sizeof(int));

/* Adjust reduction factors
  ------------------------*/
for (;;)
   {
   mostln = mostsm = 0.0;
   if((row_inc == 0) && (col_inc == 0)) return(E_FAIL);
   for (cnt = 0; cnt < (grid->nrows - 1); cnt += next[row_inc])
      for (count = 0; count < (grid->ncols - 1); count += next[col_inc])
	{
	loc = cnt * MAX_GRID_SIZE + count;
	min_samp = max_samp = grid->in_samps[loc];
	min_line = max_line = grid->in_lines[loc];
	loc = cnt * MAX_GRID_SIZE + (count + next[col_inc]);
 	max_samp = GMAX(max_samp, grid->in_samps[loc]);
 	min_samp = GMIN(min_samp, grid->in_samps[loc]);
 	max_line = GMAX(max_line, grid->in_lines[loc]);
 	min_line = GMIN(min_line, grid->in_lines[loc]);
	loc = (cnt + next[row_inc]) * MAX_GRID_SIZE + count;
 	max_samp = GMAX(max_samp, grid->in_samps[loc]);
 	min_samp = GMIN(min_samp, grid->in_samps[loc]);
 	max_line = GMAX(max_line, grid->in_lines[loc]);
 	min_line = GMIN(min_line, grid->in_lines[loc]);
	loc = (cnt + next[row_inc]) * MAX_GRID_SIZE + (count + next[col_inc]);
 	max_samp = GMAX(max_samp, grid->in_samps[loc]);
 	min_samp = GMIN(min_samp, grid->in_samps[loc]);
 	max_line = GMAX(max_line, grid->in_lines[loc]);
 	min_line = GMIN(min_line, grid->in_lines[loc]);

  	nlines = max_line - min_line;
	nsamps = max_samp - min_samp;
	mostln = GMAX(mostln, nlines);
	mostsm = GMAX(mostsm, nsamps);
	}

   max_samps_allowed = (MAX_GEOM_WORKBUF / (mostln * 2 *sizeof(int))) - fudge;

   satisfied = TRUE;
   if (mostln > max_lines_allowed)
	{
	satisfied = FALSE;
	if (row_inc > 0) row_inc--;
        else if ((mostsm <= max_samps_allowed) && (col_inc > 0)) col_inc--;
	}
   if (mostsm > max_samps_allowed)
	{
	satisfied = FALSE;
	if (col_inc > 0) col_inc--;
        else if ((mostln <= max_lines_allowed) && (row_inc > 0)) row_inc--;
	}
   if (satisfied) break;
   }

/* Eliminate output space columns
  ------------------------------*/
if (col_status == E_SUCC)
   {
   for(count = 0, k = 0; count < grid->ncols; count += next[col_inc], k++)
	grid->out_samps[k] = grid->out_samps[count];
   grid->ncols = k;
   }

/* Eliminate output space rows
  ---------------------------*/
if (row_status == E_SUCC)
   {
   for(count = 0, k = 0; count < grid->nrows; count += next[row_inc], k++)
	grid->out_lines[k] = grid->out_lines[count];
   grid->nrows = k;
   }

/* Reduce input space grid in the row and column directions
  --------------------------------------------------------*/
for (loc_reduced = 0, cnt = 0; cnt < grid->nrows; cnt++)
   for (count = 0; count < grid->ncols; count++, loc_reduced++)
	{
       	loc = (cnt * MAX_GRID_SIZE * next[row_inc]) + (count * next[col_inc]);
       	grid->in_lines[loc_reduced] = grid->in_lines[loc];
       	grid->in_samps[loc_reduced] = grid->in_samps[loc];
       	}
return(E_SUCC);
}


/****************************************************************************
NAME:				elim_col

PURPOSE:  Elimates redundant grid columns within tolerance

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
  5.0		1/89	D. Steinwand	   CSB      New development

ALGORITHM DESCRIPTION:
	Print informational message and initialize variables
	Loop
	   Check for column elimination
	   If fail, decrement inc and try again until no columns are skiped
	   If pass, increment inc to the next grid spacing to check, until done
	Return

ALGORITHM REFERENCES:
	This routine is modeled after the grid reduction routines found in
	  the LAS 4.0 GRIDGEN by M. Nishihama of SAR, 5/84.  It is, however,
	  a different approach to the problem, optimized for techniques used
	  in GEOM, the geometric rectification function.
*****************************************************************************/
int elim_col(grid, tol, next, inc)

struct GEOGRID *grid;		/* Geometric mapping grid */
double tol;			/* Grid reduction tolerance */ 
int *next;			/* List of increments for reduction check */ 
int *inc;			/* Increment (index) into NEXT -- returned */ 
{ 
int max_inc = 11;		/* Maximum value of inc */
int size;			/* Size of output space grid cell */ 
int start;			/* Starting grid location (zero relative) */ 
int end;			/* Ending grid location (zero relative) */ 

/* Initialize test spacing to skip one column
  ------------------------------------------*/
c_errmsg("Attempting grid reduction in the column direction"," ",NON_FATAL);
*inc = 1;
start = 0;
end = next[*inc];
size = grid->out_samps[end] - grid->out_samps[start];
for (;;)
   {
   if ((int)check_col(start, end, tol, size, grid) != E_SUCC)

/* If test failed, try the previous spacing, starting at the column which
   failed the test.  This is now the maximum allowed spacing and the rest of
   the grid will be checked with it.  If the previous spacing is zero, the
   grid cannot be reduced; return fail.
  -----------------------------------*/
	{
	(*inc)--;
	if (*inc == 0) return (E_FAIL);
	max_inc = *inc;
	start = next[*inc];
	end = start + next[*inc];
	size = grid->out_samps[end] - grid->out_samps[start];
	}
   else

/* If test passed, increment the skip interval to the next value of "next".
   However, is a failure occured previously, the grid spacing is not adjusted;
   starting and ending points are adjusted to the next group of columns to test
  ---------------------------------------------------------------------------*/
	{
	if(max_inc == 11)
	   {
	   if (*inc == 11) return (E_SUCC);
	   (*inc)++;
	   end = start + next[*inc];
	   size = grid->out_samps[end] - grid->out_samps[start];
	   }
	else
	   {
	   if (end == next[11]) return(E_SUCC);
	   start = end;
	   end += next[*inc];
	   }
	}
   }
}

/****************************************************************************
NAME:				elim_row

PURPOSE:  Elimates redundant grid rows within tolerance

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
  5.0		1/89	D. Steinwand	   CSB      New development

ALGORITHM DESCRIPTION:
	Print informational message and initialize variables
	Loop
	   Check for row elimination
	   If fail, decrement inc and try again until no rows are skiped
	   If pass, increment inc to the next grid spacing to check, until done
	Return

ALGORITHM REFERENCES:
	This routine is modeled after the grid reduction routines found in
	  the LAS 4.0 GRIDGEN by M. Nishihama of SAR, 5/84.  It is, however,
	  a different approach to the problem, optimized for techniques used
	  in GEOM, the geometric rectification function.
*****************************************************************************/
int elim_row (grid, tol, next, inc)

struct GEOGRID *grid;		/* Geometric mapping grid */
double tol;			/* Grid reduction tolerance */
int *next;			/* List of increments for reduction check */
int *inc;			/* Increment (index) into NEXT -- returned */
{
int max_inc = 11;		/* Maximum value of inc */
int size;			/* Size of output space grid cell */
int start;			/* Starting grid location (zero relative) */
int end;			/* Ending grid location (zero relative) */

/* Initialize test spacing to skip one row 
  ---------------------------------------*/
c_errmsg("Attempting grid reduction in the row direction"," ",NON_FATAL);
*inc = 1;
start = 0;
end = next[*inc];
size = grid->out_lines[end] - grid->out_lines[start];
for (;;)
   {
   if ((int)check_row(start, end, tol, size, grid) != E_SUCC)

/* If test failed, try the previous spacing, starting at the row which failed 
   the test.  This is now the maximum allowed spacing and the rest of the grid 
   will be checked with it.  If the previous spacing is zero, the grid cannot 
   be reduced; return fail.
  ------------------------*/
	{
	(*inc)--;
	if (*inc == 0) return (E_FAIL);
	max_inc = *inc;
	start = next[*inc];
	end = start + next[*inc];
	size = grid->out_lines[end] - grid->out_lines[start];
	}
   else

/* If test passed, increment the skip interval to the next value of "next".
   However, is a failure occured previously, the grid spacing is not adjusted;
   starting and ending points are adjusted to the next group of rows to test.
  -------------------------------------------------------------------------*/
	{
	if(max_inc == 11)
	   {
	   if (*inc == 11) return (E_SUCC);
	   (*inc)++;
	   end = start + next[*inc];
	   size = grid->out_lines[end] - grid->out_lines[start];
	   }
	else
	   {
	   if (end == next[11]) return(E_SUCC);
	   start = end;
	   end += next[*inc];
	   }
	}
   }
}

/****************************************************************************
NAME:				check_col

PURPOSE:    Check tolerance along group of columns being tested for elimination

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
  1.0      	 5/84  	M. NISHIHAMA       SAR        N/A
  5.0		 1/89	D. Steinwand	   CSB      LAS 5.0 C conversion

ALGORITHM DESCRIPTION:
	Set max deviation to zero
	Do for each row
	   Calculate starting and ending indicies into the Geometric Mapping
			Grid
	   Calculate linear coefficients
	   Find the maximum deviation along a group of columns
	   If the max deviation > tol, return (fail)
	Return (success)
*****************************************************************************/
int  check_col(start, end, tol, size, grid)

int start;		/* Starting column, zero relative */
int end;		/* Ending column, zero relative */
double tol;		/* Grid reduction tolerance value */
int size;		/* Size of "new" output space per "new" grid cell */
struct GEOGRID *grid;	/* Geometric mapping grid */
{
double maxdev;		/* Maximum deviation */
int row;		/* Current row number */
int start_index;	/* Index into input grid; starting point */
int end_index;		/* Index into input grid; ending point */
double a0,a1,b0,b1;	/* Linear approximation coefficients for "new" cell */
int cnt;		/* Loop counter */
int k;			/* Offset into mapping grid for each group element */
int x;			/* Value to evaluate "new" grid cell at */
double fx,fy;		/* Results of evaluating "new" grid cell at x */
double devx, devy;	/* Deviation from actual point & fx, fy */

maxdev = 0.0;
for (row = 0; row < grid->nrows; row++)
   {
   start_index = (row * grid->ncols) + start;
   end_index = (row * grid->ncols) + end;
   a0 = grid->in_samps[start_index];
   b0 = grid->in_lines[start_index];
   a1 = (grid->in_samps[end_index] - a0) / size;
   b1 = (grid->in_lines[end_index] - b0) / size;

/* Find maximum deviation in group from linear approximation
  ---------------------------------------------------------*/
   for (k = 1, cnt = (start + 1); cnt < end; cnt++, k++)
	{
	x = grid->out_samps[start + k] - grid->out_samps[start];
	fx = a0 + (a1 * x);	
	fy = b0 + (b1 * x);
	devx = fabs(fx - grid->in_samps[start_index + k]);
	devy = fabs(fy - grid->in_lines[start_index + k]);
	maxdev = GMAX(maxdev, devx);
	maxdev = GMAX(maxdev, devy);
	if (maxdev > tol) return(E_FAIL);
	}
   }
return(E_SUCC);
}


/****************************************************************************
NAME:				check_row

PURPOSE:    Check tolerance along group of rows being tested for elimination

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
  1.0      	 5/84  	M. NISHIHAMA       SAR        N/A
  5.0		 1/89	D. Steinwand	   CSB      LAS 5.0 C conversion

ALGORITHM DESCRIPTION:
	Set max deviation to zero
	Do for each row
	   Calculate starting and ending indicies into the Geometric Mapping
			Grid
	   Calculate linear coefficients
	   Find the maximum deviation along a group of rows
	   If the max deviation > tol, return (fail)
	Return (success)
*****************************************************************************/
int check_row(start, end, tol, size, grid)

int start;		/* Starting column, zero relative */
int end;		/* Ending column, zero relative */
double tol;		/* Grid reduction tolerance value */
int size;		/* Size of "new" output space per "new" grid cell */
struct GEOGRID *grid;	/* Geometric mapping grid */
{
double maxdev;		/* Maximum deviation */
int row;		/* Current row number */
int start_index;	/* Index into input grid; starting point */
int end_index;		/* Index into input grid; ending point */
double a0,a2,b0,b2;	/* Linear approximation coefficients for "new" cell */
int cnt;		/* Loop counter */
int k;			/* Offset into mapping grid for each group element */
int y;			/* Value to evaluate "new" grid cell at */
double fx,fy;		/* Results of evaluating "new" grid cell at y */
double devx, devy;	/* Deviation from actual point & fx, fy */

maxdev = 0.0;
for (row = 0; (row + end) < grid->nrows; row++)
   {
   start_index = (row + start) * grid->ncols;
   end_index = (row + end) * grid->ncols;
   a0 = grid->in_samps[start_index];
   b0 = grid->in_lines[start_index];
   a2 = (grid->in_samps[end_index] - a0) / size;
   b2 = (grid->in_lines[end_index] - b0) / size;

/* Find maximum deviation in group from linear approximation
  ---------------------------------------------------------*/
   for (k = 1, cnt = (start + 1); cnt < end; cnt++, k++)
	{
	y = grid->out_lines[start + k] - grid->out_lines[start];
	fx = a0 + (a2 * y);	
	fy = b0 + (b2 * y);
	devx = fabs(fx - grid->in_samps[start_index + (k * grid->ncols)]);
	devy = fabs(fy - grid->in_lines[start_index + (k * grid->ncols)]);
	maxdev = GMAX(maxdev, devx);
	maxdev = GMAX(maxdev, devy);
	if (maxdev > tol) return(E_FAIL);
	}
   }
return(E_SUCC);
}
