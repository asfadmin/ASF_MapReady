/*
**  name
**	alloc2d_1 - allocates two-dimensional buffer
**
**  synopsis
**	char **alloc2d(nrows, ncols, elsize)
**
**  description
**	Allocates a two-dimensional buffer of dimension rows by cols, each of
**	elsize bytes.  Any element in the array may then be referenced as
**	x[j][k].
**
**  diagnostics
**	Returns NULL if can't allocate, writing message to Qerrstr.
**
**  history
**	July 1982: written by J. Dozier, Department of Geography, UCSB
**	Oct. 1992: Modified by Shusun Li, Geophysical Institute, UAF
**
**  bugs
**	x[0] is the pointer to the beginning of the data, not x.
**	To free the space, first free(x[0]), then free(x).
*/

#include "asf.h"
#define		MAX(a,b)	((a > b) ? a : b)

char **
alloc2d_1(rows, cols, elsize)
	int	rows;
	int	cols;
	int	elsize;
{
	static	int	sp = sizeof(char *);

	char		*buf, **xh;
	int		j;
	unsigned	tbytes;

	union {
		char	*u_cp;
		char	**u_cpp;
	} u;

	cols *= elsize; 	/*  cols is now row-size in bytes	*/

	/*  buf is pointer to data area itself	*/
	/*  xh is pointer to row addresses	*/

	tbytes = rows * cols;

	buf = MALLOC(tbytes);

	/*  we make xh large enough to transpose if we later want to	*/
	u.u_cp = MALLOC(MAX(rows,cols)*sp);

	xh = u.u_cpp;

	/*	set pointers to the beginning of each row	*/
	for (j = 0; j < rows; j++)
		xh[j] = buf + j*cols;

	return(xh);
}
