/*********************
This matrix transpose is in a seperate file because it should always be double precision.
*********************/
typedef double xdouble;	/* I use double_t so that global search and replace on double won't */
						/* change this to float accidentally.*/

void dxpose(xdouble *indata, int iRsiz, xdouble *outdata, int oRsiz, int Nrows, int Ncols);
/* not in-place double precision matrix transpose	*/
/* INPUTS */
/* *indata = input data array	*/
/* iRsiz = offset to between rows of input data array	*/
/* oRsiz = offset to between rows of output data array	*/
/* Nrows = number of rows in input data array	*/
/* Ncols = number of columns in input data array	*/
/* OUTPUTS */
/* *outdata = output data array	*/
