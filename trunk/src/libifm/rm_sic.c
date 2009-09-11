/****************************************************************
FUNCTION NAME: ReadMatrixFromSIC()

SYNTAX: void ReadMatrix(fnm,a,m,n,c0,r0,M,N,h0,h1);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    fnm         char *          source filename
    a           SIComplex **    destination buffer
    m, n        int             columns, rows to be read
    c0, r0      int             c0 = first column, r0 = first row
    M, N        int             columns and rows of data file
    h0, h1      int             source header sizes

DESCRIPTION:
	Reads an (m across by n down) matrix from a file and places it into a
	buffer 'a'.  'type' defines the size of each sample in the matrix.
	'h0' is the number of header bytes to strip off the beginning of the
	file. 'h1' is the number of header bytes to strip off of each line in
	the file. 

RETURN VALUE:
	None. However, the function will fill the buffer pointed to by 'a'
	with the corresponding data from 'fnm'.

SPECIAL CONSIDERATIONS:
	All files are assumed to be binary. Also, all files assume a signle
	value per pixel except in the case of Complex. In this case,
	ReadMatrix assumes 2 values per sample.

	The top-left index is 0,0. In other words the first row and first
	column are 0.

PROGRAM HISTORY:
       1.0 - Mike Shindle - Original Development.
****************************************************************/
#include "asf.h"

#include <unistd.h>
#include <fcntl.h>
#include "ifm.h"

void ReadMatrixFromSIC(char *fnm,            /* source filename       */
                       SIComplex **a,        /* destination buffer    */
                       int m,                /* columns to be read    */ 
		       int n,                /* rows to be read       */
                       int c0,               /* c0 = first column     */
		       int r0,               /* r0 = first row        */ 
                       int M,                /* cols of source        */ 
		       int N,                /* rows of source file   */
                       int h0,               /* source header size    */ 
		       int h1)               /* (see below)           */
{
  int ls;
  int fd;
  int j, k;
  int dsize=sizeof(SIComplex);
  int tail;

  /* check parameters, bless all passed values */
  if (m < 1) Exit("ReadMatrix():  number of matrix columns < 1");
  if (n < 1) Exit("ReadMatrix():  number of matrix rows < 1");
  if (c0 < 0) Exit("ReadMatrix():  first column < 0");
  if (r0 < 0) Exit("ReadMatrix():  first row < 0");
  if (M < 1) Exit("ReadMatrix():  number of source columns < 1");
  if (N < 1) Exit("ReadMatrix():  number of source rows < 1");
  if (h0 < 0) Exit("ReadMatrix():  top header length < 0");
  if (h1 < 0) Exit("ReadMatrix():  record header length < 0");
  if (m + c0 > M) Exit("ReadMatrix():  columns exceed given file size");
  if (n + r0 > N) Exit("ReadMatrix():  rows exceed given file size");

  /* open file that holds data */
  fd = open (fnm, 0);
  if (fd < 3) 
    Exit("ReadMatrix(): cannot open file '%s'",fnm);

  /* 
   * lseek past 'h0' header bytes; 
   * file is now set to first line of data 
   */
  ls = lseek(fd,(int)(h0),0);

  /* lseek to row r0 */
  j = r0*(h1 + M*dsize);
  ls = lseek(fd,(int)(j),1);

  /* calculate 'tail', number of bytes from end of matrix 
   * line to end of record 
   */
  tail = dsize*(M - (c0 + m));

  /* read in matrix to buffer */
  for (j = 0; j < n; j++){
    /* lseek over header h1 */
    ls = lseek (fd, (int)(h1), 1);
    
    /* lseek to column c0 */
    k  = c0*dsize;
    ls = lseek (fd, (int)(k), 1);
    
    /* read data directly to buffer *(a+ind) */
    if (read(fd, a[j], dsize*m) != dsize*m)
      Exit("ReadMatrix(): bad read from file '%s'",fnm);

    /* lseek to end of line */
    ls = lseek(fd,(int)(tail),1);
  }

  close (fd);
  return;
}
