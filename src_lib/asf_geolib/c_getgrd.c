/****************************************************************************
NAME				c_getgrd

PURPOSE		Read a geometric mapping grid file from disk

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand	     	10/88	Original development
D. Etrheim		07/90	Standardized error message handling

PROJECT		LAS

ALGORITHM
   Check for existance of geometric mapping grid file
   Open the geometric mapping grid file for read access using label services.
   Read the first geometric mapping grid record
   Read the second geometric mapping grid record
   Close the geometric mapping grid file
******************************************************************************/


#include "asf.h"
#include "worgen.h"
#include "geompak.h"

int c_getgrd(host_name, grid)

  char *host_name;		/* Host file name of the mapping grid file */
  struct GEOGRID *grid;		/* Geometric mapping grid */

{
int i,j;		/* Loop counters */
int mode = 0;		/* access mode for lsstat */
int access = 0;	/* access mode for lsopen, set to read access */
FILE * fd;		/* file descriptor */
int rowcol;		/* grid->nrows * grid->ncols to reduce computations */
int clen;		/* number of bytes of character data per record */
int dlen;		/* number of bytes of data per record */
int action = 0;	/* action parameter for lsclos */
int *tmpbuf=NULL;		/* Temporary buffer for read */
int num_bytes;		/* Number of bytes in double portion of grid */
int num_recs;		/* Number of records of double */
int rec_inc;		/* Record length / sizeof(double) */
int rec_len;		/* Record length of double precision records */
int last_rec_len;	/* Record length of last double grid record */
float num_temp;		/* Temporary floating point value of num_recs */
double *tmpdbl;		/* Temporary buffer for read -- doubles */
double *buf_ptr;	/* Pointer to temporary buffer used during read */
char dummy[30];		/* Dummy argument for second ls read */

/* Check to see if the file aready exists.  If it does exist, an error message
   is printed.
 -----------------*/
if (c_lsstat(host_name, &mode) != E_SUCC)
   {
   c_errmsg("Geometric mapping grid file does not exist on disk",
            "getgrd-exist", NON_FATAL);
   return(E_FAIL);
   }

/* Open the output geometric mapping grid file
 --------------------------------------------*/
if (c_lsopen(&fd, host_name, &access) != E_SUCC)
   {
   c_errmsg("Error opening geometric mapping grid file for read access",
            "getgrd-open", NON_FATAL);
   return(E_FAIL);
   }

/* Read the first geometric mapping grid file record.  This read will fail,
   but will return buffer lengths
 --------------------------------*/
clen = 1;
dlen = 1;
if (c_lsread(&fd, "GEO_GRID_INT", &clen, &dlen, dummy, (unsigned char *)tmpbuf, dummy) != E_SMAL)
   {
   c_errmsg("Error reading from the geometric mapping grid file", "getgrd-read",
             NON_FATAL);
   return(E_FAIL);
   }

/* Allocate buffer space for the integer portion of the geometric mapping grid
 ----------------------------------------------------------------------------*/
tmpbuf = (int *) MALLOC(dlen);

/* Reread the first geometric mapping grid file record
 ---------------------------------------------------*/
if (c_lsread(&fd, "GEO_GRID_INT", &clen, &dlen, dummy, (unsigned char *)tmpbuf, dummy) != E_SUCC)
   {
   c_errmsg("Error reading from the geometric mapping grid file", "getgrd-read",
             NON_FATAL);
   return(E_FAIL);
   }

/* Unpack the first record into the grid stucture
  ------------------------------------------------*/
strncpy(grid->model, dummy, 15);
strncpy(grid->units, &(dummy[15]), 12);

grid->proj_valid = tmpbuf[0];
grid->fiterr_valid = tmpbuf[1];
grid->griderr_valid = tmpbuf[2];
grid->ncoeff = tmpbuf[3];
grid->code = tmpbuf[4];
grid->zone = tmpbuf[5];
grid->datum = tmpbuf[6];
grid->nrows = tmpbuf[7];
grid->ncols = tmpbuf[8];
grid->lines = tmpbuf[9];
grid->samples = tmpbuf[10];
grid->out_lines = &(tmpbuf[11]);
grid->out_samps = &(tmpbuf[11 + grid->nrows]);

/* Calculate the sizes of the second grid record, allocate space, and read
 --------------------------------------------------------------------------*/
clen = 1;
rowcol = grid->nrows * grid->ncols;
rec_len = last_rec_len = DBL_RECSIZE;
rec_inc = DBL_RECSIZE / sizeof(double);
num_bytes = (36 + (grid->ncoeff * 2) + (2 * rowcol)) * sizeof(double);
num_temp = ((float) num_bytes) / DBL_RECSIZE;
num_recs = (int)num_temp;
if (num_temp != num_recs)
   {
   last_rec_len = num_bytes - (num_recs * DBL_RECSIZE);
   num_recs++;
   }

tmpdbl = (double *) MALLOC(num_bytes);

/* Read records from disk
  --------------------*/
for (buf_ptr = tmpdbl, i = 0; i < num_recs; i++, buf_ptr += rec_inc)
   {
   if ((i + 1) == num_recs) rec_len = last_rec_len;
   if (c_lsread(&fd, " ", &clen, &rec_len, dummy, (unsigned char *)buf_ptr, dummy) != E_SUCC)
      {
      c_errmsg("Error reading from the geometric mapping grid file", 
			"getgrd-read", NON_FATAL);
      return(E_FAIL);
      }
   }

/* Unpack read buffer into structure
  ---------------------------------*/ 
for(i = 0; i < 8; i++)
   grid->corners[i] = tmpdbl[i];
grid->pdist[0] = tmpdbl[8];
grid->pdist[1] = tmpdbl[9];
for(i = 0, j = 10; i < 15; i++, j++)
   grid->projprms[i] = tmpdbl[j];
grid->coeffs = &(tmpdbl[j]);
for(i = 0, j += (grid->ncoeff * 2); i < 2; j++, i++)
   grid->max_grid_err[i] = tmpdbl[j];
for(i = 0; i < 2; j++, i++)
   grid->ave_grid_err[i] = tmpdbl[j];
grid->rms_grid_err = tmpdbl[j++];
grid->grid_tol = tmpdbl[j++];
for(i = 0; i < 2; j++, i++)
   grid->max_fit_err[i] = tmpdbl[j];
for(i = 0; i < 2; j++, i++)
   grid->ave_fit_err[i] = tmpdbl[j];
grid->rms_fit_err = tmpdbl[j];
j++;
grid->in_lines = &(tmpdbl[j]);
grid->in_samps = &(tmpdbl[j + rowcol]);

/* Close the geometric mapping grid file
 --------------------------------------*/
if (c_lsclos(&fd, host_name, &action) != E_SUCC)
   {
   c_errmsg("Error closing the geometric mapping grid file", "getgrd-close",
             NON_FATAL);
   return(E_FAIL);
   }

return(E_SUCC);
}
