/****************************************************************************
NAME				c_putgrd

PURPOSE		Write a geometric mapping grid file to disk

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand	     	10/88		Original development
D. Etrheim		07/90		Standardized error message handling

PROJECT		LAS

ALGORITHM
   Check for existance of the output file.  Abort if already there.
   Open (create) the geometric mapping grid file for write access using
    label services.
   Write the first geometric mapping grid record
   Write the second geometric mapping grid record
   Close the geometric mapping grid file
******************************************************************************/


#include "proj.h"
#include "worgen.h"
#include "geompak.h"

int c_putgrd(host_name, grid)

  char *host_name;		/* Host file name of the mapping grid file */
  struct GEOGRID *grid;		/* Geometric mapping grid */

{
int mode = 0;		/* access mode for lsstat */
int access = 1;	/* access mode for lsopen, set to write access */
FILE * fd;		/* file descriptor */
int rowcol;		/* grid->nrows * grid->ncols to reduce computations */
int clen;		/* number of bytes of character data per record */
int dlen;		/* number of bytes of data per record */
int action = 0;	/* action parameter for lsclos */
int *tmpbuf;		/* Temporary buffer for write */
int i,j;		/* Loop counters */
int num_bytes;		/* Number of bytes in double portion of grid */
int num_recs;		/* Number of label services records required */
int rec_inc;		/* Record length / sizeof (double) */
int rec_len;		/* Record length of double portion of grid */
int last_rec_len;	/* Length of last record, double part of grid */
float num_temp;		/* Temp var to hold floating point num_recs */
double *tmpdbl;		/* Temporary buffer for write--double */
double *buf_ptr;	/* Pointer to temporary buffer used during write */
char dummy[30];		/* Dummy argument for second ls write */

/* Check to see if the file aready exists.  If it does exist, an error message
   is printed.
 -----------------*/
if (c_lsstat(host_name, &mode) == E_SUCC)
   {
   c_errmsg("Geometric mapping grid file already exists","putgrd-exist", NON_FATAL);
   return(E_FAIL);
   }

/* Open the output geometric mapping grid file
 --------------------------------------------*/
if (c_lsopen(&fd, host_name, &access) != E_SUCC)
   {
   c_errmsg("Error opening geometric mapping grid file for write access",
            "putgrd-open", NON_FATAL);
   return(E_FAIL);
   }


/* Calculate the size of the integer portion of the first geometric mapping
   grid record 
 -------------*/
clen = 27;
dlen = (11 + grid->nrows + grid->ncols) * sizeof(int);

/* Allocate temporary buffer and transfer data to it
  --------------------------------------------------*/
tmpbuf = (int *) MALLOC(dlen);

tmpbuf[0] = grid->proj_valid;
tmpbuf[1] = grid->fiterr_valid;
tmpbuf[2] = grid->griderr_valid;
tmpbuf[3] = grid->ncoeff;
tmpbuf[4] = grid->code;
tmpbuf[5] = grid->zone;
tmpbuf[6] = grid->datum;
tmpbuf[7] = grid->nrows;
tmpbuf[8] = grid->ncols;
tmpbuf[9] = grid->lines;
tmpbuf[10] = grid->samples;
for(i = 0, j = 11; i < grid->nrows; i++, j++)
   tmpbuf[j] = grid->out_lines[i];
for(i = 0; i < grid->ncols; i++, j++)
   tmpbuf[j] = grid->out_samps[i];

strncpy(dummy, grid->model, 15);
strncpy(&(dummy[15]), grid->units, 12);

/* Write the first geometric mapping grid file record
 ---------------------------------------------------*/
if (c_lswrit(&fd, "GEO_GRID_INT", &clen, &dlen, dummy, (unsigned char *)tmpbuf, "I4") != E_SUCC)
   {
   c_errmsg("Error writing to the geometric mapping grid file", "putgrd-write",
             NON_FATAL);
   return(E_FAIL);
   }

/* Calculate the sizes of the second geometric mapping grid record
 -----------------------------------------------------------------*/
clen = 0;
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

/* Allocate temporary buffer and transfer data to it
  --------------------------------------------------*/
tmpdbl = (double *) MALLOC(num_bytes);

for(i = 0; i < 8; i++)
   tmpdbl[i] = grid->corners[i];
tmpdbl[8] = grid->pdist[0];
tmpdbl[9] = grid->pdist[1];
for(i = 0, j = 10; i < 15; i++, j++)
   tmpdbl[j] = grid->projprms[i];
for(i = 0; i < (grid->ncoeff * 2); i++, j++)
   tmpdbl[j] = grid->coeffs[i];
for(i = 0; i < 2; i++, j++)
   tmpdbl[j] = grid->max_grid_err[i];
for(i = 0; i < 2; i++, j++)
   tmpdbl[j] = grid->ave_grid_err[i];
tmpdbl[j++] = grid->rms_grid_err;
tmpdbl[j++] = grid->grid_tol;
for(i = 0; i < 2; i++, j++)
   tmpdbl[j] = grid->max_fit_err[i];
for(i = 0; i < 2; i++, j++)
   tmpdbl[j] = grid->ave_fit_err[i];
tmpdbl[j] = grid->rms_fit_err;
for(i = 0, j++; i < rowcol; i++, j++)
   tmpdbl[j] = grid->in_lines[i];
for(i = 0; i < rowcol; i++, j++)
   tmpdbl[j] = grid->in_samps[i];

/* Write records to disk
  --------------------*/
for (buf_ptr = tmpdbl, i = 0; i < num_recs; i++, buf_ptr += rec_inc)
   {
   if ((i + 1) == num_recs) rec_len = last_rec_len;
   if (c_lswrit(&fd, " ", &clen, &rec_len, dummy, (unsigned char *)buf_ptr, "R8") != E_SUCC)
      {
      c_errmsg("Error writing to the geometric mapping grid file", 
                      "putgrd-write", NON_FATAL);
      return(E_FAIL);
      }
   }

/* Close the geometric mapping grid file and free temp buffer space
 -----------------------------------------------------------------*/
if (c_lsclos(&fd, host_name, &action) != E_SUCC)
   {
   c_errmsg("Error closing the geometric mapping grid file", "putgrd-close",
             NON_FATAL);
   return(E_FAIL);
   }

free(tmpbuf);
free(tmpdbl);

return(E_SUCC);
}
