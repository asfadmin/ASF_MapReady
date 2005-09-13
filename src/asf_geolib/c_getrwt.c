/****************************************************************************
NAME				c_getrwt

PURPOSE		Read a resampling weight table file from disk

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand	     	2/89	Original development
D. Etrheim		7/90	Standardized error message handling

PROJECT		LAS

ALGORITHM
   Check for existance of resampling weight table file
   Open the resampling weight table file for read access using label services.
   Read the first resampling weight table record
   Read the second resampling weight table record
   Read the third resampling weight table record
   Read the fourth resampling weight table record
   Close the resampling weight table file
******************************************************************************/
#include "asf.h"


#include "worgen.h"
#include "geompak.h"

int c_getrwt(host_name, table)

char *host_name;	/* Host file name of the table file */
struct RWTAB*table;	/* Resmapling weight table */
{
int i,j,k;		/* Loop counters */
int mode = 0;		/* access mode for lsstat */
int access = 0;	/* access mode for lsopen, set to read access */
FILE * fd;		/* file descriptor */
int clen;		/* number of bytes of character data per record */
int dlen;		/* number of bytes of data per record */
int action = 0;	/* action parameter for lsclos */
int tmpbuf[3];		/* Temporary buffer for read */
double tmpdbl[528];	/* Temporary buffer for read -- doubles */
char dummy[30];		/* Dummy argument for ls read */

/* Check to see if the file aready exists.  If it does exist, an error message
   is printed.
 -----------------*/
if (c_lsstat(host_name, &mode) != E_SUCC)
   {
   c_errmsg("Resampling weight table file does not exist on disk",
            "getrwt-exist", NON_FATAL);
   return(E_FAIL);
   }

/* Open the output resampling weight table file
 --------------------------------------------*/
if (c_lsopen(&fd, host_name, &access) != E_SUCC)
   {
   c_errmsg("Error opening resampling weight table file for read access",
            "getrwt-open", NON_FATAL);
   return(E_FAIL);
   }

/* Read the first resampling weight table file record
 ---------------------------------------------------*/
clen = 1;
dlen = 12;
if (c_lsread(&fd, "RWT_INT", &clen, &dlen, dummy, (unsigned char *)tmpbuf, dummy) != E_SUCC)
   {
   c_errmsg("Error reading from the resampling weight table file", 
            "getrwt-read", NON_FATAL);
   return(E_FAIL);
   }

/* Unpack the first record into the table stucture
  ------------------------------------------------*/
table->dim[0] = tmpbuf[0];
table->dim[1] = tmpbuf[1];
table->dim[2] = tmpbuf[2];

/* Read the second record
 -----------------------*/
clen = 1;
dlen = 33 * sizeof(double) * table->dim[0];

if (c_lsread(&fd, "RWT_TAB1", &clen, &dlen, dummy, (unsigned char *)tmpdbl, dummy) != E_SUCC)
      {
      c_errmsg("Error reading from the resampling weight table file", 
			"getrwt-read", NON_FATAL);
      return(E_FAIL);
      }

/* Unpack temp buffer into structure
  ---------------------------------*/ 
for(k = 0, j = 0; j < 33; j++)
   for (i = 0; i < table->dim[0]; k++, i++)
	table->table1[j][i] = tmpdbl[k];

/* Read the thrid record
 -----------------------*/
clen = 1;
dlen = 33 * sizeof(double) * table->dim[1];

if (c_lsread(&fd, "RWT_TAB2", &clen, &dlen, dummy, (unsigned char *)tmpdbl, dummy) != E_SUCC)
      {
      c_errmsg("Error reading from the resampling weight table file", 
			"getrwt-read", NON_FATAL);
      return(E_FAIL);
      }

/* Unpack temp buffer into structure
  ---------------------------------*/ 
for(k = 0, j = 0; j < 33; j++)
   for (i = 0; i < table->dim[1]; k++, i++)
	table->table2[j][i] = tmpdbl[k];

/* Read the fourth record
 -----------------------*/
clen = 1;
dlen = 33 * sizeof(double) * table->dim[2];

if (c_lsread(&fd, "RWT_TAB3", &clen, &dlen, dummy, (unsigned char *)tmpdbl, dummy) != E_SUCC)
      {
      c_errmsg("Error reading from the resampling weight table file", 
			"getrwt-read", NON_FATAL);
      return(E_FAIL);
      }

/* Unpack temp buffer into structure
  ---------------------------------*/ 
for(k = 0, j = 0; j < 33; j++)
   for (i = 0; i < table->dim[2]; k++, i++)
	table->table3[j][i] = tmpdbl[k];

/* Close the resampling weight table file
 --------------------------------------*/
if (c_lsclos(&fd, host_name, &action) != E_SUCC)
   {
   c_errmsg("Error closing the resampling weight table file", "getrwt-close",
             NON_FATAL);
   return(E_FAIL);
   }

return(E_SUCC);
}
