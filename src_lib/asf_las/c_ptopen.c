/******************************************************************************
FUNCTION:			c_ptopen

PURPOSE:  Opens a tie point file and reads/writes header data

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development
  5.1      07/90    D. Etrheim		Standardized error message handling

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	Has to be run under TAE

PROJECT			LAS

ALGORITHM 
	Set up file descriptor pointer
	Assign access mode to descriptor
	Assign file name to descriptor
	If file access mode = IREAD or IUPDATE,
	   Check to see if the file exists
	   Open the tie point file for read access
	   Read the tie point header record (use appropriate routine)
	   If access mode = IUPDATE,
		Close the file
		Reopen the file for append
	Else (file access mode = IWRITE)
	   Check to see if file exists
	   Open the tie point file for write access
	   Write tie point header record (use appropriate routine)
	Return

ALGORITHM REFERENCES	LAS 5.0 imageio routines	
*******************************************************************************/
#include "asf.h"


#include "worgen.h"
#include "geompak.h"
#include "ptio.h"

lasErr FUNCTION c_ptopen(struct PTDESC **ptdesc, const char *fname,int * filetype, 
		int *acc,struct TPSHEAD *header)
{
int mode = 0;		/* access mode for lsstat & lsopen */

register struct PTDESC *fd;

*ptdesc = (struct PTDESC *)calloc(1,sizeof(struct PTDESC));
fd = *ptdesc;	/* used to simplify expressions */
fd->acc = *acc;
strcpy(fd->name, fname);

if ((fd->acc == IREAD) || (fd->acc == IUPDATE))
   {	

/* Check to see if the file aready exists.  
 ----------------------------------------*/
   if (c_lsstat(fname, &mode) != E_SUCC)
      {
      c_errmsg("Tie point file does not exist on disk", "ptopen-exist", NON_FATAL);
      return(E_FAIL);
      }

/* Open the output tie point file
 -------------------------------*/
   if (c_lsopen(&(fd->fptr), fname, &mode) != E_SUCC)
      {
      c_errmsg("Error opening tie point file for read access","ptopen-open",
                               NON_FATAL);
      return(E_FAIL);
      }

/* Determine file type, read header, pack descriptor fields 
  ---------------------------------------------------------*/
   if (*filetype == TPS)
	{
	if (get_tps_header(fd->fptr, (struct TPSHEAD *)header) != E_SUCC) return(E_FAIL);
	fd->routine = (ptRWroutine)read_tps;
	}

   if (*filetype == MTP)
	{
	if (get_mtp_header(fd->fptr,(struct MTPHEAD *) header) != E_SUCC) return(E_FAIL);
	fd->routine = (ptRWroutine)read_mtp;
	}

   if (*filetype == TPL)
	{
	if (get_tpl_header(fd->fptr, (struct TPLHEAD *)header) != E_SUCC) return(E_FAIL);
	fd->routine = (ptRWroutine)read_tpl;
	}

   if (fd->acc == IUPDATE)
	{
	if (c_lsclos(&(fd->fptr), fname, &mode) != E_SUCC)
	   {
	   c_errmsg("Error opening tie point file for update","ptopen-update",
				NON_FATAL);
	   return(E_FAIL);
	   }
	mode = 2;
   	if (c_lsopen(&(fd->fptr), fname, &mode) != E_SUCC)
	   {
	   c_errmsg("Error opening tie point file for update","ptopen-update",
				NON_FATAL);
	   return(E_FAIL);
	   }
	}
   }

else /* Open for write access... */
   {

/* Check to see if the file aready exists.  
 ----------------------------------------*/
   if (c_lsstat(fname, &mode) == E_SUCC)
      {
      c_errmsg("Tie point file already exists on disk", "ptopen-exist", NON_FATAL);
      return(E_FAIL);
      }

/* Open the output tie point file
 -------------------------------*/
   mode = 1;
   if (c_lsopen(&(fd->fptr), fname, &mode) != E_SUCC)
      {
      c_errmsg("Error opening tie point file for write access",
			"ptopen-open",NON_FATAL);
      return(E_FAIL);
      }

/* Determine file type, unpack descriptor fields, write header
  -----------------------------------------------------------*/
   if (*filetype == TPS)
	{
	if (put_tps_header(fd->fptr, (struct TPSHEAD *)header) != E_SUCC) return(E_FAIL);
	fd->routine = (ptRWroutine)write_tps;
	}

   if (*filetype == MTP)
	{
	if (put_mtp_header(fd->fptr, (struct MTPHEAD *)header) != E_SUCC) return(E_FAIL);
	fd->routine = (ptRWroutine)write_mtp;
	}

   if (*filetype == TPL)
	{
	if (put_tpl_header(fd->fptr, (struct TPLHEAD *)header) != E_SUCC) return(E_FAIL);
	fd->routine = (ptRWroutine)write_tpl;
	}

   }
return(E_SUCC);
}

char dummyDataType[20];

/******************************************************************************
FUNCTION:			get_tps_header

PURPOSE:  Reads header information from a tie point selection file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Read a tie point record
	Unpack the record into the tie point header structure 
	Return
*******************************************************************************/
lasErr FUNCTION get_tps_header(FILE *fp, struct TPSHEAD *header)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[17 + CMLEN];		/* Temporary character buffer */
double tmpdbl[29];		/* Temporary double precision buffer */
int cnt, count;		/* Loop counters */

clen = 17 + CMLEN;
dlen = 232;
if (c_lsread(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,dummyDataType) != E_SUCC)
   {
   c_errmsg("Error reading Tie Point Selection file","ptopen-read",NON_FATAL);
   return(E_FAIL);
   }
strncpy(header->mode, dummy, 4);
strncpy(header->units, &(dummy[4]), 12);
strncpy(header->name, &(dummy[16]), CMLEN);
for (count = 0; count < 8; count++)
   header->corners[count] = tmpdbl[count];
header->pdist[0] = tmpdbl[8];
header->pdist[1] = tmpdbl[9];
for (cnt = 0, count = 10; count < 25; count++, cnt++)
   header->projprms[cnt] = tmpdbl[count];
header->proj_valid = (int)tmpdbl[25];
header->code = (int)tmpdbl[26];
header->zone = (int)tmpdbl[27];
header->datum = (int)tmpdbl[28];

return(E_SUCC);
}

/******************************************************************************
FUNCTION:			get_mtp_header

PURPOSE:  Reads header information from a merged tie point file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Read a tie point record
	Unpack the record into the tie point header structure 
	Return
*******************************************************************************/
lasErr FUNCTION get_mtp_header(FILE *fp, struct MTPHEAD *header)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[22 + (2 * CMLEN)];	/* Temporary character buffer */
double tmpdbl[29];		/* Temporary double precision buffer */
int cnt, count;		/* Loop counters */

clen = 22 + (2 * CMLEN);
dlen = 232;
if (c_lsread(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,dummyDataType) != E_SUCC)
   {
   c_errmsg("Error reading Merged Tie Point file", "ptopen-read", NON_FATAL);
   return(E_FAIL);
   }
strncpy(header->chip_mode, dummy, 9);
strncpy(header->units, &(dummy[9]), 12);
strncpy(header->sea_name, &(dummy[21]), CMLEN);
strncpy(header->ref_name, &(dummy[21 + CMLEN]), CMLEN);
for (count = 0; count < 8; count++)
   header->corners[count] = tmpdbl[count];
header->pdist[0] = tmpdbl[8];
header->pdist[1] = tmpdbl[9];
for (cnt = 0, count = 10; count < 25; count++, cnt++)
   header->projprms[cnt] = tmpdbl[count];
header->proj_valid = (int)tmpdbl[25];
header->code = (int)tmpdbl[26];
header->zone = (int)tmpdbl[27];
header->datum = (int)tmpdbl[28];

return(E_SUCC);
}

/******************************************************************************
FUNCTION:			get_tpl_header

PURPOSE:  Reads header information from a tie point location file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Read a tie point record
	Unpack the record into the tie point header structure 
	Return
*******************************************************************************/
lasErr FUNCTION get_tpl_header(FILE *fp, struct TPLHEAD *header)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[54 + (2 * CMLEN)];	/* Temporary character buffer */
double tmpdbl[32];		/* Temporary double precision buffer */
int cnt, count;		/* Loop counters */

clen = 54 + (2 * CMLEN);
dlen = 256;
if (c_lsread(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,dummyDataType) != E_SUCC)
   {
   c_errmsg("Error reading Tie Point Location file","ptopen-read",NON_FATAL);
   return(E_FAIL);
   }

strncpy(header->ref_mode, dummy, 9);
strncpy(header->units, &(dummy[9]), 12);
strncpy(header->peak_method, &(dummy[21]), 25);
strncpy(header->corr_type, &(dummy[46]), 7);
strncpy(header->sea_name, &(dummy[53]), CMLEN);
strncpy(header->ref_name, &(dummy[53 + CMLEN]), CMLEN);
for (count = 0; count < 8; count++)
   header->corners[count] = tmpdbl[count];
header->pdist[0] = tmpdbl[8];
header->pdist[1] = tmpdbl[9];
for (cnt = 0, count = 10; count < 25; count++, cnt++)
   header->projprms[cnt] = tmpdbl[count];
header->edge_thres = tmpdbl[25];
header->min_str = tmpdbl[26];
header->max_dis = tmpdbl[27];
header->proj_valid = (int)tmpdbl[28];
header->code = (int)tmpdbl[29];
header->zone = (int)tmpdbl[30];
header->datum = (int)tmpdbl[31];

return(E_SUCC);
}


/******************************************************************************
FUNCTION:			put_tps_header

PURPOSE:  Write header information to a tie point selection file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Pack the tie point header record into temporary buffers for write
	Write the header record
	Return
*******************************************************************************/
lasErr FUNCTION put_tps_header(FILE *fp, struct TPSHEAD *header)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[16 + CMLEN];		/* Temporary character buffer */
double tmpdbl[29];		/* Temporary double precision buffer */
int cnt, count;		/* Loop counters */

strncpy(dummy, header->mode, 4);
strncpy(&(dummy[4]), header->units, 12);
strncpy(&(dummy[16]), header->name, CMLEN);
for (count = 0; count < 8; count++)
   tmpdbl[count] = header->corners[count];
tmpdbl[8] = header->pdist[0];
tmpdbl[9] = header->pdist[1];
for (cnt = 0, count = 10; count < 25; count++, cnt++)
   tmpdbl[count] = header->projprms[cnt];
tmpdbl[25] = (double)header->proj_valid;
tmpdbl[26] = (double)header->code;
tmpdbl[27] = (double)header->zone;
tmpdbl[28] = (double)header->datum;

clen = 16 + CMLEN;
dlen = 232;
if (c_lswrit(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,"R8") != E_SUCC)
   {
   c_errmsg("Error writing Tie Point Selection file","ptopen-write",NON_FATAL);
   return(E_FAIL);
   }
return(E_SUCC);
}

/******************************************************************************
FUNCTION:			put_mtp_header

PURPOSE:  Write header information to a merged tie point file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Pack the tie point header record into temporary buffers for write
	Write the header record
	Return
*******************************************************************************/
lasErr FUNCTION put_mtp_header(FILE *fp, struct MTPHEAD *header)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[21 + (2 * CMLEN)];	/* Temporary character buffer */
double tmpdbl[29];		/* Temporary double precision buffer */
int cnt, count;		/* Loop counters */

strncpy(dummy, header->chip_mode, 9);
strncpy(&(dummy[9]), header->units, 12);
strncpy(&(dummy[21]), header->sea_name, CMLEN);
strncpy(&(dummy[21 + CMLEN]), header->ref_name, CMLEN);
for (count = 0; count < 8; count++)
   tmpdbl[count] = header->corners[count];
tmpdbl[8] = header->pdist[0];
tmpdbl[9] = header->pdist[1];
for (cnt = 0, count = 10; count < 25; count++, cnt++)
   tmpdbl[count] = header->projprms[cnt];
tmpdbl[25] = (double)header->proj_valid;
tmpdbl[26] = (double)header->code;
tmpdbl[27] = (double)header->zone;
tmpdbl[28] = (double)header->datum;

clen = 21 + (2 * CMLEN);
dlen = 232;
if (c_lswrit(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,"R8") != E_SUCC)
   {
   c_errmsg("Error writing Merged Tie Point file", "ptopen-write", NON_FATAL);
   return(E_FAIL);
   }
return(E_SUCC);
}

/******************************************************************************
FUNCTION:			put_tpl_header

PURPOSE:  Write header information to a tie point location file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Pack the tie point header record into temporary buffers for write
	Write the header record
	Return
*******************************************************************************/
lasErr FUNCTION put_tpl_header(FILE *fp, struct TPLHEAD *header)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[53 + (2 * CMLEN)];	/* Temporary character buffer */
double tmpdbl[32];		/* Temporary double precision buffer */
int cnt, count;		/* Loop counters */

strncpy(dummy, header->ref_mode, 9);
strncpy(&(dummy[9]), header->units, 12);
strncpy(&(dummy[21]), header->peak_method, 25);
strncpy(&(dummy[46]), header->corr_type, 7);
strncpy(&(dummy[53]), header->sea_name, CMLEN);
strncpy(&(dummy[53 + CMLEN]), header->ref_name, CMLEN);
for (count = 0; count < 8; count++)
   tmpdbl[count] = header->corners[count];
tmpdbl[8] = header->pdist[0];
tmpdbl[9] = header->pdist[1];
for (cnt = 0, count = 10; count < 25; count++, cnt++)
   tmpdbl[count] = header->projprms[cnt];
tmpdbl[25] = header->edge_thres;
tmpdbl[26] = header->min_str;
tmpdbl[27] = header->max_dis;
tmpdbl[28] = (double)header->proj_valid;
tmpdbl[29] = (double)header->code;
tmpdbl[30] = (double)header->zone;
tmpdbl[31] = (double)header->datum;

clen = 53 + (2 * CMLEN);
dlen = 256;
if (c_lswrit(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,"R8") != E_SUCC)
   {
   c_errmsg("Error reading Tie Point Location file","ptopen-write",NON_FATAL);
   return(E_FAIL);
   }
return(E_SUCC);
}
