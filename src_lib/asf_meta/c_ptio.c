/******************************************************************************
FUNCTION:			c_ptio

PURPOSE:  Reads or writes a tie point file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development
  5.1      09/93    D. Etrheim		Change write routines to verify pt_id
					content.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	Has to be run under TAE

PROJECT			LAS

ALGORITHM 
	Get address of file pointer
	Execute read/write routine pointed to by *fd->routine
	Return
*******************************************************************************/
#include "asf.h"


#include "worgen.h"
#include "geompak.h"
#include "ptio.h"

lasErr FUNCTION c_ptio(struct PTDESC **ptdesc,struct TPSDATA *pt_data)
{
register struct PTDESC *fd;
fd = *ptdesc;	/* used to simplify expressions */
return (fd->routine(fd->fptr,(int *)pt_data));
}

/******************************************************************************
FUNCTION:			read_tps

PURPOSE:  Reads tie point records from a tie point selection file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Read a tie point record
	Unpack the record into the tie point data structure 
	Return
*******************************************************************************/
lasErr FUNCTION read_tps(FILE *fp, struct TPSDATA *pt_data)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[21 + CMLEN];		/* Temporary character buffer */
char dummy2[20];
double tmpdbl[10];		/* Temporary double precision buffer */
int status;			/* Return status flag */

clen = 21 + CMLEN;
dlen = 80;
status = c_lsread(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,dummy2);
if (status == E_EOF) return (E_EOF);
if (status != E_SUCC)
   {
   c_errmsg("Error reading Tie Point Selection file","ptio-read",NON_FATAL);
   return(E_FAIL);
   }
strncpy(pt_data->pt_id, dummy, 20);
strncpy(pt_data->chip_name, &(dummy[20]), CMLEN);
pt_data->coord[0] = tmpdbl[0];
pt_data->coord[1] = tmpdbl[1];
pt_data->image_coord[0] = tmpdbl[2];
pt_data->image_coord[1] = tmpdbl[3];
pt_data->elevation = tmpdbl[4];
pt_data->zoom_factor = tmpdbl[5];
pt_data->chip_size[0] = (int)tmpdbl[6];
pt_data->chip_size[1] = (int)tmpdbl[7];
pt_data->offsets[0] = (int)tmpdbl[8];
pt_data->offsets[1] = (int)tmpdbl[9];
return(E_SUCC);
}

/******************************************************************************
FUNCTION:			read_mtp

PURPOSE:  Reads tie point records from a merged tie point file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Read a tie point record
	Unpack the record into the tie point data structure 
	Return
*******************************************************************************/
lasErr FUNCTION read_mtp(FILE *fp, struct MTPDATA *pt_data)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[21 + (2 * CMLEN)];	/* Temporary character buffer */
double tmpdbl[12];		/* Temporary double precision buffer */
char dummy2[20];
int status;			/* Return status flag */

clen = 21 + (2 * CMLEN);
dlen = 96;
status = c_lsread(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,dummy2);
if (status == E_EOF) return (E_EOF);
if (status != E_SUCC)
   {
   c_errmsg("Error reading Merged Tie Point file","ptio-read",NON_FATAL);
   return(E_FAIL);
   }
strncpy(pt_data->pt_id, dummy, 20);
strncpy(pt_data->ref_chip, &(dummy[20]), CMLEN);
strncpy(pt_data->sea_chip, &(dummy[20 + CMLEN]), CMLEN);
pt_data->geo_coord[0] = tmpdbl[0];
pt_data->geo_coord[1] = tmpdbl[1];
pt_data->nominal[0] = tmpdbl[2];
pt_data->nominal[1] = tmpdbl[3];
pt_data->ref_coord[0] = tmpdbl[4];
pt_data->ref_coord[1] = tmpdbl[5];
pt_data->elevation = tmpdbl[6];
pt_data->zoom_factor = tmpdbl[7];
pt_data->chip_size[0] = (int)tmpdbl[8];
pt_data->chip_size[1] = (int)tmpdbl[9];
pt_data->offsets[0] = (int)tmpdbl[10];
pt_data->offsets[1] = (int)tmpdbl[11];
return(E_SUCC);
}

/******************************************************************************
FUNCTION:			read_tpl

PURPOSE:  Reads tie point records from a tie point location file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Read a tie point record
	Unpack the record into the tie point data structure 
	Return
*******************************************************************************/
lasErr FUNCTION read_tpl(FILE *fp, struct TPLDATA *pt_data)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[21 + CMLEN];		/* Temporary character buffer */
char dummy2[20];
double tmpdbl[29];		/* Temporary double precision buffer */
int status;			/* Return status flag */

clen = 21 + CMLEN;
dlen = 232;
status = c_lsread(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,dummy2);
if (status == E_EOF) return (E_EOF);
if (status != E_SUCC)
   {
   c_errmsg("Error reading Tie Point Location file","ptio-read",NON_FATAL);
   return(E_FAIL);
   }
strncpy(pt_data->pt_id, dummy, 20);
strncpy(pt_data->ref_chip, &(dummy[20]), CMLEN);
pt_data->geo_coord[0] = tmpdbl[0];
pt_data->geo_coord[1] = tmpdbl[1];
pt_data->sea_coord[0] = tmpdbl[2];
pt_data->sea_coord[1] = tmpdbl[3];
pt_data->ref_coord[0] = tmpdbl[4];
pt_data->ref_coord[1] = tmpdbl[5];
pt_data->rms_error[0] = tmpdbl[6];
pt_data->rms_error[1] = tmpdbl[7];
pt_data->strength = tmpdbl[8];
pt_data->displacement = tmpdbl[9];
pt_data->nominal[0] = tmpdbl[10];
pt_data->nominal[1] = tmpdbl[11];
pt_data->elevation = tmpdbl[12];
pt_data->zoom_factor = tmpdbl[13];
pt_data->ref_size[0] = (int)tmpdbl[14];
pt_data->ref_size[1] = (int)tmpdbl[15];
pt_data->offsets[0] = (int)tmpdbl[16];
pt_data->offsets[1] = (int)tmpdbl[17];
pt_data->chip_loc[0] = (int)tmpdbl[18];
pt_data->chip_loc[1] = (int)tmpdbl[19];
pt_data->sea_size[0] = (int)tmpdbl[20];
pt_data->sea_size[1] = (int)tmpdbl[21];
pt_data->chip_coord[0] = (int)tmpdbl[22];
pt_data->chip_coord[1] = (int)tmpdbl[23];
pt_data->req_size[0] = (int)tmpdbl[24];
pt_data->req_size[1] = (int)tmpdbl[25];
pt_data->location[0] = (int)tmpdbl[26];
pt_data->location[1] = (int)tmpdbl[27];
pt_data->active = (int)tmpdbl[28];
return(E_SUCC);
}

/******************************************************************************
FUNCTION:			write_tps

PURPOSE:  Writes tie point records to a tie point selection file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Pack the tie point data structure into temporary buffers
	Write a tie point record
	Return
*******************************************************************************/
lasErr FUNCTION write_tps(FILE *fp, struct TPSDATA *pt_data)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[20 + CMLEN];		/* Temporary character buffer */
double tmpdbl[10];		/* Temporary double precision buffer */
int len;

char  *ptid_ptr;

/* Check the pt_id so that it is not empty string */
len = strlen(pt_data->pt_id);
ptid_ptr = squeeze(pt_data->pt_id,len);
if (ptid_ptr == NULL)
   c_errmsg("Error returned from squeeze","ptio-call",NON_FATAL);

if (strlen (ptid_ptr) == 0)
   {
   c_errmsg("Invalid point ID","ptio-ptid",NON_FATAL);
   return ((lasErr)BAD_ID);
   }

strncpy(dummy, ptid_ptr, 20);
strncpy(&(dummy[20]), pt_data->chip_name, CMLEN);
tmpdbl[0] = pt_data->coord[0];
tmpdbl[1] = pt_data->coord[1];
tmpdbl[2] = pt_data->image_coord[0];
tmpdbl[3] = pt_data->image_coord[1];
tmpdbl[4] = pt_data->elevation;
tmpdbl[5] = pt_data->zoom_factor;
tmpdbl[6] = (double)pt_data->chip_size[0];
tmpdbl[7] = (double)pt_data->chip_size[1];
tmpdbl[8] = (double)pt_data->offsets[0];
tmpdbl[9] = (double)pt_data->offsets[1];
clen = 20 + CMLEN;
dlen = 80;
if (c_lswrit(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,"R8") != E_SUCC)
   {
   c_errmsg("Error writing Tie Point Selection file","ptio-write",NON_FATAL);
   return(E_FAIL);
   }
free(ptid_ptr);
return(E_SUCC);
}

/******************************************************************************
FUNCTION:			write_mtp

PURPOSE:  Writes tie point records to a merged tie point file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Pack the tie point data structure into temporary buffers
	Write a tie point record
	Return
*******************************************************************************/
lasErr FUNCTION write_mtp(FILE *fp, struct MTPDATA *pt_data)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[20 + (2 * CMLEN)];	/* Temporary character buffer */
double tmpdbl[12];		/* Temporary double precision buffer */
int len;

char  *ptid_ptr;

/* Check the pt_id so that it is not empty string */
len = strlen(pt_data->pt_id);
ptid_ptr = squeeze(pt_data->pt_id,len);
if (ptid_ptr == NULL)
   c_errmsg("Error returned from squeeze","ptio-call",NON_FATAL);

if (strlen (ptid_ptr) == 0)
   {
   c_errmsg("Invalid point ID","ptio-ptid",NON_FATAL);
   return ((lasErr)BAD_ID);
   }

strncpy(dummy, pt_data->pt_id, 20);
strncpy(&(dummy[20]), pt_data->ref_chip, CMLEN);
strncpy(&(dummy[20 + CMLEN]), pt_data->sea_chip, CMLEN);
tmpdbl[0] = pt_data->geo_coord[0];
tmpdbl[1] = pt_data->geo_coord[1];
tmpdbl[2] = pt_data->nominal[0];
tmpdbl[3] = pt_data->nominal[1];
tmpdbl[4] = pt_data->ref_coord[0];
tmpdbl[5] = pt_data->ref_coord[1];
tmpdbl[6] = pt_data->elevation;
tmpdbl[7] = pt_data->zoom_factor;
tmpdbl[8] = (double)pt_data->chip_size[0];
tmpdbl[9] = (double)pt_data->chip_size[1];
tmpdbl[10] = (double)pt_data->offsets[0];
tmpdbl[11] = (double)pt_data->offsets[1];
clen = 20 + (2 * CMLEN);
dlen = 96;
if (c_lswrit(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,"R8") != E_SUCC)
   {
   c_errmsg("Error writing Merged Tie Point file","ptio-write",NON_FATAL);
   return(E_FAIL);
   }
free(ptid_ptr);
return(E_SUCC);
}

/******************************************************************************
FUNCTION:			write_tpl

PURPOSE:  Writes tie point records to a tie point location file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development

ALGORITHM 
	Pack the tie point data structure into temporary buffers
	Write a tie point record
	Return

*******************************************************************************/
lasErr FUNCTION write_tpl(FILE *fp, struct TPLDATA *pt_data)
{
int clen, dlen;		/* Length of character & data fields */
char dummy[20 + CMLEN];		/* Temporary character buffer */
double tmpdbl[29];		/* Temporary double precision buffer */

int len;

char  *ptid_ptr;

/* Check the pt_id so that it is not empty string */
len = strlen(pt_data->pt_id);
ptid_ptr = squeeze(pt_data->pt_id,len);
if (ptid_ptr == NULL)
   c_errmsg("Error returned from squeeze","ptio-call",NON_FATAL);

if (strlen (ptid_ptr) == 0)
   {
   c_errmsg("Invalid point ID","ptio-ptid",NON_FATAL);
   return ((lasErr)BAD_ID);
   }

strncpy(dummy, pt_data->pt_id, 20);
strncpy(&(dummy[20]), pt_data->ref_chip, CMLEN);
tmpdbl[0] = pt_data->geo_coord[0];
tmpdbl[1] = pt_data->geo_coord[1];
tmpdbl[2] = pt_data->sea_coord[0];
tmpdbl[3] = pt_data->sea_coord[1];
tmpdbl[4] = pt_data->ref_coord[0];
tmpdbl[5] = pt_data->ref_coord[1];
tmpdbl[6] = pt_data->rms_error[0];
tmpdbl[7] = pt_data->rms_error[1];
tmpdbl[8] = pt_data->strength;
tmpdbl[9] = pt_data->displacement;
tmpdbl[10] = pt_data->nominal[0];
tmpdbl[11] = pt_data->nominal[1];
tmpdbl[12] = pt_data->elevation;
tmpdbl[13] = pt_data->zoom_factor;
tmpdbl[14] = (double)pt_data->ref_size[0];
tmpdbl[15] = (double)pt_data->ref_size[1];
tmpdbl[16] = (double)pt_data->offsets[0];
tmpdbl[17] = (double)pt_data->offsets[1];
tmpdbl[18] = (double)pt_data->chip_loc[0];
tmpdbl[19] = (double)pt_data->chip_loc[1];
tmpdbl[20] = (double)pt_data->sea_size[0];
tmpdbl[21] = (double)pt_data->sea_size[1];
tmpdbl[22] = (double)pt_data->chip_coord[0];
tmpdbl[23] = (double)pt_data->chip_coord[1];
tmpdbl[24] = (double)pt_data->req_size[0];
tmpdbl[25] = (double)pt_data->req_size[1];
tmpdbl[26] = (double)pt_data->location[0];
tmpdbl[27] = (double)pt_data->location[1];
tmpdbl[28] = (double)pt_data->active;
clen = 20 + CMLEN;
dlen = 232;
if (c_lswrit(&fp,"",&clen,&dlen,dummy,(unsigned char *)tmpdbl,"R8") != E_SUCC)
   {
   c_errmsg("Error writing Tie Point Location file","ptio-write",NON_FATAL);
   return(E_FAIL);
   }
free(ptid_ptr);
return(E_SUCC);
}
