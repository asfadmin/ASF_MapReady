/*********************   Label Services Subsystem   *********************
FUNCTION:	lsrepl

PURPOSE:	Replace (rewrite) the record just read in the associated file.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
           04/86    C. Greenhagen	initial development
           07/86    K. Gacke		newLAS development
           12/87    B. Ailts		change the include directory spec
					use raw 'C' types
					replace DESC arguments with char strings
					place bridge routines in a seperate file
  5.0      04/88    D. Hollaren		change include spec to las
					changed cerrmsg to c_errmsg and
					passed error code by address
					return E_SUCC/E_FAIL
  5.1     11/88	   B. Ailts		Replaced las.h with worgen.h so these
					routines can be placed in world
  5.2     07/90    D. VanderZee         Standardized error handling
  5.3     01/91    K. Gacke		Changed i/o to buffered i/o for sun --
					sun has a problem with direct i/o when
					the close takes to int to be executed
  5.4     05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  5.5     10/91    T. Mittan            Added fseek before return for sun's.
					This prevents c_lread from reading past
  5.6     09/92    T. Mittan		Typecast constant 0 to (char *) to 
					eliminate warnings on compile.
					the end of the file.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
Truncate the key field if it's to long
Position to the start of the previous record
read header information
Make sure the length of the data to be re-written is the same as
the record length.
Backup the file pointer to the beginning of the record
Update the header information
Write the new header information
Write any character information
Write any other data information

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"


#include "worgen.h"
#include "vll.h"

lasErr FUNCTION c_lsrepl (FILE **fd, const char *key, int *clen, int *dlen, 
	const char *cbuf, const unsigned char *dbuf, const char *dtype)
{
FILE *fp;
long long charlen, datalen;
long long offset;
char header[HDRL], len[LENL], odtype[TYPL], msgtxt[ERRLEN + 1];
char *out_key;
char *ptr;

fp = (FILE *) *fd;
if (strlen(key) >= KEYL)
    c_errmsg("Key length too long.  Key should be truncated.","lsrepl-key",
		NON_FATAL);

offset = -(*clen + *dlen + HDRL);	/* position at start of record  */

FSEEK64(fp, offset, 1);

FREAD (header,sizeof(char),HDRL,fp);

charlen = datalen = 0;
if (((ptr = strchr(header,'/')) != NULL) && (ptr - header < LENL))
    sscanf(header,"%lld/%lld%s",&charlen,&datalen,odtype);
else
    sscanf(header,"%lld%s",&datalen,odtype);

out_key = squeeze(header+LENL+TYPL,KEYL - 1);
if (charlen + datalen != *clen + *dlen)			
    {
    c_errmsg("Error replacing record within label services file",
	     "lsrepl-replace",NON_FATAL);
    sprintf(msgtxt,"    %d bytes requested, record contains %lld",*clen + *dlen,
             datalen);
    c_errmsg(msgtxt,"lsrepl-bytes",NON_FATAL);
    return(E_FAIL);
    }

if ((strlen(key) != 0) && (strcmp(key," ") != 0))
    sprintf(out_key,"%-*s",KEYL-1,key);
if ((strlen(dtype) != 0) && (strcmp(dtype," ") != 0))
    sprintf(odtype,"%-*s",TYPL-1,dtype);

offset = -(HDRL);
FSEEK64(fp, offset, 1);

if (*clen > 0)
   sprintf(len,"%-d/%-d",*clen,*dlen);
else
   sprintf(len,"%-d",*dlen);

sprintf(header,"%-*s%-*s%-*s",LENL,len,TYPL,odtype,KEYL-1,out_key);
FWRITE(header,sizeof(char),HDRL,fp);

if (*clen > 0)
    FWRITE(cbuf,sizeof(char),*clen,fp);

if (*dlen > 0)
    FWRITE(dbuf,sizeof(char),*dlen,fp);

FSEEK64(fp,0,1);
FREE(out_key);

return(E_SUCC);
}
