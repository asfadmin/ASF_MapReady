/*********************   Label Services Subsystem   *********************
FUNCTION:	lswrit

PURPOSE:	Write a record to the associated file.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
           04/86    C. Greenhagen	initial development
	   07/86    K. Gacke		newLAS development
	   12/87    B. Ailts		change include directory specifcations
					replace DESC arguments with cha. strings
					use raw 'C' types
					place bridge routines in seperate file
  5.0      04/88   D. Hollaren		change include spec to las
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
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
Truncate the key if it's to long
Go to the end of the file
Write the header information
Write the character information
Write the data information

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include "worgen.h"
#include "vll.h"

lasErr FUNCTION c_lswrit(FILE	**fd, const char *key, int	*clen, int	*dlen, 
	const char *cbuf, const unsigned char *dbuf, const char *dtype)
{
FILE *fp;
char header[HDRL];
char len[LENL];

fp = *fd;
if (strlen(key) >= KEYL)
    {
    c_errmsg("Error: Key length too long.  Key should be truncated.",
             "lswrit-key", NON_FATAL);
    }

/*Seek to end-of-file.*/
FSEEK(*fd, (int)0, 2);

if (*clen > 0)
   sprintf(len,"%-d/%-d",*clen,*dlen);
else
   sprintf(len,"%-d",*dlen);

sprintf (header,"%-*s%-*s%-*s",LENL,len,TYPL,dtype,KEYL-1,key);
FWRITE(header,sizeof(char),HDRL,fp);

if (*clen > 0)
    FWRITE(cbuf,sizeof(char), *clen,fp);

if (*dlen > 0)
    FWRITE(dbuf,sizeof(char), *dlen,fp);

return(E_SUCC);
}
