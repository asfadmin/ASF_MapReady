/*********************   Label Services Subsystem   *********************
FUNCTION:	lsread

PURPOSE:	Read from associated file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
           04/86    C. Greenhagen	initial development
           07/86    K. Gacke		newLAS development
	   10/87    K. Zanter           bug fix (so doesn't reset cubf->length
					or check char buffer if length = 0)
	   12/87    B. Ailts		place bridge routines in seperate file
					replace DESC arguments with char strings
					change include directory specifications
					use raw 'C' types
  5.0      04/88    D. Hollaren         change include spec to las
  5.1     11/88	   B. Ailts		Replaced las.h with worgen.h so these
					routines can be placed in world
  5.2     07/90    D. VanderZee         Standardized error handling
  5.4     01/91    K. Gacke		Changed i/o to buffered i/o for sun --
					sun has a problem with direct i/o when
					the close takes to int to be executed
  5.5     05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  5.6     09/92    T. Mittan		Typecast constant 0 to (char *) to 
					eliminate warnings on compile.
  5.7     11/92    D. Etrheim		Moved call to free so that allocated
					space for out_key is freed after each 
					loop. 
  5.8     11/93    T. Rockvam		Changed the sun version to unblock 
					signals when returning for any reason
  7.0	   4/95	   T. Logan		Removed TAE dependencies
  7.1	  6/97     O. Lawlor            Changed to standard I/O.  This makes
  					SunOS no different than other platforms.
  					I am quite sure that I will 
  					go to hell for this, but that's OK.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:   None.

PROJECT			LAS

ALGORITHM 
Read each record in the file until EOF or key match is found
    Read header record
    look for a match to the specified key else skip the record
	if the specified character length is less than the actual character
	data length or the specified data length is less than the actual
	data length
	    reset the character and/or data lengths to the actual
	    backup to the beginning of the record
	read the character data
	read the other data
Verify that the lengths of the data requested is the same as the data
lengths to be transferred.
Free up any allocated space.

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"

#include "worgen.h"
#include "vll.h"
lasErr FUNCTION c_lsread(FILE **fd,const char * key, int *clen, int *dlen, char *cbuf, 
		unsigned char *dbuf, char *dtype)
{
FILE   *fp;
int   charlen, datalen, obyte;
int   offset,bytesRead;
short   keymatch = FALSE;
char   header[HDRL];
char   msgtxt[ERRLEN + 1];
char   *in_key;
char   *out_key;
char   *ptr;

fp = (FILE *) *fd;
/*fseek(fp,0,0);*/
bytesRead=0;
in_key = squeeze(key,16);
do
    {
    charlen = datalen = 0;
    obyte=fread(header,sizeof(char),HDRL,fp);
    if (obyte!=HDRL)
    	return E_EOF;
    
     bytesRead+=HDRL;

    if (((ptr = strchr(header,'/')) != NULL) && (ptr - header < LENL))
        sscanf(header,"%d/%d%s",&charlen,&datalen,dtype);
    else
        sscanf(header,"%d%s",&datalen,dtype);
    out_key = squeeze(header+LENL+TYPL,KEYL - 1);
    if ((strlen(in_key) == 0) || (strcmp(in_key," ") == 0) 
                              || (strcmp(in_key,out_key) == 0))
        keymatch = TRUE;
    if (strcmp(out_key,"DELETED") == 0)
	keymatch = FALSE;
    if (keymatch) 
	{
	/*strcpy(key,out_key);*/
	if (((*clen <= charlen) && (charlen > 0)) || (*dlen < datalen)) 
					             /* buffers are too small */
	    {
	    if (*clen <= charlen)	/* all string to be null terminated   */
		*clen = charlen + 1;
	    if (*dlen < datalen)
		*dlen = datalen;
	    offset = -(HDRL);
	    FSEEK(fp,offset,1);
	    return(E_SMAL);
    	    }
	if (charlen > 0)                         /* read character portion    */
	    {
	    obyte = FREAD(cbuf,sizeof(char),charlen,fp);
	    *clen = obyte;
	    *(cbuf + *clen) = '\0';  
	    }
        else
	    *clen = 0;
	if (datalen > 0)                         /* read data portion         */
	    {
	    obyte = FREAD(dbuf,sizeof(char),datalen,fp);
	    *dlen = obyte;
	    }
	else
	    *dlen = 0;
	}
    else
	{
	offset = datalen + charlen;
	FSEEK(fp,offset,1);
	 bytesRead+=offset;
	}
    free(out_key);
    }
while (!keymatch);
if (*clen + *dlen != charlen + datalen)
    {
    c_errmsg("Error reading from label services file","lsread-read",NON_FATAL);
    sprintf(msgtxt,"    %d bytes requested, %d bytes transferred",
             charlen + datalen,*clen + *dlen);
    c_errmsg(msgtxt,"lsread-bytes",NON_FATAL);
    return(E_FAIL);
    }
free(in_key);

return(E_SUCC);
}
