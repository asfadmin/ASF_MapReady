/*******************************************************************************
NAME			     C_PUTBDR

PURPOSE	     Output specified band dependent record of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Feb. 1988 	original development
B. Ailts	      Apr. 1988		Replaced newlas.h with las.h
					Changed the calling sequence to use only
					  one data structure -- BDDR
					Changed the return status codes of
					  the ls___ calls to E_SUCC
D. Akkerman	      Jan. 1990		Installed error checking for an
					incorrect band number.
B. Ailts	      Dec. 1990		Updated error messages

PROJECT      LAS

ALGORITHM:
   Ensure that a valid band number was specified.
   Create the associated DDR file name from the input host image name.
   Open the DDR file for write access using label services.  
   Set up for and write the specified record.  
   Close the DDR file.

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include "las.h"

lasErr FUNCTION int_c_putbdr(const char *hname,const struct BDDR *bddr)
{
int	access;		       /* file access type			      */
int	action;		       /* file close action			      */
int	clen;		       /* length of char part of record	      	      */
int	dlen;		       /* length of data part of record	      	      */
FILE *	fd;		       /* file descriptor			      */

unsigned char *dbuf;	       /* pointer to area where data is stuffed       */

char hostddr[CMLEN];           /* host name of DDR file			      */
char key[16];	       	       /* record key				      */
char tempchar[DDTSLN];	       /* temp. storage of char. portion or record    */
char *tempptr;

/* Ensure that a valid band number was specified.
-------------------------------------------------*/

if ((bddr->bandno < 1) || (bddr->bandno > MAXBND))
 {
  c_errmsg("Incorrect band number specified for BDDR","putbdr-badnum",NON_FATAL);
  return(E_FAIL);
 }

c_lsmknm(hname,".ddr",hostddr);

/* Open DDR file for write access 
---------------------------------*/
access = 2;				 
if (c_lsopen(&fd,hostddr,&access) != E_SUCC)
   {
   c_errmsg("Error returned from lsopen","putbdr-call",NON_FATAL);
   return(E_FAIL);
   }

/* Initailize the variables needed the write the band record
------------------------------------------------------------*/
clen = DDTSLN - 1;
dlen = DBSIZE * 8;
dbuf = (unsigned char *) &(bddr->minval);
sprintf(key,"BAND%d",bddr->bandno);

/*  Place structure members into a character buffer
---------------------------------------------------*/
sprintf(tempchar,"%4d%2d",bddr->bandno,bddr->valid);
tempptr = tempchar + DDBNLN + DDVLLN;
strcpy(tempptr,bddr->source);
tempptr += DDSRLN;
strcpy(tempptr,bddr->instrument);
tempptr += DDINLN;
strcpy(tempptr,bddr->direction);
tempptr += DDDRLN;
strcpy(tempptr,bddr->date);
tempptr += DDCDLN;
strcpy(tempptr,bddr->time);

c_lswrit(&fd,key,&clen,&dlen,tempchar,dbuf,"R8");

action = 0;				  /* close associated DDR file        */
c_lsclos(&fd,hostddr,&action);

return(E_SUCC);
}
