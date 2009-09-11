/*******************************************************************************
NAME			       C_GETBDR

PURPOSE	     Retrieve specified band dependent record of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Feb. 1988 	original development
B. Ailts	      Apr. 1988		Replaced newlas.h with las.h
				  	Changed the calling sequence -- used
					  only one structure
					Replaced ls___ status to E_SUCC
					Replaced cerrmsg with c_errmsg

D. Akkerman	      Sept. 1989	Added code to convert the minval and
					maxval (double) portions of the BDR
					structure to the floating point format
					of the host system.  This occurs if the
					DDR is read from a "remote" (NFS)
					system or if the DDR was created from
					the "remote" (NFS) system and resides
					on the current host system.
B. Ailts	      Dec. 1990		Updated error messages

PROJECT      LAS

ALGORITHM 
   Create the associated DDR file name from the input host image name.
   Retrieve the host system name.
   Open the DDR file for read access using label services.  
   Read the integer portion of the DDR (record 1).
   Retrieve the system name from the DDR.
   If the host system name and the DDR specified system name are not the same:
	Set the conversion flag to TRUE.
   Set up for and read the specified record the corresponds to the input band #.
   If the conversion flag is set:
	Retrieve the system code (integer) for the host name.
	Retrieve the system code (integer) for the DDR specified system.
	Convert the double precision elements of the BDR record to the floating
	 point format of the host system.
   Close the DDR file.

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include "las.h"
#include "sysdef.h"

lasErr int_c_getbdr(const char  *hname,struct BDDR *bddr, int  *band)
{
struct DDR ddr;		       /* working DDR structure			      */

int	access;		       /* file access type			      */
int	action;		       /* file close action flag		      */
int	clen;		       /* length of character part of record	      */
int	ctemp;		       /* temporary storage for clen		      */
int	dlen;		       /* length of data part of record	      	      */
int	dtemp;		       /* temporary storage for dlen		      */
FILE *	fd;		       /* file descriptor for opened file	      */
/*int    status;*/
int	size = DBSIZE;	       /* number of doubles in the BDR		      */
int	dtyp = EDOUBLE;        /* data type of the buffer to be converted     */
int	convflg = FALSE;       /* working conversion flag		      */
int	syscode;	       /* DDR system code			      */

unsigned char *dbuf;	       /* pointer to area where data is stuffed       */

char dtype[3];	       	       /* type of data portion of record	      */
char hostddr[CMLEN];           /* host name of DDR file			      */
char key[16];	       	       /* record key				      */
char tempchar[DDTSLN];	       /* character portion of the record	      */
char *tempptr;

c_lsmknm(hname,".ddr",hostddr);	        /* make associated DDR file name */

access = 0;				/* open DDR file for read */
c_lsopen(&fd,hostddr,&access);

clen = DDSTCT * DDSYLN;	       /* set up and read record 1 of the DDR */
ctemp = clen - 1;
dlen = dtemp = DISIZE * 4;
dbuf = (unsigned char *) &ddr;
c_lsread(&fd,"DDRINT",&clen,&dlen,ddr.system,dbuf,dtype);

if ((clen != ctemp) || (dlen != dtemp))
   {
   c_errmsg("Error reading record of associated DDR file","getbdr-read",NON_FATAL);
   return(E_FAIL);
   }

if (strcmp(c_getsys(),ddr.system) != 0)	/* if the DDR system and the host    */
   convflg = TRUE;			/* system are not the same then set  */
					/* the conversion flag		     */

sprintf(key,"BAND%d",*band);   /* set up for and read in specified band record*/
clen = DDTSLN;
ctemp = clen - 1;
dlen = dtemp = DBSIZE * 8;
dbuf = (unsigned char *) &(bddr->minval);
c_lsread(&fd,key,&clen,&dlen,tempchar,dbuf,dtype);

if ( (clen != ctemp) || (dlen != dtemp) )
   {
   c_errmsg("Error reading record of associated DDR file","getbdr-read",NON_FATAL);
   return(E_FAIL);
   }

if (convflg)				/* if the conversion flag is set */
   {
   if (c_sysset(ddr.system,&syscode) != E_SUCC) /* retrieve the DDR   */
      { c_errmsg("Error returned from sysset","getbdr-call",NON_FATAL);return(E_FAIL);}

   if (c_pxsys(syscode,dbuf,dtyp,size) != E_SUCC)		  /* perform the conversion   */
      { c_errmsg("Error returned from pxsys","getbdr-call",NON_FATAL);return(E_FAIL); }
   }

sscanf(tempchar,"%4d%2d",&(bddr->bandno),&(bddr->valid));
tempptr = tempchar + DDBNLN + DDVLLN;
strcpy(bddr->source,tempptr);
tempptr += DDSRLN;
strcpy(bddr->instrument,tempptr);
tempptr += DDINLN;
strcpy(bddr->direction,tempptr);
tempptr += DDDRLN;
strcpy(bddr->date,tempptr);
tempptr += DDCDLN;
strcpy(bddr->time,tempptr);

action = 0;				 /* close associated DDR file */
c_lsclos(&fd,hostddr,&action);

return(E_SUCC);
}

