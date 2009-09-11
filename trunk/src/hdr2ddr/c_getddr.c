/*******************************************************************************
NAME			       C_GETDDR

PURPOSE	     Retrieve records 1 and 2 of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Jan. 1988 	original development
B. Ailts	      Apr. 1988		Replace newlas.h with las.h
					Changed the calling sequence -- only
					  one structure
					Replaced the ls___ status checks with
					  the constant E_SUCC
					Replaced cerrmsg with c_errmsg
					
D. Akkerman	      Sept. 1989	Added code to convert the double
					portions of the second record to the
					floating point format of the host 
					system.  This occurs if the DDR is 
					read from a "remote" (NFS) system or 
					if the DDR was created from a "remote" 
					(NFS) system and resides on the current
					host system.

T. Baltzer	      Aug. 1990		Added code to convert the long
					portions of the data to the long
					format of the current system.
B. Ailts	      Dec. 1990		updated error messages
D. Etrheim            Nov. 1991		Fix error message
D. Etrheim            Mar. 1993		Make proj_units lower case
P. Denny              Nov. 2002         Get ddr from new style meta file
                                         if possible    

PROJECT       LAS

ALGORITHM 
   Create the associated DDR file name from the input host image name.
   Open the DDR file for read access using label services.  
   Read the DDRINT record (first record).
   Retrieve the host system name.
   If the system specified in the DDR is not the same as the host system then:
	Set the convert flag to TRUE.
   Read the DDRDUB record (second one).
   If the convert flag is set to TRUE then:
	Retrieve the system code (integer) for the host system.
	Retrieve the system code (integer) for the DDR specified system.
	Convert the double precision elements of the second DDR record to the
	 floating point format of the host system.
	Copy the host system name into the system name element of the DDR
	 structure.
   Close the DDR file.

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "sysdef.h"


lasErr c_getddr_old(const char *hname,struct DDR *ddr)
{
	int   access;           /* file access type                        */
	int   action;           /* file close action flag                  */
	int   clen;             /* length of char part of record           */
	int   ctemp;            /* temporary storage for clen              */
	int   dlen;             /* length of data part of record           */
	int   dtemp;            /* temporary storage for dlen              */
	FILE *fd;               /* file descriptor for opened file         */
	int   len;
	int   mode;             /* mode for lsstat check                   */
	int   status;           /* function return status code             */
	int   size = DDSIZE;    /* number of doubles in record 2 of DDR    */
	int   dtyp = EDOUBLE;   /* data type of the buffer to be converted */
	int   convflg = FALSE;  /* working conversion flag                 */
	int   syscode;          /* DDR system code                         */
	char  dtype[3];         /* type of data portion of record          */
	char  hostddr[CMLEN];   /* host name of DDR file                   */
	char  temp[2];          /* temporary buffer                        */
	unsigned char *dbuf;    /* pointer to area where data is stuffed   */

	c_lsmknm(hname,".ddr",hostddr);      /* make associated DDR file name */

	mode = 0;                        /* make sure DDR file exists on disk */
	status = c_lsstat(hostddr,&mode);
	if (status != E_SUCC) {
	   fprintf(stderr,"****************** ERROR! ***************\n"
                	  "Couldn't open DDR file named '%s'.\n"
                	  "Does the file exist?  Is it readable?\n",hostddr);
	   exit(99);
	}

	access = 0;                                 /* open DDR file for read */
	if (c_lsopen(&fd,hostddr,&access) != E_SUCC) {
		c_errmsg("Error returned from lsopen","getddr-call",NON_FATAL);
		return(E_FAIL);
	}

	clen = DDSTCT * DDSYLN;            /* set up for and read in record 1 */
	ctemp = clen - 1;
	dlen = dtemp = DISIZE * 4;
	dbuf = (unsigned char *) MALLOC(dlen);
	if (c_lsread(&fd,"DDRINT",&clen,&dlen,ddr->system,dbuf,dtype) != E_SUCC) {
		c_errmsg("Error returned from lsread","getddr-call",NON_FATAL);
		return(E_FAIL);
	}
	if ( (clen != ctemp) || (dlen != dtemp) ) {
		c_errmsg("Error reading record 1 of associated DDR file",
			"getddr-read", NON_FATAL);
		return(E_FAIL);
	}
	byte2intArr(dbuf,(int *)ddr,DISIZE);
	free(dbuf);


	clen = ctemp = 0;                  /* set up for and read in record 2 */
	dlen = dtemp = DDSIZE * 8;        /* no character part for this record*/
	dbuf = (unsigned char *) &(ddr->proj_coef[0]);
	if (c_lsread(&fd,"DDRDUB",&clen,&dlen,temp,dbuf,dtype) != E_SUCC) {
		c_errmsg("Error returned from lsread","getddr-call",NON_FATAL);
		return(E_FAIL);
	}
	if ( (clen != ctemp) || (dlen != dtemp) ) {
		c_errmsg("Error reading record 2 of associated DDR file",
			"getddr-read",NON_FATAL);
		return(E_FAIL);
	}

	if (strcmp(c_getsys(),ddr->system) != 0) /* if the DDR system and the */
		convflg = TRUE;                /* host are not the same then  */
                                               /* set the conversion flag     */

	if (convflg) {                       /* if the conversion flag is set */
	   if (c_sysset(ddr->system,&syscode) != E_SUCC) { /* retrieve the DDR*/
	      c_errmsg("Error returned from sysset","getddr-call",NON_FATAL);
	      return(E_FAIL);
	   }

	   /* perform the conversion */
	   if (c_pxsys(syscode,dbuf,dtyp,size) != E_SUCC) {
	      c_errmsg("Error returned from pxsys","getddr-call",NON_FATAL);
	      return(E_FAIL);
	   }
	}

	/* Change the projection units to lower case */
	len = strlen(ddr->proj_units);
	c_up2low(ddr->proj_units,&len);

	action = 0;                              /* close associated DDR file */
	if (c_lsclos(&fd,hostddr,&action) != E_SUCC) {
	   c_errmsg("Error returned from lsclos","getddr-call",NON_FATAL);
	   return(E_FAIL);
	}

	return (E_SUCC);
}



lasErr c_getddr(const char *hname,struct DDR *ddr)
{
	return c_getddr_old(hname, ddr);
}
