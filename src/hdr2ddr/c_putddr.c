/*******************************************************************************
NAME			      C_PUTDDR

PURPOSE	     Output records 1 and 2 of a DDR file

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
K. Zanter	      Jan. 1988 	Original development
B. Ailts	      Apr. 1988		Replaced newlas.h with las.h
					Changed the calling sequence -- only
					  one structure
					Replaced the ls___ status checks with
					  the constat E_SUCC
					Replaced cerrmsg with c_errmsg
D. Akkerman	      Jan. 1990		Installed code to ensure that
					required parameters are specified.
B. Ailts	      Dec. 1990		Updeated error messages
P. Denny              Jan. 2003         Add metadata interaction for phasing
                                         out of DDRs

PROJECT       LAS

ALGORITHM 
   Ensure that required parameters were specified.
   Create the associated DDR file name from the input host image name.
   Open the DDR file for write access using label services.  
   Write the DDRINT record (first record) 
   Write the DDRDUB record (second record).  
   Close the DDR file.

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "las.h"
#include "diskio.h"

lasErr c_putddr(const char *hname,struct DDR *ddr)
{
    int   access;                   /* file access type                      */
    int   action;                   /* file close action                     */
    int   clen;                     /* length of char part of record         */
    int   dlen;                     /* length of data part of record         */
    FILE *fd;                       /* file descriptor                       */
    char  d_temp[DDSTCT][DDSYLN];   /* temporary for squeezed strings        */
    char *junk_temp,hostddr[1024];
    unsigned char *dbuf;            /* pointer to area where data is stuffed */
    int ii;                         /* Index for corresponding metadata      */

    /* Ensure that required parameters were specified.
    --------------------------------------------------*/
    if ((ddr->nl < 1) || (ddr->nl > MAXNL)) {
      c_errmsg("Invalid number of lines specified","putddr-badnl",NON_FATAL);
      return(E_FAIL);
    }
    if ((ddr->ns < 1) || (ddr->ns > MAXNS)) {
      c_errmsg("Invalid number of samples specified","putddr-badns",NON_FATAL);
      return(E_FAIL);
    }
    if ((ddr->nbands < 1) || (ddr->nbands > MAXBND)) {
      c_errmsg("Invalid number of bands specified","putddr-badbnds",NON_FATAL);
      return(E_FAIL);
    }
    if ((ddr->dtype < 1) || (ddr->dtype > 20)) {
      c_errmsg("Invalid data type specified","putddr-bdtype",NON_FATAL);
      return(E_FAIL);
    }

    strcpy(ddr->system,c_getsys());

    c_lsmknm(hname,".ddr",hostddr);

    access = 1;                          /* open DDR file for write access   */
    c_lsopen(&fd,hostddr,&access);

    /*  Place the string portion of the DDR into a temporary buffer
    ---------------------------------------------------------------*/
    junk_temp = (char *)&ddr->spare;
    strncpy(junk_temp,ddr->system,4);
    strcpy(d_temp[0],squeeze(ddr->system,DDSYLN));
    strcpy(d_temp[1],squeeze(ddr->proj_units,DDPULN));
    strcpy(d_temp[2],squeeze(ddr->last_used_date,DDLDLN));
    strcpy(d_temp[3],squeeze(ddr->last_used_time,DDLTLN));

    clen = DDSTCT * DDSYLN - 1;                /* set up and output record 1 */
    dlen = DISIZE * 4;
    dbuf = (unsigned char *) MALLOC(dlen);
    int2byteArr((int *)ddr,dbuf,DISIZE);
    c_lswrit(&fd,"DDRINT",&clen,&dlen,d_temp[0],dbuf,"I4");
    free(dbuf);

    /* Set up and output record 2
       There is no character part to this record 
    --------------------------------------------*/
    clen = 0;				  
    dlen = DDSIZE * 8;		  
    dbuf = (unsigned char *) &(ddr->proj_coef[0]);
    c_lswrit(&fd,"DDRDUB",&clen,&dlen,d_temp[0],dbuf,"R8");

    action = 0;                                 /* close associated DDR file */
    c_lsclos(&fd,hostddr,&action);

    {/*Create/write correct number of BDRs*/
	    int bandNo;
	    for (bandNo=1;bandNo<=ddr->nbands;bandNo++)
	    {
		    struct BDDR bdr;
		    int_c_intbdr(&bdr);
		    bdr.bandno=bandNo;
		    int_c_putbdr(hname,&bdr);
	    }
    }

    return(E_SUCC);
}


