/******************************************************************************
FUNCTION:	do_open

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0     	    T.Butzer	        Original development
  2.0	   04/88    D.Hollaren		Replaced newlas.h with las.h
 					Replaced cerrmsg with c_errmsg
  2.1      05/88    B.Ailts 	        Replaced errh with c_errmsg
					Took out eerror.h
  2.2      02/91    B.Ailts 	        Standardized error messages
  2.3      05/92    T. Mittan           Output more descriptive message
                                        when not enough room is available on a
                                        disk on VMS
  2.4	   10/92    T. Mittan		Added compression option
  2.5	   12/92    T. Mittan		Modified memory allocation for 
					fd->foffset
	    1/94    K.Gacke		Modified to read/write dal images.  TAE
					images can be read, but all images are
					written as a BSQ DAL image.
  7.0	    4/95    T. Logan		Removed dependencies on TAE code by
					stripping out all calls to TAE I/O
					routines.  This limits usage to DAL
					image types only.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS: None.

PROJECT			LAS

ALGORITHM 
If write access Open a new file
Else Open an existing file

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"
#include <fcntl.h>
#include "las.h"
#include "locinc.h"

#define I_INPUT   0	/*ASF*/
#define I_INOUT   1	/*ASF*/

short glun = 7;

lasErr FUNCTION do_open(struct FDESC *fd)
{
/*int type, chans, lines, linsiz, labels, labsiz, lun;*/
int  opflg;		       /* open flag */
/*char buf[64];*/                  /* buffer to check type of file */

if (fd->acc == IWRITE)
      {
	/*lun = glun++;*/
	/* org = ORG; */	/*ASF*/
	/*chans = fd->nbands;*/
	/*lines = fd->nl;*/
	/*if (fd->flags & CONVERSION)
		linsiz = fd->ns * datasize[fd->conv_type];
	else
		linsiz = fd->ns * datasize[fd->dtype];*/
	/*labels = NLABELS;*/
	/*labsiz = LABSIZ;*/
	if (NULL==findExt(fd->fname))
		strcpy(fd->fname,appendExt(fd->fname,".img"));
	opflg = O_RDWR | O_CREAT | O_TRUNC;
#ifdef __MWERKS__
        if ((fd->dalfd = open(fd->fname,opflg)) == -1)
#else
        if ((fd->dalfd = open(fd->fname,opflg,0777)) == -1)
#endif
          {
	   perror("doopen-dal");
           c_errmsg("Error opening file","doopen-open",NON_FATAL);
           return(E_FAIL);
          }
      }
else
      {
	/*lun = glun++;*/
	/*if (fd->acc == IREAD) type = I_INPUT;
	else type = I_INOUT;*/	

        /* open file and check to see if it is in compresssed format
        ----------------------------------------------------------*/
        fd->compres = fd->tae = fd->dal = FALSE;
        fd->dal = TRUE;
	if (fd->acc == IREAD) opflg = O_RDONLY;
	else opflg = O_RDWR;
	if (extExists(fd->fname,".img"))
		strcpy(fd->fname,appendExt(fd->fname,".img"));
	else if (extExists(fd->fname,".dem"))
		strcpy(fd->fname,appendExt(fd->fname,".dem"));
#ifdef __MWERKS__
        if ((fd->dalfd = open(fd->fname,opflg)) == -1)
#else
        if ((fd->dalfd = open(fd->fname,opflg,0777)) == -1)
#endif
          {
	   perror("doopen-dal");
           c_errmsg("Error opening file","doopen-open",NON_FATAL);
           return(E_FAIL);
          }
      }
return(E_SUCC);
}
