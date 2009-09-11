/******************************************************************************
FUNCTION:	c_ealloc

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0		    T.Butzer		Original development

  2.0	   12/87    B.Ailts		Used raw 'C' types

  2.1      04/88    D.Hollaren		Replaced newlas.h with las.h
					Replaced cerrmsg with c_errmsg
					Renamed ealloc to c_ealloc

  2.2      05/88    B.Ailts		Replaced errh with c_errmsg
					Took out error.h
  2.3	   08/88    B.Ailts		Based memory allocation on total number
					of bytes and not number of lines
  2.4      01/89    B.Ailts             Now checks to be sure the buffer is
					large enough for UPDATE access
  2.5      02/91    B.Ailts             Standardized error messages
  2.6      06/93    K.Sheffield         Modified to allow the user-defined
                                        buffer sizes.
			
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:		LAS

ALGORITHM: 
Calculate the maximum number of bytes needed to hold the largest line size 
of each of the images in the FDARRAY
Make sure buffer is large enough to hold at least on line of data
If LASBUFSZ is set
   Extract the buffer size in megabytes from LASBUFSZ
   Clamp this amount to the system allocation limit and store in maxbufsize.
Else
   Set maxbufsize to MAXBUFS
   
If maxbufsize is less than the number of bytes of whole image
   Set buffer size to maxbufsize
Else
   Set buffer size to number of bytes of whole image
Allocate the number of bytes calculated above

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include "las.h"
#include "locinc.h"

#define MBYTE 1048576
#define MAXMEG 256              /* maximum # of megabytes to allocate */

FUNCTION char *c_ealloc(struct FDESC *fdarray[], int *buffersz)
{
   int i = 0;			/* counter -- current image		*/
   int tempacc = IREAD;	/* temporary access			*/
   int maxbufsize;		/* maximum buffer size 			*/
   int maxbytes = 0;		/* maximum number of bytes needed for a line */
   int maxnbuf = 0;
   int nbytes;			/* number of bytes needed for a line	*/
   int temp_bufsiz;		/* temporary buffer size		*/
   int total_bands = 0;	/* total bands being allocated		*/
   char *bufptr;		/* buffer pointer			*/
   


   while (fdarray[i] != 0)
   {
      total_bands = total_bands + fdarray[i]->nbands;
      if (fdarray[i]->acc == IUPDATE)
	tempacc = IUPDATE;
      if (fdarray[i]->acc == IREAD)
	nbytes = fdarray[i]->linsiz;
      else
      {
	 if (fdarray[i]->cnvlsz != 0)
	   nbytes = fdarray[i]->cnvlsz;
	 else
	   nbytes = fdarray[i]->linsiz;
      }
		
      if (fdarray[i]->nbuf > maxnbuf)
	maxnbuf = fdarray[i]->nbuf;
      if (nbytes > maxbytes)
	maxbytes = nbytes;
      i++;
   }  /* While fdarray[i] != 0  */

/*  this line is replaced by the lines below so that images smaller than
    BLKFACTOR lines will allocate less space and the whole image of smaller
    images can all be read into memory at once -- 8-1-88 B.Ailts
*buffersz = maxbytes * total_bands * BLKFACTOR;
-------------------------------------------------------------------------*/
   temp_bufsiz = maxbytes * total_bands * fdarray[0]->nl;

	maxbufsize = MAXBUFS;
   


   if ( maxbufsize < temp_bufsiz)
     *buffersz = maxbufsize;
   else
     *buffersz = temp_bufsiz;

   if (tempacc == IUPDATE)
   {
   /*  When IUPDATE access is specified the buffer is split into two buffers
       Each buffer will need to hold at least 1 line of data
   ------------------------------------------------------------------------*/
      if (*buffersz < (maxbytes * total_bands * 2)) 
	*buffersz = maxbytes * total_bands * 2 * (maxnbuf + 1);
   }
   else
   /*  When IUPDATE access is NOT specified the buffer will need to hold 
       at least 1 line of data
   ------------------------------------------------------------------------*/
     if (*buffersz < (maxbytes * total_bands)) 
       *buffersz = maxbytes * total_bands * (maxnbuf + 1);  

/* allocate the buffer
----------------------*/
   bufptr = (char *)MALLOC(*buffersz);
	
   return(bufptr);
}
