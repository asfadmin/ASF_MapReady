/******************************************************************************
FUNCTION:	allocate_buffer

PURPOSE		Allocate_buffer is an internal LAS imageio
		routine that sets up the initial offsets for
		sequential access.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0	   06/85    T. Butzer		Original implementation

  1.1	   03/86    K. Johnson		Multiple file groups

  2.0	   12/87    B. Ailts	  	change include file specifications
					change to raw 'C' types

  2.1      04/88    D.Hollaren		Replaced newlas.h with las.h

  2.2	   05/88    B.Ailts		Replaced errh with c_errmsg
					Deleted eerror.h
  2.3      08/88    B.Ailts		Added error checking on the calloc calls
  2.4      02/91    B.Ailts		Standardized error messages
  6.0      6/97    O. Lawlor  Function Prototypes!  Whee!


COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	Has to be run under TAE

PROJECT			LAS

ALGORITHM 
If UPDATE mode
   Allocate space for UPBUF
   Calculate the amount of space needed for NLINES lines of data
   Divid nlines by 2 to allow for update mode
   Do for every band
      Set the offset pointers
   Allocate conversion buffer if needed
Else
   Calculate the amount of space needed for NLINES lines of data
   Do for every band
      Set the offset pointers
   Allocate conversion buffer if needed

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"


#include "las.h"
#include "locinc.h"

#define NLINES gd->gbl_nlines

lasErr FUNCTION allocate_buffer(struct FDESC *fd,struct GDESC *gd)
{
register int i;
int nlines;		/* represents number of lines to be read on */

char errtxt[ERRLEN + 1];

/* allocate buffer structures and initialize there values
---------------------------------------------------------*/
if (fd->acc == IUPDATE)
	{

/* update access requires additional buffers 
--------------------------------------------*/
	fd->upbuf = (struct UPBUF *) calloc(fd->nbands, sizeof(struct UPBUF));
	if (fd->upbuf == 0)
	  {
	  c_errmsg("Error allocating dynamic memory","allocbuf-alloc",NON_FATAL);
	  return(E_FAIL);
  	  }
		
	if (fd->nbuf > 0)
		{

	/* this calculation needs to be revised as in the non circ 
	 * buffer case or running out of buffer space could occur
	 * when circ buffering is used
        ---------------------------------------------------------- */
		NLINES = NLINES / 2;
		nlines = 2 * NLINES;
/*
		nlines = (2 * NLINES + fd->nbuf + 1)/NLINES *
			NLINES;
*/
		}
	else
		{

	/* the global number of lines must be cut in half to allow for
	   update bufferring
        -------------------------------------------------------------- */
		NLINES = NLINES / 2;
		nlines = 2 * NLINES;
		}

	for (i=0; i<fd->nbands; i++)
		{
		fd->upbuf[i].buf_start = gd->the_nxt_space;
		fd->upbuf[i].buf_end = fd->upbuf[i].buf_start +
			nlines * fd->linsiz;
		gd->the_nxt_space = fd->upbuf[i].buf_end;
		if (gd->the_nxt_space > gd->thebuf + gd->thebufsz)
		   {
		   sprintf(errtxt,
			   "Not enough buffer space allocated for file %s",
		   	   fd->fname);
		   c_errmsg(errtxt,"allocbuf-buffer",NON_FATAL);
		   return(E_FAIL);
		   }

		fd->upbuf[i].nxt_offset = fd->upbuf[i].buf_start;
		fd->upbuf[i].nxtrd_buf = fd->upbuf[i].buf_start;
		fd->upbuf[i].nxtwt_buf = fd->upbuf[i].buf_start;
		fd->upbuf[i].rdcnt = 0;
		fd->upbuf[i].wtcnt = NLINES;

		/* if conversion buffer necessary allocate it 
		---------------------------------------------*/
		if (fd->flags & CONVERSION)
		     {
	             fd->upbuf[i].conbuf = (unsigned char *)
				calloc(1, fd->file_linsiz * NLINES);
		     if (fd->upbuf[i].conbuf == NULL)
	  	        {
	  	        c_errmsg("Error allocating dynamic memory",
			         "allocbuf-alloc",NON_FATAL);
	  	        return(E_FAIL);
  	  	        }
		     }  /*  if fd->flags & CONVERSION  */
		}
	}
else
	{
	fd->buf = (struct BUF *) calloc(fd->nbands, sizeof(struct BUF));
	if (fd->nbuf > 0)
		{
/*
		nlines = (2 * NLINES + fd->nbuf + 1)/NLINES *
			NLINES;
*/
		nlines = NLINES;
		}
	else
		nlines = NLINES;

	for (i=0; i<fd->nbands; i++)
		{
		fd->buf[i].buf_start = gd->the_nxt_space;
		fd->buf[i].buf_end = fd->buf[i].buf_start +
			nlines * fd->linsiz;
		gd->the_nxt_space = fd->buf[i].buf_end;
		if (gd->the_nxt_space > gd->thebuf + gd->thebufsz)
		   {
		   sprintf(errtxt,
			   "Not enough buffer space allocated for file %s",
		   	   fd->fname);
		   c_errmsg(errtxt,"allocbuf-buffer",NON_FATAL);
		   return(E_FAIL);
		   }

		fd->buf[i].nxt_offset = fd->buf[i].buf_start;
		fd->buf[i].nxt_buf = fd->buf[i].buf_start;

		if (fd->acc == IREAD)
			fd->buf[i].cnt = 0;
		else
			fd->buf[i].cnt = NLINES;

		/* if conversion buffer necessary allocate it 
		---------------------------------------------*/
		if (fd->flags & CONVERSION)
		     {
	             fd->buf[i].conbuf = (unsigned char *)
				calloc(1, fd->file_linsiz * NLINES);
		     if (fd->buf[i].conbuf == 0)
	  	        {
	  	        c_errmsg("Error allocating dynamic memory",
			         "allocbuf-alloc",NON_FATAL);
	  	        return(E_FAIL);
  	  	        }
		     }  /*  if fd->flags & CONVERSION  */
		}
	}
return(E_SUCC);
}
