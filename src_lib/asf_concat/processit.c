/******************************************************************************
FUNCTION:	process_it

PURPOSE:	Process_it is an internal imageio function called by
		estep for each line.  Do to the complexity of asynchronous I/O
		this routine gets pretty hairy.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0	   06/85    T. Butzer		Original implementation

  1.1	   03/86    T. Butzer 		Return status

  1.2	   03/86    K. Johnson		Handle multiple file groups

  2.0	   12/87    B. Ailts		Changed include directory specification
					Changed to raw 'C' types
					Moved bridge routines to seperate file

  2.1      04/88    D.Hollaren		Replaced cerrmsg with c_errmsg
					Replaced newlas.h with las.h

  2.2      05/88    B.Ailts		Replaced errh with c_errmsg
					Replaced xi____ with i____ calls
					Removed eerror.h
  2.3	   08/88    B.Ailts		Removed the status checks from the
					i_read and i_write calls -- they do
					not return any values
					Added better error checking
  2.4      12/88    B.Ailts		Output an error message when output
					values are clipped
  2.5      02/91    B.Ailts		Standardized error messages
  2.6      05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  2.7	   10/92    T. Mittan		Added compression option
	    1/94    K.Gacke		Modified to read/write dal images.  TAE
					images can be read, but all images are
					written as a BSQ DAL image.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	Has to be run under TAE

PROJECT			LAS

ALGORITHM:
If read access
    If  
        If conversion
	   Read into the conversion buffer
	   Recalculate the offsets
	   Convert the data into the output buffer
        Else
	   Read into the output buffer
	Recalculate pointers and offsets
Else if write access
   Set up pointers
   If buffer needs to be written
   Recalculate the starting line and number of lines in buffer
   If conversion
      Recalculate buffers
      Convert the input buffer into the conversion buffer
      Write the conversion buffer
   Else
      Write the input buffer
   Recalculate pointers
Else if update accesss
   Set up pointers
   If conversion
      Output an error message
   Else
      Read in the lines
      If current line is greater than or equal to the number of lines in buffer
         Write the lines
   Recalculate the offsets
   
ALGORITHM REFERENCES	none
*******************************************************************************/

#include "pixman.h"
#include "las.h"
#include "locinc.h"
#include <unistd.h>

#define NLINES gd->gbl_nlines
/**************
#define DEBUG 1
**************/

lasErr FUNCTION process_it(struct FDESC *fd, int cline, int cband,struct GDESC * gd)
{
int line, nlines;
int status;
int count2;			/* # bytes to read/write for dal image files */
unsigned int newpos;	/* file positions for dal image files */

short big_first = TRUE;
short small_first = TRUE;

unsigned char *coffset, *inoffset;
char errtxt[ERRLEN + 1];
int i;

if (fd->acc == IREAD)
   {
     if (fd->buf[cband].cnt == 0)
	{
	line = cline + fd->sl - fd->nbuf;
        if (line < fd->sl) line = fd->sl;
        if ((cline + NLINES) > fd->nl) nlines = fd->nl - cline;
	else nlines = NLINES;
	if (fd->flags & CONVERSION)
	  {
	  if (cline < fd->nl)
            {
		newpos = ((fd->bands[cband] - 1) * fd->img_nl * fd->cnvlsz) +
			((line - 1) * fd->cnvlsz);
                if (lseek(fd->dalfd,newpos,0) < 0)
                    {
                    perror("c_eread-seek");
                    c_errmsg("error seeking dal file","eread",NON_FATAL);
                    return(E_FAIL);
                    }
                count2 = fd->cnvlsz * nlines;
                if (read(fd->dalfd,(char *)fd->buf[cband].conbuf,count2) < count2)
                    {
                    perror("c_eread-read");
                    c_errmsg("error reading dal file","eread",NON_FATAL);
                    return(E_FAIL);
                    }

		inoffset = (unsigned char *) (fd->buf[cband].conbuf
		 + (fd->ss - 1) * datasize[fd->conv_type]);
		coffset = (unsigned char *) (fd->buf[cband].nxt_buf
		 + (fd->ss - 1) * datasize[fd->dtype]);
		for (i=0; i<nlines; i++)
		   {
		    status = c_pxconv(fd->conv_type, fd->dtype, 
		    inoffset, coffset, fd->ns);
		    if (status != E_SUCC)
		      {
		       if (((status & TOOBIG) == TOOBIG) && (big_first))
		         {
		           sprintf(errtxt,"%s%s",
			    "Pixel values were larger than",
			    " maximum allowed by output data type");
		           c_errmsg(errtxt,"estep-pixel",NON_FATAL);
		           big_first = FALSE;
		         }
		       else if (((status&TOOSMALL)==TOOSMALL)&&(small_first))
		         {
		           sprintf(errtxt,"%s%s",
			    "Pixel values were smaller than",
			    " minimum allowed by output data type");
		           c_errmsg(errtxt,"estep-pixel",NON_FATAL);
		           small_first = FALSE;
		         }
		      }
		    coffset += fd->linsiz;
		    inoffset += fd->cnvlsz;
		   }
	    } /* end of if cline < fd->nl */
	  } /* end of if (fd->flags & CONVERSION) */
	else
	  {
  	   if (cline < fd->nl)
	    {
		newpos = ((fd->bands[cband] - 1) * fd->img_nl * fd->linsiz) +
			((line - 1) * fd->linsiz);
                if (lseek(fd->dalfd,newpos,0) < 0)
                    {
                    perror("c_eread-seek");
                    c_errmsg("error seeking dal file","eread",NON_FATAL);
                    return(E_FAIL);
                    }
                count2 = fd->linsiz * nlines;
                if (read(fd->dalfd,(char *)fd->buf[cband].nxt_buf,count2) < count2)
                    {
                    perror("c_eread-read");
                    c_errmsg("error reading dal file","eread",NON_FATAL);
                    return(E_FAIL);
                    }
	    } /* end if cline */
	  }  /* end else for if (fd->flags & CONVERSION) */
	fd->buf[cband].cnt = NLINES;
	fd->buf[cband].nxt_buf += (fd->linsiz * NLINES);
	if (fd->buf[cband].nxt_buf >= fd->buf[cband].buf_end)
          {
 	    fd->buf[cband].nxt_buf = fd->buf[cband].buf_start;
	    if (fd->compres) fd->foffset[cband] = 0;
          }
	}  /* end of if (fd->buf[cband].cnt == 0) */

     fd->offset[cband] = (fd->buf[cband].nxt_offset + (fd->ss - 1) *
	datasize[fd->dtype] - gd->thebuf) / datasize[fd->dtype];
     fd->buf[cband].nxt_offset += fd->linsiz;
     if (fd->buf[cband].nxt_offset >= fd->buf[cband].buf_end)
       fd->buf[cband].nxt_offset=fd->buf[cband].buf_start+(fd->nbuf*fd->linsiz);
     fd->buf[cband].cnt--;
   }  /*end of if (fd->acc == IREAD) */

else if (fd->acc == IWRITE)

   {
     fd->offset[cband] = (fd->buf[cband].nxt_offset + (fd->ss - 1) *
	datasize[fd->dtype] - gd->thebuf) / datasize[fd->dtype];
     fd->buf[cband].nxt_offset += fd->linsiz;
     if (fd->buf[cband].nxt_offset >= fd->buf[cband].buf_end)
       fd->buf[cband].nxt_offset = fd->buf[cband].buf_start;
     if ((fd->buf[cband].cnt==0)||(cline>=fd->nl)||(fd->compres&&cline!=0))
       {
	line = cline + fd->sl - NLINES;
        if (fd->compres) nlines = 1;
        else
          {
	    if (cline >= fd->nl)
	      {
		nlines = NLINES - fd->buf[cband].cnt;
		line = cline + fd->sl - (NLINES - fd->buf[cband].cnt);
    	      }
	    else nlines = NLINES;
          }
        if (fd->flags & CONVERSION)
          {
   	   if (fd->compres)
 	   inoffset=(unsigned char*)(&fd->buf[cband].nxt_buf[fd->foffset[cband]]
				    + (fd->ss - 1) * datasize[fd->dtype]);
           else
	     inoffset = (unsigned char *) (fd->buf[cband].nxt_buf
				    + (fd->ss - 1) * datasize[fd->dtype]);
	   coffset = (unsigned char *) (fd->buf[cband].conbuf
				    + (fd->ss - 1) * datasize[fd->conv_type]);
	   for (i=0; i < nlines; i++)
	    {
  	    status=c_pxconv(fd->dtype,fd->conv_type,inoffset,coffset,fd->ns);
	       if (status != E_SUCC)
	        {
	          if (((status & TOOBIG) == TOOBIG) && (big_first))
		    {
		      sprintf(errtxt,"%s%s%s", "Pixel values were larger than",
			   " maximum allowed by output", " data type");
		      c_errmsg(errtxt,"estep-pixel", NON_FATAL);
		      big_first = FALSE;
		    }
	          else if (((status & TOOSMALL)  == TOOSMALL) && (small_first))
		    {
		      sprintf(errtxt,"%s%s%s", "Pixel values were smaller than",
		           " minimum allowed by output", " data type");
		      c_errmsg(errtxt,"estep-pixel", NON_FATAL);
		      small_first = FALSE;
		    }
	        }
	       coffset += fd->cnvlsz;
	       inoffset += fd->linsiz;
	     }
           newpos = ((fd->bands[cband] - 1) * fd->img_nl * fd->cnvlsz) +
                        	        ((line - 1) * fd->cnvlsz);
           if (lseek(fd->dalfd,newpos,0) < 0)
             {
               perror("c_ewrite-seek");
               c_errmsg("error seeking dal file", "ewrite",NON_FATAL);
               return(E_FAIL);
             }
           count2 = fd->cnvlsz * nlines;
           if (write(fd->dalfd, (char *)fd->buf[cband].conbuf,count2) < 0)
             {
               perror("c_ewrite-write");
               c_errmsg("error writing dal file", "ewrite",NON_FATAL);
               return(E_FAIL);
             }
	  }
	else
	  {
           newpos = ((fd->bands[cband] - 1) * fd->img_nl * 
			fd->linsiz) + ((line - 1) * fd->linsiz);
           if (lseek(fd->dalfd,newpos,0) < 0)
             {
               perror("c_ewrite-fseek");
               c_errmsg("error seeking dal file", "ewrite",NON_FATAL);
               return(E_FAIL);
             }
           count2 = fd->linsiz * nlines;
           if (write(fd->dalfd, (char *)fd->buf[cband].nxt_buf,count2) < 0)
             {
               perror("c_ewrite-write");
               c_errmsg("error writing dal file", "ewrite",NON_FATAL);
               return(E_FAIL);
             }
	  }
	if ((fd->buf[cband].cnt == 0) || (cline >= fd->nl)) 
          {
	    fd->buf[cband].cnt = NLINES;
	    fd->buf[cband].nxt_buf += (fd->linsiz * NLINES);
	    if (fd->buf[cband].nxt_buf >= fd->buf[cband].buf_end)
              {
	       fd->buf[cband].nxt_buf = fd->buf[cband].buf_start;
	       if (fd->compres) fd->foffset[cband] = 0;
              }
          }
       }  /* end of  if ((fd->buf[cband].cnt==0)...  */

     fd->buf[cband].cnt--;
   }  /* end of else if (fd->acc == IWRITE) */

else if (fd->acc == IUPDATE)

   {
     if (fd->upbuf[cband].rdcnt == 0)
       {
	line = cline + fd->sl;
	if ((cline + NLINES) > fd->nl) nlines = fd->nl - cline;
	else nlines = NLINES;
	if (fd->flags & CONVERSION)
	 {
	  sprintf(errtxt,
  	    "Conversions not allowed with update access of file %s",fd->fname);
	  c_errmsg(errtxt,"estep-updconv",NON_FATAL);
	  return(E_FAIL);
	 }
	else
	 {
	  if (cline < fd->nl)
	   {
             newpos = ((fd->bands[cband] - 1) * fd->img_nl 
	 	       * fd->linsiz) + ((line - 1) * fd->linsiz);
             if (lseek(fd->dalfd,newpos,0) < 0)
               {
                 perror("c_eread-seek");
                 c_errmsg("error seeking dal file", "eread",NON_FATAL);
                 return(E_FAIL);
               }
             count2 = fd->linsiz * nlines;
             if (read(fd->dalfd, (char *)fd->upbuf[cband].nxtrd_buf,count2) < 0)
               {
                 perror("c_eread-read");
                 c_errmsg("error reading dal file", "eread",NON_FATAL);
                 return(E_FAIL);
               }
	   } /* end if cline */
	 }
	fd->upbuf[cband].rdcnt = NLINES;
	fd->upbuf[cband].nxtrd_buf += (fd->linsiz * NLINES);
	if (fd->upbuf[cband].nxtrd_buf >= fd->upbuf[cband].buf_end)
	  fd->upbuf[cband].nxtrd_buf = fd->upbuf[cband].buf_start;
       }

     if ((fd->upbuf[cband].wtcnt == 0) || (cline >= fd->nl))
       {
	line = cline + fd->sl - NLINES;
	if (cline >= fd->nl)
  	  {
  	    nlines = NLINES - fd->upbuf[cband].wtcnt;
 	    line = cline + fd->sl - (NLINES - fd->upbuf[cband].wtcnt);
	  }
	else nlines = NLINES;

        newpos=((fd->bands[cband]-1)*fd->img_nl*fd->linsiz)+
		((line-1)*fd->linsiz);
        if (lseek(fd->dalfd,newpos,0) < 0)
          {
            perror("c_ewrite-seek");
            c_errmsg("error seeking dal file","ewrite",NON_FATAL);
            return(E_FAIL);
          }
        count2 = fd->linsiz * nlines;
        if (write(fd->dalfd,(char *)fd->upbuf[cband].nxtwt_buf,count2) < 0)
          {
            perror("c_ewrite-write");
            c_errmsg("error writing dal file","ewrite",NON_FATAL);
            return(E_FAIL);
          }

	fd->upbuf[cband].wtcnt = NLINES;
	fd->upbuf[cband].nxtwt_buf += (fd->linsiz * NLINES);
	if (fd->upbuf[cband].nxtwt_buf >= fd->upbuf[cband].buf_end)
	   fd->upbuf[cband].nxtwt_buf = fd->upbuf[cband].buf_start;
       }

     fd->offset[cband] = (fd->upbuf[cband].nxt_offset + (fd->ss - 1)
		 * datasize[fd->dtype] - gd->thebuf) / datasize[fd->dtype];
     fd->upbuf[cband].nxt_offset += fd->linsiz;
     if (fd->upbuf[cband].nxt_offset >= fd->upbuf[cband].buf_end)
       fd->upbuf[cband].nxt_offset = fd->upbuf[cband].buf_start;

     fd->upbuf[cband].rdcnt--;
     fd->upbuf[cband].wtcnt--;
   }  /* else if (fd->acc == IUPDATE) */

return(E_SUCC);
}
