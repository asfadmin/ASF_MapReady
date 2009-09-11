/******************************************************************************
FUNCTION:	c_eread

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0	            T.Butzer		Original development
  2.0	   12/87    B.Ailts		Placed bridge routine in seperate file
  					Changed name from eread to c_eread
  2.1	   04/88    D.Hollaren	        Replaced newlas.h with las.h
  				        Replaced cerrmsg with c_errmsg
  2.2      05/88    B.Ailts		Replaced errh with c_errmsg
					Changed calling sequences of the DDR
					routines -- only one structure
					Took out eerror.h
  2.3	   08/88    L. Huewe		Added argument for number of lines to
					be read at one time
  2.4	   12/88    B. Ailts		Output an error message when the data
					is being clipped
  2.5       2/88    B. Ailts		Replaced the 'ceil' function -- does
					not work correctly
  2.6      02/91    B. Ailts		Standardized error messages
  2.7      05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  2.8	   10/92    T.Mittan		Added compression option
	    1/94    K.Gacke		Modified to read/write dal images.  TAE
					images can be read, but all images are
					written as a BSQ DAL image.
  7.0	    5/95    T. Logan		Removed TAE Dependencies
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT			LAS

ALGORITHM 
If conversion
   Read the line into temporary buffer
   Convert the data into the output buffer
Else
   Read the line into the output buffer
Return number of samples read

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include <unistd.h>
#include "las.h"
#include "locinc.h"
#include "pixman.h"

int FUNCTION  read_dal(struct FDESC *fd,int *eband,int *eline,unsigned char *euserbuf,int *nlines);

lasErr FUNCTION c_eread(struct FDESC **fdesc, int *eband,int * eline, unsigned char *euserbuf,int *nlines)
{
struct FDESC *fd;
int status;
fd = *fdesc;
if (fd->dal)
    {
    status = read_dal(fd, eband, eline, euserbuf, nlines);
    return((lasErr)status);
    }
 return (E_FAIL);
}


int FUNCTION read_dal(struct FDESC *fd,int *eband,int *eline,unsigned char *euserbuf,int *nlines)
{
static short big_first = TRUE;
static short small_first = TRUE;
int line;
int lsize;
int nbyte;
int status;
unsigned int newpos;
char errtxt[ERRLEN];
unsigned char *ubufptr;

lsize = (fd->flags & CONVERSION) ? fd->cnvlsz : fd->linsiz;
if (fd->flags & CONVERSION) 
    {
    ubufptr = euserbuf;
    for (line=0; line<*nlines; line++)
	{
/*
* Need to seek in case a window is specified.
*/
        newpos = ((*eband - 1) * fd->img_nl * lsize) +
	    ((*eline + fd->sl + line - 2) * lsize) + 
	    ((fd->ss - 1) * datasize[fd->conv_type]);
	if (lseek(fd->dalfd,newpos,0) < 0)
	    {
	    perror("c_eread-seek");
            c_errmsg("error seeking dal file","eread",NON_FATAL);
            return(E_FAIL);
	    }
	nbyte = fd->ns * datasize[fd->conv_type];
	status = read(fd->dalfd,(char *)fd->conv_buf,nbyte);
	if (status < nbyte)
            {
	    perror("c_eread-read");
            c_errmsg("error reading dal file","eread",NON_FATAL);
            return(E_FAIL);
            }
	status = c_pxconv(fd->conv_type,fd->dtype,fd->conv_buf,
		ubufptr,fd->ns);
	if (status != E_SUCC)
	   {
	   if (((status & TOOBIG) == TOOBIG) && (big_first))
	      {
	      sprintf(errtxt,"%s%s", "Pixel values were larger than",
			     " maximum allowed by output data type");
	      c_errmsg(errtxt,"eread-pixel",NON_FATAL);
	      big_first = FALSE;
	      }
	   else if (((status & TOOSMALL)  == TOOSMALL) && (small_first))
	      {
	      sprintf(errtxt,"%s%s", "Pixel values were smaller than",
			     " minimum allowed by output data type");
	      c_errmsg(errtxt,"eread-pixel",NON_FATAL);
	      small_first = FALSE;
	      }
	   }
	ubufptr += (fd->ns * datasize[fd->dtype]);
	}
    }
else if (fd->flags & MOVEDATA)
    {
    ubufptr = euserbuf;
    for (line=0; line<*nlines; line++)
	{
        newpos = ((*eband - 1) * fd->img_nl * lsize) +
	    ((*eline + fd->sl + line - 2) * lsize) + 
	    ((fd->ss - 1) * datasize[fd->dtype]);
	if (lseek(fd->dalfd,newpos,0) < 0)
	    {
	    perror("c_eread-seek");
            c_errmsg("error seeking dal file","eread",NON_FATAL);
            return(E_FAIL);
	    }
	nbyte = fd->ns * datasize[fd->dtype];
	status = read(fd->dalfd,(char *)ubufptr,nbyte);
	if (status < nbyte)
            {
	    perror("c_eread-read");
            c_errmsg("error reading dal file","eread",NON_FATAL);
            return(E_FAIL);
            }
	ubufptr += nbyte;
	}
    }
else
    {
    ubufptr = euserbuf;
    newpos = ((*eband - 1) * fd->img_nl * lsize) +
	    ((*eline + fd->sl - 2) * lsize) + 
	    ((fd->ss - 1) * datasize[fd->dtype]);
    if (lseek(fd->dalfd,newpos,0) < 0)
	{
	perror("c_eread-seek");
        c_errmsg("error seeking dal file","eread",NON_FATAL);
        return(E_FAIL);
	}
    nbyte = lsize * *nlines;
    status = read(fd->dalfd,(char *)ubufptr,nbyte);
    if (status < nbyte)
        {
	perror("c_eread-read");
        c_errmsg("error reading dal file","eread",NON_FATAL);
        return(E_FAIL);
        }
    }
return((fd->ns * (*nlines)));
}
